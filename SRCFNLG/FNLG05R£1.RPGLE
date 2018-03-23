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
009301170922     d tigcp         e ds                  extname(tigcp00f)
009302170922     d fnlg33ds      e ds                  inz
009303170922     d tigcpa        e ds                  extname(tigcp00f)
009304170922     d                                     prefix(a)
009305170922     d tigcpb        e ds                  extname(tigcp00f)
009306170922     d                                     prefix(b)
009400050315     d gcpinf          ds
009500050315     d  gcpnrr               397    400i 0
009600940914     D*  Caricamento capoconto clienti
009700940914     D*  Chiavi per caricamento tabelle
009800020222     d* chiavi filiali
009900020430     D FI              S              3    DIM(30)
010000020222     d* filiali £1
010100020430     D £1              S              3  0 DIM(30)
010200020222     d* chiavi filiali
010300020430     D KEY             S              3  0 DIM(30)
010400020222     d* codie bolla
010500940919     D C3A             S              2    DIM(50)
010600020222     d* tipo  bolla
010700940914     D T3A             S              2    DIM(50)
010800020222     d* descr.bolla
010900940914     D D3A             S             25    DIM(50)
011000021031     d* codie bolla da non trasmettere in sede
011100021031     D g3A             S              1    DIM(50)
011200020222     d* codici eventi
011300080903     D C2A             S              3    DIM(200)
011400020222     d* gg.eventi
011500080903     D G2A             S              1  0 DIM(200)
011600020222     d* descriz.eventi
011700080903     D D2A             S             45    DIM(200)
011800020222     d* immissione s/n
011900080903     D F2A             S              1    DIM(200)
012000020222     d* tipologia evento
012100080903     D T2A             S              1    DIM(200)
012200020222     d* tipologia part.giac.
012300080903     D TPG             S              1    DIM(200)
012400090520     d* tipo servizio e descrizione
012500090520     D c5e             S              1    DIM(10)
012600090520     D d5e             S             15    DIM(10)
012700040115     d* motivo ammesso per data consegna richiesta
012800080903     D dcr             S              1    DIM(200)
012900020222     d* codici part.giac.
013000080903     D C7Q             S              2    DIM(200)
013100020222     d* descr. part.giac.
013200080903     D D7Q             S             89    DIM(200)
013300021031     d* codici part.giac. estensione
013400080903     D C7W             S              2    DIM(200)
013500021031     d* descr. part.giac. estensione
013600110531     D D7W             S             89    DIM(200)
013700020222     d* codici eventi
013800940920     D C2B             S              3  0 DIM(50)
013900020222     d* descriz.eventi
014000940920     D D2B             S             20    DIM(50)
014100020222     d* codici com.giac.
014200940919     D C2F             S              1    DIM(50)
014300020222     d* tipo comun.
014400960201     D T2F             S              1    DIM(50)
014500020222     d* invio comun.
014600960201     D F2F             S              1    DIM(50)
014700050512     d* testo estero
014800050512     D e2F             S              1    DIM(50)
014900020222     d* descrz. comun.
015000940920     D D2F             S             20    DIM(50)
015100050614     d* riservata ced
015200050614     D r2F             S              1    DIM(50)
015300050614     d* stampa automatica comunicaz. mittente
015400050614     D s2F             S              1    DIM(50)
015500050512     d* scrittura FNVAG00F
015600050512     D v2F             S              1    DIM(50)
015700020222     d* cod. naz.
015800011217     D CNZ             S              3    DIM(500)
015900020222     d* flag ita/est
016000011217     D ITA             S              1    DIM(500)
016100020222     d* filiale poste
016200000526     D FIP             S              3  0 DIM(50)
016300040329      *
016400040329     D w99             S              1    dim(99)
016500020222     d* messaggi errore
016600170530     D ERR             S             70    DIM(40) CTDATA PERRCD(1)
016700020222     d* QCAEXEC
016800970225     D CHK             S             45    DIM(2) CTDATA PERRCD(1)
016900040329     D CMA4            S             99    dim(1) ctdata perrcd(1)
017000040329     D CMA5            S             99    dim(1) ctdata perrcd(1)
017100970226     D* Schiere tabelle EDI
017200020222     D* EVENTI 2A SU EDTAB
017300080903     D E2A             S              3    DIM(200)
017400020222     D* FLG AGG.LISTA LAB.
017500080903     D FTL             S              1    DIM(200)
017600020222     D* FLG AGG.LISTA VGL
017700080903     D FTV             S              1    DIM(200)
017800020222     D* COD.ANAGR.PARTNER
017900080903     D KSC             S              7    DIM(200)
018000020222     D* IDENTIF.PARTNER
018100080903     D CPT             S             35    DIM(200)
018200020222     D* TIPO LISTA
018300080903     D TLI             S              1    DIM(200)
018400150512     D fnlv59ds      E DS
018500050217     D fnlblds       E DS                  EXTNAME(FNLbl00f) inz
018600050217     D lblsav        E DS                  EXTNAME(FNLbl00f) prefix(£) inz
018700940920     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE
018800940920     D PARAM0          DS
018900940920     D  PA0AAS                 1      4  0
019000940920     D  PA0LNP                 5      7  0
019100940920     D  PA0NRS                 8      9  0
019200940920     D  PA0NSP                10     16  0
019300940921     D  PA0FLG                18     18
019400940920     D  PA0SIM                44     46  0
019500940921     D  PA0F03                48     48
019600161114     D*PARAM1          DS
019700161114     D* PA1AAS                14     17  0
019800161114     D* PA1LNP                18     20  0
019900161114     D* PA1NRS                21     22  0
020000161114     D* PA1NSP                23     29  0
020100161114     D* PA1F03                30     30
020200161114     D* PA1FLG                31     31
020300161118     d fnlru6ds      e ds
020400161114      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
020500161114     D PARAMu6ds       DS
020600161114     d* CAMPI DI OUTPUT:
020700161114     d* . Key spedizione
020800161114     D  pa1AAS                14     17  0
020900161114     D  pa1LNP                18     20  0
021000161114     D  pa1NRS                21     22  0
021100161114     D  pa1NSP                23     29  0
021200161114     d* . Flag '1'=Premuto F3=Fine
021300161114     D  PA1F03                30     30
021400161114     d* CAMPI DI INPUT:
021500161114     d* . Modalità di richiamo
021600161114     D  PA1FLG                31     31
021700161114     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
021800161114     d*                                in interrogazione della bolla ed evitare
021900161114     d*                                errore di chiamata ricorsiva
022000161114     D  PA1GIA               144    144
022100161114     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
022200161114     d  pa1rma               149    163
022300161114     d  pa1xco               164    164
022400161114     d* non utilizzato
022500161114     d  pa1flo               165    256
022600161114      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
022700161114     D PARAMU6ds1      DS
022800161114     d** INPUT/OUTPUT (se richiesta la selezione bolla)
022900161114     D  pa1AAS_               14     17  0
023000161114     D  pa1LNP_               18     20  0
023100161114     D  pa1NRS_               21     22  0
023200161114     D  pa1NSP_               23     29  0
023300161114     d** OUTPUT
023400161114     D  PA1F03_               30     30
023500161114     d** INPUT
023600161114     D  PA1FLG_               31     31
023700161114     D*    SE RICH DA PGM GIAC. = 1
023800161114     D  PA1GIA_              144    144
023900161114     d** OUTPUT
024000161114     d  pa1rma_              149    163
024100161114     d  pa1xco_              164    164
024200161114     d** INPUT
024300161114     d*   Tipo lancio: "C" -           CHIUSO CON LR
024400161114     d*                "L" - ELABORO E CHIUDO CON LR
024500161114     d*                " " - ELABORO E CHIUDO IN RETRN
024600161114     d  pa1tla_              165    165
024700161124     d** OUTPUT - Codice di errore 1=Sped. non trovata
024800161124     d  pa1err_              166    166
024900161124     d  pa1flo_              167    256
025000900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
025100940919     D  TCU                  398    697
025200900418     D                                     DIM(50)
025300900418     D  KCU                  698    847P 0
025400900418     D                                     DIM(50)
025500900418     D                                     PACKEVEN
025600021114     D Tibs36ds      E DS
025700940914     D WLBDA8          DS
025800940914     D  G02DAT                 1      8  0
025900940914     D  G02INV                 9     16  0
026000940914     D  G02ERR                17     17
026100940914     D  G02TGI                18     22  0
026200950105      *
026300950105     D DATPAR          DS
026400950105     D  GIODAT                 1      8  0
026500950105     D  GIOINV                 9     16  0
026600950105     D  GIOTGI                17     21  0
026700950105     D GFES8           DS
026800950105     D  DATADA                 1      8  0
026900950105     D  DATAA                  9     16  0
027000950105     D  GGF                   17     21  0
027100040115     d Wdata8          DS
027200040115     d  dadata                 1      8  0
027300040115     d  adata                  9     16  0
027400040115     d  GioLav                17     21  0
027500040115     d dataiso         s               d   datfmt(*iso)
027600050609     d savind28        s              1    inz('0')
027700040115      *
027800160316     D TRUL27        E DS                  EXTNAME(TRUL27DS1)
027900020222     D DSTB69        E DS                  EXTNAME(DS7PQRS)
028000970401     D DSLR33        E DS                  EXTNAME(FNLR33DS)
028100950113     D DS15          E DS
028200941003     D DS3A          E DS
028300911028     D DS2A          E DS
028400050401     D DS2U          E DS
028500950104     D DS2Z          E DS
028600940920     D DS2F          E DS
028700990805     D DS2G          E DS
028800000202     D OG143         E DS
028900070320     D OG148         E DS
029000070410     D OG146         E DS
029100050217     D DS3k          E DS
029200140410     d ds7r          e ds
029300050418     D DS7Q          E DS
029400020222     d* tipologia cod.giac.7
029500950102     D  TIP                   78     82
029600950104     D                                     DIM(5)
029700950104     D  WTIP                  78     82
029800020222     d* descr. cod.giac.7
029900950102     D  DSP                   83     87
030000950104     D                                     DIM(5)
030100021031     D DS7W          E DS
030200110531     D  Tmt                   25     39
030300110531     D                                     DIM(5)
030400900418     D TCUDS           DS
030500900418     D  F3                     3      3
030600900418     D  F4                     4      4
030700900418     D  F56                    5      6
030800900418     D CNCR80        E DS
030900900418     D KPJBA         E DS
031000950104     D* Passo pgm di stmampa segnacolli
031100950104     D  WPGMSS               492    501
031200021106     D savKPJBu        S                   like(kpjbu)
031300070705     D §kpjbu          S                   like(kpjbu)
031400100614     D savfbc          S                   like(arbfbc)
031500021106
031600970226     D EDDSPT        E DS                  EXTNAME(EDIDSPT)
031700970226     D EDDSPU        E DS                  EXTNAME(EDIDSPU)
031800970226     D EDDS2A        E DS                  EXTNAME(EDIDS2A)
031900970226     D FNLG05        E DS                  EXTNAME(FNLG05DS)
032000970624     D FNLG03        E DS                  EXTNAME(FNLG03DS)
032100950102     D FNLG30        E DS                  EXTNAME(FNLG30DS)
032200950104     D FNLG82        E DS                  EXTNAME(FNLG82DS)
032300960201     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
032400960201     D TRUL42        E DS                  EXTNAME(TRUL42DS)
032500960306     D TRUL43        E DS                  EXTNAME(TRUL43DS)
032600020430     d trul06ds      e ds
032700020430     d  lin                    1     90  0 dim(30)
032800040310     d dsbl4a        e ds
032900070320     d dsbl4l        e ds
033000040329     d fnlsb5ds      e ds
033100130315     d FNLSB6ds1     e ds                  inz
033200130315     d  DB6pdf       e                     inz('X')
033300080618     d truldesds     e ds                  inz
033400040716     d fidn12ds      e ds
033500940915     D                 DS
033600940915     D  CDTAB                 52     53
033700940915     D  DSMSG                  1     70
033800940915     D                 DS
033900940915     D  WGG                    1      2  0
034000940915     D  WMM                    3      4  0
034100940915     D  WGGMM                  1      4  0
034200950206     D                 DS
034300950206     D  §SEI                   1      6
034400950206     D  §PRF                   1      3
034500950206     D  §FLE                   4      6  0
034600990518     D FNANM1          DS
034700970401     D  ANMNR1               397    400B 0
034800021031     d param80         ds
034900021031     d  fldlnpa                       3
035000021031     d  fldaasa                       4
035100021031     d  fldnrsa                       2
035200021031     d  fldnspa                       7
035300021031     d  fldcca                        1
035400021031     d  fldgga                        2
035500940914     C*--------------------------------------------------------------*
035600940914     C*  CICLO PRINCIPALE
035700940914     C*--------------------------------------------------------------*
035800940927     C     *ENTRY        PLIST
035900940927     C                   PARM                    KPJBA
036000940927     C                   MOVEL     KPJBU         FNLG05
036100961217     C*
036200941003     C                   MOVEL     'N'           WFINE             1
036300950104     C                   MOVEL     D05PSS        WPGMSS
036400950104     C                   CLEAR                   FNLG82
036500950104     C                   MOVE      D05MOB        D82MOB
036600950104     C                   MOVE      D05PRB        D82PRB
036700950104     C                   MOVE      D05MOE        D82MOE
036800950104     C                   MOVE      D05PRE        D82PRE
036900030207     C                   MOVE      d05pss        D82PSS
037000030716     c                   eval      d82mdb4 = d05mdb4
037100030716     c                   eval      d82mdb5 = d05mdb5
037200030716     c                   eval      d82psl = d05psl
037300030716     c                   eval      d82bsl = d05bsl
037400030716     c                   eval      d82prb4 = d05prb4
037500030716     c                   eval      d82prb5 = d05prb5
037600940927     C*--------------------------------------------------------------*
037700940927     C*  Se il programma è richiamato da altri per la prima volta
037800940927     C*  oppure la funzione di manutenzione giacenze è richiesta
037900940927     C*  direttamante da menù eseguo le operazioni di definizione
038000940927     C*  chiavi e caricamento tabelle
038100940927     C     D05RIC        IFNE      'S'
038200940927     C     D05TRI        OREQ      'P'
038300940927     C                   EXSR      DEFCAM
038400970226     C*  Se sono in partenza controllo se aprire EDTAB
038500970226     C     D05GES        IFEQ      'P'
038600970226     C                   EXSR      CHKTAB
038700970226     C*  Se sono in arrivo controllo se aprire EDRDE
038800970226     C                   ELSE
038900961217     C                   EXSR      CHKRDE
039000940927     C                   END
039100970227     C                   END
039200940927     C*--------------------------------------------------------------*
039300940927     C*  Se il pgm. è richiamato per l'ultima volta da altri devo
039400940927     C*  eseguire solo le operazioni di chiusura dei file
039500940927     C*--------------------------------------------------------------*
039600940927     C     D05TRI        IFNE      'U'
039700940927     C*--------------------------------------------------------------*
039800940927     C*  Inizio pgm
039900940927     C                   EXSR      INIPGM
040000940927     C*--------------------------------------------------------------*
040100940921     C     WFINE         DOWEQ     'N'
040200940914     C*  Loop di gestione prima videata
040300940914     C     WTPVID        CASEQ     '1'           GESD01
040400940914     C     WTPVID        CASEQ     '2'           GESD02
040500960123     C     WTPVID        CASEQ     '4'           GESD04
040600961025     C     WTPVID        CASEQ     '5'           GESD05
040700940914     C                   END
040800940914     C                   END
040900940927     C*--------------------------------------------------------------*
041000940927     C                   END
041100940927     C*--------------------------------------------------------------*
041200940919     C     FINE          TAG
041300970610     C     D05GES        IFEQ      'A'
041400970610     C     D05TRI        ANDNE     'U'
041500970606     C     *LOVAL        SETLL     FNARB01L
041600970610     C                   END
041700940927     C*--------------------------------------------------------------*
041800940927     C*  Se il pgm non è richiamato o viene richiamato per l'ulitima
041900940927     C*  volta chiudo con LR diversamente chiudo in RT
042000940927     C     D05TRI        IFEQ      'U'
042100940927     C     D05RIC        ORNE      'S'
042200020306     C                   CLEAR                   TRUL27
042300020306     C                   MOVEL     'C'           I27TLA
042400160316     C                   CALL      'TRUL27R1'
042500020306     C                   PARM                    TRUL27
042600160316      *
042700160316     C                   CLEAR                   fnlv59ds
042800160316     C                   MOVEL     'C'           Ilv59tla
042900160316     C                   CALL      'FNLV59R'
043000160316     C                   PARM                    fnlv59ds
043100160316
043200050630     C                   CLEAR                   fnlg09ds
043300050630     C                   MOVEL     'C'           d09TLA
043400050630     c                   movel     fnlg09ds      kpjbu
043500050630     C                   CALL      'FNLG09R'
043600050630     C                   PARM                    kpjba
043700161114
043800161118     c*                  eval      pa1tla_='C'
043900161118     c*                  eval      kpjbu=paramu6ds1
044000161118     c*                  call      'FNLRU6R1'
044100161118     c*                  parm                    kpjba
044200161114
044300940914     C                   SETON                                        LR
044400940927     C                   ELSE
044500940927     C                   SETON                                        RT
044600940927     C                   END
044700050609     c                   eval      savind28 = '0'
044800961217     C*-----------------------------------------------------*
044900961217     C*  Chkobj su EDRDE
045000961217     C*-----------------------------------------------------*
045100961217     C     CHKRDE        BEGSR
045200961217     C*
045300961217     C*  Controllo esistenza arcchivi
045400961217     C                   Z-ADD     45            LUNG             15 5
045500961217     C* EDRDE00F  Ordini Clienti
045600961217     C                   SETOFF                                         21
045700961217     C                   MOVEL     *BLANKS       COMMAN
045800040329     C                   MOVEA     CHK(1)        COMMAN           99
045900961217     C                   CALL      'QCMDEXC'                            21
046000961217     C                   PARM                    COMMAN
046100961217     C                   PARM                    LUNG
046200961217     C*
046300961217     C                   MOVEL     'N'           WTRRDE
046400961217     C     *IN21         IFEQ      '0'
046500961217     C                   OPEN      EDRDE01L
046600961217     C                   MOVEL     'S'           WTRRDE            1
046700961217     C                   END
046800961217     C*
046900961217     C                   ENDSR
047000970225     C*-----------------------------------------------------*
047100970225     C*  Chkobj su EDTAB
047200970225     C*-----------------------------------------------------*
047300970225     C     CHKTAB        BEGSR
047400970225     C*
047500970225     C*  Controllo esistenza arcchivi
047600970225     C                   Z-ADD     45            LUNG             15 5
047700970225     C* EDTAB00F  Ordini Clienti
047800970225     C                   SETOFF                                         21
047900970225     C                   MOVEL     *BLANKS       COMMAN
048000040329     C                   MOVEA     CHK(2)        COMMAN
048100970225     C                   CALL      'QCMDEXC'                            21
048200970225     C                   PARM                    COMMAN
048300970225     C                   PARM                    LUNG
048400970225     C*
048500970225     C                   MOVEL     'N'           WTRTAB
048600970225     C     *IN21         IFEQ      '0'
048700970225     C                   OPEN      EDTAB01L
048800970225     C                   MOVEL     'S'           WTRTAB            1
048900970311     C                   ELSE
049000970311     C                   GOTO      FINEDI
049100970225     C                   END
049200970226     C* Eseguo caricamento tab. EDI codici partner
049300970226     C                   MOVEL     'PT'          KCOD1
049400970226     C                   MOVEA     *BLANKS       CPT
049500970226     C                   MOVEA     *BLANKS       TLI
049600970227     C                   MOVEA     *BLANKS       KSC
049700071107     C                   Z-ADD     0             X                 3 0
049800970226     C     KCOD1         CHAIN     EDTAB01L                           31
049900970226     C     *IN31         DOWEQ     '0'
050000080903     C     X             ANDLT     200
050100970226     C     TABFLG        IFEQ      ' '
050200970226     C                   MOVEL     TABUNI        EDDSPT
050300970226     C                   ADD       1             X
050400970227     C                   MOVEL     TABKEY        CPT(X)
050500970226     C                   MOVEL     §PTKSC        KSC(X)
050600970226     C                   END
050700970227     C     KCOD1         READE     EDTAB01L                               31
050800970226     C                   END
050900970226     C*  Se non ho caricato tutti i dati gestisco errore
051000970226     C     *IN31         IFNE      '1'
051100970226     C                   EXSR      ERREDI
051200970226     C                   GOTO      FINEDI
051300970226     C                   END
051400970226     C*  Carico tabella correlata PU
051500970226     C                   MOVEL     'PU'          KCOD1
051600970226     C     KCOD1         CHAIN     EDTAB01L                           31
051700970226     C     *IN31         DOWEQ     '0'
051800970226     C                   MOVEL     TBLUNI        EDDSPU
051900970226     C     TABFLG        IFEQ      ' '
052000071107     C                   Z-ADD     1             X
052100970227     C                   MOVEL     TABKEY        WCCPT            35
052200970226     C     WCCPT         LOOKUP    CPT(X)                                 32
052300970226     C     *IN32         IFEQ      '1'
052400970227     C                   MOVEL     TABUNI        EDDSPU
052500970226     C                   MOVE      §PULAB        TLI(X)
052600970226     C                   END
052700970226     C                   END
052800970227     C     KCOD1         READE     EDTAB01L                               31
052900970226     C                   END
053000970226     C*---------------------------------------------------------------*
053100970226     C* Carico tabella 2a
053200071107     C                   Z-ADD     0             X
053300970226     C                   MOVEL     '2A'          KCOD1
053400970226     C     KCOD1         CHAIN     EDTAB01L                           31
053500970226     C     *IN31         DOWEQ     '0'
053600080903     C     X             ANDLT     200
053700970226     C     TABFLG        IFEQ      ' '
053800970226     C                   MOVEL     TABUNI        EDDS2A
053900970226     C                   ADD       1             X
054000970226     C                   MOVEL     TABKEY        E2A(X)
054100970226     C                   MOVEL     §2ADAG        FTL(X)
054200970226     C                   MOVEL     §2ADAV        FTV(X)
054300970226     C                   END
054400970226     C     KCOD1         READE     EDTAB01L                               31
054500970226     C                   END
054600970226     C*  Se non ho caricato tutti i dati gestisco errore
054700970226     C     *IN31         IFNE      '1'
054800970226     C                   EXSR      ERREDI
054900970226     C                   GOTO      FINEDI
055000970226     C                   END
055100970226     C*
055200970226     C     FINEDI        ENDSR
055300940914     C*-----------------------------------------------------*
055400940914     C*  Gestione della prima videata
055500940914     C*-----------------------------------------------------*
055600940919     C     GESD01        BEGSR
055700940914     C*
055800940914     C                   EXFMT     LG05D01
055900940915     C                   SETOFF                                       404142
056000940915     C                   SETOFF                                       28
056100940915     C                   MOVEL     *BLANKS       $MSG
056200940914     C*  Fine Lavoro
056300940914     C     *INKC         IFEQ      '1'
056400940914     C                   MOVEL     'S'           WFINE
056500940914     C                   GOTO      FINVD1
056600940914     C                   END
056700961025     C* Cambio Filiale
056800961025     C     *INKS         IFEQ      '1'
056900961028     C                   Z-ADD     §FLE          V5CFLE
057000961028     C                   MOVEL     V1CRSU        V5CRSU
057100961025     C                   MOVEL     '5'           WTPVID
057200961025     C                   GOTO      FINVD1
057300961025     C                   END
057400940920     C*  Richiamo interr. bolle
057500940915     C     *INKG         IFEQ      '1'
057600940920     C                   EXSR      RICHBL
057700940919     C                   GOTO      FINVD1
057800940915     C                   END
057900940920     C*  Richiamo interr. giacen
058000940920     C     *INKN         IFEQ      '1'
058100950102     C                   CLEAR                   FNLG30
058200950102     C                   MOVEL     D05GES        D30POA
058300950102     C                   Z-ADD     V1CAAS        D30AAS
058400950102     C                   Z-ADD     V1CLNP        D30LNP
058500950102     C                   Z-ADD     V1CNRS        D30NRS
058600950102     C                   Z-ADD     V1CNSP        D30NSP
058700950102     C                   MOVE      *ZEROS        D30FRG
058800950102     C                   MOVEL     FNLG30        KPJBU
058900950102     C     D05GES        IFEQ      'P'
059000950102     C                   CALL      'FNLG30C'
059100940920     C                   PARM                    KPJBA
059200950102     C                   ELSE
059300950102     C                   CALL      'FNLG31C'
059400950102     C                   PARM                    KPJBA
059500950102     C                   END
059600940920     C                   GOTO      FINVD1
059700940920     C                   END
059800940920     C*
059900940920     C                   Z-ADD     V1CAAS        KAAS
060000940920     C                   Z-ADD     V1CLNP        KLNP
060100940920     C                   Z-ADD     V1CNRS        KNRS
060200940920     C                   Z-ADD     V1CNSP        KNSP
060300940920     C* Se anno a 2 lo porto a  4
060400940920     C                   MOVEL     V1CAAS        WCOM2             2 0
060500940920     C     WCOM2         IFEQ      0
060600940920     C     V1CAAS        IFGT      60
060700940920     C                   ADD       1900          V1CAAS
060800940920     C                   ELSE
060900940920     C                   ADD       2000          V1CAAS
061000940920     C                   END
061100940920     C                   END
061200000526     C     V1CLNP        LOOKUP    FIP                                    41
061300000526     C   41              MOVEL     ERR(27)       $MSG
061400000526     C   41              SETON                                        28
061500000526     C   41              GOTO      FINVD1
061600940920     C*
061700950102     C* Se sono in arrivo reperisco codice mancata consegna su figlia
061800951229     C                   Z-ADD     0             D05AAF
061900951229     C                   Z-ADD     0             D05LNF
062000951229     C                   Z-ADD     0             D05NRF
062100951229     C                   Z-ADD     0             D05NSF
062200950102     C     D05GES        IFEQ      'A'
062300950102     C     KSPE1         CHAIN     FNLBL02L                           31
062400950224     C     *IN31         IFEQ      '0'
062500950102     C                   Z-ADD     LBLAAN        KAAS
062600950102     C                   Z-ADD     LBLLPN        KLNP
062700950102     C                   Z-ADD     LBLNRN        KNRS
062800950102     C                   Z-ADD     LBLNSN        KNSP
062900950224     C                   Z-ADD     LBLAAN        D05AAF
063000950224     C                   Z-ADD     LBLLPN        D05LNF
063100950224     C                   Z-ADD     LBLNRN        D05NRF
063200950224     C                   Z-ADD     LBLNSN        D05NSF
063300050404     C     KSPE1         CHAIN(n)  FNARB01L                           31
063400950706     C     *IN31         IFEQ      '0'
063500950706     C                   MOVE      ARBCMC        D05CMC
063600950706     C                   MOVE      ARBDDC        G02INV
063700950706     C                   MOVEL     '3'           G02ERR
063800950706     C                   Z-ADD     0             G02DAT
063900950706     C                   CALL      'XSRDA8'
064000950706     C                   PARM                    WLBDA8
064100950706     C                   MOVE      G02DAT        D05DXD
064200950102     C                   END
064300021031     C                   Z-ADD     1             X
064400950706     C*
064500950706     C                   END
064600950224     C                   END
064700950102     C*
064800940915     C                   Z-ADD     V1CAAS        KAAS
064900940915     C                   Z-ADD     V1CLNP        KLNP
065000940915     C                   Z-ADD     V1CNRS        KNRS
065100940915     C                   Z-ADD     V1CNSP        KNSP
065200050824     c                   clear                   wriape
065300050315     C     KSPE2         CHAIN(e)  tigcp01L                           30
065400050315     c                   if        %error
065500070521     C     KSPE2         CHAIN(n)  tigcp01L
065600050315     c                   exsr      geslck
065700050318     C     KSPE2         CHAIN(e)  tigcp01L                           30
065800050315     c                   endif
065900940915     C* Se sono in partenza eseguo routine controlli partenza
066000940919     C     D05GES        IFEQ      'P'
066001171017     c                   eval      tigcpb = tigcp
066100940915     C                   EXSR      CTRPAR
066200940915     C                   ELSE
066300960514     C* Se la giacenza esiste già e il programma è richiamato
066400960514     C* gestisco devo effettuare una riapertura
066500050408     C     *IN30         IFEQ      '0'
066600050615     C     GCPFAS        ANDeq     35
066700050615     c                   eval      wriape = 'S'
066800050615     c                   else
066900050615     C     *IN30         IFEQ      '0'
067000050615     C     GCPFAS        ANDGT     10
067100050408     C                   SETON                                        28
067200050408     C                   MOVEL     ERR(2)        $MSG
067300050408     C                   GOTO      FINVD1
067400050408     C                   end
067500050615     C                   end
067600940915     C* Se sono in partenza eseguo routine controlli arrivo
067700940915     C                   EXSR      CTRARR
067800940915     C                   END
067900940915     C*  Se ci sono errori vado a fine controlli + accendo ind.
068000940915     C     *IN28         IFEQ      '1'
068100940915     C                   SETON                                        404142
068200940915     C                   ELSE
068300940915     C                   EXSR      INZ02
068400940915     C                   END
068500940915     C*
068600940915     C     FINVD1        ENDSR
068700940919     C*-----------------------------------------------------*
068800940919     C*  Gestione della prima videata
068900940919     C*-----------------------------------------------------*
069000940919     C     GESD02        BEGSR
069100940919     C*
069200941227     C     WRIAPE        COMP      'S'                                2222
069300971112     C* Per arrivo attivo F14 x scrittura note arrivo se estero
069400971112     C                   SETOFF                                       14
069500971112     C     D05GES        IFEQ      'A'
069600980112     C     LNAFL1        ANDEQ     'E'
069700971112     C                   SETON                                        14
069800971112     C                   END
069900941227     C*
070000070705     c     vedibolla     tag
070100950105     C     *IN28         IFEQ      '1'
070200950105     C                   SETOFF                                       28
070300950105     C                   WRITE     LG05D02
070400950105     C                   SETON                                        28
070500950105     C                   END
070600050609     c* se esco dalla subroutine aggiornamento con errori proteggo il
070700050609     c* video
070800050609     c                   if        savind28 = '1'
070900050609     C                   write     LG05D02
071000050609     C                   exfmt     LG05D08
071100050609     c                   else
071200940919     C                   EXFMT     LG05D02
071300050609     c                   end
071400070705      * richiama interrogazione bolle
071500070705     c                   if        *inkg
071600070705     c                   exsr      richbla
071700070705     c                   goto      vedibolla
071800070705     c                   endif
071900090701      * richiama blocco sblocco
072000090701     c                   if        *inki
072100090701     c                   exsr      richblosbl
072200090707     c                   if        arbfbc <> *blank
072300090701     c                   goto      vedibolla
072400090707     c                   else
072500090707     c                   eval      *inkl = *on
072600090707     c                   goto      ritorno
072700090707     c                   endif
072800090701     c                   endif
072900000808      *CONTROLLO SE SUPERATO IL LIMITE DI APERTURE EMETTO VIDEO
073000000808      *SEGNALAZIONE
073100000808     C     D05GES        IFEQ      'A'
073200050629      * se particolarità giacenza = '  ' forzo il caricamento della 'GR'
073300050629     c                   if        arbgga = *blank
073400050629     c                   z-add     1             gr                3 0
073500050629     c     'GR'          lookup    c7q(gr)                                55
073600050629     c   55              movel     d7q(gr)       ds7q
073700050629     c                   endif
073800050629     c* conto quanti record di giacenza ho e quanti di questi sono motivi
073900050629     c* di aperture tecniche
074000050629     c                   exsr      srconta
074100050630     c* se non sto facendo una riapertura tecnica
074200050629     C                   if        v2ccmc <> '999' AND V2CCMC <> ' '
074300100614      * verifico il conteggio solo se per la particolarità è inserito un
074400100614      * valore minimo di aperture altrimenti salto l'emissione delle finestre
074500100614     c                   if        §7q5rp > 0
074600050629     c* ultima riapertura possibile
074700050629     C                   if        (cnt-cntced) = (§7q5rp - 1)
074800050629     C                   EXFMT     LG05D06
074900050629     c                   endif
075000050629     c* reso per numero massimo riaperture
075100050629     C                   if        (cnt-cntced) >= §7q5rp
075200050629     C                   EXFMT     LG05D07
075300050629     c                   endif
075400100614     c                   endif
075500050629     c*
075600050629     C     *INKL         IFEQ      '1'
075700050629     C                   MOVEL     'S'           WFINE
075800050629     C                   GOTO      FINVD2
075900050629     C                   END
076000050629     C                   END
076100050629     C                   END
076200940919     C*                    ------------
076300940919     C                   SETOFF                                       404142
076400940919     C                   SETOFF                                       434445
076500940919     C                   SETOFF                                       462819
076600050609     c*
076700050609     c                   eval      *in28 = savind28
076800940919     C*  Ritorno
076900090707     c     ritorno       tag
077000940919     C     *INKL         IFEQ      '1'
077100940920     C     D05RIC        IFEQ      'S'
077200940919     C                   MOVEL     'S'           WFINE
077300940919     C                   ELSE
077400950224     C                   MOVEL     D05GES        SAVGES
077500940920     C                   MOVEL     '1'           WTPVID
077600950224     C                   CLEAR                   FNLG05
077700950224     C                   MOVEL     SAVGES        D05GES
077800940919     C                   EXSR      PULVID
077900940919     C                   END
078000940919     C                   GOTO      FINVD2
078100940919     C                   END
078200940919     C* Controllo video
078300940919     C                   EXSR      CTR02
078400960123     C* Destinatario originale
078500960123     C     *INKM         IFEQ      '1'
078600960123     C                   MOVEL     '4'           WTPVID
078700960123     C                   GOTO      FINVD2
078800960123     C                   END
078900940919     C* aggiorna archivi
079000940919     C     *IN28         IFEQ      '0'
079100940919     C     *IN19         ANDEQ     '0'
079200050609     c                   eval      savind28  = *off
079300050516     c                   EXSR      GCPWTR
079400050516     c                   if        *in28
079500050609     c                   eval      savind28 = *in28
079600050516     C                   MOVEL     ERR(33)       $MSG
079700050516     c                   goto      finvd2
079800050516     c                   end
079900970624     C* Note di apertura giacenza
080000970626     C     *INKK         IFEQ      '1'
080100970624     C                   EXSR      CALL03
080200970624     C                   END
080300970624     C*
080400060403     C* variazione assegnazione spese/e cambio di porto
080500060403     C     *INKq         IFEQ      '1'
080600060403     C                   MOVEL(p)  kpjbu         savKPJBU
080700060403     C                   MOVEL     'P'           D82GES
080800060403     c                   move      'S'           d82gia
080900060403     C                   MOVE      gcpAGC        D82AGC
081000060403     C                   MOVE      gcpFGC        D82FGC
081100060403     C                   MOVE      gcpNGC        D82NGC
081200060403     C                   MOVE      gcpfrg        D82frg
081300060403     C                   MOVE      'S'           D82comm
081400060403     C                   MOVE      'S'           D82fspe
081500060403     C                   MOVEL(p)  FNLG82        KPJBU
081600060403     C                   CALL      'FNLG10R'
081700060403     C                   PARM                    KPJBA
081800060403     C                   MOVEL     KPJBU         FNLG82
081900060403     C                   MOVEL(p)  savkpjbu      KPJBU
082000060403     C                   END
082100060403     C*
082200940920     C     D05RIC        IFEQ      'S'
082300940920     C                   MOVEL     'S'           WFINE
082400940920     C                   ELSE
082500940920     C                   MOVEL     '1'           WTPVID
082600950224     C                   MOVEL     D05GES        SAVGES
082700950224     C                   CLEAR                   FNLG05
082800950224     C                   MOVEL     SAVGES        D05GES
082900940920     C                   EXSR      PULVID
083000940920     C                   END
083100940920     C                   END
083200940919     C*
083300940919     C     FINVD2        ENDSR
083400960123     C*-----------------------------------------------------*
083500050629     c* conto quanti record di giacenza ho e quanti di questi sono motivi
083600050629     c* di aperture tecniche
083700960123     C*-----------------------------------------------------*
083800050629     C     srconta       BEGSR
083900960123     C*
084000050630     c                   clear                   fnlg09ds
084100050630     c                   eval      d09tla = 'R'
084200050630     c                   eval      d09aas = kaas
084300050630     c                   eval      d09lnp = klnp
084400050630     c                   eval      d09nrs = knrs
084500050630     c                   eval      d09nsp = knsp
084600050630     c                   movel(p)  fnlg09ds      kpjbu
084700050630     C                   CALL      'FNLG09R'
084800050630     C                   PARM                    kpjba
084900050630     c                   movel(p)  kpjbu         fnlg09ds
085000050630     C                   Z-ADD     d09cnt        cnt               2 0
085100050630     C                   Z-ADD     d09ced        cntced            2 0
085200050629     C*
085300050629     C                   ENDSR
085400050629     C*-----------------------------------------------------*
085500050629     C*  Gestione della quarta videata
085600050629     C*-----------------------------------------------------*
085700050629     C     GESD04        BEGSR
085800050629     C*
085900960123     C                   EXFMT     LG05D04
086000960123     C*                    ------------
086100960123     C*  Ritorno
086200960123     C     *INKL         IFEQ      '1'
086300960123     C                   MOVEL     '2'           WTPVID
086400960123     C                   END
086500960123     C*
086600960123     C                   ENDSR
086700961025     C*------------------------------------------------------------*
086800961025     C*  Gestione QUINTA videata
086900961025     C*------------------------------------------------------------*
087000961025     C     GESD05        BEGSR
087100961025     C*
087200961025     C                   EXFMT     LG05D05
087300961025     C                   SETOFF                                       2840
087400961025     C* Ritorno
087500961028     C     *INKL         IFEQ      '1'
087600961025     C                   MOVEL     '1'           WTPVID
087700961028     C                   GOTO      FINVD5
087800961025     C                   END
087900961025     C* Controllo se filiale in £1
088000961025     C                   SETOFF                                       32
088100961025     C     V5CFLE        IFNE      0
088200020430     C                   MOVEL     V5CFLE        W£1               3 0
088300961025     C     W£1           LOOKUP    £1                                     32
088400961025     C                   END
088500961025     C     *IN32         IFEQ      '0'
088600961025     C                   SETON                                        2840
088700961028     C                   MOVEL     ERR(20)       $MSG
088800961025     C                   ELSE
088900961028     C                   Z-ADD     V5CFLE        §FLE
089000961025     C                   EXSR      CAR£6
089100961028     C                   MOVEL     '1'           WTPVID
089200961025     C                   END
089300961025     C*
089400961028     C     FINVD5        ENDSR
089500940915     C*-----------------------------------------------------*
089600940915     C*  Inizializzo seconda videata
089700940915     C*-----------------------------------------------------*
089800940915     C     INZ02         BEGSR
089900940919     C                   MOVEL     '2'           WTPVID
090000940919     C                   SETOFF                                       404142
090100940919     C                   SETOFF                                       434445
090200940919     C                   SETOFF                                       4628
090300940921     C                   SETOFF                                       050660
090400940915     C*  Se non ci sono errori passo alle videate successive
090500021031     c                   clear                   forzato           1
090600940919     C     D05GES        IFEQ      'P'
090700940915     C                   EXSR      BLPVID
090800940915     C                   ELSE
090900940915     C                   EXSR      ARBVID
091000940915     C                   END
091100040323     c     forzato       caseq     'X'           inipgm
091200040323     c                   end
091300940915     C* Se esiste giacenza carico i dati a video
091400940915     C     *IN30         IFEQ      '0'
091500940915     C                   EXSR      GCPVID
091600940915     C                   END
091700950301     C* Reperisco tipo mod.comunicaz. apertura giacenza
091800950301     C     D05GES        IFEQ      'P'
091900950301     C                   EXSR      APEGIA
092000050608     c                   else
092100050608     c                   clear                   flgagg            1 0
092200050608     C                   EXSR      APEGIAa
092300950301     C                   END
092400950301     C*
092500940919     C                   EXSR      GENVID
092600940915     C*
092700040323     C     endrut        ENDSR
092800940915     C*-----------------------------------------------------*
092900940915     C*  Controlli PARTENZA
093000940915     C*-----------------------------------------------------*
093100940915     C     CTRPAR        BEGSR
093200940915     C* PARTENZA !!!!!!
093300961204     C     KSPE1         CHAIN(N)  FNBLP01L                           31
093400940915     C     *IN31         IFEQ      '1'
093500940915     C                   SETON                                        28
093600940915     C                   MOVEL     ERR(6)        $MSG
093700940915     C                   GOTO      FINPAR
093800940915     C                   END
093900950217     C     BLPLNP        LOOKUP    KEY                                    32
094000940915     C* Profilo di 2° livello: Lnp deve essere ug. alla fil del prof.
094100940915     C*                         oppure non deve essere in £7
094200940922     C     D05RIC        IFNE      'S'
094300950217     C     *IN32         ANDEQ     '0'
094400940915     C* Profilo di 1° livello: deve essere in £7
094500940915     C                   SETON                                        28
094600940921     C                   MOVEL     ERR(16)       $MSG
094700940915     C                   GOTO      FINPAR
094800940922     C                   END
094900940915     C* Se sono in partenza non è ammessa l'apertura giacenza
095000940915     C     *IN30         IFEQ      '1'
095100940915     C                   SETON                                        28
095200940915     C                   MOVEL     ERR(3)        $MSG
095300940915     C                   GOTO      FINPAR
095400940915     C                   END
095500050217     C* Se esiste giacenza controllo se posso modificare stato
095600940915     C     GCPFAS        IFGE      20
095700940915     C                   SETON                                        28
095800940915     C                   MOVEL     ERR(5)        $MSG
095900940915     C                   GOTO      FINPAR
096000940915     C                   END
096100940915     C*
096200940915     C     FINPAR        ENDSR
096300940915     C*-----------------------------------------------------*
096400940915     C*  Controlli ARRIVO
096500940915     C*-----------------------------------------------------*
096600940915     C     CTRARR        BEGSR
096700940915     C* ARRIVO !!!!!!!!
096800141029     C     KSPE1         CHAIN(n)  FNARB01L                           3131
096900990203     C     *IN31         IFEQ      '1'
097000940915     C                   SETON                                        28
097100940915     C                   MOVEL     ERR(6)        $MSG
097200940915     C                   GOTO      FINARR
097300940915     C                   END
097400040113      * se da menu non si può aprire giacenza con particolarità C6
097500140410     c                   if        d05ric = ' ' and arbgma <> *blank
097600140410     c                   move      '7R'          kcod
097700140410     c                   movel(p)  arbgma        kkey
097800140410     c     ktbl1         chain     tabel00f
097900140410     c                   if        %found(tabel00f)
098000140410     c                   movel     tbluni        ds7r
098100140410     c                   if        §7rag = 'N'
098200040113     C                   SETON                                        28
098300040113     C                   MOVEL     ERR(29)       $MSG
098400040113     C                   GOTO      FINARR
098500140410     C                   endif
098600140410     C                   endif
098700040113     C                   END
098800040402      * se da menu non si può aprire giacenza se arbcmc valorizzato
098900040402     C                   IF        d05ric= ' ' and arbcmc <> *blank
099000040402     C                   SETON                                        28
099100040402     C                   MOVEL     ERR(31)       $MSG
099200040402     C                   GOTO      FINARR
099300040402     C                   END
099400940915     C* Se bolla di rientro non posso aprire giacenza
099500940915     C     ARBFBR        IFEQ      'S'
099600940915     C                   SETON                                        28
099700940915     C                   MOVEL     ERR(7)        $MSG
099800940915     C                   GOTO      FINARR
099900940915     C                   END
100000940915     C* Bolla con consegna parziale impossibile aprire giacenza
100100940915     C     ARBDCP        IFGT      0
100200940915     C                   SETON                                        28
100300940915     C                   MOVEL     ERR(8)        $MSG
100400940915     C                   GOTO      FINARR
100500940915     C                   END
100600940915     C* Bolla in consegna
100700940915     C     ARBNDC        IFNE      0
100800940915     C                   SETON                                        28
100900940915     C                   MOVEL     ERR(9)        $MSG
101000940915     C                   GOTO      FINARR
101100940915     C                   END
101200950228     C*
101300950228     C     ARBDCM        IFNE      0
101400950228     C                   SETON                                        28
101500950228     C                   MOVEL     ERR(17)       $MSG
101600950228     C                   GOTO      FINARR
101700950228     C                   END
101800940922     C* Profilo di 2° livello: Lnp deve essere ug. alla fil del prof.
101900940922     C*                         oppure non deve essere in £7
102000950217     C     ARBLNA        LOOKUP    KEY                                    32
102100940922     C     D05RIC        IFNE      'S'
102200950217     C     *IN32         ANDEQ     '0'
102300940922     C                   SETON                                        28
102400940922     C                   MOVEL     ERR(16)       $MSG
102500940922     C                   GOTO      FINARR
102600940922     C                   END
102700940915     C* Se la bolla digitata è una figlia con linea d'arrivo uguale
102800940915     C* alla bolla mamma mando messaggio (solo per arrivo)
102900050217     c                   clear                   fnlblds
103000050217     c                   clear                   lblsav
103100940919     C     KSPE1         CHAIN     FNLBL01L                           31
103200940915     C     *IN31         IFEQ      '0'
103300050217     C     LBLLAN        ifeq      LBLLAP
103400940915     C                   SETON                                        28
103500940915     C                   MOVEL     ERR(4)        $MSG
103600940915     C                   GOTO      FINARR
103700940915     C                   END
103800050217     c                   eval      lblsav = fnlblds
103900050217     C                   END
104000050517     c* controllo che esita la bolla in partenza
104100050517     c                   if        £LBLAAO = 0
104200050517     c     kspe1         chain(n)  fnblp01l
104300050517     c                   else
104400050517     c     kblp          chain(n)  fnblp01l
104500050517     c                   end
104600050517     C                   IF        not %found(fnblp01l)
104700050517     C                   SETON                                        28
104800050517     C                   MOVEL     ERR(6)        $MSG
104900050517     C                   GOTO      FINARR
105000050517     C                   END
105100940915     C* Se esiste giacenza ed è già trasmessa non ammessa manuten.
105200050217     C  N30GCPfas        IFgt      10
105300941223     C     WRIAPE        ANDEQ     ' '
105400940915     C                   SETON                                        28
105500940915     C                   MOVEL     ERR(2)        $MSG
105600940915     C                   GOTO      FINARR
105700940915     C                   END
105800050217      * Controllo c.a sia come mamma che come figlia                         ALL
105900040716     c                   clear                   fidn12ds
106000040716     c                   eval      i12aas = kaas
106100040716     c                   eval      i12lnp = klnp
106200040716     c                   eval      i12nrs = knrs
106300040716     c                   eval      i12nsp = knsp
106400040716     c                   eval      i12fel = 'E'
106500040716     c                   eval      i12fan = 'E'
106600040716     c                   eval      i12fch = 'E'
106700040716     c                   call      'FIDN12R'
106800040716     c                   parm                    fidn12ds
106900040716      **
107000040716      * se c'è almeno una C.A. aperta
107100040716      **
107200040716     c     o12nca        ifgt      0
107300990203     C                   SETON                                        28
107400990203     C                   MOVEL     ERR(25)       $MSG
107500990203     C                   GOTO      FINARR
107600990203     C                   END
107700141029      **
107800141029      * se c'è almeno una disposizione di consegna
107900141029      **
108000141029     c                   if        d05ric = *blank
108100141029      *  dirottamento
108200141029     c                   clear                   fnlry09ds2
108300141029     c                   eval      ilry09tdis= 'D'
108400151204     c                   eval      ilry09tch = 'T'
108500141029     c                   eval      ilry09aas = KAAS
108600141029     c                   eval      ilry09lnp = KLNP
108700141029     c                   eval      ilry09nrs = KNRS
108800141029     c                   eval      ilry09nsp = KNSP
108900141029     c                   eval      ilry09fgs = §fle
109000141029     c                   movel     fnlry09ds2    kpjbu
109100141029     c                   call      'FNLRY09R'
109200141029     c                   parm                    kpjba
109300141029     c                   movel     kpjbu         fnlry09ds2
109400141029     c                   if        olry09err = *blank and
109500141029     c                             olry09esdi = '1'
109600141029     c                   seton                                        28
109700141029     C                   MOVEL     ERR(37)       $MSG
109800141029     C                   GOTO      FINARR
109900141029     c                   endif
110000141029      *  altro
110100141029     c                   eval      ilry09tdis= 'E'
110200141029     c                   eval      ilry09tch = 'E'
110300141029     c                   movel     fnlry09ds2    kpjbu
110400141029     c                   call      'FNLRY09R'
110500141029     c                   parm                    kpjba
110600141029     c                   movel     kpjbu         fnlry09ds2
110700141029     c                   if        olry09err = *blank and
110800141029     c                             olry09esal = '1'
110900141029     c                   seton                                        28
111000141029     C                   MOVEL     ERR(36)       $MSG
111100141029     C                   GOTO      FINARR
111200141029     c                   endif
111300141029     c                   endif
111400940915     C*
111500940915     C     FINARR        ENDSR
111600940915     C*-----------------------------------------------------*
111700940915     C*  Controlli sulla giacenza
111800940915     C*-----------------------------------------------------*
111900940915     C     CTRGIA        BEGSR
112000050411     c                   clear                   tigcp000
112100940915     C*
112200050315     C     KSPE2         CHAIN(e)  tIGCP01L                           30
112300050315     c                   if        %error
112400070521     C     KSPE2         CHAIN(n)  tIGCP01L
112500050315     c                   exsr      geslck
112600050318     C     KSPE2         CHAIN(e)  tIGCP01L                           30
112700050315     c                   endif
112701170922      *per rekord trovato salvo il dato attuale per monitor avanzamento
112702170922     c  n30              eval      tigcpb = tigcp
112800940919     C* Se la giacenza esiste già e il programma è richiamato
112900940919     C* gestisco devo effettuare una riapertura
113000940915     C     *IN30         IFEQ      '0'
113100940919     C     D05GES        ANDEQ     'A'
113200960626     C                   Z-ADD     DATEU         G02INV
113300960626     C                   MOVEL     '3'           G02ERR
113400960626     C                   Z-ADD     0             G02DAT
113500960626     C                   CALL      'XSRDA8'
113600960626     C                   PARM                    WLBDA8
113700960626     C                   MOVE      G02DAT        V2CDUR
113800940920     C                   MOVEL     'S'           WRIAPE            1
113900940915     C                   END
114000940915     C* Se sono in partenza eseguo routine controlli partenza
114100940919     C     D05GES        IFEQ      'P'
114200940915     C                   EXSR      CTRPAR
114300940915     C                   ELSE
114400940915     C* Se sono in partenza eseguo routine controlli arrivo
114500940915     C                   EXSR      CTRARR
114600940920     C                   END
114601170530     C     *IN30         IFEQ      '0'
114602170530     C     D05GES        ANDEQ     'A'
114603170530     C     gcpfas        ANDlt     35
114604170530     c                   seton                                        28
114605170530     C                   MOVEL     ERR(40)       $MSG
114606170530     c                   endif
114700940915     C*  Se ci sono errori vado a fine controlli + accendo ind.
114800940915     C     *IN28         IFEQ      '1'
114900940915     C                   Z-ADD     D05AAS        V3CAAS
115000940915     C                   Z-ADD     D05LNP        V3CLNP
115100940915     C                   Z-ADD     D05NRS        V3CNRS
115200940915     C                   Z-ADD     D05NSP        V3CNSP
115300940915     C     *IN30         IFEQ      '0'
115400940915     C                   Z-ADD     GCPAGC        V3CAGC
115500940915     C                   Z-ADD     GCPFGC        V3CFGC
115600940915     C                   Z-ADD     GCPNGC        V3CNGC
115700940915     C                   END
115800940915     C                   EXFMT     LG05D03
115900940919     C                   MOVEL     'S'           WFINE
116000940915     C                   ELSE
116100940915     C                   EXSR      INZ02
116200940915     C                   END
116300920409     C*
116400940915     C                   ENDSR
116500940920     C*-----------------------------------------------------*
116600940920     C*  Routine per gestione richiamo interrogazione bolle
116700940920     C*-----------------------------------------------------*
116800940920     C     RICHBL        BEGSR
116900940920     C*
117000161114     C**   D05GES        IFEQ      'P'
117100161114     C**                 Z-ADD     V1CAAS        PA0AAS
117200161114     C**                 Z-ADD     V1CLNP        PA0LNP
117300161114     C**                 Z-ADD     V1CNRS        PA0NRS
117400161114     C**                 Z-ADD     V1CNSP        PA0NSP
117500161114     C**                 MOVEL     ' '           PA0F03
117600161114     C**                 MOVEL     ' '           PA0FLG
117700161114     C**                 MOVEL     PARAM0        KPJBU
117800161114     C**   PA0NSP        IFGT      0
117900161114     C**                 MOVEL     '1'           PA0FLG
118000161114     C**                 CALL      'FNLS05R'
118100161114     C**                 PARM                    KPJBA
118200161114     C**                 ELSE
118300161114     C**                 CALL      'FNLS04R'
118400161114     C**                 PARM                    KPJBA
118500161114     C**                 END
118600161114     C**                 MOVEL     KPJBU         PARAM0
118700161114     C**   PA0F03        IFNE      *BLANKS
118800161114     C**                 Z-ADD     PA0AAS        V1CAAS
118900161114     C**                 Z-ADD     PA0LNP        V1CLNP
119000161114     C**                 Z-ADD     PA0NRS        V1CNRS
119100161114     C**                 Z-ADD     PA0NSP        V1CNSP
119200161114     C**                 END
119300161114     C**                 ELSE
119400161114     C**                 Z-ADD     V1CAAS        PA1AAS
119500161114     C**                 Z-ADD     V1CLNP        PA1LNP
119600161114     C**                 Z-ADD     V1CNRS        PA1NRS
119700161114     C**                 Z-ADD     V1CNSP        PA1NSP
119800161114     C**                 Z-ADD     V1CNSP        PA1NSP
119900161114     C**                 MOVEL     ' '           PA1F03
120000161114     C**                 MOVEL     '2'           PA1FLG
120100161114     C**   PA1NSP        IFGT      0
120200161114     C**                 MOVEL     '1'           PA1FLG
120300161114     C**                 END
120400161114     C**                 MOVEL     PARAM1        KPJBU
120500161114     C**                 CALL      'FNLR36R'
120600161114     C**                 PARM                    KPJBA
120700161114     C**                 MOVEL     KPJBU         PARAM1
120800161114     C**   PA1F03        IFNE      '1'
120900161114     C**                 Z-ADD     PA1AAS        V1CAAS
121000161114     C**                 Z-ADD     PA1LNP        V1CLNP
121100161114     C**                 Z-ADD     PA1NRS        V1CNRS
121200161114     C**                 Z-ADD     PA1NSP        V1CNSP
121300161114     C**                 END
121400161114     C**                 END
121500161114     C     V1CNSP        IFGT      0
121600161114     C                   CLEAR                   paramu6ds1
121700161114     C                   Z-ADD     V1CAAS        pa1AAS_
121800161114     C                   Z-ADD     V1CLNP        Pa1LNP_
121900161114     C                   Z-ADD     V1CNRS        Pa1NRS_
122000161114     C                   Z-ADD     V1CNSP        Pa1NSP_
122100161118     C                   eval      PA1tla_='L'
122200161114     C                   MOVEL     paramu6ds1    KPJBU
122300161114     C                   CALL      'FNLRU6R1'
122400161114     C                   PARM                    KPJBA
122500161124     C                   eval      paramu6ds1=KPJBU
122600161124     c                   if        pa1err_ ='1'
122700161124     c                   seton                                        28
122800161124     c                   movel     err(6)        $msg
122900161124     c                   endif
123000161114     C*
123100161114     C                   ELSE
123200161114     C*
123300161114     C                   CLEAR                   PARAmu6ds
123400161118     c                   clear                   fnlru6ds
123500161114     C                   MOVEL     '2'           Pa1FLG
123600161114     C                   MOVEL     PARAmu6ds     KPJBU
123700161118     c                   if        d05ges='P'
123800161118     c                   eval      ilru6lp1=§fle
123900161118     c                   else
124000161118     c                   eval      ilru6lna=§fle
124100161118     c                   endif
124200161114     C                   CALL      'FNLRU6R'
124300161114     C                   PARM                    KPJBA
124400161118     c                   parm                    fnlru6ds
124500161114     C                   MOVEL     KPJBU         PARAMu6ds
124600161114     C     Pa1F03        IFEQ      *BLANKS
124700161114     C                   Z-ADD     Pa1AAS        V1CAAS
124800161114     C                   Z-ADD     Pa1LNP        V1CLNP
124900161114     C                   Z-ADD     Pa1NRS        V1CNRS
125000161114     C                   Z-ADD     Pa1NSP        V1CNSP
125100161114     C                   ENDIF
125200161114     C                   ENDIF
125300940920     C*
125400940920     C                   ENDSR
125500070705     C**************************************************************************
125600070705     C*  Routine per gestione richiamo interrogazione bolle arrivo
125700070705     C**************************************************************************
125800070705     C     richbla       begsr
125900070705      *
126000070705     c                   movel     kpjbu         §kpjbu
126100070705      *
126200161114     c*                  z-add     v2caas        pa1aas
126300161114     c*                  z-add     v2clnp        pa1lnp
126400161114     c*                  z-add     v2cnrs        pa1nrs
126500161114     c*                  z-add     v2cnsp        pa1nsp
126600161114     c*                  movel     ' '           pa1f03
126700161114     c*                  movel     '1'           pa1flg
126800161114     c*                  movel     param1        kpjbu
126900161114     c*                  call      'FNLR36R'
127000161114     c*                  parm                    kpjba
127100161114     c*                  movel     kpjbu         param1
127200161114     c                   clear                   paramu6ds1
127300161114     C                   Z-ADD     V2cAAS        Pa1AAS_
127400161114     C                   Z-ADD     V2cLNP        Pa1LNP_
127500161114     C                   Z-ADD     V2cNRS        Pa1NRS_
127600161114     C                   Z-ADD     V2cNSP        Pa1NSP_
127700161118     C                   eval      PA1tla_='L'
127800161114     C                   MOVEL     paramu6ds1    KPJBU
127900161114     C                   CALL      'FNLRU6R1'
128000161114     C                   PARM                    KPJBA
128100070705      *
128200070705     c                   movel     §kpjbu        kpjbu
128300070705      *
128400070705     c                   endsr
128500090701     C**************************************************************************
128600090701     C*  Routine per gestione BLOCCO SBLOCCO SPEDIZIONE
128700090701     C**************************************************************************
128800090701     C     richblosbl    begsr
128900090701     c                   movel     kpjbu         §kpjbu
129000090709     c                   unlock    fnarb01l
129100090709     c                   if        arbfbc = 'A'
129200090709     c                   clear                   fnlrg2ds
129300090709     C                   Z-ADD     kaas          ILRG2AAS
129400090709     C                   Z-ADD     klnp          ILRG2LNP
129500090709     C                   Z-ADD     knrs          ILRG2NRS
129600090709     C                   Z-ADD     knsp          ILRG2NSP
129700090709     C                   MOVE      'L'           ILRG2CAU
129800090709     C                   CALL      'FNLRG2R'
129900090709     C                   PARM                    KPJBA
130000090709     c                   parm                    fnlrg2ds
130100090709     c                   else
130200090701     c                   clear                   dslr02
130300090701     C                   Z-ADD     kaas          DR2AAS
130400090701     C                   Z-ADD     klnp          DR2LNP
130500090701     C                   Z-ADD     knrs          DR2NRS
130600090701     C                   Z-ADD     knsp          DR2NSP
130700090701     C                   MOVEL     '1'           DR2RIC
130800090701     C                   Z-ADD     arblna        DR2FGS
130900090701     C                   MOVEL     DSLR02        KPJBU
131000090701     C                   CALL      'FNLR39R'
131100090701     C                   PARM                    KPJBA
131200090701     c                   movel     kpjbu         dslr02
131300090709     c                   endif
131400090707     c                   movel     §kpjbu        kpjbu
131500090701     c     kspe1         chain     fnarb01l
131600090701      *
131700090701     C                   ENDSR
131800900418     C*---------------------------------------------------------------*
131900900418     C*   P U L V I D                                                 *
132000900418     C*---------------------------------------------------------------*
132100900418     C     PULVID        BEGSR
132200950224     C*
132300961001     C                   SETOFF                                       33
132400950104     C                   CLEAR                   DS7Q
132500950104     C                   MOVEL     ' '           WTPGIA
132600941223     C                   MOVEL     ' '           WRIAPE            1
132700940915     C                   MOVEL     *ZEROS        V1CNRS
132800940915     C                   MOVEL     *ZEROS        V1CNSP
132900940920     C                   MOVE      WHHDAT        V1CAAS
133000940915     C                   MOVEL     *ZEROS        V1CLNP
133100940915     C                   MOVEL     *ZEROS        V2CAAS
133200940915     C                   MOVEL     *ZEROS        V2CLNP
133300940915     C                   MOVEL     *ZEROS        V2CNRS
133400940915     C                   MOVEL     *ZEROS        V2CNSP
133500960123     C                   MOVEL     *ZEROS        V4CNSP
133600960123     C                   MOVEL     *ZEROS        V4CAGC
133700960123     C                   MOVEL     *ZEROS        V4CFGC
133800960123     C                   MOVEL     *ZEROS        V4CNGC
133900940915     C                   MOVEL     *ZEROS        V2CDSP
134000940915     C                   MOVEL     *BLANK        V2CTBL
134100940915     C                   MOVEL     *BLANK        V2DTBL
134200940915     C                   MOVEL     *ZEROS        V2CLNA
134300940915     C                   MOVEL     *BLANK        V2DLNA
134400940919     C                   MOVEL     *BLANK        V2DNAS
134500940915     C                   MOVEL     *BLANK        V2CNOT
134600940915     C                   MOVEL     *BLANK        V2CNO2
134700991119     C                   MOVEL     *BLANK        V2CDIV
134800940915     C                   Z-ADD     *ZEROS        V2CIFT
134900940915     C                   Z-ADD     *ZEROS        V2CNCL
135000940915     C                   Z-ADD     *ZEROS        V2CVLF
135100940915     C                   Z-ADD     *ZEROS        V2CPKF
135200940915     C                   Z-ADD     *ZEROS        V2CCAS
135300940915     C                   MOVEL     *BLANK        V2CRSD
135400940915     C                   MOVEL     *BLANK        V2CVCA
135500940915     C                   MOVEL     *BLANK        V2CDE2
135600940915     C                   MOVEL     *BLANK        V2CIND
135700940915     C                   MOVEL     *BLANK        V2CLOD
135800940915     C                   MOVEL     *BLANK        V2CPRD
135900940915     C                   MOVEL     *ZEROS        V2CCAD
136000940915     C                   MOVEL     *ZEROS        V2CNZD
136100960123     C                   MOVEL     *BLANK        V4CRSD
136200960123     C                   MOVEL     *BLANK        V4CDE2
136300960123     C                   MOVEL     *BLANK        V4CIND
136400960123     C                   MOVEL     *BLANK        V4CLOD
136500960123     C                   MOVEL     *BLANK        V4CPRD
136600960123     C                   MOVEL     *ZEROS        V4CCAD
136700960123     C                   MOVEL     *BLANK        V2CVCA
136800960123     C                   MOVEL     *ZEROS        V2CNZD
136900940919     C                   MOVEL     *ZEROS        V2CNZM
137000940915     C                   MOVEL     *ZEROS        V2CSCM
137100940915     C                   MOVEL     *ZEROS        V2CRMN
137200940915     C                   MOVEL     *BLANK        V2CRMA
137300940915     C                   MOVEL     *BLANK        V2CRSM
137400940915     C                   MOVEL     *BLANK        V2CINM
137500940915     C                   MOVEL     *BLANK        V2CLOM
137600940915     C                   MOVEL     *BLANK        V2CPRM
137700940915     C                   MOVEL     *ZEROS        V2CCAM
137800940919     C                   MOVEL     *BLANKS       V2CTCM
137900940919     C                   MOVEL     *BLANKS       V2DTCM
138000940915     C                   MOVEL     *BLANK        V2CCMC
138100940915     C                   MOVEL     *BLANK        V2DCMC
138200940915     C                   MOVEL     *BLANK        V2CDMC
138300940915     C                   MOVEL     *ZEROS        V2CCCR
138400940915     C                   MOVEL     *BLANK        V2DCCR
138500940915     C                   MOVEL     *ZEROS        V2CNRC
138600940915     C                   MOVEL     *ZEROS        V2CDXD
138700940915     C                   MOVEL     *ZEROS        V2CDTG
138800940915     C                   MOVEL     *ZEROS        V2CDUR
138900940915     C                   MOVEL     *ZEROS        V2CNGC
139000940915     C                   MOVEL     *ZEROS        V2CFGC
139100940915     C                   MOVEL     *BLANKS       V2DFGC
139200940920     C                   MOVEL     'N'           V2CFST
139300940920     C                   MOVEL     *BLANKS       V2CMSP
139400940920     C                   MOVEL     *BLANKS       V2CMSD
139500950104     C                   MOVEL     *BLANKS       V2CEVE
139600950104     C                   MOVEL     *BLANKS       WFEL              3 0
139700080604     c                   clear                   contrdest         1
139800940915     C*
139900900418     C                   ENDSR
140000940915     C*---------------------------------------------------------------*
140100940915     C*   G E N V I D                                                 *
140200940915     C*---------------------------------------------------------------*
140300940915     C     GENVID        BEGSR
140400940915     C*
140500941003     C                   MOVEL     ' '           V2CRPG
140600940921     C                   Z-ADD     KAAS          V2CAAS
140700940921     C                   Z-ADD     KLNP          V2CLNP
140800940921     C                   Z-ADD     KNRS          V2CNRS
140900940921     C                   Z-ADD     KNSP          V2CNSP
141000960123     C                   Z-ADD     KAAS          V4CAAS
141100960123     C                   Z-ADD     KLNP          V4CLNP
141200960123     C                   Z-ADD     KNRS          V4CNRS
141300960123     C                   Z-ADD     KNSP          V4CNSP
141400960123     C                   Z-ADD     KAAS          V4CAAS
141500960123     C                   Z-ADD     KLNP          V4CLNP
141600960123     C                   Z-ADD     KNRS          V4CNRS
141700960123     C                   Z-ADD     KNSP          V4CNSP
141800940915     C* Decodifico linea di arrivo
141900940915     C                   MOVEL     V2CLNA        KFIL
142000940915     C                   MOVEL     *ALL'?'       V2DLNA
142100940915     C     KFIL          CHAIN     AZORG01L                           31
142200940915     C     *IN31         IFEQ      '0'
142300940915     C     ORGFVA        ANDNE     '*'
142400940920     C                   MOVEL     *BLANKS       V2DLNA
142500940915     C                   MOVEL     ORGDES        V2DLNA
142600940915     C                   END
142700940915     C* Decodifico codice mancata consegna
142800950111     C                   MOVEL     *BLANKS       V2DCMC
142900940915     C     V2CCMC        IFNE      *BLANK
143000940915     C                   Z-ADD     1             X
143100940915     C     V2CCMC        LOOKUP    C2A(X)                                 32
143200940920     C     *IN32         IFEQ      '1'
143300950105     C     F2A(X)        ANDEQ     ' '
143400940920     C                   MOVEL     D2A(X)        V2DCMC
143500940919     C                   END
143600940920     C                   END
143700940919     C* Decodifico luogo deposito merce
143800940919     C                   MOVEL     *ALL'?'       V2DCCR
143900940919     C                   Z-ADD     1             X
144000940919     C     V2CCCR        LOOKUP    C2B(X)                                 32
144100940919     C     *IN32         IFEQ      '1'
144200940920     C                   MOVEL     *BLANKS       V2DCCR
144300940919     C                   MOVEL     D2B(X)        V2DCCR
144400940919     C                   ELSE
144500940919     C* Se non lo trovo in tab. corrispon. lo cerco nell'organigramma
144600940919     C                   MOVE      V2CCCR        KFIL
144700940919     C     KFIL          CHAIN     AZORG01L                           31
144800940919     C     *IN31         IFEQ      '0'
144900940919     C     ORGFVA        ANDEQ     ' '
145000940919     C                   MOVEL     *BLANKS       V2DCCR
145100940919     C                   MOVEL     ORGDES        V2DCCR
145200940919     C                   END
145300940919     C                   END
145400980112     C*  Controllo se lnp della giacenza è estera
145500980112     C                   MOVEL     *BLANKS       ORGFL1
145600980112     C                   MOVE      V2CLNP        KFIL
145700980112     C     KFIL          CHAIN     AZORG01L                           31
145800020729     C                   MOVEL     *blank        LNPFL1            1
145900020729     C                   clear                   OG143
146000020729     C  n31              MOVEL     ORGDE3        OG143
146100020729     C  N31§OgNTW        ifeq      'FED'
146200020729     C     §OgNTW        oreq      'EEX'
146300020729     C     §OgNTW        oreq      'EUP'
146400020729     C                   MOVEL     'E'           LNPFL1
146500020729     c                   end
146600040310     C                   clear                   wntwlnp           3
146700040310     C  N31§OgNTW        ifeq      'DPD'
146800040310     C                   MOVEL     §ogntw        wntwlnp
146900040310     c                   end
147000940919     C* Mi salvo i dati del mittente per scrivere o no il
147100940919     C* file d'appoggio
147200940919     C                   MOVEL     V2CSCM        SAVSCM
147300940919     C                   MOVEL     V2CRSM        SAVRSM
147400940919     C                   MOVEL     V2CINM        SAVINM
147500940919     C                   MOVEL     V2CLOM        SAVLOM
147600940919     C                   MOVEL     V2CPRM        SAVPRM
147700940919     C                   MOVEL     V2CCAM        SAVCAM
147800940921     C                   MOVEL     V2CNZM        SAVNZM
147900940919     C* Prelevo comunque num. giacenza anche se la lnp non è meccaniz.
148000940919     C     *IN30         IFEQ      '1'
148100940920     C                   MOVEL     DATEU         KAAA
148200940919     C                   MOVE      ARBLNA        KFGC
148300940919     C     KNUF          CHAIN     FLNUF01L                           31
148400940919     C     *IN31         IFEQ      '0'
148500940919     C                   ADD       1             NUFNUM
148600940919     C                   Z-ADD     NUFNUM        GCPNGC
148700940919     C                   Z-ADD     NUFNUM        V2CNGC
148800940919     C                   EXCEPT    AGGNUF
148900940919     C                   ELSE
149000940919     C* Se non trovato numeratore lo creo
149100940919     C                   Z-ADD     KAAA          NUFAAA
149200940919     C                   Z-ADD     2             NUFCNU
149300940920     C                   Z-ADD     V2CLNA        NUFFIL
149400940919     C                   Z-ADD     DATEU         NUFDAT
149500940919     C                   Z-ADD     0             NUFRIN
149600940919     C                   Z-ADD     0             NUFRFI
149700940919     C                   Z-ADD     1             NUFNUM
149800940919     C                   Z-ADD     NUFNUM        GCPNGC
149900940919     C                   Z-ADD     NUFNUM        V2CNGC
150000940919     C                   WRITE     FLNUF
150100940919     C                   END
150200940919     C* Visualizzo data apertura e filiale
150300940920     C                   MOVE      WHHDAT        V2CDTG
150400940919     C                   END
150500940920     C* Controllo se devo gestire forzatura da apertura
150600950105     C                   SETOFF                                       04
150700940920     C     WFORZA        IFEQ      'S'
150800940920     C                   SETON                                        04
150900940920     C                   END
151000940919     C*
151100940919     C                   ENDSR
151200900418     C*---------------------------------------------------------------*
151300900418     C*   B L P V I D                                                 *
151400900418     C*---------------------------------------------------------------*
151500900418     C     BLPVID        BEGSR
151600940915     C*
151700941003     C                   MOVE      BLPGGA        V2CGGA
151800941003     C                   MOVE      BLPMGS        AMGDSP
151900940919     C                   MOVEL     BLPAAS        AMGDSP
152000940915     C                   MOVE      BLPMGS        WGG
152100940915     C                   MOVEL     BLPMGS        WMM
152200940915     C                   MOVEL     WGGMM         V2CDSP
152300940915     C                   MOVE      BLPAAS        V2CDSP
152400940919     C                   MOVEL     BLPCBO        V2CTBL
152500940915     C                   MOVEL     BLPFST        V2CFST
152600940915     C                   MOVEL     BLPLNA        V2CLNA
152700940919     C                   MOVEL     BLPNAS        V2DNAS
152800940915     C                   Z-ADD     BLPNCL        V2CNCL
152900940915     C                   Z-ADD     BLPVLF        V2CVLF
153000940915     C                   Z-ADD     BLPPKF        V2CPKF
153100940920     C                   MOVE      BLPLNA        V2CCCR
153200940915     C* Tipo spedizione
153300940915     C                   MOVEL     *BLANKS       V2DTSP
153400090520     c                   z-add     1             x
153500090520     C     blptsp        LOOKUP    C5e(X)                                 32
153600090520     C     *IN32         IFEQ      '1'
153700090520     C                   MOVEL     D5e(X)        V2DTsp
153800090520     C                   END
153900940915     C* Decodifico Tipo bolla
154000940915     C                   MOVEL     *BLANKS       V2DTBL
154100940915     C                   MOVEL     *BLANKS       TIPBOL            1
154200950102     C                   Z-ADD     1             X
154300940919     C     V2CTBL        LOOKUP    C3A(X)                                 32
154400940915     C     *IN32         IFEQ      '1'
154500940915     C                   MOVEL     D3A(X)        V2DTBL
154600940919     C                   MOVEL     T3A(X)        TIPBOL            1
154700021031     C                   MOVE      g3A(X)        giacen            1
154800940915     C                   END
154900940919     C* Reperisco importi fatture e C/Assegno
155000990929     C* Se prima bolla è porto Franco aggancio dati tassaz.
155100990929     C* da tipo record 1
155200991001     C     TIPBOL        IFEQ      'F'
155300990929     C                   MOVEL     '1'           KTRC
155400990929     C     KAR6          CHAIN     FIAR601L                           31
155500941223     C     *IN31         IFEQ      '0'
155600990929     C                   Z-ADD     AR6IFT        V2CIFT
155700991119     C                   MOVEL     AR6DIV        V2CDIV
155800941223     C                   END
155900990929     C                   END
156000051116     C     KSPE1         CHAIN     Fiar901L                           31
156100941223     C     *IN31         IFEQ      '0'
156200051116     C                   Z-ADD     ar9CAS        V2CCAS
156300051116     C                   MOVEL     ar9VCA        V2CVCA
156400941223     C                   END
156500960123     C* Controllo se esiste estensione descrizione destinatario
156600040325     C                   MOVEL     'D'           KTRC
156700060213     C                   MOVEL     *BLANKS       ar4NOT
156800060213     C     KAR4          CHAIN(n)  Fiar401L                           31
156900960123     C* Controllo se esiste destinatario finale
157000960123     C                   SETOFF                                       13
157100960123     C                   CLEAR                   FNLSN0
157200960123     C                   Z-ADD     BLPAAS        DN0AAS
157300960123     C                   Z-ADD     BLPLNP        DN0LNP
157400960123     C                   Z-ADD     BLPNRS        DN0NRS
157500960123     C                   Z-ADD     BLPNSP        DN0NSP
157600960123     C                   MOVEL     *BLANKS       DN0TRC
157700960123     C                   MOVEL     KPJBU         WSVKPJ          256
157800960123     C                   MOVEL     FNLSN0        KPJBU
157900960123     C                   CALL      'FNLSN0R'
158000960123     C                   PARM                    KPJBA
158100960123     C                   MOVEL     KPJBU         FNLSN0
158200960123     C                   MOVEL     WSVKPJ        KPJBU
158300960123     C* Destinatario
158400960123     C     DN0TRV        IFEQ      'S'
158500960123     C                   SETON                                        13
158600960123     C                   MOVEL     DN0RSD        V2CRSD
158700960123     C                   MOVEL     DN0RS2        V2CDE2
158800960123     C                   MOVEL     DN0IND        V2CIND
158900960123     C                   MOVEL     DN0LOD        V2CLOD
159000960123     C                   MOVEL     DN0PRD        V2CPRD
159100960123     C                   MOVEL     DN0CAD        V2CCAD
159200960123     C                   MOVEL     DN0NZD        V2CNZD
159300960123     C                   MOVEL     BLPRSD        V4CRSD
159400060213     C                   MOVEL     ar4NOT        V4CDE2
159500960123     C                   MOVEL     BLPIND        V4CIND
159600960123     C                   MOVEL     BLPLOD        V4CLOD
159700960123     C                   MOVEL     BLPPRD        V4CPRD
159800960123     C                   MOVEL     BLPCAD        V4CCAD
159900960123     C                   MOVEL     BLPNZD        V4CNZD
160000960123     C                   ELSE
160100960123     C                   MOVEL     BLPRSD        V2CRSD
160200960123     C                   MOVEL     BLPIND        V2CIND
160300960123     C                   MOVEL     BLPLOD        V2CLOD
160400960123     C                   MOVEL     BLPPRD        V2CPRD
160500960123     C                   MOVEL     BLPCAD        V2CCAD
160600960123     C                   MOVEL     BLPNZD        V2CNZD
160700060213     C                   MOVEL     ar4NOT        V2CDE2
160800960123     C                   END
160900940915     C* Mittente
161000940919     C                   MOVEL     BLPRMA        V2CRMA
161100940919     C                   MOVEL     BLPRMN        V2CRMN
161200940919     C                   MOVEL     BLPRSM        V2CRSM
161300940919     C                   MOVEL     BLPINM        V2CINM
161400940919     C                   MOVEL     BLPLOM        V2CLOM
161500940919     C                   MOVEL     BLPPRM        V2CPRM
161600940919     C                   MOVEL     BLPCAM        V2CCAM
161700940919     C                   MOVEL     BLPNZM        V2CNZM
161800091102     c*verifica se forzato in arrivo per triangolazione nel caso non forza
161900091102     c* il ccm
162000900508     C     BLPCCM        IFNE      *ZEROS
162100940915     C                   MOVEL     BLPCCM        V2CSCM
162200940920     C                   Z-ADD     BLPCCM        KKSC
162300900508     C                   ELSE
162400940915     C                   MOVEL     BLPKSC        V2CSCM
162500940920     C                   Z-ADD     BLPKSC        KKSC
162600900508     C                   END
162700091102     c                   exsr      forzacli
162800920326     C*
162900940915     C                   MOVEL     'N'           WCLIGN            1
163000940915     C                   MOVEL     V2CSCM        §RIC              3
163100920326     C                   Z-ADD     1             I                 2 0
163200940915     C* Note su bolla
163300940915     C                   MOVEL     '8'           KTRC
163400060213     C                   MOVEL     *BLANKS       ar4NOT
163500060213     C     KAR4          CHAIN(n)  Fiar401L                           31
163600060213     C                   MOVEL     ar4NOT        V2CNOT
163700940915     C                   MOVEL     '9'           KTRC
163800060213     C                   MOVEL     *BLANKS       ar4NOT
163900060213     C     KAR4          CHAIN(n)  Fiar401L                           31
164000060213     C                   MOVEL     ar4NOT        V2CNO2
164100940920     C* Carico messaggio particolarità
164200940920     C                   MOVEL     'N'           WFORZA            1
164300940920     C     BLPGGA        IFNE      *BLANKS
164400940920     C                   Z-ADD     1             X
164500940920     C     BLPGGA        LOOKUP    C7Q(X)                                 32
164600940920     C     *IN32         IFEQ      '1'
164700940920     C                   MOVE      D7Q(X)        DS7Q
164800940920     C     §7Q3MF        IFEQ      'A'
164900940920     C     §7Q3MS        ANDNE     *BLANKS
165000940920     C     §7Q3MQ        IFEQ      'P'
165100950105     C                   MOVEL     'N'           WTREVE
165200950105     C                   MOVEL     §7Q3EV        V2CEVE
165300950105     C                   MOVEL     'N'           WFORZA
165400950105     C     §7Q3EV        IFNE      *BLANKS
165500950105     C                   EXSR      CTREVE
165600950105     C                   END
165700950105     C     WTREVE        IFEQ      'N'
165800950104     C                   MOVEL     §7Q3MS        V2CMSP
165900940920     C                   MOVEL     §7Q3MC        WFORZA
166000950104     C                   MOVEL     V2CLNP        WFLE
166100950105     C                   SETON                                        05
166200950105     C                   END
166300940920     C                   ELSE
166400940920     C                   SETON                                        06
166500950104     C                   MOVEL     §7Q3MS        V2CMSD
166600940920     C                   END
166700940920     C                   END
166800940920     C                   END
166900940920     C                   END
167000020222     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" SDE CLIENTE
167100020222     C* CODIFICATO
167200020222     C                   MOVE      KKSC          APPO              4 0
167300020222     C                   CLEAR                   TRUL27
167400020222     C     APPO          IFNE      8888
167500020222     C     APPO          ANDNE     9999
167600020222     C     APPO          ANDNE     *ZEROS
167700020306     C                   MOVEL     D05TRI        I27TLA
167800020222     C                   MOVEL     BLPTSP        I27TSP
167900020222     C                   MOVE      BLPLNA        I27LNA
168000020222     C                   MOVE      BLPLNP        I27LNP
168100021118     C                   MOVE      BLPctr        I27ctb
168200020222     C                   MOVE      KKSC          I27CLI
168300160316     C********           MOVE      AMGDSP        I27DTA
168400160316     C                   CALL      'TRUL27R1'
168500020222     C                   PARM                    TRUL27
168600020222     C                   END
168700940915     C*
168800021031     C                   ENDSR
168900911011     C*---------------------------------------------------------------*
169000911011     C*   A R B V I D                                                 *
169100911011     C*---------------------------------------------------------------*
169200911011     C     ARBVID        BEGSR
169300941003     C*
169400950712     C     D05RIC        IFEQ      'S'
169500950414     C                   MOVEL     'N'           WWTR              1
169600950712     C                   ELSE
169700950712     C                   MOVEL     'S'           WWTR              1
169800950712     C                   END
169900941003     C                   MOVE      ARBGGA        V2CGGA
170000940919     C                   MOVE      ARBMGS        AMGDSP
170100940919     C                   MOVEL     ARBAAS        AMGDSP
170200940915     C                   MOVE      ARBMGS        WGG
170300940915     C                   MOVEL     ARBMGS        WMM
170400940915     C                   MOVEL     WGGMM         V2CDSP
170500940915     C                   MOVE      ARBAAS        V2CDSP
170600940919     C                   MOVEL     ARBCBO        V2CTBL
170700940915     C                   MOVEL     ARBLNA        V2CLNA
170800940919     C                   MOVEL     ARBNAS        V2DNAS
170900940915     C                   Z-ADD     ARBNCL        V2CNCL
171000940915     C                   Z-ADD     ARBVLF        V2CVLF
171100940915     C                   Z-ADD     ARBPKF        V2CPKF
171200940920     C                   MOVE      ARBLNA        V2CCCR
171300940919     C* Tipo spedizione
171400940915     C                   MOVEL     *BLANKS       V2DTSP
171500090520     c                   z-add     1             x
171600090520     C     arbtsp        LOOKUP    C5e(X)                                 32
171700090520     C     *IN32         IFEQ      '1'
171800090520     C                   MOVEL     D5e(X)        V2DTsp
171900090520     C                   END
172000090520     c*
172100950224     C     ARBFFD        IFEQ      'S'
172200950224     C     D05CMC        IFEQ      *BLANKS
172300950224     C     D05AAF        ANDNE     *ZEROS
172400950224     C     ARBCMC        OREQ      *BLANKS
172500950224     C     D05AAF        ANDEQ     *ZEROS
172600940922     C                   MOVE      9999999       GCPNRC
172700940922     C                   MOVE      9999999       V2CNRC
172800940922     C                   END
172900950224     C                   END
173000940915     C* Decodifico Tipo bolla
173100940915     C                   Z-ADD     1             X
173200940915     C                   MOVEL     *BLANKS       V2DTBL
173300940919     C     V2CTBL        LOOKUP    C3A(X)                                 32
173400940915     C     *IN32         IFEQ      '1'
173500940915     C                   MOVEL     D3A(X)        V2DTBL
173600990929     C                   MOVEL     T3A(X)        TIPBOL
173700021031     C                   MOVE      g3A(X)        giacen            1
173800940915     C                   END
173900940919     C* Reperisco importi FAttura e C/Assegno
174000990929     C     TIPBOL        IFEQ      'A'
174100990929     C                   MOVEL     '1'           KTRC
174200990929     C     KAR6          CHAIN     FIAR601L                           31
174300941223     C     *IN31         IFEQ      '0'
174400940915     C                   Z-ADD     AR6IFT        V2CIFT
174500991119     C                   MOVEL     AR6DIV        V2CDIV
174600941223     C                   END
174700990929     C                   END
174800051116     C     KSPE1         CHAIN     FIAR901L                           31
174900941223     C     *IN31         IFEQ      '0'
175000940915     C                   Z-ADD     AR9CAS        V2CCAS
175100940915     C                   MOVEL     AR9VCA        V2CVCA
175200941223     C                   END
175300940915     C* Destinatario
175400940915     C                   MOVEL     ARBRSD        V2CRSD
175500940915     C                   MOVEL     ARBIND        V2CIND
175600940915     C                   MOVEL     ARBLOD        V2CLOD
175700940915     C                   MOVEL     ARBPRD        V2CPRD
175800940915     C                   MOVEL     ARBCAD        V2CCAD
175900940915     C                   MOVEL     ARBNZD        V2CNZD
176000940915     C* Controllo se esiste estensione descrizione destinatario
176100040325     C                   MOVEL     'D'           KTRC
176200940915     C                   MOVEL     *BLANKS       AR4NOT
176300060213     C     KAR4          CHAIN(n)  FiAR401L                           31
176400941223     C                   MOVEL     AR4NOT        V2CDE2
176500940915     C* Mittente
176600911011     C     ARBCCM        IFNE      *ZEROS
176700940915     C                   MOVEL     ARBCCM        V2CSCM
176800911011     C                   ELSE
176900940915     C                   MOVEL     ARBKSC        V2CSCM
177000911011     C                   END
177100091021     c*forzatura SCM in caso di triangolazione ORM se cliente particolare
177200091021     c* presente in tabella CLI
177300091021     c                   exsr      forzacli
177400091021      *
177500940915     C                   MOVEL     ARBRMN        V2CRMN
177600940915     C                   MOVEL     ARBRMA        V2CRMA
177700940915     C                   MOVEL     ARBRSM        V2CRSM
177800940915     C                   MOVEL     ARBINM        V2CINM
177900940915     C                   MOVEL     ARBLOM        V2CLOM
178000940915     C                   MOVEL     ARBPRM        V2CPRM
178100940915     C                   MOVEL     ARBCAM        V2CCAM
178200940915     C                   MOVEL     ARBNZM        V2CNZM
178300940919     C* Preimposto codice riapertura che c'è su bolla
178400940915     C                   MOVEL     ARBCMC        V2CCMC
178500950111     C     ARBFFD        IFEQ      'S'
178600950224     C     D05CMC        IFEQ      *BLANKS
178700950224     C     D05AAF        ANDNE     *ZEROS
178800950224     C     ARBCMC        OREQ      *BLANKS
178900950224     C     D05AAF        ANDEQ     *ZEROS
179000950111     C                   MOVE      '032'         V2CCMC
179100950414     C* Se fermo deposito devo scrivere evento
179200950414     C                   MOVEL     'S'           WWTR
179300950111     C                   END
179400950224     C                   END
179500950111     C*
179600950102     C     D05CMC        IFNE      *BLANKS
179700950102     C                   MOVEL     D05CMC        V2CCMC
179800950102     C                   END
179900050217     C* Se l.avviso devo scrivere evento
180000950414     C     WWTR          IFEQ      'N'
180100950414     C                   Z-ADD     1             X
180200950414     C     V2CCMC        LOOKUP    C2A(X)                                 32
180300950414     C     *IN32         IFEQ      '1'
180400950414     C     T2A(X)        ANDEQ     'A'
180500950414     C                   MOVEL     'S'           WWTR
180600950414     C                   END
180700950414     C                   END
180800940915     C* Data comunicazione interna = data dist. pad. se in arrivo
180900941223     C                   Z-ADD     WOGGI         G02INV
181000940915     C     ARBDDC        IFGT      0
181100940919     C                   Z-ADD     ARBDDC        G02INV
181200941223     C                   END
181300950613     C*
181400941223     C                   Z-ADD     0             G02DAT
181500940915     C                   MOVEL     '3'           G02ERR
181600940919     C                   CALL      'XSRDA8'
181700940915     C                   PARM                    WLBDA8
181800940915     C                   MOVE      G02DAT        V2CDXD
181900950613     C*
182000950613     C     D05DXD        IFNE      *ZEROS
182100950613     C                   MOVE      D05DXD        V2CDXD
182200950613     C                   END
182300940915     C* Note su bolla
182400940915     C                   MOVEL     '8'           KTRC
182500940915     C                   MOVEL     *BLANKS       AR4NOT
182600060213     C     KAR4          CHAIN(n)  FiAR401L                           31
182700941223     C                   MOVEL     AR4NOT        V2CNOT
182800940915     C                   MOVEL     '9'           KTRC
182900940915     C                   MOVEL     *BLANKS       AR4NOT
183000060213     C     KAR4          CHAIN(n)  FiAR401L                           31
183100940915     C                   MOVEL     AR4NOT        V2CNO2
183200040310      *
183300040310     C                   MOVEL     'A'           KTRC
183400040310     C                   MOVEL     *BLANKS       AR4NOT
183500060213     C     KAR4          CHAIN(n)  FiAR401L                           31
183600040310     C  n31              MOVEL     AR4NOT        dsbl4a
183700040310     C   31              clear                   dsbl4a
183800940920     C* Carico messaggio particolarità
183900940920     C                   MOVEL     'N'           WFORZA
184000940920     C     ARBGGA        IFNE      *BLANKS
184100940920     C                   Z-ADD     1             X
184200940920     C     ARBGGA        LOOKUP    C7Q(X)                                 32
184300940920     C     *IN32         IFEQ      '1'
184400940920     C                   MOVE      D7Q(X)        DS7Q
184500021031     C                   Z-ADD     1             X
184600021031     C     arbGGA        LOOKUP    C7W(X)                                 32
184700021031     C     *IN32         IFEQ      '1'
184800021031     C                   MOVEl     D7w(X)        DS7w
184900040323     c                   if        §7W6T1 = 'T'  or
185000040323     c                             §7W6T2  = 'T'  or
185100040323     c                             §7W6T3  = 'T'  or
185200040323     c                             §7W6T4  = 'T'  or
185300040323     c                             §7W6T5  = 'T'
185400021031     c                   except    liberaarb
185500050627    1c                   if        gescomm = 'S'
185600050627     c                   commit
185700050627     c                   end
185800050217     c* forzo le "disposizioni" senza passare dalla giacenza
185900050217     c* es: invece di aprire giacenza distruggere la merce
186000021031     c                   movel     arblnp        fldlnpa
186100021031     c                   movel     arbaas        fldaasa
186200021031     c                   movel     arbnrs        fldnrsa
186300021031     c                   movel     arbnsp        fldnspa
186400021031     c                   movel     §7wca1        fldcca
186500021031     c                   movel     arbgga        fldgga
186600021031     c                   movel(p)  param80       kpjbu
186700021031     c                   call      'FNLG80R1'
186800021031     c                   parm                    kpjba
186900021031     c                   move      'X'           forzato
187000021031     c                   goto      forza2
187100021031     c                   endif
187200021031     c                   endif
187300050217     c* se deve mandare un msg
187400040421     C     §7Q2MF        IFEQ      'A'
187500940920     C     §7Q2MS        ANDNE     *BLANKS
187600040421      * verifico data consegna richiesta se maggiore di oggi non eseguo
187700040421     c                   if        arbdcr < woggi
187800040421     C     §7Q2MQ        IFEQ      'P'
187900950105     C                   MOVEL     'N'           WTREVE
188000950105     C                   MOVEL     'N'           WFORZA
188100950105     C                   MOVEL     §7Q2EV        V2CEVE
188200040421     C     §7Q2EV        IFNE      *BLANKS
188300950105     C                   EXSR      CTREVE
188400040421     C                   END
188500040421     C     WTREVE        IFEQ      'N'
188600950104     C                   MOVEL     §7Q2MS        V2CMSP
188700940920     C                   MOVEL     §7Q2MC        WFORZA
188800950104     C                   MOVEL     V2CLNA        WFLE              3 0
188900940920     C                   SETON                                        05
189000040421     C                   END
189100040421     C                   ELSE
189200950104     C                   MOVEL     §7Q2MS        V2CMSD
189300940920     C                   SETON                                        06
189400040421     C                   END
189500040421     C                   endif
189600040421     C                   END
189700940920     C                   END
189800940920     C                   END
189900961113     C* Se sono in arrivo reperisco descrizione aggiuntiva
190000961113     C* motivazione apertura giacenza da EDRDE
190100980112     C                   SETON                                        37
190200980112     C                   MOVEL     *BLANKS       ORGFL1
190300980112     C                   MOVE      ARBLNA        KFIL
190400980112     C     KFIL          CHAIN     AZORG01L                           31
190500020729     C                   MOVEL     *blank        LNAFL1            1
190600020729     C                   clear                   OG143
190700070320     C                   clear                   OG148
190800070410     C                   clear                   OG146
190900020729     C  n31              MOVEL     ORGDE3        OG143
191000070320     C  n31              MOVEL     ORGDE8        OG148
191100070410     C  n31              MOVEL     ORGDE6        OG146
191200070424     c  n31              if        §ogdda <> *blank
191300070424     c                   move      §ogdda        ogddan            8 0
191400070424     c                   endif
191500020729     C  N31§OgNTW        ifeq      'FED'
191600020729     C     §OgNTW        oreq      'EEX'
191700020729     C     §OgNTW        oreq      'EUP'
191800020729     C                   MOVEL     'E'           LNAFL1
191900020729     c                   end
192000961113     C     D05GES        IFEQ      'A'
192100980112     C     ORGFVA        ANDEQ     ' '
192200961217     C     WTRRDE        ANDEQ     'S'
192300020729     C     LNAFL1        ANDEQ     'E'
192400961204     C                   MOVEL     'DD2380'      KNMC
192500961204     C     KRDE          SETGT     EDRDE01L
192600961204     C     KRDE          READPE    EDRDE01L                               37
192700961204     C  N37              MOVEL     RDEVAL        V2CDMC
192800961204     C                   MOVEL     'DD4440'      KNMC
192900961113     C     KRDE          SETGT     EDRDE01L
193000961113     C     KRDE          READPE    EDRDE01L                               37
193100961113     C  N37              MOVEL     RDEVAL        V2CDMC
193200961113     C                   END
193300940915     C*
193400150515     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" SDE CLIENTE
193500150515     C* CODIFICATO
193600150515     C                   MOVE      v2cscm        APPO              4 0
193700150515     C                   CLEAR                   TRUL27
193800150515     C     APPO          IFNE      8888
193900150515     C     APPO          ANDNE     9999
194000150515     C     APPO          ANDNE     *ZEROS
194100150515     C                   MOVEL     D05TRI        I27TLA
194200150515     C                   MOVEL     arbTSP        I27TSP
194300150515     C                   MOVE      arbLNA        I27LNA
194400150515     C                   MOVE      arbLNP        I27LNP
194500150515     C                   MOVE      arbctr        I27ctb
194600150515     C                   MOVE      KKSC          I27CLI
194700160316     C*******            MOVE      AMGDSP        I27DTA
194800160316     C                   CALL      'TRUL27R1'
194900150515     C                   PARM                    TRUL27
195000150515     C                   END
195100021031     C     forza2        ENDSR
195200900418     C*---------------------------------------------------------------*
195300900418     C*   G C P V I D                                                 *
195400900418     C*---------------------------------------------------------------*
195500900418     C     GCPVID        BEGSR
195600940915     C*
195700960123     C                   Z-ADD     GCPAGC        V4CAGC
195800960123     C                   Z-ADD     GCPFGC        V4CFGC
195900960123     C                   Z-ADD     GCPNGC        V4CNGC
196000960123     C                   MOVEL     GCPFST        V2CFST
196100050805     c*                  if        gcptcm <> 'R'
196200050805     C*                  MOVEL     GCPtcm        V2Ctcm
196300050805     c*                  end
196400950908     C     D05GES        IFEQ      'P'
196500940125     C     GCPSCM        IFNE      *ZEROS
196600900423     C                   MOVE      GCPSCM        §QUATR            4
196700940919     C                   MOVE      GCPSCM        KKSC
196800900423     C     §QUATR        IFEQ      *ALL'8'
196900900423     C     §QUATR        OREQ      *ALL'9'
197000940919     C* mittente da appoggio
197100050217     C     KSPE1         CHAIN     tIGAP01L                           31
197200940915     C     *IN31         IFEQ      '0'
197300950213     C                   MOVE      GCPSCM        V2CSCM
197400940919     C                   MOVEL     GAPRSM        V2CRSM
197500940919     C                   MOVEL     GAPINM        V2CINM
197600940919     C                   MOVEL     GAPLOM        V2CLOM
197700940919     C                   MOVEL     GAPPRM        V2CPRM
197800940919     C                   MOVEL     GAPCAM        V2CCAM
197900940919     C                   MOVEL     GAPNZM        V2CNZM
198000900418     C                   END
198100900509     C*
198200900419     C                   ELSE
198300940915     C     KACO          CHAIN     CNACO000                           31
198400940915     C     *IN31         IFEQ      '0'
198500940915     C     ACOFLG        ANDEQ     ' '
198600130322     C     ACOABL        ANDeq     *blank
198700950213     C                   MOVE      GCPSCM        V2CSCM
198800940915     C                   MOVEL     ACORAG        V2CRSM
198900940919     C     KACO          CHAIN     CNIND000                           31
199000940915     C     *IN31         IFEQ      '0'
199100940915     C                   MOVEL     INDVIA        V2CINM
199200940915     C                   MOVEL     INDCIT        V2CLOM
199300940915     C                   MOVEL     INDPRV        V2CPRM
199400050223     C                   MOVEL     INDCAE        V2CCAM
199500940919     C                   MOVEL     INDSTA        V2CNZM
199600940919     C                   END
199700940125     C                   END
199800940915     C                   END
199900940919     C                   END
200000950908     C*
200100950908     C                   END
200200050217     C* apertura prendo motivo dalla giac.
200300050217     C     WRIAPE        IFeq      ' '
200400940915     C* Codice motivo giacenza
200500940915     C     V2CCMC        IFEQ      *BLANKS
200600940915     C                   MOVE      GCPCMC        V2CCMC
200700920210     C                   END
200800940919     C* Ulteriore motivazione
200900950103     C                   Z-ADD     GCPAGC        KAGC
201000950103     C                   Z-ADD     GCPFGC        KFGC
201100950103     C                   Z-ADD     GCPNGC        KNGC
201200950103     C                   Z-ADD     0             KFRG
201300960208     C                   Z-ADD     10            KFAS
201400940919     C                   MOVE      *BLANKS       GNPDMC
201500050217     C     KGNP1         CHAIN     tIGNP01L                           31
201600911014     C     GNPDMC        IFNE      *BLANKS
201700940919     C                   MOVE      GNPDMC        V2CDMC
201800911129     C                   END
201900900604     C                   END
202000940919     C* Luogo deposito merce
202100940919     C                   MOVE      GCPCCR        V2CCCR
202200940919     C* Nr. e data comunicazione
202300940919     C                   MOVE      GCPNRC        V2CNRC
202400950908     C     D05GES        IFEQ      'P'
202500940919     C                   Z-ADD     GCPDXD        G02INV
202600940920     C                   Z-ADD     GCPDXD        AMGDXD
202700940919     C                   Z-ADD     0             G02DAT
202800940919     C                   MOVEL     '3'           G02ERR
202900940919     C                   CALL      'XSRDA8'
203000940919     C                   PARM                    WLBDA8
203100940919     C                   Z-ADD     G02DAT        V2CDXD
203200940919     C                   END
203300940919     C* Data apertura giacenza
203400940919     C                   MOVE      GCPMGC        WGG
203500940919     C                   MOVEL     GCPMGC        WMM
203600940919     C                   MOVEL     WGGMM         V2CDTG
203700940919     C                   MOVE      GCPAGC        V2CDTG
203800940919     C                   MOVE      GCPNGC        V2CNGC
203900940919     C* Fil. che ha aperto la giacenza
204000900419     C     GCPFGC        IFNE      *ZEROS
204100940919     C                   MOVE      GCPFGC        V2CFGC
204200940919     C                   MOVE      GCPFGC        KFIL
204300940919     C                   MOVEL     *ALL'?'       V2DFGC
204400940919     C     KFIL          CHAIN     AZORG01L                           31
204500940919     C     *IN31         IFEQ      '0'
204600940919     C     ORGFVA        ANDEQ     ' '
204700940920     C                   MOVEL     *BLANKS       V2DFGC
204800940920     C                   MOVEL     ORGDES        V2DFGC
204900900419     C                   END
205000940919     C                   END
205100940919     C* Data ultima riapertura
205200050223     c                   if        gcpriap = 'R'
205300940919     C                   MOVE      GCPDUR        G02INV
205400940919     C                   Z-ADD     0             G02DAT
205500940919     C                   MOVEL     '3'           G02ERR
205600940919     C                   CALL      'XSRDA8'
205700940919     C                   PARM                    WLBDA8
205800940919     C                   Z-ADD     G02DAT        V2CDUR
205900050223     c                   else
206000050223     C                   Z-ADD     0             V2CDUR
206100050223     c                   end
206200940919     C*
206300900418     C                   ENDSR
206400940922     C*---------------------------------------------------------------*
206500940922     C*   C H I U G C                                                 *
206600940922     C*---------------------------------------------------------------*
206700950301     C     APEGIA        BEGSR
206800940922     C*
206900940922     C                   MOVEL     'P'           V2CTCM
207000940922     C                   MOVEL     'N'           WTRTCM            1
207100950116     C                   Z-ADD     1             Y                 3 0
207200950301     C                   MOVE      V2CSCM        APPO              4 0
207300950228     C     BLPNZD        LOOKUP    CNZ(Y)                                 39
207400940922     C     APPO          IFNE      8888
207500940922     C     APPO          ANDNE     9999
207600950116     C     APPO          ANDNE     *ZEROS
207700990730     C*  Cerco progressivo tariffa in vigore su TNTAM
207800150512     C                   CLEAR                   fnlv59ds
207900150512     C                   MOVE      BLPCTR        kctr
208000150512     C                   MOVE      BLPCTR        ilv59ctr
208100150514     C                   MOVEL     ' '           ilv59tbo
208200150512     C                   Z-ADD     AMGDSP        ilv59dsp
208300150512     C                   MOVE      KKSC          ilv59ksc
208400150512     C                   MOVE      KCI           ilv59kci
208500150512     C                   MOVEL     O27FIE        ilv59fie
208600150512     C                   MOVEL     BLPTSP        ilv59tsp
208700150512     C                   MOVEL     'S'           ilv59con
208800150512     c                   eval      ilv59ta2='B'
208900150512     c                   eval      ilv59blo='S'
209000150512     C                   CALL      'FNLV59R'
209100150512     C                   PARM                    fnlv59ds
209200950113     C*
209300150512     C     olv59err      IFEQ      *BLANKS
209400150512     C     olv59tks      ANDEQ     'S'
209500950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
209600950113     C*  delle tariffe giacenze con il proggressivo giusto
209700150512     C                   MOVE      olv59prg      KPRG
209800990730     C     KTGC          CHAIN     TITGC01L                           31
209900950113     C     *IN31         IFEQ      '0'
210000950116     C     TGCTCM        ANDNE     *BLANKS
210100950113     C                   MOVE      TGCTCM        V2CTCM
210200950113     C                   MOVEL     'S'           WTRTCM
210300950113     C                   END
210400950113     C                   END
210500950113     C                   END
210600940922     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
210700940922     C*  lo cerco sull'anagrafica cliente
210800050217     c                   exsr      srcli
210900940922     C*
211000940922     C                   ENDSR
211100050517     C*---------------------------------------------------------------*
211200050517     C*   C H I U G C                                                 *
211300050517     C*---------------------------------------------------------------*
211400050517     C     APEGIAa       BEGSR
211500050608     C*
211600050517     c                   if        £LBLAAO = 0
211700050608     c* sono in aggiornamento archivi
211800050608     c                   if        flgagg = 1
211900050608     C     kspe1         chain     fnblp01l
212000050608     c                   else
212100050608     c* sono nei controlli del video
212200050608     C     kspe1         chain(n)  fnblp01l
212300050608     c                   end
212400050517     c                   else
212500050608     c                   if        flgagg = 1
212600050608     C     kblp          chain     fnblp01l
212700050608     c                   else
212800050608     C     kblp          chain(n)  fnblp01l
212900050517     c                   end
213000050608     c                   end
213100050608     c*
213200050517     C                   IF        %FOUND(FNBLP01L)
213300050517     C     BLPCCM        IFNE      *ZEROS
213400050517     C                   MOVEL     BLPCCM        V2CSCM
213500050517     C                   Z-ADD     BLPCCM        KKSC
213600050517     C                   ELSE
213700050517     C                   MOVEL     BLPKSC        V2CSCM
213800050517     C                   Z-ADD     BLPKSC        KKSC
213900050517     C                   END
214000091102     c                   exsr      forzacli
214100050809     C                   MOVE      V2CSCM        APPO              4 0
214200050809     c* tipo comunicazione
214300050701     c                   if        v2ctcm = ' '
214400050517     C                   MOVEL     'P'           V2CTCM
214500050517     C                   MOVEL     'N'           WTRTCM            1
214600050517     C                   Z-ADD     1             Y                 3 0
214700050517     C     BLPNZD        LOOKUP    CNZ(Y)                                 39
214800050517     C     APPO          IFNE      8888
214900050517     C     APPO          ANDNE     9999
215000050517     C     APPO          ANDNE     *ZEROS
215100050517     C*  Cerco progressivo tariffa in vigore su TNTAM
215200150512     C                   CLEAR                   fnlv59ds
215300150512     C                   MOVE      BLPCTR        kctr
215400150512     C                   MOVE      BLPCTR        ilv59ctr
215500150514     C                   MOVEL     ' '           ilv59tbo
215600150512     C                   Z-ADD     AMGDSP        ilv59dsp
215700150512     C                   MOVE      KKSC          ilv59ksc
215800150512     C                   MOVE      KCI           ilv59kci
215900150512     C                   MOVEL     O27FIE        ilv59fie
216000150512     C                   MOVEL     BLPTSP        ilv59tsp
216100150512     C                   MOVEL     'S'           ilv59con
216200150512     c                   eval      ilv59ta2='B'
216300150512     c                   eval      ilv59blo='S'
216400150512     C                   CALL      'FNLV59R'
216500150512     C                   PARM                    fnlv59ds
216600050517     C*
216700150512     C     olv59ERR      IFEQ      *BLANKS
216800150512     C     olv59tks      ANDEQ     'S'
216900050517     C*  Se ho trovato tariffa in vigore mi posiziono sul file
217000050517     C*  delle tariffe giacenze con il proggressivo giusto
217100150512     C                   MOVE      olv59prg      KPRG
217200050517     C     KTGC          CHAIN     TITGC01L                           31
217300050517     C     *IN31         IFEQ      '0'
217400050517     C     TGCTCM        ANDNE     *BLANKS
217500050517     C                   MOVE      TGCTCM        V2CTCM
217600160316     C                   MOVEL     'S'           WTRTCM
217700050517     C                   END
217800050517     C                   END
217900050517     C                   END
218000050517     c                   else
218100050517     C                   MOVEL     'S'           WTRTCM            1
218200050517     c                   end
218300050517     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
218400050517     C*  lo cerco sull'anagrafica cliente
218500050517     c                   exsr      srcli
218600050809     c* se cliente vario e cod. mancata consegna = 999 (riapertura tecnica)
218700050809     c* forzo la riapertura come autoconfermata
218800050809     c                   if        v2ccmc = '999' and (appo = 8888 or
218900050809     c                             appo = 9999)
219000050809     c                   eval      §2uape = 'S'
219100050608     C                   END
219200050809     C*
219300050809     C                   END
219400050517     C*
219500050517     C                   ENDSR
219600900418     C*---------------------------------------------------------------*
219700900418     C*   C T R V I D                                                 *
219800900418     C*---------------------------------------------------------------*
219900940919     C     CTR02         BEGSR
220000021031     c                   if        giacen = 'N'
220100021031     C                   MOVEL     ERR(28)       $MSG
220200021031     C                   SETON                                        28
220300021031     C                   GOTO      FIctr2
220400021031     c                   endif
220500941003     C* Cod.particolarità giacenza
220600941003     C     '?'           SCAN      V2CRPG                                 32
220700941003     C     *IN32         IFEQ      '1'
220800941003     C                   MOVEL     'S'           DS7RIC
220900941003     C                   MOVEL     V2CGGA        DS7COD
221000941003     C                   MOVEL     'V'           DS7GES
221100941003     C                   MOVEL     DSTB69        KPJBU
221200941003     C                   CALL      'TRTB69R'
221300941003     C                   PARM                    KPJBA
221400941003     C                   SETON                                        19
221500941003     C                   MOVEL     ' '           V2CRPG
221600941003     C                   GOTO      FICTR2
221700941003     C                   END
221800940919     C* Cod.giacenza
221900940919     C     '?'           SCAN      V2CCMC                                 32
222000940919     C     *IN32         IFEQ      '1'
222100940919     C*§Ricerca in Tabella
222200940919     C                   MOVEL     '2A'          KCOD
222300940919     C                   MOVEL     *BLANK        KKEY
222400940919     C                   MOVEL     *BLANK        §DES             60
222500940919     C                   MOVE      *BLANK        V2CCMC
222600940919     C                   CALL      'X§TB60R'
222700940919     C                   PARM                    CODUT
222800940919     C                   PARM                    KCOD
222900940919     C                   PARM                    KKEY
223000940919     C                   PARM                    §DES
223100940919     C                   MOVEL     KKEY          V2CCMC
223200940919     C                   MOVEL     §DES          V2DCMC
223300940919     C                   SETON                                        19
223400940920     C                   GOTO      FICTR2
223500940919     C                   END
223600110531     C*particolarità giacenza con motivi non ammessi per apertura giacenza
223700110601     c                   if        v2ccmc <> *blank and v2cgga <> *blank
223800110531     c     v2ccmc        lookup    tmt                                    32
223900110531     c                   if        *in32
224000110531     C                   SETON                                        2842
224100110531     C                   MOVEL     ERR(35)       $MSG
224200110531     C                   GOTO      FICTR2
224300110531     c                   endif
224400110531     c                   endif
224500110531      *
224600940919     C                   Z-ADD     1             X
224700940919     C     V2CCMC        LOOKUP    C2A(X)                                 32
224800040324     c     d05cmc        ifne      d05cmch
224900040115     c     d05cmc        ifne      *blank
225000040115     c     dcr(x)        andeq     *blank
225100040115     C     *IN32         andeq     '1'
225200040115     C                   MOVEL     *ALL'?'       V2DCMC
225300040115     C                   SETON                                        2842
225400040115     C                   MOVEL     ERR(13)       $MSG
225500040115     C                   GOTO      FICTR2
225600040115     c                   else
225700040115     c                   z-add     x             xdcr              3 0
225800040115     c                   endif
225900040324     c                   else
226000040324     c                   z-add     x             xdcr              3 0
226100040324     c                   endif
226200040202     c     d05cmc        ifeq      *blank
226300040202     c     v2ccmc        andeq     §2gmdr
226400040325     c     d05cmc        oreq      *blank
226500040325     c     v2ccmc        andeq     §2gadr
226600040202     C                   MOVEL     *ALL'?'       V2DCMC
226700040202     C                   SETON                                        2842
226800040202     C                   MOVEL     ERR(13)       $MSG
226900040202     C                   GOTO      FICTR2
227000040202     c                   endif
227100080604      * verifica se destinatario e indirizzo già frequentati x segnalazione
227200080604      * motivo di non trovato ecc.ecc.
227300080604     c                   if        contrdest = *blank
227400080604     c                   if        v2ccmc = '021' and d05ges = 'A' or
227500080604     c                             v2ccmc = '022' and d05ges = 'A'
227600080604     c                   clear                   truldesds
227700080604     c                   move      arbRSD        idsRSD
227800080604     c                   move      arbIND        idSIND
227900080604     c                   move      arbLOD        idSLOD
228000080604     c                   move      arbPRD        idSPRD
228100080604     c                   move      arbNZD        idSNZD
228200080604     c                   call      'TRULDESR'
228300080604     c                   parm                    truldesds
228400080604     c                   if        odsesito = 'S'
228500080604     c                   eval      $msg = 'Esistono già -
228600080604     c                             spedizioni -
228700080604     c                             consegnate a questo indirizzo ' +
228800080604     c                             odsbolla
228900080604     c                   eval      contrdest = 'X'
229000080604     C                   SETON                                        2842
229100080604     C                   GOTO      FICTR2
229200080604     c                   endif
229300080604     c                   endif
229400080604     c                   endif
229500170412      * motivo 008 non permesso se non presente valore contrasssegno
229600170412     c     v2ccmc        ifeq      '008'
229700170412     c     V2CCAS        andeq     0
229701170725     c     d82ges        andeq     'A'
229800170412     C                   SETON                                        2842
229900170412     C                   MOVEL     ERR(38)       $MSG
230000170412     C                   GOTO      FICTR2
230100170412     c                   endif
230200170412      * motivo 012 non permesso se spedizione in assegnato
230300170412     c     v2ccmc        ifeq      '012'
230400170412     c     tipbol        andeq     'F'
230500170412     C                   SETON                                        2842
230600170412     C                   MOVEL     ERR(39)       $MSG
230700170412     C                   GOTO      FICTR2
230800170412     c                   endif
230900040310      * motivo 035 non permesso se linea partenza italia e DDT=si
231000170410      *paking list dopo controllo DDT
231100040310     c     v2ccmc        ifeq      '035'
231200040310     c     lnpfl1        andeq     ' '
231300040310     c     wntwlnp       andeq     *blank
231400040310     c                   if        §b4abm = 'S' or §b4abm = 'P' or
231500040310     c                             §b4abm = 'C' or §b4abm = 'Q'
231600040310     C                   SETON                                        2842
231700050608     C                   MOVEL     ERR(30)       $MSG
231800040310     C                   GOTO      FICTR2
231900040310     c                   endif
232000170410      *stesso controllo viene effettuato anche per la presenza di un
232100170410      *paking list
232200170410     c                   if        §AR5ALM ='S' or
232300170410     c                             §AR5ALS ='S' or
232400170410     c                             §AR5ALX ='S'
232500170410     C                   SETON                                        2842
232600170410     C                   MOVEL     ERR(30)       $MSG
232700170410     C                   GOTO      FICTR2
232800170410     c                   endif
232900040310     c                   endif
233000050608      * motivo 999 non utilizzabile in partenza
233100050608     c     v2ccmc        ifeq      '999'
233200050608     c     d05ges        andeq     'P'
233300050608     C                   SETON                                        2842
233400050608     C                   MOVEL     ERR(13)       $MSG
233500050608     C                   GOTO      FICTR2
233600050608     c                   endif
233700040310      *
233800950104     C     V2CCMC        IFNE      'GEN'
233900940919     C     *IN32         IFEQ      '1'
234000941228     C     V2CCMC        ANDNE     *BLANKS
234100950105     C     F2A(X)        ANDEQ     ' '
234200940920     C                   MOVEL     *BLANKS       V2DCMC
234300940920     C                   MOVEL     D2A(X)        V2DCMC
234400940919     C                   ELSE
234500950105     C     WFORZA        IFNE      'S'
234600950105     C     *INKA         OREQ      '1'
234700940919     C                   MOVEL     *ALL'?'       V2DCMC
234800940919     C                   SETON                                        2842
234900940919     C                   MOVEL     ERR(13)       $MSG
235000940919     C                   GOTO      FICTR2
235100940919     C                   END
235200950105     C                   END
235300970226     C* Se sto gestendo giacenza in partenza di una bolla import
235400970226     C* ctrl se devo gestrie obbligatoriamente l'ulteriore motiv.
235500980112     C     LNPFL1        IFEQ      'E'
235600970226     C     D05GES        ANDEQ     'P'
235700970310     C     V2CDMC        IFEQ      *BLANKS
235800970310     C     *IN03         OREQ      '0'
235900970226     C                   EXSR      CTRFTX
236000970226     C   28              GOTO      FICTR2
236100970226     C                   END
236200970310     C                   END
236300940919     C* Se codice motiv. generico obbligo l'ulteriore motivaz.
236400940919     C                   ELSE
236500980112     C     LNPFL1        IFEQ      'E'
236600961113     C                   SETON                                        2842
236700961113     C                   MOVEL     ERR(21)       $MSG
236800961113     C                   GOTO      FICTR2
236900961113     C                   END
237000940919     C     V2CDMC        IFEQ      *BLANKS
237100940919     C                   SETON                                        2843
237200961113     C                   MOVEL     ERR(14)       $MSG
237300940919     C                   GOTO      FICTR2
237400940919     C                   END
237500940919     C                   END
237600950104     C   32              MOVEL     TPG(X)        WTPGIA            1
237700940919     C*--------------------------------------------------------------*
237800940919     C*  Partenza  !!!!
237900940919     C     D05GES        IFEQ      'P'
238000940920     C*--------------------------------------------------------------*
238100940919     C* Ricerca cliente
238200940919     C     '?'           SCAN      V2CSCM                                 32
238300940919     C     *IN32         IFEQ      '1'
238400940919     C                   Z-ADD     KCI           KKCC
238500940919     C                   MOVEL     *BLANKS       ACORAG
238600940919     C                   MOVEL     V2CRSM        ACORAG
238700940919     C                   Z-ADD     0             ACOKSC
238800940919     C                   Z-ADD     0             PARSTA            1 0
238900940919     C                   CALL      'XALFA1'
239000940919     C                   PARM                    PARDUT
239100940919     C                   PARM                    CODUT
239200940919     C                   PARM                    ACORAG
239300940919     C                   PARM                    ACOKCC
239400940919     C                   PARM                    ACOKSC
239500940919     C                   PARM                    PARSTA
239600961016     C                   PARM                    PARFLR           90
239700940919     C     PARSTA        IFNE      -1
239800940919     C                   MOVE      ACOKSC        V2CSCM
239900940919     C                   ELSE
240000940919     C                   MOVE      *ZEROS        V2CSCM
240100940919     C                   END
240200940919     C                   SETON                                        19
240300940919     C                   GOTO      FICTR2
240400940919     C                   END
240500940919     C* Controllo se cambiato il mittente
240600940919     C     V2CSCM        IFEQ      *ZEROS
240700940919     C                   SETON                                        4028
240800940919     C                   MOVEL     ERR(10)       $MSG
240900940919     C                   GOTO      FICTR2
241000940919     C                   END
241100940919     C*  Se cambiato codice cliente lo decodifico
241200940919     C                   MOVE      V2CSCM        §SCM
241300050606     C                   MOVE      V2CSCM        §QUATR
241400940921     C                   MOVE      V2CSCM        KKSC
241500940920     C     §QUATR        IFNE      *ALL'8'
241600940920     C     §QUATR        ANDNE     *ALL'9'
241700940920     C     §SCM          IFNE      SAVSCM
241800940919     C     KACO          CHAIN     CNACO000                           31
241900940919     C     *IN31         IFEQ      '0'
242000940919     C     ACOFLG        ANDEQ     ' '
242100130322     C     ACOABL        ANDeq     *blank
242200940919     C                   MOVEL     ACORAG        V2CRSM
242300940920     C  NKF              SETON                                        6019
242400940919     C     KACO          CHAIN     CNIND000                           21
242500940919     C     *IN31         IFEQ      '0'
242600940919     C     INDFLG        ANDEQ     ' '
242700940919     C                   MOVEL     INDVIA        V2CINM
242800940919     C                   MOVEL     INDCIT        V2CLOM
242900940919     C                   MOVEL     INDPRV        V2CPRM
243000050223     C                   MOVEL     INDCAE        V2CCAM
243100940920     C                   MOVEL     INDSTA        V2CNZM
243200940919     C                   END
243300940919     C                   ELSE
243400940919     C                   SETON                                        4028
243500940919     C                   MOVEL     ERR(10)       $MSG
243600940919     C                   GOTO      FICTR2
243700940919     C                   END
243800940920     C                   END
243900940920     C                   ELSE
244000940920     C     §SCM          IFNE      SAVSCM
244100940920     C     V2CRSM        ORNE      SAVRSM
244200940920     C     V2CINM        ORNE      SAVINM
244300940920     C     V2CLOM        ORNE      SAVLOM
244400940920     C     V2CPRM        ORNE      SAVPRM
244500940920     C     V2CCAM        ORNE      SAVCAM
244600940920     C     V2CNZM        ORNE      SAVNZM
244700940920     C  NKF              SETON                                        6019
244800940920     C                   END
244900940919     C                   END
245000940919     C*--------------------------------------------------------------*
245100050606     C*  Ricerca tipo comunicazione
245200050606    1C     V2CTCM        IFEQ      '?'
245300050606     C*§  RICERCA IN TABEL00F
245400050606     C                   MOVEL     '2F'          KCOD
245500050606     C                   MOVEL     *BLANK        KKEY
245600050606     C                   MOVEL     *BLANK        §DES
245700050606     C                   MOVE      *BLANK        V2CTCM
245800050606     C                   CALL      'X§TABER'
245900050606     C                   PARM                    CODUT
246000050606     C                   PARM                    KCOD
246100050606     C                   PARM                    KKEY
246200050606     C                   PARM                    §DES
246300050606     C                   MOVEL     KKEY          V2CTCM
246400050606     C                   MOVEL     §DES          V2DTCM
246500050606     C                   SETON                                        19
246600050606     C                   GOTO      FICTR2
246700050606    1C                   END
246800050608     c                   clear                   savs2f            1
246900050608     c                   clear                   savf2f            1
247000050608     c                   clear                   savv2f            1
247100050608     c                   clear                   save2f            1
247200050606     C                   Z-ADD     1             X
247300050606     C     V2CTCM        LOOKUP    C2F(X)                                 32
247400050606    1C     *IN32         IFEQ      '1'
247500050614     c     r2f(x)        andeq     *blank
247600050606     C                   MOVEL     D2F(X)        V2DTCM
247700050606     C*                  MOVEL     F2F(X)        TIPTCM            1
247800050608     c                   eval      savs2f = s2f(x)
247900050608     c                   eval      savf2f = f2f(x)
248000050608     c                   eval      savv2f = v2f(x)
248100050608     c                   eval      save2f = e2f(x)
248200050606     C*  Controllo Fax
248300050606    2C     T2F(X)        IFEQ      'F'
248400160223      *
248500050606    3C     §QUATR        IFEQ      *ALL'8'
248600050606     C     §QUATR        OREQ      *ALL'9'
248700050606     C                   SETON                                        2841
248800050606     C                   MOVEL     ERR(18)       $MSG
248900050606     C                   GOTO      FICTR2
249000050606     C                   ELSE
249100160223      *
249200050606     C                   CLEAR                   TRUL43
249300050606     C                   MOVE      V2CSCM        D43KSC
249400050606     C                   CALL      'TRUL43R'
249500050606     C                   PARM                    TRUL43
249600050606     C                   CLEAR                   TRUL42
249700050606     C                   MOVEL     INDTLF        D42FAX
249800050606    4C     D43TRV        IFEQ      'S'
249900050606     C                   MOVEL     D43FAX        D42FAX
250000050606    4C                   END
250100050606     C                   CALL      'TRUL42R'
250200050606     C                   PARM                    TRUL42
250300050606    4C     D42ERR        IFEQ      '1'
250400050606     C                   SETON                                        2841
250500050606     C                   MOVEL     ERR(19)       $MSG
250600050606     C                   GOTO      FICTR2
250700050606    4C                   END
250800050606    3C                   END
250900050606    2C                   END
251000050606     C*  Controllo invi tramite mail
251100050606    2C     T2F(X)        IFEQ      'M'
251200160223      *
251300050606    3C     §QUATR        IFEQ      *ALL'8'
251400050606     C     §QUATR        OREQ      *ALL'9'
251500050606     C                   SETON                                        2841
251600050606     C                   MOVEL     ERR(18)       $MSG
251700050606     C                   GOTO      FICTR2
251800050606     C                   ELSE
251900160223      *
252000160223      * controlla se si tratta di una bolla DPD con tanto di PARCEL
252100160223      *   da cui prendere il DEPOT
252200160223     c                   clear                   fnlg24ds
252300160223     c                   z-add     V1CAAS        i24aas
252400160223     c                   z-add     V1CLNP        i24lnp
252500160223     c                   z-add     V1CNRS        i24nrs
252600160223     c                   z-add     V1CNSP        i24nsp
252700160223      *
252800160223     c                   call      'FNLG24R'
252900160223     C                   parm                    fnlg24ds
253000160223      *
253100050606      * reperimento luogo e ind. e-mail
253200050606     C                   CLEAR                   TRUL43
253300160223     c*
253400160223     c*  se c'è il parcel allora deve passare il Depot e verificare
253500160223     c*  se presente l'indirizzo mail del DEPOT mittente per inviare
253600160223     c*  l'informazione di apertura giacenza.
253700160223     c                   if          o24npr <> *blank
253800160303     c                   if        %subst(o24npr:1:4)<>'0844' and
253900160303     c                             %subst(o24npr:1:4)<>'0845'
254000160223     c                   movel     o24npr        D43DEPOT
254100160303     c                   end
254200160223     c                   end
254300160223     c*
254400050606     C                   MOVE      V2CSCM        D43KSC
254500050606     C                   CALL      'TRUL43R'
254600050606     C                   PARM                    TRUL43
254700160223     c*
254800050606    4C     D43TRV        IFEQ      'S'
254900160223     c*
255000050606    5C     D43TRVm       ifne      'S'
255100050606     C                   SETON                                        2841
255200050606     C                   MOVEL     ERR(32)       $MSG
255300050606     C                   GOTO      FICTR2
255400050606    5c                   endif
255500160223     c*
255600050606     c                   else
255700050606     c* note reperimento nota 88 eventuale indirizzo mail in assenza
255800050606      * del luogo
255900050606     c                   move      v2cscm        kcnk1
256000050606     c     kntc          chain     tfntc01l
256100050606    5c                   if        not %found
256200050606     C                   SETON                                        2841
256300050606     C                   MOVEL     ERR(32)       $MSG
256400050606     C                   GOTO      FICTR2
256500050606    5c                   endif
256600050606    4c                   endif
256700050606    3c                   endif
256800050606    2c                   endif
256900050606      *
257000050606     C                   ELSE
257100050606     C                   SETON                                        2841
257200050606     C                   MOVEL     ERR(12)       $MSG
257300050606     C                   GOTO      FICTR2
257400050606    1C                   END
257500940919     C*--------------------------------------------------------------*
257600940919     C*   Arrivo !!!!!
257700940919     C                   ELSE
257800940919     C*--------------------------------------------------------------*
257900940919     C* Se sono in riapertura controllo se è possibile riaprire
258000940919     C     WRIAPE        IFEQ      'S'
258100050629     C     CNT           andeq     15
258200980114     C                   SETON                                        2819
258300940919     C                   MOVEL     ERR(11)       $MSG
258400940919     C                   GOTO      FICTR2
258500940919     C                   END
258600050629     c* ammetto l'apertura tecnica solo per xx volte (da tabella 2G)
258700050629     c                   if        v2ccmc = '999'
258800050629     c     cntced        add       1             comcnt            2 0
258900050701     c                   if        comcnt > §2glat
259000050629     C                   MOVEL     *ALL'?'       V2DCMC
259100050629     C                   SETON                                        2842
259200050629     C                   MOVEL     ERR(34)       $MSG
259300050629     C                   GOTO      FICTR2
259400050629     c                   end
259500050629     c                   end
259600950104     C* Controllo se posso aprire giacenza x motivo esaminato
259700960403     C     WRIAPE        IFNE      'S'
259800050607     c* codice giacenza 999 ammesso solo se riapertura
259900050607     c                   if        v2ccmc = '999'
260000050607     C                   MOVEL     *ALL'?'       V2DCMC
260100050607     C                   SETON                                        2842
260200050607     C                   MOVEL     ERR(13)       $MSG
260300050607     C                   GOTO      FICTR2
260400050607     c                   end
260500960605     C* Controllo se devo gestire forzatura da apertura
260600960605     C     V2CEVE        IFEQ      *BLANKS
260700960625     C     *IN07         OREQ      '0'
260800960605     C     *INKA         OREQ      '1'
260900960605     C*
261000960625     C     WFORZA        IFEQ      'S'
261100960625     C     *INKA         ANDEQ     '0'
261200960625     C                   SETON                                        19
261300960625     C                   ELSE
261400950104     C     V2CGGA        IFNE      *BLANKS
261500950104     C     WTIP          ANDNE     *BLANKS
261600950104     C                   SETOFF                                       32
261700950104     C     WTPGIA        IFNE      *BLANKS
261800950104     C                   Z-ADD     1             X
261900950104     C     WTPGIA        LOOKUP    TIP(X)                                 32
262000950104     C                   END
262100950104     C     *IN32         IFEQ      '0'
262200071107     C                   Z-ADD     1             X
262300950104     C     'T'           LOOKUP    TIP(X)                                 32
262400950104     C                   END
262500950104     C     *IN32         IFEQ      '1'
262600970624     C     *INKN         ANDEQ     '1'
262700970624     C                   SETON                                        28
262800970624     C                   MOVEL     ERR(24)       $MSG
262900970624     C                   GOTO      FICTR2
263000970624     C                   END
263100970624     C     *IN32         IFEQ      '1'
263200990805     C     DSP(X)        IFEQ      'R'
263300990805     C     WOGGI         ANDGE     §2GDPI
263400990805     C     WOGGI         ANDLE     §2GDPF
263500990805     C                   SETON                                        28
263600990805     C                   MOVEL     ERR(26)       $MSG
263700990805     C                   GOTO      FICTR2
263800990805     C                   END
263900950105     C                   SETON                                        19
264000950104     C     KSPE1         CHAIN     FNARB01L                           34
264100950104     C     *IN34         IFEQ      '0'
264200100614     C                   MOVEL     ARBFBC        savfbc
264300950104     C                   MOVEL     ' '           ARBFBC
264400950104     C                   EXCEPT    AGG
264500050627    1c                   if        gescomm = 'S'
264600050627     c                   commit
264700050627     c                   end
264800950104     C                   END
264900950104     C                   MOVE      V2CLNA        D82FGS
265000950104     C                   MOVE      *ZEROS        D82FRG
265100950104     C                   MOVEL     DSP(X)        D82DIS
265200950104     C                   MOVEL     'A'           D82GES
265300950104     C                   MOVEL     'N'           D82GIA
265400950104     C                   MOVE      KAAS          D82AAS
265500950104     C                   MOVE      KLNP          D82LNP
265600950104     C                   MOVE      KNRS          D82NRS
265700950104     C                   MOVE      KNSP          D82NSP
265800030716     C                   MOVEL     D05PSS        WPGMSS
265900021203     C                   MOVEL(p)  FNLG82        KPJBU
266000950104     C                   CALL      'FNLG20R'
266100950104     C                   PARM                    KPJBA
266200970204     C                   MOVEL     KPJBU         FNLG82
266300100614     C*  Se premuto F12 riMETTO IL BLOCCO A COME ERA PRIMA DELLA CHAIN
266400100614     C*IN MODO DA NON VARIARE COMUNQUE LA BOLLA PRIMA INSERIVA SEMPRE IL BLOCCO
266500970204     C     D82F12        IFEQ      '1'
266600970204     C     KSPE1         CHAIN     FNARB01L                           34
266700100614     C                   MOVEL     savFBC        ARBFBC
266800100614     C*mm                MOVEL     'G'           ARBFBC
266900970204     C  N34              EXCEPT    AGG
267000050627    1c  n34              if        gescomm = 'S'
267100050627     c                   commit
267200050627     c                   end
267300970204     C                   END
267400950104     C*
267500950104     C     D05RIC        IFEQ      'S'
267600950104     C                   MOVEL     'S'           WFINE
267700950104     C                   ELSE
267800950104     C                   MOVEL     '1'           WTPVID
267900950224     C                   MOVEL     D05GES        SAVGES
268000950224     C                   CLEAR                   FNLG05
268100950224     C                   MOVEL     SAVGES        D05GES
268200950104     C                   EXSR      PULVID
268300950104     C                   END
268400950104     C                   END
268500950104     C                   END
268600960403     C                   END
268700960625     C                   END
268800960605     C*
268900960605     C                   END
269000940919     C*--------------------------------------------------------------*
269100940919     C* Luogo deposito merce
269200940919     C                   Z-ADD     1             X
269300940919     C     V2CCCR        LOOKUP    C2B(X)                                 32
269400940919     C     *IN32         IFEQ      '1'
269500940920     C                   MOVEL     *BLANKS       V2DCCR
269600940920     C                   MOVEL     D2B(X)        V2DCCR
269700940919     C                   ELSE
269800940919     C* Se non lo trovo nella tabella corrisp. lo cerco in organigramma
269900940919     C                   MOVE      V2CCCR        KFIL
270000940919     C     KFIL          CHAIN     AZORG01L                           31
270100940919     C     *IN31         IFEQ      '0'
270200940919     C     ORGFVA        ANDEQ     ' '
270300940920     C                   MOVEL     *BLANKS       V2DCCR
270400940920     C                   MOVEL     ORGDES        V2DCCR
270500940919     C                   ELSE
270600940919     C                   SETON                                        2846
270700940919     C                   MOVEL     *ALL'?'       V2DCCR
270800940920     C                   MOVEL     ERR(15)       $MSG
270900940919     C                   GOTO      FICTR2
271000940919     C                   END
271100940919     C                   END
271200940919     C*--------------------------------------------------------------*
271300940919     C                   END
271400940920     C*--------------------------------------------------------------*
271500940920     C* Controllo se devo gestire forzatura da apertura
271600960605     C     WFORZA        IFNE      'S'
271700960605     C     *INKA         OREQ      '0'
271800950104     C     V2CEVE        IFNE      *BLANKS
271900950105     C     *IN07         ANDEQ     '1'
272000960610     C                   SETON                                        19
272100950104     C                   MOVEL     V2CEVE        EVBCEV
272200950104     C                   MOVEL     *BLANKS       EVBATB
272300950104     C                   Z-ADD     WOGGI         EVBDTV
272400950104     C                   TIME                    EVBORV
272500950104     C                   Z-ADD     WFLE          EVBFLE
272600021114     C                   Z-ADD     o36cou        EVBCDU
272700950104     C                   Z-ADD     V2CLNP        EVBLNP
272800950104     C                   Z-ADD     V2CAAS        EVBAAS
272900950104     C                   Z-ADD     V2CNRS        EVBNRS
273000950104     C                   Z-ADD     V2CNSP        EVBNSP
273100950104     C                   Z-ADD     WOGGI         EVBDEV
273200950113     C                   MOVEL     EVBORV        EVBHEV
273300950104     C                   MOVEL     *BLANKS       EVBFTR
273400950104     C                   Z-ADD     0             EVBDTR
273500990803     C                   MOVEL     *BLANKS       EVBNOT
273600990803     C                   Z-ADD     0             EVBDTC
273700990803     C                   WRITE     FNEVB000
273800051114    1c                   if        gescomm = 'S'
273900051114     c                   commit
274000051114     c                   end
274100950104     C*
274200950104     C     D05RIC        IFEQ      'S'
274300950104     C                   MOVEL     'S'           WFINE
274400950104     C                   ELSE
274500950104     C                   MOVEL     '1'           WTPVID
274600950224     C                   MOVEL     D05GES        SAVGES
274700950224     C                   CLEAR                   FNLG05
274800950224     C                   MOVEL     SAVGES        D05GES
274900950104     C                   EXSR      PULVID
275000950104     C                   END
275100961001     C                   ELSE
275200961001     C*  Se evento trovato ma non ancora trascorsi i giorni vado a fine
275300961001     C*  se non effettuata forzatura
275400961001     C     V2CEVE        IFNE      *BLANKS
275500961001     C     *IN33         ANDEQ     '0'
275600961001     C     WFORZA        ANDEQ     'S'
275700961001     C     *INKA         ANDEQ     '0'
275800961001     C*
275900961001     C                   SETON                                        19
276000961001     C     D05RIC        IFEQ      'S'
276100961001     C                   MOVEL     'S'           WFINE
276200961001     C                   ELSE
276300961001     C                   MOVEL     '1'           WTPVID
276400961001     C                   MOVEL     D05GES        SAVGES
276500961001     C                   CLEAR                   FNLG05
276600961001     C                   MOVEL     SAVGES        D05GES
276700961001     C                   EXSR      PULVID
276800961001     C                   END
276900961001     C                   END
277000961001     C*
277100960611     C                   END
277200950104     C*
277300940920     C                   END
277400940919     C*--------------------------------------------------------------*
277500940919     C     FICTR2        ENDSR
277600900418     C*---------------------------------------------------------------*
277700950105     C* Controllo esistenza evento
277800950105     C*---------------------------------------------------------------*
277900950105     C     CTREVE        BEGSR
278000950105     C*
278100950105     C                   SETON                                        07
278200950105     C                   MOVE      V2CEVE        KCEV
278300950105     C                   Z-ADD     1             X
278400950105     C     V2CEVE        LOOKUP    C2A(X)                                 32
278500950105     C* Posizionamento su file degli eventi x reperire data
278600950105     C* tentata consegna
278700950105     C                   Z-ADD     *HIVAL        KDEV
278800990803     C     KEVB1         SETLL     FNEVB05L
278900990803     C     KEVB2         READPE    FNEVB05L                               33
279000960625     C  N33KCEV          COMP      EVBCEV                             3333
279100960625     C*
279200950105     C     *IN33         IFEQ      '0'
279300950105     C                   SETOFF                                       07
279400950105     C                   Z-ADD     EVBDEV        DATADA
279500950105     C                   Z-ADD     0             GGF
279600950105     C     G2A(X)        IFNE      0
279700950105     C                   Z-ADD     WOGGI         DATAA
279800990906     C                   CALL      'XGGFEST'
279900950105     C                   PARM                    GFES8
280000950105     C                   END
280100950105     C*
280200950105     C                   Z-ADD     DATADA        G02INV
280300950105     C                   MOVE      '3'           G02ERR
280400950105     C                   CALL      'XSRDA8'
280500950105     C                   PARM                    WLBDA8
280600950105     C     G02TGI        ADD       G2A(X)        GIOTGI
280700950105     C                   ADD       GGF           GIOTGI
280800950105     C                   CALL      'XSRGI8'
280900950105     C                   PARM                    DATPAR
281000950105     C*
281100950105     C     GIOINV        IFLT      WOGGI
281200950105     C                   MOVEL     'S'           WTREVE            1
281300950105     C                   END
281400960625     C                   ELSE
281500960625     C* ARRIVO !!!!
281600960625     C     D05GES        IFEQ      'A'
281700960625     C                   Z-ADD     0             KDEV
281800960625     C* Se una riapertura controllo se è già stato immesso un
281900960625     C* evento di attesa successivo ad essa
282000960625     C     WRIAPE        IFEQ      'S'
282100961001     C                   Z-ADD     GCPDUR        G02INV
282200961001     C                   MOVE      *ZEROS        G02DAT
282300961001     C                   MOVE      '3'           G02ERR
282400960625     C                   CALL      'XSRDA8'
282500960625     C                   PARM                    WLBDA8
282600960625     C                   MOVE      G02INV        KDEV
282700960625     C                   END
282800960625     C*  Mi posiziono sugli eventi successivi alla data consegna per
282900960625     C*  verificare se ci sono degli eventi di attesa
283000990803     C     KEVB1         SETLL     FNEVB05L
283100990803     C     KEVB2         READE     FNEVB05L                               34
283200961001     C     *IN34         DOWEQ     '0'
283300960625     C     KCEV          ANDNE     EVBCEV
283400990803     C  N34KEVB2         READE     FNEVB05L                               34
283500960625     C                   END
283600960625     C* se dopo l'ultima apertura è stato emesso un evento di giac.
283700960625     C* non devo più accettarne un altro
283800960625     C     KCEV          IFEQ      EVBCEV
283900960625     C                   SETOFF                                       07
284000960625     C                   MOVEL     'S'           WTREVE            1
284100950105     C                   END
284200960625     C                   END
284300950105     C*
284400960625     C                   END
284500950105     C*
284600950105     C                   ENDSR
284700970226     C*---------------------------------------------------------------*
284800970226     C*   C T R F T X : Controllo se devo scrivere x ulteriore motiv. *
284900970226     C*---------------------------------------------------------------*
285000970226     C     CTRFTX        BEGSR
285100970226     C*
285200970226     C* Controllo se cliente usa lista labarta o VGL
285300970226     C                   Z-ADD     1             X
285400970226     C     V2CSCM        LOOKUP    KSC(X)                                 32
285500970226     C     *IN32         IFEQ      '1'
285600970226     C                   MOVEL     TLI(X)        WTPLIS            1
285700970226     C                   Z-ADD     1             X
285800970227     C     V2CCMC        LOOKUP    E2A(X)                                 32
285900970226     C* Se cliente usa lista Labrta verifico se flag x tale lista
286000970226     C* prevede la descrizione aggiuntiva obbligatoria altrimenti
286100970226     C* controllo se lista VGL vuole motivazione aggiun.obbligat.
286200970226     C     WTPLIS        IFEQ      'S'
286300970227     C     FTL(X)        ANDEQ     'S'
286400970226     C     WTPLIS        OREQ      ' '
286500970227     C     FTV(X)        ANDEQ     'S'
286600970310     C     V2CDMC        ORNE      *BLANKS
286700970310     C     *IN03         ANDEQ     '0'
286800970226     C                   SETON                                        2843
286900970226     C                   MOVEL     ERR(23)       $MSG
287000970226     C                   END
287100970226     C                   END
287200970226     C*
287300970226     C                   ENDSR
287400970624     C*---------------------------------------------------------------*
287500970624     C*   CALL03: Richiamo pgm per visualizzazione note apertura      *
287600970624     C*---------------------------------------------------------------*
287700970624     C     CALL03        BEGSR
287800970624     C*
287900970624     C* Imposto parametri
288000970624     C                   CLEAR                   FNLG03
288100970624     C                   Z-ADD     V2CLNP        D03LNP
288200970624     C                   Z-ADD     V2CNRS        D03NRS
288300970624     C                   Z-ADD     V2CNSP        D03NSP
288400970624     C                   Z-ADD     V2CDSP        D03DSP
288500970624     C                   MOVEL     V2CTBL        D03CBO
288600970624     C                   MOVEL     V2DTBL        D03DBO
288700970624     C                   Z-ADD     GCPFGC        D03FGC
288800970624     C                   MOVEL     V2DLNA        D03DFG
288900970624     C                   Z-ADD     V2CNGC        D03NGC
289000970624     C                   Z-ADD     V2CDTG        D03DGC
289100970624     C                   Z-ADD     V2CDUR        D03DUR
289200970624     C                   Z-ADD     GCPNRC        D03NRC
289300970624     C                   Z-ADD     V2CDXD        D03DXD
289400970624     C                   MOVEL     V2CCCR        D03CCR
289500970624     C                   MOVEL     V2DCCR        D03DCR
289600970624     C                   MOVE      V2CSCM        D03KSC
289700970624     C                   Z-ADD     V2CRMN        D03RMN
289800970624     C                   MOVEL     V2CRMA        D03RMA
289900970624     C                   MOVEL     V2CRSM        D03RSM
290000970624     C                   MOVEL     V2CINM        D03INM
290100970624     C                   MOVEL     V2CCAM        D03CAM
290200970624     C                   MOVEL     V2CLOM        D03LOM
290300970624     C                   MOVEL     V2CPRM        D03PRM
290400970624     C                   MOVEL     V2CNZM        D03NZM
290500970624     C                   MOVEL     V2CRSD        D03RSD
290600970624     C                   MOVEL     V2CDE2        D03RS2
290700970624     C                   MOVEL     V2CIND        D03IND
290800970624     C                   MOVEL     V2CCAD        D03CAD
290900970624     C                   MOVEL     V2CLOD        D03LOD
291000970624     C                   MOVEL     V2CPRD        D03PRD
291100970624     C                   MOVEL     V2CNZD        D03NZD
291200970624     C                   MOVEL     V2CCMC        D03CMC
291300970624     C                   MOVEL     V2DCMC        D03CMD
291400970624     C                   MOVEL     V2CDMC        D03DMC
291500970624     C                   MOVEL     'M'           D03TPV
291600970624     C                   CALL      'FNLG03R'
291700970624     C                   PARM                    FNLG03
291800970624     C*
291900970624     C                   ENDSR
292000950105     C*---------------------------------------------------------------*
292100900418     C*   G C P W T R                                                 *
292200900418     C*---------------------------------------------------------------*
292300900418     C     GCPWTR        BEGSR
292400940919     C*---------------------------------------------------------------*
292500940919     C*  Arrivo !!!
292600050512    1C     D05GES        IFEQ      'A'
292700050420     c                   clear                   gcpriap
292800050412     C                   ADD       1             CNT
292900050512    2C     WRIAPE        IFEQ      'S'
293000941223     C                   Z-ADD     GCPAGC        KAGC
293100941223     C                   Z-ADD     GCPNGC        KNGC
293200941223     C                   Z-ADD     GCPFGC        KFGC
293300050217     C     KGNP          CHAIN     tIGNP01L                           31
293400940919     C                   Z-ADD     CNT           GNPFRG
293500940919     C* Aggiorno motivo giacenza con nuovo prog.
293600050512    3C     *IN31         DOWEQ     '0'
293700050512    4C     GNPTPN        IFNE      'I'
293800970423     C     GNPFAS        OREQ      35
293900050217     C                   UPDATE    tIGNP000
294000050512    4C                   END
294100050217     C     KGNP          READE     tIGNP01L                               31
294200941223     C                   Z-ADD     CNT           GNPFRG
294300050512    3C                   ENDdo
294400050217     C* Chiudo giacenza mettendola a fase 40
294500050315     C     KSPE2         CHAIN(e)  tIGCP01L                           31
294600050512    3c                   if        %error
294700070521     C     KSPE2         CHAIN(n)  tIGCP01L
294800050315     c                   exsr      geslck
294900050318     C     KSPE2         CHAIN(e)  tIGCP01L                           31
295000050512    3c                   endif
295001170922     c                   eval      tigcpb = tigcp
295100941223     C                   Z-ADD     CNT           GCPFRG
295200050217     C                   Z-ADD     40            GCPFAS
295300050217     C                   WRITE     tIGCP000
295301170922     c                   eval      ILG33CTA = 'M'
295302170922     c                   eval      ILG33PRU = knmus
295303170922     c                   eval      ILG33NOJ = knmeb
295304170922     c                   eval      ILG33PGM = 'FNLG05R'
295305170922     c                   eval      tigcpa = tigcp
295306170922     c                   call      'FNLG33R'
295307170922     c                   parm                    fnlg33ds
295308170922     c                   parm                    tigcpb
295309170922     c                   parm                    tigcpa
295400050315     C     KSPE2         CHAIN(e)  tIGCP01L                           30
295500050512    3c                   if        %error
295600070521     C     KSPE2         CHAIN(n)  tIGCP01L
295700050315     c                   exsr      geslck
295800050318     C     KSPE2         CHAIN(e)  tIGCP01L                           30
295900050512    3c                   endif
295901170922     c   30              clear                   tigcpb
295902170922     c  n30              eval      tigcpb = tigcp
296000941223     C                   MOVE      WOGGI         GCPDUR
296100050217     c                   eval      gcpriap = 'R'
296200050607     c* se cod. giacenza 999 forzo tipo comunicazione = a Z
296300050607     c                   if        v2ccmc = '999'
296400050609     c                   eval      v2ctcm = 'R'
296500050607    2C                   END
296600050512    2C                   END
296700970401     C* Procedo all'eventuale chiusura anomalia 130
296800970401     C                   EXSR      CLO130
296900940919     C* Numero comunicazione interna
297000940919     C                   MOVE      GCPNGC        GCPNRC
297100950414     C* Abblenca comunque cod. motivo mancata consegna
297200940920     C     KSPE1         CHAIN     FNARB01L                           31
297300940919     C                   MOVEL     *BLANKS       ARBCMC
297400940919     C                   MOVEL     'G'           ARBFBC
297500940922     C  N31              EXCEPT    AGG
297600050512    2C     D05AAF        IFNE      0
297700940919     C                   Z-ADD     D05AAF        KAAS
297800940919     C                   Z-ADD     D05LNF        KLNP
297900940919     C                   Z-ADD     D05NRF        KNRS
298000940919     C                   Z-ADD     D05NSF        KNSP
298100940919     C     KSPE1         CHAIN     FNARB01L                           31
298200940919     C                   MOVEL     *BLANKS       ARBCMC
298300940919     C                   MOVEL     'G'           ARBFBC
298400940919     C  N31              EXCEPT    AGG
298500050512    2C                   END
298600050608     c                   eval      flgagg = 1
298700050517     c                   exsr      apegiaa
298800050608     c                   clear                   flgagg
298900050331     c* controllo che fase impostare 10/15 da FNCLS per apertura automatica
299000050331     c* imposto il apertura automatica = S se nell'anagrafica c'è
299100050331     c* blanks
299200050512    2c                   if        §2uape = 'S'
299300050512     c* per le autoconfermate se possibile (tab. 2F) stampa la
299400050512     c* comunicazione al mittente e porta la fase a 20
299500050513     c                   if        savs2f = 'S' or savf2f = 'N'
299600050512     c                   eval      gcpfas = 20
299700050512     c                   eval      gcpdmm = woggi
299800050513     C     D43TRV        IFEQ      'S'
299900050513     C                   MOVEL     'S'           v2cFST
300000050513     C                   END
300100050512     c                   else
300200050218     c                   eval      gcpfas = 15
300300050512     c                   end
300400050613     c                   eval      gcpapeaut = 'S'
300500050218     c                   else
300600050218     c                   eval      gcpfas = 10
300700050331     c                   eval      gcpapeaut = *blanks
300800050512    2c                   end
300900050517     C* Scrivo nuovo record giacenza con proggressivo a 0
301000050517    2C     *IN30         IFEQ      '1'
301100050517     C                   MOVEL     *BLANKS       GCPATB
301200050517     C                   MOVE      WHHDAT        GCPAGC
301300050517     C                   MOVE      DATEU         GCPMGC
301400050517     C                   Z-ADD     KFGC          GCPFGC
301500050517     C                   MOVEL     woggi         GCPDUR
301600050517     C* imposto i riferimenti della spedizione originale/partenza e arrivo.
301700050517     c* In caso di dirottamento. i campi della sped. orig./partenza li ho
301800050517     c* già memorizzati nei controlli
301900050517    3c                   if        £LBLAAO = 0
302000050517     C                   MOVEL     V2CLNP        GCPLNP
302100050517     C                   MOVEL     V2CAAS        GCPAAS
302200050517     C                   MOVEL     V2CNRS        GCPNRS
302300050517     C                   MOVEL     V2CNSP        GCPNSP
302400050517     C                   MOVEL     V2CLNA        GCPLNA
302500050517     c                   else
302600050517     C                   MOVEL     £LBLAAO       GCPAAS
302700050517     C                   MOVEL     £LBLLPO       GCPLNP
302800050517     C                   MOVEL     £LBLNRO       GCPNRS
302900050517     C                   MOVEL     £LBLNSO       GCPNSP
303000050517     C                   MOVEL     £LBLLAO       GCPLNA
303100050517    3c                   end
303200050517     C                   MOVEL     V2CLNP        GCPLNPa
303300050517     C                   MOVEL     V2CAAS        GCPAASa
303400050517     C                   MOVEL     V2CNRS        GCPNRSa
303500050517     C                   MOVEL     V2CNSP        GCPNSPa
303600050517     C                   MOVEL     V2CLNA        GCPLNAa
303700050517    2C                   END
303800040115      *in caso di data consegna richiesta oltre i limiti imposto la
303900050218      *data esecuzione disposizioni calcolando la data richiesta -1 gg lav.
304000090728      *se il flag della data consegna richiesta è diverso da 'D' = Dopo il
304100050512    2c                   if        d05cmc <> *blank and dcr(xdcr) <> *blank
304200040324     C                             AND arbdcr > woggi
304300040115     c                   move      arbdcr        dadata
304400040115     c                   move      arbdcr        adata
304500040115     c                   CALL      'XSRLAV8'
304600040115     c                   PARM                    Wdata8
304700040115     c                   z-add     giolav        giolavs           1 0
304800040115     c                   move      arbdcr        dataiso
304900090728     c                   if        arbtcr <> 'D'
305000050512    3c                   do        *hival
305100040115     c     dataiso       subdur    1:*d          dataiso
305200040115     c                   move      dataiso       dadata
305300040115     c                   move      arbdcr        adata
305400040115     c                   CALL      'XSRLAV8'
305500040115     c                   PARM                    Wdata8
305600050512    4c                   if        giolav = 0 or giolav > giolavs
305700040115     c                   leave
305800050512    4c                   endif
305900050512    3c                   enddo
306000090728     c                   endif
306100040115     c                   move      dataiso       gcpded
306200050405     C                   MOVE      V2CSCM        §QUATR
306300050512    3C     §QUATR        IFEQ      *ALL'8'
306400050405     C     §QUATR        OREQ      *ALL'9'
306500050405     C                   MOVEL     *ZEROS        GCPDDM
306600050405     c                   else
306700050405     C                   MOVEL     woggi         GCPDDM
306800050512    3c                   end
306900050512     c* se devo stampare la comunicazione al mittente forzo la fase
307000050512     c* 20 per poter chiamare il pgm FNLG07R in ristampa
307100050512     c* poi una volta stampata la comunicazione avanzo la fase
307200050513     c                   if        savs2f = 'S' or savf2f = 'N'
307300050512     c                   eval      gcpfas = 20
307400050512     c                   eval      gcpdmm = woggi
307500050513     C     D43TRV        IFEQ      'S'
307600050513     C                   MOVEL     'S'           v2cfst
307700050513     C                   END
307800050512     c                   else
307900050405     C                   z-add     15            GCPfas
308000050512     c                   end
308100050405     C                   MOVE      'C'           GCPDIS
308200050426     C                   MOVE      'S'           GCPapeaut
308300050405     C* imposto campi adebito spese in base al porto bolla
308400050405     c                   exsr      datarich
308500040329      * verifica se devo stampare la LDV
308600070410      * solo se non attivata gestione automatica distinte
308700070424     c                   if        §ogdda = *blank or §ogdda = '20391231' or
308800070424     c                             dateu < ogddan
308900070410    3c                   if        d05cmc = '100'
309000070410     c                   exsr      stampaLDV
309100070410    3c                   endif
309200070410    3c                   endif
309300040517     c                   else
309400040517     c                   z-add     0             gcpded
309500050405     c                   z-add     0             gcpddm
309600050405     C                   MOVE      *BLANKS       GCPDIS
309700050405     C                   MOVE      *BLANKS       GCPPSG
309800050405     C                   MOVE      *BLANKS       GCPASG
309900050405     C                   MOVE      *BLANKS       GCPPPC
310000050405     C                   MOVE      *BLANKS       GCPAPC
310100050512    2c                   endif
310200950414     C* Controllo se devo scrivere evento generico di giac.
310300050512    2C     WWTR          IFEQ      'S'
310400040401     C                   MOVEL     v2ccmc        EVBCEV
310500950414     C                   MOVEL     *BLANKS       EVBATB
310600950414     C                   Z-ADD     WOGGI         EVBDTV
310700950414     C                   TIME                    EVBORV
310800950414     C                   Z-ADD     GCPFGC        EVBFLE
310900021114     C                   Z-ADD     o36cou        EVBCDU
311000950414     C                   Z-ADD     V2CLNP        EVBLNP
311100950414     C                   Z-ADD     V2CAAS        EVBAAS
311200950414     C                   Z-ADD     V2CNRS        EVBNRS
311300950414     C                   Z-ADD     V2CNSP        EVBNSP
311400950414     C                   Z-ADD     WOGGI         EVBDEV
311500950414     C                   MOVEL     EVBORV        EVBHEV
311600950414     C                   MOVEL     *BLANKS       EVBFTR
311700950414     C                   Z-ADD     0             EVBDTR
311800990803     C                   MOVEL     *BLANKS       EVBNOT
311900990803     C                   Z-ADD     0             EVBDTC
312000990803     C                   WRITE     FNEVB000
312100050512    2C                   END
312200050512   x1C                   ELSE
312300050217     C*  Partenza !!!
312400940919     C                   Z-ADD     15            GCPFAS
312500050608     c* metto la fase a 20 se devo anche stampare
312600050608     c                   if        savs2f = 'S' or savf2f = 'N'
312700050608     c                   eval      gcpfas = 20
312800050608     c                   eval      gcpdmm = woggi
312900050608     C     D43TRV        IFEQ      'S'
313000050608     C                   MOVEL     'S'           v2cFST
313100050608     C                   END
313200050608     c                   end
313300050512    1C                   END
313400940919     C                   MOVE      V2CSCM        GCPSCM
313500940919     C                   MOVE      V2CCMC        GCPCMC
313600940919     C                   MOVE      V2CCCR        GCPCCR
313700940919     C                   MOVE      V2CNRC        GCPNRC
313800050512    1C     GCPNRC        IFEQ      0
313900941223     C                   MOVE      GCPNGC        GCPNRC
314000050512    1C                   END
314100941223     C                   MOVE      V2CDXD        G02DAT
314200941223     C                   MOVE      *ZEROS        G02INV
314300941223     C                   MOVE      ' '           G02ERR
314400941223     C                   CALL      'XSRDA8'
314500941223     C                   PARM                    WLBDA8
314600941223     C                   MOVE      G02INV        GCPDXD
314700940919     C                   MOVE      V2CFST        GCPFST
314800940919     C                   MOVE      V2CTCM        GCPTCM
314900050512    1C     GCPFAS        IFNE      20
315000940919     C                   MOVEL     *ZEROS        GCPDMM
315100050512    1C                   END
315200040520      * carico data apertura x confronti
315300040520     c                   clear                   gcpdape           8 0
315400040520     c                   movel     gcpagc        gcpdape
315500040520     c                   move      gcpmgc        gcpdape
315600050512    1c                   if        dcr(xdcr) = *blank or
315700040520     c                             gcpded <= gcpdape or
315800040520     c                             gcpded <= gcpdur
315900950530     C                   MOVEL     *ZEROS        GCPDED
316000050412     C                   MOVE      *BLANK        GCPDIS
316100050413     C                   MOVEL     *BLANK        GCPPPC
316200050413     C                   MOVEL     *BLANK        GCPAPC
316300050413     C                   MOVEL     *BLANK        GCPVCA
316400050413     C                   MOVEL     *ZEROS        GCPCAS
316500050413     C                   MOVEL     *BLANK        GCPNPC
316600050413     C                   MOVEL     *BLANK        GCPVCS
316700050413     C                   MOVEL     *BLANK        GCPPSG
316800050413     C                   MOVEL     *BLANK        GCPASG
316900050512    1c                   end
317000940919     C                   MOVEL     *ZEROS        GCPDLD
317100940919     C                   MOVEL     *ZEROS        GCPFRG
317200940919     C                   MOVEL     *ZEROS        GCPDCG
317300940919     C                   MOVEL     *BLANK        GCPCFG
317400940919     C                   MOVEL     *ZEROS        GCPDLM
317500940919     C                   MOVEL     *ZEROS        GCPSGS
317600940919     C                   MOVEL     *ZEROS        GCPSGR
317700940919     C                   MOVEL     *ZEROS        GCPSGD
317800990625     C                   MOVEL     *BLANKS       GCPDIV
317900940919     C                   MOVEL     *ZEROS        GCPDSD
318000940919     C                   MOVEL     *ZEROS        GCPDSA
318100940919     C                   MOVEL     *ZEROS        GCPGGS
318200940919     C                   MOVEL     *ZEROS        GCPSGF
318300940919     C                   MOVEL     *ZEROS        GCPSGA
318400940919     C                   MOVEL     *ZEROS        GCPTFT
318500940919     C                   MOVEL     *ZEROS        GCPDBG
318600940919     C                   MOVEL     *ZEROS        GCPLPW
318700940919     C                   MOVEL     *ZEROS        GCPASW
318800940919     C                   MOVEL     *ZEROS        GCPNSW
318900940919     C                   MOVEL     *ZEROS        GCPNBW
319000940919     C                   MOVEL     *ZEROS        GCPKSC
319100940919     C                   MOVEL     *ZEROS        GCPDFT
319200940919     C                   MOVEL     *ZEROS        GCPNFT
319300940919     C                   MOVEL     *ZEROS        GCPFIV
319400050217     C                   MOVEL     *BLANK        GCPSede
319500051020     C                   MOVEL     *BLANK        GCPflr
319600050414      *solo in arrivo
319700050512    1c                   if        d05ges = 'A'
319800050217     C                   MOVEL     *ZEROS        GCPDeda
319900050217     C                   MOVEL     *ZEROS        GCPDlma
320000050217     C                   MOVEL     *ZEROS        GCPDuma
320100050217     C                   MOVEL     *ZEROS        GCPLPWa
320200050217     C                   MOVEL     *ZEROS        GCPASWa
320300050217     C                   MOVEL     *ZEROS        GCPNSWa
320400050217     C                   MOVEL     *ZEROS        GCPNBWa
320500050420     C*                  MOVEL     *BLANK        GCPriap
320600050512    1c                   endif
320700940919     c*
320800940919     C* Scrive ulteriore motivo giacenza solo se non è blanks
320900950302     C                   Z-ADD     GCPAGC        KAGC
321000950302     C                   Z-ADD     GCPFGC        KFGC
321100950302     C                   Z-ADD     GCPNGC        KNGC
321200950302     C                   Z-ADD     GCPFRG        KFRG
321300050407     C                   Z-ADD     10            KFAS
321400050217     C     KGNP1         CHAIN     tIGNP01L                           31
321500940919     C                   MOVE      V2CDMC        GNPDMC
321600050512    1C     *IN31         IFEQ      '1'
321700050512    2C     GNPDMC        IFNE      *BLANKS
321800940919     C                   Z-ADD     GCPAGC        GNPAGC
321900940919     C                   Z-ADD     GCPFGC        GNPFGC
322000940919     C                   Z-ADD     GCPNGC        GNPNGC
322100940919     C                   Z-ADD     GCPFRG        GNPFRG
322200940919     C                   MOVEL     *BLANKS       GNPTPN
322300990625     C                   Z-ADD     0             GNPDIF
322400990625     C                   Z-ADD     0             GNPNRI
322500050407     C                   Z-ADD     10            GNPFAS
322600940919     C                   Z-ADD     1             GNPNPR
322700050217     C                   WRITE     TIGNP000
322800050512    2C                   END
322900911206     C                   ELSE
323000050217     C                   UPDATE    tIGNP000
323100050512    1C                   END
323200050218     c* controllo se devo fare il reso forzato per numero giacenze > di
323300050629     c* quello consentito dalla particolarità giacenza escludendo le
323400050629     c* riaperture tecniche
323500050629    1C                   if        gcpriap = 'R' and  gcpcmc <> '999' and
323600050629     C                             §7Q5RP <> 0 and (cnt-cntced-1) >= §7Q5RP
323700050405     C                   Z-ADD     woggi         gcpDMM
323800050218     C                   Z-ADD     26            gcpFAS
323900050809     C                   MOVEl     'M'           gcppsg
324000050809     C                   MOVEl     'S'           gcpasg
324100050809     C                   MOVEl     ' '           gcpppc
324200050809     C                   MOVEl     'N'           gcpapc
324300050809     C                   MOVE      'R'           gcpDIS
324400050512     C                   MOVE      'R'           gcpTCM
324500050218     C                   Z-ADD     0             gcpddm
324600050218     C                   Z-ADD     0             gcpded
324700050218     c                   clear                   gcpapeaut
324800050512    1c                   end
324900050217     C   30              WRITE     tIGCP000
325000050217     C  N30              UPDATE    tIGCP000
325001171010      * solo in arrivo verifica se apertura o riapertura
325002171010     c                   if        d05ges = 'A'
325003171010     c  n30              eval      ILG33CTA = 'R'
325004171010     c   30              eval      ILG33CTA = 'A'
325005171010     c                   eval      ILG33PRU = knmus
325006171010     c                   eval      ILG33NOJ = knmeb
325007171010     c                   eval      ILG33PGM = 'FNLG05R'
325008171010     c   30              clear                   tigcpb
325009171010     c                   eval      bgcpfas = 10
325010171010     c* forzo fase a 0 per scrittura testata riapertura
325013171010     c                   eval      tigcpa = tigcp
325014171010     c                   call      'FNLG33R'
325015171010     c                   parm                    fnlg33ds
325016171010     c                   parm                    tigcpb
325017171010     c                   parm                    tigcpa
325018171010      *se ho scritto un rekord di riapertura ma ho disposizioni forzate di reso
325019171010      * scrivo anche una riga in modifica per evidenziare
325020171010     c                   if        ilg33cta = 'R'
325021171010     c                             and gcpfas  < 35
325022171010     c                             and gcpfas  > 20
325023171010     c                   eval      bgcpfas = 10
325024171010     c                   eval      agcpfas = gcpfas
325025171010     c                   eval      ilg33cta = 'M'
325026171010     c                   call      'FNLG33R'
325027171010     c                   parm                    fnlg33ds
325028171010     c                   parm                    tigcpb
325029171010     c                   parm                    tigcpa
325030171010     c                   endif
325031171010      * partenza
325032171010     c                   else
325033171010     c                   if        bgcpfas = 10
325037171010     c                   eval      agcpfas = gcpfas
325038171010     c                   eval      ilg33cta = 'M'
325039171010     c                   eval      ILG33PRU = knmus
325040171010     c                   eval      ILG33NOJ = knmeb
325041171010     c                   eval      ILG33PGM = 'FNLG05R'
325042171010     c                   eval      tigcpa = tigcp
325043171010     c                   call      'FNLG33R'
325044171010     c                   parm                    fnlg33ds
325045171010     c                   parm                    tigcpb
325046171010     c                   parm                    tigcpa
325047171010     c                   endif
325048171010     c                   endif
325100070320      * se impostato flag abilitazione in anagrafica verifico se esistono
325200070321      * dei blocchi nel caso updato il rekord per abblencarli
325300070320     c                   if        §oggeot = 'S' and d05ges = 'A'
325400070321     c                   exsr      elibloc
325500070320     c                   endif
325600050614     C*  Stampo la comunicazione al mittente se se previsto nella tab. 2f
325700050608    1C                   if        gcpfas = 20
325800050512     c                   exsr      srstampa
325801170925      * richiamo forzato a pgm monitor variazioni per avanzamento automatico
325802170925      * fase e invio mail(forzo fase 15 rekord BEFORE) per scrivere variazione
325803171013     c                   if        d05ges = 'A'
325804170925     c                   eval      ILG33CTA = 'M'
325805170925     c                   eval      ILG33PRU = knmus
325806170925     c                   eval      ILG33NOJ = knmeb
325807170925     c                   eval      ILG33PGM = 'FNLG05R'
325808170925     c                   eval      tigcpb = tigcp
325809170925     c                   eval      bgcpfas = 15
325810170925     c                   eval      tigcpa = tigcp
325811170925     c                   call      'FNLG33R'
325812170925     c                   parm                    fnlg33ds
325813170925     c                   parm                    tigcpb
325814170925     c                   parm                    tigcpa
325815171013    1c                   endif
325900050512    1c                   end
326000050512     C*  Scrivo il FIGIC00F per aggiornare titas e tncsb solo per APERTURA
326100050512    1c                   if        *in30 and d05ges = 'A' and gcpriap = ' '
326200050516     c                             and not *in28
326300050512     c                   exsr      gicwtr
326400050512    1c                   end
326500950622     C*  Partenza !!
326600050512    1C     D05GES        IFEQ      'P'
326700050217     C     KSPE1         CHAIN     tIGAP01L                           31
326800940919     C                   MOVE      V2CSCM        §QUATR
326900050512    2C     §QUATR        IFEQ      *ALL'8'
327000900423     C     §QUATR        OREQ      *ALL'9'
327100940919     C                   MOVEL     V2CLNP        GAPLNP
327200940919     C                   MOVEL     V2CAAS        GAPAAS
327300940919     C                   MOVEL     V2CNRS        GAPNRS
327400940919     C                   MOVEL     V2CNSP        GAPNSP
327500940919     C                   MOVEL     V2CRSM        GAPRSM
327600940919     C                   MOVEL     V2CINM        GAPINM
327700940919     C                   MOVEL     V2CLOM        GAPLOM
327800940919     C                   MOVEL     V2CCAM        GAPCAM
327900940919     C                   MOVEL     V2CPRM        GAPPRM
328000050217     C  N31              UPDATE    tIGAP000
328100050217     C   31              WRITE     tIGAP000
328200050512   x2C                   ELSE
328300050217     C  N31              DELETE    tIGAP000
328400050512    2C                   END
328500050608     C*  Se bolla estero oppure se ho stampato la comunicazione al mit.
328600050608     C*  sfleggo la bolla per trasmettere informazione al cliente
328700050608c    c                   if        lnpfl1 = 'E' or
328800050615c    c                             (§3kcq5 = 'S' and savv2f <>'N' and
328900050615     C                             (§3KGQI = 'A' or §3KGQI ='E'))
329000050217     C     KSPE1         CHAIN     FNBLP01L                           31
329100050512    3C     *IN31         IFEQ      '0'
329200050217     C                   MOVEL     *BLANKS       BLPFT3
329300050217     C                   Z-ADD     0             BLPDT3
329400961204     C                   EXCEPT    SFLEGG
329500050512    3C                   END
329600050512    2C                   END
329700961204     C*
329800050512    1C                   END
329900921103     C*
330000050315      * gestione del commit se il flag
330100050516    1c                   if        gescomm = 'S'
330200050516     c                   if        not *in28
330300050315     c                   commit
330400050516     c                   else
330500050516     c                   rolbk
330600050512    1c                   endif
330700050516    1c                   endif
330800050315     C*
330900900418     C                   ENDSR
331000070321      *---------------------------------------------------------*
331100070321      *  elimina blocchi alla consegna da bolla data x consegnata
331200070321      *---------------------------------------------------------*
331300070321     c     elibloc       begsr
331400070321     C                   clear                   dsbl4l
331500070321     C                   move      'L'           KTRC
331600070321     C     Kar4l         chain     fiar401l
331700070321     C                   if        %found(fiar401l)
331800070321     C                   movel     ar4not        dsbl4l
331900070321     c                   move      *blank        §b4lbas
332000070321     c                   move      *blank        §b4lbge
332100070521      * solo se contrassegno già abilitato
332200070521     c                   if        arbacc = 'S'
332300070321     c                   move      *blank        §b4ltca
332400070521     c                   endif
332500070321     c                   move      *blank        §b4ltfd
332600070321     C                   movel     dsbl4l        ar4not
332700070321     c                   update    fiar4000
332800070321     C                   endif
332900070321     c
333000070321     C                   endsr
333100040329     C*--------------------------------------------------------------*
333200040329     C*  Richiamo pgm stampa lettera di vettura per ddt no mai stampati
333300040329     C*--------------------------------------------------------------*
333400040329     C     stampaldv     BEGSR
333500040329     c                   if        ovr = *blank
333600040329     c                   z-add     99            lung
333700040329      * eseguo ovrprtf - A4
333800040329     C                   movea(P)  cma4(1)       w99
333900040329     C                   movea     d05prb4       w99(24)
334000040329     C                   movea     d05mdb4       w99(46)
334100040329     C                   movea     w99           comman
334200040329     c                   move      'SHARE(*YES)' comman
334300040329     C                   call      'QCMDEXC'
334400040329     C                   parm                    comman
334500040329     C                   parm                    lung
334600040329      * eseguo ovrprtf - A5
334700040329     C                   movea(P)  cma5(1)       w99
334800040329     C                   movea     d05prb5       w99(24)
334900040329     C                   movea     d05mdb5       w99(46)
335000040329     C                   movea     w99           comman
335100040329     c                   move      'SHARE(*YES)' comman
335200040329     C                   call      'QCMDEXC'
335300040329     C                   parm                    comman
335400040329     C                   parm                    lung
335500040329     c                   move      'X'           ovr               1
335600040329     c                   endif
335700040329      *
335800040329     C                   clear                   dsbl4a
335900040329     C                   movel     'A'           ktrc
336000040329     C                   movel     *blanks       ar4not
336100060213     c     kar4          chain(n)  fiar401l                           31
336200040329     c                   if        not *in31
336300040329     C                   movel     ar4not        dsbl4a
336400040329     c                   if        §b4abm = 'N' or §b4abm = 'K'
336500130315     c                   reset                   FNLSB6ds1
336600040329     C                   CLEAR                   fnlsb5ds
336700040329     C                   z-add     kaas          DB0AAS
336800040329     C                   z-add     klnp          DB0LNP
336900040329     C                   z-add     knrs          DB0NRS
337000040329     C                   z-add     knsp          DB0NSP
337100040329     C                   MOVEL     'A'           DB0TBO
337200040329     C                   MOVEL     ' '           DB0RIS
337300040329     c                   call      D05psl
337400040329     c                   parm                    fnlsb5ds
337500130315     c                   parm                    FNLSB6ds1
337600060213     C     KAR4          CHAIN     FiAR401L                           31
337700040329     c                   if        not *in31
337800040329     c                   movel     ar4not        dsbl4a
337900040329     c                   Select
338000040329     c                   When      §B4Abm = 'N'
338100040329     c                   Eval      §B4Abm = 'J'
338200040329     c                   When      §B4Abm = 'K'
338300040329     c                   Eval      §B4Abm = 'W'
338400040329     c                   EndSl
338500040329
338600040329     c                   z-add     woggi         §b4asl
338700040329     c                   movel     dsbl4a        ar4not
338800060213     C                   update    FiAR4000
338900040329      *
339000040329     c                   endif
339100040329     c                   endif
339200040329     c                   endif
339300040329     C                   ENDSR
339400970401     C*--------------------------------------------------------------*
339500970401     C*  Call a pgm Chiusura anomalia 130
339600970401     C*--------------------------------------------------------------*
339700970401     C     CLO130        BEGSR
339800970401     C*
339900970401     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
340000970401     C                   CLEAR                   DSLR33
340100970401     C                   MOVE      '4'           D33FSC
340200970401     C                   Z-ADD     WOGGI         D33DCH
340300970401     C*
340400970401     C                   Z-ADD     V2CLNP        KLNP2
340500970401     C                   Z-ADD     V2CNRS        KNRS2
340600970401     C                   Z-ADD     V2CNSP        KNSP2
340700970401     C                   Z-ADD     V2CAAS        KAAS2
340800970401     C                   Z-ADD     130           KCAA
340900990518     C     KANM          CHAIN     FNANM01L                           37
341000970401     C     *IN37         DOWEQ     '0'
341100970401     C     ANMFSA        IFEQ      'G'
341200970401     C                   Z-ADD     ANMNR1        D33NRR
341300970401     C                   CALL      'FNLR33R'
341400970401     C                   PARM                    DSLR33
341500970401     C                   END
341600990518     C     KANM          READE     FNANM01L                               37
341700970401     C                   END
341800970401     C*
341900970401     C                   ENDSR
342000961028     C*---------------------------------------------------------------*
342100050512     C* srstampa - stampa comincazione al mittente per le autoconfer. *
342200961028     C*---------------------------------------------------------------*
342300050512     C     srstampa      BEGSR
342400050516     c                   do
342500050512     c* se previsto (tab. 2F) chiamo anche il pgm di scrittura del VAG
342600050513     C                   if        savv2f <>'N' and (§3KGQI = 'A' or
342700050512     C                             §3KGQI ='E')
342800050512     c                   clear                   fnlg08ds
342900050512     c                   eval      D08AGC= gcpagc
343000050512     c                   eval      D08FGC= gcpfgc
343100050512     c                   eval      D08NGC= gcpngc
343200050512     c                   eval      D08uni= §3kgks
343300050606     c                   eval      D08GSD= §3kGSD
343400050513     c                   eval      D08est= save2f
343500050516     c                   eval      D08commit= 'N'
343600050608     c                   eval      D08pgm= 'FNLG05R'
343700050512     c                   movel(p)  fnlg08ds      kpjbu
343800050516     c                   call      'FNLG08R'                            28
343900050512     c                   parm                    kpjba
344000050516     c                   movel     kpjbu         fnlg08ds
344100050516     c                   if        d08ok = 'N'
344200050516     c                   seton                                        28
344300050516     c                   end
344400050516     c                   if        *in28
344500050516     c                   leave
344600050516     c                   end
344700050516     c                   end
344800050516     C* lancio il pgm di stampa in modalità ristampa
344900050516     c                   if        savs2f = 'S'
345000050516     c                   clear                   fnlg07ds
345100050516     c                   eval      D07RIS= 'S'
345200050516     c                   eval      D07RAC= 'N'
345300050516     c                   eval      D07AGC= gcpagc
345400050516     c                   eval      D07FGC= gcpfgc
345500050516     c                   eval      D07NGC= gcpngc
345600050518     c* attenzione al parametri d07fil: per fax e posta non è impostato
345700050518     c* correttamente verificare il pgm FNLG07R
345800050518     c                   eval      D07fil= gcplnp
345900050516     c                   eval      D07tip= v2ctcm
346000050516     c                   movel(p)  fnlg07ds      kpjbu
346100050516     c                   call      'FNLG07R'                            28
346200050516     c                   parm                    kpjba
346300050516     c                   movel     kpjbu         fnlg07ds
346400050516     c                   if        d07ok = 'N'
346500050516     c                   seton                                        28
346600050516     c                   end
346700050516     c                   if        *in28
346800050516     c                   leave
346900050516     c                   end
347000050516     c                   end
347100170214      *richiamo pgm invio avviso al destinatario
347200170214     c                   if        gcpriap = *blank
347300170214     c                   clear                   fnlg071ds
347400170214     c                   movel     gcpAAS        i071AAS
347500170214     c                   movel     gcpLNP        i071LNP
347600170214     c                   movel     gcpNRS        i071NRS
347700170214     c                   move      gcpNSP        i071NSP
347800170214     c                   movel     gcpLNa        i071LNa
347900170214     c                   move      'A'           i071CHI
348000170214     c                   movel     gcpcmc        i071cmc
348100170214     c                   call      'FNLG07R1'                           28
348200170214     c                   parm                    kpjba
348300170214     c                   parm                    fnlg071ds
348400170214     c                   endif
348500050516     c* per data consegna richiesta devo avanzare la fase a seconda
348600050516     c* che sia un codificato oppure no
348700050516     c                   if        dcr(xdcr) <> ' '  and
348800050516     c                             gcpdis <> *blank and gcpdis <> 'R'
348900050516     c* non ho ancora committato quindi il record è ancora mio
349000051125     C                   Z-ADD     0             Kfrg
349100051125     c                   if        d05ges = 'P'
349200051125     C                   Z-ADD     gcpAAS        KAAS
349300051125     C                   Z-ADD     gcpLNP        KLNP
349400051125     C                   Z-ADD     gcpNRS        KNRS
349500051125     C                   Z-ADD     gcpNSP        KNSP
349600051125     c                   else
349700051125     C                   Z-ADD     gcpAASa       KAAS
349800051125     C                   Z-ADD     gcpLNPa       KLNP
349900051125     C                   Z-ADD     gcpNRSa       KNRS
350000051125     C                   Z-ADD     gcpNSPa       KNSP
350100051125     c                   end
350200050516     C     KSPE2         CHAIN     tIGCP01L
350201170922     c                   eval      tigcpa = tigcp
350300050606     C                   MOVE      GCPSCM        §QUATR
350400050516    3C     §QUATR        IFEQ      *ALL'8'
350500050516     C     §QUATR        OREQ      *ALL'9'
350600050516     C                   Z-ADD     25            GCPFAS
350700050516     c                   else
350800050516     c                   z-add     30            gcpfas
350900050516     c                   end
351000050516     C                   UPDATE    tIGCP000
351001170922     c                   eval      tigcpb = tigcp
351002170922     c                   eval      ILG33CTA = 'M'
351003170922     c                   eval      ILG33PRU = knmus
351004170922     c                   eval      ILG33NOJ = knmeb
351005170922     c                   eval      ILG33PGM = 'FNLG05R'
351006170922     c                   eval      tigcpa = tigcp
351007170922     c                   call      'FNLG33R'
351008170922     c                   parm                    fnlg33ds
351009170922     c                   parm                    tigcpb
351010170922     c                   parm                    tigcpa
351100050516     c                   end
351200050516     C                   enddo
351300050516     c*
351400050512     C                   ENDSR
351500050512     C*---------------------------------------------------------------*
351600050512     C*   Caricamento £6-£7                                           *
351700050512     C*---------------------------------------------------------------*
351800050512     C     CAR£6         BEGSR
351900961028     C* Carico £7 se siamo in partenza, carico £6 se siamo in arrivo
352000961028     C     D05GES        IFEQ      'P'
352100961028     C                   MOVEL     '£7'          KCOD
352200961028     C                   CLEAR                   KEY
352300961028     C                   MOVEL     *BLANKS       KKEY
352400961028     C                   MOVEL     §FLE          KKEY
352500961028     C                   Z-ADD     0             §F
352600961028     C     KTBL1         CHAIN     TABEL                              31
352700961028     C     *IN31         IFEQ      '0'
352800961028     C     TBLFLG        ANDEQ     ' '
352900961028     C                   MOVEA     TBLUNI        FI
353000020430     C                   DO        30            H                 2 0
353100961028     C     FI(H)         IFGT      *ZERO
353200961028     C                   ADD       1             §F
353300961028     C                   MOVE      FI(H)         KEY(§F)           3 0
353400961028     C                   END
353500961028     C                   END
353600961028     C                   ELSE
353700961028     C                   Z-ADD     1             §F                3 0
353800961028     C                   MOVE      §FLE          KEY(§F)
353900961028     C                   END
354000020430     C                   ELSE
354100961028     C*
354200020430     c                   clear                   trul06ds
354300020430     c                   eval      d06cod = '£6'
354400020430     c                   movel     §fle          d06key
354500021106     c                   movel     kpjbu         savkpjbu
354600020430     c                   movel     trul06ds      kpjbu
354700020430     c                   call      'TRUL06R'
354800020430     c                   parm                    kpjba
354900020430     c                   movel     kpjbu         trul06ds
355000020430     c                   movea     lin           key
355100021106     c                   movel     savkpjbu      kpjbu
355200020430     C                   END
355300020430     C*
355400961028     C                   ENDSR
355500940915     C*---------------------------------------------------------------*
355600940915     C*   Caricamento tabelle                                         *
355700940915     C*---------------------------------------------------------------*
355800940915     C     CARTAB        BEGSR
355900940915     C*---------------------------------------------------------------*
356000961028     C                   EXSR      CAR£6
356100961017     C*
356200961025     C* Carico £1
356300020430     c                   clear                   trul06ds
356400020430     c                   eval      d06cod = '£1'
356500020430     c                   movel     simfel        d06key
356600021106     c                   movel     kpjbu         savkpjbu
356700020430     c                   movel     trul06ds      kpjbu
356800020430     c                   call      'TRUL06R'
356900020430     c                   parm                    kpjba
357000020430     c                   movel     kpjbu         trul06ds
357100020430     c                   movea     lin           £1
357200021106     c                   movel     savkpjbu      kpjbu
357300990805     C*---------------------------------------------------------------*
357400990805     C* Carico 2G
357500990805     C                   MOVEL     '2G'          KCOD
357600990805     C                   MOVEL     *BLANKS       KKEY
357700990805     C                   MOVEL     '1'           KKEY
357800990805     C                   Z-ADD     0             §F
357900990805     C     KTBL1         CHAIN     TABEL                              31
358000990805     C     *IN31         IFEQ      '0'
358100990805     C     TBLFLG        ANDEQ     ' '
358200990805     C                   MOVEL     TBLUNI        DS2G
358300990805     C                   END
358400940915     C*---------------------------------------------------------------*
358500940915     C* Carico tabella 3a
358600071107     C                   Z-ADD     0             X
358700940915     C                   MOVEL     '3A'          KCOD
358800940915     C     KTBL2         CHAIN     TABEL                              31
358900940915     C     *IN31         DOWEQ     '0'
359000940915     C     X             ANDLT     50
359100940915     C     TBLFLG        IFEQ      ' '
359200940919     C                   MOVEL     TBLUNI        DS3A
359300940915     C                   ADD       1             X
359400940915     C                   MOVEL     TBLKEY        C3A(X)
359500940915     C                   MOVEL     §3ADES        D3A(X)
359600940915     C                   MOVEL     §3ATB1        T3A(X)
359700021031     C                   MOVEL     §3Absd        g3A(X)
359800940915     C                   END
359900940915     C     KTBL2         READE     TABEL                                  31
360000940915     C                   END
360100940915     C*  Se non ho caricato tutti i dati gestisco errore
360200940915     C     *IN31         IFNE      '1'
360300940915     C                   EXSR      ERRTAB
360400940915     C                   GOTO      FINTAB
360500940915     C                   END
360600940915     C*---------------------------------------------------------------*
360700940915     C* Carico tabella 2a
360800071107     C                   Z-ADD     0             X
360900940915     C                   MOVEL     '2A'          KCOD
361000940915     C     KTBL2         CHAIN     TABEL                              31
361100940915     C     *IN31         DOWEQ     '0'
361200080903     C     X             ANDLT     200
361300940915     C     TBLFLG        IFEQ      ' '
361400940919     C                   MOVEL     TBLUNI        DS2A
361500940915     C                   ADD       1             X
361600940915     C                   MOVEL     TBLKEY        C2A(X)
361700940915     C                   MOVEL     §2ADES        D2A(X)
361800950105     C                   MOVEL     §2AGLA        G2A(X)
361900950105     C                   MOVEL     §2AFLG        F2A(X)
362000950414     C                   MOVEL     §2AFTC        T2A(X)
362100940919     C                   END
362200940915     C     KTBL2         READE     TABEL                                  31
362300940915     C                   END
362400940915     C*  Se non ho caricato tutti i dati gestisco errore
362500940915     C     *IN31         IFNE      '1'
362600940915     C                   EXSR      ERRTAB
362700940915     C                   GOTO      FINTAB
362800940915     C                   END
362900950104     C*  Carico tabella correlata 2Z
363000950104     C                   MOVEL     '2Z'          KCOD
363100950104     C     KTBL2         CHAIN     TABEL                              31
363200950104     C     *IN31         DOWEQ     '0'
363300950104     C                   MOVEL     TBLUNI        DS2A
363400950104     C     TBLFLG        IFEQ      ' '
363500071107     C                   Z-ADD     1             X
363600950104     C                   MOVEL     TBLKEY        WCD2A             3
363700950104     C     WCD2A         LOOKUP    C2A(X)                                 32
363800950104     C     *IN32         IFEQ      '1'
363900950104     C                   MOVEL     TBLUNI        DS2Z
364000950104     C                   MOVE      §2ZTIG        TPG(X)
364100040115     C                   MOVE      §2Zdcr        dcr(X)
364200950104     C                   END
364300950104     C                   END
364400950104     C     KTBL2         READE     TABEL                                  31
364500950104     C                   END
364600940915     C*---------------------------------------------------------------*
364700940915     C* Carico tabella 2b
364800980105     C                   Z-ADD     *HIVAL        C2B
364900071107     C                   Z-ADD     0             X
365000940915     C                   MOVEL     '2B'          KCOD
365100940915     C     KTBL2         CHAIN     TABEL                              31
365200940915     C     *IN31         DOWEQ     '0'
365300940915     C     X             ANDLT     50
365400940915     C     TBLFLG        IFEQ      ' '
365500940915     C                   ADD       1             X
365600940915     C                   MOVEL     TBLKEY        C2B(X)
365700940915     C                   MOVEL     TBLUNI        D2B(X)
365800940915     C                   END
365900940915     C     KTBL2         READE     TABEL                                  31
366000940915     C                   END
366100940915     C*  Se non ho caricato tutti i dati gestisco errore
366200940915     C     *IN31         IFNE      '1'
366300940915     C                   EXSR      ERRTAB
366400940915     C                   GOTO      FINTAB
366500940915     C                   END
366600940915     C*---------------------------------------------------------------*
366700940915     C* Carico tabella 2f
366800970110     C                   MOVEA     *HIVAL        C2F
366900071107     C                   Z-ADD     0             X
367000940915     C                   MOVEL     '2F'          KCOD
367100940915     C     KTBL2         CHAIN     TABEL                              31
367200940915     C     *IN31         DOWEQ     '0'
367300940915     C     X             ANDLT     50
367400940915     C     TBLFLG        IFEQ      ' '
367500031027     C                   MOVEL     TBLUNI        DS2F
367600031027      * solo se non è riservato al CED lo carica
367700050614     c*                  if        §2Fris = *blank
367800940915     C                   ADD       1             X
367900940915     C                   MOVEL     TBLKEY        C2F(X)
368000940919     C                   MOVEL     §2FDES        D2F(X)
368100940920     C                   MOVEL     §2FFTM        F2F(X)
368200960201     C                   MOVEL     §2FFTP        T2F(X)
368300050512     C                   MOVEL     §2Fsta        s2F(X)
368400050512     C                   MOVEL     §2FVAG        V2F(X)
368500050512     C                   MOVEL     §2Ftxt        e2F(X)
368600050614     C                   MOVEL     §2Fris        r2F(X)
368700050614     c*                  end
368800940915     C                   END
368900940915     C     KTBL2         READE     TABEL                                  31
369000940915     C                   END
369100940915     C*  Se non ho caricato tutti i dati gestisco errore
369200940915     C     *IN31         IFNE      '1'
369300940915     C                   EXSR      ERRTAB
369400940915     C                   GOTO      FINTAB
369500940915     C                   END
369600940920     C*---------------------------------------------------------------*
369700940920     C* Carico tabella 7Q
369800071107     C                   Z-ADD     0             X
369900940920     C                   MOVEL     '7Q'          KCOD
370000940920     C     KTBL2         CHAIN     TABEL                              31
370100940920     C     *IN31         DOWEQ     '0'
370200940920     C     X             ANDLT     50
370300940920     C     TBLFLG        IFEQ      ' '
370400940920     C                   ADD       1             X
370500940920     C                   MOVEL     TBLKEY        C7Q(X)
370600950102     C                   MOVEL     TBLUNI        D7Q(X)
370700940920     C                   END
370800940920     C     KTBL2         READE     TABEL                                  31
370900940920     C                   END
371000940920     C*  Se non ho caricato tutti i dati gestisco errore
371100940920     C     *IN31         IFNE      '1'
371200940920     C                   EXSR      ERRTAB
371300940920     C                   GOTO      FINTAB
371400940920     C                   END
371500021031     C*---------------------------------------------------------------*
371600021031     C* Carico tabella 7w
371700071107     C                   Z-ADD     0             X
371800021031     C                   MOVEL     '7W'          KCOD
371900021031     C     KTBL2         CHAIN     TABEL                              31
372000021031     C     *IN31         DOWEQ     '0'
372100021031     C     X             ANDLT     50
372200021031     C     TBLFLG        IFEQ      ' '
372300021031     C                   ADD       1             X
372400021031     C                   MOVEL     TBLKEY        C7w(X)
372500021031     C                   MOVEL     TBLUNI        D7w(X)
372600021031     C                   END
372700021031     C     KTBL2         READE     TABEL                                  31
372800021031     C                   END
372900021031     C*  Se non ho caricato tutti i dati gestisco errore
373000021031     C     *IN31         IFNE      '1'
373100021031     C                   EXSR      ERRTAB
373200021031     C                   GOTO      FINTAB
373300021031     C                   END
373400950113     C*---------------------------------------------------------------*
373500950113     C* Carico CAMBI/DIVISE
373600011221     C                   Z-ADD     0             Y
373700950116     C                   MOVEL     '15'          KCOD
373800950113     C     KTBL2         CHAIN     TABEL                              31
373900950113     C     *IN31         DOWEQ     '0'
374000011221     C     Y             ANDLT     500
374100950113     C     TBLFLG        IFEQ      *BLANK
374200011221     C                   ADD       1             Y
374300950113     C                   MOVEL     TBLUNI        DS15
374400011221     C                   MOVEL     TBLKEY        CNZ(Y)
374500011221     C                   MOVEL     §15ITA        ITA(Y)
374600950113     C                   END
374700950113     C     KTBL2         READE     TABEL                                  31
374800950113     C                   END
374900940915     C*---------------------------------------------------------------*
375000940915     C*  Se non ho caricato tutti i dati gestisco errore
375100940915     C     *IN31         IFNE      '1'
375200940915     C                   EXSR      ERRTAB
375300940915     C                   END
375400090520     C*---------------------------------------------------------------*
375500090520     C* Carico tabella 5e
375600090520     C                   Z-ADD     0             X
375700090520     C                   MOVEL     '5E'          KCOD
375800090520     C     KTBL2         CHAIN     TABEL                              31
375900090520     C     *IN31         DOWEQ     '0'
376000090520     C     X             ANDLT     50
376100090520     C     TBLFLG        IFEQ      ' '
376200090520     C                   ADD       1             X
376300090520     C                   MOVEL     TBLKEY        C5e(X)
376400090520     C                   MOVEL     TBLUNI        D5e(X)
376500090520     C                   END
376600090520     C     KTBL2         READE     TABEL                                  31
376700090520     C                   END
376800090520     C*  Se non ho caricato tutti i dati gestisco errore
376900090520     C     *IN31         IFNE      '1'
377000090520     C                   EXSR      ERRTAB
377100090520     C                   GOTO      FINTAB
377200090520     C                   END
377300940915     C     FINTAB        ENDSR
377400091021     C*---------------------------------------------------------------*
377500091021     C* verifica se cod.cliente da forzare
377600091021     C*---------------------------------------------------------------*
377700091021     C     forzacli      BEGSR
377800091021     c                   if        d05ges = 'P'
377900091021     C     kar5p         chain     fiar501l
378000091021     c                   else
378100091021     C     kar5          chain     fiar501l
378200091021     c                   endif
378300091021     c                   if        %found(fiar501l)
378400091021     c                   movel     ar5uni        dar5gen
378500091021     c                   if        §ar5ass = 'T'
378600091021     c                   clear                   tibs02ds
378700091021     c                   movel     'C'           t02mod
378800091021     c                   movel     knsif         t02sif
378900091021     c                   movel     'CLI'         t02cod
379000091021     c                   if        d05ges = 'P'
379100091102     c                   movel     gcpscm        t02ke1
379200091021     c                   else
379300091021     c                   movel     arbksc        t02ke1
379400091021     c                   endif
379500091021     c                   clear                   t02ke2
379600091021     c                   call      'TIBS02R'
379700091021     c                   parm                    KPJBA
379800091021     c                   parm                    TIBS02DS
379900091021     c                   if        t02err = *blanks
380000091021     c                   movel     t02uni        dcli
380100091021     c                   if        §cligcp = 'S'
380200091021     c                   if        d05ges = 'P'
380300091102     c                   movel     gcpscm        v2cscm
380400091102     c                   movel     gcpscm        kksc
380500091021     c                   else
380600091102     c                   movel     arbksc        v2cscm
380700091102     c                   movel     arbksc        kksc
380800091021     c                   endif
380900091021     c                   endif
381000091021     c                   endif
381100091021     c                   endif
381200091021     c                   endif
381300091021     c
381400091021     c                   endsr
381500050217     C*---------------------------------------------------------------*
381600050218     C* Srcivo figic00f per allineare saltuariamente la sede (TITAS+TNCSB)
381700940915     C*---------------------------------------------------------------*
381800050218     C     gicwtr        BEGSR
381900050218     C                   MOVEL     'APGC'        GICCAV
382000050408     C                   Z-ADD     woggi         GICDTV
382100050218     C                   TIME                    GICORV
382200050411     C                   Z-ADD     GCPLNPa       GICFEV
382300050411     C                   Z-ADD     GCPLNPa       GICLNP
382400050411     C                   Z-ADD     GCPLNAa       GICLNA
382500050411     C                   Z-ADD     GCPAASa       GICAAS
382600050411     C                   Z-ADD     GCPNRSa       GICNRS
382700050411     C                   Z-ADD     GCPNSPa       GICNSP
382800050218     C                   MOVEL     GCPAGC        GICDGC
382900050218     C                   MOVE      GCPMGC        GICDGC
383000050218     C                   Z-ADD     GCPFGC        GICFGC
383100050218     C                   Z-ADD     GCPNGC        GICNGC
383200050218     C                   Z-ADD     0             GICDCH
383300050218     C                   WRITE     FIGIC000
383400050218     c
383500050218     C                   ENDSR
383600050405     C**---------------------------------------------------------
383700050405     c     datarich      begsr
383800050405     C**---------------------------------------------------------
383900050405     C                   MOVE      '2Z'          KCOD
384000050405     C                   MOVEL(p)  V2Ccmc        KKEY
384100050405     C     KTBL1         CHAIN     TABEL00F
384200050405     C                   IF        %found(tabel00f)
384300050405     C                   MOVEL     TBLUNI        DS2z
384400050405     c                   if        §2zdcr <> *blank
384500050405     c                   move      'X'           azzer             1
384600050405      * controllo il tipo bolla per l'attribuzione delle spese e provvigioni
384700050405     C                   MOVE      '3A'          KCOD
384800050405     C                   MOVEL(p)  blpcbo        kKEY
384900050405     C     KTbl1         CHAIN     TABEL00F
385000050405     C                   IF        %found(tabel00f)
385100050405     C                   MOVEL     TBLUNI        DS3a
385200050405     C                   MOVEL     §3ATB1        PORT              1
385300050405      * spese trasporto
385400050405     c                   if        port = 'F'
385500050405     c                   move      'M'           gcppsg
385600050405     c                   move      'S'           gcpasg
385700050405     c                   else
385800050405     c                   move      'D'           gcppsg
385900050405     c                   move      'S'           gcpasg
386000050405     c                   endif
386100050405      * provv. contrassegno
386200051116     c     kSPE1         chain     fiar901l
386300051116     c                   if        %found(fiar901l)
386400050405     c                   if        port = 'F'
386500050405     c                   if        §3atb2 <> *blank
386600050405     c                   move      'D'           gcpppc
386700050405     c                   move      'S'           gcpapc
386800050405     c                   else
386900050405     c                   move      'M'           gcpppc
387000050405     c                   move      'S'           gcpapc
387100050405     c                   endif
387200050405     c                   else
387300050405     c                   move      'D'           gcpppc
387400050405     c                   move      'S'           gcpapc
387500050405     c                   endif
387600050405      *no contrassegno
387700050405     c                   else
387800050405     c                   clear                   gcpppc
387900050405     c                   clear                   gcpapc
388000050405     c                   endif
388100050405
388200050405     C                   END
388300050405     C                   END
388400050405     C                   END
388500050405     c                   endsr
388600050218     C*---------------------------------------------------------------*
388700050218     C*   FNCLS + 3K                                                  *
388800050218     C*---------------------------------------------------------------*
388900050218     C     srcli         BEGSR
389000050217     c*
389100050217     C                   MOVEL     *BLANKS       CLSFLO
389200050401     C                   clear                   ds2u
389300050217     C     KKSC          CHAIN     FNCLS01L                           31
389400050217     C     CLSFLO        IFNE      *BLANKS
389500050217     c* se non ho trovato il tipo di comunicazione nelle tariffe
389600050217     c                   if        WTRTCM = 'N'
389700050217     C                   MOVE      CLSFLO        V2CTCM
389800050217     c                   end
389900050217     c* apertura giacenza autoconfermata
390000050401     c                   eval      kkey= %subst(clsflo: 9: 1)
390100050401     C                   MOVEL     '2U'          KCOD
390200050401     C     KTBL1         CHAIN     TABEL00f
390300050401     C                   IF        %found(tabel00f) and tblflg = ' '
390400050401     C                   MOVEl     TBLUNI        ds2u
390500050401     c                   else
390600050401     c                   eval      §2uape='N'
390700050401     C                   END
390800050217     C                   END
390900050606     C* SE PREVISTO IN TABELLA INVIO INFORMAZIONI BOLLE GIACENTI AL CLIENTI
391000050608     C* SFLEGGO LA BOLLA se sono in aggiornamento
391100050606     C                   MOVEL     '3K'          kCOD
391200050606     C                   MOVEL     *BLANKS       kKEY
391300050606     C     BLPCCM        IFNE      0
391400050606     C                   MOVEL     BLPCCM        kKEY
391500050606     C                   ELSE
391600050606     C                   MOVEL     BLPKSC        kKEY
391700050606     C                   END
391800050606     C     KTbl1         CHAIN     TABEL                              35
391900050606     C*
392000050606     C  n35              MOVEL     TBLUNI        DS3K
392100050606     C   35              clear                   DS3K
392200050608     C     d05ges        ifeq      'A'
392300050606     C     §3KCQ5        IFEQ      'S'
392400050608     C     flgagg        andeq     1
392500050606     C                   MOVEL     *BLANKS       BLPFT3
392600050606     C                   Z-ADD     0             BLPDT3
392700050606     C                   EXCEPT    SFLEGG
392800050608     c                   endif
392900050513     c* reperisco i dati della tabella 2F
393000050513     c                   clear                   savs2f            1
393100050513     c                   clear                   savf2f            1
393200050513     c                   clear                   savv2f            1
393300050513     c                   clear                   save2f            1
393400050513     c                   if        v2ctcm <> ' '
393500050517     c                   z-add     1             x
393600050513     C     V2CTCM        LOOKUP    C2F(X)                                 32
393700050513     c                   if        *in32
393800050513     c                   eval      savs2f = s2f(x)
393900050513     c                   eval      savf2f = f2f(x)
394000050513     c                   eval      savv2f = v2f(x)
394100050513     c                   eval      save2f = e2f(x)
394200050614      * reperimento luogo e ind. e-mail  mi serve per impostare gcpfst
394300050614     C                   CLEAR                   TRUL43
394400050614    2C     T2F(X)        IFEQ      'F'
394500050614    2C     T2F(X)        orEQ      'M'
394600160223      *solo x la Mail
394700160223    2C     T2F(X)        ifEQ      'M'
394800160223      *
394900160223      * controlla se si tratta di una bolla DPD con tanto di PARCEL
395000160223      *   da cui prendere il DEPOT
395100160223     c                   clear                   fnlg24ds
395200160223     c                   z-add     V1CAAS        i24aas
395300160223     c                   z-add     V1CLNP        i24lnp
395400160223     c                   z-add     V1CNRS        i24nrs
395500160223     c                   z-add     V1CNSP        i24nsp
395600160223      *
395700160223     c                   call      'FNLG24R'
395800160223     C                   parm                    fnlg24ds
395900160223      *
396000160223     c*  se c'è il parcel allora deve passare il Depot e verificare
396100160223     c*  se presente l'indirizzo mail del DEPOT mittente per inviare
396200160223     c*  l'informazione di apertura giacenza.
396300160223     c                   if          o24npr <> *blank
396400160303     c                   if        %subst(o24npr:1:4)<>'0844' and
396500160303     c                             %subst(o24npr:1:4)<>'0845'
396600160223     c                   movel     o24npr        D43DEPOT
396700160303     c                   end
396800160223     c                   end
396900160223     c*
397000160223     c                   endIF
397100160223      *
397200050614     C                   MOVE      v2cscm        §QUATR
397300050614    3C                   IF        §quatr <> *ALL'8' and §quatr <> *all'9'
397400050614     C                   MOVE      V2CSCM        D43KSC
397500050614     C                   CALL      'TRUL43R'
397600050614     C                   PARM                    TRUL43
397700050513     c                   end
397800050614     c                   end
397900050614     c                   end
398000050513     c                   end
398100050608     c                   end
398200050606     c*
398300050217     C                   ENDSR
398400050217     C*---------------------------------------------------------------*
398500050217     C*   ERRORE CARICAMENTO TABELLE                                  *
398600050217     C*---------------------------------------------------------------*
398700050217     C     ERRTAB        BEGSR
398800940915     C*
398900940915     C                   MOVEL     ERR(1)        DSMSG
399000940915     C                   MOVEL     KCOD          CDTAB
399100940915     C                   MOVEL     DSMSG         $MSG
399200940915     C                   Z-ADD     D05AAS        V3CAAS
399300940915     C                   Z-ADD     D05LNP        V3CLNP
399400940915     C                   Z-ADD     D05NRS        V3CNRS
399500940915     C                   Z-ADD     D05NSP        V3CNSP
399600940915     C     D05AGC        IFEQ      0
399700940920     C     D05RIC        ANDEQ     'S'
399800940915     C                   Z-ADD     D05AAS        KAAS
399900940915     C                   Z-ADD     D05LNP        KLNP
400000940915     C                   Z-ADD     D05NRS        KNRS
400100940915     C                   Z-ADD     D05NSP        KNSP
400200940915     C                   Z-ADD     0             GCPAGC
400300940915     C                   Z-ADD     0             GCPFGC
400400940915     C                   Z-ADD     0             GCPNGC
400500050315     C     KSPE2         CHAIN(e)  tIGCP01L                           31
400600050315     c                   if        %error
400700070521     C     KSPE2         CHAIN(n)  tIGCP01L
400800050315     c                   exsr      geslck
400900050318     C     KSPE2         CHAIN(e)  tIGCP01L                           31
401000050315     c                   endif
401100940915     C                   Z-ADD     GCPAGC        V3CAGC
401200940915     C                   Z-ADD     GCPFGC        V3CFGC
401300940915     C                   Z-ADD     GCPNGC        V3CNGC
401400940915     C                   ELSE
401500940915     C                   Z-ADD     D05AGC        V3CAGC
401600940915     C                   Z-ADD     D05FGC        V3CFGC
401700940915     C                   Z-ADD     D05NGC        V3CNGC
401800940915     C                   END
401900940915     C*
402000940915     C                   SETON                                        28
402100940915     C                   EXFMT     LG05D03
402200940915     C                   MOVEL     'S'           WFINE
402300940915     C                   ENDSR
402400970226     C*---------------------------------------------------------------*
402500970226     C*   ERRORE CARICAMENTO TABELLE  EDI                             *
402600970226     C*---------------------------------------------------------------*
402700970226     C     ERREDI        BEGSR
402800970226     C*
402900970226     C                   MOVEL     ERR(22)       DSMSG
403000970226     C                   MOVEL     KCOD1         CDTAB
403100970226     C                   MOVEL     DSMSG         $MSG
403200970226     C                   Z-ADD     D05AAS        V3CAAS
403300970226     C                   Z-ADD     D05LNP        V3CLNP
403400970226     C                   Z-ADD     D05NRS        V3CNRS
403500970226     C                   Z-ADD     D05NSP        V3CNSP
403600970226     C     D05AGC        IFEQ      0
403700970226     C     D05RIC        ANDEQ     'S'
403800970226     C                   Z-ADD     D05AAS        KAAS
403900970226     C                   Z-ADD     D05LNP        KLNP
404000970226     C                   Z-ADD     D05NRS        KNRS
404100970226     C                   Z-ADD     D05NSP        KNSP
404200970226     C                   Z-ADD     0             GCPAGC
404300970226     C                   Z-ADD     0             GCPFGC
404400970226     C                   Z-ADD     0             GCPNGC
404500050315     C     KSPE2         CHAIN(e)  tIGCP01L                           31
404600050315     c                   if        %error
404700070521     C     KSPE2         CHAIN(n)  tIGCP01L
404800050315     c                   exsr      geslck
404900050318     C     KSPE2         CHAIN(e)  tIGCP01L                           31
405000050315     c                   endif
405100970226     C                   Z-ADD     GCPAGC        V3CAGC
405200970226     C                   Z-ADD     GCPFGC        V3CFGC
405300970226     C                   Z-ADD     GCPNGC        V3CNGC
405400970226     C                   ELSE
405500970226     C                   Z-ADD     D05AGC        V3CAGC
405600970226     C                   Z-ADD     D05FGC        V3CFGC
405700970226     C                   Z-ADD     D05NGC        V3CNGC
405800970226     C                   END
405900970226     C*
406000970226     C                   SETON                                        28
406100970226     C                   EXFMT     LG05D03
406200970226     C                   MOVEL     'S'           WFINE
406300970226     C                   ENDSR
406400940915     C*---------------------------------------------------------------*
406500940915     C*   OPERAZIONI INIZIALI                                         *
406600940915     C*---------------------------------------------------------------*
406700940927     C     DEFCAM        BEGSR
406800940915     C*
406900020813     c                   exsr      repute
407000940915     C*
407100050315      *flag per sapere se devo eseguire il commit o lo esegue chi mi ha chiamato
407200050315     c                   move      d05comm       gescomm           1
407300940920     C                   MOVEL     D05GES        SAVGES            1
407400940920     C     D05RIC        IFNE      'S'
407500940919     C                   CLEAR                   FNLG05
407600940920     C                   MOVEL     SAVGES        D05GES
407700940915     C                   END
407800940915     C* Se sono in arrivo apro bolle in arrivo se sono in partenza
407900940915     C* apro bolle in partenza
408000940919     C                   SETOFF                                       01
408100940919     C     D05GES        IFEQ      'A'
408200940915     C                   OPEN      FNARB01L
408300990929     C                   OPEN      FIAR601L
408400051116     C                   OPEN      FIAR901L
408500940920     C                   MOVEL     ' IN ARR'     V1CGES
408600940920     C                   MOVE      'IVO '        V1CGES
408700940919     C                   SETON                                        01
408800940915     C                   ELSE
408900990929     C                   OPEN      FIAR601L
409000051116     C                   OPEN      Fiar901L
409100940920     C                   MOVEL     'IN PART'     V1CGES
409200940920     C                   MOVE      'ENZA'        V1CGES
409300940915     C                   END
409400940915     C* Richiamo XPARUT
409500940915     C                   Z-ADD     1             CODUT
409600940915     C                   CALL      'X§PARUT'
409700940915     C                   PARM                    UTEDSE
409800940915     C                   MOVEL     REC80         CNCR80
409900940919     C     CODUT         CABEQ     -1            FINE
410000940915     C                   MOVEL     RAGUT         V1CRSU
410100940920     C                   MOVEL     '* FNLG05'    V1CPGM
410200940915     C                   MOVE      'R *'         V1CPGM
410300940915     C                   MOVEL     RAGUT         PARDUT           30
410400940915     C*--- RICERCA CAPOCONTI------------------------------------------*
410500071107     C                   DO        50            X
410600940915     C                   MOVE      TCU(X)        TCUDS
410700940915     C     F56           IFEQ      'CG'
410800940915     C     F4            ANDEQ     '1'
410900940915     C     F3            ANDEQ     '0'
411000940915     C                   Z-ADD     KCU(X)        KCI               4 0
411100940915     C                   END
411200940915     C                   END
411300940915     C*---------------------------------------------------------------*
411400940915     C*  KLIST
411500940919     C     KSPE1         KLIST
411600940915     C                   KFLD                    KAAS
411700940915     C                   KFLD                    KLNP
411800940915     C                   KFLD                    KNRS
411900940915     C                   KFLD                    KNSP
412000050517     C     Kblp          KLIST
412100050517     C                   KFLD                    £LBLAAO
412200050517     C                   KFLD                    £LBLLPO
412300050517     C                   KFLD                    £LBLNRO
412400050517     C                   KFLD                    £LBLNSO
412500990929     C     KAR6          KLIST
412600990929     C                   KFLD                    KAAS
412700990929     C                   KFLD                    KLNP
412800990929     C                   KFLD                    KNRS
412900990929     C                   KFLD                    KNSP
413000990929     C                   KFLD                    KTRC
413100940915     C*
413200940919     C     KSPE2         KLIST
413300940915     C                   KFLD                    KAAS
413400940915     C                   KFLD                    KLNP
413500940915     C                   KFLD                    KNRS
413600940915     C                   KFLD                    KNSP
413700940915     C                   KFLD                    KFRG
413800940915     C*
413900940915     C     KTBL1         KLIST
414000940915     C                   KFLD                    KKUT
414100940915     C                   KFLD                    KCOD
414200940915     C                   KFLD                    KKEY
414300940915     C*
414400940915     C     KTBL2         KLIST
414500940915     C                   KFLD                    KKUT
414600940915     C                   KFLD                    KCOD
414700940915     C*
414800940915     C     KACO          KLIST
414900940915     C                   KFLD                    KKUT
415000940915     C                   KFLD                    KKCC
415100940915     C                   KFLD                    KKSC
415200940915     C*
415300940915     C     KGNP          KLIST
415400940915     C                   KFLD                    KAGC
415500940915     C                   KFLD                    KFGC
415600940915     C                   KFLD                    KNGC
415700940915     C                   KFLD                    KFRG
415800960208     C*
415900960208     C     KGNP1         KLIST
416000960208     C                   KFLD                    KAGC
416100960208     C                   KFLD                    KFGC
416200960208     C                   KFLD                    KNGC
416300960208     C                   KFLD                    KFRG
416400960208     C                   KFLD                    KFAS
416500940915     C*
416600940915     C     KNUF          KLIST
416700940915     C                   KFLD                    KAAA
416800940915     C                   KFLD                    KCNU
416900940915     C                   KFLD                    KFGC
417000940915     C                   Z-ADD     2             KCNU
417100940919     C*
417200940919     C     KAR4          KLIST
417300940919     C                   KFLD                    KAAS
417400940919     C                   KFLD                    KLNP
417500940919     C                   KFLD                    KNRS
417600940919     C                   KFLD                    KNSP
417700940919     C                   KFLD                    KTRC
417800940922     C*
417900070320     C     Kar4l         klist
418000070320     C                   kfld                    gcpAASa
418100070320     C                   kfld                    gcpLNPa
418200070320     C                   kfld                    gcpNRSa
418300070320     C                   kfld                    gcpNSPa
418400070320     C                   kfld                    KTRCl
418500070320     C                   move      'L'           KTRCl             1
418600070320     C*
418700940922     C     KTGC          KLIST
418800940922     C                   KFLD                    KKSC
418900940922     C                   KFLD                    KCTR
419000940922     C                   KFLD                    KPRG
419100950105     C*
419200950105     C     KEVB1         KLIST
419300990803     C                   KFLD                    KAAS
419400950105     C                   KFLD                    KLNP
419500950105     C                   KFLD                    KNRS
419600950105     C                   KFLD                    KNSP
419700960216     C                   KFLD                    KDEV
419800950105     C*
419900950105     C     KEVB2         KLIST
420000990803     C                   KFLD                    KAAS
420100950105     C                   KFLD                    KLNP
420200950105     C                   KFLD                    KNRS
420300950105     C                   KFLD                    KNSP
420400961113     C     KRDE          KLIST
420500961113     C                   KFLD                    KAAS
420600961113     C                   KFLD                    KLNP
420700961113     C                   KFLD                    KNRS
420800961113     C                   KFLD                    KNSP
420900961113     C                   KFLD                    KNMC
421000970401     C     KANM          KLIST
421100970401     C                   KFLD                    KAAS2
421200970401     C                   KFLD                    KLNP2
421300970401     C                   KFLD                    KNRS2
421400970401     C                   KFLD                    KNSP2
421500970401     C                   KFLD                    KCAA
421600040713     C     Kntc          KLIST
421700040713     C                   KFLD                    kcapl
421800040713     C                   KFLD                    kcnk1
421900040713     C                   KFLD                    kcnk2
422000040713     C                   KFLD                    kctnt
422100040713     c                   move      'C'           kcapl
422200040713     c                   movel     '0151'        kcnk1
422300040713     c                   move      '88'          kctnt
422400091021     C     Kar5          KLIST
422500091021     C                   KFLD                    ARbAAS
422600091021     C                   KFLD                    ARbLNP
422700091021     C                   KFLD                    ARbNRS
422800091021     C                   KFLD                    ARbNSP
422900091021     C                   KFLD                    k5trd             3
423000091021     C                   movel     'GEN'         k5trd
423100091021     C     Kar5p         KLIST
423200091021     C                   KFLD                    blpAAS
423300091021     C                   KFLD                    blpLNP
423400091021     C                   KFLD                    blpNRS
423500091021     C                   KFLD                    blpNSP
423600091021     C                   KFLD                    k5trd
423700940915     C*---------------------------------------------------------------*
423800940915     C*  LIKE DEFN
423900040713     C     *LIKE         DEFINE    NTCAPL        kcapl
424000040713     C     *LIKE         DEFINE    NTCNK1        kcnk1
424100040713     C     *LIKE         DEFINE    NTCNK2        kcnk2
424200040713     C     *LIKE         DEFINE    NTCTNT        kctnt
424300950105     C     *LIKE         DEFINE    EVBCEV        KCEV
424400950105     C     *LIKE         DEFINE    EVBDEV        KDEV
424500950105     C     *LIKE         DEFINE    GCPAAS        KAAS
424600940915     C     *LIKE         DEFINE    GCPLNP        KLNP
424700940915     C     *LIKE         DEFINE    GCPNRS        KNRS
424800940915     C     *LIKE         DEFINE    GCPNSP        KNSP
424900940915     C     *LIKE         DEFINE    GCPFRG        KFRG
425000940919     C     *LIKE         DEFINE    AR4TRC        KTRC
425100940915     C     *LIKE         DEFINE    ACOKUT        KKUT
425200940915     C     *LIKE         DEFINE    TBLCOD        KCOD
425300940915     C     *LIKE         DEFINE    TBLKEY        KKEY
425400970226     C     *LIKE         DEFINE    TABCOD        KCOD1
425500940915     C     *LIKE         DEFINE    ACOKCC        KKCC
425600940915     C     *LIKE         DEFINE    ACOKSC        KKSC
425700950113     C     *LIKE         DEFINE    TGCPRG        KPRG
425800950113     C     *LIKE         DEFINE    TGCCTR        KCTR
425900940915     C     *LIKE         DEFINE    GCPAGC        KAGC
426000940915     C     *LIKE         DEFINE    GCPFGC        KFGC
426100940915     C     *LIKE         DEFINE    GCPNGC        KNGC
426200940915     C     *LIKE         DEFINE    NUFAAA        KAAA
426300940915     C     *LIKE         DEFINE    NUFCNU        KCNU
426400940919     C     *LIKE         DEFINE    ORGFIL        KFIL
426500960208     C     *LIKE         DEFINE    GCPFAS        KFAS
426600940915     C     *LIKE         DEFINE    GCPSCM        §SCM
426700940919     C     *LIKE         DEFINE    GCPSCM        SAVSCM
426800940915     C     *LIKE         DEFINE    GAPRSM        SAVRSM
426900940915     C     *LIKE         DEFINE    GAPINM        SAVINM
427000940915     C     *LIKE         DEFINE    GAPLOM        SAVLOM
427100940915     C     *LIKE         DEFINE    GAPPRM        SAVPRM
427200940915     C     *LIKE         DEFINE    GAPCAM        SAVCAM
427300940919     C     *LIKE         DEFINE    GAPNZM        SAVNZM
427400940915     C     *LIKE         DEFINE    GCPDXD        AMGDTG
427500940915     C     *LIKE         DEFINE    GCPDXD        AMGDXD
427600940915     C     *LIKE         DEFINE    GCPDXD        AMGDSP
427700961113     C     *LIKE         DEFINE    RDENMC        KNMC
427800970401     C     *LIKE         DEFINE    ANMAAS        KAAS2
427900970401     C     *LIKE         DEFINE    ANMLNP        KLNP2
428000970401     C     *LIKE         DEFINE    ANMNRS        KNRS2
428100970401     C     *LIKE         DEFINE    ANMNSP        KNSP2
428200970401     C     *LIKE         DEFINE    ANMCAA        KCAA
428300940915     C*---------------------------------------------------
428400940915     C* Creo data del giorno
428500940920     C                   TIME                    WHHDAT           14 0
428600940920     C                   MOVE      WHHDAT        G02DAT
428700940920     C                   MOVE      *ZEROS        G02INV
428800940920     C                   MOVEL     *BLANK        G02ERR
428900940920     C                   CALL      'XSRDA8'
429000940920     C                   PARM                    WLBDA8
429100940920     C                   Z-ADD     G02INV        WOGGI             8 0
429200940920     C                   Z-ADD     G02INV        DATEU             6 0
429300940915     C* Inizializzo variabili
429400950206     C                   MOVEL     KNMUS         §SEI              6
429500020730     C**** §PRF          COMP      'REM'                                  02
429600020730     C**N02              Z-ADD     SIMFEL        §FLE
429700020730     C                   Z-ADD     SIMpou        §FLE
429800940919     C                   MOVEL     *BLANKS       §DES             60
429900940915     C                   Z-ADD     1             KKUT
430000940915     C                   Z-ADD     KCI           KKCC
430100940927     C                   Z-ADD     D05FRG        KFRG
430200940920     C                   MOVE      §FLE          PA0SIM
430300941003     C                   CLEAR                   DSTB69
430400940915     C*---------------------------------------------------
430500940915     C* Eseguo caricamento tabelle
430600940915     C                   EXSR      CARTAB
430700940915     C*---------------------------------------------------
430800940927     C                   ENDSR
430900020813     C*-------------------------------------------------------------------------
431000020813     C* Reperisce i dati dell'utente
431100020813     C*-------------------------------------------------------------------------
431200020813     C     Repute        BEGSR
431300020813     C*
431400020813     C* Azzera le variabili di lavoro
431500021114     C                   CLEAR                   Tibs36Ds
431600021114     c                   EVAL      I36ute = knmus
431700021114     c                   EVAL      I36Tla = 'L'
431800021114     C                   CALL      'TIBS36R'
431900021114     C                   PARM                    Tibs36Ds
432000020813     C*
432100020813     C                   ENDSR
432200940927     C*---------------------------------------------------------------*
432300940927     C*   INIZIO PGM.                                                 *
432400940927     C*---------------------------------------------------------------*
432500940927     C     INIPGM        BEGSR
432600940927     C*
432700940915     C     WFINE         IFNE      'S'
432800941223     C                   MOVEL     ' '           WRIAPE            1
432900941223     C                   SETOFF                                       28
433000941223     C                   MOVEL     *BLANKS       $MSG
433100940919     C*  Pulizia parametri a video
433200940919     C                   EXSR      PULVID
433300940919     C                   MOVEL     '1'           WTPVID
433400940915     C*  Se il programma è richiamato imposto gestione prima videata
433500940920     C     D05RIC        IFEQ      'S'
433600940919     C                   MOVEL     '2'           WTPVID            1
433700940915     C                   Z-ADD     D05AAS        KAAS
433800940915     C                   Z-ADD     D05LNP        KLNP
433900940915     C                   Z-ADD     D05NRS        KNRS
434000940915     C                   Z-ADD     D05NSP        KNSP
434100940915     C*  Richiamo routine controllo dati spedizione
434200940915     C                   EXSR      CTRGIA
434300940915     C                   END
434400940915     C                   END
434500940915     C*
434600000526     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
434700000526     C                   CLEAR                   FIP
434800000526     C     *LOVAL        SETLL     AZORG01L
434900000526     C                   DO        *HIVAL
435000000526     C                   READ      AZORG01L                               95
435100000526     C   95              LEAVE
435200000526     C                   MOVEL     ORGDE3        OG143
435300020729     C     §OgNTW        IFEQ      'PPT'
435400000526     C                   Z-ADD     1             X
435500000526     C     *ZEROS        LOOKUP    FIP(X)                                 23
435600000526     C   23              Z-ADD     ORGFIL        FIP(X)
435700000526     C                   END
435800000526     C                   END
435900940915     C                   ENDSR
436000050315     C*----------------------------------------------------*
436100050315     C     geslck        begsr
436200050315     C*----------------------------------------------------*
436300050315     c                   Eval      ul82§rrn = gcpnrr
436400050315     c                   Eval      UL82§FIL = 'TIGCP00F'
436500050315     c                   Eval      UL82§WIN = 'S'
436600050315     c                   Eval      UL82§F7  = 'S'
436700050315     c                   Eval      UL82§num = 3
436800050315     c                   Eval      UL82§att = 3
436900050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza rel-
437000050404     c                             lativa alla spedizione '
437100050404     c                             + %editc(kaas:'Z') +  ' '+ %editc(klnp:'Z')
437200050404     c                             + ' ' +  %editc(knrs:'Z') + ' ' +
437300050405     c                             %editc(knsp:'Z')
437400050404     c                   Eval      UL82§msw = 'La giacenza relativa alla sped.'
437500050404     c                             + %editc(kaas:'Z') +  ' '+ %editc(klnp:'Z')
437600050404     c                             + ' ' +  %editc(knrs:'Z') + ' ' +
437700050404     c                             %editc(knsp:'Z') + ' non è manutenzionabile'
437800050315      * Effettuo la chiamata al *PGM d utilità
437900050315     c                   Call(e)   'TRUL82R'
438000050315     c                   Parm                    Trul82ds
438100050315     c                   if        ul82§sts = 'A'
438200050315     c                   goto      fine
438300050315     c                   endif
438400050315
438500050315     C                   ENDSR
438600900418     C*----------------------------------------------------*
438700940919     OFNARB000  E            AGG
438800911017     O                       ARBCMC
438900911107     O                       ARBFBC
439000021031     OFNARB000  E            liberaarb
439100940919     OFLNUF     E            AGGNUF
439200940919     O                       NUFNUM
439300961204     OFNBLP000  E            SFLEGG
439400961204     O                       BLPDT3
439500961204     O                       BLPFT3
439600970225**
439700940915Impossibile proseguire nr.elementi caricamento tab.   non sufficienti 01
439800050408Giacenza non manutenzionabile fase non congruente                     02
439900940920Impossibile aprire giacenza in partenza                               03
440000940915Bolla figlia !! Per aprire giacenza agire da distinta di consegna     04
440100940915Giacenza non manutenzionabile: già stampata lettera mittente          05
440200050404Bolla inesistente o non disponibile!!  Impossibile gestire giacenza   06
440300940915Attenzione !!!  Bolla di rientro non si può aprire giacenza           07
440400940915Bolla con consegna parziale !!! Impossibile aprire giacenza           08
440500940915Attenzione !!!  Bolla in consegna, non si può aprire giacenza         09
440600940919Codice cliente errato !!!                                             10
440700000404Attenzione !!!  Sedicesima riapertura, avvisare CED di sede !!!!      11
440800031027Tipo comunicazione non presente in tabella o non utilizzabile         12
440900940921Codice motivo giacenza non presente o non ammesso                     13
441000940919Ulteriore motivazione obbligatoria                                    14
441100940920Luogo deposito merce errato                                           15
441200961028Giacenza non manutenzionabile: Punto operativo non in gestione        16
441300950228Attenzione !!!  Merce già consegnata non si può aprire giacenza       17
441400960201Tp.invio comunicazione apertura non utilizzabile per clienti generici 18
441500960201Tp.invio comunicazione apertura non utilizzabile: numero fax errato   19
441600961028Punto operativo non in gestione                                       20
441700961113Motivo di giacenza generico non ammesso per bolle import              21
441800970226Impossibile proseguire numero elementi tabella EDI    non sufficienti 22
441900970310Modifica ulteriore motivazione obbligatoria per partner-evento EDI    23
442000970624La particolarità non permette l'apertura giacenza note non ammesse    24
442100990203Bolla con pratica C.A. aperta ! Impossibile aprire giacenza           25
442200990805Reso automatico bloccato per il periodo in corso                      26
442300000526FILIALE POSTE NON GESTIBILE !!!!                                      27
442400021031Il tipo bolla non permette l'apertura della giacenza                  28
442500040113La particolarità consegna non permette di aprire giacenza             29
442600170410Motivo giacenza non ammesso bolla con DDT = SI o PACKING LIST         30
442700040402Presente codice mancata consegna agire da aperture da distinta        31
442800040713Tp.invio comunicazione apertura non utilizzabile: IND.E-MAIL errato   32
442900050609ERRORI!!! La giacenza non è stata aperta o aggiornata F12=Ritorno     33
443000050629Si sta tentando di fare un'apertura tecnica oltre il limite consentito34
443100110531Codice motivo giacenza non ammesso con questa particolarità           35
443200141029Esistono delle disposizioni di consegna che devono essere risolte     36
443300151204PRESENTI disposizioni Dirottamento/Reso spedizione non gestibile      37
443400170412Motivo giacenza non ammesso se non presente contrassegno              38
443500170412Motivo giacenza non ammesso se spedizione in porto franco             39
443501170530Errore la giacenza è già stata aperta da un altro utente              40
443600961217**
443700961217CHKOBJ OBJ(EDRDE01L) OBJTYPE(*FILE)
443800970225CHKOBJ OBJ(EDTAB01L) OBJTYPE(*FILE)
443900040329**         CMA4
444000040329OVRPRTF FNLSB5PA4 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle-A4')
444100040329**         CMA5
444200040329OVRPRTF FNLSB5PA5 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle-A5')
