000100930305      ****************************************************************
000200960209      *                                                              *
000300941227      *   CALCOLA I TEMPI DI DELIVERY (TEMPO PART. + TRAZ. +         *
000400930423      *   DI CONSEGNA) PER UNA BOLLA (PATENZA O ARRIVO)              *
000500930204      *                                                              *
000600970731      *        ---- CON LOCALITA' -----      31/07/97                *
000700970731      *                                                              *
000800930204      *   PARAMETRI UTILIZZATI DAL PGM:                              *
000900941227      *   SE IL PGM TROVA IL NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
001000941227      *   FNBLP E FNARB ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI    *
001100931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE.                        *
001200930323      *   INPUT: - TIPO LANCIO:                                      *
001300930420      *            SPAZIO = APRE E NON CHIUDE FILE                   *
001400930420      *            C = ESEGUE SOLAMENTE LA CHIUSURA DEI FILE         *
001500960229      *            T = ESEGUE SOLAMENTE CALCOLO TEMPI PREV. TRAZIONE *
001600930323      *          - TIPO BOLLA: A=ARRIVO , P=PARTENZA                 *
001700930323      *          - ANNO BOLLA                                        *
001800930323      *          - LINEA PARTENZA                                    *
001900930323      *          - SERIE SPEDIZIONE                                  *
002000930323      *          - NUMERO SPEDIZIONE                                 *
002100930422      *   OUTUT: - EVENTUALE ERRORE 0=NO ERRORE,1=ERRORE             *
002200930323      *          - EVENTUALE MESSAGGIO DI ERRORE                     *
002300941227      *          - TEMPI TEORICI DI PART./TRAZ./CONS./RESA           *
002400941227      *          - TEMPI REALI DI PART./TRAZ./CONS./RESA             *
002500930915      *          - TEMPI DI DIFFERENZA TRA TEORICI E REALI           *
002600930323      *          - CONFORMITA'                                       *
002700930324      *          - TEMPI DI NON CONFORMITA'                          *
002800930324      *          - CAMPI VARI UTILIZZATI ALL'INTERNO DEL PGM         *
002900930324      *            RESI DISPONIBILI AL PGM CHIAMANTE                 *
003000930204      *                                                              *
003100930204      ****************************************************************
003200930413     H DECEDIT('0,') DATEDIT(*DMY.)
003300041116     FFNBLP01L  IF   E           K DISK    usropn
003400050405     FTIGCP01L  IF   E           K DISK    usropn
003500150120     F**FNBLT01L  IF   E           K DISK    usropn
003600041116     FFNARB01L  IF   E           K DISK    usropn
003700051115     FFiAR901L  IF   E           K DISK    usropn
003800150325     FFiAR401L  IF   E           K DISK    usropn
003900050405     FTIGCP21L  IF   E           K DISK    usropn
004000050405     F                                     RENAME(TIGCP000:TIGCP021)
004100150218     FFNART01L  IF   E           K DISK    usropn
004200150218     FFNbrv07l  IF   E           K DISK    usropn
004300041116     FFNLBL01L  IF   E           K DISK    usropn
004400070824     fFiar501l  if   e           k Disk    usropn
004500150325     fFiapd01l  if   e           k Disk    usropn
004600930323     FTABEL00F  IF   E           K DISK
004700010821     FTNTBE01L  IF   E           K DISK
004800941227     FAZCLN01L  IF   E           K DISK
004900941227     FAZCAS01L  IF   E           K DISK
005000041116     FFNEVB01L  IF   E           K DISK    usropn
005100971210     FAZORG01L  IF   E           K DISK
005200170925     Ffipnd01L  IF   E           K DISK
005300171112     FFNFGV02L  iF   E           K DISK    usropn
005400171112     FFNFGW01L  IF   E           K DISK    usropn
005500950406     D*****************************************************************
005600950406     D* SCHIERE PER ROUTINE GIORNO>DATA E DATA>GIORNO
005700950406     D GPM             S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND     GG. PROGR. MESI
005800950406     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)            GG. DEL MESE
005900930304     D*****************************************************************
006000930304     D* MESSAGGI
006100941227     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
006200950406     D*****************************************************************
006300950406     D* SCHIERE DI LAVORO PER ROUTINE GIORNO>DATA E DATA>GIORNO
006400950406     D $PM             S              3  0 DIM(12) ASCEND                       GG. PROGR. MESI
006500950406     D $GM             S              2  0 DIM(12)                              GG. DEL MESE
006600941227     D*****************************************************************
006700941227     D* SCHIERA TABELLA CODICI BOLLA -> TIPO BOLLA (FORMATO SEDE)
006800941228     D C3A             S              2    DIM(50)                              *KEY
006900941227     D T3A             S              2    DIM(50)                              *TIPO BOLLA
007000941228     D F3A             S              1  0 DIM(50)                              *FLAG CONTRASSEGNO
007100990308     D R3A             S              1    DIM(50)                              *RAGGRUPP.BOLLE
007200930316     D*****************************************************************
007300930405     D* SCHIERA TABELLA TEMPI FISSI DI DELIVERY (1X)  (PER FILIALE)
007400080513     d c_dim_1X        c                   const(500)
007500080513     D C1X             S              8    DIM(c_dim_1X)                        *KEY
007600080513     D G1X             S              3  0 DIM(c_dim_1X)                        *TEMPO GIORNO
007700080513     D O1X             S              4  0 DIM(c_dim_1X)                        *ORA SOGLIA
007800080513     D M1X             S              3  0 DIM(c_dim_1X)                        *TEMPO MATTINO
007900080513     D P1X             S              3  0 DIM(c_dim_1X)                        *TEMPO POMERIGGIO
008000080513     D I1X             S              3  0 DIM(c_dim_1X)                        *ORE INF.CONFORM.
008100080513     D S1X             S              3  0 DIM(c_dim_1X)                        *ORE SUP.CONFORM.
008200930405     D*****************************************************************
008300930405     D* SCHIERA TABELLA SIMBOLI CALENDARIO (1Y)
008400930421     D C1Y             S              8    DIM(30)                              *KEY
008500930421     D I1Y             S              1    DIM(30)                              *FEST.INTERNA ?
008600930421     D E1Y             S              1    DIM(30)                              *FEST.CLIENTE ?
008700930421     D*****************************************************************
008800930421     D* SCHIERA TABELLA CAUSALI EVENTI BOLLE (2A)
008900060403     D C2A             S              3    DIM(199)                             *KEY
009000060406     D D2A             S              3    DIM(199)                             *RECORD
009100170915     D D2Z             S              6    DIM(199)                             *RECORD
009200031201     D*****************************************************************
009300031201     D* SCHIERA TABELLA CODICI GESTIONE PARTICOLARITA' GIACENZE (7Q) + SUA ESTENSIONE (7W)
009400060403     D SK7QW           S              2    DIM(199)
009500930421     D*****************************************************************
009600060406     D* SCHIERA EVENTI BOLLE di l.avviso e giacenza
009700060403     D EVD             S              8  0 DIM(199)                             *DATA EVENTO
009800060403     D EVO             S              4  0 DIM(199)                             *ORA EVENTO
009900060403     D EVC             S              3    DIM(199)                             *CAUSALE EVENTO
010000131218     D EVT             S              1    DIM(199)                             *tipo G/A
010100031125     D*****************************************************************
010200031125     D* SCHIERA GIACENZE BOLLE
010300031125     D GCPD            S              8  0 DIM(99)                              *DATA GIACENZA
010400930421     D*****************************************************************
010500930421     D* SCHIERA RANGE DATE INIZIO/FINE PER LASCIATO AVVISO
010600981029     D LAI             S              9  0 DIM(99)                              *INIZIO
010700981029     D LAF             S              9  0 DIM(99)                              *FINE
010800091014     D LAp             S              3  0 DIM(99)                              *FINE
010900981029£$   D LCI             S              9  0 DIM(99)                              *INIZ X CONSEGNABILI
011000981029     D LCF             S              9  0 DIM(99)                              *FINE X CONSEGNABILI
011100050520     D LCon            S              1    DIM(99)                              *FINE X CONSEGNABILI
011200960129$$£  D*****************************************************************
011300960129$$£  D* SCHIERA RANGE DATE INIZIO/FINE PER EVENTI GIACENZA
011400981029$$£  D GII             S              9  0 DIM(99)                              *INIZIO
011500981029$$£  D GIF             S              9  0 DIM(99)                              *FINE
011600091014$$£  D GIp             S              3  0 DIM(99)                              *FINE
011700981029$$£  D GCI             S              9  0 DIM(99)                              *INIZ X CONSEGNABILI
011800981029$$£  D GCF             S              9  0 DIM(99)                              *FINE X CONSEGNABILI
011900931213     D*****************************************************************
012000931213     D* SCHIERA TURNI CHIUSURA
012100931213     D TUC             S              2    DIM(4)                               *TURNI CHIUSURA
012200931201     D*****************************************************************
012300931201     D* SCHIERA RANGE DATE INIZIO/FINE PER TURNO CHIUSURA
012400981029     D TII             S              9  0 DIM(99)                              *INTERNO/INIZIO
012500981029     D TIF             S              9  0 DIM(99)                              *INTERNO/FINE
012600981029     D TCI             S              9  0 DIM(99)                              *CLIENTE/INIZIO
012700981029     D TCF             S              9  0 DIM(99)                              *CLIENTE/FINE
012800060406     D*****************************************************************
012900060406     D* SCHIERA EVENTI particolari bolle
013000060406     D pVD             S              8  0 DIM(199)                             *DATA EVENTO
013100060406     D pVO             S              4  0 DIM(199)                             *ORA EVENTO
013200060406     D pVC             S              3    DIM(199)                             *CAUSALE EVENTO
013300060406     D pVF             S              1    DIM(199)                             *CAUSALE EVENTO
013400060406     D* SCHIERA RANGE DATE INIZIO/FINE PER Eventi particolari bolle "G"
013500060406     d*  che  Giustificano l'affidabilità
013600060406     D pGII            S              9  0 DIM(99)                              *INTERNO/INIZIO
013700060406     D pGIF            S              9  0 DIM(99)                              *INTERNO/FINE
013800060406     D pGCI            S              9  0 DIM(99)                              *CLIENTE/INIZIO
013900060406     D pGCF            S              9  0 DIM(99)                              *CLIENTE/FINE
014000060418     d*  che  Allungano    l'affidabilità
014100060418     D pAII            S              9  0 DIM(99)                              *INTERNO/INIZIO
014200060418     D pAIF            S              9  0 DIM(99)                              *INTERNO/FINE
014300060418     D pACI            S              9  0 DIM(99)                              *CLIENTE/INIZIO
014400060418     D pACF            S              9  0 DIM(99)                              *CLIENTE/FINE
014500931207     D*****************************************************************
014600931207     D* SCHIERA DATE PER CALCOLO ORE VALIDE TRA DUE DATE (DATE FINE L.A. E GIAC)
014700981029     D COV             S              9  0 DIM(99)
014800930325     D*****************************************************************
014900930405     D* SCHIERA GIORNI SETTIMANA
015000930405     D*****************************************************************
015100930405     D* SCHIERA GIORNI FESTIVI
015200930517     D*****************************************************************
015300971113     D* SCHIERA RECORD DEL CALENDARIO PIU' UTILIZZATI
015400050523     D CAK             S             12    DIM(600)                             *KEY CALENDARIO
015500050523     D CAM             S             31    DIM(600)                             *MATTINA
015600050523     D CAP             S             31    DIM(600)                             *POMERIGGIO
015700941227     D*****************************************************************
015800941227     D* SCHIERA POSSIBILI COMBINAZIONI LINEE/TERMINAL
015900941227     D LTP             S              3  0 DIM(4)
016000941227     D LTA             S              3  0 DIM(4)
016100971210     D*****************************************************************
016200971210     D* INDICE=COD.FILIALE ,   FL1 => BLANK=ITALIA,'E'=ESTERO
016300971210     D FL1             S              1    DIM(999)                             *FLAG ITALIA/ESTERO
016400020226     D*****************************************************************
016500160331     D* SCHIERA FILIALI FEDEX   e DPD
016600060406     D FILFED          S              3  0 DIM(99)
016700160331     D FILDPD          S              3  0 DIM(99)
016800970701     D*****************************************************************
016900970701     D* PARAMETRI CHIAMATA PGM TISI95R (CONTROLLO CAP)
017000970701     D SI95          E DS                  EXTNAME(TISI95DS)
017100930325     D*****************************************************************
017200930405     D* DS PER RIDEFINIRE SCHIERA GIORNI SETTIMANA
017300930405     D GSEDS           DS
017400930405     D  GSE                    1     31
017500930405     D                                     DIM(31)
017600930405     D*****************************************************************
017700930405     D* DS PER RIDEFINIRE SCHIERA GIORNI FESTIVI
017800930405     D GFEDS           DS
017900930405     D  GFE                    1     31
018000930405     D                                     DIM(31)
018100150325     D*****************************************************************
018200150325     D* DS per record L del FIAR4
018300150325     D DSbl4L        E DS
018400160331     d
018500170925     D** DSbl4I        E DS
018600170821
018700170925     D DPNDFLO       E DS                  inz
018800170925
018900170925     D fnlrj6ds      E DS                  inz
019000930323     D*****************************************************************
019100930323     D* DS ESTERNA IN INPUT AL PGM
019200970731     D DS99          E DS                  EXTNAME(TNSD99DS)
019300930323     D*****************************************************************
019400930323     D* DS ESTERNA TABELLA 1X TEMPI FISSI DI DELIVERY
019500930326     D DS1X          E DS
019600930405     D*****************************************************************
019700971113     D* DS ESTERNA TABELLA 1Y SIMBOLI CALENDARIO
019800930405     D DS1Y          E DS
019900941227     D*****************************************************************
020000941227     D* DS ESTERNA TABELLA 3A (CODICI BOLLA)
020100941227     D DS3A          E DS
020200930421     D*****************************************************************
020300060406     D* DS ESTERNA TABELLA 2A CAUSALI EVENTI BOLLE
020400060406     D DS2A          E DS                  prefix(W:1)
020500170915     D DS2Z          E DS                  prefix(W:1)
020600020226     D*****************************************************************
020700020226     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
020800020226     D OG143         E DS
020900020702     D*****************************************************************
021000020702     D* DS PER LETTURA TABELLA "NTW"
021100020702     D DNTW          E DS
021200150326     D* DS PER LETTURA TABELLA "VPO" flag operativi
021300150326     D Dvpo          E DS
021400031201     D*****************************************************************
021500150331     D* DS ESTERNA TABELLA 7Q/7W/7R
021600031201     D DS7Q          E DS
021700031201     D DS7W          E DS
021800150331     D DS7R          E DS
021900070514     D*****************************************************************
022000140825     D* DS per reperimento orari di consegna
022100140825     D TRULORSDS     E DS
022200140825     D TRULOR2DS     E DS
022300150331     D TRULOR3DS     E DS
022400140825     D*****************************************************************
022500150219     D FNLV53DS      E DS
022600150219     D FNLV55DS      E DS
022700150401     D*****************************************************************
022800150401     D fnlv82ds      E DS
022900150219     D*****************************************************************
023000060406     D usa2a           DS
023100060406     D  §2aftc                 1      1
023200060406     D  §2agla                 2      2  0
023300060406     D  §2aFTG                 3      3
023400060406     D usa2Z           DS
023500060406     D  §2Zcep                 1      1
023600060406     D  §2Znep                 2      3
023700060406     D  §2Zdep                 4      4
023800060406     D  §2Zfep                 5      5
023900170915     D  §2Ztel                 6      6
024000930517     D***************************************************************
024100971113     D* DS PER SCOMPORRE CHIAVE CALENDARIO
024200930517     D                 DS
024300941227     D  DCATFP                 1      3
024400941227     D  DCATFA                 4      6
024500941227     D  DCAANN                 7     10
024600941227     D  DCAMES                11     12
024700941227     D  DCALEN                 1     12
024800950406     D***************************************************************
024900950406     D* DS PER SCOMPORRE WDAT IN AAAA/MM/GG
025000950406     D                 DS
025100950406     D  WDAT                   1      8  0
025200950406     D  WAA                    1      4  0
025300950406     D  WMM                    5      6  0
025400950406     D  WGG                    7      8  0
025500970304     D***************************************************************
025600970304     D* DS PER CONFRONTI TRA DATE
025700970304     D                 DS
025800970304     D  WDO1                   1     12  0
025900970304     D  WD1                    1      8  0
026000970304     D  WO1                    9     12  0
026100970304     D                 DS
026200970304     D  WDO2                   1     12  0
026300970304     D  WD2                    1      8  0
026400970304     D  WO2                    9     12  0
026500150330     D***************************************************************
026600150330     D                 DS
026700150330     D  who                    1      2  0
026800150330     D  wmn                    3      4  0
026900150330     D  WORA                   1      4  0
027000941227     D***************************************************************
027100941227     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
027200941227     D DS§DAT          DS
027300941227     D  DSSEC                  1      2  0
027400941227     D  DSANN                  3      4  0
027500941227     D  DSMES                  5      6  0
027600941227     D  DSGIO                  7      8  0
027700941227     D  DSAAS                  1      4  0
027800941228     D  DSMGS                  5      8  0
027900941227     D  DSDAT6                 3      8  0
028000941227     D  DSDAT8                 1      8  0
028100941227     D DS§TIM          DS
028200941227     D  DTORA                  1      2  0
028300941227     D  DTMIN                  3      4  0
028400941227     D  DTSCN                  5      6  0
028500941227     D  DTGIO                  7      8  0
028600941227     D  DTMES                  9     10  0
028700941227     D  DTSEC                 11     12  0
028800941227     D  DTANN                 13     14  0
028900941227     D  DTINV                  7     14  0
029000941227     D  DTORA6                 1      6  0
029100941227     D  DTORA4                 1      4  0
029200941227     D  DTTIME                 1     14  0
029300171108     D***************************************************************
029400171108     D                 DS
029500171108     D  FGVLNA                 1      3  0
029600171108     D  FGVFFV                 4    243
029700171108     D  FGVFF2               244    450
029800171108     D  FGWFF3               451    690
029900171108     D  FGWFF4               691    900
030000171108     D  VUOTO                901    903
030100171108     D  FFV                    1    903
030200171110     D                                     DIM(301)
030300010821     D***************************************************************
030400050523     D kpjba         E DS
030500010821     D*  CAMBIO DIVISA
030600010821     D YEURDS        E DS                  EXTNAME(YEURCODS)
030700010821     D***************************************************************
030800010821     D*  REPERIMENTO DIVISE DI CONTO
030900010821     D DGED          E DS
031000070827     D*------------------
031100070827     D DAR5GEN       E DS
031200011211     D*------------------
031300011211     D* DS REPERIMENTO IMPORTO MASSIMO CONTRASSEGNO
031400011211     D*------------------
031500011211     D TRUL21DS      E DS
031600050523     d
031700050523     d* DS x controllo riempimento schiera
031800050523     D TRUL0sDS      E DS
031900040809     D*------------------
032000040809     D* ds per il controllo della presenza di CA per la spedizione richiesta
032100040809     D*------------------
032200040809     d FIDN12DS      e ds
032300040809     d  skKey                 26    305    dim(20)
032400040809     d  skAnn                306    325    dim(20)
032500040809     d  skDca                326    485  0 dim(20)
032600040809     d  skFca                486    545  0 dim(20)
032700040809     d  skDch                546    705  0 dim(20)
032800040809     d  skCch                706    745    dim(20)
032900040809     d*
033000040809     d dsKey           ds
033100040809     d  dsaac                         4  0
033200040809     d  dsfil                         3  0
033300040809     d  dsnca                         7  0
033400041028     d wesarb          s              1
033500141216     d wtrasm          s              1
033600041116     d wapri           s              1    inz(' ')
033700050908     d Usacons         s              1    inz(' ')
033800050405     D*------------------
033900050405     D* VARIABILI D WRK RIFERITE AL DATA BASE
034000050405     D*------------------
034100050405     D KWGCPAAS        s                   like(GCPAAS)
034200050405     D KWGCPLNP        s                   like(GCPLNP)
034300050405     D KWGCPNRS        s                   like(GCPNRS)
034400050405     D KWGCPNSP        s                   like(GCPNSP)
034500050405     D KWGCPFRG        s                   like(GCPFRG)
034600050901     d
034700050901     D salvaoreHMC     s                   like(wr3hho)
034800050901     D salvaoreHCR     s                   like(wr3hho)
034900050907     D oreILoDOPO      s                   like(worecr)
035000060526     D wpodata         s                   like(w12dat)
035100070514     D wdata8          s                   like(w12dat)
035200060406     D w003a           s              3
035300060406     D w11poi          s              9  0
035400150217     D picklim         s              4  0
035500060420     D wgiroepa        s              1
035600140911     D wtuctut         s              1    inz
035700060420     D wfine           s              1
035800070827     D CallAR5         s              1    inz
035900140825     D Wtrulor         s              1    inz
036000070827     d kAr5Trd         s                   Like(Ar5Trd) inz('GEN')
036100150325     d kAr4Trc         s                   Like(Ar4Trc) inz('L')
036200071122     d d98cbo          s                   Like(arbcbo)
036300090827     d w99tci          s                   Like(d98tci)
036400090827     d w99tcc          s                   Like(d98tcc)
036500090921     d w99tri          s                   Like(d98tri)
036600090921     d w99trc          s                   Like(d98trc)
036700090928     d Primotipo       s                   Like(§2aftc)
036800090928     d Primodev        s                   Like(evbdev)
036900090928     d Primohev        s                   Like(evbhev)
037000091014     d SalvaEveL       s              3  0
037100091014     d SalvaEveG       s              3  0
037200140911     d EX              s              3  0
037300150325     d II              s              3  0
037400150630     d wdiff           s              9  0
037500100308     d s98dcr          s                   Like(d98dcr)
037600100308     d s98hcr          s                   Like(d98hcr)
037700100308     d s98tcr          s                   Like(d98tcr)
037800140825     d wd98ndc         s                   Like(arbndc)
037900140911     d wd98fbc         s                   Like(arbfbc)
038000150119     d wd98acc         s                   Like(arbacc)
038100150219     d
038200150219     d IMPdata         s                   like(d53dfv)
038300150219     d IMPfgs          s                   like(brvfgs)
038400150219     d DISfgs          s                   like(brvfgs)
038500150219     d DISdata         s                   like(d53dfv)
038600150219     d DISnpg          s                   like(brvnpg)
038700150219     d DISora          s                   like(brvhcs)
038800150219     d IDdata          s                   like(d53dfv)
038900150219     d IDora           s                   like(brvhcs)
039000150219     d IDfgs           s                   like(brvfgs)
039100150219     d IDnpg           s              1
039200150325     D kpdpdr          S                   LIKE(APDpdr)
039300150325     D kpdtip          S                   LIKE(APDtip)
039400170208     d wsolodam        s              1
039500140825     d
039600171110     D wrkoggiiso      s               d     datfmt(*iso)
039700930304     C*****************************************************************
039800930323     C*/ INIZIO MAIN DEL PGM
039900170208     C* ESEGUE ROUTINE wdaodiLE
040000930304     C                   EXSR      RUTINZ
040100930528     C*
040200930528     C     D98ERR        IFEQ      '0'                                          --- 1 -->
040300930602     C*
040400930602     C* SE LANCIATO CON TIPO LANCIO='C' (CHIUDI FILE) VADO A FINE PGM
040500930602     C     D98TLA        IFNE      'C'                                          --- 2 -->
040600960729     C*
040700960729     C* CONTROLLO SE MI HANNO LANCIATO PER DETERMINARE LA CONSEGNABILITA'
040800960729     C                   MOVEL     'N'           FLCONS            1            *FLAG ESISTE CONSEGN
040900960729     C     D98DIC        IFGT      *ZEROS
041000960729     C     D98DFC        ORGT      *ZEROS
041100960729     C                   MOVEL     'S'           FLCONS                         *SI ESISTE CONSEGNAB
041200960729     C                   ENDIF
041300930323     C*
041400930511     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
041500931125     C* SE NON E' STATA IMPOSTATA DS DAL PGM CHIAMANTE (NSP=ZERO)
041600930420     C     D98ERR        IFEQ      '0'
041700010821     C     D98NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
041800930415     C     D98TBO        IFEQ      'A'
041900941227     C                   EXSR      LETARB                                       *LEGGE BOLLA ARRIVO
042000930323     C                   ELSE
042100941227     C                   EXSR      LETBLP                                       *LEGGE BOLLA PART.
042200930323     C                   END
042300140402     c
042400150401     c* Leggo fiar5 recod GEN  e FIAR4
042500140402     c                   exsr      leggiar5
042600140402     c
042700931206     C                   ELSE
042800931206     C* DS IMPOSTATA DAL PGM CHIAMANTE IMPOSTO CAMPI LAVORO
042900931206     C                   Z-ADD     D98DCG        W98DCG
043000990308     C                   MOVEL     *BLANKS       WRBL              1            *RAGGRUPP.BOLLE
043100990308     C                   MOVEL     *BLANKS       WFBR              1            *BOLLA DI RESO
043200031201     C                   MOVEL     *BLANKS       WGGA              2            *PARTICOLARIT' GIAC.
043300140822     C                   Z-ADD     d98DCR        s98DCR
043400140822     C                   Z-ADD     d98HCR        s98HCR
043500140822     C                   MOVEL     d98TCR        s98TCR
043600141104     c                   eval      gc1part=d98gc1
043700141104     c                   eval      gc2part=d98gc2
043800150303     c* imposto flag trasmissione bolla = 'N'
043900150303     c                   eval      wtrasm='N'
044000931125     C                   END
044100930420     C                   END
044200930528     C*
044300010821     C     D98ERR        IFEQ      '0'                                          --- 3 -->
044400160420
044500160420     c* se bolla in partenza, devo ricostruire il record "L"
044600160420    2c                   if        wtrasm='N'
044700160420    3c                   if        d98tc1 = 'A' or d98tc1 = 'S' or d98tc2 = 'A'
044800160420     c                             or d98tc2 = 'S'
044900160420     c                   eval      §b4lbas = 'S'
045000160420      * se consegna tassativa sblocco
045100160420    4c                   if        d98dcr <> *zeros and d98tcr = *blanks
045200160420     c                   clear                   §b4lbas
045300160420    4c                   endif
045400160420    3c                   endif
045500160420
045600160420    3c                   if        d98ffd<>' '
045700170206
045800160420     c                   eval      §b4ltfd = 'S'
045900170206     c* se Fermo deposito tolgo il contatto per l'appuntamento
046000170206    4c                   if        §b4lbas = 'S' and
046100170206     c                             (d98tc1 = 'A' or d98tc2= 'A')
046200170206     c                   clear                   §b4lbas
046300170206    4c                   endif
046400160420    3c                   endif
046500160420
046600160420    2c                   endif
046700160420
046800050908     c* Se esiste la data consegnabilità, faccio finta che la bolla
046900050908     c*  non si consegnata, azzerando proprio la dta di consegna
047000050908     c*  perchè può trarre in inganno
047100050908     c                   if        d98dfc>0 and d98dcm>0
047200050908     c                   clear                   d98dcm
047300050908     c                   clear                   d98hmc
047400050908     c                   clear                   d98cca
047500050908     c                   endif
047600090902     c*
047700090902     c* Se non c'e' nè data consegna, ne data arrivo trazione
047800090902     c*  faccio finta che data arrivo trazione= udate utime ai fini dei calcoli
047900090902     c* in fondo al pgm dovrò azzerare i tempi di consegna interni e cliente
048000090902     c* perchè non calcolabili
048100090902     c                   clear                   wmancaTRAZ        1
048200090902     c
048300010821     C*
048400010821     C* EFFETTUO SUBITO CONSIDERAZIONI SU DIVISE (X QUESTIONE EURO)
048500010821     C                   EXSR      CHKVCA
048600141204     c
048700141204     c* Controllo se ha pratica di DANNO aperta
048800141204     c                   EXSR      CHKDAN
048900010108     C*
049000010108     C* SE BOLLA POSTA, UTILIZZA LA DATA BORDERO' ANZICHE' LA DATA SPEDIZIONE
049100091009     C**   D98TSP        IFEQ      'P'                                          *BOLLA POSTA
049200091009     C**   D98DBR        IFGT      *ZEROS                                       *DATA BORDERO' PRESE
049300091009     C**                 Z-ADD     D98DBR        D98DSP                         *DATA BORDERO' --> D
049400091009     C**                 ENDIF
049500091009     C**                 ENDIF
049600930304     C*
049700950110     C* RECUPERO I TEMPI TEORICI DI PARTENZA, TRAZIONE E DI CONSEGNA
049800930420     C     D98ERR        IFEQ      '0'
049900930323     C                   EXSR      RTTEOR
050000930420     C                   END
050100930528     C*
050200930602     C     D98ERR        IFEQ      '0'                                          --- 4 -->
050300970304     C*
050400970304     C* SE RICHIESTO LANCIO CON DATA RIFERIMENTO CONTROLLO LE DATE DA AZZERARE
050500970304     C     D98ERR        IFEQ      '0'
050600970304     C                   EXSR      DATRIF
050700970304     C                   END
050800940901     C*
050900940901     C* TRASF. DATA ARRIVO TRAZIONE E RICONSIDERO TEMPO CONSEGNA SE RITARDO
051000940901     C     D98ERR        IFEQ      '0'
051100940901     C                   EXSR      TDTIPO
051200940901     C                   END
051300930420     C*
051400930511     C* CARICO IL FILE DEGLI EVENTI BOLLA
051500930420     C     D98ERR        IFEQ      '0'
051600041116     c     d98nsp        andgt     0
051700930420     C                   EXSR      CAREVB
051800930420     C                   END
051900931213     C*
052000931213     C* CARICO LA SCHIERA DEI TURNI DI CHIUSURA
052100931213     C     D98ERR        IFEQ      '0'
052200931213     C                   EXSR      CARTUC
052300931213     C                   END
052400930528     C*
052500930602     C     D98ERR        IFEQ      '0'                                          --- 5 -->
052600930405     C*
052700940901     C* TRASFORMO LE DATE (SPED,CONS,ECC.) IN PROGRESSIVI ORE
052800930420     C     D98ERR        IFEQ      '0'
052900930405     C                   EXSR      TDATPO
053000930420     C                   END
053100930528     C*
053200930602     C     D98ERR        IFEQ      '0'                                          --- 6 -->
053300950110     C*
053400950110     C* CALCOLA LE DATE DI PREVISTA PARTENZA DELL'ULTIMO COLLO
053500950110     C     D98ERR        IFEQ      '0'
053600950110     C                   EXSR      CALDPP
053700950110     C                   END
053800930405     C*
053900930511     C* CALCOLA LE DATE DI PREVISTO ARRIVO DELLA TRAZIONE
054000930420     C     D98ERR        IFEQ      '0'
054100930511     C                   EXSR      CALDPT
054200090902     c*
054300090902     c* Ricalcolo le date facendo finta che la trazione stia arrivando
054400090903     c*  in questo momento se non presente
054500090902     c                   if        ricalcoloTRAZ='S'
054600090902     c                   eval      d98dti=WDAODI
054700090902     c                   eval      d98hti=WHHODI
054800090902     c                   eval      wmancaTRAZ='S'
054900090902     c
055000090902     C                   EXSR      TDTIPO
055100090902     C                   EXSR      CALDPP
055200090902     C                   EXSR      CALDPT
055300090902     c                   endif
055400090902     c
055500930420     C                   END
055600960229     C*
055700091009     d* D98TLA = 'T' --> in output solo i tempi di trazione
055800960229     C     D98TLA        IFNE      'T'                                          --- 6A ->
055900060407     C*
056000931206     C* CALCOLA IL RANGE PER IL CONTRASSEGNO
056100931206     C     D98ERR        IFEQ      '0'
056200931206     C                   EXSR      CALASS
056300931206     C                   END
056400931206     C*
056500931206     C* CALCOLA IL RANGE PER IL FERMO DEPOSITO
056600931206     C     D98ERR        IFEQ      '0'
056700931206     C                   EXSR      CALFED
056800931206     C                   END
056900931206     C*
057000931206     C* CALCOLA LE DATE TEORICHE DI CONSEGNA
057100931206     C     D98ERR        IFEQ      '0'
057200931206     C                   EXSR      CALDTE
057300931206     C                   END
057400930511     C*
057500930511     C* CALCOLA IL RANGE PER DATA CONSEGNA RICHIESTA
057600930511     C     D98ERR        IFEQ      '0'
057700930511     C                   EXSR      CALDCR
057800930511     C                   END
057900930511     C*
058000930511     C* CALCOLA I RANGE PER LASCIATO AVVISO
058100930511     C     D98ERR        IFEQ      '0'
058200930511     C                   EXSR      CALLAV
058300930511     C                   END
058400930511     C*
058500930511     C* CALCOLA IL RANGE PER LA GIACENZA
058600930511     C     D98ERR        IFEQ      '0'
058700930511     C                   EXSR      CALGIA
058800930511     C                   END
058900060420     C* CALCOLA IL RANGE PER Eventi Particolari che Allungano
059000060420     C     D98ERR        IFEQ      '0'
059100060420     C                   EXSR      CALcepa
059200060420     C                   END
059300930511     C*
059400930511     C* CALCOLA LE DATE DI PREVISTA CONSEGNA
059500930511     C     D98ERR        IFEQ      '0'
059600930511     C                   EXSR      CALDPC
059700930511     C                   END
059800960229     C*
059900960229     C                   ENDIF                                                  <-- 6A --
060000960229     C*
060100930602     C* TRASFORMO I PROGRESSIVI ORE IN DATE
060200930602     C                   EXSR      TPODAT
060300930602     C*
060400930914     C* CONTROLLO SE BOLLA RIENTRA IN UNO DEI CASI IN CUI NON RIESCO A CALCOLA
060500930602     C* RE I RITARDI PER CUI FORZO CONFORMITA' A SI
060600930602     C     D98ERR        IFEQ      '0'
060700960229     C     D98TLA        ANDNE     'T'
060800930602     C                   EXSR      CHKFOR
060900930602     C                   END
061000930602     C*
061100930602     C     D98ERR        IFEQ      '0'                                          --- 7 -->
061200960229     C     D98TLA        ANDNE     'T'
061300941228     C*
061400941228     C* CALCOLA LE ORE REALI DI PARTENZA
061500941228     C     D98ERR        IFEQ      '0'
061600941228     C                   EXSR      CALORP
061700941228     C                   END
061800930405     C*
061900930511     C* CALCOLA LE ORE REALI DI TRAZIONE
062000930420     C     D98ERR        IFEQ      '0'
062100930407     C                   EXSR      CALORT
062200930420     C                   END
062300930405     C*
062400930511     C* CALCOLA LE ORE REALI DI CONSEGNA
062500930420     C     D98ERR        IFEQ      '0'
062600930416     C                   EXSR      CALORC
062700930420     C                   END
062800930416     C*
062900930511     C* CALCOLA LE ORE REALI DI RESA
063000930420     C     D98ERR        IFEQ      '0'
063100930416     C                   EXSR      CALORR
063200930420     C                   END
063300930420     C*
063400930511     C* CALCOLA LE ORE DI DIFFERENZA REALI/TEORICHE
063500930420     C     D98ERR        IFEQ      '0'
063600930420     C                   EXSR      CALDIF
063700930420     C                   END
063800930420     C*
063900930511     C* CALCOLA LE CONFORMITA'
064000930420     C     D98ERR        IFEQ      '0'
064100930420     C                   EXSR      CALCNF
064200930420     C                   END
064300930913     C*
064400930913     C* CALCOLA LE ORE DI CONSEGNABILITA'
064500930913     C     D98ERR        IFEQ      '0'
064600950407     C     FLCONS        ANDEQ     'S'
064700930913     C                   EXSR      CALCNS
064800930913     C                   END
064900930423     C*
065000930511     C* PORTO I VALORI DAI CAMPI DI LAVORO AI CAMPI IN OUTPUT DELLA DS
065100930423     C     D98ERR        IFEQ      '0'
065200930423     C                   EXSR      W98D98
065300930423     C                   END
065400930528     C*
065500930602     C                   END                                                    <-- 7 ---
065600930602     C                   END                                                    <-- 6 ---
065700930528     C                   END                                                    <-- 5 ---
065800930528     C                   END                                                    <-- 4 ---
065900930528     C                   END                                                    <-- 3 ---
066000930528     C                   END                                                    <-- 2 ---
066100930528     C                   END                                                    <-- 1 ---
066200930323     C*
066300930324     C* SE PGM LANCIATO CON 'C' (LANCI MULTIPLI) CHIUDE CON LR (CHIUDE FILE)
066400930527     C* E LANCIA TUTTI I PGM CHIAMATI PER CHIUDERE I FILE DA LORO APERTI
066500930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
066600930324     C     D98TLA        IFEQ      'C'
066700970714     C                   CLEAR                   SI95
066800970701     C                   MOVEL     'C'           I95TLA
066900020226     C                   CALL      'TISI95R'
067000970701     C                   PARM                    SI95
067100040809     c*
067200040809     c                   clear                   fidn12ds
067300040809     c                   eval      i12tla = 'C'
067400040809     c                   call      'FIDN12R'
067500040809     c                   parm                    fidn12ds
067600050523     c**
067700050523     c* controllo riempimento skiera CAK solo alla fine
067800050523     c**                 clear                   trul0sds
067900050523     c**                 eval      i0sski='CAK/CAM/CAP'
068000050523     c**                 eval      i0sele=%elem(CAK)
068100050523     c**                 if        windca>i0sele
068200050523     c**                 eval      i0spie=i0sele
068300050523     c**                 else
068400050523     c**                 eval      i0spie=windca
068500050523     c**                 endif
068600050523     c**                 eval      i0sfile='AZCLN00F  '
068700050523     c**                 eval      i0scod='     '
068800050523     c**                 eval      i0spgm='TNSD99R'
068900050523     c**                 movel     trul0sds      kpjbu
069000050523     c**                 call      'TRUL0SR'
069100050523     c**                 parm                    kpjba
069200050523     c
069300930323     C                   SETON                                        LR
069400930324     C                   ELSE
069500930324     C                   SETON                                        RT
069600930323     C                   END
069700941227     C********************************************************************
069800941227     C*   LETARB - LEGGE IL FILE FNARB (BOLLE ARRIVI)
069900941227     C********************************************************************
070000941227     C     LETARB        BEGSR
070100941227     C*
070200140820     C* LEGGO IL FILE FNARB01L
070300941227     C                   Z-ADD     D98AAS        ARBAAS
070400941227     C                   Z-ADD     D98LNP        ARBLNP
070500941227     C                   Z-ADD     D98NRS        ARBNRS
070600941227     C                   Z-ADD     D98NSP        ARBNSP
070700941228     C     KEYARB        CHAIN     FNARB01L                           99
070800941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
070900941227     C                   MOVEL     '1'           D98ERR
071000941227     C                   MOVEL     MSG(20)       D98MSG                         *BOLLA INESISTENTE
071100941227     C                   END                                                    <-- 1 ---
071200941228     C     D98ERR        IFEQ      '0'                                          --- 1 --> *TROVATA
071300941227     C                   MOVEL     ARBTSP        D98TSP
071400140820     c* Se forzato da Ds lo imposto al posto di quello della bolla
071500130529     c                   if        i98tsp_for<>' '
071600130529     C                   MOVEL     i98tsp_for    D98TSP
071700130529     c                   endif
071800941228     C                   MOVEL     ARBNZD        D98NZD
071900941227     C                   MOVEL     ARBCAD        D98CAD
072000970701     C                   MOVEL     ARBLOD        D98LOD
072100941227     C                   Z-ADD     ARBTFP        D98TFP
072200941227     C                   Z-ADD     ARBTFA        D98TFA
072300941227     C                   Z-ADD     ARBLNA        D98LNA
072400941227     C                   Z-ADD     ARBAAS        DSAAS
072500941227     C                   Z-ADD     ARBMGS        DSMGS
072600941227     C                   Z-ADD     DSDAT8        D98DSP
072700941227     C                   Z-ADD     ARBMGS        D98MGS
072800941227     C                   Z-ADD     ARBDTI        D98DTI
072900941227     C                   Z-ADD     ARBHTI        D98HTI
073000941227     C                   Z-ADD     ARBDCM        D98DCM
073100941227     C                   Z-ADD     ARBHMC        D98HMC
073200941227     C                   MOVEL     ARBGC1        D98GC1
073300941227     C                   MOVEL     ARBGC2        D98GC2
073400941227     C                   Z-ADD     ARBDCR        D98DCR
073500941227     C                   Z-ADD     ARBHCR        D98HCR
073600100308     C                   MOVEL     ARBTCR        D98TCR
073700100308     c* salvo i campi perchè in alcuni casi li clearo
073800100308     C                   Z-ADD     ARBDCR        s98DCR
073900100308     C                   Z-ADD     ARBHCR        s98HCR
074000100308     C                   MOVEL     ARBTCR        s98TCR
074100100308     c
074200941227     C                   MOVEL     ARBCMC        D98CMC
074300170818     C                   MOVEL     ARBntc        wd98ntc_con
074400941227     C                   MOVEL     ARBFFD        D98FFD
074500941227     C                   Z-ADD     ARBNCD        D98NCD
074600941227     C                   MOVEL     ARBFNS        D98FNS
074700941227     C                   Z-ADD     ARBDUC        D98DUC
074800150508     C                   Z-ADD     ARBDPC        W98DPC
074900970310     C                   Z-ADD     *ZEROS        W98HUC
075000071122     c
075100071122     c                   movel     arbcbo        d98cbo
075200140820     c                   z-add     arbddc        d98ddc
075300140825     c                   z-add     arbndc        wd98ndc
075400140911     c                   movel     arbfbc        wd98fbc
075500150119     c                   movel     arbacc        wd98acc
075600150325     c                   exsr      IN_dist
075700150331     c* Se in fermo deposito vedo se presente particlarità consegna
075800150331     c                   if        arbffd='S' and arbgma<>*blanks
075900150331     C                   Z-ADD     CODUT         KTBUTE
076000150331     C                   MOVEL     '7R'          KTBCOD
076100150331     C                   MOVEL(P)  arbgma        KTBKEY
076200150331     C     KEYTAB        chain     TABEL00F
076300150331     c                   if        %found(tabel00f)
076400150331     c                   movel     tbluni        ds7r
076500150331     c                   endif
076600150331     c                   endif
076700150325     c
076800070511     c
076900141215     C                   Z-ADD     ARBDBR        D98DBR
077000170104     c
077100170105     C                   Z-ADD     ARBDBR        d98dfv
077200141215     c                   eval      d98fdrt=' '
077300070511     c
077400141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
077500070511     c* La data ritiro è presente solo su fnblp
077600070511     C     KEYARB        CHAIN     FNblp01L                           99
077700141215     c                   if        %found(fnblp01l)
077800070511     C                   Z-ADD     blpDrt        D98DBR
077900070511     c                   eval      d98fdrt='S'
078000070511     c                   endif
078100141215     c******             endif
078200070511     c
078300110923     C                   movel     ARBxco        D98xco
078400110923     C                   Z-ADD     ARBDAM        D98DAM
078500941227     C                   MOVEL     ARBFIN        D98FIN
078600990308     C                   MOVEL     ARBFBR        WFBR
078700031201     C                   MOVEL     ARBGGA        WGGA
078800050901     C                   MOVEL     ARBcca        d98cca
078900051124     C                   MOVEL     ARBTC1        d98TC1
079000051124     C                   MOVEL     ARBTC2        d98TC2
079100941227     C                   Z-ADD     1             I
079200941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
079300941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
079400941227     C                   MOVEL     '1'           D98ERR
079500941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
079600941227     C                   ELSE                                                   <-- 2 -->
079700941227     C                   MOVEL     T3A(I)        D98TBL
079800941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
079900990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
080000941227     C                   END                                                    <-- 2 ---
080100130826
080200130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
080300130826     c                   if        %subst(d98tbl:1:1) ='F'
080400130826     C                   Z-ADD     ARBKSC        D98KSC
080500130826     c                   else
080600130826     c
080700130826     c                   if        arbccm>0
080800130826     C                   Z-ADD     ARBccm        D98KSC
080900130826     c                   endif
081000130826     c                   if        d98ksc=0
081100130826     c                   z-add     8888          d98ksc
081200130826     c                   movel     d98lnp        d98ksc
081300130826     c                   endif
081400130826     c                   endif
081500130826     c
081600941227     C                   END                                                    <-- 1 ---
081700941228     C*
081800051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
081900950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
082000941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
082100941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
082200941228     C                   Z-ADD     D98AAS        AR9AAS
082300941228     C                   Z-ADD     D98LNP        AR9LNP
082400941228     C                   Z-ADD     D98NRS        AR9NRS
082500941228     C                   Z-ADD     D98NSP        AR9NSP
082600051115     C     KEYAR9        CHAIN     FiAR901L                           99
082700950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
082800950102     C                   MOVEL     'S'           WFLASS
082900950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
083000950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
083100950102     C                   Z-ADD     D98AAS        LBLAAN
083200950102     C                   Z-ADD     D98LNP        LBLLPN
083300950102     C                   Z-ADD     D98NRS        LBLNRN
083400950102     C                   Z-ADD     D98NSP        LBLNSN
083500950102     C     KEYLBL        CHAIN     FNLBL01L                           99
083600950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
083700950102     C                   Z-ADD     LBLAAP        AR9AAS
083800950102     C                   Z-ADD     LBLLPP        AR9LNP
083900950102     C                   Z-ADD     LBLNRP        AR9NRS
084000950102     C                   Z-ADD     LBLNSP        AR9NSP
084100051115     C     KEYAR9        CHAIN     FiAR901L                           99
084200950102     C     *IN99         IFEQ      '0'
084300950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
084400950102     C                   ENDIF
084500950102     C                   ENDIF                                                  <-- 3 ---
084600950102     C                   ENDIF                                                  <-- 2 ---
084700950102     C*
084800950102     C* CONTRASSEGNO NON TROVATO
084900950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
085000941228     C                   ELSE                                                   <-- 2 -->
085100950102     C* CONTRASSEGNO TROVATO
085200941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
085300010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
085400010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
085500941228     C                   ENDIF                                                  <-- 2 ---
085600941228     C                   ENDIF                                                  <-- 1 ---
085700150325     c
085800941227     C*
085900050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
086000941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
086100050405     C                   Z-ADD     D98AAS        KWGCPAAS
086200050405     C                   Z-ADD     D98LNP        KWGCPLNP
086300050405     C                   Z-ADD     D98NRS        KWGCPNRS
086400050405     C                   Z-ADD     D98NSP        KWGCPNSP
086500050405     C                   Z-ADD     *ZEROS        KWGCPFRG
086600050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
086700941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
086800941227     C     GCPATB        ANDNE     'A'
086900960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
087000960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
087100960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
087200161107$$$$ C                   movel     gcpfas        D98gfas
087300050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
087400960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
087500960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
087600050405     C* O UGUALE A 35 INVECE DI 40)
087700960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
087800050405     C     GCPFAS        ANDGE     40
087900941227     C                   Z-ADD     GCPDCG        D98DCG
088000950112     C                   Z-ADD     GCPDCG        W98DCG
088100960123     C                   ENDIF
088200960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
088300960729     C     GCPFAS        ANDGE     35
088400070202     C     GCPFAS        ANDne     36
088500960729     C                   Z-ADD     GCPDCG        D98DCG
088600960729     C                   Z-ADD     GCPDCG        W98DCG
088700960729     C                   ENDIF
088800941228     C                   ENDIF
088900941228     C                   ENDIF                                                  <-- 1 ---
089000970911     C*
089100970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
089200970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
089300970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
089400050922     C*****              Z-ADD     D98DTI        W98DTI
089500971210     C                   Z-ADD     D98LNP        F
089600971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
089700970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
089800970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
089900150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
090000150120     c                   if        arbdcp=0 and d98dfc=0
090100150120     C                   Z-ADD     d98dam        D98DTI
090200150120     c                   endif
090300150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
090400150120     C                   Z-ADD     d98dam        D98DTI
090500150120     c                   endif
090600150120     c                   ENDIF
090700150120     c***
090800150120     C***                Z-ADD     D98AAS        ARTAAS
090900150120     C***                Z-ADD     D98LNP        ARTLNP
091000150120     C***                Z-ADD     D98NRS        ARTNRS
091100150120     C***                Z-ADD     D98NSP        ARTNSP
091200150120     C***  KEYART        SETLL     FNART01L                           99
091300150120     C**N99KEYART        READE     FNART01L                               99
091400150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
091500150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
091600970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
091700150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
091800970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
091900970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
092000150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
092100150120     C***  ARTDCM        ANDEQ     *ZEROS
092200970912     C* SE PRIMO REC. TROVATO, MEM. DATA
092300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
092400150120     C***                Z-ADD     ARTDAM        D98DTI
092500150120     C***                ELSE                                                   <-- 6 -->
092600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
092700150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
092800150120     C***                Z-ADD     ARTDAM        D98DTI
092900150120     C***                ENDIF                                                  <-- 7 ---
093000150120     C***                ENDIF                                                  <-- 6 ---
093100150120     C***                ENDIF                                                  <-- 5 ---
093200970911     C* C'E' DATA CONSEGNAB.
093300150120     C***                ELSE                                                   <-- 4 -->
093400970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
093500970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
093600150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
093700150120     C***  ARTDAM        ANDLE     D98DFC
093800970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
093900970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
094000150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
094100150120     C***  ARTDCM        ORGT      D98DFC
094200970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
094300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
094400150120     C***                Z-ADD     ARTDAM        D98DTI
094500150120     C***                ELSE                                                   <-- 7 -->
094600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
094700150120     C***  ARTDAM        IFLT      D98DTI
094800150120     C***                Z-ADD     ARTDAM        D98DTI
094900150120     C***                ENDIF
095000150120     C***                ENDIF                                                  <-- 7 ---
095100150120     C***                ENDIF                                                  <-- 6 ---
095200150120     C***                ENDIF                                                  <-- 5 ---
095300150120     C***                ENDIF                                                  <-- 4 ---
095400150120     C***                ENDIF                                                  <-- 3 ---
095500150120     C***  KEYART        READE     FNART01L                               99
095600150120     C***                ENDDO                                                  <-- 2 ---
095700150120     C***                ENDIF                                                  <-- 1 ---
095800941227     C*
095900941227     C                   ENDSR
096000150325     C********************************************************************
096100150325     C*   Verifico se bolla in distinta
096200150325     C********************************************************************
096300150325     c     IN_dist       BEGSR
096400150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
096500150325     c                   if         ARBfbc = *blanks and
096600150325     c                               ARBddc > 0 and
096700150325     c                               ARBndc > 0 and ARBdcm=0
096800150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
096900150325     c                   if        not %open(fiapd01l)
097000150325     c                   open      fiapd01l
097100150325     c                   endif
097200150325     c
097300150325     C                   move      'A'           kpdtip
097400150325     c                   eval      kpdpdr=arbpdc
097500150325     C     kfapd         chain     fiapd01l
097600150325     c
097700150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
097800150325     c                   eval       d98cons = 'S'
097900160506     c                   else
098000160506     c                   eval       d98disf = 'F'
098100150325     c                   endif
098200150325     c                   endif
098300150325     c                   ENDSR
098400941227     C********************************************************************
098500941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
098600941227     C********************************************************************
098700941227     C     LETBLP        BEGSR
098800941227     C*
098900941227     C                   Z-ADD     D98AAS        BLPAAS
099000941227     C                   Z-ADD     D98LNP        BLPLNP
099100941227     C                   Z-ADD     D98NRS        BLPNRS
099200941227     C                   Z-ADD     D98NSP        BLPNSP
099300941227     C     KEYBLP        CHAIN     FNBLP01L                           99
099400941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
099500941227     C                   MOVEL     '1'           D98ERR
099600941227     C                   MOVEL     MSG(20)       D98MSG
099700941227     C                   END                                                    <-- 1 ---
099800941228     C     *IN99         IFEQ      '0'                                                    *TROV.
099900041028     C     KEYBLP        CHAIN     FNarb01L
100000041028     c                   if        %found(fnarb01l)
100100041028     c                   eval      wesarb='S'
100200041028     c                   else
100300041028     c                   eval      wesarb='N'
100400150219     c                   eval      wtrasm='D'
100500041028     c                   endif
100600141216     c
100700141216     c* se non trasmessa la bolla in arrivo memorizzo
100800150219     c                   if        blpft1= ' '
100900141229     c                   eval      wtrasm='N'
101000141216     c                   endif
101100141216     c
101200110923     C                   MOVEL     BLPxco        D98xco
101300110923     C                   MOVEL     BLPTSP        D98TSP
101400130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
101500130529     c                   if        i98tsp_for<>' '
101600130529     C                   MOVEL     i98tsp_for    D98TSP
101700130529     c                   endif
101800960209     C                   MOVEL     BLPNZD        D98NZD
101900941228     C                   MOVEL     BLPCAD        D98CAD
102000970701     C                   MOVEL     BLPLOD        D98LOD
102100941227     C                   Z-ADD     BLPTFP        D98TFP
102200941227     C                   Z-ADD     BLPTFA        D98TFA
102300941227     C                   Z-ADD     BLPLNA        D98LNA
102400941227     C                   Z-ADD     BLPAAS        DSAAS
102500941227     C                   Z-ADD     BLPMGS        DSMGS
102600941227     C                   Z-ADD     DSDAT8        D98DSP
102700941227     C                   Z-ADD     BLPMGS        D98MGS
102800170104
102900170104     c* se si tratta di prepagato NON confermato, devo fare finta che sia bollettato oggi
103000170104     c                   if        wtrasm='N'  and blppdr=0
103100170104     c                   movel     wdaodi        d98dsp
103200170104     c                   z-add     wdaodi        d98mgs
103300170104     c                   endif
103400170104     c
103500051124     C                   movel     BLPTC1        D98TC1
103600051124     C                   movel     BLPTC2        D98TC2
103700041028     c                   if        wesarb='S'
103800041028     C                   Z-ADD     arbDTI        D98DTI
103900041028     C                   Z-ADD     arbHTI        D98HTI
104000041028     C                   Z-ADD     arbDCM        D98DCM
104100041028     C                   Z-ADD     arbHMC        D98HMC
104200041028     C                   Z-ADD     arbDAM        D98DAM
104300050901     C                   MOVEL     arbcca        d98cca
104400140820     C                   MOVEL     arbddc        d98ddc
104500140911     C                   z-add     arbndc        wd98ndc
104600140910     C                   MOVEL     arbfbc        wd98fbc
104700150119     c                   movel     arbacc        wd98acc
104800170818     c                   movel     arbcmc        d98cmc
104900170818     C                   MOVEL     ARBntc        wd98ntc_con
105000150325     c                   exsr      IN_DIST
105100140820     c*
105200041028     c                   else
105300941227     C                   Z-ADD     BLPDTI        D98DTI
105400941227     C                   Z-ADD     BLPHTI        D98HTI
105500941227     C                   Z-ADD     BLPDCM        D98DCM
105600941227     C                   Z-ADD     BLPHMC        D98HMC
105700041028     C                   Z-ADD     BLPDAM        D98DAM
105800050901     C                   MOVEL     BLPcca        d98cca
105900140820     C                   MOVEL     BLPddc        d98ddc
106000140825     C                   MOVEL     BLPndc        wd98ndc
106100140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
106200140820     c                   clear                   d98cons
106300170818     C                   MOVEL     *BLANKS       D98CMC
106400170821     c* in partenza non posso determinale i tentivi di consegna
106500170818     C                   clear                   wd98ntc_con
106600041028     c                   endif
106700941227     C                   MOVEL     BLPGC1        D98GC1
106800941227     C                   MOVEL     BLPGC2        D98GC2
106900941227     C                   Z-ADD     BLPDCR        D98DCR
107000941227     C                   Z-ADD     BLPHCR        D98HCR
107100941227     C                   MOVEL     BLPTCR        D98TCR
107200100308     c* Salvo i campi perchè in alcuni casi li clearo
107300100308     C                   Z-ADD     blpDCR        s98DCR
107400100308     C                   Z-ADD     blpHCR        s98HCR
107500100308     C                   MOVEL     blpTCR        s98TCR
107600100308     c
107700941227     C                   MOVEL     BLPFFD        D98FFD
107800941227     C                   Z-ADD     BLPNCD        D98NCD
107900941227     C                   MOVEL     BLPFNS        D98FNS
108000941228     C                   Z-ADD     BLPDUC        D98DUC
108100150508     C                   Z-ADD     BLPDPC        w98DPC
108200970310     C                   Z-ADD     *ZEROS        W98HUC
108300071122     c                   movel     blpcbo        d98cbo
108400150331     c*
108500150331     c* Se in fermo deposito vedo se presente particlarità consegna
108600150331     c                   if        blpffd='S' and blpgma<>*blanks
108700150331     C                   Z-ADD     CODUT         KTBUTE
108800150331     C                   MOVEL     '7R'          KTBCOD
108900150331     C                   MOVEL(P)  blpgma        KTBKEY
109000150331     C     KEYTAB        chain     TABEL00F
109100150331     c                   if        %found(tabel00f)
109200150331     c                   movel     tbluni        ds7r
109300150331     c                   endif
109400150331     c                   endif
109500150331     c
109600970304     C                   Z-ADD     BLPDBR        D98DBR
109700170105     C                   Z-ADD     BLPDBR        d98dfv
109800070514     c                   eval      d98fdrt=' '
109900070514     c
110000141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
110100070514     C                   Z-ADD     blpDrt        D98DBR
110200070514     c                   eval      d98fdrt='S'
110300141215     c********           endif
110400070514     c*
110500941227     C                   MOVEL     BLPFIN        D98FIN
110600130826     C****               Z-ADD     BLPKSC        D98KSC
110700990308     C                   MOVEL     BLPFBR        WFBR
110800031201     C                   MOVEL     BLPGGA        WGGA
110900941227     C                   Z-ADD     1             I
111000941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
111100941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
111200941227     C                   MOVEL     '1'           D98ERR
111300941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
111400941227     C                   ELSE                                                   <-- 2 -->
111500941227     C                   MOVEL     T3A(I)        D98TBL
111600941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
111700990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
111800941227     C                   END                                                    <-- 2 ---
111900130826     c                   if        %subst(d98tbl:1:1) ='F'
112000130826     C                   Z-ADD     blpKSC        D98KSC
112100130826     c                   else
112200130826     c
112300130826     c                   if        blpccm>0
112400130826     C                   Z-ADD     blpccm        D98KSC
112500130826     c                   endif
112600130826     c                   if        d98ksc=0
112700130826     c                   z-add     8888          d98ksc
112800130826     c                   movel     d98lnp        d98ksc
112900130826     c                   endif
113000130826     c                   endif
113100130826     c
113200941227     C                   END
113300941228     C*
113400051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
113500950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
113600941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
113700941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
113800051115     C                   Z-ADD     D98AAS        ar9AAS
113900051115     C                   Z-ADD     D98LNP        ar9LNP
114000051115     C                   Z-ADD     D98NRS        ar9NRS
114100051115     C                   Z-ADD     D98NSP        ar9NSP
114200051115     C     KEYar9        CHAIN     Fiar901L                           99
114300950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
114400950102     C                   MOVEL     'S'           WFLASS
114500950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
114600950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
114700950102     C                   Z-ADD     D98AAS        LBLAAN
114800950102     C                   Z-ADD     D98LNP        LBLLPN
114900950102     C                   Z-ADD     D98NRS        LBLNRN
115000950102     C                   Z-ADD     D98NSP        LBLNSN
115100950102     C     KEYLBL        CHAIN     FNLBL01L                           99
115200950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
115300051115     C                   Z-ADD     LBLAAP        ar9AAS
115400051115     C                   Z-ADD     LBLLPP        ar9LNP
115500051115     C                   Z-ADD     LBLNRP        ar9NRS
115600051115     C                   Z-ADD     LBLNSP        ar9NSP
115700051115     C     KEYar9        CHAIN     Fiar901L                           99
115800950102     C     *IN99         IFEQ      '0'
115900950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
116000950102     C                   ENDIF
116100950102     C                   ENDIF                                                  <-- 3 ---
116200950102     C                   ENDIF                                                  <-- 2 ---
116300950102     C*
116400950102     C* CONTRASSEGNO TROVATO
116500150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
116600051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
116700051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
116800051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
116900941228     C                   ENDIF                                                  <-- 2 ---
117000941228     C                   ENDIF                                                  <-- 1 ---
117100941227     C*
117200050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
117300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
117400050405     C                   Z-ADD     D98AAS        KWGCPAAS
117500050405     C                   Z-ADD     D98LNP        KWGCPLNP
117600050405     C                   Z-ADD     D98NRS        KWGCPNRS
117700050405     C                   Z-ADD     D98NSP        KWGCPNSP
117800050405     C                   Z-ADD     *ZEROS        KWGCPFRG
117900050405     C     KEYGCP        CHAIN     TIGCP01L                           99
118000941228     C     *IN99         IFEQ      '0'                                          *TROV.
118100941227     C     GCPATB        ANDNE     'A'
118200960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
118300960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
118400960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
118500960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
118600161107$$$$ C                   movel     gcpfas        D98Gfas
118700050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
118800960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
118900960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
119000050405     C* O UGUALE A 35 INVECE DI 40)
119100960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
119200050405     C     GCPFAS        ANDGE     40
119300941227     C                   Z-ADD     GCPDCG        D98DCG
119400950112     C                   Z-ADD     GCPDCG        W98DCG
119500960123     C                   ENDIF
119600960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
119700960729     C     GCPFAS        ANDGE     35
119800070202     C     GCPFAS        ANDne     36
119900960729     C                   Z-ADD     GCPDCG        D98DCG
120000960729     C                   Z-ADD     GCPDCG        W98DCG
120100960729     C                   ENDIF
120200941228     C                   ENDIF
120300941228     C                   ENDIF                                                  <-- 1 ---
120400970912     C*
120500970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
120600970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
120700970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
120800050922     C******             Z-ADD     D98DTI        W98DTI
120900971210     C                   Z-ADD     D98LNP        F
121000971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
121100970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
121200970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
121300150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
121400150120     c                   if        blpdcp=0 and d98dfc=0
121500150120     C                   Z-ADD     d98dam        D98DTI
121600150120     c                   endif
121700150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
121800150120     C                   Z-ADD     d98dam        D98DTI
121900150120     c                   endif
122000150120     c                   ENDIF
122100150120     c*
122200150120     C***                Z-ADD     D98AAS        BLTAAS
122300150120     C****               Z-ADD     D98LNP        BLTLNP
122400150120     C***                Z-ADD     D98NRS        BLTNRS
122500150120     C***                Z-ADD     D98NSP        BLTNSP
122600150120     C***  KEYBLT        SETLL     FNBLT01L                           99
122700150120     C**N99KEYBLT        READE     FNBLT01L                               99
122800150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
122900150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
123000970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
123100150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
123200970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
123300970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
123400150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
123500150120     C**   BLTDCM        ANDEQ     *ZEROS
123600970912     C* SE PRIMO REC. TROVATO, MEM. DATA
123700150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
123800150120     C**                 Z-ADD     BLTDAM        D98DTI
123900150120     C**                 ELSE                                                   <-- 6 -->
124000970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
124100150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
124200150120     C**                 Z-ADD     BLTDAM        D98DTI
124300150120     C**                 ENDIF                                                  <-- 7 ---
124400150120     C**                 ENDIF                                                  <-- 6 ---
124500150120     C**                 ENDIF                                                  <-- 5 ---
124600970912     C* C'E' DATA CONSEGNAB.
124700150120     **                  ELSE                                                   <-- 4 -->
124800970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
124900970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
125000150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
125100150120     C**   BLTDAM        ANDLT     D98DFC
125200970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
125300970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
125400150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
125500150120     C**   BLTDCM        ORGT      D98DFC
125600970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
125700150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
125800150120     C**                 Z-ADD     BLTDAM        D98DTI
125900150120     C**                 ELSE                                                   <-- 7 -->
126000970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
126100150120     C**   BLTDAM        IFLT      D98DTI
126200150120     C**                 Z-ADD     BLTDAM        D98DTI
126300150120     C**                 ENDIF
126400150120     C**                 ENDIF                                                  <-- 7 ---
126500150120     C**                 ENDIF                                                  <-- 6 ---
126600150120     C**                 ENDIF                                                  <-- 5 ---
126700150120     C**                 ENDIF                                                  <-- 4 ---
126800150120     C**                 ENDIF                                                  <-- 3 ---
126900150120     C**   KEYBLT        READE     FNBLT01L                               99
127000150120     C**                 ENDDO                                                  <-- 2 ---
127100150120     C**                 ENDIF                                                  <-- 1 ---
127200941227     C*
127300941227     C                   ENDSR
127400930323     C********************************************************************
127500941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
127600930323     C********************************************************************
127700930323     C     RTTEOR        BEGSR
127800941227     C*
127900941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
128000941228     C     D98ERR        IFEQ      '0'
128100941227     C                   EXSR      RTPART
128200941227     C                   ENDIF
128300941227     C*
128400941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
128500941228     C     D98ERR        IFEQ      '0'
128600150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
128700150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
128800150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
128900150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
129000150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
129100941227     C                   EXSR      RTTRAZ
129200150219     c                   if        d98err<>'1'
129300150219     C                   Z-ADD     CASTIN        D98TTI
129400150219     C                   Z-ADD     CASTCN        D98TTC
129500150219     C                   endif
129600150219     C                   ENDIF
129700941227     C*
129800941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
129900941228     C     D98ERR        IFEQ      '0'
130000941227     C                   EXSR      RTCONS
130100941227     C                   ENDIF
130200941227     C*
130300941228     C     D98ERR        IFEQ      '0'
130400941227     C*
130500941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
130600941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
130700941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
130800090921     c                   z-add     d98tri        w99tri
130900090921     c* per la DS
131000090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
131100090921     c* per i calcoli interi
131200090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
131300090921     c
131400941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
131500941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
131600941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
131700090921     c                   z-add     d98trc        w99trc
131800090921     c* per la DS
131900090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
132000090921     c* per i calcoli interi
132100090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
132200941227     C*
132300941227     C                   ENDIF
132400930323     C*
132500930323     C                   ENDSR
132600941227     C********************************************************************
132700941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
132800941227     C********************************************************************
132900941227     C     RTPART        BEGSR
133000941228     C*
133100941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
133200941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
133300941228     C                   MOVEL     D98TSP        WTSP              1
133400130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
133500130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
133600050301     C*******
133700050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
133800050301     C*
133900050301     C*    W001          IFEQ      'A'
134000050301     C*    W0040         OREQ      8888
134100050301     C*                  MOVEL     'E'           WTSP
134200050301     C*                  ENDIF
134300050301     C*******
134400941227     C*
134500941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
134600941227     C                   MOVEL     'N'           WTRAOK            1
134700941227     C                   MOVEL     'P'           CASPTA
134800941227     C                   Z-ADD     D98LNP        CASLNP
134900941227     C                   Z-ADD     *ZEROS        CASLNA
135000941228     C                   MOVEL     WTSP          CASTSP
135100941228     C                   Z-ADD     99999999      CASDDE
135200941227     C     KEYCAS        SETGT     AZCAS01L                             99
135300941227     C  N99              READP     AZCAS01L                               99
135400941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
135500941227     C     WTRAOK        ANDEQ     'N'
135600941227     C     CASLNP        ANDEQ     D98LNP
135700950102     C     CASTSP        ANDEQ     WTSP
135800941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
135900941227     C     CASATB        ANDEQ     *BLANKS
136000941227     C                   MOVEL     'S'           WTRAOK
136100941227     C                   ELSE                                                   <-- 2 -->
136200941227     C                   READP     AZCAS01L                               99
136300941227     C                   ENDIF                                                  <-- 2 ---
136400941227     C                   ENDDO                                                  <-- 1 ---
136500941227     C*
136600941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
136700941227     C     WTRAOK        IFEQ      'N'
136800941227     C                   MOVEL     '1'           D98ERR
136900941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
137000941227     C                   ELSE
137100941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
137200941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
137300941227     C                   END
137400970214     C*
137500140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
137600140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
137700140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
137800140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
137900140403     C**                 Z-ADD     D98LNA        F
138000140403     C**   D98ERR        IFEQ      '0'
138100140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
138200140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
138300140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
138400140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
138500140403     C**                 ENDIF
138600140403     C**                 ENDIF
138700140402     c
138800140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
138900140402     c*  se tempi  = 0
139000140402     c                   if        wpick2='S' and d98tpi<=0
139100140402     c                   add       24            D98TPI
139200140402     c                   endif
139300941227     C*
139400941227     C                   ENDSR
139500941227     C********************************************************************
139600941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
139700941227     C********************************************************************
139800941227     C     RTTRAZ        BEGSR
139900941227     C*
140000941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
140100150219     c
140200150219     C                   Z-ADD     D98LNA        LTA(1)
140300150219     C                   Z-ADD     D98LNA        LTA(2)
140400150219     C                   Z-ADD     D98TFA        LTA(3)
140500941227     C                   Z-ADD     D98TFA        LTA(4)
140600941227     C*
140700941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
140800941227     C                   MOVEL     'N'           WTRAOK            1
140900941227     C                   Z-ADD     1             K                 3 0
141000941227     C*
141100941227     C* CICLO FINO A:
141200941227     C*    - FINE SCHIERA
141300941227     C*    - TROVATO TEMPI VALIDI
141400941227     C     K             DOUGT     4                                            --- 1 -->
141500941227     C     WTRAOK        OREQ      'S'
141600941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
141700941227     C     LTA(K)        ORNE      *ZEROS
141800941227     C                   MOVEL     'T'           CASPTA
141900941227     C                   Z-ADD     LTP(K)        CASLNP
142000941227     C                   Z-ADD     LTA(K)        CASLNA
142100941227     C                   MOVEL     D98TSP        CASTSP
142200941228     C                   Z-ADD     99999999      CASDDE
142300941227     C     KEYCAS        SETGT     AZCAS01L                             99
142400941227     C  N99              READP     AZCAS01L                               99
142500941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
142600941227     C     WTRAOK        ANDEQ     'N'
142700941227     C     LTP(K)        ANDEQ     CASLNP
142800941227     C     LTA(K)        ANDEQ     CASLNA
142900941227     C     D98TSP        ANDEQ     CASTSP
143000960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
143100941227     C     CASATB        ANDEQ     *BLANKS
143200941227     C                   MOVEL     'S'           WTRAOK
143300941227     C                   ELSE                                                   <-- 4 -->
143400941227     C                   READP     AZCAS01L                               99
143500941227     C                   ENDIF                                                  <-- 4 ---
143600941227     C                   ENDDO                                                  <-- 3 ---
143700941227     C                   ENDIF                                                  <-- 2 ---
143800941227     C                   ADD       1             K
143900941227     C                   ENDDO                                                  <-- 1 ---
144000941227     C*
144100941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
144200941227     C     WTRAOK        IFEQ      'N'
144300941227     C                   MOVEL     '1'           D98ERR
144400941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
144500150219     C***                ELSE
144600150219     C***                Z-ADD     CASTIN        D98TTI
144700150219     C***                Z-ADD     CASTCN        D98TTC
144800941227     C                   END
144900941227     C*
145000941227     C                   ENDSR
145100941227     C********************************************************************
145200941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
145300941227     C********************************************************************
145400941227     C     RTCONS        BEGSR
145500150629     c                   clear                   WTsogliaE         1
145600970701     C*
145700970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
145800970701     C                   CLEAR                   SI95
145900970701     C                   MOVEL     '3'           I95TCN
146000970701     C                   MOVEL     D98NZD        I95NAR
146100970701     C                   MOVEL     D98CAD        I95CAP
146200970701     C                   MOVEL     D98LOD        I95LOC
146300970701     C                   MOVEL     D98DSP        I95DAT
146400970701     C                   MOVEL     D98TSP        I95TSP
146500980602     C                   MOVEL     'S'           I95FRT
146600150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
146700150511     c                   if        d98ffd='S'
146800150511     c                   eval      i95tsp='C'
146900150511     c                   endif
147000020226     C*
147100020226     C* VERIFICO SE LNA E' FILIALE FEDEX
147200020226     C                   IF        D98LNA > *ZEROS
147300020226     C     D98LNA        LOOKUP    FILFED                                 55
147400020226     C                   IF        *IN55 = *ON
147500020226     C                   EVAL      I95FI1 = 'S'
147600020226     C                   ENDIF
147700020226     C                   ENDIF
147800020226     C*
147900020226     C                   CALL      'TISI95R'
148000970701     C                   PARM                    SI95
148100941227     C*
148200941228     C* SE NON TROVATO, ERRORE
148300970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
148400941227     C                   MOVEL     '1'           D98ERR
148500970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
148600941227     C                   ELSE                                                   <-- 1 -->
148700941227     C*
148800941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
148900941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
149000941227     C     CASTIN        DIV       24            WDIV              3 0
149100941227     C                   MVR                     WRES              3 0
149200941227     C     WRES          IFEQ      *ZEROS
149300970701     C                   Z-ADD     O950IN        D98TCI
149400941227     C                   ELSE
149500970701     C                   Z-ADD     O951IN        D98TCI
149600941227     C                   END
149700941227     C*
149800941227     C     CASTCN        DIV       24            WDIV                     99
149900941227     C                   MVR                     WRES
150000941227     C     WRES          IFEQ      *ZEROS
150100970701     C                   Z-ADD     O950CN        D98TCC
150200150629     c                   eval      wTSogliaE='0'
150300941227     C                   ELSE
150400970701     C                   Z-ADD     O951CN        D98TCC
150500150629     c                   eval      wTSogliaE='1'
150600941227     C                   END
150700941227     C*
150800150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
150900941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
151000150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
151100150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
151200150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
151300150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
151400150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
151500941227     C*
151600941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
151700941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
151800970701     C     O95ISO        IFEQ      'I'
151900941227     C     D98FIN        OREQ      'I'
152000970701     C                   ADD       O95TIS        D98TCI
152100970701     C                   ADD       O95TIS        D98TCC
152200941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
152300970701     C                   ADD       O95TIS        W990IN
152400970701     C                   ADD       O95TIS        W991IN
152500150629     C                   ADD       O95TIS        W990CN
152600150629     C                   ADD       O95TIS        W991CN
152700941227     C                   END
152800090827     c
152900090827     c* Sistemo i tempi di consegna se non sono multipli di 12
153000090827     c                   exsr      SisCONS
153100160404     c
153200160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
153300160404     c                   if        wVP='S'
153400160404     c                   add       24            w991IN
153500160404     c                   add       24            w991CN
153600160404     c                   add       24            w990IN
153700160404     c                   add       24            w990CN
153800160404     c                   endif
153900941228     C*
154000941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
154100941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
154200941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
154300941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
154400941228     C                   Z-ADD     D98TCC        W99CCE            3 0
154500941228     C                   ELSE                                                   <-- 3 -->
154600970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
154700970701     C                   CLEAR                   SI95
154800970701     C                   MOVEL     '3'           I95TCN
154900970701     C                   MOVEL     D98NZD        I95NAR
155000970701     C                   MOVEL     D98CAD        I95CAP
155100970701     C                   MOVEL     D98LOD        I95LOC
155200970701     C                   MOVEL     D98DSP        I95DAT
155300970701     C                   MOVEL     'E'           I95TSP
155400980602     C                   MOVEL     'S'           I95FRT
155500020226     C*
155600020226     C* VERIFICO SE LNA E' FILIALE FEDEX
155700020226     C                   IF        D98LNA > *ZEROS
155800020226     C     D98LNA        LOOKUP    FILFED                                 55
155900020226     C                   IF        *IN55 = *ON
156000020226     C                   EVAL      I95FI1 = 'S'
156100020226     C                   ENDIF
156200020226     C                   ENDIF
156300020226     C*
156400020226     C                   CALL      'TISI95R'
156500970701     C                   PARM                    SI95
156600970701     C*
156700941228     C* SE NON TROVATO, ERRORE
156800970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
156900941228     C                   MOVEL     '1'           D98ERR
157000970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
157100941228     C                   ELSE                                                   <-- 4 -->
157200941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
157300941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
157400941228     C     CASTCN        DIV       24            WDIV                     99
157500941228     C                   MVR                     WRES
157600941228     C     WRES          IFEQ      *ZEROS
157700970701     C                   Z-ADD     O950CN        W99CCE
157800941228     C                   ELSE
157900970701     C                   Z-ADD     O951CN        W99CCE
158000941228     C                   END
158100941228     C                   ENDIF                                                  <-- 4 ---
158200941228     C                   ENDIF                                                  <-- 3 ---
158300941228     C                   ENDIF                                                  <-- 2 ---
158400941227     C*
158500941227     C                   END                                                    <-- 1 ---
158600941227     C*
158700941227     C                   ENDSR
158800090827     C********************************************************************
158900090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
159000090827     C********************************************************************
159100090827     C     SisCONS       BEGSR
159200090827     c                   eval      w99tci=d98tci
159300090827     c                   if        d98tci=10
159400090827     c                   z-add     12            w99tci
159500090827     c                   endif
159600090827     c
159700090827     c                   eval      w99tcc=d98tcc
159800090827     c                   if        d98tcc=10
159900090827     c                   z-add     12            w99tcc
160000090827     c                   endif
160100160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
160200160404     c                   if        wVP='S'
160300160404     c                   add       24            w99tcc
160400160404     c                   add       24            w99tci
160500160404     c                   endif
160600090827     c
160700090827     C                   ENDSR
160800930420     C********************************************************************
160900930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
161000930420     C********************************************************************
161100930420     C     CAREVB        BEGSR
161200930420     C*
161300930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
161400930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
161500060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
161600930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
161700930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
161800060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
161900060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
162000930420     C                   Z-ADD     D98AAS        KEBAAS
162100930420     C                   Z-ADD     D98LNP        KEBLNP
162200930420     C                   Z-ADD     D98NRS        KEBNRS
162300930420     C                   Z-ADD     D98NSP        KEBNSP
162400990810     C     KEYEVB        SETLL     FNEVB01L                           99
162500060406     C  N99keyevb        READe     FNEVB01L                               99
162600060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
162700930421     C     D98ERR        ANDEQ     '0'
162800060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
162900930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
163000930421     C                   Z-ADD     1             I
163100930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
163200060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
163300930421     C                   MOVEL     '1'           D98ERR
163400930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
163500060406   x3C                   ELSE                                                   <-- 3 -->
163600060406     C                   MOVEL     D2A(I)        usa2A
163700060406     C                   MOVEL     D2Z(I)        usa2Z
163800930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
163900060406    4C     §2AFTC        IFEQ      'A'
164000930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
164100060406    4C                   END
164200060406    4C     §2AFTC        IFEQ      'G'
164300930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
164400060406    4C                   END
164500150216     c* Se si tratta di MIC verifico se uguale alla sped.
164600150303    4c                   if        evbcev='MIC' or evbcev='NIC'
164700150303    5c                   if        %subst(evbnot:1:7) >*zeros
164800150303     c
164900150303    6c                   if        evbcev='MIC' and
165000150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
165100150216     c                   eval      evemic='SI'
165200150303    6c                   endif
165300150303     c
165400150303    6c                   if        evbcev='NIC' and
165500150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
165600150216     c                   eval      evemic='  '
165700150303    6c                   endif
165800150303     c
165900150303   x5c                   else
166000150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
166100150303     c                   eval      evemic='SI'
166200150303    6c                   endif
166300150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
166400150303     c                   eval      evemic='  '
166500150303    6c                   endif
166600150303
166700150303    5c                   endif
166800150303    4c                   endif
166900150216     c
167000060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
167100930421     C     §2AFTC        OREQ      'G'
167200930421     C                   ADD       1             IE
167300990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
167400930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
167500930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
167600131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
167700090928     c
167800090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
167900091014     c**                 if        ie=1
168000091014     c**                 eval      Primotipo=§2aftc
168100091014     c**                 eval      Primodev=evbdev
168200091014     c**                 eval      Primohev=evbhev
168300091014     c**                 endif
168400090928     c
168500060406    4C                   END                                                    <-- 4 ---
168600060406     c* Verifico se si tratta di evento particolare
168700060406    4c                   if        §2zCEP<>' '
168800060526     c* non carico l'evento se cade in giorno festivo
168900060526     c                   z-add     evbdev        wpodata
169000060526     C                   EVAL      WTC='C'
169100060526     C                   EVAL      WIC='C'
169200060526     c                   eval      wslitt='N'
169300060526     c                   exsr      chkfes
169400060526     c
169500060526     c* Wslitt='N' non festivo
169600060526   4ac                   if        wslitt='N'
169700060526     c
169800060406    5c                   if        §2zfep='A'
169900060406     c                   eval      wevpaA='S'
170000060406    5C                   END                                                    <-- 3 ---
170100060406    5c                   if        §2zfep='G'
170200060406     c                   eval      wevpaG='S'
170300060406    5C                   END                                                    <-- 3 ---
170400060406     c                   add       1             ip
170500060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
170600060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
170700060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
170800060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
170900060526   4aC                   ENDif                                                  <-- 3 ---
171000060526    4C                   END                                                    <-- 3 ---
171100060406    3C                   END                                                    <-- 3 ---
171200060406    2C                   END                                                    <-- 2 ---
171300060406     C     keyevb        READe     FNEVB01L                               99
171400060406    1C                   ENDDO                                                  <-- 1 ---
171500050523     c* controllo riempimento skiere
171600050523     c                   clear                   trul0sds
171700050523     c                   eval      i0sski='EVD/EVO/EVC'
171800050523     c                   eval      i0sele=%elem(EVD)
171900050523     c                   eval      i0spie=ie
172000050523     c                   eval      i0sfile='FNEVB01L  '
172100050523     c                   eval      i0scod='     '
172200050523     c                   eval      i0spgm='TNSD99R'
172300050523     c                   movel     trul0sds      kpjbu
172400050523     c                   call      'TRUL0SR'
172500050523     c                   parm                    kpjba
172600930420     C*
172700060406     c* controllo riempimento skiere
172800060406     c                   clear                   trul0sds
172900060406     c                   eval      i0sski='PVD/PVO/PVC'
173000060406     c                   eval      i0sele=%elem(PVD)
173100060406     c                   eval      i0spie=ie
173200060406     c                   eval      i0sfile='FNEVB01L  '
173300060406     c                   eval      i0scod='     '
173400060406     c                   eval      i0spgm='TNSD99R'
173500060406     c                   movel     trul0sds      kpjbu
173600060406     c                   call      'TRUL0SR'
173700060406     c                   parm                    kpjba
173800060406     C*
173900930420     C                   ENDSR
174000931213     C********************************************************************
174100931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
174200931213     C********************************************************************
174300931213     C     CARTUC        BEGSR
174400931213     C*
174500931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
174600931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
174700931214     C     D98GC2        ANDEQ     *BLANKS
174800931213     C                   ELSE                                                   <-- 1 -->
174900931213     C*
175000931213     C                   MOVE      D98GC1        TUC(1)
175100931214     C                   MOVE      D98GC2        TUC(2)
175200931213     C*
175300931213     C                   END                                                    <-- 1 ---
175400931213     C*
175500931213     C                   ENDSR
175600970304     C********************************************************************
175700970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
175800970304     C********************************************************************
175900970304     C     DATRIF        BEGSR
176000970304     C*
176100050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
176200050922     c*  partenza
176300970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
176400970304     C*
176500970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
176600070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
176700070605     C***                MOVEL     '1'           D98ERR
176800070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
176900070605     C***                ELSE                                                   <-- 2 -->
177000970304     C*
177100970304     C                   Z-ADD     D98DRF        WD1
177200970304     C                   Z-ADD     D98ORF        WO1
177300970310     C* DATA PARTENZA ULTIMO COLLO
177400970310     C     D98DRF        IFLE      D98DUC
177500050928     c     d98duc        oreq      0
177600970310     C                   Z-ADD     D98DRF        D98DUC
177700970310     C                   Z-ADD     D98ORF        W98HUC
177800070605     C                   ENDIF
177900970304     C* DATA ARRIVO TRAZIONE
178000050922     C***                Z-ADD     D98DTI        WD2
178100050922     C***                Z-ADD     D98HTI        WO2
178200050922     C***  WDO1          IFLE      WDO2
178300050922     C***                Z-ADD     D98DRF        D98DTI
178400050922     C***                Z-ADD     D98ORF        D98HTI
178500050922     C***                ENDIF
178600970304     C* DATA CONSEGNA
178700050922     C***                Z-ADD     D98DCM        WD2
178800050922     C***                Z-ADD     D98HMC        WO2
178900050922     C***  WDO1          IFLE      WDO2
179000050922     C***                Z-ADD     D98DRF        D98DCM
179100050922     C***                Z-ADD     D98ORF        D98HMC
179200050922     C***                ENDIF
179300970304     C*
179400070605     C***                ENDIF                                                  <-- 2 ---
179500970304     C                   ENDIF                                                  <-- 1 ---
179600970304     C*
179700970304     C                   ENDSR
179800940901     C********************************************************************
179900940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
180000940901     C********************************************************************
180100940901     C     TDTIPO        BEGSR
180200940901     C*
180300940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
180400940902     C                   Z-ADD     D98TTI        W98TTI            3 0
180500941011     C*
180600941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
180700941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
180800941011     C     W98TTI        DIV       24            WDIV              3 0
180900941011     C                   MVR                     WRES              3 0
181000941011     C     WRES          IFEQ      *ZEROS
181100941011     C                   Z-ADD     *ZEROS        WASTA             9 0
181200941011     C                   ELSE
181300941011     C                   Z-ADD     12            WASTA
181400941011     C                   END
181500940901     C*
181600940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
181700940901     C     D98DTI        IFNE      *ZEROS
181800940901     C                   Z-ADD     D98DTI        WDAT
181900940901     C                   EXSR      DATORE
182000960911     C                   Z-ADD     WORE          WPODTI
182100960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
182200960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
182300960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
182400960911     C                   Z-ADD     WPODTI        W10POI            9 0
182500960911     C                   EXSR      R10FES
182600960911     C                   Z-ADD     W10POO        WPODTI
182700960911     C* AGGIUNGO LE ORA D'ASTA
182800960911     C                   ADD       WASTA         WPODTI
182900960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
183000960911     C     W10POI        IFEQ      W10POO
183100940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
183200940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
183300940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
183400940901     C                   EXSR      RO3ORE
183500940901     C                   ADD       WR3HHO        WPODTI
183600940901     C                   ADD       WR3HHO        W98TTI
183700940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
183800940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
183900940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
184000940901     C                   EXSR      RO3ORE
184100940901     C                   ADD       WR3HHO        WPODTI
184200940901     C                   ADD       WR3HHO        W98TTI
184300960911     C                   ENDIF                                                  *SE NON FESTIVO
184400940901     C                   END
184500940901     C*
184600940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
184700940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
184800940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
184900940901     C     W98TTI        DIV       24            WDIV              3 0
185000940901     C                   MVR                     WRES              3 0
185100940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
185200941228     C                   Z-ADD     W990IN        D98TCI
185300160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
185400160405     c*  per cui le sottraggo, per riagguingerle alla fine
185500160405     c                   if        wVP='S'
185600160405     c                   sub       24            D98TCI
185700160405     c                   endif
185800160405     c
185900150630     c                   eval      wTSogliaE='0'
186000940901     C                   ELSE                                                   <-- 2 -->
186100941228     C                   Z-ADD     W991IN        D98TCI
186200160405     c                   if        wVP='S'
186300160405     c                   sub       24            D98TCI
186400160405     c                   endif
186500150630     c                   eval      wTSogliaE='1'
186600940908     C                   ENDIF                                                  <-- 2 ---
186700090827     c* Sistemo i tempi di consegna se non sono multipli di 12
186800090827     c                   exsr      SisCONS
186900090827     c*
187000940908     C* RICALCOLO TEMPO DI RESA INTERNO
187100950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
187200950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
187300090921     c                   z-add     d98tri        w99tri
187400090921     c* per la DS
187500090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
187600090921     c* per i calcoli interni
187700090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
187800940908     C                   ENDIF                                                  <-- 1 ---
187900940901     C*
188000940901     C                   ENDSR
188100930325     C********************************************************************
188200930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
188300930325     C********************************************************************
188400171108     C     TDATPO        BEGSR
188500171108     c                   clear                   wodifes           1
188600171108     c                   clear                   wdspfes           1
188700930406     C*
188800171108     c* Verifico se la data odierna è festiva solo se bolla NON borderizzata
188900171108     c                   if        d98dfv=0
189000171108     c                   eval      WTC='P'
189100171108     C                   EVAL      WIC='I'
189200171108     c                   eval      wslitt='N'
189300171107     c                   eval      wpodata=wdaodi
189400171107     c                   exsr      chkfes
189500171108     c                   if        wslitt='S'
189600171110     c                   eval      wodifes ='S'
189700171108     c                   endif
189800171108     c                   endif
189900171107     c*
190000930420     C* TRASFORMO LA DATA ODIERNA
190100930416     C                   Z-ADD     WDAODI        WDAT
190200930416     C                   EXSR      DATORE
190300050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
190400960911     C                   MOVEL     'OSC  '       WR3TAB
190500960911     C                   Z-ADD     WHHODI        WR3HHI
190600960911     C                   EXSR      RO3ORE
190700050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
190800050922     c                   if        wr3hho=12
190900050922     c     wpoatt        add       12            wpoattdata24
191000050922     c                   else
191100050922     c                   z-add     wpoatt        wpoattdata24
191200050922     c                   endif
191300930325     C*
191400930420     C* TRASFORMO LA DATA DI SPEDIZIONE
191500931125     C     D98DSP        IFEQ      *ZEROS
191600930326     C                   MOVEL     '1'           D98ERR
191700930326     C                   MOVEL     MSG(2)        D98MSG
191800930326     C                   ELSE
191900931125     C                   Z-ADD     D98DSP        WDAT
192000930325     C                   EXSR      DATORE
192100930406     C                   Z-ADD     WORE          WPODSP
192200930326     C                   END
192300171108
192400171108     c* Verifico se la data sped.   è festiva
192500171110     c                   if        wodifes ='S'
192600171108     c                   eval      WTC='P'
192700171108     C                   EVAL      WIC='I'
192800171108     c                   eval      wslitt='N'
192900171108     c                   eval      wpodata=d98dsp
193000171108     c                   exsr      chkfes
193100171108     c                   if        wslitt='S'
193200171110     c                   eval      wdspfes ='S'
193300171108     c                   endif
193400171108     c
193500171108     c* In questo caso verifico se il terminal di partenza della bolla ha FGV aperto con la linea
193600171108     c*  di arrivo della bolla
193700171112     c                   if        %open(fnfgv02l)
193800171108     c                   exsr      Ctr_fgv
193900171108     c                   endif
194000171112     c                   endif
194100930414     C*
194200930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
194300930414     C                   MOVEL     'LIM  '       WR3TAB
194400930414     C                   Z-ADD     *ZEROS        WR3HHI
194500930414     C                   EXSR      RO3ORE
194600931222     C     WR3HHO        MULT      10            WTM060            6 0
194700931222     C     WPODSP        ADD       WTM060        WPOLIM
194800140822     c
194900930325     C*
195000930420     C* TRASFORMO LA DATA DI CONSEGNA
195100931125     C     D98DCM        IFNE      *ZEROS
195200931125     C                   Z-ADD     D98DCM        WDAT
195300930325     C                   EXSR      DATORE
195400930326     C                   Z-ADD     WORE          WPODCM
195500930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
195600930326     C                   MOVEL     'OSC  '       WR3TAB
195700931125     C                   Z-ADD     D98HMC        WR3HHI
195800930326     C                   EXSR      RO3ORE
195900930326     C                   ADD       WR3HHO        WPODCM
196000930326     C                   END
196100941228     C*
196200941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
196300941228     C     D98DUC        IFNE      *ZEROS
196400941228     C                   Z-ADD     D98DUC        WDAT
196500941228     C                   EXSR      DATORE
196600941228     C                   Z-ADD     WORE          WPODUC
196700970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
196800970310     C                   MOVEL     'OSP  '       WR3TAB
196900970310     C                   Z-ADD     W98HUC        WR3HHI
197000970310     C                   EXSR      RO3ORE
197100970310     C     D98DRF        IFNE      *ZEROS
197200050928     c     w98huc        andgt     1200
197300970310     C                   SUB       12            WR3HHO
197400970310     C                   ENDIF
197500970310     C                   ADD       WR3HHO        WPODUC
197600941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
197700070605     C***  WPODSP        IFGT      WPODUC
197800070605     C***                Z-ADD     WPODSP        WPODUC
197900070605     C***                ENDIF
198000941228     C                   ENDIF
198100960402     C*
198200141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
198300970304     C     D98DBR        IFNE      *ZEROS
198400970304     C                   Z-ADD     D98DBR        WDAT
198500960402     C                   EXSR      DATORE
198600960402     C                   Z-ADD     WORE          WPODBR
198700070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
198800960402     C                   ELSE
198900960402     C                   Z-ADD     WPODSP        WPODBR
199000960402     C                   ENDIF
199100930913     C*
199200960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
199300930913     C* CONSEGNABILITA'
199400960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
199500950407     C*
199600930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
199700931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
199800931125     C                   Z-ADD     D98DIC        WDAT
199900930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
200000930913     C                   Z-ADD     WORE          WPODIC
200100930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
200200931125     C                   Z-ADD     D98OIC        WR3HHI
200300930913     C                   EXSR      RO3ORE
200400930913     C                   ADD       WR3HHO        WPODIC
200500931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
200600950407     C*
200700930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
200800931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
200900931125     C                   Z-ADD     D98DFC        WDAT
201000930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
201100930913     C                   Z-ADD     WORE          WPODFC
201200140822     C                   Z-ADD     WORE          WPODFC_00
201300930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
201400931125     C                   Z-ADD     D98OFC        WR3HHI
201500930913     C                   EXSR      RO3ORE
201600930913     C                   ADD       WR3HHO        WPODFC
201700931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
201800930913     C*
201900930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
202000930913     C     D98ERR        IFEQ      '0'
202100930913     C     WPODFC        IFEQ      *ZEROS
202200930913     C     WPODIC        ANDNE     *ZEROS
202300930913     C                   MOVEL     '1'           D98ERR
202400930913     C                   MOVEL     MSG(17)       D98MSG
202500930913     C                   ENDIF
202600930913     C                   ENDIF
202700930907     C*
202800930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
202900930913     C     D98ERR        IFEQ      '0'
203000930913     C     WPODIC        IFGT      WPODFC
203100930907     C                   MOVEL     '1'           D98ERR
203200930913     C                   MOVEL     MSG(18)       D98MSG
203300930913     C                   ENDIF
203400930913     C                   ENDIF
203500950407     C*
203600950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
203700930511     C*
203800930511     C                   ENDSR
203900171108     C********************************************************************
204000171108     C*   CTR_FGV- controlla fogli viaggio se presente la linea di arrivo da borderizzare
204100171108     C********************************************************************
204200171108     C     CTR_FGV       BEGSR
204300171108
204400171108     c     kfgv02        setll     fnfgv02l
204500171108     c     kfgv02        reade     fnfgv02l
204600171108     c                   dow       not %eof(fnfgv02l)
204700171108     c     kfgw          chain     fnfgw01l
204800171108     c                   if        not %found(fnfgw01l)
204900171110     c                   clear                   fgwff3
205000171110     c                   clear                   fgwff4
205100171108     c                   endif
205200171108
205300171108     c* faccio finta che sia borderizzata la bolla
205400171124     c*  e che il tipo servizio sia "C"
205500171110     c                   movel     d98lna        alfalna           3
205600171110     c                   if        %lookup(alfalna:ffv)>0
205700171108     c                   eval      d98dfv=d98dsp
205800171124     c                   if        d98tsp='D'
205900171124     c                   eval      d98tsp='C'
206000171124     c                   eval      d98duc=d98dsp
206100171124     c                   endif
206200171124     c
206300171108     c                   leave
206400171108     c                   endif
206500171108     c
206600171108     c     kfgv02        reade     fnfgv02l
206700171108     c                   enddo
206800171108     c
206900171108     C                   ENDSR
207000950110     C********************************************************************
207100950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
207200950110     C********************************************************************
207300950110     C     CALDPP        BEGSR
207400950110     C*
207500950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
207600950110     C                   MOVEL     'P'           WTC                            *PARTENZA
207700950110     C                   MOVEL     'I'           WIC                            *INTERNA
207800960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
207900950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
208000950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
208100950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
208200950110     C*
208300950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
208400950110     C                   MOVEL     'P'           WTC                            *PARTENZA
208500950110     C                   MOVEL     'C'           WIC                            *CLIENTE
208600950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
208700170104     C* SE TEMPO PARTENZA =0 sommo comunque 1 ore per slittare dal
208800170104     c*  festivo se fosse data festiva la data sped e non bortderizzata
208900170104     c*  in questa data
209000170104     c                   if        d98tpc=0
209100170104     c                   z-add     1             WR4NOI
209200170104     c                   else
209300950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
209400170104     c                   endif
209500950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
209600950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
209700170104     c
209800170104     c                   if        d98tpc=0
209900170104     c                   sub       1             wpoppc
210000170104     c                   endif
210100960229     C*
210200960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
210300050922     c
210400050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
210500050922     c
210600050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
210700090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
210800050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
210900090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
211000050922%%%  C*  ritardo alla partenza.
211100050922     c* Il tutto solo se la data di oggi > della
211200050922     c*  data teorica di partenza determinata
211300050922     c*
211400950110%%%  C     D98ERR        IFEQ      '0'
211500950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
211600950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
211700950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
211800050922     c     wpoattData24  andgt     wpoppi
211900960911%%%  C********             MOVEL'OSC  '   WR3TAB
212000960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
212100960911%%%  C********             EXSR RO3ORE
212200960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
212300050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
212400090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
212500090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
212600950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
212700050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
212800050922     c* La nuova data calcolata non può essere < della minima prevista
212900050922     c                   if        wr0poo>wpoppi
213000050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
213100050922     c                   endif
213200050922     c
213300950110%%%  C                   ENDIF
213400950110%%%  C                   ENDIF
213500950110%%%  C*
213600950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
213700050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
213800050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
213900050915%%%  C*  alla prtenza.
214000950110%%%  C     D98ERR        IFEQ      '0'
214100950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
214200950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
214300950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
214400950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
214500950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
214600950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
214700050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
214800950110%%%  C                   ENDIF
214900950110%%%  C                   ENDIF
215000950110%%%  C*
215100950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
215200950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
215300950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
215400950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
215500950110%%%  C     D98ERR        IFEQ      '0'
215600950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
215700950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
215800950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
215900950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
216000090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
216100090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
216200950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
216300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
216400950110%%%  C                   ENDIF
216500950110%%%  C                   ENDIF
216600960229     C*
216700960229     C                   ENDIF                                                  <-- 1 ---
216800950110     C*
216900950110     C                   ENDSR
217000930604     C********************************************************************
217100930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
217200930604     C********************************************************************
217300930604     C     CALDPT        BEGSR
217400090902     c                   clear                   RicalcoloTRAZ     1
217500930604     C*
217600930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
217700931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
217800931129     C                   MOVEL     'I'           WIC                            *INTERNA
217900050915     c* se non presente la data partenza ultimo collo, uso la teorica
218000950110     C     D98DUC        IFEQ      *ZEROS
218100950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
218200950110     C                   ELSE
218300950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
218400950110     C                   ENDIF
218500930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
218600930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
218700930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
218800930604     C*
218900930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
219000931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
219100931129     C                   MOVEL     'C'           WIC                            *CLIENTE
219200950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
219300930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
219400930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
219500930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
219600150508
219700150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
219800150508     c* se presente la data partenza PRIMO  collo --> la uso
219900150508     c                   if        w98dpc=0
220000150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
220100150508     c                   else
220200150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
220300150508     C                   MOVEL     'C'           WIC                            *INTERNA
220400150508     C                   Z-ADD     W98DPC        WDAT
220500150508     C                   EXSR      DATORE
220600150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
220700150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
220800150508     C                   EXSR      RO4CDP
220900150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
221000150508     c                   endif
221100960229     C*
221200960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
221300940624%%%  C*
221400050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
221500050915
221600940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
221700050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
221800050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
221900050915     c*   attribuito alla trazione
222000940624%%%  C     D98ERR        IFEQ      '0'
222100940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
222200940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
222300090903     c     wpoatt        andge     wpopti
222400050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
222500090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
222600090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
222700940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
222800050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
222900050922     c                   if        wr0poo>wpopti
223000050922     c                   eval      wpopti=wr0poo
223100050922%%%  C                   endif
223200090902     c*
223300090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
223400090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
223500090902     c*  della trazione udate e utime
223600090902     c                   eval      RicalcoloTRAZ='S'
223700090902     c
223800940624%%%  C                   ENDIF
223900940624%%%  C                   ENDIF
224000940628%%%  C*
224100050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
224200050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
224300050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
224400940628%%%  C     D98ERR        IFEQ      '0'
224500940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
224600940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
224700940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
224800090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
224900090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
225000940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
225100050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
225200940628%%%  C                   ENDIF
225300940628%%%  C                   ENDIF
225400960229     C*
225500960229     C                   ENDIF                                                  <-- 1 ---
225600930604     C*
225700930604     C                   ENDSR
225800930511     C********************************************************************
225900930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
226000930511     C********************************************************************
226100930511     C     CALDCR        BEGSR
226200050914     c                   clear                   consideraora
226300100308     c
226400100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
226500100308     c*
226600100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
226700100308     c* 1) se presente solo ORA --> ignoro sempre
226800100308     c* 2) se presente data e ora IL --> ignoro l'ora
226900100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
227000100309     c***                if        d98tsp='E'
227100100309     c***                clear                   d98dcr
227200100309     c***                clear                   d98hcr
227300100309     c**+                clear                   d98tcr
227400100309     c***                endif
227500100308     c
227600100308     c                   if        d98hcr>0
227700100308     c
227800100308     c                   if        d98dcr=0
227900100308     c                   clear                   d98hcr
228000100308     c                   endif
228100100308
228200100308     c                   if        d98dcr>0 and d98tcr=' '
228300100308     c                   clear                   d98hcr
228400100308     c                   endif
228500100308     c                   endif
228600941228     C*
228700941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
228800941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
228900050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
229000050901     c*  (per uguale si intende entrambe nel mattino o entrambe
229100050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
229200050901     c*  (EDPES 1/9/05)
229300050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
229400050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
229500050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
229600050831     C***                Z-ADD     D98DCM        D98DCR
229700050831     C***                ENDIF
229800050901     c*
229900070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
230000050914     c
230100100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
230200100308     c***                clear                   d98hcr
230300100308     c***                clear                   d98tcr
230400100308     c***                endif
230500050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
230600050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
230700070521     c*  richiesta dal cliente
230800100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
230900100308     C***                MOVEL     'OSC  '       WR3TAB
231000100308     C***                Z-ADD     D98HMC        WR3HHI
231100100308     C***                EXSR      RO3ORE
231200100308     c***                z-add     wr3hho        salvaoreHMC
231300050901     c
231400100308     C***                Z-ADD     D98Hcr        WR3HHI
231500100308     C***                EXSR      RO3ORE
231600100308     c***                z-add     wr3hho        salvaoreHCR
231700100308     c***
231800050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
231900100308    2c***                if        salvaoreHMC=salvaoreHCR
232000100308     C***                eval      Consideraora='S'
232100100308     c***                else
232200100308     C***                eval      Consideraora=' '
232300100308     c***                clear                   d98hcr
232400100308    2c***                endif
232500100308    1c***                endif
232600930511     C*
232700930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
232800931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
232900931125     C                   Z-ADD     D98DCR        WDAT
233000930325     C                   EXSR      DATORE
233100930326     C                   Z-ADD     WORE          WPODCR
233200930420     C*
233300930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
233400930420     C*
233500931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
233600930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
233700930326     C                   MOVEL     'CRD  '       WR3TAB
233800931125     C                   Z-ADD     D98HCR        WR3HHI
233900930419     C                   EXSR      RO3ORE
234000930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
234100940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
234200050902     c
234300931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
234400930420     C                   MOVEL     'CRD1 '       WR3TAB
234500930420     C                   Z-ADD     *ZEROS        WR3HHI
234600930420     C                   EXSR      RO3ORE
234700931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
234800090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
234900090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
235000050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
235100060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
235200050907     c                   z-add     worecr        oreILoDOPO
235300931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
235400931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
235500931207     C                   MOVEL     'C'           WIC                            *CLIENTE
235600931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
235700150330     C                   Z-ADD     WORECR        WR4NOI
235800931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
235900931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
236000931209     C     WPOCRF        SUB       12            WR7POI            9 0
236100931209     C                   MOVEL     'X'           WIC
236200931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
236300931209     C                   ADD       12            WR7POO
236400931207     C     WR7POO        IFGT      WPOCRF
236500931207     C                   Z-ADD     WR7POO        WPOCRF
236600931207     C                   END
236700150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
236800150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
236900150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
237000150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
237100150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
237200150330     C                   MOVEL     'C'           WIC                            *CLIENTE
237300150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
237400150330     C                   Z-ADD     w99TCC        WR4NOI
237500150330     c                   sub       12            wr4noi
237600150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
237700150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
237800150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
237900150330     c
238000150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
238100150330     C                   MOVEL     'X'           WIC
238200150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
238300150330     C******             ADD       12            WR7POO
238400150330     C     WR7POO        IFGT      WPOCRF_dee
238500150330     C                   Z-ADD     WR7POO        WPOCRF_dee
238600150330     C                   END
238700050902     c
238800050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
238900050902     c*  sono le ore di fine della non consegnabilità
239000050905     c* Se aggiungendo 1 finisco in giorno festivo
239100050905     c*  cerco primo giorno non festivo
239200050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
239300050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
239400050907     C****               MOVEL     'C'           WIC                            *CLIENTE
239500050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
239600050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
239700050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
239800050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
239900050907     c
240000050907     c                   add       1             wpocri
240100050902     c
240200050902   FFC                   Z-ADD     WPOCRI        WPOCRC
240300050902     c
240400930325     C                   END
240500150330     C*
240600931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
240700930420     C* DATA SPED. = DATA INIZIO RANGE
240800930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
240900940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
241000940324£$   C                   Z-ADD     WPOCRI        WPOCRC
241100930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
241200930420     C                   MOVEL     'CRP  '       WR3TAB
241300931125     C                   Z-ADD     D98HCR        WR3HHI
241400930420     C                   EXSR      RO3ORE
241500931207     C     WPODCR        ADD       WR3HHO        WPOCRF
241600931207     C                   SUB       24            WPOCRF                         *FINE RANGE
241700931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
241800931207     C* ANDANDO INDIETRO NEL TEMPO
241900050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
242000050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
242100050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
242200050907     c* data utile venedì sera
242300130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
242400130411     C                   MOVEL     'X'           WIC                            *CLIENTE
242500931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
242600931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
242700931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
242800930326     C                   END
242900930526     C*
243000931125     C     D98TCR        IFEQ      'P'
243100931125     C     D98TCR        OREQ      'D'
243200930526     C                   ELSE                                                   *CONS.RICH.IL ..
243300940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
243400940324£$   C                   Z-ADD     WPODCR        WPOCRC
243500930526     C* DATA SPED. = DATA INIZIO RANGE
243600050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
243700050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
243800050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
243900930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
244000930526     C                   MOVEL     'CRI  '       WR3TAB
244100931125     C                   Z-ADD     D98HCR        WR3HHI
244200930526     C                   EXSR      RO3ORE
244300050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
244400050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
244500050907     c                   z-add     wr3hho        oreILoDOPO
244600930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
244700050902     c
244800050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
244900050902     c                   if        wr3MATPOM='P' and d98hcr>0
245000050902     c                   add       12            wpocri
245100100308     c* tolgo 12 ore dalle ore conseganbili
245200050907     c                   sub       12            oreILoDOPO
245300050902     c                   endif
245400050902     c
245500931209     C     WPOCRF        SUB       12            WR7POI            9 0
245600931209     C                   MOVEL     'X'           WIC
245700931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
245800931209     C                   ADD       12            WR7POO
245900931209     C     WR7POO        IFGT      WPOCRF
246000931209     C                   Z-ADD     WR7POO        WPOCRF
246100931209     C                   END
246200050905     c
246300050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
246400050905     c*  sono le ore di fine della non consegnabilità
246500050905     c* Se aggiungendo 1 finisco in giorno festivo
246600050905     c*  cerco primo giorno non festivo
246700050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
246800050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
246900050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
247000050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
247100050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
247200050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
247300050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
247400050907     c
247500050907     c                   add       1             wpocri
247600050905     c
247700050905   FFC                   Z-ADD     WPOCRI        WPOCRC
247800050905     c
247900930526     C                   END
248000931206     C*
248100931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
248200931206     C* LA IMPOSTO COME LA CONS.TEORICA
248300140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
248400140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
248500931206     C                   ELSE
248600931206     C                   Z-ADD     WPOCRF        WPOCR1
248700931206     C                   END
248800150608     c* ai fini del calcolo affidabilità
248900150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
249000150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
249100931206     C                   ELSE
249200931206     C                   Z-ADD     WPOCRF        WPOCR2
249300931206     C                   END
249400150608     c* ai fini della data prevista consegna cliente
249500150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
249600150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
249700150608     C                   ELSE
249800150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
249900150608     C                   END
250000150331     c
250100150331     c                   if        wpocrf_dee>0
250200150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
250300150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
250400150330     C                   END
250500150331     C                   END
250600150331     C*
250700150331     c                   if        wpocrf_dee=0
250800150608     C     wpocr2_dee    sub       24            WPOCRF_dee
250900150331     c                   endif
251000150331     c
251100930526     C                   END                                                    <-- 1 ---
251200930511     C*
251300930511     C                   ENDSR
251400930511     C********************************************************************
251500930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
251600930511     C********************************************************************
251700930511     C     CALLAV        BEGSR
251800930511     C*
251900941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
252000930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
252100930421     C*
252200930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
252300930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
252400930421     C     1             DO        IE            E                 3 0          --- 2 -->
252500930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
252600930421     C                   Z-ADD     1             I
252700930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
252800060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
252900930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
253000930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
253100930421     C                   ADD       1             IL
253200930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
253300930421     C                   EXSR      DATORE
253400930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
253500091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
253600091014     c* Mi salvo la posizione dell'evento
253700091014     C                   Z-ADD     E             LAp(IL)
253800930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
253900930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
254000930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
254100930421     C                   EXSR      RO3ORE
254200930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
254300930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
254400930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
254500930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
254600091015     c**                 if        §2agla>1
254700091015     c**                 sub       1             §2agla
254800930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
254900091015     c**                 else
255000091015     c**                 clear                   whh
255100091015     c**                 endif
255200930510     C                   ADD       WHH           WORELA
255300940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
255400930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
255500090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
255600150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
255700930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
255800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
255900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
256000930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
256100930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
256200930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
256300930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
256400931209     C                   Z-ADD     LAF(IL)       WR7POI
256500931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
256600931202     C     WR7POO        IFGT      LAF(IL)
256700931201     C                   Z-ADD     WR7POO        LAF(IL)
256800931202     C                   END
256900930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
257000930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
257100091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
257200930510     C                   END                                                    <-- 3 ---
257300931223     C                   ENDDO                                                  <-- 2 ---
257400930421     C*
257500930421     C                   END                                                    <-- 1 ---
257600930618££$$ C*
257700930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
257800931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
257900941228££$$ C     1             DO        IL            I
258000930618££$$ C     LAF(I)        IFGT      WPOLAF
258100930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
258200930618££$$ C                   END
258300931223££$$ C                   ENDDO
258400930511     C*
258500930511     C                   ENDSR
258600940324£$   C********************************************************************
258700940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
258800940324£$   C********************************************************************
258900940324£$   C     CALLAC        BEGSR
259000941229     C*
259100950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
259200950407     C*
259300950210     C                   Z-ADD     WORELA        WTM060
259400941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
259500941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
259600941229     C     D98DAG        IFEQ      *ZEROS
259700050520     c     §2aftg        andeq     'S'
259800090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
259900090827     C     WORELA        ADD       w99TCC        WTM060            6 0
260000160404     c                   if        wVP='S'
260100160404     c                   sub       24            wTM060
260200160404     c                   endif
260300941229     C                   ENDIF
260400941229     C*
260500940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
260600940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
260700941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
260800941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
260900941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
261000940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
261100941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
261200050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
261300050520     c                   movel     §2aftg        LCON(IL)
261400950407     C*
261500950407     C                   ENDIF
261600960129     C*
261700940324£$   C                   ENDSR
261800930511     C********************************************************************
261900930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
262000930511     C********************************************************************
262100930511     C     CALGIA        BEGSR
262200960129     C*
262300960129     C*-------------------------------------------------------------------
262400960129     C* EVENTI GIACENZA
262500960129     C*-------------------------------------------------------------------
262600960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
262700960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
262800960129     C*
262900050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
263000041116     c     d98nsp        ifgt      0
263100031125     C                   EXSR      CARGIAC
263200041116     c                   endif
263300031125     C*
263400960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
263500960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
263600960129     C     1             DO        IE            E                 3 0          --- 2 -->
263700960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
263800960129     C                   Z-ADD     1             I
263900960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
264000060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
264100960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
264200960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
264300960129     C                   ADD       1             IG
264400960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
264500960129     C                   EXSR      DATORE
264600960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
264700140606     c* Mi salvo la posizione dell'evento
264800140606     C                   Z-ADD     E             gip(IG)
264900960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
265000960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
265100960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
265200960129     C                   EXSR      RO3ORE
265300960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
265400960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
265500960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
265600960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
265700960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
265800960129     C                   ADD       WHH           WOREGI
265900960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
266000960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
266100090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
266200090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
266300160404     c                   if        wVP='S'
266400160404     c                   sub       24            wOREGI
266500160404     c                   endif
266600960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
266700960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
266800960129     C                   MOVEL     'C'           WIC                            *CLIENTE
266900960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
267000960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
267100960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
267200960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
267300960129     C                   Z-ADD     GIF(IG)       WR7POI
267400960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
267500031125     C     WR7POO        IFGT      GIF(IG)
267600031125     C                   Z-ADD     WR7POO        GIF(IG)
267700031125     C                   ENDIF
267800031201     C*
267900031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
268000031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
268100031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
268200031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
268300031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
268400031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
268500031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
268600031201     C                   IF        WPOATT > GIF(IG)
268700031201     C*
268800031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
268900031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
269000031201     C                   IF        WGGA = *BLANKS
269100031201     C                   SETOFF                                       54
269200031201     C                   ELSE
269300031201     C     WGGA          LOOKUP    SK7QW                                  54
269400031201     C                   ENDIF
269500031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
269600031201     C                   MOVEL     '1'           FLGGIACOK
269700031201     C                   ELSE
269800140127     c
269900031125     C*
270000031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
270100031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
270200031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
270300031125     C                   MOVEL     '0'           FLGGIACOK         1
270400031125     C*
270500031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
270600031125     C     1             DO        IAG           EG                3 0
270700031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
270800031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
270900031125     C                   EXSR      DATORE
271000031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
271100031125     C*
271200031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
271300031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
271400031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
271500031125     C                   EXSR      DATORE
271600031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
271700031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
271800031229     C* => NN LO CONSIDERO
271900031229     C                   IF        EVC(E+1) = 'G' AND
272000031229     C                             WPROXGII = WGCPDORE
272100031229     C                   MOVEL     '1'           FLGGIACOK
272200031229     C                   ENDIF
272300031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
272400031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
272500031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
272600031125     C                   EXSR      RO3ORE
272700031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
272800031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
272900031125     C* DELL'EVENTO GIACENZA CORRENTE
273000031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
273100060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
273200031125     C                   MOVEL     '1'           FLGGIACOK
273300031125     C                   LEAVE
273400031125     C                   ENDIF
273500031125     C                   ELSE
273600031229     C                   IF        EVC(E)  = 'G' AND
273700031229     C                             GII(IG) = WGCPDORE
273800031229     C                   MOVEL     '1'           FLGGIACOK
273900031229     C                   LEAVE
274000031229     C                   ENDIF
274100031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
274200031125     C                   MOVEL     '1'           FLGGIACOK
274300031125     C                   LEAVE
274400031125     C                   ENDIF
274500031125     C                   ENDIF
274600031125     C                   ENDDO
274700031201     C                   ENDIF
274800140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
274900140127     C                   IF        FLGGIACOK = '0'  and
275000140127     c                             d98dcm=evd(E)
275100140127     C                   MOVEL     '1'           FLGGIACOK
275200140127     c                   endif
275300031125     C*
275400031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
275500031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
275600031125     C* DELL'EVENTO GIACENZA CORRENTE
275700031125     C                   IF        FLGGIACOK = '0'
275800031125     C                   Z-ADD     *ZEROS        GIF(IG)
275900031125     C                   ELSE
276000031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
276100031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
276200031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
276300031125     C                   ENDIF
276400031125     C                   ELSE
276500031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
276600031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
276700031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
276800031125     C                   ENDIF
276900031125     C*
277000960129     C                   ENDIF                                                  <-- 3 ---
277100960129     C                   ENDDO                                                  <-- 2 ---
277200960129     C*
277300960129     C                   ENDIF                                                  <-- 1 ---
277400960129££$$ C*
277500960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
277600960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
277700960129££$$ C     1             DO        IG            I
277800960129££$$ C     GIF(I)        IFGT      WPOGIF
277900960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
278000960129££$$ C                   ENDIF
278100960129££$$ C                   ENDDO
278200941229     C*
278300941229     C*-------------------------------------------------------------------
278400941229     C* APERTURA GIACENZA
278500941229     C*-------------------------------------------------------------------
278600960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
278700960129     C*
278800960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
278900931125     C                   Z-ADD     D98DAG        WDAT
279000930325     C                   EXSR      DATORE
279100950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
279200960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
279300950413£££  C                   Z-ADD     WORE          WPODAG
279400960129$$£  C*********************ENDIF
279500930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
279600930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
279700930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
279800930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
279900930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
280000941229     C*
280100941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
280200941229     C                   Z-ADD     WPODAG        WPOAGC
280300960129     C*
280400960129     C                   ENDIF                                                  <-- 1 ---
280500941229     C*
280600941229     C*-------------------------------------------------------------------
280700930511     C* CHIUSURA GIACENZA
280800941229     C*-------------------------------------------------------------------
280900930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
281000930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
281100931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
281200931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
281300931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
281400931201     C                   Z-ADD     D98DCM        W98DCG
281500930416     C                   ELSE
281600930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
281700930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
281800931201     C                   Z-ADD     WDAODI        W98DCG
281900061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
282000061123     c*  chiusura giacenza, come già fa in altri posti
282100061123     c                   if        d98dfc>0
282200061123     C                   Z-ADD     d98dfc        W98DCG
282300061123     c                   endif
282400061123     c
282500930409     C                   END
282600930409     C                   END
282700930409     C                   END
282800930414     C* TRASF.DATA CHIUSURA
282900931201     C     W98DCG        IFNE      *ZEROS
283000931201     C                   Z-ADD     W98DCG        WDAT
283100930325     C                   EXSR      DATORE
283200930326     C                   Z-ADD     WORE          WPODCG
283300930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
283400930407     C* GIORNO DA ESCLUDERE
283500150216     C                   ADD       12            WPODCG
283600150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
283700150123     c*  il giorno di chiusura dovrebe già essere protetto
283800941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
283900941229     C     WPOCGC        IFEQ      *ZEROS
284000941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
284100941229     C                   ENDIF
284200930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
284300930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
284400931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
284500931129     C                   MOVEL     'C'           WIC                            *CLIENTE
284600150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
284700150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
284800160404     c                   if        wVP='S'
284900160404     c                   sub       24            wr4NOI
285000160404     c                   endif
285100930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
285200930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
285300930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
285400931209     C                   Z-ADD     WPODCG        WR7POI
285500931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
285600931202     C     WR7POO        IFGT      WPODCG
285700931201     C                   Z-ADD     WR7POO        WPODCG
285800930326     C                   END
285900931202     C                   END
286000930511     C*
286100930511     C                   ENDSR
286200031125     C********************************************************************
286300031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
286400031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
286500031125     C*             DEGLI EVENTI DI TIPO GIACENZA
286600031125     C********************************************************************
286700031125     C     CARGIAC       BEGSR
286800031125     C*
286900031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
287000031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
287100031125     C*
287200050405     C                   Z-ADD     D98AAS        KWGCPAAS
287300050405     C                   Z-ADD     D98LNP        KWGCPLNP
287400050405     C                   Z-ADD     D98NRS        KWGCPNRS
287500050405     C                   Z-ADD     D98NSP        KWGCPNSP
287600031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
287700050405     C     KEYGC_P       SETLL     TIGCP01L
287800050405     C                   IF        %equal(TIGCP01L)
287900050405     C     KEYGC_P       READE     TIGCP01L
288000050405     C                   DOW       not %eof(TIGCP01L)
288100031125     C                   ADD       1             IAG
288200031125     C                   EVAL      GCPD(IAG) = GCPDUR
288300050405     C     KEYGC_P       READE     TIGCP01L
288400031125     C                   ENDDO
288500031125     C                   ENDIF
288600031125     C                   ELSE
288700050405     C     KEYGC_P       SETLL     TIGCP21L
288800050405     C                   IF        %equal(TIGCP21L)
288900050405     C     KEYGC_P       READE     TIGCP21L
289000050405     C                   DOW       not %eof(TIGCP21L)
289100031125     C                   ADD       1             IAG
289200031125     C                   EVAL      GCPD(IAG) = GCPDUR
289300050405     C     KEYGC_P       READE     TIGCP21L
289400031125     C                   ENDDO
289500031125     C                   ENDIF
289600031125     C                   ENDIF
289700031125     C*
289800031125     C                   ENDSR
289900941229     C********************************************************************
290000960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
290100941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
290200941229     C********************************************************************
290300960129     C     CALGIC        BEGSR
290400941229     C*
290500960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
290600960129     C*
290700960129     C                   Z-ADD     WOREGI        WTM060
290800960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
290900960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
291000960129     C     D98DAG        IFEQ      *ZEROS
291100960129$$£  C     WOREGI        ADD       12            WTM060            6 0
291200960129     C                   ENDIF
291300960129     C*
291400960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
291500960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
291600960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
291700960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
291800960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
291900960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
292000960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
292100960129     C*
292200960129     C                   ENDIF
292300941229     C*
292400941229     C                   ENDSR
292500060418     C********************************************************************
292600060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
292700060418     c*            l'affidabilità
292800060418     C********************************************************************
292900060418     C     CALCEPA       BEGSR
293000060418     C*
293100060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
293200060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
293300060420     c                   clear                   numggcepa         3 0
293400060418     C*
293500060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
293600060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
293700060420     c                   eval      wgiroepa='0'
293800060418     c                   eval      wfine=' '
293900060418     c
294000060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
294100060420    2C     W             DOwle     IP                                           --- 2 -->
294200060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
294300060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
294400060418     c
294500060420    4c                   if        wgiroepa='0'
294600060420     c                   exsr      ElaInizCEPA
294700060418   x4c                   else
294800060420    5c                   if        pvd(W)>wdat
294900060418     c                   eval      wfine='S'
295000060418    5c                   endif
295100060418    4c                   endif
295200060418     c
295300060420    4c                   if        wfine=' ' and pvd(W)=wdat
295400060420     C*
295500060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
295600060420     c*  finale
295700060420     c                   add       1             numggcepa
295800060420     c
295900060420     C                   z-add     24            WORELA
296000060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
296100060418     c
296200060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
296300060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
296400060418     C                   MOVEL     'C'           WIC                            *CLIENTE
296500060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
296600060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
296700060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
296800060418     c
296900060418     c* Trasformo le ore in data e verifico se c'e' altro evento
297000060418     c*  in sta data che allunga ancora
297100060420     c                   z-add     wr4poo        wore
297200060418     c                   EXSR      OREDAT
297300060418    4c                   endif
297400060418     c
297500060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
297600060418    4c                   if        wfine='S'
297700060418     c                   EXSR      ELAFINECEPA
297800060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
297900060420     c*  inizio range
298000060420     c                   else
298100060420     c                   add       1             w
298200060418    4C                   ENDif                                                  <-- 3 ---
298300060420     c
298400060420     c                   else
298500060420     c                   add       1             w
298600060418    3C                   ENDif                                                  <-- 3 ---
298700060418    2C                   ENDDO                                                  <-- 2 ---
298800060418     c
298900060420     c                   if        wgiroepa='1'
299000060418     c                   EXSR      ELAFINECEPA
299100060418     c                   endif
299200060418     C*
299300060418    1C                   END                                                    <-- 1 ---
299400060418££$$ C*
299500060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
299600060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
299700060420££$$ C     1             DO        Ipa           w
299800060420££$$ C     paif(w)       IFGT      WPOpaif
299900060420££$$ C                   Z-ADD     paif(w)       WPOpaif
300000060418££$$ C                   END
300100060418££$$ C                   ENDDO
300200060418     C*
300300060418     C                   ENDSR
300400060420     C********************************************************************
300500060420     c* Elabora Inizio range eventi particolari che allungano
300600060420     C********************************************************************
300700060420     c     ElaInizCEPA   BEGSR
300800060420     C                   ADD       1             IPA
300900060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
301000060420     C                   EXSR      DATORE
301100060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
301200061017     c
301300060420     c                   eval      wgiroepa='1'
301400060420     c                   eval      wdat=pvd(w)
301500060420     C                   ENDSR
301600060418     C********************************************************************
301700060418     c* Elabora fine range eventi particolari che allungano
301800060418     C********************************************************************
301900060418     c     ElafineCEPA   BEGSR
302000060420     c     numggcepa     mult      24            worela
302100060420     c                   sub       12            worela
302200090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
302300090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
302400160404     c                   if        wVP='S'
302500160404     c                   sub       24            WORELA
302600160404     c                   endif
302700060418     c
302800060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
302900060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
303000060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
303100060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
303200060418     C                   Z-ADD     paif(IPA)     WR7POI
303300060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
303400060418    5C     WR7POO        IFGT      paif(IPA)
303500060418     C                   Z-ADD     WR7POO        paif(IPA)
303600060418    5C                   END
303700061017     c*Sommo 24 ore di inizio giornata
303800061017     c*            : non ho capito perchè ma ci vogliono
303900061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
304000090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
304100090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
304200160404     c                   if        wVP='S'
304300160404     c                   sub       24            paii(IPA)
304400160404     c                   endif
304500061017     c* Se tempi di consegna inferiori alle 24 ore , devo
304600061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
304700061017     c*  partire prima
304800061017     c****               if        d98tcc<24
304900061017     c****               sub       12            paii(ipa)
305000061017     c****               endif
305100060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
305200060418     c* Imposto nuovo giro
305300060420     c                   eval      wgiroepa='0'
305400060418     c                   eval      wfine=' '
305500060420     c                   clear                   numggcepa         3 0
305600060418     c                   ENDSR
305700930511     C********************************************************************
305800930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
305900930511     C********************************************************************
306000010822     C     CALASS        BEGSR
306100010821     C*
306200011211     C                   EXSR      REPMAXCSB
306300930511     C*
306400930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
306500010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
306600011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
306700930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
306800930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
306900930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
307000930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
307100930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
307200930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
307300930511     C                   ELSE                                                   <-- 4 -->
3074009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
307500930512     C                   END                                                    <-- 4 ---
307600930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
307700930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
307800930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
307900930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
308000930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
308100930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
308200930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
308300930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
308400930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
308500930413     C                   END                                                    <-- 2 ---
308600011211     C                   END                                                    <-- 1 ---
308700930406     C*
308800930325     C                   ENDSR
308900930608     C********************************************************************
309000930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
309100930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
3092009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
309300930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
309400930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
309500930608     C********************************************************************
309600930608     C     RDRASS        BEGSR
309700930608     C*
309800930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
3099009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
3100009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
3101009407151507 C*********************EXSR RO3ORE
3102009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
310300930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
3104009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
3105009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
3106009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
3107009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3108009407191907 C                   Z-ADD     WPOASI        WPOROU
3109009407191907 C                   EXSR      CHKFES
3110009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3111009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
3112009407191907 C                   ELSE                                                   *E' FERIALE
3113009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
3114009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3115009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
3116009407191907 C                   ENDIF
311700930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
311800930608     C                   MOVEL     'ASS1 '       WR3TAB
311900930608     C                   Z-ADD     *ZEROS        WR3HHI
312000930608     C                   EXSR      RO3ORE
312100930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
312200930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
312300931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
312400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
312500930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
312600930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
312700930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
312800930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
312900930608     C*
313000930608     C                   ENDSR
313100930511     C********************************************************************
313200930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
313300930511     C********************************************************************
313400930511     C     CALFED        BEGSR
313500150331     c                   clear                   fddata
313600150331     c                   clear                   WPOFDdata
313700930511     C*
313800051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
313900931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
314000051108     C     D98FFD        oreq      'M'                                          --- 1 -->
314100930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
314200930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
314300930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
314400930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
314500150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
314600150331     c                   if        §ar5dfd>*zeros
314700150331     c                   movel     §ar5dfd       fddata            8 0
314800150331     c                   endif
314900150331     c                   if        fddata>0 and ((d98dti>0 and
315000150331     c                             fddata>d98dti) or d98dti=0)
315100150331     C                   Z-ADD     fddata        WDAT
315200150331     C                   EXSR      DATORE
315300150331     C                   Z-ADD     WORE          WPOfddata
315400150331     c                   endif
315500150331     c
315600930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
315700930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
315800930511     C                   ELSE                                                   <-- 2 -->
3159009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
316000930512     C                   END                                                    <-- 2 ---
316100150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3162001504081507 C                   Z-ADD     WPOfddata     WPOFDI
316300150331     c                   endif
316400150331
316500930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
316600930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
316700930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
316800930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
316900930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
317000930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
317100930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
317200930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
317300930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
317400930511     C                   END                                                    <-- 1 ---
317500930511     C*
317600930511     C                   ENDSR
317700930608     C********************************************************************
317800930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
317900930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3180009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
318100930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
318200930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
318300930608     C********************************************************************
318400930608     C     RDRFED        BEGSR
318500930608     C*
318600930608     C* CALCOLO ORE DEL FERMO DEPOSITO
318700930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3188009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3189001503311907 C                   Z-ADD     WPOFDI        WPOROU
3190001503311907 C                   EXSR      CHKFES
3191001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3192001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3193001503311907 C                   ELSE                                                   *E' FERIALE
3194001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3195001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3196001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3197001503311907 C                   ENDIF
319800150331     c* Se presente la particolarità consegna con più giorni di fermo depo
319900150331     c*  uso questi
320000150331     c                   if        §7rggfd>*zeros
320100150424     c                   movel     §7rggfd       wggfd             2 0
320200150331     c     wggfd         mult      24            Wnore
320300150331     c                   add       wnore         worefd
320400150331     c                   else
320500930608     C                   MOVEL     'FED1 '       WR3TAB
320600930608     C                   Z-ADD     *ZEROS        WR3HHI
320700930608     C                   EXSR      RO3ORE
320800940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
320900150331     c                   endif
321000930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
321100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
321200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
321300930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
321400930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
321500930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
321600930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
321700930608     C*
321800930608     C                   ENDSR
321900931201     C********************************************************************
322000931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
322100931201     C********************************************************************
322200931201     C     CALDTE        BEGSR
322300140825     c
322400140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
322500140825     c*  che tiene conto solo dei festivi per restituire in output
322600140825     c*  la data teorica di consegna
322700140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
322800140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
322900140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
323000140825     C                   ELSE
323100140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
323200140825     C                   END
323300140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
323400141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
323500141106     c*  quindi passo un flag
323600141106     c                   eval      nosomma='N'
323700140825     C                   EXSR      RO4CDP
323800141106     c                   eval      nosomma=' '
323900140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
324000140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
324100140825     c*  di consegna
324200140825     c                   EXSR      TRAS_DEI
324300140825     c
324400140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
324500140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
324600140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
324700140825     c                             ='M' or %subst(d98gc2:2:1)='P')
324800140825     c                   exsr      Reper_tempi
324900140825     c                   endif
325000931201     C*
325100140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
325200140825     c*  che tiene conto di:
325300140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
325400150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
325500931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
325600931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
325700931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
325800931201     C                   ELSE
325900931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
326000931201     C                   END
326100090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
326200090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
326300931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
326400140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
326500931201     C*
326600150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
326700931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
326800931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
326900090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
327000931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
327100150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
327200150608     c
327300150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
327400150608     c*  che non tiene conto dei tempi del C/Ass
327500150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
327600150609     c*
327700150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
327800150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
327900150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
328000150630     c                   Z-ADD     WPODTI        WR6POI
328100150609     c                   else
328200150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
328300150609     c                   endif
328400150609     c
328500150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
328600150608     c                   eval      wdee='S'
328700150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
328800150608     c                   eval      wdee=' '
328900150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
329000141017
329100141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
329200150219     C**                 EVAL      WTC='C'
329300150219     C**                 EVAL      WIC='C'
329400150219     c**                 z-add     wpoattdata00  WPOROU
329500150219     c**                 z-add     wdaodi        wpodata
329600150219     c**                 exsr      chkfes
329700150219     c**                 if        wslitt='S'
329800150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
329900150219     c**                 z-add     24            wr6noi
330000150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
330100150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
330200150219     c**                 else
330300150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
330400150219     c**                 endif
330500141204     c* Ricalcolo WPOrifDatLav
330600141229     C**                 MOVEL     'OSC  '       WR3TAB
330700141229     C**                 Z-ADD     WHHODI        WR3HHI
330800141229     C**                 EXSR      RO3ORE
330900141229     c**
331000141229     C**                 EVAL      WTC='C'
331100141229     C**                 EVAL      WIC='C'
331200141229     c**                 z-add     wpoattdata00  WPOROU
331300141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
331400141229     c**                 z-add     wr3hho        wr6noi
331500141229     c**                 add       12            wr6noi
331600141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
331700141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
331800141229     c
331900141017     c
332000141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
332100141017     c*  ma sempre devo slittare i festivi
332200141017     C* CONSEGNABILITA'
332300150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
332400150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
332500150219     C**                 EVAL      WTC='C'
332600150219     C**                 EVAL      WIC='C'
332700150219     c**                 z-add     d98dfc        WPOdata
332800150219     c**                 exsr      chkfes
332900150219     c**                 if        wslitt='S'
333000150219     c**                 z-add     wpodfc_00     Wr6poi
333100150219     c**                 z-add     24            wr6noi
333200150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
333300150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
333400150219     c**                 else
333500150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
333600150219     c**                 endif
333700150219     c**                 endif
333800931201     C*
333900931201     C                   ENDSR
334000930511     C********************************************************************
334100930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
334200930511     C********************************************************************
334300930511     C     CALDPC        BEGSR
334400930325     C*
334500930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
334600930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
334700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
334800930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
334900930325     C                   ELSE
335000930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
335100930407     C                   END
335200090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
335300090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
335400930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
335500090902     c
335600931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
335700090901
335800930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
335900930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
336000930423     C* A QUELLA DI RICHIESTA CONSEGNA,
336100930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
336200931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
336300931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
336400931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
336500931206     C                   END
336600930423     C                   END
336700930325     C*
336800930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
336900930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
337000930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
337100090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
337200090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
337300930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
337400931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
337500930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
337600930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
337700930423     C* A QUELLA DI RICHIESTA CONSEGNA,
337800930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
337900931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
338000931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
338100931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
338200931206     C                   END
338300930423     C                   END
338400140822     C*
338500930324     C                   ENDSR
338600150113     C********************************************************************
338700150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
338800150113     C********************************************************************
338900150113     c     Cal_DEE       BEGSR
339000150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
339100150508     c                   clear                   wtempi00C         1
339200151007     c                   clear                   wtempi12C         1
339300150722     c                   clear                   woreOLP
339400151009     c                   clear                   wdiff
339500150219     c
339600150219     c* 1) Se presente l'arrivo della trazione lo uso
339700150219    1c                   select
339800150219     c                   when      wmancatraz=' '  and d98dti>0
339900150630     c                   Z-ADD     WPODTI        WR1POI
340000150219     c
340100150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
340200150219    1c                   when      IDdata>0
340300150325     c                   exsr      CalcoloIMP
340400151008     c* Passo in output il terminal di disguido
340500151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
340600151008     c                   eval      d98flp=%editc(LTP(2):'X')
340700151008     c                   endif
340800151008
340900150528     c* stato IMP/disguido solo se >  alla data di oggi
341000151006     c                   if        wr1poi > wpoattdata00 and
341100151006     c                             (wr1poi-wpoattdata00)>12
341200150427     c                   eval      d98spcdee='IMP'
341300151008     c                   endif
341400151006     c                   if        wr1poi > wpoattdata00 and
341500151006     c                             (wr1poi-wpoattdata00)=12
341600151006     c                             and w991In>12
341700151006     c                   eval      d98spcdee='IMP'
341800151008     c                   endif
341900150219     c
342000150219     c                   other
342100150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
342200150225     c* devo far finta che sia arrivato.
342300150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
342400150508     c*  che tiene conto della data partenza PRIMO  collo
342500150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
342600150508     c*
342700150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
342800150508     c                   if        wr1poi >=wpoattdata00
342900150508     c                              and WPOPTC_DEE > WPOPTC
343000150508     c                   eval      d98spcdee='IMP'
343100150508     c                   endif
343200150219     c                   endsl
343300150113     c
343400150219     c* se il calcolo è < di oggi --> imposto oggi
343500150113     c                   if        wr1poi < wpoattdata00
343600150113     c                   eval      wr1poi= wpoattdata00
343700150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
343800150508     c                   eval      wTempi00C='S'
343900151009     c                   clear                   wdiff
344000150113     c                   endif
344100170208     c
344200170208     c* Calcolo la data di arrivo reale/teorica dei colli da passare in outout
344300170208     C                   Z-ADD     wr1poi        WORE
344400170208     C                   EXSR      OREDAT
344500170208     C                   Z-ADD     WDAT          D98dam_RTC
344600150217     c
344700150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
344800150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
344900150630     c                   if        wr1poi>wpoattdata00
345000150630     c                   eval      wdiff=wr1poi-wpoattdata00
345100150630     c                   endif
345200151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
345300151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
345400151006     c                   eval      wTempi12C='S'
345500151006     c                   endif
345600160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
345700151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
345800151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
345900160404     c                             and idfgs>0
346000151009     C     wdiff         DIV       24            WDIV
346100151009     C                   MVR                     WRES
346200151009     C                   IF        wres>0
346300151009     c                   eval      wTempi12C='S'
346400151009     c                   else
346500151009     c                   eval      wTempi00C='S'
346600151009     c                   endif
346700151009     c                   endif
346800151009     c
346900151006     c
347000151006     c                   select
347100151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
347200151006     c                             w991in>12 and
347300151006     c                             (deeindist<>'S' or d98cons<>'S'
347400151006     c                             or d98ddc>wdaodi)
347500151006     c
347600151006     c* Se stato IMP lo mantengo per avere messaggio diverso
347700151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
347800151006     c                   if        d98spcdee<>'IMP'
347900151006     c                   eval      d98spcdee='PRV'
348000151006     c                   endif
348100151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
348200150330     c                             (deeindist<>'S' or d98cons<>'S'
348300150330     c                             or d98ddc>wdaodi)
348400150427     c
348500150427     c* Se stato IMP lo mantengo per avere messaggio diverso
348600150427     c                   if        d98spcdee<>'IMP'
348700150304     c                   eval      d98spcdee='PRV'
348800150427     c                   endif
348900150427     c
349000151006   x1c                   other
349100150330     c                   exsr      CAL_OLP
349200150217     c
349300150528     c* Se passato l'orario oppure data distinta < data di oggi
349400150508     c* 1) spedizione in distinta          --> in consegna
349500150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
349600150528     c                             d98ddc<wdaodi)
349700150508    3c                   IF        deeindist='S' and d98cons='S'
349800150508     c                   eval      d98spcdee= 'DDC'
349900150508
350000150508   x3c                   else
350100150507     c
350200150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
350300150722     c*  entro sera
350400150630     c                   if        wr1poi>wpoattdata00
350500150722     c                   z-add     12            woreOLP           3 0
350600150722     c***                add       12            wr1poi
350700150630    4c                   else
350800150507    4c                   if        wr1poi<=wpoattdata00
350900150722     c                   add       24            woreOLP
351000150722     c***                add       24            wr1poi
351100150507    4c                   endif
351200150630    4c                   endif
351300150507     c
351400150722     c****               eval      d98spcdee='OLP'
351500150507     c* memorizzo orario limite picking
351600150507     c                   eval      d98picklim=%editc(picklim:'X')
351700150507    3c                   endif
351800150507    2c                   endif
351900150507     c
352000151006    1c                   endsl
352100150217     c
352200150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
352300150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
352400150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
352500150113     C                   Z-ADD     Wr1poi        W10POI            9 0
352600150113     C                   EXSR      R10FES
352700150113     C                   Z-ADD     W10POO        wr1poi
352800150113     C
352900151006     c                   select
353000151006    1c                   when      wtempi12C='S'
353100151006     c                   z-add     W991IN        wr1noi
353200151006     c                   if        woreOLP>0
353300151006     c                   add       woreOLP       wr1noi
353400151006     c                   eval      d98spcdee='OLP'
353500160404     c                   else
353600160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
353700160404     c**                 if        wVP='S'
353800160404     c**                 add       24            wr1noi
353900160404     c**                 endif
354000151006     c                   endif
354100160404
354200151006    1c                   when      wtempi00C='S'
354300150630     c***                z-add     W990CN        wr1noi
354400150630     c                   z-add     W990IN        wr1noi
354500150722     c
354600150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
354700150722     c*  quindi sottraggo 24
354800151009     c                   if        wr1noi>24  and wdiff=0
354900150722     c                   sub       24            wr1noi
355000150722     c                   endif
355100150722     c
355200150722     c* se passato orario limite picking, slitto in avanti
355300150722     c                   if        woreOLP>0
355400150722     c                   add       woreOLP       wr1noi
355500150722     c                   eval      d98spcdee='OLP'
355600160404     c                   else
355700160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
355800160404     c**                 if        wVP='S'
355900160404     c**                 add       24            wr1noi
356000160404     c**                 endif
356100150722     c                   endif
356200150722     c
356300151006   x1c                   other
356400150722     c
356500150714     c                   if        wmancatraz=' '  and d98dti>0
356600150630     c                   z-add     w99tci        wr1noi
356700150714     c                   else
356800150714     c                   z-add     w99tcc        wr1noi
356900150714     c                   endif
357000160404
357100160401     c
357200150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
357300150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
357400150722     c                   if        woreOLP=24 and wr1noi<=24
357500150722     c                   add       woreOLP       wr1noi
357600150722     c                   eval      d98spcdee='OLP'
357700150722     c                   endif
357800150722     c                   if        woreOLP=12 and wr1noi<24
357900150722     c                   add       woreOLP       wr1noi
358000150722     c                   eval      d98spcdee='OLP'
358100150722     c                   endif
358200150722     c
358300151006    1c                   endsl
358400150629     c
358500150722    1c**                 if        WR1NOI=10
358600150722     c**                 z-add     12            wr1noi
358700150722     c**                 endif
358800150722     c
358900150722     c     wr1noi        div       12            wdiv
359000150722     c                   mvr                     wres
359100150722     c                   if        wres >0
359200150722     c                   add       2             wr1noi
359300150722     c                   endif
359400150722     c
359500150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
359600150629    1c******             endif
359700150508     c
359800150605     c                   eval      wdee='S'
359900150113     C                   EXSR      RO1CDP
360000150605     c                   eval      wdee=' '
360100150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
360200150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
360300150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
360400150113     C* A QUELLA DI RICHIESTA CONSEGNA,
360500150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
360600150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
360700150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
360800150331     c*  2) IL        -> è il giorno indicato
360900150331     C     D98DCR        IFNE      *ZEROS
361000150331     C     D98tcr        andne     'P'
361100150331     C***  WPOdpcee      IFLE      WPOCR2
361200150331     C***  WPOCR2        SUB       12            WPOdpcee
361300150330     c
361400150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
361500150330     c*   dare la prima data utile di consegna
361600150330     C     WPOdpcee      IFLE      WPOCRF_dee
361700150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
361800150330     C                   z-add     WPOCRF_dee    WPOdpcee
361900150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
362000150409     c                   if        wpodpcee>wpoattdata00
362100150409     c                   eval      d98spcdee='PRV'
362200150409     c                   endif
362300150113     C                   END
362400150331     c
362500150220     c
362600150113     C                   END
362700150220     c
362800150113     c                   ENDSR
362900150330     C********************************************************************
363000150330     C*   Calcolo orario da tabella OLP
363100150330     C********************************************************************
363200150330     c     Cal_OLP       BEGSR
363300150330     c                   eval      picklim=0900
363400150330     c* Dopo l'ora limite picking, passo al giorno dopo
363500150330     C                   MOVEL(P)  'OLP'         TBECOD
363600150330     C                   MOVEL(P)  d98lna        TBEKE1
363700150330     C                   MOVEL(P)  wdaodi        TBEKE2
363800150330     C     KTBE          CHAIN     TNTBE01L
363900150330     C                   IF        not %found(TNTBE01L)
364000150330     c                   clear                   tbeke2
364100150330     C     KTBE          CHAIN     TNTBE01L
364200150330     c                   endif
364300150330     C                   IF        not %found(TNTBE01L)
364400150330     C                   MOVEL(P)  '000'         TBEKE1
364500150330     C     KTBE          CHAIN     TNTBE01L
364600150330     c                   endif
364700150330     c
364800150330     C                   IF        %found(TNTBE01L) and
364900150330     c                             %subst(tbeuni:1:4)>*zeros
365000150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
365100150330     c                   endif
365200150330     c                   ENDSR
365300150325     C********************************************************************
365400150325     C*   Riclcolo data se colli in IMP/disguido
365500150325     C********************************************************************
365600150325     c     CalcoloIMP    BEGSR
365700150325     C                   Z-ADD     IDdata        WDAT
365800150325     C                   EXSR      DATORE
365900150325     C                   Z-ADD     WORE          WR4POI
366000150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
366100151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
366200150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
366300150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
366400150325     c* calcolo il terminal di partenza
366500150325     c                   clear                   fnlv55ds
366600150325     c                   eval      d55tla='L'
366700150325     c                   eval      d55tpt='P'
366800150325     c                   eval      d55lin=IDfgs
366900150325     c                   eval      d55drf=IDdata
367000150325     c                   call      'FNLV55R'
367100150325     c                   parm                    fnlv55ds
367200150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
367300150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
367400150325     c                   EXSR      RTTRAZ
367500150325    3c                   if        d98err<>'1'
367600150325     C                   Z-ADD     CASTcn        wr4noi
367700150325     c                   else
367800150325     C                   Z-ADD     d98ttc        wr4noi
367900150325    3C                   endif
368000150325
368100150325   x2c                   else
368200150325     C                   Z-ADD     d98ttc        wr4noi
368300150325    2C                   endif
368400150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
368500150325     c*  perchè partirà un giorno dopo
368600150325    3c                   if        IDnpg='P'
368700150325     c                   add       24            WR4noi
368800150325    3c                   endif
368900150325     c
369000150325     c                   exsr      ro4cdp
369100150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
369200150325     c                   ENDSR
369300930604     C********************************************************************
369400930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
369500930604     C********************************************************************
369600930604     C     TPODAT        BEGSR
369700960229     C*
369800960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
369900960229     C*
370000960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
370100960229     C     WPOPTI        IFEQ      *ZEROS
370200960229     C                   Z-ADD     *ZEROS        D98DTI
370300960229     C                   Z-ADD     *ZEROS        D98HTI
370400960229     C                   ELSE
370500960229     C                   Z-ADD     WPOPTI        WORE
370600960229     C                   EXSR      OREDAT
370700960229     C                   Z-ADD     WDAT          D98DTI
370800960229     C     WRES          MULT      100           D98HTI
370900960229     C                   END
371000960229     C*
371100960229     C                   ELSE                                                   <-- 1 -->
371200930604     C*
371300931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
371400140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
371500140825     c*******            exsr      Tras_DEI
371600140825     c
371700930604     C*
371800150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
371900140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
372000140820     c* è data da :
372100140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
372200140820     c*  Tenere  conto, ovvero slittare in avanti, per :
372300140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
372400140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
372500140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
372600140911     c*
372700140820     c*  Da azzerare nei seguenti casi:
372800150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
372900150324     c* 4) bolla export    7) Cod manca consegna
373000140911     c* 8) bolla con DCR DOPOil o PRIMAdel
373100150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
373200150119     c*10) se completamente non arrivata e collo in IMP
373300150119     c
373400140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
373500150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
373600140925     C*  - FD   - export -
373700150217     c                   movel     'S'           DEEinDIST         1
373800150113     c                   eval      wcaldpcee='S'
373900150304     c                   eval      D98SPCDEE='PRV'
374000150324     c                   clear                   avvcmc            1
374100150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
374200150323     c                   if        d98cmc<>*blanks
374300150324     C                   Z-ADD     1             II
374400150326     C     d98cmc        LOOKUP    C2A(II)                                99
374500150324     C                   MOVEL     D2A(II)       usa2A
374600170915     C                   MOVEL     D2Z(II)       usa2Z
374700150324    4C     §2AFTC        IFEQ      'A'
374800150326     c                   eval      avvcmc='A'
374900170915     c* se non è prevista telefonata significa che è un "rimanda svincolo"
375000170915     c                   if        §2ztel=' '
375100170915     c                   eval      avvcmc='R'
375200170915     c                   endif
375300150323     c                   endif
375400150324    4C     §2AFTC        IFEQ      'G'
375500150326     c                   eval      avvcmc='G'
375600150324     c                   endif
375700150324     c                   endif
375800140820     c
375900140911     c                   z-add     d98lna        EX
376000150113     c*  la azzero nei casi previsti                 0
376100150601    1c                   select
376200150119     c* 1) Se la bolla e consegnata
376300140925     c                   when      d98dcm>0
376400150113     c                   clear                   wcaldpcee
376500150216     c                   eval      d98spcdee='CON'
376600150217     c                   movel     ' '           DEEinDIST         1
376700150323     c* 2) Se pratica di danno
376800150603     c                   when      flgdan='S'
376900150323     c                   clear                   wcaldpcee
377000150323     c                   eval      d98spcdee='CA '
377100150323     c                   eval      deeindist='S'
377200150324     c* 3) Se bloccata/rimpallo
377300150114     c                   when      wd98fbc='B'
377400150113     c                   clear                   wcaldpcee
377500150216     c                   eval      d98spcdee='BLO'
377600150217     c                   movel     ' '           DEEinDIST         1
377700150324     c* 4) Se bolla export
377800170406     c                   when      ex>0 and fl1(EX)='E'
377900150324     c                   clear                   wcaldpcee
378000150324     c                   movel     ' '           DEEinDIST         1
378100150324     c                   eval      d98spcdee='EXP'
378200150324     c* 5) Se giacente  o in attesa di apertura giacenza
378300150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
378400150324     c                   clear                   wcaldpcee
378500150324     c                   eval      d98spcdee='GIA'
378600150324     c                   eval      deeindist='S'
378700170915     c* 6a) Se codice manca consegna "A" e prevista telefonata
378800170915     c*     singifica che sono al secondo tentativo di cons.
378900150326     c                   when      d98cmc<>*blanks and avvcmc='A'
379000150324     c                   clear                   wpodpcee
379100150324     c                   clear                   wcaldpcee
379200150324     c                   movel     ' '           DEEinDIST         1
379300150324     c                   eval      d98spcdee='ATT'
379400170915     c* 6b) Se codice manca consegna "A"  e non prevista telefonata,
379500170915     c*     singifica che è un "rimanda svincolo"
379600170915     c                   when      d98cmc<>*blanks and avvcmc='R'
379700170915     c                             and d98dcr>0
379800170915     c                   clear                   wpodpcee
379900170915     c                   clear                   wcaldpcee
380000170915     c                   movel     ' '           DEEinDIST         1
380100170915     c                   eval      d98spcdee='AT1'
380200170915     c* imposto la data consegna richeista . Se < di oggi
380300170915     c* imposto oggi
380400170915     C     wpoattdata00  IFLE      WPOCRF_dee
380500170915     C                   z-add     WPOCRF_dee    WPOdpcee
380600170915     c                   else
380700170915     C                   z-add     wpoattdata00  WPOdpcee
380800170915     c                   endif
380900170915     c* 6c) Se codice manca consegna "A"  e non prevista telefonata,
381000170915     c*     singifica che è un "rimanda svincolo" ma non c'è cons richiesta
381100170915     c*     (non dovrebbe succedere): metto stato generico
381200170915     c                   when      d98cmc<>*blanks and avvcmc='R'
381300170915     c                             and d98dcr=0
381400170915     c                   clear                   wpodpcee
381500170915     c                   clear                   wcaldpcee
381600170915     c                   movel     ' '           DEEinDIST         1
381700170915     c                   eval      d98spcdee='SOS'
381800150410     c* 7) Bolla in sospensione consegna
381900150410     c                   when      §B4LBGE='S'
382000150410     c                   clear                   wpodpcee
382100150410     c                   clear                   wcaldpcee
382200150410     c                   movel     ' '           DEEinDIST         1
382300150410     c                   eval      d98spcdee='SOS'
382400150616     c
382500150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
382600161031     c                   when      wrbl='R'  or d98cbo='M '
382700150616     c                   clear                   wcaldpcee
382800150616     c                   eval      d98spcdee='   '
382900150616     c                   eval      deeindist='S'
383000150601   x1c                   other
383100150325     c*
383200150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
383300150331     c                   if        d98ffd='S'
383400150601    2c                   select
383500150403     c* FD escluso dai contatti
383600150601    2c                   when      §B4LTFD= 'N'
383700150403     c                   eval      d98spcdee='FD3'
383800150403     c                   clear                   wcaldpcee
383900150403     c                   movel     ' '           DEEinDIST         1
384000150403
384100150403     c* FD non contattato ( o da ricontattare)
384200170821    2c                   when      §B4LTFD<>' '  and  (olv82sts<>'0'  and
384300170821     c                             olv82sts<>'1' or oLRJ6FALL=' ')
384400150323     c                   clear                   wcaldpcee
384500150323     c                   movel     ' '           DEEinDIST         1
384600150323     c                   eval      d98spcdee='FFD'
384700150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
384800150424     c                   eval      wpodpcee=wpoffi
384900150403
385000150403     c* FD contattato e     IN distinta
385100150601    2c                   when      d98cons='S'
385200150401     c                   eval      d98spcdee='FD2'
385300150401     c                   clear                   wcaldpcee
385400150401     c                   movel     ' '           DEEinDIST         1
385500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
385600150424     c                   eval      wpodpcee=wpoffi
385700150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
385800150427     c*  ritirare la merce
385900150427     C                   MOVEL     'C'           WTC                            *PARTENZA
386000150427     C                   MOVEL     'C'           WIC
386100150427     C                   Z-ADD     D98ddc        WDAT
386200150427     C                   EXSR      DATORE
386300150427     c                   z-add     wore          wr4poi
386400150427     c                   z-add     24            wr4noi
386500150427     C                   EXSR      RO4CDP
386600150427     c                   z-add     wr4poo        wore
386700150427     c                   exsr      OREDAT
386800150427     c                   movel     wdat          D98DTCTD
386900150601   x2c                   other
387000150403     c* FD contattato e NON in distinta
387100150331     c                   eval      d98spcdee='FD1'
387200150408     c                   eval      wpodpcee=wpoffi
387300150331     c                   clear                   wcaldpcee
387400150331     c                   movel     ' '           DEEinDIST         1
387500150601    2c                   endsl
387600150331     c
387700150331     c                   endif
387800150403     c
387900150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
388000150601    2c                   if        (d98tc1='A' or d98tc2='A' or
388100150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
388200150601     c                             and d98ffd= ' '
388300150325     c                   clear                   wcaldpcee
388400150325     c                   movel     ' '           DEEinDIST         1
388500150601    3c                   if        §B4LBAS= 'N'
388600150427     c                   eval      d98spcdee='AP3'
388700150403     c                   else
388800150403     c                   eval      d98spcdee='APP'
388900150601    3c                   endif
389000150601    2c                   endif
389100150325     c*   se la bolla ha il C/Assegno verifico se da contattare
389200150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
389300150325     c                   clear                   wcaldpcee
389400150325     c                   movel     ' '           DEEinDIST         1
389500170208    3c                   if        §B4LTCA= 'N'
389600150427     c                   eval      d98spcdee='CA3'
389700150403     c                   else
389800150403     c                   eval      d98spcdee='CAS'
389900150601    3c                   endif
390000150601    2c                   endif
390100150325     c* Per questi 3 casi devo impostare la data contatto destinatario
390200150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
390300150325     c                             d98spcdee='CAS'
390400170208     c
390500170208     c                   exsr      Caldam_rtc
390600170208     c                   endif
390700170208    1c                   endsl
390800140820     c
390900150113     c* Se la bolla non è in distinta calcolo la data se previsto
391000150220     c                   if        wcaldpcee='S' and
391100150220     c                             (d98cons<>'S'  or (d98cons='S' and
391200150220     c                             evemic<>'SI'))
391300150219     c
391400150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
391500150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
391600150219     c                   if        d98nsp>0 and wtrasm<>'D' and
391700150219     c                             d98dam=0
391800150219     c                   exsr      letBRV
391900150219     c                   endif
392000150219     c
392100150217     c                   eval      d98spcdee='PRG'
392200150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
392300150320     c                             d98tc1='S' or d98tc2='S')
392400150320     c                   eval      d98spcdee='APP'
392500150320     c                   endif
392600150320     c
392700150113     c                   exsr      cal_DEE
392800150113     c                   endif
392900150113     c
393000140820     C     WPOdpcee      IFEQ      *ZEROS
393100931202     C                   Z-ADD     *ZEROS        D98DEE
393200931202     C                   Z-ADD     *ZEROS        D98OEE
393300930604     C                   ELSE
393400140820     C                   Z-ADD     WPOdpcee      WORE
393500141021     C******             SUB       12            WORE
393600930604     C                   EXSR      OREDAT
393700931202     C                   Z-ADD     WDAT          D98DEE
393800090903     c                   if        d98tcc<>10
393900090903     C                   ADD       12            WRES
394000090903     c                   else
394100090903     C                   ADD       10            WRES
394200090903     c                   endif
394300090903
394400931202     C     WRES          MULT      100           D98OEE
394500090903     c
394600090903     c                   if        d98oee=1000
394700090903     c                   eval      d98oee=1030
394800090903     c                   endif
394900140925     C                   END
395000150311     c
395100150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
395200150311     c                   select
395300150311     c                   when      d98spcdee='OLP'
395400140820     c
395500150220     c* Se bolla in distinta --> data prevista = data distinta
395600150311     c                   when      deeindist='S'  and
395700150113     c                             d98cons='S'
395800150220
395900150225     c*  non la propongo senza MIC e se presente disguido o IMP
396000150220     c                   if        d98dee>d98ddc and evemic<>'SI'
396100150220     c                             and IDdata>0
396200150220     c                   else
396300150220
396400140820     c                   eval      d98dee=d98ddc
396500140925     c                   clear                   d98oee
396600150320     c* se la data è > di oggi stato PRV se non è in IMP
396700150320     c                   if        d98dee>wdaodi
396800150320     c                   if        d98spcdee<>'IMP'
396900150311     c                   eval      d98spcdee='PRV'
397000150320     c                   endif
397100150320     c
397200150225     c                   else
397300150217     c* se c'è il MIC
397400150217     c                   if        evemic='SI'
397500150217     c                   eval      d98spcdee='DDC'
397600150225     c                   endif
397700150217     c                   endif
397800150220     c                   endif
397900150217     c
398000150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
398100150722     c                   eval      d98spcdee='PRV'
398200150311     c                   endsl
398300170208     c
398400170208     c* se non ho ancora calcolato la data arrivo reale o teorica tempi cliente, lo faccio ora
398500170208     c                   if        d98dam_rtc=0
398600170208     c                   eval      wsolodam='S'
398700170208     c                   exsr      Caldam_rtc
398800170208     c                   eval      wsolodam=' '
398900170208     c                   endif
399000931202     C*
399100931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
399200931202     C     WPODCI        IFEQ      *ZEROS
399300931202     C                   Z-ADD     *ZEROS        D98DCI
399400931202     C                   Z-ADD     *ZEROS        D98HCI
399500931202     C                   ELSE
399600931202     C                   Z-ADD     WPODCI        WORE
399700931202     C                   EXSR      OREDAT
399800931202     C                   Z-ADD     WDAT          D98DCI
399900090902     c                   if        d98tci<>10
400000931202     C                   ADD       12            WRES
400100090902     c                   else
400200090902     C                   ADD       10            WRES
400300090902     c                   endif
400400090902
400500931202     C     WRES          MULT      100           D98HCI
400600090901     c
400700090903     c                   if        d98hci=1000
400800090901     c                   eval      d98hci=1030
400900090901     c                   endif
401000090901     c
401100931202     C                   END
401200931202     C*
401300931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
401400931202     C     WPODCE        IFEQ      *ZEROS
401500931202     C                   Z-ADD     *ZEROS        D98DCE
401600931202     C                   Z-ADD     *ZEROS        D98HCE
401700931202     C                   ELSE
401800931202     C                   Z-ADD     WPODCE        WORE
401900931202//// C**************       SUB  12        WORE
402000931202     C                   EXSR      OREDAT
402100931202     C                   Z-ADD     WDAT          D98DCE
402200090902     c                   if        d98tcc >10
402300931202     C                   ADD       12            WRES
402400090902     c                   else
402500090902     C                   ADD       10            WRES
402600090902     c                   endif
402700090902     c
402800931202     C     WRES          MULT      100           D98HCE
402900090902     c
403000090903     c                   if        d98hce=1000
403100090902     c                   eval      d98hce=1030
403200090902     c                   endif
403300090902     c
403400931202     C                   END
403500960229     C*
403600960229     C                   ENDIF                                                  <-- 1 ---
403700930604     C*
403800930604     C                   ENDSR
403900170208     C********************************************************************
404000170208     C* Calcolo data di arrivo teorica o reale cliente
404100170208     C********************************************************************
404200170208     c     Caldam_rtc    BEGSR
404300170208     c
404400170208    2c                   select
404500170220    2c                   when      d98dti>0 and wmancatraz= ' ' and wpodti>0
404600170220     c
404700170220     c* questa specifica non serve: se è piena la data è pieno wpodti e viceversa
404800170220     c*****              movel     d98dti        d98dtctd
404900170208     c* se piene le ore già portate ad un giorno lavorativo
405000170220     c*******            if        wpodti>0
405100170208     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
405200170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
405300170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
405400170208     C                   Z-ADD     WPODTI        W10POI            9 0
405500170208     C                   EXSR      R10FES
405600170208     c                   z-add     w10poo        wore
405700170208     C                   EXSR      OREDAT
405800170208     c                   if        wsolodam<>'S'
405900170208     C                   movel     WDAT          D98dtctd
406000170208     c                   endif
406100170208     C                   movel     WDAT          D98dam_rtc
406200170220     c*****              endif
406300170208     c
406400170208    2c                   when      d98dam>0
406500170208     C                   Z-ADD     D98dam        WDAT
406600170208     C                   EXSR      DATORE
406700170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
406800170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
406900170208     C                   Z-ADD     Wore          W10POI            9 0
407000170208     C                   EXSR      R10FES
407100170208     c                   z-add     w10poo        wore
407200170208     C                   EXSR      OREDAT
407300170208     c                   if        wsolodam<>'S'
407400170208     C                   movel     WDAT          D98dtctd
407500170208     c                   endif
407600170208     C                   movel     WDAT          D98dam_rtc
407700170208   x2c                   other
407800170208     c* data teorica di arrivo tenendo conto dell'IMP
407900170208     c                   if        d98nsp>0
408000170208     c                   exsr      letbrv
408100170208     c                   endif
408200170208     c                   if        idDATA>0
408300170208     c                   exsr      CalcoloIMP
408400170208     c                   z-add     wr1poi        WORE
408500170208     c                   else
408600170208     c                   z-add     wpoptc_dee    WORE
408700170208     c                   endif
408800170208     C                   EXSR      OREDAT
408900170208     c                   if        wsolodam<>'S'
409000170208     c                   movel     wdat          d98dtctd
409100170208     c                   endif
409200170208     C                   movel     WDAT          D98dam_rtc
409300170208    2c                   endsl
409400170208     c
409500170208     c
409600170208     c                   ENDSR
409700150218     C********************************************************************
409800150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
409900150218     C********************************************************************
410000150218     c     LetBRV        BEGSR
410100150219     c                   clear                   bollaOK           1
410200150219     c                   clear                   IDnpg
410300150219     c                   clear                   IDdata
410400150219     c                   clear                   IDora
410500150219     c                   clear                   IDfgs
410600150219     c
410700150218     C     Kart          SETLL     FNART01L
410800150218     C     Kart          READE     FNART01L
410900151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
411000150219     c                   clear                   DISfgs
411100150219     c                   clear                   DISdata
411200150219     c                   clear                   DISnpg
411300150219     c                   clear                   DISora
411400150219     c                   clear                   IMPdata
411500150219     c                   clear                   IMPfgs
411600150508     c* per ogni collo verifico se in IMP o disguidato
411700150218     c* al primo che NON trovo, esco come --> OK
411800150218     c     kbrv          setll     fnbrv07l
411900150218     c     kbrv          reade     fnbrv07l
412000150219    2c                   dow       not %eof(fnbrv07l)
412100150219    3c                   if        brvatr='D'  or brvnpg=3
412200150218     c* prendo la data della spunta
412300150218     c                   clear                   fnlv53ds
412400150219     c                   eval      d53tla='L'
412500150218     C                   MOVEL     BRvNPG        D53NPG
412600150218     C                   Z-ADD     BRvNFV        D53NFV
412700150218     C                   MOVEL     BRvFGS        D53FGS
412800150218     C                   CALL      'FNLV53R'
412900150218     C                   PARM                    fnlv53ds
413000150218    4c                   if        d53err<>*blanks
413100150219    5c                   if        brvdcs>0
413200150218     c                   eval      d53dfv=brvdcs
413300150218     c                   else
413400150219    6c                   if        brvdfs>0
413500150218     c                   eval      d53dfv=brvdfs
413600150219    6c                   endif
413700150219    5c                   endif
413800150219    4c                   endif
413900150218     c
414000150219    4c                   select
414100150219     c* se si tratta di DISGUIDO
414200150218     c                   when      brvatr='D'
414300150219     c
414400150219    5c                   if        d53dfv=DISdata
414500150219     c* Verifico l'ora
414600150219    6c                   if        brvhcs>0 and brvhcs>DISora
414700150219     c                   eval      DISdata=d53dfv
414800150219     c                   eval      DISfgs =brvfgs
414900150219     c                   eval      DISnpg =brvnpg
415000150219     c                   eval      DISora =brvhcs
415100150219   x6c                   else
415200150219    7c                   if        brvhms>0 and brvhms>DISora
415300150219     c                   eval      DISdata=d53dfv
415400150219     c                   eval      DISfgs =brvfgs
415500150219     c                   eval      DISnpg =brvnpg
415600150219     c                   eval      DISora =brvhms
415700150219    7c                   endif
415800150219    6c                   endif
415900150219    5c                   endif
416000150219     c
416100150219    5c                   if        d53dfv>DISdata
416200150219     c                   eval      DISdata=d53dfv
416300150219     c                   eval      DISfgs =brvfgs
416400150219     c                   eval      DISnpg =brvnpg
416500150219     c                   eval      DISora =brvhcs
416600150219     c                   if        disora=0
416700150219     c                   eval      DISora =brvhms
416800150219     c                   endif
416900150219    5c                   endif
417000150219     c
417100151009     c* se si tratta di IMP
417200151009    4c                   when      d53spg='P' and d53dfv>impdata
417300151009     c                   eval      IMPdata=d53dfv
417400151009     c                   eval      IMPfgs =brvfgs
417500150219    4c                   endsl
417600150219    3c                   endif
417700151009
417800150219     c     kbrv          reade     fnbrv07l
417900150219    2c                   enddo
418000150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
418100150219     c*  esco dal ciclo: la bolla è OK
418200150219    2c                   if        impdata=0 and disdata=0
418300150219     c                   eval      bollaOK='S'
418400150219     c                   clear                   IDdata
418500150219     c                   clear                   IDfgs
418600150219     c                   clear                   IDora
418700150219     c                   clear                   IDnpg
418800150219   x2c                   else
418900150219     c* memorizzo la data più alta
419000150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
419100150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
419200150219     c                   eval      IDdata=IMPdata
419300150219     c                   eval      IDfgs=IMPfgs
419400150219     c                   eval      IDora=0
419500150219     c                   eval      IDnpg='P'
419600150219    3c                   endif
419700150219    3c                   if        (DISdata>0 and DISdata=IDdata
419800150219     c                             and IDnpg<>'P' and DISora>IDora) or
419900150219     c                             (DISdata>0 and DISdata>IDdata)
420000150219     c                   eval      IDdata=DISdata
420100150219     c                   eval      IDfgs=DISfgs
420200150219     c                   eval      IDora=DISora
420300150219     c                   eval      IDnpg=%char(DISnpg)
420400150219    3c                   endif
420500150219    2c                   endif
420600150218     c
420700150218     C     Kart          READE     FNART01L
420800150219    1c                   enddo
420900150219     c
421000150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
421100150508     c**                 if        d98duc> 0 and d98duc>IDdata
421200150508     c**                 eval      bollaOK='S'
421300150508     c**                 clear                   IDdata
421400150508     c**                 clear                   IDfgs
421500150508     c**                 clear                   IDora
421600150508     c**                 clear                   IDnpg
421700150508     c**                 endif
421800150219     c
421900150218     c
422000150218     C                   ENDSR
422100140825     C********************************************************************
422200140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
422300140825     C********************************************************************
422400140825     c     TRAS_DEI      BEGSR
422500140825     C     WPODEI        IFEQ      *ZEROS
422600140825     C                   Z-ADD     *ZEROS        D98DEI
422700140825     C                   Z-ADD     *ZEROS        D98OEI
422800140825     C                   ELSE
422900140825     C                   Z-ADD     WPODEI        WORE
423000140825     C                   SUB       12            WORE
423100140825     C                   EXSR      OREDAT
423200140825     c
423300140825     C                   Z-ADD     WDAT          D98DEI
423400140825     c                   if        d98tci<>10
423500140825     C                   ADD       12            WRES
423600140825     c                   else
423700140825     C                   ADD       10            WRES
423800140825     c                   endif
423900140825     c
424000140825     C     WRES          MULT      100           D98OEI
424100140825     c
424200140825     c                   if        d98oei=1000
424300140825     c                   eval      d98oei=1030
424400140825     c                   endif
424500140825     c
424600140825     C                   END
424700140825     c                   ENDSR
424800930604     C********************************************************************
424900930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
425000930604     C********************************************************************
425100930604     C     CHKFOR        BEGSR
425200930604     C*
425300930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
425400930604     C* E QUINDI FORZO LA CONFORMITA' A SI
425500941227     C     D98ERR        IFEQ      '0'
425600930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
425700930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
425800930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
425900930604     C                   MOVEL     '1'           D98ERR
426000930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
426100930604     C                   END
426200930604     C                   END
426300990212     C*
426400990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
426500990212     C* E QUINDI FORZO LA CONFORMITA' A SI
426600990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
426700990212     C     D98ERR        IFEQ      '0'
426800990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
426900141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
427000990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
427100990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
427200990212     C                   MOVEL     '1'           D98ERR
427300990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
427400990212     C                   END
427500990212     C                   END
427600990308     C*
427700990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
427800990308     C* E QUINDI FORZO LA CONFORMITA' A SI
427900100614     C**   D98ERR        IFEQ      '0'
428000100614     C**   WFBR          IFEQ      'S'                                          *RESO
428100100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
428200100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
428300100614     C**                 MOVEL     '1'           D98ERR
428400100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
428500100614     C**                 END
428600100614     C**                 END
428700930604     C*
428800930604     C                   ENDSR
428900990212     C********************************************************************
429000990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
429100990212     C********************************************************************
429200990212     C     CHKDAN        BEGSR
429300990212     C*
429400990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
429500150603     c                   movel     'N'           opendan           1
429600040809      *
429700040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
429800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
429900040809      * legate alla spedizione che passo
430000040809     c                   clear                   fidn12ds
430100040809     c                   eval      i12aas = D98AAS
430200040809     c                   eval      i12lnp = D98LNP
430300040809     c                   eval      i12nrs = D98NRS
430400040809     c                   eval      i12nsp = D98NSP
430500040809     c                   eval      i12fel = 'E'
430600040809     c                   eval      i12fan = 'E'
430700040809     c*
430800040809     c                   call      'FIDN12R'
430900040809     c                   parm                    fidn12ds
431000040809     c*
431100040809     c* se non ci sono errori
431200040809     c                   if        o12err = ' '
431300040809     c* se numero di CA trovate maggiore di zero
431400040809     c                   if        o12nca > 0
431500040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
431600150603     c
431700161215     c                   z-add     1             aa                2 0
431800161215     c                   dow       aa <= o12nca
431900150603     C* APERTA
432000161215     C***  skdch(aa)     IFgt      0
432100161215     c****               movel     'S'           opendan
432200161215     c***                endif
432300161215     c                   if        skdca(aa)>0 and skdca(aa)>d98dac
432400161215     c                   eval      d98dac =skdca(aa)
432500161215     c                   eval      d98dcc =skdch(aa)
432600161215     c                   endif
432700150603     c*
432800161215     c                   add       1             aa
432900161215     c                   enddo
433000150603     c
433100040809     c                   endif
433200040809     c                   endif
433300990212     C*
433400990212     C                   ENDSR
433500941228     C********************************************************************
433600941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
433700941228     C********************************************************************
433800941228     C     CALORP        BEGSR
433900941228     C*
434000941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
434100941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
434200941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
434300960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
434400950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
434500950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
434600950110     C                   ELSE
434700950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
434800950110     C                   ENDIF
434900941228     C                   EXSR      RO2COP
435000941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
435100941228     C* SPED. IL GIORNO DOPO
435200941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
435300941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
435400941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
435500941228     C*
435600941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
435700941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
435800941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
435900941228     C                   Z-ADD     WPODSP        WR2POI
436000950110     C                   Z-ADD     WPOPPC        WR2POF
436100941228     C                   EXSR      RO2COP
436200941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
436300941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
436400941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
436500941228     C*
436600941228     C                   ENDSR
436700930414     C********************************************************************
436800930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
436900930414     C********************************************************************
437000930414     C     CALORT        BEGSR
437100930414     C*
437200930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
437300931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
437400930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
437500931126     C                   Z-ADD     *ZEROS        W98HF
437600950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
437700950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
437800950110     C                   ELSE
437900950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
438000950110     C                   ENDIF
438100931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
438200931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
438300930608     C                   ELSE
438400931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
438500930608     C                   END
438600931126     C                   EXSR      RO2COV
438700931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
438800930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
438900930414     C*
439000930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
439100930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
439200930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
439300931126     C                   Z-ADD     *ZEROS        W98HF
439400950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
439500931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
439600931126     C                   EXSR      RO2COV
439700931126     C                   Z-ADD     WR2NOV        W98RTC
439800930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
439900930414     C*
440000930414     C                   ENDSR
440100930414     C********************************************************************
440200930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
440300930414     C********************************************************************
440400930414     C     CALORC        BEGSR
440500930511     C*
440600930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
440700930511     C* I TEMPI DI RITARDO AD OGGI
440800930511     C     WPODCM        IFEQ      *ZEROS
440900960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
441000930511     C                   END
441100940117     C*
441200940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
441300940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
441400940117     C* ALLA DATA DI FINE CONSEGNABILITA'
441500940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
441600940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
441700940118££££ C     WPODFC        IFNE      *ZEROS
441800940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
441900940118££££ C                   END
442000930414     C*
442100930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
442200930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
442300930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
442400931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
442500931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
442600930608     C                   ELSE
442700931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
442800930608     C                   END
442900931126     C                   Z-ADD     WPODCM        WR5POF            9 0
443000931126     C                   EXSR      RO5COV
443100931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
443200930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
443300930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
443400060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
443500060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
443600930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
443700930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
443800930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
443900930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
444000930414     C*
444100930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
444200930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
444300930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
444400931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
444500931126     C                   Z-ADD     WPODCM        WR5POF            9 0
444600931126     C                   EXSR      RO5COV
444700931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
444800930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
444900930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
445000060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
445100060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
445200930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
445300930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
445400930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
445500930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
445600930414     C*
445700930414     C                   ENDSR
445800930414     C********************************************************************
445900930414     C*   CALORR - CALCOLA ORE REALI DI RESA
446000930414     C********************************************************************
446100930414     C     CALORR        BEGSR
446200930414     C*
446300950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
446400950110     C                   ADD       W98RTI        W98RRI
446500941228     C                   ADD       W98RCI        W98RRI
446600950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
446700950110     C                   ADD       W98RTC        W98RRC
446800941228     C                   ADD       W98RCC        W98RRC
446900930414     C*
447000930414     C                   ENDSR
447100930420     C********************************************************************
447200930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
447300930420     C********************************************************************
447400930420     C     CALDIF        BEGSR
447500930420     C*
447600941228     C* PARTENZA
447700941228     C     W98RPI        SUB       D98TPI        W98FPI
447800941228     C     W98RPC        SUB       D98TPC        W98FPC
447900941228     C*
448000930420     C* TRAZIONE
448100930423     C     W98RTI        SUB       D98TTI        W98FTI
448200930423     C     W98RTC        SUB       D98TTC        W98FTC
448300930420     C*
448400930420     C* CONSEGNA
448500090827     C***  W98RCI        SUB       D98TCI        W98FCI
448600090827     C     W98RCI        SUB       w99TCI        W98FCI
448700090827     C***  W98RCC        SUB       D98TCC        W98FCC
448800090827     C     W98RCC        SUB       w99TCC        W98FCC
448900930420     C*
449000930420     C* RESA
449100090921     C***  W98RRI        SUB       D98TRI        W98FRI
449200090921     C     W98RRI        SUB       w99TRI        W98FRI
449300090921     c
449400090921     C***  W98RRC        SUB       D98TRC        W98FRC
449500170104     c
449600170104     c*** se le ore reali interne di RESA usate sono < delle ore reali cliente, uso le
449700170104     c*    reali interne
449800170104     c                   if        w98rri<w98rrc
449900170104     C     W98RRI        SUB       w99TRC        W98FRC
450000170104     c                   else
450100170104     C     W98RRC        SUB       w99TRC        W98FRC
450200170104     c                   endif
450300930420     C*
450400930420     C                   ENDSR
450500930420     C********************************************************************
450600930420     C*   CALCNF - CALCOLA LE CONFORMITA'
450700930420     C********************************************************************
450800930420     C     CALCNF        BEGSR
450900941228     C*
451000941228     C*-------------------------------------------------------------------
451100941228     C* CONFORMITA' PARTENZA  INTERNA
451200941228     C*-------------------------------------------------------------------
451300941228     C                   Z-ADD     W98FPI        W98NPI
451400941228     C     W98FPI        IFLE      *ZEROS
451500941228     C                   MOVEL     'S'           D98CPI
451600941228     C                   ELSE
451700941228     C                   MOVEL     'N'           D98CPI
451800941228     C                   END
451900941228     C*
452000941228     C*-------------------------------------------------------------------
452100941228     C* CONFORMITA' PARTENZA  CLIENTE
452200941228     C*-------------------------------------------------------------------
452300941228     C                   Z-ADD     W98FPC        W98NPC
452400941228     C     W98FPC        IFLE      *ZEROS
452500941228     C                   MOVEL     'S'           D98CPC
452600941228     C                   ELSE
452700941228     C                   MOVEL     'N'           D98CPC
452800941228     C                   END
452900930420     C*
453000930420     C*-------------------------------------------------------------------
453100930420     C* CONFORMITA' TRAZIONE  INTERNA
453200930420     C*-------------------------------------------------------------------
453300930510     C                   Z-ADD     W98FTI        W98NTI
453400930423     C     W98FTI        IFLE      *ZEROS
453500930420     C                   MOVEL     'S'           D98CTI
453600930420     C                   ELSE
453700930420     C                   MOVEL     'N'           D98CTI
453800930420     C                   END
453900930420     C*
454000930420     C*-------------------------------------------------------------------
454100930420     C* CONFORMITA' TRAZIONE  CLIENTE
454200930420     C*-------------------------------------------------------------------
454300930510     C                   Z-ADD     W98FTC        W98NTC
454400930423     C     W98FTC        IFLE      *ZEROS
454500930420     C                   MOVEL     'S'           D98CTC
454600930420     C                   ELSE
454700930420     C                   MOVEL     'N'           D98CTC
454800930420     C                   END
454900930420     C*
455000930420     C*-------------------------------------------------------------------
455100930420     C* CONFORMITA' CONSEGNA  INTERNA
455200930420     C*-------------------------------------------------------------------
455300150226     c                   if        wmancatraz='S'
455400150226     c                   clear                   d98dti
455500150226     c                   clear                   d98hti
455600150226     c                   endif
455700150226     c
455800930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
455900930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
456000930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
456100050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
456200050914     c                   if        consideraora='S'
456300050914     c     w98fci        sub       24            w98nci
456400050914     c                   add       24            d98hrc
456500050914     c                   else
456600930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
456700050914     c                   endif
456800080116     c
456900110923     c                   select
457000080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
457100080116     c*  --> trasformo il ritardo in 0
457200090827     c                   when      d98cca='2'
457300080116     c                   z-add     0             w98nci
457400090827
457500080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
457600080116     c*  e sto leggendo la bolla in arrivo
457700080116     c*  --> trasformo il ritardo = 0
457800090827     c                   when      d98cca='1' and d98tbo='A'
457900080116     c                   z-add     0             w98nci
458000090902     c
458100090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
458200090902     c                   when      wmancaTRAZ='S'
458300090902     c                   z-add     0             w98nci
458400090902     c                   clear                   d98dti
458500090902     c                   clear                   d98hti
458600110923     c
458700110923     c* Ignoro ritardo se pefect order
458800130221     c                   when      d98xco >*zeros
458900110923     c                   z-add     0             w98nci
459000090827     c
459100090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
459200091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
459300091014     c*  controllo ora soglia  consegna da tabella OSC)
459400091009     c                   when      d98tci=10   and
459500091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
459600091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
459700091014     c*
459800091014     c                             d98hci=1030
459900091014     C                   MOVEL     'OSC  '       WR3TAB
460000091014     C                   eval      WR3HHI=d98hmc
460100091014     C                   EXSR      RO3ORE
460200131218
460300091014     c                   if        wr3matpom='M' and d98hmc>1030
460400131218     c                   exsr      ctrLAVH1030
460500131218
460600131218     c                   if        wlav1030<>'S'
460700091014     c                   add       12            w98nci
460800091014     c                   endif
460900131218     c                   endif
461000090928     c
461100090827     c                   endsl
461200050914     c
461300930524     C                   ELSE                                                   <-- 1 -->
461400930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
461500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
461600930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
461700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
461800930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
461900930608     C                   ELSE
462000930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
462100930608     C                   END
462200930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
462300930525     C                   EXSR      RO2COV
462400930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
462500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
462600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
462700930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
462800931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
462900930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
463000930608     C                   ELSE
463100930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
463200930608     C                   END
463300931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
463400930525     C                   EXSR      RO2COV
463500930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
463600930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
463700930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
463800930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
463900930525     C* E' IL RITARDO
464000930610     C     WNOCRI        SUB       W98RCI        W98NCI
464100050901     c
464200050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
464300050901     c*  --> trasformo il ritardo in anticipo
464400050901     c                   if        d98cca='2'
464500080116     c***                z-sub     w98nci        w98nci
464600080116     c                   z-add     0             w98nci
464700050901     c                   endif
464800111018     c* Ignoro ritardo se pefect order
464900130221     c                   if        d98xco >*zeros
465000111018     c                   z-add     0             w98nci
465100111018     c                   endif
465200061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
465300061121     c*  e sto leggendo la bolla in arrivo
465400061121     c*  --> trasformo il ritardo in anticipo
465500061121     c                   if        d98cca='1' and d98tbo='A'
465600080116     c***                z-sub     w98nci        w98nci
465700080116     c                   z-add     0             w98nci
465800061121     c                   endif
465900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
466000070827     c*  mittente, --> consegnato in anticipo
466100070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
466200140403     c                   if        Callar5=' '
466300070827     c                   exsr      LeggiAR5
466400140403     c                   endif
466500070827     c
466600070827     c                   if        dcrpart<>'S'
466700070824     c                   z-sub     w98nci        w98nci
466800070824     c                   endif
466900070827     c
467000070824     c                   endif
467100050907     c
467200050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
467300050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
467400050907     c*  lo trasformo in anticipo perchè non sono in ritardo
467500050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
467600050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
467700050916     c***                if        d98dfc>0
467800070828     c                   if        d98dcm=0  and w98nci>0
467900050907     c                   z-sub     w98nci        w98nci
468000050907     c                   sub       oreILoDOPO    w98nci
468100050907     c                   endif
468200050901     c
468300930525     C                   ELSE                                                   <-- 2 -->
468400070521     c* non do ritardo se la consegna reale cade nel giorno
468500070521     c*  della consegna richiesta se presente IL
468600100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
468700070521     c                             wpodcm<=wpocr1
468800070521     c                   clear                   w98nci
468900100224     c                   else                                                   ---2a -->
469000930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
469100090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
469200090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
469300930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
469400930525     C* E' IL RITARDO O L'ANTICIPO
469500930525     C     W98RCI        SUB       WNOCRF        W98NCI
469600070115     c
469700070115     c* non do ritardo se comunque la consegna reale cade nel giorno
469800070115     c*  della consegna richiesta se presente IL
469900070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
470000070521     c***                          wpodcm<=wpocr1  and w98nci>0
470100070521     c***                clear                   w98nci
470200070521     c***                endif
470300070115     c
470400930525     C                   ELSE                                                   <-- 3 -->
470500930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
470600930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
470700930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
470800930525     C                   END                                                    <-- 3 ---
470900100224     C                   END                                                    <-- 2a---
471000100224
471100100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
471200100224     c                   select
471300100224     c                   when      d98cca='2'
471400100224     c                   z-add     0             w98nci
471500100224
471600100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
471700100224     c*  e sto leggendo la bolla in arrivo
471800100224     c                   when      d98cca='1' and d98tbo='A'
471900100224     c                   z-add     0             w98nci
472000100224     c                   endsl
472100100224     c
472200100224     C                   END                                                    <-- 2 ---
472300070521     C                   END                                                    <-- 1 ---
472400070521     c
472500970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
472600970915     C*/         WAZNCI    IFEQ 'S'
472700970915     C*/         W98NCI    ANDLT*ZEROS
472800970915     C*/                   Z-ADD*ZEROS    W98NCI
472900970915     C*/                   ENDIF
473000970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
473100050922     c* lo faccio solo se non c'e' la data cons richiesta
473200970915     C     D98DTI        IFEQ      *ZEROS
473300050922     c     d98dcr        andeq     *zeros
473400970915     C                   Z-ADD     *ZEROS        W98NCI
473500970915     C                   ENDIF
473600930524     C*
473700930524     C* CONTROLLO CONFORMITA
473800930423     C     W98NCI        IFLE      *ZEROS
473900930524     C                   MOVEL     'S'           D98CCI
474000930420     C                   ELSE
474100930524     C                   MOVEL     'N'           D98CCI
474200930420     C                   END
474300930420     C*
474400930420     C*-------------------------------------------------------------------
474500930420     C* CONFORMITA' CONSEGNA  CLIENTE
474600930420     C*-------------------------------------------------------------------
474700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
474800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
474900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
475000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
475100050914     c                   if        consideraora='S'
475200050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
475300050914     c                   else
475400050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
475500050914     c                   endif
475600080116     c
475700080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
475800080116     c*  --> trasformo il ritardo =0
475900090827     c                   select
476000090827     c                   when      d98cca='2'
476100080116     c                   z-add     0             w98ncc
476200090827
476300080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
476400080116     c*  e sto leggendo la bolla in arrivo
476500080116     c*  --> trasformo il ritardo =0
476600090827     c                   when      d98cca='1' and d98tbo='A'
476700080116     c                   z-add     0             w98ncc
476800090902     c
476900090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
477000090902     c                   when      wmancaTRAZ='S'
477100090902     c                   z-add     0             w98ncc
477200110923     c* Ignoro ritardo se Perfect Order
477300130221     c                   when      d98xco >*zeros
477400110923     c                   z-add     0             w98ncc
477500090902     c
477600090827     c
477700090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
477800090827     c* sono  in ritardo di 12 ore
477900091009     c                   when      d98tcc=10  and
478000091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
478100091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
478200091014     c*
478300091014     c                             d98hce=1030
478400091014     C                   MOVEL     'OSC  '       WR3TAB
478500091014     C                   eval      WR3HHI=d98hmc
478600091014     C                   EXSR      RO3ORE
478700091014     c                   if        wr3matpom='M' and d98hmc>1030
478800131218     c                   exsr      ctrLAVH1030
478900131218
479000131218     c                   if        wlav1030<>'S'
479100131218     c                   add       12            w98ncc
479200131218     c                   endif
479300131218     c****               add       12            w98ncc
479400091014     c                   endif
479500131218     c
479600091014     c***                z-add     12            w98ncc
479700090827     c                   endsl
479800050914     c
479900930524     C                   ELSE                                                   <-- 1 -->
480000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
480100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
480200930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
480300930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
480400930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
480500930525     C                   EXSR      RO2COV
480600930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
480700930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
480800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
480900930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
481000930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
481100931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
481200930525     C                   EXSR      RO2COV
481300930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
481400930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
481500930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
481600930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
481700930525     C* E' IL RITARDO
481800930525     C     WNOCRI        SUB       W98RCC        W98NCC
481900050901     c
482000050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
482100050901     c*  --> trasformo il ritardo in anticipo
482200050901     c                   if        d98cca='2'
482300080116     c***                z-sub     w98ncc        w98ncc
482400080116     c                   z-add     0             w98ncc
482500050901     c                   endif
482600111018     c* Ignoro ritardo se pefect order
482700130221     c                   if        d98xco >*zeros
482800111018     c                   z-add     0             w98ncc
482900111018     c                   endif
483000061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
483100061121     c*  e sto leggendo la bolla in arrivo
483200061121     c*  --> trasformo il ritardo in anticipo
483300061121     c                   if        d98cca='1' and d98tbo='A'
483400080116     c***                z-sub     w98ncc        w98ncc
483500080116     c                   z-add     0             w98ncc
483600061121     c                   endif
483700070824     c
483800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
483900070827     c*  mittente, --> consegnato in anticipo
484000070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
484100070827     c* SBR per vedere se tassativa
484200070827     c                   if        Callar5=' '
484300070827     c                   EXSR      LeggiAR5
484400070827     c                   endif
484500070827     c                   if        dcrpart<>'S'
484600070824     c                   z-sub     w98ncc        w98ncc
484700070824     c                   endif
484800070824     c                   endif
484900050907     c
485000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
485100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
485200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
485300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
485400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
485500050916     c***                if        d98dfc>0
485600070828     c                   if        d98dcm=0 and w98ncc>0
485700050907     c                   z-sub     w98ncc        w98ncc
485800050907     c                   sub       oreILoDOPO    w98ncc
485900050907     c                   endif
486000050901     c
486100930525     C                   ELSE                                                   <-- 2 -->
486200070521     c* Non do ritardo se la consegna reale cade nel giorno
486300070521     c*  della consegna richiesta se presente IL
486400100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
486500070521     c                             wpodcm<=wpocr2
486600070521     c                   clear                   w98ncc
486700100224     c                   else                                                   <--2a -->
486800070521     c
486900930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
487000090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
487100090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
487200930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
487300930525     C* E' IL RITARDO O L'ANTICIPO
487400930525     C     W98RCC        SUB       WNOCRF        W98NCC
487500070115     c
487600070115     c* non do ritardo se comunque la consegna reale cade nel giorno
487700070115     c*  della consegna richiesta se presente IL
487800070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
487900070521     c***                          wpodcm<=wpocr2 and w98ncc>0
488000070521     c***                clear                   w98ncc
488100070521     c***                endif
488200070115     c
488300930525     C                   ELSE                                                   <-- 3 -->
488400930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
488500930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
488600930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
488700930525     C                   END                                                    <-- 3 ---
488800100224     C                   END                                                    <-- 2a---
488900100224
489000100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
489100100224     c                   select
489200100224     c                   when      d98cca='2'
489300100224     c                   z-add     0             w98nci
489400100224
489500100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
489600100224     c*  e sto leggendo la bolla in arrivo
489700100224     c                   when      d98cca='1' and d98tbo='A'
489800100224     c                   z-add     0             w98nci
489900100224     c                   endsl
490000100224     c
490100100224     C                   END                                                    <-- 2 ---
490200070521     C                   END                                                    <-- 1 ---
490300930524     C*
490400930524     C* CONTROLLO CONFORMITA
490500930423     C     W98NCC        IFLE      *ZEROS
490600930524     C                   MOVEL     'S'           D98CCC
490700930420     C                   ELSE
490800930524     C                   MOVEL     'N'           D98CCC
490900930420     C                   END
491000930420     C*
491100930420     C*-------------------------------------------------------------------
491200930420     C* CONFORMITA' RESA INTERNA
491300930420     C*-------------------------------------------------------------------
491400930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
491500930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
491600930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
491700050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
491800050914     c                   if        consideraora='S'
491900050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
492000050914     c                   else
492100050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
492200050914     c                   endif
492300080116     c
492400080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
492500090827     c                   select
492600090827     c                   when      d98cca='2' and w98nri>0
492700080116     c                   z-add     0             w98nri
492800090827     c
492900080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
493000080116     c*  e sto leggendo la bolla in arrivo
493100090827     c                   when      d98cca='1' and d98tbo='A' and
493200080116     c                                            w98nri>0
493300080116     c                   z-add     0             w98nri
493400110923     c
493500110923     c* Ignoro ritardo se Perfect Order
493600130221     c                   when      d98xco >*zeros
493700110923     c                   z-add     0             w98nri
493800110923     c
493900090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
494000090827     c* sono  in ritardo di 12 ore
494100091009     c                   when      d98tci=10  and
494200091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
494300091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
494400091014     c*
494500091014     c                             d98hci=1030
494600091014     C                   MOVEL     'OSC  '       WR3TAB
494700091014     C                   eval      WR3HHI=d98hmc
494800091014     C                   EXSR      RO3ORE
494900091014     c                   if        wr3matpom='M' and d98hmc>1030
495000131218     c                   exsr      ctrLAVH1030
495100131218
495200131218     c                   if        wlav1030<>'S'
495300091014     c                   add       12            w98nri
495400091014     c                   endif
495500131218     c                   endif
495600091014     c
495700091014     c***                z-add     12            w98nri
495800090827     c                   endsl
495900090827
496000930524     C                   ELSE                                                   <-- 1 -->
496100930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
496200930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
496300930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
496400931206     C     WPODCM        ANDLE     WPOCR1
496500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
496600930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
496700930525     C                   Z-ADD     WPODCM        WR2POI            9 0
496800931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
496900930525     C                   EXSR      RO2COV
497000930525     C     WR2NOV        IFNE      *ZEROS
497100930525     C     WR2NOV        MULT      -1            W98NRI
497200930525     C                   END
497300930524     C                   ELSE                                                   <-- 2 -->
497400930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
497500930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
497600930524     C     WPODCM        IFLT      WPOCRI
497700930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
497800930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
497900930524     C                   ELSE
498000931206     C                   Z-ADD     WPOCR1        WPORFI
498100930524     C                   Z-ADD     WPODCM        WPORFF
498200930524     C                   END
498300930524     C* CALCOLO ORE DI DIFFERENZA
498400930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
498500930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
498600930524     C                   Z-ADD     WPORFI        WR2POI            9 0
498700930524     C                   Z-ADD     WPORFF        WR2POF            9 0
498800930524     C                   EXSR      RO2COV
498900930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
499000050902     c
499100050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
499200100224     c                   if        d98cca='2' and w98nri>0
499300100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
499400080116     c***                z-sub     w98nri        w98nri
499500080116     c                   z-add     0             w98nri
499600050902     c                   endif
499700111018     c* Ignoro ritardo se pefect order
499800130221     c                   if        d98xco >*zeros
499900111018     c                   z-add     0             w98nri
500000111018     c                   endif
500100061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
500200061121     c*  e sto leggendo la bolla in arrivo
500300100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
500400100224     c***                if        d98cca='1' and d98tbo='A' and
500500100224     c***                                         w98nri>0 and wpodcm<wpocri
500600080116     c***                z-sub     w98nri        w98nri
500700080116     c                   z-add     0             w98nri
500800061121     c                   endif
500900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
501000070827     c*  mittente, --> consegnato in anticipo
501100070828     c                   if        d98tcr=' ' and d98nsp>0  and
501200070828     c                                            w98nri>0 and wpodcm<wpocri
501300070827     c                   if        CallAR5=' '
501400070827     c                   EXSR      LeggiAR5
501500070824     c                   endif
501600070827     c                   if        dcrpart<>'S'
501700070824     c                   z-sub     w98nri        w98nri
501800070824     c                   endif
501900070824     c                   endif
502000050907     c
502100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
502200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
502300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
502400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
502500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
502600050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
502700050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
502800050907     c                   z-sub     w98nri        w98nri
502900050907     c                   sub       oreILoDOPO    w98nri
503000050907     c                   endif
503100050907     c
503200050907     C                   END                                                    <-- 2 ---
503300930524     C                   END                                                    <-- 1 ---
503400930420     C* CALCOLO LE ORE DI RESA
503500930420     C* CONTROLLO CONFORMITA
503600930423     C     W98NRI        IFLE      *ZEROS
503700930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
503800930420     C                   ELSE
503900930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
504000930420     C                   END
504100930420     C*
504200930420     C*-------------------------------------------------------------------
504300930420     C* CONFORMITA' RESA CLIENTE
504400930420     C*-------------------------------------------------------------------
504500930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
504600930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
504700930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
504800050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
504900050914     c                   if        consideraora='S'
505000050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
505100050914     c                   else
505200050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
505300050914     c                   endif
505400090827     c
505500080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
505600090827     c                   select
505700090827     c                   when      d98cca='2' and w98nrc>0
505800080116     c                   z-add     0             w98nrc
505900090827
506000080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
506100080116     c*  e sto leggendo la bolla in arrivo
506200090827     c                   when      d98cca='1' and d98tbo='A' and
506300080116     c                                            w98nrc>0
506400080116     c                   z-add     0             w98nrc
506500110923     c
506600110923     c* Ignoro ritardo se Perfect Order
506700130221     c                   when      d98xco >*zeros
506800110923     c                   z-add     0             w98nrc
506900090827     c
507000090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
507100090827     c* sono  in ritardo di 12 ore
507200091009     c                   when      d98tcc=10  and
507300091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
507400091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
507500091014     c                             d98hce=1030
507600091014     C                   MOVEL     'OSC  '       WR3TAB
507700091014     C                   eval      WR3HHI=d98hmc
507800091014     C                   EXSR      RO3ORE
507900091014     c                   if        wr3matpom='M' and d98hmc>1030
508000131218     c                   exsr      ctrLAVH1030
508100131218
508200131218     c                   if        wlav1030<>'S'
508300091014     c                   add       12            w98nrc
508400091014     c                   endif
508500131218     c                   endif
508600091014     c
508700091014     c**                 z-add     12            w98nrc
508800090827     c                   endsl
508900090827     c
509000930524     C                   ELSE                                                   <-- 1 -->
509100930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
509200930524     C* DI DIFFERENZA SONO UGUALI A ZERO
509300930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
509400931206     C     WPODCM        ANDLE     WPOCR2
509500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
509600930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
509700930525     C                   Z-ADD     WPODCM        WR2POI            9 0
509800931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
509900930525     C                   EXSR      RO2COV
510000930525     C     WR2NOV        IFNE      *ZEROS
510100930525     C     WR2NOV        MULT      -1            W98NRC
510200930525     C                   END
510300930524     C                   ELSE                                                   <-- 2 -->
510400930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
510500930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
510600930524     C     WPODCM        IFLT      WPOCRI
510700930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
510800930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
510900930524     C                   ELSE
511000931206     C                   Z-ADD     WPOCR2        WPORFI
511100930524     C                   Z-ADD     WPODCM        WPORFF
511200930524     C                   END
511300930524     C* CALCOLO ORE DI DIFFERENZA
511400930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
511500930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
511600930524     C                   Z-ADD     WPORFI        WR2POI            9 0
511700930524     C                   Z-ADD     WPORFF        WR2POF            9 0
511800930524     C                   EXSR      RO2COV
511900930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
512000050902     c
512100050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
512200100224     c                   if        d98cca='2' and w98nrc>0
512300100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
512400080116     c***                z-sub     w98nrc        w98nrc
512500080116     c                   z-add     0             w98nrc
512600050902     c                   endif
512700111018     c* Ignoro ritardo se pefect order
512800130221     c                   if        d98xco >*zeros
512900111018     c                   z-add     0             w98nrc
513000111018     c                   endif
513100070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
513200061121     c*  e sto leggendo la bolla in arrivo
513300100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
513400100224     c***                if        d98cca='1' and d98tbo='A' and
513500100224     c***                                         w98nrc>0 and wpodcm<wpocri
513600080116     c***                z-sub     w98nrc        w98nrc
513700080116     c                   z-add     0             w98nrc
513800061121     c                   endif
513900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
514000070827     c*  mittente, --> consegnato in anticipo
514100070828     c                   if        d98tcr=' ' and d98nsp>0 and
514200070828     c                                            w98nrc>0 and wpodcm<wpocri
514300070827     c                   if        CallAR5=' '
514400070827     c                   EXSR      LeggiAR5
514500070827     c                   endif
514600070827     c                   if        dcrpart<>'S'
514700070824     c                   z-sub     w98nrc        w98nrc
514800070824     c                   endif
514900070824     c                   endif
515000050907     c
515100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
515200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
515300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
515400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
515500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
515600050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
515700050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
515800050907     c                   z-sub     w98nrc        w98nrc
515900050907     c                   sub       oreILoDOPO    w98nrc
516000050907     c                   endif
516100050907     c
516200050907     C                   END                                                    <-- 2 ---
516300930524     C                   END                                                    <-- 1 ---
516400930420     C* CALCOLO LE ORE DI RESA
516500930420     C* CONTROLLO CONFORMITA
516600930423     C     W98NRC        IFLE      *ZEROS
516700930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
516800930420     C                   ELSE
516900930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
517000930420     C                   END
517100930420     C*
517200930420     C                   ENDSR
517300070827     c
517400131218     c********************************************************************
517500131218     c     CtrlavH1030   BEGSR
517600131218     c********************************************************************
517700131218     c                   clear                   wlav1030          1
517800140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
517900131218     c                   z-add     1             xx                4 0
518000131218     c                   dow       evc(xx)<>*blanks
518100140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
518200140127     c                   if        evd(xx)=d98dcm
518300131218     c                   if        evo(xx)<=1030
518400131218     c                   eval      wlav1030='S'
518500131218     c                   leave
518600131218     c                   endif
518700131218     c                   endif
518800131218     c                   add       1             xx
518900131218     c                   enddo
519000131218     c                   ENDSR
519100070827     c********************************************************************
519200070827     c* Controllo record GEN di FIAR5
519300070827     c********************************************************************
519400070827     c     LeggiAR5      BEGSR
519500070827     c                   clear                   DCRPART           1
519600141104     c                   clear                   dar5gen
519700141104     c                   clear                   gc1PART           2
519800141104     c                   clear                   gc2PART           2
519900070827     c     kar5          chain     fiar501l
520000070827    1c                   if        not %found
520100070827     c                   eval      dcrpart='S'
520200141104     c                   eval      gc1part=d98gc1
520300141104     c                   eval      gc2part=d98gc2
520400070827   x1c                   else
520500070827     c* i dati devono essere quelli immessi dalla partenza
520600070827     c                   movel     ar5uni        dar5gen
520700070827     c
520800070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
520900070827     c*  ma il record GEN è stato scritto per altri motivi
521000070827    2c                   if        §ar5mdp='S'
521100070827     c                   movel     d98dcr        alfadcr           8
521200141104     c                   eval      gc1part=§ar5gc1
521300141104     c                   eval      gc2part=§ar5gc2
521400070827     c
521500070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
521600070827     c                   eval      dcrpart='S'
521700070827    3c                   endif
521800070827     c
521900070827   x2c                   else
522000070827     c                   eval      dcrpart='S'
522100141106     c                   eval      gc1part=d98gc1
522200141106     c                   eval      gc2part=d98gc2
522300070827    2c                   endif
522400070827    1c                   endif
522500070827     c
522600140402     c* Memorizzo il flag pickup 2
522700140402     c                   eval      wpick2=§AR5PK2
522800140402     c
522900150401     c*
523000150414     c**                 if        d98cas>0 or d98ffd<>' ' or
523100150414     c**                           d98tc1='A'  or d98tc2='A' or
523200150414     c**                           d98tc1='S'  or d98tc2='S'
523300160331     c                   eval      kar4trc='L'
523400150401     c     kar4          chain     fiar401l
523500160420    1c                   if        %found(fiar401l)
523600150401     c                   eval      dsbl4L=ar4not
523700160420   x1c                   else
523800160420    1c                   endif
523900160420
524000150401     c* Solo per fermo deposito verifico se inviato alert
524100150401     c                   if        d98ffd<>' '
524200150401     c                   clear                   fnlv82ds
524300150401     c                   eval      ILV82AAS=d98aas
524400150401     c                   eval      ILV82LNP=d98lnp
524500150401     c                   eval      ILV82NRS=d98nrs
524600150401     c                   eval      ILV82NSP=d98nsp
524700150401     c                   eval      ILV82TIPA='FD'
524800150401     c                   call      'FNLV82R'
524900150401     c                   parm                    fnlv82ds
525000170821     c* inoltre se stato '0' o '1', verifico se sono arrivati tutti i colli
525100170821     c*  alla lna finale
525200170821     c                   if        olv82sts= '0' or olv82sts='1'
525300170822     c                   clear                   fnlrj6ds
525400170821     c                   if        d98dam>0
525500170821     c                   eval      ilrj6tla ='L'
525600170822     c                   eval      ilrj6tric= 'A'
525700170821     c                   eval      ilrj6aas=d98aas
525800170821     c                   eval      ilrj6lnp=d98lnp
525900170821     c                   eval      ilrj6nrs=d98nrs
526000170821     c                   eval      ilrj6nsp=d98nsp
526100170821     c                   call      'FNLRJ6R'
526200170821     c                   parm                    fnlrj6ds
526300150401     c                   endif
526400170821     c                   endif
526500170821     c
526600170821     c                   endif
526700150414     c**                 endif
526800150401
526900150401     c                   eval      callar5='S'
527000160331     c
527100160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
527200160331     c                   if        %lookup(d98lnp:fildpd)>0
527300170925     c**                 eval      kar4trc='I'
527400170925     c**   kar4          chain     fiar401l
527500170925     c**                 if        %found(fiar401l)
527600170925     c**                 eval      dsbl4I=ar4not
527700170925     c**                 endif
527800170925     c**                 if        §b4trd='1' or §b4trd='2'
527900170925     c**                 eval      wVP='S'
528000170925     c**                 endif
528100170925
528200170925     c** nuovo file dati DPD
528300170925     c     kart          chain     fipnd01l
528400170925     c                   if        %found(fipnd01l)
528500170925     c                   eval      dpndflo=pndflo
528600170925     c                   if        §PNDTRD='1' or §PNDTRD='2'
528700170925     c                   eval      wVP='S'
528800170925     c                   endif
528900170925     c                   endif
529000160401     c
529100160331     c                   endif
529200150401
529300070827     c                   ENDSR
529400930913     C********************************************************************
529500930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
529600930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
529700930913     C*            DATA/ORA (FINE RANGE)
529800930913     C********************************************************************
529900930913     C     CALCNS        BEGSR
530000930914     C*
530100930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
530200930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
530300930914     C     WPODTI        IFEQ      *ZEROS
530400960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
530500930914     C                   ELSE
530600930914     C                   Z-ADD     WPODTI        WPOIRC
530700930914     C                   END
530800930914     C*
530900930914     C* SE NON ANCORA CONSEGNATA
531000930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
531100050922     c* in realtà nella data consegna c'e' la data fine calcolo
531200050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
531300930914     C     WPODCM        IFEQ      *ZEROS
531400050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
531500930914     C                   ELSE
531600930916     C                   Z-ADD     WPODCM        WPOFRC
531700930914     C                   END
531800930914     C*
531900930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
532000930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
532100930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
532200930914     C     WPODFC        ANDLT     WPOIRC
532300930914     C                   ELSE
532400930914     C*
532500930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
532600930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
532700930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
532800930914     C     WPODFC        ANDGT     WPOFRC
532900930914     C                   ELSE
533000930916     C*
533100930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
533200930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
533300930916     C     WPODFC        ANDGT     WPOFRC
533400930916     C                   ELSE
533500930916     C*
533600930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
533700930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
533800930916     C     WPODFC        ANDLT     WPOIRC
533900930916     C                   ELSE
534000930914     C*
534100930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
534200930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
534300930914     C     WPODIC        IFNE      *ZEROS
534400930914     C     WPODIC        ANDGT     WPOFRC
534500930914     C                   Z-ADD     WPOFRC        WPODIC
534600930914     C                   ENDIF
534700930914     C*
534800930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
534900930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
535000930914     C     WPODFC        IFGT      WPOFRC
535100930914     C                   Z-ADD     WPOFRC        WPODFC
535200930914     C                   ENDIF
535300930914     C*
535400930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
535500930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
535600930914     C     WPODIC        IFNE      *ZEROS
535700930914     C     WPODIC        ANDLT     WPOIRC
535800930914     C                   Z-ADD     WPOIRC        WPODIC
535900930914     C                   ENDIF
536000930914     C*
536100930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
536200930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
536300930914     C     WPODFC        IFLT      WPOIRC
536400930914     C                   Z-ADD     WPOIRC        WPODFC
536500930914     C                   ENDIF
536600930914     C*
536700930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
536800931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
536900930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
537000930916     C*
537100931210     C***********WPODIC    IFNE *ZEROS
537200931210     C***********WPODFC    ANDNE*ZEROS
537300931210     C*********************ADD  12        WPODFC
537400931210     C*********************ENDIF
537500930914     C     WPODIC        IFEQ      *ZEROS
537600930914     C     WPODFC        ANDNE     *ZEROS
537700931130     C                   Z-ADD     WPODFC        WPODIC
537800931210     C*********************ADD  12        WPODFC
537900930916     C                   ENDIF
538000930913     C*
538100930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
538200930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
538300930914     C* INTERNA
538400050908     c                   eval      Usacons='S'
538500930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
538600930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
538700931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
538800931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
538900940324     C                   EXSR      RO9COV
539000931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
539100930914     C* CLIENTE
539200930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
539300931210     C                   EXSR      RO9COV
539400931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
539500050908     c                   eval      Usacons=' '
539600930914     C*
539700930916     C                   ENDIF                                                  <-- 5 ---
539800930916     C                   ENDIF                                                  <-- 4 ---
539900930916     C                   ENDIF                                                  <-- 3 ---
540000930914     C                   ENDIF                                                  <-- 2 ---
540100930914     C                   ENDIF                                                  <-- 1 ---
540200930913     C*
540300930913     C                   ENDSR
540400930325     C********************************************************************
540500930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
540600930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
540700930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
540800930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
540900930325     C********************************************************************
541000930325     C     RO1CDP        BEGSR
541100930414     C*
541200931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
541300931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
541400931126     C*
541500931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
541600931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
541700150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
541800150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
541900931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
542000060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
542100930326     C*
542200930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
542300931130     C                   Z-ADD     WR1POI        WR1POO            9 0
542400930325     C*
542500930325     C* CICLO FINO A CHE:
542600930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
542700930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
542800930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
542900930406     C     WR1POO        ANDLT     WPOLIM
543000930414     C*
543100930407     C                   Z-ADD     WR1POO        WPOROU            9 0
543200930325     C                   MOVEL     'N'           WSLITT            1
543300930414     C*
543400930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
543500060420     C*
543600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
543700150605     c     wdee          andeq     ' '
543800930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
543900930414     C                   END
544000930511     C*
544100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
544200150608     c     wdee          andeq     ' '
544300930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
544400930511     C                   END
544500930414     C*
544600930325     C     WSLITT        IFEQ      'N'
544700930423     C                   SUB       12            WR1NOI            5 0
544800930325     C                   END
544900931130     C                   ADD       12            WR1POO            9 0
545000930414     C*
545100931223     C                   ENDDO                                                  <-- 1 ---
545200931126     C*
545300931126     C                   ELSE                                                   <-- 0 -->
545400931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
545500931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
545600931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
545700960129     C     WPOLAF        IFGT      WPODCG
545800960129     C                   Z-ADD     WPOLAF        WTM090            9 0
545900960129     C                   ELSE
546000960129     C                   Z-ADD     WPODCG        WTM090
546100960129     C                   ENDIF
546200960129     C     WPOGIF        IFGT      WTM090
546300960129     C                   Z-ADD     WPOGIF        WTM090
546400960129     C                   ENDIF
546500060420     C     WPOpaif       IFGT      WTM090
546600060420     C                   Z-ADD     WPOpaif       WTM090
546700060420     C                   ENDIF
546800960129     C*
546900960129     C     12            ADD       WTM090        WR1POO
547000960129     C                   ENDIF                                                  <-- 0 ---
547100930512     C*
547200931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
547300931202     C     WR1POO        SUB       12            WR7POI
547400931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
547500931202     C                   ADD       12            WR7POO
547600931202     C     WR7POO        IFGT      WR1POO
547700931202     C                   Z-ADD     WR7POO        WR1POO
547800931202     C                   END
547900930406     C*
548000930325     C                   ENDSR
548100930414     C********************************************************************
548200930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
548300930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
548400930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
548500930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
548600930414     C********************************************************************
548700930414     C     RO2COV        BEGSR
548800091014     c                   clear                   salvaEveL
548900091014     c                   clear                   salvaEveG
549000931130     C*
549100930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
549200930414     C                   Z-ADD     *ZEROS        WR2NOV
549300930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
549400931130     C                   Z-ADD     WR2POI        WR2POW
549500930414     C*
549600930414     C* CICLO FINO A CHE:
549700930414     C*   - DATA INIZIO SUPERATO DATA FINE
549800930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
549900931223     C     WR2POW        ANDLT     WPOLIM
550000930414     C*
550100930414     C                   Z-ADD     WR2POW        WPOROU
550200930414     C                   MOVEL     'N'           WSLITT            1
550300930414     C*
550400930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
550500930414     C*
550600930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
550700930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
550800930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
550900930414     C                   END
551000930414     C*
551100930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
551200930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
551300930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
551400930618     C                   END
551500060420     c
551600060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
551700060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
551800060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
551900060420     C                   END
552000060420     c
552100931201     C*
552200931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
552300931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
552400931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
552500931201     C                   END
552600060418     c
552700060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
552800060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
552900060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
553000060418     C                   END
553100930414     C*
553200930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
553300930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
553400930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
553500930414     C                   END
553600930511     C*
553700930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
553800930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
553900930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
554000930511     C                   END
554100930414     C*
554200930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
554300930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
554400930414     C     WSLITT        IFEQ      'N'
554500930423     C                   ADD       12            WR2NOV            5 0
554600930414     C                   END
554700931130     C                   ADD       12            WR2POW            9 0
554800930414     C*
554900931223     C                   ENDDO                                                  <-- 1 ---
555000930414     C*
555100930414     C                   ENDSR
555200941228     C********************************************************************
555300941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
555400941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
555500941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
555600941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
555700941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
555800941228     C********************************************************************
555900941228     C     RO2COP        BEGSR
556000941228     C*
556100941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
556200941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
556300941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
556400941228     C                   Z-ADD     *ZEROS        WR2NOV
556500941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
556600941228     C                   Z-ADD     WR2POI        WR2POW
556700941228     C*
556800941228     C* CICLO FINO A CHE:
556900941228     C*   - DATA INIZIO SUPERATO DATA FINE
557000941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
557100941228     C*
557200950215     C                   Z-ADD     WR2POW        WPOROU
557300941228     C                   MOVEL     'N'           WSLITT
557400941228     C* TRASFORMA IL PROGR. ORE IN DATA
557500950215     C*******              Z-ADDWR2POW    WORE
557600950215     C*******              EXSR OREDAT
557700950215     C*******              Z-ADDWDAT      W12DAT  80
557800950215     C*******              Z-ADDWRES      W12RES  30
557900941228     C*
558000941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
558100941228     C*
558200941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
558300941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
558400941228     C     WSLITT        IFEQ      'N'
558500941228     C                   ADD       12            WR2NOV            5 0
558600941228     C                   END
558700941228     C                   ADD       12            WR2POW            9 0
558800941228     C*
558900941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
559000941228     C* ENTRI IN LOOP
559100941228     C     WR2NOV        IFGT      999
559200941228     C                   LEAVE                                                  *VA A ENDDO
559300941228     C                   END
559400941228     C*
559500941228     C                   ENDDO                                                  <-- 1 ---
559600941228     C*
559700941228     C                   ENDSR
559800930510     C********************************************************************
559900930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
560000930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
560100930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
560200930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
560300050905     c*                     il num ore deve essere un multiplo di 12
560400050905     c*                     oppure 1 ora
560500930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
560600930510     C********************************************************************
560700930510     C     RO4CDP        BEGSR
560800930510     C*
560900930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
561000930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
561100050905     c                   movel     '1'           wgiro             1
561200930510     C*
561300930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
561400930510     C                   Z-ADD     WR4POI        WR4POO            9 0
561500930510     C*
561600930510     C* CICLO FINO A CHE:
561700930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
561800930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
561900930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
562000930510     C     WR4POO        ANDLT     WPOLIM
562100930510     C*
562200141106     c                   if        nosomma<>'N'
562300931130     C                   ADD       12            WR4POO            9 0
562400141106     c                   endif
562500930510     C                   Z-ADD     WR4POO        WPOROU            9 0
562600930510     C                   MOVEL     'N'           WSLITT            1
562700930510     C*
562800930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
562900050905     c
563000050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
563100930510     C     WSLITT        IFEQ      'N'
563200050905     c
563300050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
563400050905     c*  se cado in giorno non festivo
563500050905     c                   if        wr4noi=1 and wgiro='1'
563600050905     c                   sub       12            wr4poo
563700050905     c                   add       wr4noi        wr4poo
563800050905     c                   endif
563900050905     c*
564000050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
564100050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
564200050905     c*  calcolate dal pgm
564300050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
564400050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
564500050905     c*  del giorno sucessivo non festivo
564600050905     c                   if        wr4noi=1 and wgiro<>'1'
564700050905     c                   add       wr4noi        wr4poo
564800050905     c                   endif
564900050905     c
565000930510     C                   SUB       12            WR4NOI            5 0
565100930510     C                   END
565200141106     c
565300141106     c                   if        nosomma= 'N'
565400141106     C                   ADD       12            WR4POO            9 0
565500141106     c                   endif
565600930510     C*
565700050905     c                   eval      wgiro='2'
565800931223     C                   ENDDO                                                  <-- 1 ---
565900930510     C*
566000930510     C                   ENDSR
566100931130     C********************************************************************
566200931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
566300931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
566400931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
566500931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
566600931130     C********************************************************************
566700931130     C     RO5COV        BEGSR
566800931130     C*
566900931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
567000060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
567100060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
567200060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
567300931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
567400931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
567500931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
567600931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
567700931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
567800931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
567900931130     C                   Z-ADD     *ZEROS        WR5NOV
568000940714     C*
568100940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
568200940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
568300931207     C*
568400931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
568500940908     C     WIC           IFEQ      'I'
568600090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
568700090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
568800931207     C                   ELSE
568900090827     C*****              Z-ADD     D98TCC        WPOTCT
569000090827     C                   Z-ADD     w99TCC        WPOTCT
569100931207     C                   END
569200931130     C*
569300931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
569400060420     C                   clear                   COV
569500931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
569600931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
569700931207     C                   Z-ADD     2             CO                3 0
569800931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
569900931207     C                   EXSR      RO5SUB
570000981102     C                   DO        99            IC                3 0
570100931207     C     LAF(IC)       IFNE      *ZEROS
570200931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
570300931207     C                   EXSR      RO5SUB
570400931207     C                   ENDIF
570500931207     C                   ENDDO
570600060420     c
570700060420     C                   DO        99            IC                3 0
570800060420     C     paif(IC)      IFNE      *ZEROS
570900060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
571000060420     C                   EXSR      RO5SUB
571100060420     C                   ENDIF
571200060420     C                   ENDDO
571300931207     C* ORDINA LA SCHIERA COV
571400931207     C                   SORTA     COV
571500931130     C*
571600931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
571700931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
571800981102     C                   DO        98            IC                             --- 1 -->
571900931207     C     IC            ADD       1             I1C               3 0
572000931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
572100931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
572200931207     C     COV(I1C)      OREQ      *ZEROS
572300931207     C                   ELSE
572400931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
572500931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
572600931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
572700931130     C                   EXSR      RO2COV
572800931207     C                   ADD       WR2NOV        WR5NOV            9 0
572900931209     C* SE NON SONO IN DATA CONSEGNA
573000931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
573100931207     C     WR5NOV        IFLT      WPOTCT
573200931209     C     COV(I1C)      ANDNE     WPODCM
5733009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5734009709111009 C     WPOTCT        SUB       12            WR5NOV
573500931130     C                   END
573600931207     C                   ENDIF                                                  <-- 2 ---
573700931207     C                   ENDDO                                                  <-- 1 ---
573800940714     C                   ENDIF                                                  <-- 0 ---
573900931130     C*
574000931130     C                   ENDSR
574100931207     C********************************************************************
574200931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
574300931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
574400931207     C********************************************************************
574500931207     C     RO5SUB        BEGSR
574600931207     C*
574700931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
574800931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
574900931207     C     WR5COV        ANDLT     WR5POF
575000931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
575100931207     C                   MOVEL     'N'           WR5PRE            1
575200981102     C*---                 DO   30        I
575300981102     C                   DO        99            I
575400931207     C     WR5COV        IFEQ      COV(I)
575500931207     C                   MOVEL     'S'           WR5PRE
575600931207     C                   LEAVE
575700931207     C                   ENDIF
575800931207     C                   ENDDO
575900931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
576000931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
576100931207     C                   ADD       1             CO                3 0
576200931207     C                   Z-ADD     WR5COV        COV(CO)
576300931207     C                   ENDIF                                                  <-- 2 ---
576400931207     C                   ENDIF                                                  <-- 1 ---
576500931207     C*
576600931207     C                   ENDSR
576700931201     C********************************************************************
576800060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
576900931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
577000931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
577100931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
577200931201     C********************************************************************
577300931201     C     RO6CDT        BEGSR
577400931201     C*
577500931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
577600931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
577700931201     C*
577800931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
577900931201     C                   Z-ADD     WR6POI        WR6POO            9 0
578000931201     C*
578100931201     C* CICLO FINO A CHE:
578200931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
578300931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
578400931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
578500931201     C     WR6POO        ANDLT     WPOLIM
578600931201     C*
578700931201     C                   Z-ADD     WR6POO        WPOROU            9 0
578800931201     C                   MOVEL     'N'           WSLITT            1
578900931201     C*
579000931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
579100931201     C*
579200931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
579300150605     c     wdee          andeq     ' '
579400931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
579500931201     C                   END
579600931201     C*
579700931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
579800150608     c     wdee          andeq     ' '
579900931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
580000931201     C                   END
580100931201     C*
580200931201     C     WSLITT        IFEQ      'N'
580300931201     C                   SUB       12            WR6NOI            5 0
580400931201     C                   END
580500931201     C                   ADD       12            WR6POO            9 0
580600931201     C*
580700931223     C                   ENDDO                                                  <-- 1 ---
580800931201     C*
580900931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
581000931202     C     WR6POO        SUB       12            WR7POI
581100931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
581200931207     C                   ADD       12            WR7POO
581300931202     C     WR7POO        IFGT      WR6POO
581400931201     C                   Z-ADD     WR7POO        WR6POO
581500931202     C                   END
581600931201     C*
581700931201     C                   ENDSR
581800931201     C********************************************************************
581900060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
582000060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
582100060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
582200060407     C*            INPUT:  -WR7POI (PROGR.ORE)
582300931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
582400931201     C********************************************************************
582500931201     C     RO7TCF        BEGSR
582600060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
582700060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
582800060407     C                   MOVEL     'N'           WSLITT
582900931201     C*
583000931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
583100931201     C                   Z-ADD     WR7POI        WR7POO            9 0
583200931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
583300931201     C* COINCIDA CON UN TURNO DI CHIUSURA
583400060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
583500931201     C     D98GC2        ANDEQ     *BLANKS
583600060407   x1C                   ELSE                                                   <-- 1 -->
583700931214     C*
583800931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
583900931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
584000931214     C*
584100931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
584200931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
584300931223     C     WPOROU        ANDLT     WPOLIM
584400931214     C* MEMORIZZO INIZIO E FINE RANGE
584500931202     C     WPOTI         IFEQ      *ZEROS
584600931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
584700931202     C                   END
584800931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
584900931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
585000931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
585100931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
585200931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
585300931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
585400931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
585500931201     C                   MOVEL     'N'           WSLITT
585600931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
585700060407     c* Se non trovat, controllo se c'e' evento part che giustifica
585800060407     c                   if        wslitt='N'
585900060407     c                   exsr      datcepg
586000060407     c                   endif
586100060407     c
586200931201     C                   ENDDO                                                  <-- 2 ---
586300931214     C*
586400931201     C                   Z-ADD     WPOROU        WR7POO
586500931214     C*
586600931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
586700931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
586800931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
586900931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
587000931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
587100931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
587200931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
587300931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
587400931209     C                   ELSE                                                   <-- 3 -->
587500931209     C     WIC           IFEQ      'I'
587600931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
587700931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
587800931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
587900931201     C                   ELSE
588000931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
588100931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
588200931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
588300931201     C                   ENDIF
588400931209     C                   ENDIF                                                  <-- 3 ---
588500931209     C                   ENDIF                                                  <-- 2 ---
588600931214     C*
588700060407    1C                   ENDIF                                                  <-- 1 ---
588800060407     c
588900060407     c* Se non trovato turno di chiusura, verifico se c'e'
589000060407     c*  evento particolare che GIUSTIFICA
589100060407     c                   if        wpoti=0
589200060418     C                   EXSR      RO7CEPG
589300060407     c                   endif
589400931201     C*
589500931201     C                   ENDSR
589600060406     C********************************************************************
589700060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
589800060406     c*                        che giustifica
589900060407     C*            INPUT:-WR7POI (PROGR.ORE)
590000060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
590100060406     C********************************************************************
590200060407     C     RO7CEPG       BEGSR
590300060406     C*
590400060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
590500060407     C                   Z-ADD     WR7POI        WR7POO            9 0
590600060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
590700060406     C* coincide
590800060406    1C                   if        wevpag='S'                                   <-- 1 -->
590900060406     C*
591000060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
591100060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
591200060406     C                   MOVEL     'N'           WSLITT
591300060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
591400060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
591500060406     C*
591600060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
591700060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
591800060406     C     WPOROU        ANDLT     WPOLIM
591900060406     C* MEMORIZZO INIZIO E FINE RANGE
592000060406    3C     WPOTI         IFEQ      *ZEROS
592100060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
592200060406    3C                   END
592300060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
592400060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
592500060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
592600060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
592700060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
592800060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
592900060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
593000060406     C                   MOVEL     'N'           WSLITT
593100060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
593200060406     C* Se non trovato,
593300060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
593400060406    3c                   if        wslitt='N'
593500060406     c                   exsr      DATTUC
593600060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
593700060406    2C                   ENDDO                                                  <-- 2 ---
593800060406     C*
593900060407     C                   Z-ADD     WPOROU        WR7POO
594000060406     C*
594100060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
594200060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
594300060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
594400060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
594500060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
594600060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
594700060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
594800060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
594900060406   x3C                   ELSE                                                   <-- 3 -->
595000060406    4C     WIC           IFEQ      'I'
595100060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
595200060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
595300060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
595400060406   x4C                   ELSE
595500060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
595600060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
595700060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
595800060406    4C                   ENDIF
595900060406    3C                   ENDIF                                                  <-- 3 ---
596000060406    2C                   ENDIF                                                  <-- 2 ---
596100060406     C*
596200060406    1C                   ENDIF                                                  <-- 1 ---
596300060406     C*
596400060406     C                   ENDSR
596500931207     C********************************************************************
596600931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
596700931207     C*            INPUT:  -WR8POI (PROGR.ORE)
596800931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
596900931207     C********************************************************************
597000931207     C     RO8FES        BEGSR
597100130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
597200130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
597300931207     C*
597400931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
597500130411     C     WR8POI        SUB       12            WR8POO            9 0
597600931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
597700931207     C                   MOVEL     'N'           WSLITT
597800130411     c
597900130411     c* Non salto i festivi ???
598000130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
598100130411     c
598200931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
598300130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
598400130411     C     WPOROU        ANDGT     WPODSP
598500130411     C                   SUB       12            WPOROU
598600130411     C                   MOVEL     'N'           WSLITT
598700130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
598800130411     C                   ENDDO                                                  <-- 2 ---
598900130411     c
599000130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
599100130411     C     D98GC2        ANDEQ     *BLANKS
599200130411   x1C                   ELSE                                                   <-- 1 -->
599300130411     c* eseguo controllo per i turni di chiusura
599400130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
599500130411     C*
599600130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
599700130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
599800130411     C     WPOROU        ANDgT     WPOdsp
599900130411     C* MEMORIZZO INIZIO E FINE RANGE
600000130411     C     WPOTf         IFEQ      *ZEROS
600100130411     C                   z-add     wporou        WPOTf
600200130411     C                   END
600300140826     C     WPOROU        SUB       wnoadd        WPOROU
600400130411     c
600500130411     C                   MOVEL     'N'           WSLITT
600600130411     c                   exsr      dattuc
600700130411     c                   if        wslitt='N'
600800130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
600900140826     c* Se slitta per festivo --> tolgo sempre 12 ore
601000140826     c                   z-add     12            WNOADD
601100130411     c                   endif
601200130411     C* SLITTA
601300140826     C     wporou        add       12            WPOTi
601400130411    2C                   ENDDO                                                  <-- 2 ---
601500130411     c
601600130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
601700130411     c
601800130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
601900130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
602000130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
602100130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
602200130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
602300130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
602400130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
602500130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
602600130411   x3C                   ELSE                                                   <-- 3 -->
602700130411    4C     WIC           IFEQ      'I'
602800130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
602900130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
603000130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
603100130411   x4C                   ELSE
603200130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
603300130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
603400130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
603500130411    4C                   ENDIF
603600130411    3C                   ENDIF                                                  <-- 3 ---
603700130411    2C                   ENDIF                                                  <-- 3 ---
603800130411     C*
603900130411    1C                   ENDIF                                                  <-- 1 ---
604000130411
604100130411     C**   WPOROU        ADD       12            WR8POO
604200130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
604300130411     c                   z-add     wr8poi        wr8poo
604400931207     C*
604500931207     C                   ENDSR
604600931210     C********************************************************************
604700931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
604800940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
604900931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
605000931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
605100931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
605200931210     C********************************************************************
605300931210     C     RO9COV        BEGSR
605400931210     C*
605500931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
605600931210     C                   Z-ADD     *ZEROS        WR9NOV
605700931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
605800931210     C                   Z-ADD     WR9POI        WR9POW
605900931210     C* CICLO FINO A CHE:
606000931210     C*   - DATA INIZIO SUPERATO DATA FINE
606100931223     C*   - RAGGIUNTO LIMITE MASSIMO
606200931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
606300931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
606400931223     C     WR9POW        ANDLT     WPOLIM
606500931210     C*
606600931210     C                   Z-ADD     WR9POW        WPOROU
606700931210     C                   MOVEL     'N'           WSLITT            1
606800940324     C* FESTIVI
606900931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
607000940324     C* GIACENZA
607100931214     C     WSLITT        IFEQ      'N'
607200941229£$   C     WPOAGC        IFNE      *ZEROS
607300940324£$   C     WPOCGC        ORNE      *ZEROS
607400941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
607500940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
607600940324£$   C                   MOVEL     'S'           WSLITT
607700940324£$   C                   END
607800940324£$   C                   END
607900940324£$   C                   END
608000960129     C* EVENTI GIACENZA
608100960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
608200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
608300960129     C                   Z-ADD     *ZEROS        G                 3 0
608400960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
608500960129     C     G             ANDLT     IG
608600960129     C                   ADD       1             G
608700960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
608800960209     C     WPOROU        ANDLE     GCF(G)
608900960129     C                   MOVEL     'S'           WSLITT
609000960129     C                   ENDIF                                                  <-- 3 ---
609100960129     C                   ENDDO                                                  <-- 2 ---
609200960129     C                   ENDIF                                                  <-- 1 ---
609300960129     C                   ENDIF                                                  <-- 0 ---
609400940324     C* LASCIATO AVVISO
609500931214     C     WSLITT        IFEQ      'N'
609600941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
609700940324£$   C                   Z-ADD     *ZEROS        L                 3 0
609800940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
609900940324£$   C     L             ANDLT     IL
610000940324£$   C                   ADD       1             L
610100941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
610200941229£$   C     WPOROU        ANDLE     LCF(L)
610300050520     c     LCon(L)       andeq     'S'
610400940324£$   C                   MOVEL     'S'           WSLITT
610500940324£$   C                   END                                                    <-- 3 ---
610600940324£$   C                   ENDDO                                                  <-- 2 ---
610700940324£$   C                   END                                                    <-- 1 ---
610800931210     C                   END
610900940324     C* TURNO CHIUSURA
611000140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
611100140826     c*  di sistemare le ore da sommare 12 o 24...
611200931214     C     WSLITT        IFEQ      'N'
611300931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
611400931210     C                   END
611500940324     C* DATA CONSEGNA RICHIESTA
611600050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
611700050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
611800050907     c
611900931214     C     WSLITT        IFEQ      'N'
612000050907     c                   add       1             wporou
612100940324£$   C     WPOCRI        IFNE      *ZEROS
612200940324£$   C     WPOROU        IFLT      WPOCRC
612300940324£$   C                   MOVEL     'S'           WSLITT
612400940324£$   C                   END
612500940324£4   C                   END
612600050907     c                   sub       1             wporou
612700931210     C                   END
612800931210     C*
612900931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
613000931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
613100931210     C     WSLITT        IFEQ      'N'
613200931210     C                   ADD       12            WR9NOV            5 0
613300931210     C                   END
613400931210     C                   ADD       12            WR9POW            9 0
613500931210     C*
613600931223     C                   ENDDO                                                  <-- 1 ---
613700931210     C*
613800931210     C                   ENDSR
613900940628%%%  C********************************************************************
614000940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
614100940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
614200940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
614300940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
614400940628%%%  C********************************************************************
614500940628%%%  C     RO0GES        BEGSR
614600940628%%%  C*
614700940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
614800940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
614900940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
615000940628%%%  C*
615100940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
615200940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
615300940628%%%  C*
615400940628%%%  C* CICLO FINO A CHE:
615500940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
615600940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
615700940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
615800050922%%%  C     WR0POO        ANDGe     WPODSP
615900940628%%%  C*
616000940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
616100940628%%%  C                   MOVEL     'N'           WSLITT            1
616200940628%%%  C*
616300940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
616400940628%%%  C*
616500940628%%%  C     WSLITT        IFEQ      'N'
616600940628%%%  C                   SUB       12            WR0NOI            5 0
616700940628%%%  C                   END
616800940628%%%  C                   SUB       12            WR0POO            9 0
616900940628%%%  C*
617000940628%%%  C                   ENDDO                                                  <-- 1 ---
617100940628%%%  C*
617200940628%%%  C                   ADD       12            WR0POO            9 0
617300940628%%%  C*
617400940628%%%  C                   ENDSR
617500960911     C********************************************************************
617600960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
617700960911     C*            INPUT:  -W10POI (PROGR.ORE)
617800960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
617900960911     C********************************************************************
618000960911     C     R10FES        BEGSR
618100960911     C*
618200960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
618300960911     C                   Z-ADD     W10POI        W10POO            9 0
618400960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
618500960911     C                   MOVEL     'N'           WSLITT
618600960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
618700960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
618800960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
618900960911     C                   ADD       12            WPOROU
619000960911     C                   MOVEL     'N'           WSLITT
619100960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
619200960911     C                   ENDDO                                                  <-- 2 ---
619300960911     C                   Z-ADD     WPOROU        W10POO
619400960911     C*
619500960911     C                   ENDSR
619600930325     C********************************************************************
619700930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
619800930325     C********************************************************************
619900930325     C     CHKGIA        BEGSR
620000930406     C*
620100960129     C* APERTURA/CHIUSURA GIACENZA
620200930326     C     WPODAG        IFNE      *ZEROS
620300930406     C     WPODCG        ORNE      *ZEROS
620400930407     C     WPOROU        IFGE      WPODAG
620500930407     C     WPOROU        ANDLE     WPODCG
620600930325     C                   MOVEL     'S'           WSLITT
620700930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
620800930325     C                   END
620900930326     C                   END
621000960129     C*
621100960129     C* EVENTI GIACENZA
621200960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
621300960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
621400960129     C                   Z-ADD     *ZEROS        G                 3 0
621500960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
621600960129     C     G             ANDLT     IG
621700960129     C                   ADD       1             G
621800960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
621900960129     C     WPOROU        ANDLE     GIF(G)
622000091009     c
622100091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
622200091009     c*  l'orario dell'evento
622300091009     c                   select
622400091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
622500091009     c* Se orario entro le 10.30  va bene
622600091014     c*  solo la prima volta per evento cioè quando controllo
622700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
622800091014     c*  e non devo dare errore
622900091014     C                   MOVEL     'OSC  '       WR3TAB
623000091014     C                   eval      WR3HHI=evo(gip(G))
623100091014     C                   EXSR      RO3ORE
623200091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
623300091014     c*  in ritardo come per "E")
623400091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
623500091014     c                              wr3matpom='P'
623600091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
623700091009     C                   MOVEL     'S'           WSLITT
623800091009     c                   endif
623900091014     c                   eval      SalvaEveG=gip(G)
624000091009     c
624100091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
624200091009     c* Se orario entro le 10.30  va bene
624300091014     c*  solo la prima volta per evento cioè quando controllo
624400091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
624500091014     c*  e non devo dare errore
624600091014     C                   MOVEL     'OSC  '       WR3TAB
624700091014     C                   eval      WR3HHI=evo(gip(G))
624800091014     C                   EXSR      RO3ORE
624900091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
625000091014     c*  in ritardo come per "E")
625100091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
625200091014     c                              wr3matpom='P'
625300091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
625400091009     C                   MOVEL     'S'           WSLITT
625500091009     c                   endif
625600091014     c                   eval      SalvaEveG=gip(G)
625700091009     c
625800091009     c                   other
625900960129     C                   MOVEL     'S'           WSLITT
626000960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
626100091009     c                   endsl
626200091009
626300960129     C                   ENDIF                                                  <-- 3 ---
626400960129     C                   ENDDO                                                  <-- 2 ---
626500960129     C                   ENDIF                                                  <-- 1 ---
626600960129     C                   ENDIF                                                  <-- 0 ---
626700930406     C*
626800930325     C                   ENDSR
626900060420     C********************************************************************
627000930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
627100930325     C********************************************************************
627200930325     C     CHKLAV        BEGSR
627300930325     C*
627400941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
627500930421     C                   Z-ADD     *ZEROS        L                 3 0
627600930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
627700930421     C     L             ANDLT     IL
627800930421     C                   ADD       1             L
627900930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
628000930421     C     WPOROU        ANDLE     LAF(L)
628100090928     c
628200090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
628300091009     c*  l'orario dell'evento
628400091009     c                   select
628500091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
628600091009     c* Se orario entro le 10.30  va bene
628700091014     c*  solo la prima volta per evento cioè quando controllo
628800091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
628900091014     c*  e non devo dare errore
629000091014     C                   MOVEL     'OSC  '       WR3TAB
629100091014     C                   eval      WR3HHI=evo(lap(L))
629200091014     C                   EXSR      RO3ORE
629300091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
629400091014     c*  in ritardo come per "E")
629500091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
629600091014     c                              wr3matpom='P'
629700091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
629800091009     C                   MOVEL     'S'           WSLITT
629900091009     c                   endif
630000091014     c                   eval      SalvaEveL=lap(L)
630100091009     c
630200091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
630300091009     c* Se orario entro le 10.30  va bene
630400091014     c*  solo la prima volta per evento cioè quando controllo
630500091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
630600091014     c*  e non devo dare errore
630700091014     C                   MOVEL     'OSC  '       WR3TAB
630800091014     C                   eval      WR3HHI=evo(lap(L))
630900091014     C                   EXSR      RO3ORE
631000091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
631100091014     c*  in ritardo come per "E")
631200091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
631300091014     c                              wr3matpom='P'
631400091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
631500091009     C                   MOVEL     'S'           WSLITT
631600091009     c                   endif
631700091014     c                   eval      SalvaEveL=lap(L)
631800090928     c
631900091009     c                   other
632000930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
632100091009     C                   MOVEL     'S'           WSLITT
632200091009     c                   endsl
632300090928     c
632400930421     C                   END                                                    <-- 3 ---
632500931223     C                   ENDDO                                                  <-- 2 ---
632600930421     C                   END                                                    <-- 1 ---
632700930325     C*
632800930325     C                   ENDSR
632900060420     C********************************************************************
633000060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
633100060420     c*                   che Allungano
633200060420     C********************************************************************
633300060420     C     CHKCEPA       BEGSR
633400060420     C*
633500060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
633600060420     C                   Z-ADD     *ZEROS        p                 3 0
633700060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
633800060420     C     p             ANDLT     IpA
633900060420     C                   ADD       1             P
634000060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
634100060420     C     WPOROU        ANDLE     paif(P)
634200060420     C                   MOVEL     'S'           WSLITT
634300060420     C                   ADD       12            W98EA
634400060420     C                   END                                                    <-- 3 ---
634500060420     C                   ENDDO                                                  <-- 2 ---
634600060420     C                   END                                                    <-- 1 ---
634700060420     C*
634800060420     C                   ENDSR
634900930414     C********************************************************************
635000930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
635100930414     C********************************************************************
635200930414     C     CHKCNT        BEGSR
635300930511     C*
635400930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
635500930611     C     WIC           IFEQ      'I'                                          *INTERNO
635600930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
635700930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
635800930611     C                   ELSE
635900930611     C     WPOROU        IFGE      WPOAII
636000930611     C     WPOROU        ANDLE     WPOAFI
636100930611     C                   MOVEL     'S'           WSLITT
636200930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
636300930611     C                   END
636400930611     C                   END
636500930611     C                   ELSE                                                   *CLIENTE
636600930611     C     WPOAIC        IFEQ      *ZEROS
636700930611     C     WPOAFC        ANDEQ     *ZEROS
636800930611     C                   ELSE
636900930611     C     WPOROU        IFGE      WPOAIC
637000930611     C     WPOROU        ANDLE     WPOAFC
637100930611     C                   MOVEL     'S'           WSLITT
637200930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
637300930611     C                   END
637400930611     C                   END
637500930611     C                   END
637600930414     C*
637700930414     C                   ENDSR
637800930511     C********************************************************************
637900930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
638000930511     C********************************************************************
638100930511     C     CHKFED        BEGSR
638200930511     C*
638300930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
638400930611     C     WIC           IFEQ      'I'                                          *INTERNO
638500930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
638600930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
638700930611     C                   ELSE
638800930611     C     WPOROU        IFGE      WPOFII
638900930611     C     WPOROU        ANDLE     WPOFFI
639000930611     C                   MOVEL     'S'           WSLITT
639100930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
639200930611     C                   END
639300930611     C                   END
639400930611     C                   ELSE                                                   *CLIENTE
639500930611     C     WPOFIC        IFEQ      *ZEROS
639600930611     C     WPOFFC        ANDEQ     *ZEROS
639700930611     C                   ELSE
639800930611     C     WPOROU        IFGE      WPOFIC
639900930611     C     WPOROU        ANDLE     WPOFFC
640000930611     C                   MOVEL     'S'           WSLITT
640100930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
640200930611     C                   END
640300930611     C                   END
640400930611     C                   END
640500930511     C*
640600930511     C                   ENDSR
640700930325     C********************************************************************
640800930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
640900930325     C********************************************************************
641000930325     C     CHKFES        BEGSR
641100060526     c* se già passata la data, non controllo wporou
641200060526     c                   if        wpodata>0
641300060526     c                   z-add     wpodata       w12dat
641400060526     c                   clear                   w12res
641500060526     c                   else
641600931130     C*
641700931130     C* TRASFORMA IL  PROGR.ORE IN DATA
641800931130     C                   Z-ADD     WPOROU        WORE
641900931130     C                   EXSR      OREDAT
642000941227     C                   Z-ADD     WDAT          W12DAT            8 0
642100931130     C                   Z-ADD     WRES          W12RES            3 0
642200060526     c                   endif
642300930325     C*
642400941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
642500930325     C* E' FESTIVO
642600170117     c* l'affidabilità partenza ora fa un doppio tentativo
642700170117    1C     WTC           IFEQ      'P'                                          *PARTENZA
642800170117     C                   Z-ADD     D98LNP        CLNTFP
642900170117     C                   Z-ADD     *ZEROS        CLNTFA
643000170117     c                   exsr      azcln_fes
643100170117     c* se non è festivo guardo anche la trazione
643200170117     c                   if        wslitt<>'S'
643300170117     C                   Z-ADD     D98TFP        CLNTFP
643400170117     C                   Z-ADD     D98TFA        CLNTFA
643500170117     c                   exsr      azcln_fes
643600170117     C                   ENDIF
643700170117     c
643800170117   x1C                   Else
643900170117     c
644000090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
644100090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
644200090108     c**   è possibile rendere festivo in terminal per un altro terminal
644300090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
644400170117     c
644500170117     c** ES 17/01/17 - Faccio doppio tentativo: prima con la filiale e poi con
644600170117     c**               la trazione , se filiale non festiva
644700930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
644800170117     C***  WTC           oreq      'P'                                          *TRAZIONE
644900941227     C                   Z-ADD     D98TFP        CLNTFP
645000941227     C                   Z-ADD     D98TFA        CLNTFA
645100941228     C                   ENDIF
645200941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
645300941227     C                   Z-ADD     *ZEROS        CLNTFP
645400941227     C                   Z-ADD     D98LNA        CLNTFA
645500941228     C                   ENDIF
645600170117     c
645700170117     c                   exsr      azcln_fes
645800170117    1c                   endif
645900930325     C*
646000060526     c                   clear                   wpodata
646100930325     C                   ENDSR
646200170117
646300170117     C********************************************************************
646400170117     c* Verifica se giorno festivo su calendario
646500170117     C********************************************************************
646600170117     c     Azcln_fes     BEGSR
646700170117     C                   Z-ADD     W12DAT        DSDAT8
646800170117     C                   Z-ADD     DSAAS         CLNANN
646900170117     C                   Z-ADD     DSMES         CLNMES
647000170117     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
647100170117     C     D98ERR        IFEQ      '0'                                          --- 1 -->
647200170117     C                   Z-ADD     DSGIO         G
647300170117     C     W12RES        IFEQ      *ZEROS
647400170117     C                   MOVEL     CLNMAT        GFEDS
647500170117     C                   ELSE
647600170117     C                   MOVEL     CLNPOM        GFEDS
647700170117     C                   END
647800170117     c
647900170117     c* se sto verificando i tempi partenza o trazione e
648000170117     c*  la data che sto controllando corrisponde alla data spedizione,
648100170117     c*  la considero sempre lavorativa
648200170117     c                   if        (wtc='P' or wtc='T') and w12dat=d98dfv
648300170117     c                   else
648400170117     c
648500170117     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
648600170117     C                   MOVEL     *BLANKS       WC1Y
648700170117     C                   MOVEL     GFE(G)        WC1Y              8
648800170117     C                   Z-ADD     1             I
648900170117     C     WC1Y          LOOKUP    C1Y(I)                                 99
649000170117     C     *IN99         IFEQ      '1'                                          --- 3 -->
649100170117     C     WIC           IFEQ      'I'
649200170117     C     I1Y(I)        ANDEQ     'S'
649300170117     C                   MOVEL     'S'           WSLITT
649400170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
649500170117     C                   END
649600170117     C     WIC           IFEQ      'C'
649700170117     C     E1Y(I)        ANDEQ     'S'
649800170117     C                   MOVEL     'S'           WSLITT
649900170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
650000170117     C                   END
650100170117     C     WIC           IFEQ      'X'
650200170117     C     E1Y(I)        ANDEQ     'S'
650300170117     C                   MOVEL     'S'           WSLITT
650400170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
650500170117     C                   END
650600170117     C                   ELSE                                                   <-- 3 -->
650700170117     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
650800170117     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
650900170117     C                   END                                                    <-- 3 ---
651000170117     C                   END                                                    <-- 2 ---
651100170117     C                   END                                                    <-- 1 ---
651200170117     c
651300170117     c                   endif
651400170117     c                   ENDSR
651500931201     C********************************************************************
651600931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
651700931201     C********************************************************************
651800931201     C     CHKTUC        BEGSR
651900931201     C*
652000931201     C     WIC           IFEQ      'I'                                          --- 1 -->
652100931201     C                   DO        TI            T                 3 0          --- 2 -->
652200931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
652300931201     C     WPOROU        ANDLE     TIF(T)
652400931201     C                   MOVEL     'S'           WSLITT
652500931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
652600931201     C                   LEAVE                                                  *VA A FINE DO
652700931201     C                   ENDIF                                                  <-- 3 ---
652800931201     C                   ENDDO                                                  <-- 2 ---
652900931201     C                   ELSE                                                   <-- 1 -->
653000931201     C                   DO        TC            T                 3 0          --- 2 -->
653100931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
653200931201     C     WPOROU        ANDLE     TCF(T)
653300931201     C                   MOVEL     'S'           WSLITT
653400931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
653500931201     C                   LEAVE                                                  *VA A FINE DO
653600931201     C                   ENDIF                                                  <-- 3 ---
653700931201     C                   ENDDO                                                  <-- 2 ---
653800931201     C                   ENDIF                                                  <-- 1 ---
653900931201     C*
654000931201     C                   ENDSR
654100060418     C********************************************************************
654200060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
654300060418     c*          - eventi particolari che giustificano
654400060418     C********************************************************************
654500060418     C     CHKCEPG       BEGSR
654600060418     C*
654700060418     C     WIC           IFEQ      'I'                                          --- 1 -->
654800060420     C                   DO        PGI           p                 3 0          --- 2 -->
654900060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
655000060420     C     WPOROU        ANDLE     PGIF(p)
655100060418     C                   MOVEL     'S'           WSLITT
655200060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
655300060418     C                   LEAVE                                                  *VA A FINE DO
655400060418     C                   ENDIF                                                  <-- 3 ---
655500060418     C                   ENDDO                                                  <-- 2 ---
655600060418     C                   ELSE                                                   <-- 1 -->
655700060420     C                   DO        PGC           p                 3 0          --- 2 -->
655800060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
655900060420     C     WPOROU        ANDLE     PGCF(p)
656000060418     C                   MOVEL     'S'           WSLITT
656100060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
656200060418     C                   LEAVE                                                  *VA A FINE DO
656300060418     C                   ENDIF                                                  <-- 3 ---
656400060418     C                   ENDDO                                                  <-- 2 ---
656500060418     C                   ENDIF                                                  <-- 1 ---
656600060418     C*
656700060418     C                   ENDSR
656800930512     C********************************************************************
656900931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
657000930512     C********************************************************************
657100931130     C     DATTUC        BEGSR
657200931214     C*
657300931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
657400931214     C                   Z-ADD     12            WNOADD            9 0
657500930325     C*
657600931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
657700931125     C     D98GC2        ORNE      *BLANKS
657800931130     C*
657900931213     C                   Z-ADD     WPOROU        WORE
658000931130     C                   EXSR      OREDAT                                       *ORE->DATA
658100931130     C                   Z-ADD     WDAT          W12DAT
658200931130     C                   Z-ADD     WRES          W12RES
658300930405     C*
658400941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
658500930405     C* DELLA SETTIMANA
658600941227     C                   Z-ADD     W12DAT        DSDAT8
658700941227     C                   Z-ADD     DSAAS         CLNANN
658800941227     C                   Z-ADD     DSMES         CLNMES
658900941227     C                   Z-ADD     *ZEROS        CLNTFP
659000941227     C                   Z-ADD     *ZEROS        CLNTFA
659100930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
659200930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
659300941227     C                   MOVEL     CLNMAT        GSEDS
659400960129     C                   Z-ADD     DSGIO         GI                6 0
659500960129     C                   MOVEL     GSE(GI)       WGSE              1
659600140911     c
659700140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
659800140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
659900140911     c                   if        wtuctut='S'
660000140911     c                   eval      wtuctut='P'
660100140911     c                   endif
660200930326     C*
660300931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
660400931213     C                   DO        4             IT                3 0          --- 3 -->
660500931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
660600931213     C                   MOVEL     TUC(IT)       W93GSE            1
660700931213     C                   MOVE      TUC(IT)       W93GMP            1
660800930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
660900930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
661000931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
661100930512     C     W93GSE        OREQ      ' '
661200140911     c     WTUCTUT       andeq     ' '
661300140911     c
661400930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
661500931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
661600930401     C     W93GMP        OREQ      ' '
661700930325     C                   MOVEL     'S'           WSLITT
661800931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
661900931213     C                   ELSE                                                   <-- 6 -->
662000001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
662100001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
662200001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
662300050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
662400050908     C     Usacons       IFNE      'S'                                          --- 7 -->
662500141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
662600141107     c*    orari giorno per giorno
662700141107     c                   if        OOR2FCSGL<>' '
662800141108     c                             and w12dat<>sav_12dat
662900141107     c                   eval      or_datarif=w12dat
663000141107     c                   eval      or_DDC    =w12dat
663100141107     c                   exsr      Reper_tempi
663200141108     c                   eval      sav_12dat=w12dat
663300141107     c                   endif
663400141107     c
663500930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
663600930325     C     W93GMP        IFEQ      'M'
663700930326     C     W12RES        ANDEQ     *ZEROS
663800141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
663900141111     c                             or OOR2STfS =0
664000141105     c                             or (tuc(IT)<>gc1part and
664100170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
664200170207     c
664300140825     C                   MOVEL     'S'           WSLITT
664400140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
664500140911     c* slitto solo una volta  se chiuso tutti " M"
664600140911     c                   if        W93GSE=' '
664700140911     c                   eval      wTUCTUT='S'
664800140911     c                   endif
664900140911     c
665000140825     c                   endif
665100930325     C                   END
665200930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
665300930325     C     W93GMP        IFEQ      'P'
665400930326     C     W12RES        ANDNE     *ZEROS
665500140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
665600140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
665700141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
665800141111     c                             or OOR2STiS =0
665900141105     c                             or (tuc(IT)<>gc1part and
666000170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
666100930325     C                   MOVEL     'S'           WSLITT
666200140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
666300140911     c* slitto solo una volta  se chiuso tutti " P"
666400140911     c                   if        W93GSE=' '
666500140911     c                   eval      wTUCTUT='S'
666600140911     c                   endif
666700140825     c                   endif
666800140825     c
666900931213     C                   ENDIF
667000001128     C                   ENDIF                                                  <-- 7 ---
667100931213     C                   ENDIF                                                  <-- 6 ---
667200931213     C                   ENDIF                                                  <-- 5 ---
667300931213     C                   ENDIF                                                  <-- 4 ---
667400931213     C                   ENDDO                                                  <-- 3 ---
667500930405     C*
667600931213     C                   ENDIF                                                  <-- 2 ---
667700931213     C                   ENDIF                                                  <-- 1 ---
667800140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
667900140911     c                   if        wtuctut='P'  and wslitt='N'
668000140911     c                   clear                   wtuctut
668100140911     c                   endif
668200140911     c
668300930325     C                   ENDSR
668400060406     C********************************************************************
668500060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
668600060406     C********************************************************************
668700060406     C     DATCEPG       BEGSR
668800060406     C*
668900060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
669000060406     C                   Z-ADD     12            WNOADD            9 0
669100060406     C*
669200060406    1C                   if        wevpag='S'
669300060406     C*
669400060406     C                   Z-ADD     WPOROU        WORE
669500060406     C                   EXSR      OREDAT                                       *ORE->DATA
669600060406     C                   Z-ADD     WDAT          W12DAT
669700060406     C                   Z-ADD     WRES          W12RES
669800060418     c                   z-add     1             P
669900060406     c* Verifico se nella data esiste un evento Part che giustifica
670000060418     c     w12dat        lookup    pvd(P)                                 30
670100060418    2c                   dow       *in30 and pvf(P)<>'G'
670200060418     c                   add       1             P
670300060420     c     w12dat        lookup    pvd(P)                                 30
670400060406    2c                   enddo
670500060406     c
670600060406     c* 30 on --> TROVATO !!
670700060406    2c                   if        *in30
670800060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
670900060406     C                   MOVEL     'S'           WSLITT
671000060406     c                   z-add     24            wnoadd
671100060406    3C                   ENDIF
671200060406     C*
671300060406    2C                   ENDIF                                                  <-- 2 ---
671400060406    1C                   ENDIF                                                  <-- 1 ---
671500060406     C*
671600060406     C                   ENDSR
671700931210     C********************************************************************
671800931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
671900931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
672000931210     C********************************************************************
672100050805     C***  CHKDCR        BEGSR
672200931210     C*
672300050805     C***  WPOCRI        IFNE      *ZEROS
672400050805     C***  WPOROU        IFGE      WPOCRI
672500050805     C***  WPOROU        ANDLT     WPOCRF
672600050805     C***                ELSE
672700050805     C***                MOVEL     'S'           WSLITT
672800050805     C***                END
672900050805     C***                END
673000931210     C*
673100050805     C***                ENDSR
673200140822     C********************************************************************
673300140822     C*  Reperimento tempi di consegna per decidere se slittare
673400140822     C********************************************************************
673500140822     C     Reper_tempi   BEGSR
673600140822
673700140822     c                   clear                   trulorSds
673800140825     c                   clear                   trulor2ds
673900140822     c                   eval       IOREtfp = d98tfp
674000140822     c                   eval       IOREtfa = d98tfa
674100140825     c                   eval       IOREdti = d98dti
674200140825     c                   eval       IOREhti = d98hti
674300140825     c                   eval       IOREdcr = s98dcr
674400140825     c                   eval       IOREhcr = s98hcr
674500140825     c                   eval       IOREtcr = s98tcr
674600140825     c                   eval       IOREDEI = d98dei
674700140825     c                   eval       IOREOEI = d98oei
674800140825     c                   eval       IOREfil = d98lna
674900140825     c                   eval       IOREcap = d98cad
675000140825     c                   eval       IOREloc = d98lod
675100140825     c                   eval       IOREnar = d98nzd
675200140825     c
675300140825     c* Data di Riferimento:
675400140825     c* Se impostata la consegnabilità --> prendo questa
675500140822     c                   select
675600140825     c                   when      d98dfc>0
675700141107     c                   if        or_datarif<d98dfc and or_datarif>0
675800141107     c                   eval      ioredta=or_datarif
675900141107     c                   else
676000141107     c                   eval      ioredta=d98dfc
676100141107     c                   endif
676200140825     c* altrimenti data di consegna
676300140825     c                   when      d98dcm>0
676400141107     c                   if        or_datarif<d98dcm and or_datarif>0
676500141107     c                   eval      ioredta=or_datarif
676600141107     c                   else
676700140825     c                   eval      ioredta=d98dcm
676800141107     c                   endif
676900140825     c
677000140822     c                   other
677100140825     c* altrimenti data teorica consegna se > = oggi
677200140825     C                   time                    wrkoggiiso
677300140825     C     *iso          move      wrkoggiiso    ioredta
677400140822     c* se < di oggi, imposto oggi
677500140825     c                   if        d98dei>ioredta
677600140825     c                   eval      ioredta=d98dei
677700140822     c                   endif
677800141107     c
677900141107     c                   if        or_datarif<ioredta and or_datarif>0
678000141107     c                   eval      ioredta=or_datarif
678100141107     c                   endif
678200141107     c
678300140822     c                   endsl
678400140822     c
678500140825     c                   eval       IOREtser = 'C'
678600141107     c                   if         or_ddc>0 and or_ddc<d98ddc
678700141107     c                   eval       IOREDDC=or_ddc
678800141107     c                   eval       IOREnDC=8888888
678900141107     c                   eval       IOREcons = 'S'
679000141107     c                   else
679100140825     c                   eval       IOREDDC=d98ddc
679200140825     c                   eval       IOREnDC=wd98ndc
679300140825     c                   eval       IOREcons = d98cons
679400141107     c                   endif
679500141017     c                   eval       IOREtsp = d98tsp
679600140822     c
679700140822     c                   call      'TRULORSR'
679800140822     c                   parm                    kpjba
679900140822     c                   parm                    trulorSds
680000140825     c                   parm                    trulor2ds
680100140825     c
680200140825     c                   eval      wtrulor= 'S'
680300141107     c                   clear                   or_datarif        8 0
680400141107     c                   clear                   or_ddc            8 0
680500140822     C                   ENDSR
680600950406     C********************************************************************
680700950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
680800950406     C********************************************************************
680900950406     C     DATORE        BEGSR
681000950406     C*
681100950406     C                   Z-ADD     *ZEROS        WORE
681200950406     C     WDAT          IFNE      0
681300950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
681400950406     C     WAA           DIV       4             WQUA              4 0
681500950406     C                   MVR                     WRES
681600950406     C     WAA           MULT      365           WGIO              9 0
681700950406     C                   ADD       WQUA          WGIO
681800950406     C                   ADD       WGG           WGIO
681900950406     C     WMM           SUB       1             MC                3 0
682000950406     C                   DO        MC            WX                3 0
682100950406     C                   ADD       MES(WX)       WGIO
682200950406     C                   END
682300950406     C     WRES          IFEQ      0
682400950406     C     WMM           ANDLE     2
682500950406     C                   SUB       1             WGIO
682600950406     C                   END
682700950406     C     WGIO          MULT      24            WORE              9 0
682800950406     C                   ENDIF
682900950406     C*
683000950406     C                   ENDSR
683100930323     C********************************************************************
683200930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
683300930323     C********************************************************************
683400930323     C     OREDAT        BEGSR
683500930323     C*
683600950407     C                   SETOFF                                       717273
683700950407     C                   SETOFF                                       7475
683800930323     C     WORE          DIV       24            WGIO
683900930323     C                   MVR                     WRES              3 0
684000950406     C*
684100950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
684200950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
684300950406     C*  TROVO ANNO
684400950406     C     WGIO          DIV       365           WAA
684500950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
684600950406     C                   MVR                     WREST                71
684700950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
684800950406     C     WGIO          DIV       365           WAA
684900950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
685000950406     C*                                                    0 = 31/12/PR
685100950406     C   75
685200950406     CAN 71              DO                                                     BIS. E NON ERA
685300950406     C     WAA           SUB       1             WAA
685400950406     C                   Z-ADD     365           WREST
685500950406     C                   END
685600950406     C*
685700950406     C     WAA           DIV       4             WNBIS                                      T.
685800950406     C                   MVR                     WRBIS             3 073
685900950406     C   73
686000950406     CANN71                                                                     NON PIU' BISEST
686100950406     CORN73              ADD       1             WREST                          E'BISESTILE
686200950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
686300950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
686400950406     C  N73              SUB       1             $PM(1)
686500950406     C                   Z-ADD     1             WI                2 0
686600950406     C* TROVO IL MESE E GIORNI
686700950406     C     WREST         LOOKUP    $PM(WI)                            74  73
686800950406     C                   MOVE      WI            WMM
686900950406     C     WI            COMP      1                                      72
687000950406     C  N72
687100950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
687200950406     C   74              SUB       1             WI
687300950406     C  N72
687400950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
687500950406     C   72              MOVE      WREST         WGG
687600950406     C                   ADD       1900          WAA
687700950406     C*
687800930323     C*
687900930323     C                   ENDSR
6880009407151507 C********************************************************************
6881009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6882009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6883009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6884009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6885009407151507 C********************************************************************
6886009407151507 C     FINGIO        BEGSR
6887009407151507 C*
6888009407151507 C     WPORE         DIV       24            WGIO
6889009407151507 C                   MVR                     WRES
6890009407151507 C     WRES          IFEQ      *ZEROS
6891009407151507 C                   Z-ADD     12            WNORE             3 0
6892009407151507 C                   ELSE
6893009407151507 C                   Z-ADD     *ZEROS        WNORE
6894009407151507 C                   ENDIF
6895009407151507 C*
6896009407151507 C                   ENDSR
689700930517     C********************************************************************
689800971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
689900930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
690000930517     C********************************************************************
690100930517     C     CERCAL        BEGSR
690200930517     C*
690300930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
690400941227     C                   MOVE      CLNTFP        DCATFP
690500941227     C                   MOVE      CLNTFA        DCATFA
690600941227     C                   MOVE      CLNANN        DCAANN
690700941227     C                   MOVE      CLNMES        DCAMES
690800930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
690900930517     C                   Z-ADD     1             I
691000930517     C     DCALEN        LOOKUP    CAK(I)                                 99
691100930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
691200930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
691300941227     C     KEYCLN        CHAIN     AZCLN01L                           99
691400930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
691500930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
691600930517     C                   MOVEL     '1'           D98ERR
691700930517     C                   MOVEL     MSG(3)        D98MSG
691800930517     C                   ELSE                                                   <-- 2 -->
691900970401     C* CONTROLLA SE ANNULLATO
692000970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
692100970401     C                   MOVEL     '1'           D98ERR
692200970401     C                   MOVEL     MSG(3)        D98MSG
692300970401     C                   ELSE                                                   <-- 3 -->
692400930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
692500930517     C                   ADD       1             WINDCA            6 0
692600970401     C     WINDCA        IFLE      200                                          --- 4 -->
692700970401     C     WINDCA        ANDGT     *ZEROS
692800930517     C                   Z-ADD     WINDCA        I
692900930517     C                   MOVEL     DCALEN        CAK(I)
693000941227     C                   MOVEL     CLNMAT        CAM(I)
693100941227     C                   MOVEL     CLNPOM        CAP(I)
693200970401     C                   END                                                    <-- 4 ---
693300970401     C                   ENDIF                                                  <-- 3 ---
693400930517     C                   END                                                    <-- 2 ---
693500930517     C                   ELSE                                                   <-- 1 -->
693600930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
693700941227     C                   MOVEL     CAM(I)        CLNMAT
693800941227     C                   MOVEL     CAP(I)        CLNPOM
693900930517     C                   END                                                    <-- 1 ---
694000930517     C*
694100930517     C                   ENDSR
694200010821     C********************************************************************
694300010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
694400010821     C********************************************************************
694500010821     C     CHKVCA        BEGSR
694600010821     C*
694700010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
694800010821     C     D98CAS        IFGT      *ZEROS
694900010821     C     D98VCA        ANDEQ     *BLANKS
695000010821     C                   MOVEL     'ITL'         D98VCA
695100010821     C                   ENDIF
695200010822     C*
695300010822     C     D98VCA        IFNE      'ITL'
695400010822     C     D98VCA        ANDNE     'EUR'
695500010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
695600010822     C                   Z-ADD     D98CAS        INPIMP
695700010822     C                   MOVEL     D98VCA        INPDIV
695800010822     C                   EXSR      EXECNV
695900010822     C                   Z-ADD     OUTIMP        D98CAS
696000010822     C                   MOVEL     §GEDCN        D98VCA
696100010822     C                   ENDIF
696200010821     C*
696300010821     C                   ENDSR
696400010821     C********************************************************************
696500010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
696600010821     C********************************************************************
696700010821     C     EXECNV        BEGSR
696800010821     C*
696900010821     C                   CLEAR                   YEURDS
697000010821     C*
697100010821     C                   MOVEL     INPDIV        YECDVI
697200010821     C                   Z-ADD     INPIMP        YECIMI
697300010821     C                   MOVEL     §GEDCN        YECDVO
697400010821     C                   MOVEL     '3'           YECDCO
697500010821     C                   MOVEL     'H'           YECTAR
697600010821     C                   CALL      'YEURCO'
697700010821     C                   PARM                    YEURDS
697800010821     C*
697900010821     C                   Z-ADD     YECIMO        OUTIMP
698000010821     C*
698100010821     C                   ENDSR
698200011211     C*------------------------------------------------------------------------*
698300011211     C* Controllo importo massimo contrassegno.
698400011211     C*------------------------------------------------------------------------*
698500011211     C     REPMAXCSB     BEGSR
698600011211     C*
698700011211     C                   CLEAR                   Trul21DS
698800011211     C*
698900160420     c                   eval      i21cbo = d98cbo
699000011211     C                   EVAL      I21TSp = d98TSP
699100011211     C                   EVAL      I21NzD = d98NZD
699200011211     C                   EVAL      I21LnP = d98LNP
699300011211     C                   EVAL      I21LnA = d98LNA
699400011211     C                   EVAL      I21Ksc = d98KSC
699500011211     C                   EVAL      I21TiC = d98TIC
699600011211     C                   EVAL      I21Imp = d98CAS
699700011211     C                   EVAL      I21Div = d98VCA
699800160420     c                   eval      i21cbo = d98cbo
699900011211     C*
700000011211     C                   CALL      'TRUL21R'
700100011211     C                   PARM                    Trul21DS
700200160420
700300160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
700400160420     c                   if        wtrasm='N'
700500160420     c                   if        o21fca <>*blanks
700600160420     c                   eval      §b4ltca = 'S'
700700160420     c                   endif
700800160420     c                   endif
700900011211     C*
701000011211     C                   ENDSR
701100011211     C*------------------------------------------------------------------------*
701200011211
701300930414     C********************************************************************
701400930414     C*   RUTINZ - ROUTINE INIZIALE
701500930414     C********************************************************************
701600930414     C     RUTINZ        BEGSR
701700930414     C*
701800930414     C* PARAMETRI IN INPUT ALLA ROUTINE
701900930414     C     *ENTRY        PLIST
702000970731     C                   PARM                    DS99
702100941227     C*
702200941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
702300941227     C     KEYBLP        KLIST
702400941227     C                   KFLD                    BLPAAS
702500941227     C                   KFLD                    BLPLNP
702600941227     C                   KFLD                    BLPNRS
702700941227     C                   KFLD                    BLPNSP
702800050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
702900941227     C     KEYGCP        KLIST
703000050405     C                   KFLD                    KWGCPAAS
703100050405     C                   KFLD                    KWGCPLNP
703200050405     C                   KFLD                    KWGCPNRS
703300050405     C                   KFLD                    KWGCPNSP
703400050405     C                   KFLD                    KWGCPFRG
703500050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
703600031125     C     KEYGC_P       KLIST
703700050405     C                   KFLD                    KWGCPAAS
703800050405     C                   KFLD                    KWGCPLNP
703900050405     C                   KFLD                    KWGCPNRS
704000050405     C                   KFLD                    KWGCPNSP
704100970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
704200150120     C**   KEYBLT        KLIST
704300150120     C**                 KFLD                    BLTAAS
704400150120     C**                 KFLD                    BLTLNP
704500150120     C**                 KFLD                    BLTNRS
704600150120     C**                 KFLD                    BLTNSP
704700941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
704800941227     C     KEYARB        KLIST
704900941227     C                   KFLD                    ARBAAS
705000941227     C                   KFLD                    ARBLNP
705100941227     C                   KFLD                    ARBNRS
705200941227     C                   KFLD                    ARBNSP
705300051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
705400941228     C     KEYAR9        KLIST
705500941228     C                   KFLD                    AR9AAS
705600941228     C                   KFLD                    AR9LNP
705700941228     C                   KFLD                    AR9NRS
705800941228     C                   KFLD                    AR9NSP
705900941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
706000050405     C     KEYGCP21      KLIST
706100050405     C                   KFLD                    KWGCPAAS
706200050405     C                   KFLD                    KWGCPLNP
706300050405     C                   KFLD                    KWGCPNRS
706400050405     C                   KFLD                    KWGCPNSP
706500050405     C                   KFLD                    KWGCPFRG
706600970912     C* CHIAVE LETTURA FNART01L - PARZIALE
706700150120     C***  KEYART        KLIST
706800150120     C***                KFLD                    ARTAAS
706900150120     C***                KFLD                    ARTLNP
707000150120     C***                KFLD                    ARTNRS
707100150120     C***                KFLD                    ARTNSP
707200950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
707300950102     C     KEYLBL        KLIST
707400950102     C                   KFLD                    LBLAAN
707500950102     C                   KFLD                    LBLLPN
707600950102     C                   KFLD                    LBLNRN
707700950102     C                   KFLD                    LBLNSN
707800930414     C*
707900070827     c     kAR5          Klist
708000070827     c                   Kfld                    d98Aas
708100070827     c                   Kfld                    d98Lnp
708200070827     c                   Kfld                    d98nrs
708300070827     c                   Kfld                    d98nsp
708400070827     c                   Kfld                    kAr5Trd
708500150325     c     kAR4          Klist
708600150325     c                   Kfld                    d98Aas
708700150325     c                   Kfld                    d98Lnp
708800150325     c                   Kfld                    d98nrs
708900150325     c                   Kfld                    d98nsp
709000150325     c                   Kfld                    kAr4Trc
709100150218     c*
709200150218     c     kart          Klist
709300150218     c                   Kfld                    d98Aas
709400150218     c                   Kfld                    d98Lnp
709500150218     c                   Kfld                    d98nrs
709600150218     c                   Kfld                    d98nsp
709700150218     c     kbrv          Klist
709800150218     c                   Kfld                    artfls
709900150218     c                   Kfld                    artlna
710000150218     c                   Kfld                    artnrs
710100150218     c                   Kfld                    artnsc
710200150325     C     Kfapd         klist
710300150325     C                   kfld                    kpdtip
710400150325     C                   kfld                    kpdpdr
710500930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
710600930414     C     KEYTAB        KLIST
710700930414     C                   KFLD                    KTBUTE
710800930414     C                   KFLD                    KTBCOD
710900930414     C                   KFLD                    KTBKEY
711000060406     C     Ktab          KLIST
711100060406     C                   KFLD                    KTBUTE
711200060406     C                   KFLD                    KTBCOD
711300930414     C*
711400941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
711500941227     C     KEYCLN        KLIST
711600941227     C                   KFLD                    CLNTFP
711700941227     C                   KFLD                    CLNTFA
711800941227     C                   KFLD                    CLNANN
711900941227     C                   KFLD                    CLNMES
712000941227     C*
712100941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
712200941227     C     KEYCAS        KLIST
712300941227     C                   KFLD                    CASPTA
712400941227     C                   KFLD                    CASLNP
712500941227     C                   KFLD                    CASLNA
712600941227     C                   KFLD                    CASTSP
712700941227     C                   KFLD                    CASDDE
712800930420     C*
712900990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
713000930420     C     KEYEVB        KLIST
713100930420     C                   KFLD                    KEBAAS
713200930420     C                   KFLD                    KEBLNP
713300930420     C                   KFLD                    KEBNRS
713400930420     C                   KFLD                    KEBNSP
713500010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
713600010821     C     KTBE          KLIST
713700010821     C                   KFLD                    TBECOD
713800010821     C                   KFLD                    TBEKE1
713900010821     C                   KFLD                    TBEKE2
714000171108     C*
714100171108     C     Kfgv02        KLIST
714200171108     C                   KFLD                    d98tfp
714300171108     C                   KFLD                    wdaodi
714400171108     C     Kfgw          KLIST
714500171108     C                   KFLD                    fgvnfv
714600171108     C                   KFLD                    fgvlnp
714700930414     C*
714800010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
714900930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
715000930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
715100930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
715200930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
715300930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
715400930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
715500930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
715600930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
715700930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
715800010821     C     *LIKE         DEFINE    D98CAS        INPIMP
715900010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
716000010821     C     *LIKE         DEFINE    D98VCA        INPDIV
716100930414     C*
716200930414     C* RECUPERO IL CODICE DELL'AZIENDA
716300930414     C                   Z-ADD     1             CODUT             1 0
716400970701     C                   Z-ADD     *ZEROS        I                 6 0
716500930414     C*
716600930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
716700930414     C                   EXSR      AZZERA
716800930414     C*
716900930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
717000941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
717100941227     C                   MOVEL     'S'           CTABSN            1
717200930414     C                   EXSR      CARTAB
717300941227     C                   ENDIF
717400041116     c* Se passato umero di spedizioni apro i files , altrimenti no
717500041116     c                   if        d98nsp>0 and wapri=' '
717600041116     c                   open      fnblp01l
717700050405     c                   open      tigcp01l
717800150120     c***                open      fnblt01l
717900041116     c                   open      fnarb01l
718000051115     c                   open      fiar901l
718100150325     c                   open      fiar401l
718200050405     c                   open      tigcp21l
718300150218     c                   open      fnart01l
718400150218     c                   open      fnbrv07l
718500041116     c                   open      fnlbl01l
718600041116     c                   open      fnevb01l
718700070827     c                   open      fiar501l
718800171112     c                   open      fnfgv02l
718900171112     c                   open      fnfgw01l
719000041116     c                   eval      wapri='S'
719100041116     c                   endif
719200930414     C*
719300930414     C                   ENDSR
719400941227     C*------------------------------------------------------------------------*
719500941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
719600941227     C*------------------------------------------------------------------------*
719700941227     C     SUBDAT        BEGSR
719800941227     C*
719900941227     C                   TIME                    DTTIME
720000941227     C                   Z-ADD     DTANN         DSANN
720100941227     C                   Z-ADD     DTMES         DSMES
720200941227     C                   Z-ADD     DTGIO         DSGIO
720300941227     C                   Z-ADD     DTSEC         DSSEC
720400941227     C*
720500941227     C                   ENDSR
720600930414     C********************************************************************
720700930414     C*   CARTAB - CARICA LE TABELLE
720800930414     C********************************************************************
720900930414     C     CARTAB        BEGSR
721000930414     C*
721100930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
721200930414     C     D98ERR        IFEQ      '0'
721300930421     C                   Z-ADD     0             I
721400930414     C                   Z-ADD     CODUT         KTBUTE
721500930414     C                   MOVEL     '1Y'          KTBCOD
721600930414     C                   MOVEL     *BLANKS       KTBKEY
721700930414     C     KEYTAB        SETLL     TABEL00F                           99
721800930414     C  N99              READ      TABEL00F                               99
721900930414     C     *IN99         IFEQ      '1'
722000930414     C                   MOVEL     '1'           D98ERR
722100930414     C                   MOVEL     MSG(5)        D98MSG
722200930414     C                   END
722300930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
722400930414     C     TBLCOD        ANDEQ     '1Y'
722500930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
722600930421     C                   ADD       1             I
722700930421     C                   MOVEL     TBLKEY        C1Y(I)
722800930414     C                   MOVEL     TBLUNI        DS1Y
722900930421     C                   MOVEL     §1YFEI        I1Y(I)
723000930421     C                   MOVEL     §1YFEC        E1Y(I)
723100930414     C                   END                                                    <-- 3 ---
723200930414     C                   READ      TABEL00F                               99
723300930414     C                   END                                                    <-- 2 ---
723400930414     C                   END
723500930414     C*
723600930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
723700930414     C     D98ERR        IFEQ      '0'
723800930421     C                   Z-ADD     0             I
723900930414     C                   Z-ADD     CODUT         KTBUTE
724000930414     C                   MOVEL     '1X'          KTBCOD
724100930414     C                   MOVEL     *BLANKS       KTBKEY
724200930414     C     KEYTAB        SETLL     TABEL00F                           99
724300930414     C  N99              READ      TABEL00F                               99
724400930414     C     *IN99         IFEQ      '1'
724500930414     C                   MOVEL     '1'           D98ERR
724600930414     C                   MOVEL     MSG(1)        D98MSG
724700930414     C                   END
724800930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
724900930414     C     TBLCOD        ANDEQ     '1X'
725000930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
725100930421     C                   ADD       1             I
725200930421     C                   MOVEL     TBLKEY        C1X(I)
725300930414     C                   MOVEL     TBLUNI        DS1X
725400930421     C                   Z-ADD     §1XHHG        G1X(I)
725500930421     C                   Z-ADD     §1XHHO        O1X(I)
725600930421     C                   Z-ADD     §1XHHM        M1X(I)
725700930421     C                   Z-ADD     §1XHHP        P1X(I)
725800930421     C                   Z-ADD     §1XHHI        I1X(I)
725900930421     C                   Z-ADD     §1XHHS        S1X(I)
726000930414     C                   END                                                    <-- 3 ---
726100930414     C                   READ      TABEL00F                               99
726200931223     C                   ENDDO                                                  <-- 2 ---
726300050523     c
726400050523     c* controllo riempimento skiere
726500050523     c                   clear                   trul0sds
726600050523     c                   eval      i0sski='C1X/G1X/O1X/...'
726700050523     c                   eval      i0sele=%elem(c1x)
726800050523     c                   eval      i0spie=i
726900050523     c                   eval      i0sfile='TABEL00F  '
727000050523     c                   eval      i0scod=' 1X  '
727100050523     c                   eval      i0spgm='TNSD99R'
727200050523     c                   movel     trul0sds      kpjbu
727300050523     c                   call      'TRUL0SR'
727400050523     c                   parm                    kpjba
727500050523     C                   END
727600930421     C*
727700930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
727800930421     C     D98ERR        IFEQ      '0'
727900930421     C                   Z-ADD     0             I
728000930421     C                   Z-ADD     CODUT         KTBUTE
728100930421     C                   MOVEL     '2A'          KTBCOD
728200060406     C     Ktab          SETLL     TABEL00F                           99
728300060406     C  N99ktab          READe     TABEL00F                               99
728400930421     C     *IN99         IFEQ      '1'
728500930421     C                   MOVEL     '1'           D98ERR
728600930421     C                   MOVEL     MSG(8)        D98MSG
728700930421     C                   END
728800930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
728900930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
729000930421     C                   ADD       1             I
729100930421     C                   MOVEL     TBLKEY        C2A(I)
729200060406     C                   MOVEL     TBLUNI        ds2a
729300060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
729400060406     C                   MOVEL     w003a         D2A(I)
729500930421     C                   END                                                    <-- 3 ---
729600060406     C     ktab          READe     TABEL00F                               99
729700931223     C                   ENDDO                                                  <-- 2 ---
729800050523     c* controllo riempimento skiere
729900050523     c                   clear                   trul0sds
730000050523     c                   eval      i0sski='C2A/D2A'
730100050523     c                   eval      i0sele=%elem(c2a)
730200050523     c                   eval      i0spie=i
730300050523     c                   eval      i0sfile='TABEL00F  '
730400060406     c                   eval      i0scod='2A/2Z'
730500050523     c                   eval      i0spgm='TNSD99R'
730600050523     c                   movel     trul0sds      kpjbu
730700050523     c                   call      'TRUL0SR'
730800050523     c                   parm                    kpjba
730900930421     C                   END
731000060406     c* Tabella  2Z
731100060406     C     D98ERR        IFEQ      '0'
731200060406     C                   Z-ADD     CODUT         KTBUTE
731300060406     C                   MOVEL     '2Z'          KTBCOD
731400060406     C     Ktab          SETLL     TABEL00F                           99
731500060406     C  N99ktab          READe     TABEL00F                               99
731600060406     C     *IN99         IFEQ      '1'
731700060406     C                   MOVEL     '1'           D98ERR
731800060406     C                   MOVEL     MSG(33)       D98MSG
731900060406     C                   END
732000060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
732100060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
732200060406     C                   Z-ADD     1             I
732300060406     C                   MOVEL     TBLKEY        w003a
732400060406     c     w003a         lookup    c2a(i)                                 99
732500060406     C     *IN99         IFEQ      *off
732600060406     C                   MOVEL     '1'           D98ERR
732700060406     C                   MOVEL     MSG(34)       D98MSG
732800060406     C                   Else
732900060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
733000170915     c                   eval      ds2z=tbluni
733100170915     c                   eval      %subst(d2z(i):6:1)=w2ztel
733200060406     C                   END                                                    <-- 3 ---
733300060406     C                   END                                                    <-- 3 ---
733400060406     C     ktab          READe     TABEL00F                               99
733500060406     C                   ENDDO                                                  <-- 2 ---
733600060406     C                   END
733700941227     C*
733800941227     C* CARICA TABELLA CODICI BOLLA -3A-
733900941227     C     D98ERR        IFEQ      '0'
734000941228     C                   Z-ADD     0             I
734100941227     C                   Z-ADD     CODUT         KTBUTE
734200941227     C                   MOVEL     '3A'          KTBCOD
734300941227     C                   MOVEL     *BLANKS       KTBKEY
734400941227     C     KEYTAB        SETLL     TABEL00F                           99
734500941227     C  N99              READ      TABEL00F                               99
734600941227     C     *IN99         IFEQ      '1'
734700941227     C                   MOVEL     '1'           D98ERR
734800941227     C                   MOVEL     MSG(22)       D98MSG
734900941227     C                   END
735000941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
735100941227     C     TBLCOD        ANDEQ     '3A'
735200941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
735300941227     C                   ADD       1             I
735400941227     C                   MOVEL     TBLKEY        C3A(I)
735500941227     C                   MOVEL     TBLUNI        DS3A
735600941227     C                   MOVEL     §3ATB1        T3A(I)
735700941228     C                   Z-ADD     §3AFCA        F3A(I)
735800990308     C                   MOVEL     §3ARBL        R3A(I)
735900941227     C                   END                                                    <-- 3 ---
736000941227     C                   READ      TABEL00F                               99
736100941227     C                   END                                                    <-- 2 ---
736200050523     c* controllo riempimento skiere
736300050523     c                   clear                   trul0sds
736400050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
736500050523     c                   eval      i0sele=%elem(c3a)
736600050523     c                   eval      i0spie=i
736700050523     c                   eval      i0sfile='TABEL00F  '
736800050523     c                   eval      i0scod=' 3A  '
736900050523     c                   eval      i0spgm='TNSD99R'
737000050523     c                   movel     trul0sds      kpjbu
737100050523     c                   call      'TRUL0SR'
737200050523     c                   parm                    kpjba
737300941227     C                   END
737400971210     C*
737500020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
737600020226     C                   Z-ADD     *ZEROS        J                 3 0
737700160331     C                   Z-ADD     *ZEROS        D                 3 0
737800971210IF  0C     D98ERR        IFEQ      '0'
737900971210     C     *LOVAL        SETLL     AZORG01L
738000971210     C                   READ      AZORG01L                               98
738100971210DO  1C     *IN98         DOWEQ     *OFF
738200971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
738300971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
738400971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
738500020627     C                   MOVEL     ORGDE3        OG143
738600020702     C*
738700020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
738800020702     C                   CLEAR                   DNTW
738900020702     C*
739000020702     C                   MOVEL(P)  'NTW'         TBECOD
739100020702     C                   MOVEL(P)  §OGNTW        TBEKE1
739200020702     C     KTBE          CHAIN     TNTBE01L
739300020702     C                   IF        %found(TNTBE01L)
739400020702     C                   MOVEL     TBEUNI        DNTW
739500020702     C                   ENDIF
739600020702     C*
739700020702     C                   MOVEL     §NTWFIE       FL1(F)
739800020627     C*
739900020226     C* VERIFICO SE FILIALE FEDEX
740000020226     C                   IF        §OGNTW = 'FED'
740100020226     C                   ADD       1             J
740200020226     C                   Z-ADD     ORGFIL        FILFED(J)
740300020226     C                   ENDIF
740400160331     C* VERIFICO SE FILIALE DPD
740500160331     C                   IF        §OGNTW = 'DPD'
740600160331     C                   ADD       1             D
740700160331     C                   Z-ADD     ORGFIL        FILDPD(D)
740800160331     C                   ENDIF
740900020226     C*
741000971210E   2C                   ENDIF
741100971210     C                   READ      AZORG01L                               98
741200971210E   1C                   ENDDO
741300971210E   0C                   ENDIF
741400010821     C*
741500010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
741600010821     C                   MOVEL     'GED'         TBECOD
741700070514     C                   MOVEL     *BLANKS       TBEKE1
741800070514     C                   MOVEL     '1'           TBEKE1
741900010821     C                   MOVEL     *BLANKS       TBEKE2
742000010821     C     KTBE          CHAIN     TNTBE01L                           21
742100070514     C  N21              MOVEL     TBEUNI        DGED
742200150326     c
742300150326     C* REPERISCO LA tabella flag operativi per tabella OLP
742400150326     c                   clear                   dvpo
742500150326     C                   MOVEL     'VPO'         TBECOD
742600150326     C                   MOVEL     *BLANKS       TBEKE2
742700150326     C                   MOVEL(P)  'VPO'         TBEKE1
742800150326     C     KTBE          CHAIN     TNTBE01L                           21
742900150326     C  N21              MOVEL     TBEUNI        DVPO
743000150326     C*
743100031201     C*
743200031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
743300031201     C     D98ERR        IFEQ      '0'
743400060403     C                   Z-ADD     0             I7QW              3 0
743500031201     C                   Z-ADD     CODUT         KTBUTE
743600031201     C                   MOVEL     '7Q'          KTBCOD
743700031201     C                   MOVEL     *BLANKS       KTBKEY
743800031201     C     KEYTAB        SETLL     TABEL00F                           99
743900031201     C  N99              READ      TABEL00F                               99
744000031201     C     *IN99         IFEQ      '1'
744100031201     C                   MOVEL     '1'           D98ERR
744200031201     C                   MOVEL     MSG(8)        D98MSG
744300031201     C                   ENDIF
744400031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
744500031201     C     TBLCOD        ANDEQ     '7Q'
744600031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
744700031201     C                   MOVEL     TBLUNI        DS7Q
744800031201     C                   IF        §7Q4D1 = 'R' OR
744900031201     C                             §7Q4D2 = 'R' OR
745000031201     C                             §7Q4D3 = 'R' OR
745100031201     C                             §7Q4D4 = 'R' OR
745200031201     C                             §7Q4D5 = 'R'
745300031201     C                   ADD       1             I7QW
745400031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
745500031201     C                   ENDIF                                                  <-- 3 ---
745600031201     C                   ENDIF                                                  <-- 3 ---
745700031201     C                   READ      TABEL00F                               99
745800031201     C                   ENDDO                                                  <-- 2 ---
745900031201     C                   ENDIF
746000031201     C*
746100031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
746200031201     C     D98ERR        IFEQ      '0'
746300031201     C                   Z-ADD     CODUT         KTBUTE
746400031201     C                   MOVEL     '7W'          KTBCOD
746500031201     C                   MOVEL     *BLANKS       KTBKEY
746600031201     C     KEYTAB        SETLL     TABEL00F                           99
746700031201     C  N99              READ      TABEL00F                               99
746800031201     C     *IN99         IFEQ      '1'
746900031201     C                   MOVEL     '1'           D98ERR
747000031201     C                   MOVEL     MSG(8)        D98MSG
747100031201     C                   ENDIF
747200031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
747300031201     C     TBLCOD        ANDEQ     '7W'
747400031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
747500031201     C                   MOVEL     TBLUNI        DS7W
747600031201     C                   IF        §7W6D1 = 'C' OR
747700031201     C                             §7W6D2 = 'C' OR
747800031201     C                             §7W6D3 = 'C' OR
747900031201     C                             §7W6D4 = 'C' OR
748000031201     C                             §7W6D5 = 'C'
748100031201     C                   ADD       1             I7QW
748200031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
748300031201     C                   ENDIF                                                  <-- 3 ---
748400031201     C                   ENDIF                                                  <-- 3 ---
748500031201     C                   READ      TABEL00F                               99
748600031201     C                   ENDDO                                                  <-- 2 ---
748700050523     c* controllo riempimento skiere
748800050523     c                   clear                   trul0sds
748900050523     c                   eval      i0sski='SK7QW'
749000050523     c                   eval      i0sele=%elem(sk7qw)
749100050523     c                   eval      i0spie=i7qw
749200050523     c                   eval      i0sfile='TABEL00F  '
749300050523     c                   eval      i0scod='7Q-7W'
749400050523     c                   eval      i0spgm='TNSD99R'
749500050523     c                   movel     trul0sds      kpjbu
749600050523     c                   call      'TRUL0SR'
749700050523     c                   parm                    kpjba
749800031201     C                   ENDIF
749900930414     C*
750000930414     C                   ENDSR
750100930414     C********************************************************************
750200930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
750300930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
750400930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
750500930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
750600930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
750700930414     C********************************************************************
750800930414     C     RO3ORE        BEGSR
750900930414     C*
751000930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
751100930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
751200930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
751300050902     c                   movel     ' '           WR3MATPOM         1
751400931209     C                   Z-ADD     *ZEROS        WR3HHO
751500931209     C                   Z-ADD     *ZEROS        WR3HHA
751600930414     C                   MOVE      WR3TAB        WR3COD
751700930604     C                   MOVEL     D98LNA        WR3COD            8
751800930421     C                   Z-ADD     1             I
751900930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
752000930414     C     *IN99         IFEQ      '0'
752100930414     C                   MOVE      WR3TAB        WR3COD
752200930414     C                   MOVEL     '   '         WR3COD            8
752300930421     C                   Z-ADD     1             I
752400930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
752500930414     C                   END
752600930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
752700930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
752800930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
752900930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
753000930414     C                   ELSE                                                   <-- 2 -->
753100930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
753200930421     C                   Z-ADD     M1X(I)        WR3HHO
753300930510     C                   Z-ADD     I1X(I)        WR3HHA
753400050902     c                   movel     'M'           WR3MATPOM         1
753500930414     C                   END
753600930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
753700930421     C                   Z-ADD     P1X(I)        WR3HHO
753800930510     C                   Z-ADD     S1X(I)        WR3HHA
753900050902     c                   movel     'P'           WR3MATPOM
754000930414     C                   END
754100930414     C                   END                                                    <-- 2 ---
754200930414     C                   END                                                    <-- 1 ---
754300930414     C*
754400930414     C                   ENDSR
754500930423     C********************************************************************
754600930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
754700930423     C********************************************************************
754800930423     C     W98D98        BEGSR
754900930423     C*
755000941228     C*
755100941228     C* ORE REALI PARTENZA INTERNE
755200941228     C                   Z-ADD     W98RPI        W98               3 0
755300941228     C                   EXSR      W98999
755400941228     C                   Z-ADD     D98           D98RPI
755500941228     C* ORE REALI PARTENZA CLIENTE
755600941228     C                   Z-ADD     W98RPC        W98
755700941228     C                   EXSR      W98999
755800941228     C                   Z-ADD     D98           D98RPC
755900930423     C* ORE REALI TRAZIONE INTERNE
756000941228     C                   Z-ADD     W98RTI        W98
756100941228     C                   EXSR      W98999
756200941228     C                   Z-ADD     D98           D98RTI
756300930423     C* ORE REALI TRAZIONE CLIENTE
756400941228     C                   Z-ADD     W98RTC        W98
756500941228     C                   EXSR      W98999
756600941228     C                   Z-ADD     D98           D98RTC
756700930423     C* ORE REALI CONSEGNA INTERNE
756800941228     C                   Z-ADD     W98RCI        W98
756900941228     C                   EXSR      W98999
757000941228     C                   Z-ADD     D98           D98RCI
757100930423     C* ORE REALI CONSEGNA CLIENTE
757200941228     C                   Z-ADD     W98RCC        W98
757300941228     C                   EXSR      W98999
757400941228     C                   Z-ADD     D98           D98RCC
757500930423     C* ORE REALI RESA INTERNE
757600941228     C                   Z-ADD     W98RRI        W98
757700941228     C                   EXSR      W98999
757800941228     C                   Z-ADD     D98           D98RRI
757900930423     C* ORE REALI RESA CLIENTE
758000941228     C                   Z-ADD     W98RRC        W98
758100941228     C                   EXSR      W98999
758200941228     C                   Z-ADD     D98           D98RRC
758300941228     C* ORE DIFF. PARTENZA INTERNE
758400941228     C                   Z-ADD     W98FPI        W98
758500941228     C                   EXSR      W98999
758600941228     C                   Z-ADD     D98           D98FPI
758700941228     C* ORE DIFF. PARTENZA CLIENTE
758800941228     C                   Z-ADD     W98FPC        W98
758900941228     C                   EXSR      W98999
759000941228     C                   Z-ADD     D98           D98FPC
759100930423     C* ORE DIFF. TRAZIONE INTERNE
759200941228     C                   Z-ADD     W98FTI        W98
759300941228     C                   EXSR      W98999
759400941228     C                   Z-ADD     D98           D98FTI
759500930423     C* ORE DIFF. TRAZIONE CLIENTE
759600941228     C                   Z-ADD     W98FTC        W98
759700941228     C                   EXSR      W98999
759800941228     C                   Z-ADD     D98           D98FTC
759900930423     C* ORE DIFF. CONSEGNA INTERNE
760000941228     C                   Z-ADD     W98FCI        W98
760100941228     C                   EXSR      W98999
760200941228     C                   Z-ADD     D98           D98FCI
760300930423     C* ORE DIFF. CONSEGNA CLIENTE
760400941228     C                   Z-ADD     W98FCC        W98
760500941228     C                   EXSR      W98999
760600941228     C                   Z-ADD     D98           D98FCC
760700930423     C* ORE DIFF. RESA INTERNE
760800941228     C                   Z-ADD     W98FRI        W98
760900941228     C                   EXSR      W98999
761000941228     C                   Z-ADD     D98           D98FRI
761100930423     C* ORE DIFF. RESA CLIENTE
761200941228     C                   Z-ADD     W98FRC        W98
761300941228     C                   EXSR      W98999
761400941228     C                   Z-ADD     D98           D98FRC
761500941228     C* ORE NON CONF. PARTENZA INTERNE
761600941228     C     W98NPI        IFGT      999
761700941228     C                   Z-ADD     999           D98NPI
761800941228     C                   ELSE
761900941228     C                   Z-ADD     W98NPI        D98NPI
762000941228     C                   END
762100941228     C* ORE NON CONF. PARTENZA CLIENTE
762200941228     C     W98NPC        IFGT      999
762300941228     C                   Z-ADD     999           D98NPC
762400941228     C                   ELSE
762500941228     C                   Z-ADD     W98NPC        D98NPC
762600941228     C                   END
762700930423     C* ORE NON CONF. TRAZIONE INTERNE
762800930423     C     W98NTI        IFGT      999
762900930423     C                   Z-ADD     999           D98NTI
763000930423     C                   ELSE
763100930423     C                   Z-ADD     W98NTI        D98NTI
763200930423     C                   END
763300930423     C* ORE NON CONF. TRAZIONE CLIENTE
763400930423     C     W98NTC        IFGT      999
763500930423     C                   Z-ADD     999           D98NTC
763600930423     C                   ELSE
763700930423     C                   Z-ADD     W98NTC        D98NTC
763800930423     C                   END
763900930423     C* ORE NON CONF. CONSEGNA INTERNE
764000930423     C     W98NCI        IFGT      999
764100930423     C                   Z-ADD     999           D98NCI
764200930423     C                   ELSE
764300930423     C                   Z-ADD     W98NCI        D98NCI
764400930423     C                   END
764500930423     C* ORE NON CONF. CONSEGNA CLIENTE
764600930423     C     W98NCC        IFGT      999
764700930423     C                   Z-ADD     999           D98NCC
764800930423     C                   ELSE
764900930423     C                   Z-ADD     W98NCC        D98NCC
765000930423     C                   END
765100930423     C* ORE NON CONF. RESA INTERNE
765200930423     C     W98NRI        IFGT      999
765300930423     C                   Z-ADD     999           D98NRI
765400930423     C                   ELSE
765500930423     C                   Z-ADD     W98NRI        D98NRI
765600930423     C                   END
765700930423     C* ORE NON CONF. RESA CLIENTE
765800930423     C     W98NRC        IFGT      999
765900930423     C                   Z-ADD     999           D98NRC
766000930423     C                   ELSE
766100930423     C                   Z-ADD     W98NRC        D98NRC
766200930423     C                   END
766300930423     C* ORE X TURNO CHIUSURA INTERNE
766400941228     C                   Z-ADD     W98HHI        W98
766500941228     C                   EXSR      W98999
766600941228     C                   Z-ADD     D98           D98HHI
766700930423     C* ORE X TURNO CHIUSURA CLIENTE
766800941228     C                   Z-ADD     W98HHC        W98
766900941228     C                   EXSR      W98999
767000941228     C                   Z-ADD     D98           D98HHC
767100060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
767200060418     C                   Z-ADD     W98EGI        W98
767300060418     C                   EXSR      W98999
767400060418     C                   Z-ADD     D98           D98hEGI
767500060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
767600060418     C                   Z-ADD     W98EGC        W98
767700060418     C                   EXSR      W98999
767800060418     C                   Z-ADD     D98           D98hEGC
767900060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
768000060420     C                   Z-ADD     W98EAI        W98
768100060420     C                   EXSR      W98999
768200060420     C                   Z-ADD     D98           D98hEAI
768300060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
768400060420     C                   Z-ADD     W98EAC        W98
768500060420     C                   EXSR      W98999
768600060420     C                   Z-ADD     D98           D98hEAC
768700930423     C* ORE X LASC. AVVISO INTERNE
768800941228     C                   Z-ADD     W98HLI        W98
768900941228     C                   EXSR      W98999
769000941228     C                   Z-ADD     D98           D98HLI
769100930423     C* ORE X LASC. AVVISO CLIENTE
769200941228     C                   Z-ADD     W98HLC        W98
769300941228     C                   EXSR      W98999
769400941228     C                   Z-ADD     D98           D98HLC
769500930423     C* ORE X GIACENZA INTERNE
769600941228     C                   Z-ADD     W98HGI        W98
769700941228     C                   EXSR      W98999
769800941228     C                   Z-ADD     D98           D98HGI
769900930423     C* ORE X GIACENZA CLIENTE
770000941228     C                   Z-ADD     W98HGC        W98
770100941228     C                   EXSR      W98999
770200941228     C                   Z-ADD     D98           D98HGC
770300941228     C* ORE X FEST. PART. INTERNE
770400941228     C                   Z-ADD     W98HPI        W98
770500941228     C                   EXSR      W98999
770600941228     C                   Z-ADD     D98           D98HPI
770700941228     C* ORE X FEST. PART. CLIENTE
770800941228     C                   Z-ADD     W98HPC        W98
770900941228     C                   EXSR      W98999
771000941228     C                   Z-ADD     D98           D98HPC
771100930423     C* ORE X FEST. TRAZ. INTERNE
771200941228     C                   Z-ADD     W98HFI        W98
771300941228     C                   EXSR      W98999
771400941228     C                   Z-ADD     D98           D98HFI
771500930423     C* ORE X FEST. TRAZ. CLIENTE
771600941228     C                   Z-ADD     W98HFC        W98
771700941228     C                   EXSR      W98999
771800941228     C                   Z-ADD     D98           D98HFC
771900930423     C* ORE X FEST. CONS. INTERNE
772000941228     C                   Z-ADD     W98HEI        W98
772100941228     C                   EXSR      W98999
772200941228     C                   Z-ADD     D98           D98HEI
772300930423     C* ORE X FEST. CONS. CLIENTE
772400941228     C                   Z-ADD     W98HEC        W98
772500941228     C                   EXSR      W98999
772600941228     C                   Z-ADD     D98           D98HEC
772700930511     C* ORE X F.DEPOSITO INTERNE
772800941228     C                   Z-ADD     W98HDI        W98
772900941228     C                   EXSR      W98999
773000941228     C                   Z-ADD     D98           D98HDI
773100930511     C* ORE X F.DEPOSITO CLIENTE
773200941228     C                   Z-ADD     W98HDC        W98
773300941228     C                   EXSR      W98999
773400941228     C                   Z-ADD     D98           D98HDC
773500930511     C* ORE X CONTRASSEGNO INTERNE
773600941228     C                   Z-ADD     W98HOI        W98
773700941228     C                   EXSR      W98999
773800941228     C                   Z-ADD     D98           D98HOI
773900930511     C* ORE X CONTRASSEGNO CLIENTE
774000941228     C                   Z-ADD     W98HOC        W98
774100941228     C                   EXSR      W98999
774200941228     C                   Z-ADD     D98           D98HOC
774300930914     C* ORE DI CONSEGNABILITA' INTERNE
774400941228     C                   Z-ADD     W98OCI        W98
774500941228     C                   EXSR      W98000
774600941228     C                   Z-ADD     D98           D98OCI
774700930914     C* ORE DI CONSEGNABILITA' CLIENTE
774800941228     C                   Z-ADD     W98OCC        W98
774900941228     C                   EXSR      W98000
775000941228     C                   Z-ADD     D98           D98OCC
775100071122     C
775200071122     c* Per i codici bolla M prepagati annullati, azzero tutto
775300071122     c                   if        d98cbo='M '
775400071122     c                   clear                   d98rpi
775500071122     c                   clear                   d98rpc
775600071122     c                   clear                   d98rti
775700071122     c                   clear                   d98rtc
775800071122     c                   clear                   d98rci
775900071122     c                   clear                   d98rcc
776000071122     c                   clear                   d98rri
776100071122     c                   clear                   d98rrc
776200071122     c                   clear                   d98fpi
776300071122     c                   clear                   d98fpc
776400071122     c                   clear                   d98fti
776500071122     c                   clear                   d98ftc
776600071122     c                   clear                   d98fci
776700071122     c                   clear                   d98fcc
776800071122     c                   clear                   d98fri
776900071122     c                   clear                   d98frc
777000071122     c                   clear                   d98npi
777100071122     c                   clear                   d98npc
777200071122     c                   clear                   d98nti
777300071122     c                   clear                   d98ntc
777400071122     c                   clear                   d98nci
777500071122     c                   clear                   d98ncc
777600071122     c                   clear                   d98nri
777700071122     c                   clear                   d98nrc
777800071122     c                   clear                   d98OCi
777900071122     c                   clear                   d98OCC
778000071122     c                   endif
778100930423     C*
778200100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
778300100308     c*  calcolo
778400100308     c                   eval      d98dcr=s98dcr
778500100308     c                   eval      d98hcr=s98hcr
778600100308     c                   eval      d98tcr=s98tcr
778700160405     c
778800160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
778900160405     c*  perchè il pgm usa il campo verificado se =10 )
779000160405     c                   if        wVP='S'
779100160405     c                   add       24            d98tci
779200160405     c                   add       24            d98tcc
779300160405     c                   add       24            d98tri
779400160405     c                   add       24            d98trc
779500160405     c                   endif
779600100308     c
779700930423     C                   ENDSR
779800941228     C***********************************************************************
779900941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
780000941228     C***********************************************************************
780100941228     C     W98999        BEGSR
780200941228     C     W98           IFLT      -999
780300941228     C                   Z-ADD     -999          D98               3 0
780400941228     C                   ELSE
780500941228     C     W98           IFGT      999
780600941228     C                   Z-ADD     999           D98
780700941228     C                   ELSE
780800941228     C                   Z-ADD     W98           D98
780900941228     C                   END
781000941228     C                   END
781100941228     C                   ENDSR
781200941228     C***********************************************************************
781300941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
781400941228     C***********************************************************************
781500941228     C     W98000        BEGSR
781600941228     C     W98           IFLE      *ZEROS
781700941228     C                   Z-ADD     *ZEROS        D98
781800941228     C                   ELSE
781900941228     C     W98           IFGT      999
782000941228     C                   Z-ADD     999           D98
782100941228     C                   ELSE
782200941228     C                   Z-ADD     W98           D98
782300941228     C                   END
782400941228     C                   END
782500941228     C                   ENDSR
782600930305     C***********************************************************************
782700930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
782800930305     C***********************************************************************
782900930305     C     AZZERA        BEGSR
783000150219     c* data e ora del momento
783100150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
783200150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
783300150219     C                   Z-ADD     DTORA4        WHHODI            4 0
783400171110     c                   clear                   wdaodi_m1
783500171110     c* Verifico se orario < 7,00 se il giorno precedente è festivo
783600171110     c*  se festivo faccio finta che sia domenica se bolla NON borderizzata
783700171110     c                   if        whhodi<0600
783800171110     c     *iso          movel     wdaodi        wrkoggiiso
783900171110     c                   subdur    1:*d          wrkoggiiso
784000171110     c     *iso          movel     wrkoggiiso    wdaodi_m1         8 0
784100171110     c* Verifico se festivo
784200171110     c                   eval      wslitt='N'
784300171110     c                   eval      wpodata=wdaodi_m1
784400171110     c                   exsr      chkfes
784500171110     c                   if        wslitt<>'S'
784600171110     c                   clear                   wdaodi_m1
784700171110     c                   endif
784800171110     c                   endif
784900930305     C*
785000141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
785100150603     C                   MOVEL     'N'           opendan
785200141107     c                   clear                   or_ddc
785300141108     c                   clear                   sav_12dat         8 0
785400141107     c                   clear                   or_datarif
785500140402     c                   clear                   wpick2            1
785600140402     c                   clear                   Primotipo
785700090928     c                   clear                   Primodev
785800090928     c                   clear                   Primohev
785900070827     c                   clear                   CallAR5
786000071122     c                   clear                   d98cbo
786100100308     c                   clear                   s98dcr
786200100308     c                   clear                   s98hcr
786300100308     c                   clear                   s98tcr
786400141106     c                   clear                   nosomma           1
786500141229     c                   clear                   wtrasm
786600150324     c                   clear                   avvcmc
786700150325     c                   clear                   DSbl4L
786800170925     c**                 clear                   dsbl4I
786900170925     c                   clear                   dpndflo
787000150401     c                   clear                   fnlv82ds
787100150331     c                   clear                   DS7R
787200150605     c                   clear                   wDEE              1
787300150629     c                   clear                   wtsogliaE
787400930323     C                   MOVEL     '0'           D98ERR
787500930323     C                   MOVEL     *BLANKS       D98MSG
787600931124     C                   Z-ADD     *ZEROS        D98TPI
787700931124     C                   Z-ADD     *ZEROS        D98TPC
787800930325     C                   Z-ADD     *ZEROS        D98TTI
787900930325     C                   Z-ADD     *ZEROS        D98TTC
788000930325     C                   Z-ADD     *ZEROS        D98TCI
788100930325     C                   Z-ADD     *ZEROS        D98TCC
788200090827     C                   Z-ADD     *ZEROS        w99TCI
788300090827     C                   Z-ADD     *ZEROS        w99TCC
788400930325     C                   Z-ADD     *ZEROS        D98TRI
788500930325     C                   Z-ADD     *ZEROS        D98TRC
788600090921     C                   Z-ADD     *ZEROS        w99TRI
788700090921     C                   Z-ADD     *ZEROS        w99TRC
788800931124     C                   Z-ADD     *ZEROS        D98RPI
788900931124     C                   Z-ADD     *ZEROS        D98RPC
789000930405     C                   Z-ADD     *ZEROS        D98RTI
789100930405     C                   Z-ADD     *ZEROS        D98RTC
789200930405     C                   Z-ADD     *ZEROS        D98RCI
789300930405     C                   Z-ADD     *ZEROS        D98RCC
789400930405     C                   Z-ADD     *ZEROS        D98RRI
789500930405     C                   Z-ADD     *ZEROS        D98RRC
789600931124     C                   Z-ADD     *ZEROS        D98FPI
789700931124     C                   Z-ADD     *ZEROS        D98FPC
789800930325     C                   Z-ADD     *ZEROS        D98FTI
789900930325     C                   Z-ADD     *ZEROS        D98FTC
790000930325     C                   Z-ADD     *ZEROS        D98FCI
790100930325     C                   Z-ADD     *ZEROS        D98FCC
790200930325     C                   Z-ADD     *ZEROS        D98FRI
790300930325     C                   Z-ADD     *ZEROS        D98FRC
790400931124     C                   MOVEL     *BLANKS       D98CPI
790500931124     C                   MOVEL     *BLANKS       D98CPC
790600930325     C                   MOVEL     *BLANKS       D98CTI
790700930325     C                   MOVEL     *BLANKS       D98CTC
790800930325     C                   MOVEL     *BLANKS       D98CCI
790900930325     C                   MOVEL     *BLANKS       D98CCC
791000930325     C                   MOVEL     *BLANKS       D98CRI
791100930325     C                   MOVEL     *BLANKS       D98CRC
791200931124     C                   Z-ADD     *ZEROS        D98NPI
791300931124     C                   Z-ADD     *ZEROS        D98NPC
791400930325     C                   Z-ADD     *ZEROS        D98NTI
791500930325     C                   Z-ADD     *ZEROS        D98NTC
791600930325     C                   Z-ADD     *ZEROS        D98NCI
791700930325     C                   Z-ADD     *ZEROS        D98NCC
791800930325     C                   Z-ADD     *ZEROS        D98NRI
791900930325     C                   Z-ADD     *ZEROS        D98NRC
792000930414     C                   Z-ADD     *ZEROS        D98HHI
792100930414     C                   Z-ADD     *ZEROS        D98HHC
792200930414     C                   Z-ADD     *ZEROS        D98HOI
792300930414     C                   Z-ADD     *ZEROS        D98HOC
792400930414     C                   Z-ADD     *ZEROS        D98HLI
792500930414     C                   Z-ADD     *ZEROS        D98HLC
792600930414     C                   Z-ADD     *ZEROS        D98HGI
792700930414     C                   Z-ADD     *ZEROS        D98HGC
792800931124     C                   Z-ADD     *ZEROS        D98HPI
792900931124     C                   Z-ADD     *ZEROS        D98HPC
793000930414     C                   Z-ADD     *ZEROS        D98HFI
793100930414     C                   Z-ADD     *ZEROS        D98HFC
793200930416     C                   Z-ADD     *ZEROS        D98HEI
793300930416     C                   Z-ADD     *ZEROS        D98HEC
793400930414     C                   Z-ADD     *ZEROS        D98HRI
793500930414     C                   Z-ADD     *ZEROS        D98HRC
793600930511     C                   Z-ADD     *ZEROS        D98HDI
793700930511     C                   Z-ADD     *ZEROS        D98HDC
793800930325     C                   Z-ADD     *ZEROS        D98DCI
793900930325     C                   Z-ADD     *ZEROS        D98HCI
794000930325     C                   Z-ADD     *ZEROS        D98DCE
794100930325     C                   Z-ADD     *ZEROS        D98HCE
794200931125     C                   Z-ADD     *ZEROS        D98OCI
794300931125     C                   Z-ADD     *ZEROS        D98OCC
794400931202     C                   Z-ADD     *ZEROS        D98DEI
794500931202     C                   Z-ADD     *ZEROS        D98OEI
794600931202     C                   Z-ADD     *ZEROS        D98DEE
794700931202     C                   Z-ADD     *ZEROS        D98OEE
794800151019     c                   CLEAR                   D98FLP
794900161107     c                   CLEAR                   D98Gfas
795000150325     C                   clear                   D98dtctd
795100150326     C                   clear                   D98picklim
795200150325     C                   clear                   D98spcdee
795300150331     C                   clear                   fddata
795400170208     C                   clear                   D98dam_rtc
795500140825     c
795600141017     c                   clear                   d98ddc
795700141017     c                   clear                   d98cons
795800160506     c                   clear                   d98disf
795900141017     c                   clear                   wd98ndc
796000140910     c                   clear                   wd98fbc
796100150119     c                   clear                   wd98acc
796200140825     c                   clear                   wtrulor
796300160401     c                   clear                   wVP               1
796400170208     c                   clear                   wsolodam
796500150219     c
796600150219     c                   clear                   IMPdata
796700150219     c                   clear                   IMPfgs
796800150219     c                   clear                   DISdata
796900150219     c                   clear                   DISfgs
797000150219     c                   clear                   DISora
797100150219     c                   clear                   DISnpg
797200150219     c                   clear                   IDnpg
797300150219     c                   clear                   IDdata
797400150219     c                   clear                   IDora
797500150219     c                   clear                   IDfgs
797600931125     C* CAMPI VARI
797700931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
797800140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
797900140825      *   DALLA LETTURA DI QUESTI FILE                               *
798000931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
798100931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
798200931125     C     D98NSP        IFNE      *ZEROS
798300930525     C                   MOVEL     *BLANKS       D98TSP
798400941228     C                   MOVEL     *BLANKS       D98CAD
798500970728     C                   MOVEL     *BLANKS       D98LOD
798600930525     C                   Z-ADD     *ZEROS        D98TFP
798700930525     C                   Z-ADD     *ZEROS        D98TFA
798800930525     C                   Z-ADD     *ZEROS        D98LNA
798900930525     C                   Z-ADD     *ZEROS        D98DSP
799000941228     C                   Z-ADD     *ZEROS        D98MGS
799100930525     C                   Z-ADD     *ZEROS        D98DTI
799200930525     C                   Z-ADD     *ZEROS        D98HTI
799300930525     C                   Z-ADD     *ZEROS        D98DCM
799400930525     C                   Z-ADD     *ZEROS        D98HMC
799500930525     C                   MOVEL     *BLANKS       D98GC1
799600930525     C                   MOVEL     *BLANKS       D98GC2
799700010820     C                   Z-ADD     *ZEROS        D98CAS
799800010821     C                   MOVEL     *BLANKS       D98VCA
799900930525     C                   MOVEL     *BLANKS       D98TIC
800000930525     C                   Z-ADD     *ZEROS        D98DCR
800100930525     C                   Z-ADD     *ZEROS        D98HCR
800200930525     C                   MOVEL     *BLANKS       D98TCR
800300050914     C                   MOVEL     *BLANKS       Consideraora      1
800400930525     C                   MOVEL     *BLANKS       D98CMC
800500170818     C                   clear                   wD98ntc_con       1 0
800600930525     C                   Z-ADD     *ZEROS        D98DAG
800700161215     C                   Z-ADD     *ZEROS        D98DCc
800800161215     C                   Z-ADD     *ZEROS        D98DAc
800900161215     C                   Z-ADD     *ZEROS        D98DCG
801000930525     C                   MOVEL     *BLANKS       D98FFD
801100930525     C                   Z-ADD     *ZEROS        D98NCD
801200930525     C                   MOVEL     *BLANKS       D98FNS
801300931124     C                   Z-ADD     *ZEROS        D98DAM
801400170105     C                   Z-ADD     *ZEROS        D98Dfv
801500940628     C                   MOVEL     *BLANKS       D98FIN
801600940628     C                   Z-ADD     *ZEROS        D98KSC
801700940628     C                   MOVEL     *BLANKS       D98TBL
801800931125     C                   END
801900960229     C     D98TLA        IFEQ      'T'
802000960229     C                   Z-ADD     *ZEROS        D98DTI
802100960229     C                   Z-ADD     *ZEROS        D98HTI
802200960229     C                   ENDIF
802300931125     C* CAMPI WORK
802400930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
802500930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
802600150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
802700930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
802800941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
802900960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
803000930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
803100930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
803200940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
803300930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
803400150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
803500931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
803600931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
803700150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
803800930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
803900930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
804000930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
804100930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
804200930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
804300940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
804400941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
804500930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
804600050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
804700050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
804800140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
804900930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
805000930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
805100930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
805200930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
805300930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
805400930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
805500930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
805600930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
805700930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
805800930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
805900930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
806000930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
806100930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
806200930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
806300140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
806400931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
806500060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
806600960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
806700931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
806800140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
806900140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
807000150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
807100140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
807200941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
807300970304     C******************** Z-ADD*ZEROS    W98DBR  80
807400970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
807500150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
807600050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
807700930414     C*
807800941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
807900941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
808000930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
808100930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
808200930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
808300930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
808400930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
808500930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
808600941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
808700941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
808800930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
808900930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
809000930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
809100930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
809200930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
809300930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
809400941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
809500941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
809600930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
809700930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
809800930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
809900930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
810000930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
810100930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
810200930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
810300930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
810400060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
810500060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
810600060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
810700060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
810800930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
810900930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
811000930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
811100930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
811200930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
811300930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
811400930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
811500930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
811600930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
811700930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
811800930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
811900930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
812000930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
812100930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
812200941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
812300941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
812400930423     C*
812500930511     C                   Z-ADD     *ZEROS        W98HF             5 0
812600930423     C                   Z-ADD     *ZEROS        W98HH             5 0
812700060418     C                   Z-ADD     *ZEROS        W98EG             5 0
812800060420     C                   Z-ADD     *ZEROS        W98EA             5 0
812900930423     C                   Z-ADD     *ZEROS        W98HG             5 0
813000930423     C                   Z-ADD     *ZEROS        W98HL             5 0
813100930423     C                   Z-ADD     *ZEROS        W98HO             5 0
813200930511     C                   Z-ADD     *ZEROS        W98HD             5 0
813300930611     C*
813400930611     C                   Z-ADD     *ZEROS        WORELA            3 0
813500960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
813600930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
813700930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
813800931207     C                   Z-ADD     *ZEROS        WORECR            3 0
813900090902     c
814000090902     c                   clear                   wmancaTRAZ
814100150113     c                   clear                   wcaldpcee         1
814200150220     c                   clear                   evemic            2
814300930305     C*
814400060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
814500060406     C                   clear                   LAF                            *LASC.AVVISO FINE
814600091014     C                   clear                   LAp                            *posizione l.avviso
814700060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
814800060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
814900060406c    c                   clear                   LCon
815000060406     C                   clear                   EVD                            *DATA EVENTO
815100060406     C                   clear                   EVO                            *ORA EVENTO
815200060406     C                   clear                   EVC                            *CAUSALE EVENTO
815300131218     C                   clear                   EVT
815400060406     C                   clear                   pVD                            *DATA EVENTO
815500060406     C                   clear                   pVO                            *ORA EVENTO
815600060406     C                   clear                   pVC                            *CAUSALE EVENTO
815700060406     C                   clear                   pVF                            *CAUSALE EVENTO
815800060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
815900060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
816000060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
816100060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
816200060406     C                   clear                   COV                            *DATE FINE L.A. E GI
816300060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
816400060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
816500091014     C                   clear                   GIp                            *posizione vento gia
816600060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
816700060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
816800060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
816900931213     C*
817000060406     C                   clear                   TUC                            *TURNI CHIUSURA
817100931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
817200931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
817300060406     c
817400060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
817500060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
817600060406     c                   clear                   pgii
817700060406     c                   clear                   pgif
817800060406     c                   clear                   pgci
817900060406     c                   clear                   pgcf
818000060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
818100060420     c                   clear                   paii
818200060420     c                   clear                   paif
818300930420     C*
818400930305     C                   ENDSR
818500930512     C***********************************************************************
818600930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
818700930512     C***********************************************************************
818800930512     C     FORCON        BEGSR
818900930512     C*
819000931124     C                   Z-ADD     *ZEROS        D98RPI
819100931124     C                   Z-ADD     *ZEROS        D98RPC
819200930512     C                   Z-ADD     *ZEROS        D98RTI
819300930512     C                   Z-ADD     *ZEROS        D98RTC
819400930512     C                   Z-ADD     *ZEROS        D98RCI
819500930512     C                   Z-ADD     *ZEROS        D98RCC
819600930512     C                   Z-ADD     *ZEROS        D98RRI
819700930512     C                   Z-ADD     *ZEROS        D98RRC
819800931124     C                   Z-ADD     *ZEROS        D98FPI
819900931124     C                   Z-ADD     *ZEROS        D98FPC
820000930512     C                   Z-ADD     *ZEROS        D98FTI
820100930512     C                   Z-ADD     *ZEROS        D98FTC
820200930512     C                   Z-ADD     *ZEROS        D98FCI
820300930512     C                   Z-ADD     *ZEROS        D98FCC
820400930512     C                   Z-ADD     *ZEROS        D98FRI
820500930512     C                   Z-ADD     *ZEROS        D98FRC
820600931124     C                   MOVEL     'S'           D98CPI
820700931124     C                   MOVEL     'S'           D98CPC
820800930512     C                   MOVEL     'S'           D98CTI
820900930512     C                   MOVEL     'S'           D98CTC
821000930512     C                   MOVEL     'S'           D98CCI
821100930512     C                   MOVEL     'S'           D98CCC
821200930512     C                   MOVEL     'S'           D98CRI
821300930512     C                   MOVEL     'S'           D98CRC
821400931124     C                   Z-ADD     *ZEROS        D98NPI
821500931124     C                   Z-ADD     *ZEROS        D98NPC
821600930512     C                   Z-ADD     *ZEROS        D98NTI
821700930512     C                   Z-ADD     *ZEROS        D98NTC
821800930512     C                   Z-ADD     *ZEROS        D98NCI
821900930512     C                   Z-ADD     *ZEROS        D98NCC
822000930512     C                   Z-ADD     *ZEROS        D98NRI
822100930512     C                   Z-ADD     *ZEROS        D98NRC
822200930512     C                   Z-ADD     *ZEROS        D98HHI
822300930512     C                   Z-ADD     *ZEROS        D98HHC
822400930512     C                   Z-ADD     *ZEROS        D98HOI
822500930512     C                   Z-ADD     *ZEROS        D98HOC
822600930512     C                   Z-ADD     *ZEROS        D98HLI
822700930512     C                   Z-ADD     *ZEROS        D98HLC
822800930512     C                   Z-ADD     *ZEROS        D98HGI
822900930512     C                   Z-ADD     *ZEROS        D98HGC
823000931124     C                   Z-ADD     *ZEROS        D98HPI
823100931124     C                   Z-ADD     *ZEROS        D98HPC
823200930512     C                   Z-ADD     *ZEROS        D98HFI
823300930512     C                   Z-ADD     *ZEROS        D98HFC
823400930512     C                   Z-ADD     *ZEROS        D98HEI
823500930512     C                   Z-ADD     *ZEROS        D98HEC
823600930512     C                   Z-ADD     *ZEROS        D98HRI
823700930512     C                   Z-ADD     *ZEROS        D98HRC
823800930512     C                   Z-ADD     *ZEROS        D98HDI
823900930512     C                   Z-ADD     *ZEROS        D98HDC
824000930520     C*
824100930512     C                   ENDSR
824200930305     C********************************************************************
824300971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
824400950406031059090120151181212243273304334365
824500950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
824600950406312831303130313130313031
824700930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
824800971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
824900971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
825000971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
825100971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
825200971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
825300971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
825400971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
825500971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
825600971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
825700971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
825800971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
825900971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
826000971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
826100971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
826200971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
826300971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
826400971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
826500971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
826600971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
826700971127TNSD99R- Bolla inesistente.                                               BOIN20
826800971127TNSD99R-     Cap inesistente.                                             CAPI21
826900971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
827000971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
827100971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
827200971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
827300971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
827400971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
827500971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
827600971127TNSD99R- Contrassegno non trovato.                                        CONT29
827700971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
827800990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
827900990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
828000060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
828100060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
828200971127TNSD99R- 35                                                                   35
828300971127TNSD99R- 36                                                                   36
828400971127TNSD99R- 37                                                                   37
828500971127TNSD99R- 38                                                                   38
828600971127TNSD99R- 39                                                                   39
828700971127TNSD99R- 40                                                                   40
