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
005300950406     D*****************************************************************
005400950406     D* SCHIERE PER ROUTINE GIORNO>DATA E DATA>GIORNO
005500950406     D GPM             S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND     GG. PROGR. MESI
005600950406     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)            GG. DEL MESE
005700930304     D*****************************************************************
005800930304     D* MESSAGGI
005900941227     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
006000950406     D*****************************************************************
006100950406     D* SCHIERE DI LAVORO PER ROUTINE GIORNO>DATA E DATA>GIORNO
006200950406     D $PM             S              3  0 DIM(12) ASCEND                       GG. PROGR. MESI
006300950406     D $GM             S              2  0 DIM(12)                              GG. DEL MESE
006400941227     D*****************************************************************
006500941227     D* SCHIERA TABELLA CODICI BOLLA -> TIPO BOLLA (FORMATO SEDE)
006600941228     D C3A             S              2    DIM(50)                              *KEY
006700941227     D T3A             S              2    DIM(50)                              *TIPO BOLLA
006800941228     D F3A             S              1  0 DIM(50)                              *FLAG CONTRASSEGNO
006900990308     D R3A             S              1    DIM(50)                              *RAGGRUPP.BOLLE
007000930316     D*****************************************************************
007100930405     D* SCHIERA TABELLA TEMPI FISSI DI DELIVERY (1X)  (PER FILIALE)
007200080513     d c_dim_1X        c                   const(500)
007300080513     D C1X             S              8    DIM(c_dim_1X)                        *KEY
007400080513     D G1X             S              3  0 DIM(c_dim_1X)                        *TEMPO GIORNO
007500080513     D O1X             S              4  0 DIM(c_dim_1X)                        *ORA SOGLIA
007600080513     D M1X             S              3  0 DIM(c_dim_1X)                        *TEMPO MATTINO
007700080513     D P1X             S              3  0 DIM(c_dim_1X)                        *TEMPO POMERIGGIO
007800080513     D I1X             S              3  0 DIM(c_dim_1X)                        *ORE INF.CONFORM.
007900080513     D S1X             S              3  0 DIM(c_dim_1X)                        *ORE SUP.CONFORM.
008000930405     D*****************************************************************
008100930405     D* SCHIERA TABELLA SIMBOLI CALENDARIO (1Y)
008200930421     D C1Y             S              8    DIM(30)                              *KEY
008300930421     D I1Y             S              1    DIM(30)                              *FEST.INTERNA ?
008400930421     D E1Y             S              1    DIM(30)                              *FEST.CLIENTE ?
008500930421     D*****************************************************************
008600930421     D* SCHIERA TABELLA CAUSALI EVENTI BOLLE (2A)
008700060403     D C2A             S              3    DIM(199)                             *KEY
008800060406     D D2A             S              3    DIM(199)                             *RECORD
008900170915     D D2Z             S              6    DIM(199)                             *RECORD
009000031201     D*****************************************************************
009100031201     D* SCHIERA TABELLA CODICI GESTIONE PARTICOLARITA' GIACENZE (7Q) + SUA ESTENSIONE (7W)
009200060403     D SK7QW           S              2    DIM(199)
009300930421     D*****************************************************************
009400060406     D* SCHIERA EVENTI BOLLE di l.avviso e giacenza
009500060403     D EVD             S              8  0 DIM(199)                             *DATA EVENTO
009600060403     D EVO             S              4  0 DIM(199)                             *ORA EVENTO
009700060403     D EVC             S              3    DIM(199)                             *CAUSALE EVENTO
009800131218     D EVT             S              1    DIM(199)                             *tipo G/A
009900031125     D*****************************************************************
010000031125     D* SCHIERA GIACENZE BOLLE
010100031125     D GCPD            S              8  0 DIM(99)                              *DATA GIACENZA
010200930421     D*****************************************************************
010300930421     D* SCHIERA RANGE DATE INIZIO/FINE PER LASCIATO AVVISO
010400981029     D LAI             S              9  0 DIM(99)                              *INIZIO
010500981029     D LAF             S              9  0 DIM(99)                              *FINE
010600091014     D LAp             S              3  0 DIM(99)                              *FINE
010700981029£$   D LCI             S              9  0 DIM(99)                              *INIZ X CONSEGNABILI
010800981029     D LCF             S              9  0 DIM(99)                              *FINE X CONSEGNABILI
010900050520     D LCon            S              1    DIM(99)                              *FINE X CONSEGNABILI
011000960129$$£  D*****************************************************************
011100960129$$£  D* SCHIERA RANGE DATE INIZIO/FINE PER EVENTI GIACENZA
011200981029$$£  D GII             S              9  0 DIM(99)                              *INIZIO
011300981029$$£  D GIF             S              9  0 DIM(99)                              *FINE
011400091014$$£  D GIp             S              3  0 DIM(99)                              *FINE
011500981029$$£  D GCI             S              9  0 DIM(99)                              *INIZ X CONSEGNABILI
011600981029$$£  D GCF             S              9  0 DIM(99)                              *FINE X CONSEGNABILI
011700931213     D*****************************************************************
011800931213     D* SCHIERA TURNI CHIUSURA
011900931213     D TUC             S              2    DIM(4)                               *TURNI CHIUSURA
012000931201     D*****************************************************************
012100931201     D* SCHIERA RANGE DATE INIZIO/FINE PER TURNO CHIUSURA
012200981029     D TII             S              9  0 DIM(99)                              *INTERNO/INIZIO
012300981029     D TIF             S              9  0 DIM(99)                              *INTERNO/FINE
012400981029     D TCI             S              9  0 DIM(99)                              *CLIENTE/INIZIO
012500981029     D TCF             S              9  0 DIM(99)                              *CLIENTE/FINE
012600060406     D*****************************************************************
012700060406     D* SCHIERA EVENTI particolari bolle
012800060406     D pVD             S              8  0 DIM(199)                             *DATA EVENTO
012900060406     D pVO             S              4  0 DIM(199)                             *ORA EVENTO
013000060406     D pVC             S              3    DIM(199)                             *CAUSALE EVENTO
013100060406     D pVF             S              1    DIM(199)                             *CAUSALE EVENTO
013200060406     D* SCHIERA RANGE DATE INIZIO/FINE PER Eventi particolari bolle "G"
013300060406     d*  che  Giustificano l'affidabilità
013400060406     D pGII            S              9  0 DIM(99)                              *INTERNO/INIZIO
013500060406     D pGIF            S              9  0 DIM(99)                              *INTERNO/FINE
013600060406     D pGCI            S              9  0 DIM(99)                              *CLIENTE/INIZIO
013700060406     D pGCF            S              9  0 DIM(99)                              *CLIENTE/FINE
013800060418     d*  che  Allungano    l'affidabilità
013900060418     D pAII            S              9  0 DIM(99)                              *INTERNO/INIZIO
014000060418     D pAIF            S              9  0 DIM(99)                              *INTERNO/FINE
014100060418     D pACI            S              9  0 DIM(99)                              *CLIENTE/INIZIO
014200060418     D pACF            S              9  0 DIM(99)                              *CLIENTE/FINE
014300931207     D*****************************************************************
014400931207     D* SCHIERA DATE PER CALCOLO ORE VALIDE TRA DUE DATE (DATE FINE L.A. E GIAC)
014500981029     D COV             S              9  0 DIM(99)
014600930325     D*****************************************************************
014700930405     D* SCHIERA GIORNI SETTIMANA
014800930405     D*****************************************************************
014900930405     D* SCHIERA GIORNI FESTIVI
015000930517     D*****************************************************************
015100971113     D* SCHIERA RECORD DEL CALENDARIO PIU' UTILIZZATI
015200050523     D CAK             S             12    DIM(600)                             *KEY CALENDARIO
015300050523     D CAM             S             31    DIM(600)                             *MATTINA
015400050523     D CAP             S             31    DIM(600)                             *POMERIGGIO
015500941227     D*****************************************************************
015600941227     D* SCHIERA POSSIBILI COMBINAZIONI LINEE/TERMINAL
015700941227     D LTP             S              3  0 DIM(4)
015800941227     D LTA             S              3  0 DIM(4)
015900971210     D*****************************************************************
016000971210     D* INDICE=COD.FILIALE ,   FL1 => BLANK=ITALIA,'E'=ESTERO
016100971210     D FL1             S              1    DIM(999)                             *FLAG ITALIA/ESTERO
016200020226     D*****************************************************************
016300160331     D* SCHIERA FILIALI FEDEX   e DPD
016400060406     D FILFED          S              3  0 DIM(99)
016500160331     D FILDPD          S              3  0 DIM(99)
016600970701     D*****************************************************************
016700970701     D* PARAMETRI CHIAMATA PGM TISI95R (CONTROLLO CAP)
016800970701     D SI95          E DS                  EXTNAME(TISI95DS)
016900930325     D*****************************************************************
017000930405     D* DS PER RIDEFINIRE SCHIERA GIORNI SETTIMANA
017100930405     D GSEDS           DS
017200930405     D  GSE                    1     31
017300930405     D                                     DIM(31)
017400930405     D*****************************************************************
017500930405     D* DS PER RIDEFINIRE SCHIERA GIORNI FESTIVI
017600930405     D GFEDS           DS
017700930405     D  GFE                    1     31
017800930405     D                                     DIM(31)
017900150325     D*****************************************************************
018000150325     D* DS per record L del FIAR4
018100150325     D DSbl4L        E DS
018200160331     d
018300170925     D** DSbl4I        E DS
018400170821
018500170925     D DPNDFLO       E DS                  inz
018600170925
018700170925     D fnlrj6ds      E DS                  inz
018800930323     D*****************************************************************
018900930323     D* DS ESTERNA IN INPUT AL PGM
019000970731     D DS99          E DS                  EXTNAME(TNSD99DS)
019100930323     D*****************************************************************
019200930323     D* DS ESTERNA TABELLA 1X TEMPI FISSI DI DELIVERY
019300930326     D DS1X          E DS
019400930405     D*****************************************************************
019500971113     D* DS ESTERNA TABELLA 1Y SIMBOLI CALENDARIO
019600930405     D DS1Y          E DS
019700941227     D*****************************************************************
019800941227     D* DS ESTERNA TABELLA 3A (CODICI BOLLA)
019900941227     D DS3A          E DS
020000930421     D*****************************************************************
020100060406     D* DS ESTERNA TABELLA 2A CAUSALI EVENTI BOLLE
020200060406     D DS2A          E DS                  prefix(W:1)
020300170915     D DS2Z          E DS                  prefix(W:1)
020400020226     D*****************************************************************
020500020226     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
020600020226     D OG143         E DS
020700020702     D*****************************************************************
020800020702     D* DS PER LETTURA TABELLA "NTW"
020900020702     D DNTW          E DS
021000150326     D* DS PER LETTURA TABELLA "VPO" flag operativi
021100150326     D Dvpo          E DS
021200031201     D*****************************************************************
021300150331     D* DS ESTERNA TABELLA 7Q/7W/7R
021400031201     D DS7Q          E DS
021500031201     D DS7W          E DS
021600150331     D DS7R          E DS
021700070514     D*****************************************************************
021800140825     D* DS per reperimento orari di consegna
021900140825     D TRULORSDS     E DS
022000140825     D TRULOR2DS     E DS
022100150331     D TRULOR3DS     E DS
022200140825     D*****************************************************************
022300150219     D FNLV53DS      E DS
022400150219     D FNLV55DS      E DS
022500150401     D*****************************************************************
022600150401     D fnlv82ds      E DS
022700150219     D*****************************************************************
022800060406     D usa2a           DS
022900060406     D  §2aftc                 1      1
023000060406     D  §2agla                 2      2  0
023100060406     D  §2aFTG                 3      3
023200060406     D usa2Z           DS
023300060406     D  §2Zcep                 1      1
023400060406     D  §2Znep                 2      3
023500060406     D  §2Zdep                 4      4
023600060406     D  §2Zfep                 5      5
023700170915     D  §2Ztel                 6      6
023800930517     D***************************************************************
023900971113     D* DS PER SCOMPORRE CHIAVE CALENDARIO
024000930517     D                 DS
024100941227     D  DCATFP                 1      3
024200941227     D  DCATFA                 4      6
024300941227     D  DCAANN                 7     10
024400941227     D  DCAMES                11     12
024500941227     D  DCALEN                 1     12
024600950406     D***************************************************************
024700950406     D* DS PER SCOMPORRE WDAT IN AAAA/MM/GG
024800950406     D                 DS
024900950406     D  WDAT                   1      8  0
025000950406     D  WAA                    1      4  0
025100950406     D  WMM                    5      6  0
025200950406     D  WGG                    7      8  0
025300970304     D***************************************************************
025400970304     D* DS PER CONFRONTI TRA DATE
025500970304     D                 DS
025600970304     D  WDO1                   1     12  0
025700970304     D  WD1                    1      8  0
025800970304     D  WO1                    9     12  0
025900970304     D                 DS
026000970304     D  WDO2                   1     12  0
026100970304     D  WD2                    1      8  0
026200970304     D  WO2                    9     12  0
026300150330     D***************************************************************
026400150330     D                 DS
026500150330     D  who                    1      2  0
026600150330     D  wmn                    3      4  0
026700150330     D  WORA                   1      4  0
026800941227     D***************************************************************
026900941227     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
027000941227     D DS§DAT          DS
027100941227     D  DSSEC                  1      2  0
027200941227     D  DSANN                  3      4  0
027300941227     D  DSMES                  5      6  0
027400941227     D  DSGIO                  7      8  0
027500941227     D  DSAAS                  1      4  0
027600941228     D  DSMGS                  5      8  0
027700941227     D  DSDAT6                 3      8  0
027800941227     D  DSDAT8                 1      8  0
027900941227     D DS§TIM          DS
028000941227     D  DTORA                  1      2  0
028100941227     D  DTMIN                  3      4  0
028200941227     D  DTSCN                  5      6  0
028300941227     D  DTGIO                  7      8  0
028400941227     D  DTMES                  9     10  0
028500941227     D  DTSEC                 11     12  0
028600941227     D  DTANN                 13     14  0
028700941227     D  DTINV                  7     14  0
028800941227     D  DTORA6                 1      6  0
028900941227     D  DTORA4                 1      4  0
029000941227     D  DTTIME                 1     14  0
029100010821     D***************************************************************
029200050523     D kpjba         E DS
029300010821     D*  CAMBIO DIVISA
029400010821     D YEURDS        E DS                  EXTNAME(YEURCODS)
029500010821     D***************************************************************
029600010821     D*  REPERIMENTO DIVISE DI CONTO
029700010821     D DGED          E DS
029800070827     D*------------------
029900070827     D DAR5GEN       E DS
030000011211     D*------------------
030100011211     D* DS REPERIMENTO IMPORTO MASSIMO CONTRASSEGNO
030200011211     D*------------------
030300011211     D TRUL21DS      E DS
030400050523     d
030500050523     d* DS x controllo riempimento schiera
030600050523     D TRUL0sDS      E DS
030700040809     D*------------------
030800040809     D* ds per il controllo della presenza di CA per la spedizione richiesta
030900040809     D*------------------
031000040809     d FIDN12DS      e ds
031100040809     d  skKey                 26    305    dim(20)
031200040809     d  skAnn                306    325    dim(20)
031300040809     d  skDca                326    485  0 dim(20)
031400040809     d  skFca                486    545  0 dim(20)
031500040809     d  skDch                546    705  0 dim(20)
031600040809     d  skCch                706    745    dim(20)
031700040809     d*
031800040809     d dsKey           ds
031900040809     d  dsaac                         4  0
032000040809     d  dsfil                         3  0
032100040809     d  dsnca                         7  0
032200041028     d wesarb          s              1
032300141216     d wtrasm          s              1
032400041116     d wapri           s              1    inz(' ')
032500050908     d Usacons         s              1    inz(' ')
032600050405     D*------------------
032700050405     D* VARIABILI D WRK RIFERITE AL DATA BASE
032800050405     D*------------------
032900050405     D KWGCPAAS        s                   like(GCPAAS)
033000050405     D KWGCPLNP        s                   like(GCPLNP)
033100050405     D KWGCPNRS        s                   like(GCPNRS)
033200050405     D KWGCPNSP        s                   like(GCPNSP)
033300050405     D KWGCPFRG        s                   like(GCPFRG)
033400050901     d
033500050901     D salvaoreHMC     s                   like(wr3hho)
033600050901     D salvaoreHCR     s                   like(wr3hho)
033700050907     D oreILoDOPO      s                   like(worecr)
033800060526     D wpodata         s                   like(w12dat)
033900070514     D wdata8          s                   like(w12dat)
034000060406     D w003a           s              3
034100060406     D w11poi          s              9  0
034200150217     D picklim         s              4  0
034300060420     D wgiroepa        s              1
034400140911     D wtuctut         s              1    inz
034500060420     D wfine           s              1
034600070827     D CallAR5         s              1    inz
034700140825     D Wtrulor         s              1    inz
034800070827     d kAr5Trd         s                   Like(Ar5Trd) inz('GEN')
034900150325     d kAr4Trc         s                   Like(Ar4Trc) inz('L')
035000071122     d d98cbo          s                   Like(arbcbo)
035100090827     d w99tci          s                   Like(d98tci)
035200090827     d w99tcc          s                   Like(d98tcc)
035300090921     d w99tri          s                   Like(d98tri)
035400090921     d w99trc          s                   Like(d98trc)
035500090928     d Primotipo       s                   Like(§2aftc)
035600090928     d Primodev        s                   Like(evbdev)
035700090928     d Primohev        s                   Like(evbhev)
035800091014     d SalvaEveL       s              3  0
035900091014     d SalvaEveG       s              3  0
036000140911     d EX              s              3  0
036100150325     d II              s              3  0
036200150630     d wdiff           s              9  0
036300100308     d s98dcr          s                   Like(d98dcr)
036400100308     d s98hcr          s                   Like(d98hcr)
036500100308     d s98tcr          s                   Like(d98tcr)
036600140825     d wd98ndc         s                   Like(arbndc)
036700140911     d wd98fbc         s                   Like(arbfbc)
036800150119     d wd98acc         s                   Like(arbacc)
036900150219     d
037000150219     d IMPdata         s                   like(d53dfv)
037100150219     d IMPfgs          s                   like(brvfgs)
037200150219     d DISfgs          s                   like(brvfgs)
037300150219     d DISdata         s                   like(d53dfv)
037400150219     d DISnpg          s                   like(brvnpg)
037500150219     d DISora          s                   like(brvhcs)
037600150219     d IDdata          s                   like(d53dfv)
037700150219     d IDora           s                   like(brvhcs)
037800150219     d IDfgs           s                   like(brvfgs)
037900150219     d IDnpg           s              1
038000150325     D kpdpdr          S                   LIKE(APDpdr)
038100150325     D kpdtip          S                   LIKE(APDtip)
038200170208     d wsolodam        s              1
038300140825     d
038400140825     D wrkoggiiso      s               d
038500930304     C*****************************************************************
038600930323     C*/ INIZIO MAIN DEL PGM
038700170208     C* ESEGUE ROUTINE wdaodiLE
038800930304     C                   EXSR      RUTINZ
038900930528     C*
039000930528     C     D98ERR        IFEQ      '0'                                          --- 1 -->
039100930602     C*
039200930602     C* SE LANCIATO CON TIPO LANCIO='C' (CHIUDI FILE) VADO A FINE PGM
039300930602     C     D98TLA        IFNE      'C'                                          --- 2 -->
039400960729     C*
039500960729     C* CONTROLLO SE MI HANNO LANCIATO PER DETERMINARE LA CONSEGNABILITA'
039600960729     C                   MOVEL     'N'           FLCONS            1            *FLAG ESISTE CONSEGN
039700960729     C     D98DIC        IFGT      *ZEROS
039800960729     C     D98DFC        ORGT      *ZEROS
039900960729     C                   MOVEL     'S'           FLCONS                         *SI ESISTE CONSEGNAB
040000960729     C                   ENDIF
040100930323     C*
040200930511     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
040300931125     C* SE NON E' STATA IMPOSTATA DS DAL PGM CHIAMANTE (NSP=ZERO)
040400930420     C     D98ERR        IFEQ      '0'
040500010821     C     D98NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
040600930415     C     D98TBO        IFEQ      'A'
040700941227     C                   EXSR      LETARB                                       *LEGGE BOLLA ARRIVO
040800930323     C                   ELSE
040900941227     C                   EXSR      LETBLP                                       *LEGGE BOLLA PART.
041000930323     C                   END
041100140402     c
041200150401     c* Leggo fiar5 recod GEN  e FIAR4
041300140402     c                   exsr      leggiar5
041400140402     c
041500931206     C                   ELSE
041600931206     C* DS IMPOSTATA DAL PGM CHIAMANTE IMPOSTO CAMPI LAVORO
041700931206     C                   Z-ADD     D98DCG        W98DCG
041800990308     C                   MOVEL     *BLANKS       WRBL              1            *RAGGRUPP.BOLLE
041900990308     C                   MOVEL     *BLANKS       WFBR              1            *BOLLA DI RESO
042000031201     C                   MOVEL     *BLANKS       WGGA              2            *PARTICOLARIT' GIAC.
042100140822     C                   Z-ADD     d98DCR        s98DCR
042200140822     C                   Z-ADD     d98HCR        s98HCR
042300140822     C                   MOVEL     d98TCR        s98TCR
042400141104     c                   eval      gc1part=d98gc1
042500141104     c                   eval      gc2part=d98gc2
042600150303     c* imposto flag trasmissione bolla = 'N'
042700150303     c                   eval      wtrasm='N'
042800931125     C                   END
042900930420     C                   END
043000930528     C*
043100010821     C     D98ERR        IFEQ      '0'                                          --- 3 -->
043200160420
043300160420     c* se bolla in partenza, devo ricostruire il record "L"
043400160420    2c                   if        wtrasm='N'
043500160420    3c                   if        d98tc1 = 'A' or d98tc1 = 'S' or d98tc2 = 'A'
043600160420     c                             or d98tc2 = 'S'
043700160420     c                   eval      §b4lbas = 'S'
043800160420      * se consegna tassativa sblocco
043900160420    4c                   if        d98dcr <> *zeros and d98tcr = *blanks
044000160420     c                   clear                   §b4lbas
044100160420    4c                   endif
044200160420    3c                   endif
044300160420
044400160420    3c                   if        d98ffd<>' '
044500170206
044600160420     c                   eval      §b4ltfd = 'S'
044700170206     c* se Fermo deposito tolgo il contatto per l'appuntamento
044800170206    4c                   if        §b4lbas = 'S' and
044900170206     c                             (d98tc1 = 'A' or d98tc2= 'A')
045000170206     c                   clear                   §b4lbas
045100170206    4c                   endif
045200160420    3c                   endif
045300160420
045400160420    2c                   endif
045500160420
045600050908     c* Se esiste la data consegnabilità, faccio finta che la bolla
045700050908     c*  non si consegnata, azzerando proprio la dta di consegna
045800050908     c*  perchè può trarre in inganno
045900050908     c                   if        d98dfc>0 and d98dcm>0
046000050908     c                   clear                   d98dcm
046100050908     c                   clear                   d98hmc
046200050908     c                   clear                   d98cca
046300050908     c                   endif
046400090902     c*
046500090902     c* Se non c'e' nè data consegna, ne data arrivo trazione
046600090902     c*  faccio finta che data arrivo trazione= udate utime ai fini dei calcoli
046700090902     c* in fondo al pgm dovrò azzerare i tempi di consegna interni e cliente
046800090902     c* perchè non calcolabili
046900090902     c                   clear                   wmancaTRAZ        1
047000090902     c
047100010821     C*
047200010821     C* EFFETTUO SUBITO CONSIDERAZIONI SU DIVISE (X QUESTIONE EURO)
047300010821     C                   EXSR      CHKVCA
047400141204     c
047500141204     c* Controllo se ha pratica di DANNO aperta
047600141204     c                   EXSR      CHKDAN
047700010108     C*
047800010108     C* SE BOLLA POSTA, UTILIZZA LA DATA BORDERO' ANZICHE' LA DATA SPEDIZIONE
047900091009     C**   D98TSP        IFEQ      'P'                                          *BOLLA POSTA
048000091009     C**   D98DBR        IFGT      *ZEROS                                       *DATA BORDERO' PRESE
048100091009     C**                 Z-ADD     D98DBR        D98DSP                         *DATA BORDERO' --> D
048200091009     C**                 ENDIF
048300091009     C**                 ENDIF
048400930304     C*
048500950110     C* RECUPERO I TEMPI TEORICI DI PARTENZA, TRAZIONE E DI CONSEGNA
048600930420     C     D98ERR        IFEQ      '0'
048700930323     C                   EXSR      RTTEOR
048800930420     C                   END
048900930528     C*
049000930602     C     D98ERR        IFEQ      '0'                                          --- 4 -->
049100970304     C*
049200970304     C* SE RICHIESTO LANCIO CON DATA RIFERIMENTO CONTROLLO LE DATE DA AZZERARE
049300970304     C     D98ERR        IFEQ      '0'
049400970304     C                   EXSR      DATRIF
049500970304     C                   END
049600940901     C*
049700940901     C* TRASF. DATA ARRIVO TRAZIONE E RICONSIDERO TEMPO CONSEGNA SE RITARDO
049800940901     C     D98ERR        IFEQ      '0'
049900940901     C                   EXSR      TDTIPO
050000940901     C                   END
050100930420     C*
050200930511     C* CARICO IL FILE DEGLI EVENTI BOLLA
050300930420     C     D98ERR        IFEQ      '0'
050400041116     c     d98nsp        andgt     0
050500930420     C                   EXSR      CAREVB
050600930420     C                   END
050700931213     C*
050800931213     C* CARICO LA SCHIERA DEI TURNI DI CHIUSURA
050900931213     C     D98ERR        IFEQ      '0'
051000931213     C                   EXSR      CARTUC
051100931213     C                   END
051200930528     C*
051300930602     C     D98ERR        IFEQ      '0'                                          --- 5 -->
051400930405     C*
051500940901     C* TRASFORMO LE DATE (SPED,CONS,ECC.) IN PROGRESSIVI ORE
051600930420     C     D98ERR        IFEQ      '0'
051700930405     C                   EXSR      TDATPO
051800930420     C                   END
051900930528     C*
052000930602     C     D98ERR        IFEQ      '0'                                          --- 6 -->
052100950110     C*
052200950110     C* CALCOLA LE DATE DI PREVISTA PARTENZA DELL'ULTIMO COLLO
052300950110     C     D98ERR        IFEQ      '0'
052400950110     C                   EXSR      CALDPP
052500950110     C                   END
052600930405     C*
052700930511     C* CALCOLA LE DATE DI PREVISTO ARRIVO DELLA TRAZIONE
052800930420     C     D98ERR        IFEQ      '0'
052900930511     C                   EXSR      CALDPT
053000090902     c*
053100090902     c* Ricalcolo le date facendo finta che la trazione stia arrivando
053200090903     c*  in questo momento se non presente
053300090902     c                   if        ricalcoloTRAZ='S'
053400090902     c                   eval      d98dti=WDAODI
053500090902     c                   eval      d98hti=WHHODI
053600090902     c                   eval      wmancaTRAZ='S'
053700090902     c
053800090902     C                   EXSR      TDTIPO
053900090902     C                   EXSR      CALDPP
054000090902     C                   EXSR      CALDPT
054100090902     c                   endif
054200090902     c
054300930420     C                   END
054400960229     C*
054500091009     d* D98TLA = 'T' --> in output solo i tempi di trazione
054600960229     C     D98TLA        IFNE      'T'                                          --- 6A ->
054700060407     C*
054800931206     C* CALCOLA IL RANGE PER IL CONTRASSEGNO
054900931206     C     D98ERR        IFEQ      '0'
055000931206     C                   EXSR      CALASS
055100931206     C                   END
055200931206     C*
055300931206     C* CALCOLA IL RANGE PER IL FERMO DEPOSITO
055400931206     C     D98ERR        IFEQ      '0'
055500931206     C                   EXSR      CALFED
055600931206     C                   END
055700931206     C*
055800931206     C* CALCOLA LE DATE TEORICHE DI CONSEGNA
055900931206     C     D98ERR        IFEQ      '0'
056000931206     C                   EXSR      CALDTE
056100931206     C                   END
056200930511     C*
056300930511     C* CALCOLA IL RANGE PER DATA CONSEGNA RICHIESTA
056400930511     C     D98ERR        IFEQ      '0'
056500930511     C                   EXSR      CALDCR
056600930511     C                   END
056700930511     C*
056800930511     C* CALCOLA I RANGE PER LASCIATO AVVISO
056900930511     C     D98ERR        IFEQ      '0'
057000930511     C                   EXSR      CALLAV
057100930511     C                   END
057200930511     C*
057300930511     C* CALCOLA IL RANGE PER LA GIACENZA
057400930511     C     D98ERR        IFEQ      '0'
057500930511     C                   EXSR      CALGIA
057600930511     C                   END
057700060420     C* CALCOLA IL RANGE PER Eventi Particolari che Allungano
057800060420     C     D98ERR        IFEQ      '0'
057900060420     C                   EXSR      CALcepa
058000060420     C                   END
058100930511     C*
058200930511     C* CALCOLA LE DATE DI PREVISTA CONSEGNA
058300930511     C     D98ERR        IFEQ      '0'
058400930511     C                   EXSR      CALDPC
058500930511     C                   END
058600960229     C*
058700960229     C                   ENDIF                                                  <-- 6A --
058800960229     C*
058900930602     C* TRASFORMO I PROGRESSIVI ORE IN DATE
059000930602     C                   EXSR      TPODAT
059100930602     C*
059200930914     C* CONTROLLO SE BOLLA RIENTRA IN UNO DEI CASI IN CUI NON RIESCO A CALCOLA
059300930602     C* RE I RITARDI PER CUI FORZO CONFORMITA' A SI
059400930602     C     D98ERR        IFEQ      '0'
059500960229     C     D98TLA        ANDNE     'T'
059600930602     C                   EXSR      CHKFOR
059700930602     C                   END
059800930602     C*
059900930602     C     D98ERR        IFEQ      '0'                                          --- 7 -->
060000960229     C     D98TLA        ANDNE     'T'
060100941228     C*
060200941228     C* CALCOLA LE ORE REALI DI PARTENZA
060300941228     C     D98ERR        IFEQ      '0'
060400941228     C                   EXSR      CALORP
060500941228     C                   END
060600930405     C*
060700930511     C* CALCOLA LE ORE REALI DI TRAZIONE
060800930420     C     D98ERR        IFEQ      '0'
060900930407     C                   EXSR      CALORT
061000930420     C                   END
061100930405     C*
061200930511     C* CALCOLA LE ORE REALI DI CONSEGNA
061300930420     C     D98ERR        IFEQ      '0'
061400930416     C                   EXSR      CALORC
061500930420     C                   END
061600930416     C*
061700930511     C* CALCOLA LE ORE REALI DI RESA
061800930420     C     D98ERR        IFEQ      '0'
061900930416     C                   EXSR      CALORR
062000930420     C                   END
062100930420     C*
062200930511     C* CALCOLA LE ORE DI DIFFERENZA REALI/TEORICHE
062300930420     C     D98ERR        IFEQ      '0'
062400930420     C                   EXSR      CALDIF
062500930420     C                   END
062600930420     C*
062700930511     C* CALCOLA LE CONFORMITA'
062800930420     C     D98ERR        IFEQ      '0'
062900930420     C                   EXSR      CALCNF
063000930420     C                   END
063100930913     C*
063200930913     C* CALCOLA LE ORE DI CONSEGNABILITA'
063300930913     C     D98ERR        IFEQ      '0'
063400950407     C     FLCONS        ANDEQ     'S'
063500930913     C                   EXSR      CALCNS
063600930913     C                   END
063700930423     C*
063800930511     C* PORTO I VALORI DAI CAMPI DI LAVORO AI CAMPI IN OUTPUT DELLA DS
063900930423     C     D98ERR        IFEQ      '0'
064000930423     C                   EXSR      W98D98
064100930423     C                   END
064200930528     C*
064300930602     C                   END                                                    <-- 7 ---
064400930602     C                   END                                                    <-- 6 ---
064500930528     C                   END                                                    <-- 5 ---
064600930528     C                   END                                                    <-- 4 ---
064700930528     C                   END                                                    <-- 3 ---
064800930528     C                   END                                                    <-- 2 ---
064900930528     C                   END                                                    <-- 1 ---
065000930323     C*
065100930324     C* SE PGM LANCIATO CON 'C' (LANCI MULTIPLI) CHIUDE CON LR (CHIUDE FILE)
065200930527     C* E LANCIA TUTTI I PGM CHIAMATI PER CHIUDERE I FILE DA LORO APERTI
065300930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
065400930324     C     D98TLA        IFEQ      'C'
065500970714     C                   CLEAR                   SI95
065600970701     C                   MOVEL     'C'           I95TLA
065700020226     C                   CALL      'TISI95R'
065800970701     C                   PARM                    SI95
065900040809     c*
066000040809     c                   clear                   fidn12ds
066100040809     c                   eval      i12tla = 'C'
066200040809     c                   call      'FIDN12R'
066300040809     c                   parm                    fidn12ds
066400050523     c**
066500050523     c* controllo riempimento skiera CAK solo alla fine
066600050523     c**                 clear                   trul0sds
066700050523     c**                 eval      i0sski='CAK/CAM/CAP'
066800050523     c**                 eval      i0sele=%elem(CAK)
066900050523     c**                 if        windca>i0sele
067000050523     c**                 eval      i0spie=i0sele
067100050523     c**                 else
067200050523     c**                 eval      i0spie=windca
067300050523     c**                 endif
067400050523     c**                 eval      i0sfile='AZCLN00F  '
067500050523     c**                 eval      i0scod='     '
067600050523     c**                 eval      i0spgm='TNSD99R'
067700050523     c**                 movel     trul0sds      kpjbu
067800050523     c**                 call      'TRUL0SR'
067900050523     c**                 parm                    kpjba
068000050523     c
068100930323     C                   SETON                                        LR
068200930324     C                   ELSE
068300930324     C                   SETON                                        RT
068400930323     C                   END
068500941227     C********************************************************************
068600941227     C*   LETARB - LEGGE IL FILE FNARB (BOLLE ARRIVI)
068700941227     C********************************************************************
068800941227     C     LETARB        BEGSR
068900941227     C*
069000140820     C* LEGGO IL FILE FNARB01L
069100941227     C                   Z-ADD     D98AAS        ARBAAS
069200941227     C                   Z-ADD     D98LNP        ARBLNP
069300941227     C                   Z-ADD     D98NRS        ARBNRS
069400941227     C                   Z-ADD     D98NSP        ARBNSP
069500941228     C     KEYARB        CHAIN     FNARB01L                           99
069600941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
069700941227     C                   MOVEL     '1'           D98ERR
069800941227     C                   MOVEL     MSG(20)       D98MSG                         *BOLLA INESISTENTE
069900941227     C                   END                                                    <-- 1 ---
070000941228     C     D98ERR        IFEQ      '0'                                          --- 1 --> *TROVATA
070100941227     C                   MOVEL     ARBTSP        D98TSP
070200140820     c* Se forzato da Ds lo imposto al posto di quello della bolla
070300130529     c                   if        i98tsp_for<>' '
070400130529     C                   MOVEL     i98tsp_for    D98TSP
070500130529     c                   endif
070600941228     C                   MOVEL     ARBNZD        D98NZD
070700941227     C                   MOVEL     ARBCAD        D98CAD
070800970701     C                   MOVEL     ARBLOD        D98LOD
070900941227     C                   Z-ADD     ARBTFP        D98TFP
071000941227     C                   Z-ADD     ARBTFA        D98TFA
071100941227     C                   Z-ADD     ARBLNA        D98LNA
071200941227     C                   Z-ADD     ARBAAS        DSAAS
071300941227     C                   Z-ADD     ARBMGS        DSMGS
071400941227     C                   Z-ADD     DSDAT8        D98DSP
071500941227     C                   Z-ADD     ARBMGS        D98MGS
071600941227     C                   Z-ADD     ARBDTI        D98DTI
071700941227     C                   Z-ADD     ARBHTI        D98HTI
071800941227     C                   Z-ADD     ARBDCM        D98DCM
071900941227     C                   Z-ADD     ARBHMC        D98HMC
072000941227     C                   MOVEL     ARBGC1        D98GC1
072100941227     C                   MOVEL     ARBGC2        D98GC2
072200941227     C                   Z-ADD     ARBDCR        D98DCR
072300941227     C                   Z-ADD     ARBHCR        D98HCR
072400100308     C                   MOVEL     ARBTCR        D98TCR
072500100308     c* salvo i campi perchè in alcuni casi li clearo
072600100308     C                   Z-ADD     ARBDCR        s98DCR
072700100308     C                   Z-ADD     ARBHCR        s98HCR
072800100308     C                   MOVEL     ARBTCR        s98TCR
072900100308     c
073000941227     C                   MOVEL     ARBCMC        D98CMC
073100170818     C                   MOVEL     ARBntc        wd98ntc_con
073200941227     C                   MOVEL     ARBFFD        D98FFD
073300941227     C                   Z-ADD     ARBNCD        D98NCD
073400941227     C                   MOVEL     ARBFNS        D98FNS
073500941227     C                   Z-ADD     ARBDUC        D98DUC
073600150508     C                   Z-ADD     ARBDPC        W98DPC
073700970310     C                   Z-ADD     *ZEROS        W98HUC
073800071122     c
073900071122     c                   movel     arbcbo        d98cbo
074000140820     c                   z-add     arbddc        d98ddc
074100140825     c                   z-add     arbndc        wd98ndc
074200140911     c                   movel     arbfbc        wd98fbc
074300150119     c                   movel     arbacc        wd98acc
074400150325     c                   exsr      IN_dist
074500150331     c* Se in fermo deposito vedo se presente particlarità consegna
074600150331     c                   if        arbffd='S' and arbgma<>*blanks
074700150331     C                   Z-ADD     CODUT         KTBUTE
074800150331     C                   MOVEL     '7R'          KTBCOD
074900150331     C                   MOVEL(P)  arbgma        KTBKEY
075000150331     C     KEYTAB        chain     TABEL00F
075100150331     c                   if        %found(tabel00f)
075200150331     c                   movel     tbluni        ds7r
075300150331     c                   endif
075400150331     c                   endif
075500150325     c
075600070511     c
075700141215     C                   Z-ADD     ARBDBR        D98DBR
075800170104     c
075900170105     C                   Z-ADD     ARBDBR        d98dfv
076000141215     c                   eval      d98fdrt=' '
076100070511     c
076200141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
076300070511     c* La data ritiro è presente solo su fnblp
076400070511     C     KEYARB        CHAIN     FNblp01L                           99
076500141215     c                   if        %found(fnblp01l)
076600070511     C                   Z-ADD     blpDrt        D98DBR
076700070511     c                   eval      d98fdrt='S'
076800070511     c                   endif
076900141215     c******             endif
077000070511     c
077100110923     C                   movel     ARBxco        D98xco
077200110923     C                   Z-ADD     ARBDAM        D98DAM
077300941227     C                   MOVEL     ARBFIN        D98FIN
077400990308     C                   MOVEL     ARBFBR        WFBR
077500031201     C                   MOVEL     ARBGGA        WGGA
077600050901     C                   MOVEL     ARBcca        d98cca
077700051124     C                   MOVEL     ARBTC1        d98TC1
077800051124     C                   MOVEL     ARBTC2        d98TC2
077900941227     C                   Z-ADD     1             I
078000941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
078100941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
078200941227     C                   MOVEL     '1'           D98ERR
078300941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
078400941227     C                   ELSE                                                   <-- 2 -->
078500941227     C                   MOVEL     T3A(I)        D98TBL
078600941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
078700990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
078800941227     C                   END                                                    <-- 2 ---
078900130826
079000130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
079100130826     c                   if        %subst(d98tbl:1:1) ='F'
079200130826     C                   Z-ADD     ARBKSC        D98KSC
079300130826     c                   else
079400130826     c
079500130826     c                   if        arbccm>0
079600130826     C                   Z-ADD     ARBccm        D98KSC
079700130826     c                   endif
079800130826     c                   if        d98ksc=0
079900130826     c                   z-add     8888          d98ksc
080000130826     c                   movel     d98lnp        d98ksc
080100130826     c                   endif
080200130826     c                   endif
080300130826     c
080400941227     C                   END                                                    <-- 1 ---
080500941228     C*
080600051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
080700950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
080800941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
080900941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
081000941228     C                   Z-ADD     D98AAS        AR9AAS
081100941228     C                   Z-ADD     D98LNP        AR9LNP
081200941228     C                   Z-ADD     D98NRS        AR9NRS
081300941228     C                   Z-ADD     D98NSP        AR9NSP
081400051115     C     KEYAR9        CHAIN     FiAR901L                           99
081500950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
081600950102     C                   MOVEL     'S'           WFLASS
081700950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
081800950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
081900950102     C                   Z-ADD     D98AAS        LBLAAN
082000950102     C                   Z-ADD     D98LNP        LBLLPN
082100950102     C                   Z-ADD     D98NRS        LBLNRN
082200950102     C                   Z-ADD     D98NSP        LBLNSN
082300950102     C     KEYLBL        CHAIN     FNLBL01L                           99
082400950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
082500950102     C                   Z-ADD     LBLAAP        AR9AAS
082600950102     C                   Z-ADD     LBLLPP        AR9LNP
082700950102     C                   Z-ADD     LBLNRP        AR9NRS
082800950102     C                   Z-ADD     LBLNSP        AR9NSP
082900051115     C     KEYAR9        CHAIN     FiAR901L                           99
083000950102     C     *IN99         IFEQ      '0'
083100950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
083200950102     C                   ENDIF
083300950102     C                   ENDIF                                                  <-- 3 ---
083400950102     C                   ENDIF                                                  <-- 2 ---
083500950102     C*
083600950102     C* CONTRASSEGNO NON TROVATO
083700950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
083800941228     C                   ELSE                                                   <-- 2 -->
083900950102     C* CONTRASSEGNO TROVATO
084000941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
084100010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
084200010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
084300941228     C                   ENDIF                                                  <-- 2 ---
084400941228     C                   ENDIF                                                  <-- 1 ---
084500150325     c
084600941227     C*
084700050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
084800941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
084900050405     C                   Z-ADD     D98AAS        KWGCPAAS
085000050405     C                   Z-ADD     D98LNP        KWGCPLNP
085100050405     C                   Z-ADD     D98NRS        KWGCPNRS
085200050405     C                   Z-ADD     D98NSP        KWGCPNSP
085300050405     C                   Z-ADD     *ZEROS        KWGCPFRG
085400050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
085500941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
085600941227     C     GCPATB        ANDNE     'A'
085700960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
085800960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
085900960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
086000161107$$$$ C                   movel     gcpfas        D98gfas
086100050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
086200960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
086300960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
086400050405     C* O UGUALE A 35 INVECE DI 40)
086500960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
086600050405     C     GCPFAS        ANDGE     40
086700941227     C                   Z-ADD     GCPDCG        D98DCG
086800950112     C                   Z-ADD     GCPDCG        W98DCG
086900960123     C                   ENDIF
087000960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
087100960729     C     GCPFAS        ANDGE     35
087200070202     C     GCPFAS        ANDne     36
087300960729     C                   Z-ADD     GCPDCG        D98DCG
087400960729     C                   Z-ADD     GCPDCG        W98DCG
087500960729     C                   ENDIF
087600941228     C                   ENDIF
087700941228     C                   ENDIF                                                  <-- 1 ---
087800970911     C*
087900970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
088000970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
088100970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
088200050922     C*****              Z-ADD     D98DTI        W98DTI
088300971210     C                   Z-ADD     D98LNP        F
088400971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
088500970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
088600970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
088700150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
088800150120     c                   if        arbdcp=0 and d98dfc=0
088900150120     C                   Z-ADD     d98dam        D98DTI
089000150120     c                   endif
089100150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
089200150120     C                   Z-ADD     d98dam        D98DTI
089300150120     c                   endif
089400150120     c                   ENDIF
089500150120     c***
089600150120     C***                Z-ADD     D98AAS        ARTAAS
089700150120     C***                Z-ADD     D98LNP        ARTLNP
089800150120     C***                Z-ADD     D98NRS        ARTNRS
089900150120     C***                Z-ADD     D98NSP        ARTNSP
090000150120     C***  KEYART        SETLL     FNART01L                           99
090100150120     C**N99KEYART        READE     FNART01L                               99
090200150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
090300150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
090400970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
090500150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
090600970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
090700970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
090800150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
090900150120     C***  ARTDCM        ANDEQ     *ZEROS
091000970912     C* SE PRIMO REC. TROVATO, MEM. DATA
091100150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
091200150120     C***                Z-ADD     ARTDAM        D98DTI
091300150120     C***                ELSE                                                   <-- 6 -->
091400970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
091500150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
091600150120     C***                Z-ADD     ARTDAM        D98DTI
091700150120     C***                ENDIF                                                  <-- 7 ---
091800150120     C***                ENDIF                                                  <-- 6 ---
091900150120     C***                ENDIF                                                  <-- 5 ---
092000970911     C* C'E' DATA CONSEGNAB.
092100150120     C***                ELSE                                                   <-- 4 -->
092200970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
092300970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
092400150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
092500150120     C***  ARTDAM        ANDLE     D98DFC
092600970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
092700970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
092800150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
092900150120     C***  ARTDCM        ORGT      D98DFC
093000970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
093100150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
093200150120     C***                Z-ADD     ARTDAM        D98DTI
093300150120     C***                ELSE                                                   <-- 7 -->
093400970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
093500150120     C***  ARTDAM        IFLT      D98DTI
093600150120     C***                Z-ADD     ARTDAM        D98DTI
093700150120     C***                ENDIF
093800150120     C***                ENDIF                                                  <-- 7 ---
093900150120     C***                ENDIF                                                  <-- 6 ---
094000150120     C***                ENDIF                                                  <-- 5 ---
094100150120     C***                ENDIF                                                  <-- 4 ---
094200150120     C***                ENDIF                                                  <-- 3 ---
094300150120     C***  KEYART        READE     FNART01L                               99
094400150120     C***                ENDDO                                                  <-- 2 ---
094500150120     C***                ENDIF                                                  <-- 1 ---
094600941227     C*
094700941227     C                   ENDSR
094800150325     C********************************************************************
094900150325     C*   Verifico se bolla in distinta
095000150325     C********************************************************************
095100150325     c     IN_dist       BEGSR
095200150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
095300150325     c                   if         ARBfbc = *blanks and
095400150325     c                               ARBddc > 0 and
095500150325     c                               ARBndc > 0 and ARBdcm=0
095600150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
095700150325     c                   if        not %open(fiapd01l)
095800150325     c                   open      fiapd01l
095900150325     c                   endif
096000150325     c
096100150325     C                   move      'A'           kpdtip
096200150325     c                   eval      kpdpdr=arbpdc
096300150325     C     kfapd         chain     fiapd01l
096400150325     c
096500150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
096600150325     c                   eval       d98cons = 'S'
096700160506     c                   else
096800160506     c                   eval       d98disf = 'F'
096900150325     c                   endif
097000150325     c                   endif
097100150325     c                   ENDSR
097200941227     C********************************************************************
097300941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
097400941227     C********************************************************************
097500941227     C     LETBLP        BEGSR
097600941227     C*
097700941227     C                   Z-ADD     D98AAS        BLPAAS
097800941227     C                   Z-ADD     D98LNP        BLPLNP
097900941227     C                   Z-ADD     D98NRS        BLPNRS
098000941227     C                   Z-ADD     D98NSP        BLPNSP
098100941227     C     KEYBLP        CHAIN     FNBLP01L                           99
098200941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
098300941227     C                   MOVEL     '1'           D98ERR
098400941227     C                   MOVEL     MSG(20)       D98MSG
098500941227     C                   END                                                    <-- 1 ---
098600941228     C     *IN99         IFEQ      '0'                                                    *TROV.
098700041028     C     KEYBLP        CHAIN     FNarb01L
098800041028     c                   if        %found(fnarb01l)
098900041028     c                   eval      wesarb='S'
099000041028     c                   else
099100041028     c                   eval      wesarb='N'
099200150219     c                   eval      wtrasm='D'
099300041028     c                   endif
099400141216     c
099500141216     c* se non trasmessa la bolla in arrivo memorizzo
099600150219     c                   if        blpft1= ' '
099700141229     c                   eval      wtrasm='N'
099800141216     c                   endif
099900141216     c
100000110923     C                   MOVEL     BLPxco        D98xco
100100110923     C                   MOVEL     BLPTSP        D98TSP
100200130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
100300130529     c                   if        i98tsp_for<>' '
100400130529     C                   MOVEL     i98tsp_for    D98TSP
100500130529     c                   endif
100600960209     C                   MOVEL     BLPNZD        D98NZD
100700941228     C                   MOVEL     BLPCAD        D98CAD
100800970701     C                   MOVEL     BLPLOD        D98LOD
100900941227     C                   Z-ADD     BLPTFP        D98TFP
101000941227     C                   Z-ADD     BLPTFA        D98TFA
101100941227     C                   Z-ADD     BLPLNA        D98LNA
101200941227     C                   Z-ADD     BLPAAS        DSAAS
101300941227     C                   Z-ADD     BLPMGS        DSMGS
101400941227     C                   Z-ADD     DSDAT8        D98DSP
101500941227     C                   Z-ADD     BLPMGS        D98MGS
101600170104
101700170104     c* se si tratta di prepagato NON confermato, devo fare finta che sia bollettato oggi
101800170104     c                   if        wtrasm='N'  and blppdr=0
101900170104     c                   movel     wdaodi        d98dsp
102000170104     c                   z-add     wdaodi        d98mgs
102100170104     c                   endif
102200170104     c
102300051124     C                   movel     BLPTC1        D98TC1
102400051124     C                   movel     BLPTC2        D98TC2
102500041028     c                   if        wesarb='S'
102600041028     C                   Z-ADD     arbDTI        D98DTI
102700041028     C                   Z-ADD     arbHTI        D98HTI
102800041028     C                   Z-ADD     arbDCM        D98DCM
102900041028     C                   Z-ADD     arbHMC        D98HMC
103000041028     C                   Z-ADD     arbDAM        D98DAM
103100050901     C                   MOVEL     arbcca        d98cca
103200140820     C                   MOVEL     arbddc        d98ddc
103300140911     C                   z-add     arbndc        wd98ndc
103400140910     C                   MOVEL     arbfbc        wd98fbc
103500150119     c                   movel     arbacc        wd98acc
103600170818     c                   movel     arbcmc        d98cmc
103700170818     C                   MOVEL     ARBntc        wd98ntc_con
103800150325     c                   exsr      IN_DIST
103900140820     c*
104000041028     c                   else
104100941227     C                   Z-ADD     BLPDTI        D98DTI
104200941227     C                   Z-ADD     BLPHTI        D98HTI
104300941227     C                   Z-ADD     BLPDCM        D98DCM
104400941227     C                   Z-ADD     BLPHMC        D98HMC
104500041028     C                   Z-ADD     BLPDAM        D98DAM
104600050901     C                   MOVEL     BLPcca        d98cca
104700140820     C                   MOVEL     BLPddc        d98ddc
104800140825     C                   MOVEL     BLPndc        wd98ndc
104900140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
105000140820     c                   clear                   d98cons
105100170818     C                   MOVEL     *BLANKS       D98CMC
105200170821     c* in partenza non posso determinale i tentivi di consegna
105300170818     C                   clear                   wd98ntc_con
105400041028     c                   endif
105500941227     C                   MOVEL     BLPGC1        D98GC1
105600941227     C                   MOVEL     BLPGC2        D98GC2
105700941227     C                   Z-ADD     BLPDCR        D98DCR
105800941227     C                   Z-ADD     BLPHCR        D98HCR
105900941227     C                   MOVEL     BLPTCR        D98TCR
106000100308     c* Salvo i campi perchè in alcuni casi li clearo
106100100308     C                   Z-ADD     blpDCR        s98DCR
106200100308     C                   Z-ADD     blpHCR        s98HCR
106300100308     C                   MOVEL     blpTCR        s98TCR
106400100308     c
106500941227     C                   MOVEL     BLPFFD        D98FFD
106600941227     C                   Z-ADD     BLPNCD        D98NCD
106700941227     C                   MOVEL     BLPFNS        D98FNS
106800941228     C                   Z-ADD     BLPDUC        D98DUC
106900150508     C                   Z-ADD     BLPDPC        w98DPC
107000970310     C                   Z-ADD     *ZEROS        W98HUC
107100071122     c                   movel     blpcbo        d98cbo
107200150331     c*
107300150331     c* Se in fermo deposito vedo se presente particlarità consegna
107400150331     c                   if        blpffd='S' and blpgma<>*blanks
107500150331     C                   Z-ADD     CODUT         KTBUTE
107600150331     C                   MOVEL     '7R'          KTBCOD
107700150331     C                   MOVEL(P)  blpgma        KTBKEY
107800150331     C     KEYTAB        chain     TABEL00F
107900150331     c                   if        %found(tabel00f)
108000150331     c                   movel     tbluni        ds7r
108100150331     c                   endif
108200150331     c                   endif
108300150331     c
108400970304     C                   Z-ADD     BLPDBR        D98DBR
108500170105     C                   Z-ADD     BLPDBR        d98dfv
108600070514     c                   eval      d98fdrt=' '
108700070514     c
108800141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
108900070514     C                   Z-ADD     blpDrt        D98DBR
109000070514     c                   eval      d98fdrt='S'
109100141215     c********           endif
109200070514     c*
109300941227     C                   MOVEL     BLPFIN        D98FIN
109400130826     C****               Z-ADD     BLPKSC        D98KSC
109500990308     C                   MOVEL     BLPFBR        WFBR
109600031201     C                   MOVEL     BLPGGA        WGGA
109700941227     C                   Z-ADD     1             I
109800941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
109900941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
110000941227     C                   MOVEL     '1'           D98ERR
110100941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
110200941227     C                   ELSE                                                   <-- 2 -->
110300941227     C                   MOVEL     T3A(I)        D98TBL
110400941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
110500990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
110600941227     C                   END                                                    <-- 2 ---
110700130826     c                   if        %subst(d98tbl:1:1) ='F'
110800130826     C                   Z-ADD     blpKSC        D98KSC
110900130826     c                   else
111000130826     c
111100130826     c                   if        blpccm>0
111200130826     C                   Z-ADD     blpccm        D98KSC
111300130826     c                   endif
111400130826     c                   if        d98ksc=0
111500130826     c                   z-add     8888          d98ksc
111600130826     c                   movel     d98lnp        d98ksc
111700130826     c                   endif
111800130826     c                   endif
111900130826     c
112000941227     C                   END
112100941228     C*
112200051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
112300950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
112400941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
112500941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
112600051115     C                   Z-ADD     D98AAS        ar9AAS
112700051115     C                   Z-ADD     D98LNP        ar9LNP
112800051115     C                   Z-ADD     D98NRS        ar9NRS
112900051115     C                   Z-ADD     D98NSP        ar9NSP
113000051115     C     KEYar9        CHAIN     Fiar901L                           99
113100950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
113200950102     C                   MOVEL     'S'           WFLASS
113300950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
113400950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
113500950102     C                   Z-ADD     D98AAS        LBLAAN
113600950102     C                   Z-ADD     D98LNP        LBLLPN
113700950102     C                   Z-ADD     D98NRS        LBLNRN
113800950102     C                   Z-ADD     D98NSP        LBLNSN
113900950102     C     KEYLBL        CHAIN     FNLBL01L                           99
114000950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
114100051115     C                   Z-ADD     LBLAAP        ar9AAS
114200051115     C                   Z-ADD     LBLLPP        ar9LNP
114300051115     C                   Z-ADD     LBLNRP        ar9NRS
114400051115     C                   Z-ADD     LBLNSP        ar9NSP
114500051115     C     KEYar9        CHAIN     Fiar901L                           99
114600950102     C     *IN99         IFEQ      '0'
114700950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
114800950102     C                   ENDIF
114900950102     C                   ENDIF                                                  <-- 3 ---
115000950102     C                   ENDIF                                                  <-- 2 ---
115100950102     C*
115200950102     C* CONTRASSEGNO TROVATO
115300150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
115400051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
115500051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
115600051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
115700941228     C                   ENDIF                                                  <-- 2 ---
115800941228     C                   ENDIF                                                  <-- 1 ---
115900941227     C*
116000050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
116100941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
116200050405     C                   Z-ADD     D98AAS        KWGCPAAS
116300050405     C                   Z-ADD     D98LNP        KWGCPLNP
116400050405     C                   Z-ADD     D98NRS        KWGCPNRS
116500050405     C                   Z-ADD     D98NSP        KWGCPNSP
116600050405     C                   Z-ADD     *ZEROS        KWGCPFRG
116700050405     C     KEYGCP        CHAIN     TIGCP01L                           99
116800941228     C     *IN99         IFEQ      '0'                                          *TROV.
116900941227     C     GCPATB        ANDNE     'A'
117000960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
117100960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
117200960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
117300960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
117400161107$$$$ C                   movel     gcpfas        D98Gfas
117500050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
117600960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
117700960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
117800050405     C* O UGUALE A 35 INVECE DI 40)
117900960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
118000050405     C     GCPFAS        ANDGE     40
118100941227     C                   Z-ADD     GCPDCG        D98DCG
118200950112     C                   Z-ADD     GCPDCG        W98DCG
118300960123     C                   ENDIF
118400960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
118500960729     C     GCPFAS        ANDGE     35
118600070202     C     GCPFAS        ANDne     36
118700960729     C                   Z-ADD     GCPDCG        D98DCG
118800960729     C                   Z-ADD     GCPDCG        W98DCG
118900960729     C                   ENDIF
119000941228     C                   ENDIF
119100941228     C                   ENDIF                                                  <-- 1 ---
119200970912     C*
119300970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
119400970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
119500970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
119600050922     C******             Z-ADD     D98DTI        W98DTI
119700971210     C                   Z-ADD     D98LNP        F
119800971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
119900970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
120000970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
120100150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
120200150120     c                   if        blpdcp=0 and d98dfc=0
120300150120     C                   Z-ADD     d98dam        D98DTI
120400150120     c                   endif
120500150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
120600150120     C                   Z-ADD     d98dam        D98DTI
120700150120     c                   endif
120800150120     c                   ENDIF
120900150120     c*
121000150120     C***                Z-ADD     D98AAS        BLTAAS
121100150120     C****               Z-ADD     D98LNP        BLTLNP
121200150120     C***                Z-ADD     D98NRS        BLTNRS
121300150120     C***                Z-ADD     D98NSP        BLTNSP
121400150120     C***  KEYBLT        SETLL     FNBLT01L                           99
121500150120     C**N99KEYBLT        READE     FNBLT01L                               99
121600150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
121700150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
121800970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
121900150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
122000970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
122100970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
122200150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
122300150120     C**   BLTDCM        ANDEQ     *ZEROS
122400970912     C* SE PRIMO REC. TROVATO, MEM. DATA
122500150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
122600150120     C**                 Z-ADD     BLTDAM        D98DTI
122700150120     C**                 ELSE                                                   <-- 6 -->
122800970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
122900150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
123000150120     C**                 Z-ADD     BLTDAM        D98DTI
123100150120     C**                 ENDIF                                                  <-- 7 ---
123200150120     C**                 ENDIF                                                  <-- 6 ---
123300150120     C**                 ENDIF                                                  <-- 5 ---
123400970912     C* C'E' DATA CONSEGNAB.
123500150120     **                  ELSE                                                   <-- 4 -->
123600970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
123700970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
123800150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
123900150120     C**   BLTDAM        ANDLT     D98DFC
124000970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
124100970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
124200150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
124300150120     C**   BLTDCM        ORGT      D98DFC
124400970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
124500150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
124600150120     C**                 Z-ADD     BLTDAM        D98DTI
124700150120     C**                 ELSE                                                   <-- 7 -->
124800970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
124900150120     C**   BLTDAM        IFLT      D98DTI
125000150120     C**                 Z-ADD     BLTDAM        D98DTI
125100150120     C**                 ENDIF
125200150120     C**                 ENDIF                                                  <-- 7 ---
125300150120     C**                 ENDIF                                                  <-- 6 ---
125400150120     C**                 ENDIF                                                  <-- 5 ---
125500150120     C**                 ENDIF                                                  <-- 4 ---
125600150120     C**                 ENDIF                                                  <-- 3 ---
125700150120     C**   KEYBLT        READE     FNBLT01L                               99
125800150120     C**                 ENDDO                                                  <-- 2 ---
125900150120     C**                 ENDIF                                                  <-- 1 ---
126000941227     C*
126100941227     C                   ENDSR
126200930323     C********************************************************************
126300941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
126400930323     C********************************************************************
126500930323     C     RTTEOR        BEGSR
126600941227     C*
126700941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
126800941228     C     D98ERR        IFEQ      '0'
126900941227     C                   EXSR      RTPART
127000941227     C                   ENDIF
127100941227     C*
127200941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
127300941228     C     D98ERR        IFEQ      '0'
127400150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
127500150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
127600150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
127700150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
127800150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
127900941227     C                   EXSR      RTTRAZ
128000150219     c                   if        d98err<>'1'
128100150219     C                   Z-ADD     CASTIN        D98TTI
128200150219     C                   Z-ADD     CASTCN        D98TTC
128300150219     C                   endif
128400150219     C                   ENDIF
128500941227     C*
128600941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
128700941228     C     D98ERR        IFEQ      '0'
128800941227     C                   EXSR      RTCONS
128900941227     C                   ENDIF
129000941227     C*
129100941228     C     D98ERR        IFEQ      '0'
129200941227     C*
129300941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
129400941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
129500941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
129600090921     c                   z-add     d98tri        w99tri
129700090921     c* per la DS
129800090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
129900090921     c* per i calcoli interi
130000090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
130100090921     c
130200941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
130300941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
130400941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
130500090921     c                   z-add     d98trc        w99trc
130600090921     c* per la DS
130700090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
130800090921     c* per i calcoli interi
130900090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
131000941227     C*
131100941227     C                   ENDIF
131200930323     C*
131300930323     C                   ENDSR
131400941227     C********************************************************************
131500941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
131600941227     C********************************************************************
131700941227     C     RTPART        BEGSR
131800941228     C*
131900941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
132000941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
132100941228     C                   MOVEL     D98TSP        WTSP              1
132200130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
132300130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
132400050301     C*******
132500050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
132600050301     C*
132700050301     C*    W001          IFEQ      'A'
132800050301     C*    W0040         OREQ      8888
132900050301     C*                  MOVEL     'E'           WTSP
133000050301     C*                  ENDIF
133100050301     C*******
133200941227     C*
133300941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
133400941227     C                   MOVEL     'N'           WTRAOK            1
133500941227     C                   MOVEL     'P'           CASPTA
133600941227     C                   Z-ADD     D98LNP        CASLNP
133700941227     C                   Z-ADD     *ZEROS        CASLNA
133800941228     C                   MOVEL     WTSP          CASTSP
133900941228     C                   Z-ADD     99999999      CASDDE
134000941227     C     KEYCAS        SETGT     AZCAS01L                             99
134100941227     C  N99              READP     AZCAS01L                               99
134200941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
134300941227     C     WTRAOK        ANDEQ     'N'
134400941227     C     CASLNP        ANDEQ     D98LNP
134500950102     C     CASTSP        ANDEQ     WTSP
134600941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
134700941227     C     CASATB        ANDEQ     *BLANKS
134800941227     C                   MOVEL     'S'           WTRAOK
134900941227     C                   ELSE                                                   <-- 2 -->
135000941227     C                   READP     AZCAS01L                               99
135100941227     C                   ENDIF                                                  <-- 2 ---
135200941227     C                   ENDDO                                                  <-- 1 ---
135300941227     C*
135400941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
135500941227     C     WTRAOK        IFEQ      'N'
135600941227     C                   MOVEL     '1'           D98ERR
135700941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
135800941227     C                   ELSE
135900941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
136000941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
136100941227     C                   END
136200970214     C*
136300140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
136400140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
136500140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
136600140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
136700140403     C**                 Z-ADD     D98LNA        F
136800140403     C**   D98ERR        IFEQ      '0'
136900140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
137000140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
137100140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
137200140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
137300140403     C**                 ENDIF
137400140403     C**                 ENDIF
137500140402     c
137600140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
137700140402     c*  se tempi  = 0
137800140402     c                   if        wpick2='S' and d98tpi<=0
137900140402     c                   add       24            D98TPI
138000140402     c                   endif
138100941227     C*
138200941227     C                   ENDSR
138300941227     C********************************************************************
138400941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
138500941227     C********************************************************************
138600941227     C     RTTRAZ        BEGSR
138700941227     C*
138800941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
138900150219     c
139000150219     C                   Z-ADD     D98LNA        LTA(1)
139100150219     C                   Z-ADD     D98LNA        LTA(2)
139200150219     C                   Z-ADD     D98TFA        LTA(3)
139300941227     C                   Z-ADD     D98TFA        LTA(4)
139400941227     C*
139500941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
139600941227     C                   MOVEL     'N'           WTRAOK            1
139700941227     C                   Z-ADD     1             K                 3 0
139800941227     C*
139900941227     C* CICLO FINO A:
140000941227     C*    - FINE SCHIERA
140100941227     C*    - TROVATO TEMPI VALIDI
140200941227     C     K             DOUGT     4                                            --- 1 -->
140300941227     C     WTRAOK        OREQ      'S'
140400941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
140500941227     C     LTA(K)        ORNE      *ZEROS
140600941227     C                   MOVEL     'T'           CASPTA
140700941227     C                   Z-ADD     LTP(K)        CASLNP
140800941227     C                   Z-ADD     LTA(K)        CASLNA
140900941227     C                   MOVEL     D98TSP        CASTSP
141000941228     C                   Z-ADD     99999999      CASDDE
141100941227     C     KEYCAS        SETGT     AZCAS01L                             99
141200941227     C  N99              READP     AZCAS01L                               99
141300941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
141400941227     C     WTRAOK        ANDEQ     'N'
141500941227     C     LTP(K)        ANDEQ     CASLNP
141600941227     C     LTA(K)        ANDEQ     CASLNA
141700941227     C     D98TSP        ANDEQ     CASTSP
141800960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
141900941227     C     CASATB        ANDEQ     *BLANKS
142000941227     C                   MOVEL     'S'           WTRAOK
142100941227     C                   ELSE                                                   <-- 4 -->
142200941227     C                   READP     AZCAS01L                               99
142300941227     C                   ENDIF                                                  <-- 4 ---
142400941227     C                   ENDDO                                                  <-- 3 ---
142500941227     C                   ENDIF                                                  <-- 2 ---
142600941227     C                   ADD       1             K
142700941227     C                   ENDDO                                                  <-- 1 ---
142800941227     C*
142900941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
143000941227     C     WTRAOK        IFEQ      'N'
143100941227     C                   MOVEL     '1'           D98ERR
143200941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
143300150219     C***                ELSE
143400150219     C***                Z-ADD     CASTIN        D98TTI
143500150219     C***                Z-ADD     CASTCN        D98TTC
143600941227     C                   END
143700941227     C*
143800941227     C                   ENDSR
143900941227     C********************************************************************
144000941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
144100941227     C********************************************************************
144200941227     C     RTCONS        BEGSR
144300150629     c                   clear                   WTsogliaE         1
144400970701     C*
144500970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
144600970701     C                   CLEAR                   SI95
144700970701     C                   MOVEL     '3'           I95TCN
144800970701     C                   MOVEL     D98NZD        I95NAR
144900970701     C                   MOVEL     D98CAD        I95CAP
145000970701     C                   MOVEL     D98LOD        I95LOC
145100970701     C                   MOVEL     D98DSP        I95DAT
145200970701     C                   MOVEL     D98TSP        I95TSP
145300980602     C                   MOVEL     'S'           I95FRT
145400150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
145500150511     c                   if        d98ffd='S'
145600150511     c                   eval      i95tsp='C'
145700150511     c                   endif
145800020226     C*
145900020226     C* VERIFICO SE LNA E' FILIALE FEDEX
146000020226     C                   IF        D98LNA > *ZEROS
146100020226     C     D98LNA        LOOKUP    FILFED                                 55
146200020226     C                   IF        *IN55 = *ON
146300020226     C                   EVAL      I95FI1 = 'S'
146400020226     C                   ENDIF
146500020226     C                   ENDIF
146600020226     C*
146700020226     C                   CALL      'TISI95R'
146800970701     C                   PARM                    SI95
146900941227     C*
147000941228     C* SE NON TROVATO, ERRORE
147100970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
147200941227     C                   MOVEL     '1'           D98ERR
147300970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
147400941227     C                   ELSE                                                   <-- 1 -->
147500941227     C*
147600941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
147700941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
147800941227     C     CASTIN        DIV       24            WDIV              3 0
147900941227     C                   MVR                     WRES              3 0
148000941227     C     WRES          IFEQ      *ZEROS
148100970701     C                   Z-ADD     O950IN        D98TCI
148200941227     C                   ELSE
148300970701     C                   Z-ADD     O951IN        D98TCI
148400941227     C                   END
148500941227     C*
148600941227     C     CASTCN        DIV       24            WDIV                     99
148700941227     C                   MVR                     WRES
148800941227     C     WRES          IFEQ      *ZEROS
148900970701     C                   Z-ADD     O950CN        D98TCC
149000150629     c                   eval      wTSogliaE='0'
149100941227     C                   ELSE
149200970701     C                   Z-ADD     O951CN        D98TCC
149300150629     c                   eval      wTSogliaE='1'
149400941227     C                   END
149500941227     C*
149600150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
149700941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
149800150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
149900150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
150000150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
150100150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
150200150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
150300941227     C*
150400941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
150500941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
150600970701     C     O95ISO        IFEQ      'I'
150700941227     C     D98FIN        OREQ      'I'
150800970701     C                   ADD       O95TIS        D98TCI
150900970701     C                   ADD       O95TIS        D98TCC
151000941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
151100970701     C                   ADD       O95TIS        W990IN
151200970701     C                   ADD       O95TIS        W991IN
151300150629     C                   ADD       O95TIS        W990CN
151400150629     C                   ADD       O95TIS        W991CN
151500941227     C                   END
151600090827     c
151700090827     c* Sistemo i tempi di consegna se non sono multipli di 12
151800090827     c                   exsr      SisCONS
151900160404     c
152000160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
152100160404     c                   if        wVP='S'
152200160404     c                   add       24            w991IN
152300160404     c                   add       24            w991CN
152400160404     c                   add       24            w990IN
152500160404     c                   add       24            w990CN
152600160404     c                   endif
152700941228     C*
152800941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
152900941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
153000941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
153100941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
153200941228     C                   Z-ADD     D98TCC        W99CCE            3 0
153300941228     C                   ELSE                                                   <-- 3 -->
153400970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
153500970701     C                   CLEAR                   SI95
153600970701     C                   MOVEL     '3'           I95TCN
153700970701     C                   MOVEL     D98NZD        I95NAR
153800970701     C                   MOVEL     D98CAD        I95CAP
153900970701     C                   MOVEL     D98LOD        I95LOC
154000970701     C                   MOVEL     D98DSP        I95DAT
154100970701     C                   MOVEL     'E'           I95TSP
154200980602     C                   MOVEL     'S'           I95FRT
154300020226     C*
154400020226     C* VERIFICO SE LNA E' FILIALE FEDEX
154500020226     C                   IF        D98LNA > *ZEROS
154600020226     C     D98LNA        LOOKUP    FILFED                                 55
154700020226     C                   IF        *IN55 = *ON
154800020226     C                   EVAL      I95FI1 = 'S'
154900020226     C                   ENDIF
155000020226     C                   ENDIF
155100020226     C*
155200020226     C                   CALL      'TISI95R'
155300970701     C                   PARM                    SI95
155400970701     C*
155500941228     C* SE NON TROVATO, ERRORE
155600970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
155700941228     C                   MOVEL     '1'           D98ERR
155800970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
155900941228     C                   ELSE                                                   <-- 4 -->
156000941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
156100941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
156200941228     C     CASTCN        DIV       24            WDIV                     99
156300941228     C                   MVR                     WRES
156400941228     C     WRES          IFEQ      *ZEROS
156500970701     C                   Z-ADD     O950CN        W99CCE
156600941228     C                   ELSE
156700970701     C                   Z-ADD     O951CN        W99CCE
156800941228     C                   END
156900941228     C                   ENDIF                                                  <-- 4 ---
157000941228     C                   ENDIF                                                  <-- 3 ---
157100941228     C                   ENDIF                                                  <-- 2 ---
157200941227     C*
157300941227     C                   END                                                    <-- 1 ---
157400941227     C*
157500941227     C                   ENDSR
157600090827     C********************************************************************
157700090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
157800090827     C********************************************************************
157900090827     C     SisCONS       BEGSR
158000090827     c                   eval      w99tci=d98tci
158100090827     c                   if        d98tci=10
158200090827     c                   z-add     12            w99tci
158300090827     c                   endif
158400090827     c
158500090827     c                   eval      w99tcc=d98tcc
158600090827     c                   if        d98tcc=10
158700090827     c                   z-add     12            w99tcc
158800090827     c                   endif
158900160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
159000160404     c                   if        wVP='S'
159100160404     c                   add       24            w99tcc
159200160404     c                   add       24            w99tci
159300160404     c                   endif
159400090827     c
159500090827     C                   ENDSR
159600930420     C********************************************************************
159700930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
159800930420     C********************************************************************
159900930420     C     CAREVB        BEGSR
160000930420     C*
160100930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
160200930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
160300060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
160400930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
160500930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
160600060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
160700060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
160800930420     C                   Z-ADD     D98AAS        KEBAAS
160900930420     C                   Z-ADD     D98LNP        KEBLNP
161000930420     C                   Z-ADD     D98NRS        KEBNRS
161100930420     C                   Z-ADD     D98NSP        KEBNSP
161200990810     C     KEYEVB        SETLL     FNEVB01L                           99
161300060406     C  N99keyevb        READe     FNEVB01L                               99
161400060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
161500930421     C     D98ERR        ANDEQ     '0'
161600060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
161700930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
161800930421     C                   Z-ADD     1             I
161900930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
162000060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
162100930421     C                   MOVEL     '1'           D98ERR
162200930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
162300060406   x3C                   ELSE                                                   <-- 3 -->
162400060406     C                   MOVEL     D2A(I)        usa2A
162500060406     C                   MOVEL     D2Z(I)        usa2Z
162600930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
162700060406    4C     §2AFTC        IFEQ      'A'
162800930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
162900060406    4C                   END
163000060406    4C     §2AFTC        IFEQ      'G'
163100930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
163200060406    4C                   END
163300150216     c* Se si tratta di MIC verifico se uguale alla sped.
163400150303    4c                   if        evbcev='MIC' or evbcev='NIC'
163500150303    5c                   if        %subst(evbnot:1:7) >*zeros
163600150303     c
163700150303    6c                   if        evbcev='MIC' and
163800150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
163900150216     c                   eval      evemic='SI'
164000150303    6c                   endif
164100150303     c
164200150303    6c                   if        evbcev='NIC' and
164300150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
164400150216     c                   eval      evemic='  '
164500150303    6c                   endif
164600150303     c
164700150303   x5c                   else
164800150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
164900150303     c                   eval      evemic='SI'
165000150303    6c                   endif
165100150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
165200150303     c                   eval      evemic='  '
165300150303    6c                   endif
165400150303
165500150303    5c                   endif
165600150303    4c                   endif
165700150216     c
165800060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
165900930421     C     §2AFTC        OREQ      'G'
166000930421     C                   ADD       1             IE
166100990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
166200930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
166300930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
166400131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
166500090928     c
166600090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
166700091014     c**                 if        ie=1
166800091014     c**                 eval      Primotipo=§2aftc
166900091014     c**                 eval      Primodev=evbdev
167000091014     c**                 eval      Primohev=evbhev
167100091014     c**                 endif
167200090928     c
167300060406    4C                   END                                                    <-- 4 ---
167400060406     c* Verifico se si tratta di evento particolare
167500060406    4c                   if        §2zCEP<>' '
167600060526     c* non carico l'evento se cade in giorno festivo
167700060526     c                   z-add     evbdev        wpodata
167800060526     C                   EVAL      WTC='C'
167900060526     C                   EVAL      WIC='C'
168000060526     c                   eval      wslitt='N'
168100060526     c                   exsr      chkfes
168200060526     c
168300060526     c* Wslitt='N' non festivo
168400060526   4ac                   if        wslitt='N'
168500060526     c
168600060406    5c                   if        §2zfep='A'
168700060406     c                   eval      wevpaA='S'
168800060406    5C                   END                                                    <-- 3 ---
168900060406    5c                   if        §2zfep='G'
169000060406     c                   eval      wevpaG='S'
169100060406    5C                   END                                                    <-- 3 ---
169200060406     c                   add       1             ip
169300060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
169400060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
169500060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
169600060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
169700060526   4aC                   ENDif                                                  <-- 3 ---
169800060526    4C                   END                                                    <-- 3 ---
169900060406    3C                   END                                                    <-- 3 ---
170000060406    2C                   END                                                    <-- 2 ---
170100060406     C     keyevb        READe     FNEVB01L                               99
170200060406    1C                   ENDDO                                                  <-- 1 ---
170300050523     c* controllo riempimento skiere
170400050523     c                   clear                   trul0sds
170500050523     c                   eval      i0sski='EVD/EVO/EVC'
170600050523     c                   eval      i0sele=%elem(EVD)
170700050523     c                   eval      i0spie=ie
170800050523     c                   eval      i0sfile='FNEVB01L  '
170900050523     c                   eval      i0scod='     '
171000050523     c                   eval      i0spgm='TNSD99R'
171100050523     c                   movel     trul0sds      kpjbu
171200050523     c                   call      'TRUL0SR'
171300050523     c                   parm                    kpjba
171400930420     C*
171500060406     c* controllo riempimento skiere
171600060406     c                   clear                   trul0sds
171700060406     c                   eval      i0sski='PVD/PVO/PVC'
171800060406     c                   eval      i0sele=%elem(PVD)
171900060406     c                   eval      i0spie=ie
172000060406     c                   eval      i0sfile='FNEVB01L  '
172100060406     c                   eval      i0scod='     '
172200060406     c                   eval      i0spgm='TNSD99R'
172300060406     c                   movel     trul0sds      kpjbu
172400060406     c                   call      'TRUL0SR'
172500060406     c                   parm                    kpjba
172600060406     C*
172700930420     C                   ENDSR
172800931213     C********************************************************************
172900931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
173000931213     C********************************************************************
173100931213     C     CARTUC        BEGSR
173200931213     C*
173300931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
173400931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
173500931214     C     D98GC2        ANDEQ     *BLANKS
173600931213     C                   ELSE                                                   <-- 1 -->
173700931213     C*
173800931213     C                   MOVE      D98GC1        TUC(1)
173900931214     C                   MOVE      D98GC2        TUC(2)
174000931213     C*
174100931213     C                   END                                                    <-- 1 ---
174200931213     C*
174300931213     C                   ENDSR
174400970304     C********************************************************************
174500970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
174600970304     C********************************************************************
174700970304     C     DATRIF        BEGSR
174800970304     C*
174900050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
175000050922     c*  partenza
175100970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
175200970304     C*
175300970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
175400070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
175500070605     C***                MOVEL     '1'           D98ERR
175600070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
175700070605     C***                ELSE                                                   <-- 2 -->
175800970304     C*
175900970304     C                   Z-ADD     D98DRF        WD1
176000970304     C                   Z-ADD     D98ORF        WO1
176100970310     C* DATA PARTENZA ULTIMO COLLO
176200970310     C     D98DRF        IFLE      D98DUC
176300050928     c     d98duc        oreq      0
176400970310     C                   Z-ADD     D98DRF        D98DUC
176500970310     C                   Z-ADD     D98ORF        W98HUC
176600070605     C                   ENDIF
176700970304     C* DATA ARRIVO TRAZIONE
176800050922     C***                Z-ADD     D98DTI        WD2
176900050922     C***                Z-ADD     D98HTI        WO2
177000050922     C***  WDO1          IFLE      WDO2
177100050922     C***                Z-ADD     D98DRF        D98DTI
177200050922     C***                Z-ADD     D98ORF        D98HTI
177300050922     C***                ENDIF
177400970304     C* DATA CONSEGNA
177500050922     C***                Z-ADD     D98DCM        WD2
177600050922     C***                Z-ADD     D98HMC        WO2
177700050922     C***  WDO1          IFLE      WDO2
177800050922     C***                Z-ADD     D98DRF        D98DCM
177900050922     C***                Z-ADD     D98ORF        D98HMC
178000050922     C***                ENDIF
178100970304     C*
178200070605     C***                ENDIF                                                  <-- 2 ---
178300970304     C                   ENDIF                                                  <-- 1 ---
178400970304     C*
178500970304     C                   ENDSR
178600940901     C********************************************************************
178700940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
178800940901     C********************************************************************
178900940901     C     TDTIPO        BEGSR
179000940901     C*
179100940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
179200940902     C                   Z-ADD     D98TTI        W98TTI            3 0
179300941011     C*
179400941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
179500941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
179600941011     C     W98TTI        DIV       24            WDIV              3 0
179700941011     C                   MVR                     WRES              3 0
179800941011     C     WRES          IFEQ      *ZEROS
179900941011     C                   Z-ADD     *ZEROS        WASTA             9 0
180000941011     C                   ELSE
180100941011     C                   Z-ADD     12            WASTA
180200941011     C                   END
180300940901     C*
180400940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
180500940901     C     D98DTI        IFNE      *ZEROS
180600940901     C                   Z-ADD     D98DTI        WDAT
180700940901     C                   EXSR      DATORE
180800960911     C                   Z-ADD     WORE          WPODTI
180900960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
181000960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
181100960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
181200960911     C                   Z-ADD     WPODTI        W10POI            9 0
181300960911     C                   EXSR      R10FES
181400960911     C                   Z-ADD     W10POO        WPODTI
181500960911     C* AGGIUNGO LE ORA D'ASTA
181600960911     C                   ADD       WASTA         WPODTI
181700960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
181800960911     C     W10POI        IFEQ      W10POO
181900940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
182000940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
182100940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
182200940901     C                   EXSR      RO3ORE
182300940901     C                   ADD       WR3HHO        WPODTI
182400940901     C                   ADD       WR3HHO        W98TTI
182500940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
182600940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
182700940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
182800940901     C                   EXSR      RO3ORE
182900940901     C                   ADD       WR3HHO        WPODTI
183000940901     C                   ADD       WR3HHO        W98TTI
183100960911     C                   ENDIF                                                  *SE NON FESTIVO
183200940901     C                   END
183300940901     C*
183400940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
183500940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
183600940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
183700940901     C     W98TTI        DIV       24            WDIV              3 0
183800940901     C                   MVR                     WRES              3 0
183900940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
184000941228     C                   Z-ADD     W990IN        D98TCI
184100160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
184200160405     c*  per cui le sottraggo, per riagguingerle alla fine
184300160405     c                   if        wVP='S'
184400160405     c                   sub       24            D98TCI
184500160405     c                   endif
184600160405     c
184700150630     c                   eval      wTSogliaE='0'
184800940901     C                   ELSE                                                   <-- 2 -->
184900941228     C                   Z-ADD     W991IN        D98TCI
185000160405     c                   if        wVP='S'
185100160405     c                   sub       24            D98TCI
185200160405     c                   endif
185300150630     c                   eval      wTSogliaE='1'
185400940908     C                   ENDIF                                                  <-- 2 ---
185500090827     c* Sistemo i tempi di consegna se non sono multipli di 12
185600090827     c                   exsr      SisCONS
185700090827     c*
185800940908     C* RICALCOLO TEMPO DI RESA INTERNO
185900950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
186000950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
186100090921     c                   z-add     d98tri        w99tri
186200090921     c* per la DS
186300090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
186400090921     c* per i calcoli interni
186500090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
186600940908     C                   ENDIF                                                  <-- 1 ---
186700940901     C*
186800940901     C                   ENDSR
186900930325     C********************************************************************
187000930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
187100930325     C********************************************************************
187200930325     C     TDATPO        BEGSR
187300930406     C*
187301171107     c* Verifico se la data odierna è festivo
187302171107     c                   eval      wpodata=wdaodi
187303171107     c                   exsr      chkfes
187304171107     c*
187400930420     C* TRASFORMO LA DATA ODIERNA
187500930416     C                   Z-ADD     WDAODI        WDAT
187600930416     C                   EXSR      DATORE
187700050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
187800960911     C                   MOVEL     'OSC  '       WR3TAB
187900960911     C                   Z-ADD     WHHODI        WR3HHI
188000960911     C                   EXSR      RO3ORE
188100050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
188200050922     c                   if        wr3hho=12
188300050922     c     wpoatt        add       12            wpoattdata24
188400050922     c                   else
188500050922     c                   z-add     wpoatt        wpoattdata24
188600050922     c                   endif
188700930325     C*
188800930420     C* TRASFORMO LA DATA DI SPEDIZIONE
188900931125     C     D98DSP        IFEQ      *ZEROS
189000930326     C                   MOVEL     '1'           D98ERR
189100930326     C                   MOVEL     MSG(2)        D98MSG
189200930326     C                   ELSE
189300931125     C                   Z-ADD     D98DSP        WDAT
189400930325     C                   EXSR      DATORE
189500930406     C                   Z-ADD     WORE          WPODSP
189600930326     C                   END
189700930414     C*
189800930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
189900930414     C                   MOVEL     'LIM  '       WR3TAB
190000930414     C                   Z-ADD     *ZEROS        WR3HHI
190100930414     C                   EXSR      RO3ORE
190200931222     C     WR3HHO        MULT      10            WTM060            6 0
190300931222     C     WPODSP        ADD       WTM060        WPOLIM
190400140822     c
190500930325     C*
190600930420     C* TRASFORMO LA DATA DI CONSEGNA
190700931125     C     D98DCM        IFNE      *ZEROS
190800931125     C                   Z-ADD     D98DCM        WDAT
190900930325     C                   EXSR      DATORE
191000930326     C                   Z-ADD     WORE          WPODCM
191100930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
191200930326     C                   MOVEL     'OSC  '       WR3TAB
191300931125     C                   Z-ADD     D98HMC        WR3HHI
191400930326     C                   EXSR      RO3ORE
191500930326     C                   ADD       WR3HHO        WPODCM
191600930326     C                   END
191700941228     C*
191800941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
191900941228     C     D98DUC        IFNE      *ZEROS
192000941228     C                   Z-ADD     D98DUC        WDAT
192100941228     C                   EXSR      DATORE
192200941228     C                   Z-ADD     WORE          WPODUC
192300970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
192400970310     C                   MOVEL     'OSP  '       WR3TAB
192500970310     C                   Z-ADD     W98HUC        WR3HHI
192600970310     C                   EXSR      RO3ORE
192700970310     C     D98DRF        IFNE      *ZEROS
192800050928     c     w98huc        andgt     1200
192900970310     C                   SUB       12            WR3HHO
193000970310     C                   ENDIF
193100970310     C                   ADD       WR3HHO        WPODUC
193200941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
193300070605     C***  WPODSP        IFGT      WPODUC
193400070605     C***                Z-ADD     WPODSP        WPODUC
193500070605     C***                ENDIF
193600941228     C                   ENDIF
193700960402     C*
193800141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
193900970304     C     D98DBR        IFNE      *ZEROS
194000970304     C                   Z-ADD     D98DBR        WDAT
194100960402     C                   EXSR      DATORE
194200960402     C                   Z-ADD     WORE          WPODBR
194300070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
194400960402     C                   ELSE
194500960402     C                   Z-ADD     WPODSP        WPODBR
194600960402     C                   ENDIF
194700930913     C*
194800960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
194900930913     C* CONSEGNABILITA'
195000960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
195100950407     C*
195200930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
195300931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
195400931125     C                   Z-ADD     D98DIC        WDAT
195500930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
195600930913     C                   Z-ADD     WORE          WPODIC
195700930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
195800931125     C                   Z-ADD     D98OIC        WR3HHI
195900930913     C                   EXSR      RO3ORE
196000930913     C                   ADD       WR3HHO        WPODIC
196100931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
196200950407     C*
196300930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
196400931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
196500931125     C                   Z-ADD     D98DFC        WDAT
196600930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
196700930913     C                   Z-ADD     WORE          WPODFC
196800140822     C                   Z-ADD     WORE          WPODFC_00
196900930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
197000931125     C                   Z-ADD     D98OFC        WR3HHI
197100930913     C                   EXSR      RO3ORE
197200930913     C                   ADD       WR3HHO        WPODFC
197300931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
197400930913     C*
197500930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
197600930913     C     D98ERR        IFEQ      '0'
197700930913     C     WPODFC        IFEQ      *ZEROS
197800930913     C     WPODIC        ANDNE     *ZEROS
197900930913     C                   MOVEL     '1'           D98ERR
198000930913     C                   MOVEL     MSG(17)       D98MSG
198100930913     C                   ENDIF
198200930913     C                   ENDIF
198300930907     C*
198400930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
198500930913     C     D98ERR        IFEQ      '0'
198600930913     C     WPODIC        IFGT      WPODFC
198700930907     C                   MOVEL     '1'           D98ERR
198800930913     C                   MOVEL     MSG(18)       D98MSG
198900930913     C                   ENDIF
199000930913     C                   ENDIF
199100950407     C*
199200950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
199300930511     C*
199400930511     C                   ENDSR
199500950110     C********************************************************************
199600950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
199700950110     C********************************************************************
199800950110     C     CALDPP        BEGSR
199900950110     C*
200000950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
200100950110     C                   MOVEL     'P'           WTC                            *PARTENZA
200200950110     C                   MOVEL     'I'           WIC                            *INTERNA
200300960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
200400950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
200500950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
200600950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
200700950110     C*
200800950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
200900950110     C                   MOVEL     'P'           WTC                            *PARTENZA
201000950110     C                   MOVEL     'C'           WIC                            *CLIENTE
201100950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
201200170104     C* SE TEMPO PARTENZA =0 sommo comunque 1 ore per slittare dal
201300170104     c*  festivo se fosse data festiva la data sped e non bortderizzata
201400170104     c*  in questa data
201500170104     c                   if        d98tpc=0
201600170104     c                   z-add     1             WR4NOI
201700170104     c                   else
201800950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
201900170104     c                   endif
202000950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
202100950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
202200170104     c
202300170104     c                   if        d98tpc=0
202400170104     c                   sub       1             wpoppc
202500170104     c                   endif
202600960229     C*
202700960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
202800050922     c
202900050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
203000050922     c
203100050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
203200090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
203300050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
203400090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
203500050922%%%  C*  ritardo alla partenza.
203600050922     c* Il tutto solo se la data di oggi > della
203700050922     c*  data teorica di partenza determinata
203800050922     c*
203900950110%%%  C     D98ERR        IFEQ      '0'
204000950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
204100950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
204200950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
204300050922     c     wpoattData24  andgt     wpoppi
204400960911%%%  C********             MOVEL'OSC  '   WR3TAB
204500960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
204600960911%%%  C********             EXSR RO3ORE
204700960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
204800050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
204900090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
205000090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
205100950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
205200050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
205300050922     c* La nuova data calcolata non può essere < della minima prevista
205400050922     c                   if        wr0poo>wpoppi
205500050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
205600050922     c                   endif
205700050922     c
205800950110%%%  C                   ENDIF
205900950110%%%  C                   ENDIF
206000950110%%%  C*
206100950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
206200050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
206300050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
206400050915%%%  C*  alla prtenza.
206500950110%%%  C     D98ERR        IFEQ      '0'
206600950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
206700950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
206800950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
206900950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
207000950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
207100950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
207200050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
207300950110%%%  C                   ENDIF
207400950110%%%  C                   ENDIF
207500950110%%%  C*
207600950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
207700950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
207800950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
207900950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
208000950110%%%  C     D98ERR        IFEQ      '0'
208100950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
208200950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
208300950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
208400950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
208500090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
208600090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
208700950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
208800050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
208900950110%%%  C                   ENDIF
209000950110%%%  C                   ENDIF
209100960229     C*
209200960229     C                   ENDIF                                                  <-- 1 ---
209300950110     C*
209400950110     C                   ENDSR
209500930604     C********************************************************************
209600930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
209700930604     C********************************************************************
209800930604     C     CALDPT        BEGSR
209900090902     c                   clear                   RicalcoloTRAZ     1
210000930604     C*
210100930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
210200931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
210300931129     C                   MOVEL     'I'           WIC                            *INTERNA
210400050915     c* se non presente la data partenza ultimo collo, uso la teorica
210500950110     C     D98DUC        IFEQ      *ZEROS
210600950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
210700950110     C                   ELSE
210800950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
210900950110     C                   ENDIF
211000930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
211100930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
211200930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
211300930604     C*
211400930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
211500931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
211600931129     C                   MOVEL     'C'           WIC                            *CLIENTE
211700950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
211800930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
211900930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
212000930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
212100150508
212200150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
212300150508     c* se presente la data partenza PRIMO  collo --> la uso
212400150508     c                   if        w98dpc=0
212500150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
212600150508     c                   else
212700150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
212800150508     C                   MOVEL     'C'           WIC                            *INTERNA
212900150508     C                   Z-ADD     W98DPC        WDAT
213000150508     C                   EXSR      DATORE
213100150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
213200150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
213300150508     C                   EXSR      RO4CDP
213400150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
213500150508     c                   endif
213600960229     C*
213700960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
213800940624%%%  C*
213900050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
214000050915
214100940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
214200050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
214300050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
214400050915     c*   attribuito alla trazione
214500940624%%%  C     D98ERR        IFEQ      '0'
214600940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
214700940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
214800090903     c     wpoatt        andge     wpopti
214900050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
215000090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
215100090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
215200940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
215300050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
215400050922     c                   if        wr0poo>wpopti
215500050922     c                   eval      wpopti=wr0poo
215600050922%%%  C                   endif
215700090902     c*
215800090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
215900090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
216000090902     c*  della trazione udate e utime
216100090902     c                   eval      RicalcoloTRAZ='S'
216200090902     c
216300940624%%%  C                   ENDIF
216400940624%%%  C                   ENDIF
216500940628%%%  C*
216600050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
216700050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
216800050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
216900940628%%%  C     D98ERR        IFEQ      '0'
217000940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
217100940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
217200940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
217300090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
217400090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
217500940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
217600050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
217700940628%%%  C                   ENDIF
217800940628%%%  C                   ENDIF
217900960229     C*
218000960229     C                   ENDIF                                                  <-- 1 ---
218100930604     C*
218200930604     C                   ENDSR
218300930511     C********************************************************************
218400930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
218500930511     C********************************************************************
218600930511     C     CALDCR        BEGSR
218700050914     c                   clear                   consideraora
218800100308     c
218900100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
219000100308     c*
219100100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
219200100308     c* 1) se presente solo ORA --> ignoro sempre
219300100308     c* 2) se presente data e ora IL --> ignoro l'ora
219400100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
219500100309     c***                if        d98tsp='E'
219600100309     c***                clear                   d98dcr
219700100309     c***                clear                   d98hcr
219800100309     c**+                clear                   d98tcr
219900100309     c***                endif
220000100308     c
220100100308     c                   if        d98hcr>0
220200100308     c
220300100308     c                   if        d98dcr=0
220400100308     c                   clear                   d98hcr
220500100308     c                   endif
220600100308
220700100308     c                   if        d98dcr>0 and d98tcr=' '
220800100308     c                   clear                   d98hcr
220900100308     c                   endif
221000100308     c                   endif
221100941228     C*
221200941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
221300941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
221400050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
221500050901     c*  (per uguale si intende entrambe nel mattino o entrambe
221600050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
221700050901     c*  (EDPES 1/9/05)
221800050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
221900050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
222000050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
222100050831     C***                Z-ADD     D98DCM        D98DCR
222200050831     C***                ENDIF
222300050901     c*
222400070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
222500050914     c
222600100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
222700100308     c***                clear                   d98hcr
222800100308     c***                clear                   d98tcr
222900100308     c***                endif
223000050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
223100050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
223200070521     c*  richiesta dal cliente
223300100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
223400100308     C***                MOVEL     'OSC  '       WR3TAB
223500100308     C***                Z-ADD     D98HMC        WR3HHI
223600100308     C***                EXSR      RO3ORE
223700100308     c***                z-add     wr3hho        salvaoreHMC
223800050901     c
223900100308     C***                Z-ADD     D98Hcr        WR3HHI
224000100308     C***                EXSR      RO3ORE
224100100308     c***                z-add     wr3hho        salvaoreHCR
224200100308     c***
224300050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
224400100308    2c***                if        salvaoreHMC=salvaoreHCR
224500100308     C***                eval      Consideraora='S'
224600100308     c***                else
224700100308     C***                eval      Consideraora=' '
224800100308     c***                clear                   d98hcr
224900100308    2c***                endif
225000100308    1c***                endif
225100930511     C*
225200930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
225300931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
225400931125     C                   Z-ADD     D98DCR        WDAT
225500930325     C                   EXSR      DATORE
225600930326     C                   Z-ADD     WORE          WPODCR
225700930420     C*
225800930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
225900930420     C*
226000931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
226100930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
226200930326     C                   MOVEL     'CRD  '       WR3TAB
226300931125     C                   Z-ADD     D98HCR        WR3HHI
226400930419     C                   EXSR      RO3ORE
226500930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
226600940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
226700050902     c
226800931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
226900930420     C                   MOVEL     'CRD1 '       WR3TAB
227000930420     C                   Z-ADD     *ZEROS        WR3HHI
227100930420     C                   EXSR      RO3ORE
227200931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
227300090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
227400090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
227500050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
227600060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
227700050907     c                   z-add     worecr        oreILoDOPO
227800931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
227900931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
228000931207     C                   MOVEL     'C'           WIC                            *CLIENTE
228100931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
228200150330     C                   Z-ADD     WORECR        WR4NOI
228300931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
228400931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
228500931209     C     WPOCRF        SUB       12            WR7POI            9 0
228600931209     C                   MOVEL     'X'           WIC
228700931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
228800931209     C                   ADD       12            WR7POO
228900931207     C     WR7POO        IFGT      WPOCRF
229000931207     C                   Z-ADD     WR7POO        WPOCRF
229100931207     C                   END
229200150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
229300150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
229400150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
229500150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
229600150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
229700150330     C                   MOVEL     'C'           WIC                            *CLIENTE
229800150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
229900150330     C                   Z-ADD     w99TCC        WR4NOI
230000150330     c                   sub       12            wr4noi
230100150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
230200150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
230300150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
230400150330     c
230500150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
230600150330     C                   MOVEL     'X'           WIC
230700150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
230800150330     C******             ADD       12            WR7POO
230900150330     C     WR7POO        IFGT      WPOCRF_dee
231000150330     C                   Z-ADD     WR7POO        WPOCRF_dee
231100150330     C                   END
231200050902     c
231300050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
231400050902     c*  sono le ore di fine della non consegnabilità
231500050905     c* Se aggiungendo 1 finisco in giorno festivo
231600050905     c*  cerco primo giorno non festivo
231700050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
231800050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
231900050907     C****               MOVEL     'C'           WIC                            *CLIENTE
232000050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
232100050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
232200050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
232300050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
232400050907     c
232500050907     c                   add       1             wpocri
232600050902     c
232700050902   FFC                   Z-ADD     WPOCRI        WPOCRC
232800050902     c
232900930325     C                   END
233000150330     C*
233100931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
233200930420     C* DATA SPED. = DATA INIZIO RANGE
233300930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
233400940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
233500940324£$   C                   Z-ADD     WPOCRI        WPOCRC
233600930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
233700930420     C                   MOVEL     'CRP  '       WR3TAB
233800931125     C                   Z-ADD     D98HCR        WR3HHI
233900930420     C                   EXSR      RO3ORE
234000931207     C     WPODCR        ADD       WR3HHO        WPOCRF
234100931207     C                   SUB       24            WPOCRF                         *FINE RANGE
234200931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
234300931207     C* ANDANDO INDIETRO NEL TEMPO
234400050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
234500050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
234600050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
234700050907     c* data utile venedì sera
234800130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
234900130411     C                   MOVEL     'X'           WIC                            *CLIENTE
235000931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
235100931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
235200931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
235300930326     C                   END
235400930526     C*
235500931125     C     D98TCR        IFEQ      'P'
235600931125     C     D98TCR        OREQ      'D'
235700930526     C                   ELSE                                                   *CONS.RICH.IL ..
235800940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
235900940324£$   C                   Z-ADD     WPODCR        WPOCRC
236000930526     C* DATA SPED. = DATA INIZIO RANGE
236100050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
236200050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
236300050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
236400930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
236500930526     C                   MOVEL     'CRI  '       WR3TAB
236600931125     C                   Z-ADD     D98HCR        WR3HHI
236700930526     C                   EXSR      RO3ORE
236800050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
236900050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
237000050907     c                   z-add     wr3hho        oreILoDOPO
237100930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
237200050902     c
237300050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
237400050902     c                   if        wr3MATPOM='P' and d98hcr>0
237500050902     c                   add       12            wpocri
237600100308     c* tolgo 12 ore dalle ore conseganbili
237700050907     c                   sub       12            oreILoDOPO
237800050902     c                   endif
237900050902     c
238000931209     C     WPOCRF        SUB       12            WR7POI            9 0
238100931209     C                   MOVEL     'X'           WIC
238200931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
238300931209     C                   ADD       12            WR7POO
238400931209     C     WR7POO        IFGT      WPOCRF
238500931209     C                   Z-ADD     WR7POO        WPOCRF
238600931209     C                   END
238700050905     c
238800050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
238900050905     c*  sono le ore di fine della non consegnabilità
239000050905     c* Se aggiungendo 1 finisco in giorno festivo
239100050905     c*  cerco primo giorno non festivo
239200050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
239300050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
239400050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
239500050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
239600050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
239700050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
239800050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
239900050907     c
240000050907     c                   add       1             wpocri
240100050905     c
240200050905   FFC                   Z-ADD     WPOCRI        WPOCRC
240300050905     c
240400930526     C                   END
240500931206     C*
240600931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
240700931206     C* LA IMPOSTO COME LA CONS.TEORICA
240800140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
240900140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
241000931206     C                   ELSE
241100931206     C                   Z-ADD     WPOCRF        WPOCR1
241200931206     C                   END
241300150608     c* ai fini del calcolo affidabilità
241400150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
241500150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
241600931206     C                   ELSE
241700931206     C                   Z-ADD     WPOCRF        WPOCR2
241800931206     C                   END
241900150608     c* ai fini della data prevista consegna cliente
242000150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
242100150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
242200150608     C                   ELSE
242300150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
242400150608     C                   END
242500150331     c
242600150331     c                   if        wpocrf_dee>0
242700150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
242800150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
242900150330     C                   END
243000150331     C                   END
243100150331     C*
243200150331     c                   if        wpocrf_dee=0
243300150608     C     wpocr2_dee    sub       24            WPOCRF_dee
243400150331     c                   endif
243500150331     c
243600930526     C                   END                                                    <-- 1 ---
243700930511     C*
243800930511     C                   ENDSR
243900930511     C********************************************************************
244000930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
244100930511     C********************************************************************
244200930511     C     CALLAV        BEGSR
244300930511     C*
244400941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
244500930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
244600930421     C*
244700930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
244800930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
244900930421     C     1             DO        IE            E                 3 0          --- 2 -->
245000930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
245100930421     C                   Z-ADD     1             I
245200930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
245300060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
245400930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
245500930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
245600930421     C                   ADD       1             IL
245700930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
245800930421     C                   EXSR      DATORE
245900930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
246000091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
246100091014     c* Mi salvo la posizione dell'evento
246200091014     C                   Z-ADD     E             LAp(IL)
246300930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
246400930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
246500930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
246600930421     C                   EXSR      RO3ORE
246700930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
246800930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
246900930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
247000930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
247100091015     c**                 if        §2agla>1
247200091015     c**                 sub       1             §2agla
247300930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
247400091015     c**                 else
247500091015     c**                 clear                   whh
247600091015     c**                 endif
247700930510     C                   ADD       WHH           WORELA
247800940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
247900930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
248000090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
248100150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
248200930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
248300931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
248400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
248500930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
248600930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
248700930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
248800930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
248900931209     C                   Z-ADD     LAF(IL)       WR7POI
249000931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
249100931202     C     WR7POO        IFGT      LAF(IL)
249200931201     C                   Z-ADD     WR7POO        LAF(IL)
249300931202     C                   END
249400930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
249500930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
249600091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
249700930510     C                   END                                                    <-- 3 ---
249800931223     C                   ENDDO                                                  <-- 2 ---
249900930421     C*
250000930421     C                   END                                                    <-- 1 ---
250100930618££$$ C*
250200930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
250300931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
250400941228££$$ C     1             DO        IL            I
250500930618££$$ C     LAF(I)        IFGT      WPOLAF
250600930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
250700930618££$$ C                   END
250800931223££$$ C                   ENDDO
250900930511     C*
251000930511     C                   ENDSR
251100940324£$   C********************************************************************
251200940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
251300940324£$   C********************************************************************
251400940324£$   C     CALLAC        BEGSR
251500941229     C*
251600950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
251700950407     C*
251800950210     C                   Z-ADD     WORELA        WTM060
251900941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
252000941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
252100941229     C     D98DAG        IFEQ      *ZEROS
252200050520     c     §2aftg        andeq     'S'
252300090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
252400090827     C     WORELA        ADD       w99TCC        WTM060            6 0
252500160404     c                   if        wVP='S'
252600160404     c                   sub       24            wTM060
252700160404     c                   endif
252800941229     C                   ENDIF
252900941229     C*
253000940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
253100940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
253200941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
253300941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
253400941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
253500940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
253600941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
253700050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
253800050520     c                   movel     §2aftg        LCON(IL)
253900950407     C*
254000950407     C                   ENDIF
254100960129     C*
254200940324£$   C                   ENDSR
254300930511     C********************************************************************
254400930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
254500930511     C********************************************************************
254600930511     C     CALGIA        BEGSR
254700960129     C*
254800960129     C*-------------------------------------------------------------------
254900960129     C* EVENTI GIACENZA
255000960129     C*-------------------------------------------------------------------
255100960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
255200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
255300960129     C*
255400050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
255500041116     c     d98nsp        ifgt      0
255600031125     C                   EXSR      CARGIAC
255700041116     c                   endif
255800031125     C*
255900960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
256000960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
256100960129     C     1             DO        IE            E                 3 0          --- 2 -->
256200960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
256300960129     C                   Z-ADD     1             I
256400960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
256500060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
256600960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
256700960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
256800960129     C                   ADD       1             IG
256900960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
257000960129     C                   EXSR      DATORE
257100960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
257200140606     c* Mi salvo la posizione dell'evento
257300140606     C                   Z-ADD     E             gip(IG)
257400960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
257500960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
257600960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
257700960129     C                   EXSR      RO3ORE
257800960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
257900960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
258000960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
258100960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
258200960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
258300960129     C                   ADD       WHH           WOREGI
258400960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
258500960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
258600090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
258700090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
258800160404     c                   if        wVP='S'
258900160404     c                   sub       24            wOREGI
259000160404     c                   endif
259100960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
259200960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
259300960129     C                   MOVEL     'C'           WIC                            *CLIENTE
259400960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
259500960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
259600960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
259700960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
259800960129     C                   Z-ADD     GIF(IG)       WR7POI
259900960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
260000031125     C     WR7POO        IFGT      GIF(IG)
260100031125     C                   Z-ADD     WR7POO        GIF(IG)
260200031125     C                   ENDIF
260300031201     C*
260400031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
260500031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
260600031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
260700031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
260800031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
260900031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
261000031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
261100031201     C                   IF        WPOATT > GIF(IG)
261200031201     C*
261300031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
261400031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
261500031201     C                   IF        WGGA = *BLANKS
261600031201     C                   SETOFF                                       54
261700031201     C                   ELSE
261800031201     C     WGGA          LOOKUP    SK7QW                                  54
261900031201     C                   ENDIF
262000031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
262100031201     C                   MOVEL     '1'           FLGGIACOK
262200031201     C                   ELSE
262300140127     c
262400031125     C*
262500031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
262600031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
262700031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
262800031125     C                   MOVEL     '0'           FLGGIACOK         1
262900031125     C*
263000031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
263100031125     C     1             DO        IAG           EG                3 0
263200031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
263300031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
263400031125     C                   EXSR      DATORE
263500031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
263600031125     C*
263700031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
263800031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
263900031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
264000031125     C                   EXSR      DATORE
264100031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
264200031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
264300031229     C* => NN LO CONSIDERO
264400031229     C                   IF        EVC(E+1) = 'G' AND
264500031229     C                             WPROXGII = WGCPDORE
264600031229     C                   MOVEL     '1'           FLGGIACOK
264700031229     C                   ENDIF
264800031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
264900031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
265000031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
265100031125     C                   EXSR      RO3ORE
265200031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
265300031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
265400031125     C* DELL'EVENTO GIACENZA CORRENTE
265500031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
265600060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
265700031125     C                   MOVEL     '1'           FLGGIACOK
265800031125     C                   LEAVE
265900031125     C                   ENDIF
266000031125     C                   ELSE
266100031229     C                   IF        EVC(E)  = 'G' AND
266200031229     C                             GII(IG) = WGCPDORE
266300031229     C                   MOVEL     '1'           FLGGIACOK
266400031229     C                   LEAVE
266500031229     C                   ENDIF
266600031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
266700031125     C                   MOVEL     '1'           FLGGIACOK
266800031125     C                   LEAVE
266900031125     C                   ENDIF
267000031125     C                   ENDIF
267100031125     C                   ENDDO
267200031201     C                   ENDIF
267300140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
267400140127     C                   IF        FLGGIACOK = '0'  and
267500140127     c                             d98dcm=evd(E)
267600140127     C                   MOVEL     '1'           FLGGIACOK
267700140127     c                   endif
267800031125     C*
267900031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
268000031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
268100031125     C* DELL'EVENTO GIACENZA CORRENTE
268200031125     C                   IF        FLGGIACOK = '0'
268300031125     C                   Z-ADD     *ZEROS        GIF(IG)
268400031125     C                   ELSE
268500031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
268600031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
268700031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
268800031125     C                   ENDIF
268900031125     C                   ELSE
269000031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
269100031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
269200031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
269300031125     C                   ENDIF
269400031125     C*
269500960129     C                   ENDIF                                                  <-- 3 ---
269600960129     C                   ENDDO                                                  <-- 2 ---
269700960129     C*
269800960129     C                   ENDIF                                                  <-- 1 ---
269900960129££$$ C*
270000960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
270100960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
270200960129££$$ C     1             DO        IG            I
270300960129££$$ C     GIF(I)        IFGT      WPOGIF
270400960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
270500960129££$$ C                   ENDIF
270600960129££$$ C                   ENDDO
270700941229     C*
270800941229     C*-------------------------------------------------------------------
270900941229     C* APERTURA GIACENZA
271000941229     C*-------------------------------------------------------------------
271100960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
271200960129     C*
271300960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
271400931125     C                   Z-ADD     D98DAG        WDAT
271500930325     C                   EXSR      DATORE
271600950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
271700960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
271800950413£££  C                   Z-ADD     WORE          WPODAG
271900960129$$£  C*********************ENDIF
272000930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
272100930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
272200930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
272300930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
272400930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
272500941229     C*
272600941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
272700941229     C                   Z-ADD     WPODAG        WPOAGC
272800960129     C*
272900960129     C                   ENDIF                                                  <-- 1 ---
273000941229     C*
273100941229     C*-------------------------------------------------------------------
273200930511     C* CHIUSURA GIACENZA
273300941229     C*-------------------------------------------------------------------
273400930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
273500930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
273600931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
273700931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
273800931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
273900931201     C                   Z-ADD     D98DCM        W98DCG
274000930416     C                   ELSE
274100930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
274200930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
274300931201     C                   Z-ADD     WDAODI        W98DCG
274400061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
274500061123     c*  chiusura giacenza, come già fa in altri posti
274600061123     c                   if        d98dfc>0
274700061123     C                   Z-ADD     d98dfc        W98DCG
274800061123     c                   endif
274900061123     c
275000930409     C                   END
275100930409     C                   END
275200930409     C                   END
275300930414     C* TRASF.DATA CHIUSURA
275400931201     C     W98DCG        IFNE      *ZEROS
275500931201     C                   Z-ADD     W98DCG        WDAT
275600930325     C                   EXSR      DATORE
275700930326     C                   Z-ADD     WORE          WPODCG
275800930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
275900930407     C* GIORNO DA ESCLUDERE
276000150216     C                   ADD       12            WPODCG
276100150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
276200150123     c*  il giorno di chiusura dovrebe già essere protetto
276300941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
276400941229     C     WPOCGC        IFEQ      *ZEROS
276500941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
276600941229     C                   ENDIF
276700930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
276800930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
276900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
277000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
277100150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
277200150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
277300160404     c                   if        wVP='S'
277400160404     c                   sub       24            wr4NOI
277500160404     c                   endif
277600930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
277700930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
277800930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
277900931209     C                   Z-ADD     WPODCG        WR7POI
278000931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
278100931202     C     WR7POO        IFGT      WPODCG
278200931201     C                   Z-ADD     WR7POO        WPODCG
278300930326     C                   END
278400931202     C                   END
278500930511     C*
278600930511     C                   ENDSR
278700031125     C********************************************************************
278800031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
278900031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
279000031125     C*             DEGLI EVENTI DI TIPO GIACENZA
279100031125     C********************************************************************
279200031125     C     CARGIAC       BEGSR
279300031125     C*
279400031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
279500031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
279600031125     C*
279700050405     C                   Z-ADD     D98AAS        KWGCPAAS
279800050405     C                   Z-ADD     D98LNP        KWGCPLNP
279900050405     C                   Z-ADD     D98NRS        KWGCPNRS
280000050405     C                   Z-ADD     D98NSP        KWGCPNSP
280100031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
280200050405     C     KEYGC_P       SETLL     TIGCP01L
280300050405     C                   IF        %equal(TIGCP01L)
280400050405     C     KEYGC_P       READE     TIGCP01L
280500050405     C                   DOW       not %eof(TIGCP01L)
280600031125     C                   ADD       1             IAG
280700031125     C                   EVAL      GCPD(IAG) = GCPDUR
280800050405     C     KEYGC_P       READE     TIGCP01L
280900031125     C                   ENDDO
281000031125     C                   ENDIF
281100031125     C                   ELSE
281200050405     C     KEYGC_P       SETLL     TIGCP21L
281300050405     C                   IF        %equal(TIGCP21L)
281400050405     C     KEYGC_P       READE     TIGCP21L
281500050405     C                   DOW       not %eof(TIGCP21L)
281600031125     C                   ADD       1             IAG
281700031125     C                   EVAL      GCPD(IAG) = GCPDUR
281800050405     C     KEYGC_P       READE     TIGCP21L
281900031125     C                   ENDDO
282000031125     C                   ENDIF
282100031125     C                   ENDIF
282200031125     C*
282300031125     C                   ENDSR
282400941229     C********************************************************************
282500960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
282600941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
282700941229     C********************************************************************
282800960129     C     CALGIC        BEGSR
282900941229     C*
283000960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
283100960129     C*
283200960129     C                   Z-ADD     WOREGI        WTM060
283300960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
283400960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
283500960129     C     D98DAG        IFEQ      *ZEROS
283600960129$$£  C     WOREGI        ADD       12            WTM060            6 0
283700960129     C                   ENDIF
283800960129     C*
283900960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
284000960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
284100960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
284200960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
284300960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
284400960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
284500960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
284600960129     C*
284700960129     C                   ENDIF
284800941229     C*
284900941229     C                   ENDSR
285000060418     C********************************************************************
285100060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
285200060418     c*            l'affidabilità
285300060418     C********************************************************************
285400060418     C     CALCEPA       BEGSR
285500060418     C*
285600060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
285700060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
285800060420     c                   clear                   numggcepa         3 0
285900060418     C*
286000060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
286100060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
286200060420     c                   eval      wgiroepa='0'
286300060418     c                   eval      wfine=' '
286400060418     c
286500060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
286600060420    2C     W             DOwle     IP                                           --- 2 -->
286700060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
286800060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
286900060418     c
287000060420    4c                   if        wgiroepa='0'
287100060420     c                   exsr      ElaInizCEPA
287200060418   x4c                   else
287300060420    5c                   if        pvd(W)>wdat
287400060418     c                   eval      wfine='S'
287500060418    5c                   endif
287600060418    4c                   endif
287700060418     c
287800060420    4c                   if        wfine=' ' and pvd(W)=wdat
287900060420     C*
288000060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
288100060420     c*  finale
288200060420     c                   add       1             numggcepa
288300060420     c
288400060420     C                   z-add     24            WORELA
288500060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
288600060418     c
288700060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
288800060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
288900060418     C                   MOVEL     'C'           WIC                            *CLIENTE
289000060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
289100060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
289200060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
289300060418     c
289400060418     c* Trasformo le ore in data e verifico se c'e' altro evento
289500060418     c*  in sta data che allunga ancora
289600060420     c                   z-add     wr4poo        wore
289700060418     c                   EXSR      OREDAT
289800060418    4c                   endif
289900060418     c
290000060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
290100060418    4c                   if        wfine='S'
290200060418     c                   EXSR      ELAFINECEPA
290300060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
290400060420     c*  inizio range
290500060420     c                   else
290600060420     c                   add       1             w
290700060418    4C                   ENDif                                                  <-- 3 ---
290800060420     c
290900060420     c                   else
291000060420     c                   add       1             w
291100060418    3C                   ENDif                                                  <-- 3 ---
291200060418    2C                   ENDDO                                                  <-- 2 ---
291300060418     c
291400060420     c                   if        wgiroepa='1'
291500060418     c                   EXSR      ELAFINECEPA
291600060418     c                   endif
291700060418     C*
291800060418    1C                   END                                                    <-- 1 ---
291900060418££$$ C*
292000060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
292100060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
292200060420££$$ C     1             DO        Ipa           w
292300060420££$$ C     paif(w)       IFGT      WPOpaif
292400060420££$$ C                   Z-ADD     paif(w)       WPOpaif
292500060418££$$ C                   END
292600060418££$$ C                   ENDDO
292700060418     C*
292800060418     C                   ENDSR
292900060420     C********************************************************************
293000060420     c* Elabora Inizio range eventi particolari che allungano
293100060420     C********************************************************************
293200060420     c     ElaInizCEPA   BEGSR
293300060420     C                   ADD       1             IPA
293400060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
293500060420     C                   EXSR      DATORE
293600060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
293700061017     c
293800060420     c                   eval      wgiroepa='1'
293900060420     c                   eval      wdat=pvd(w)
294000060420     C                   ENDSR
294100060418     C********************************************************************
294200060418     c* Elabora fine range eventi particolari che allungano
294300060418     C********************************************************************
294400060418     c     ElafineCEPA   BEGSR
294500060420     c     numggcepa     mult      24            worela
294600060420     c                   sub       12            worela
294700090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
294800090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
294900160404     c                   if        wVP='S'
295000160404     c                   sub       24            WORELA
295100160404     c                   endif
295200060418     c
295300060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
295400060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
295500060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
295600060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
295700060418     C                   Z-ADD     paif(IPA)     WR7POI
295800060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
295900060418    5C     WR7POO        IFGT      paif(IPA)
296000060418     C                   Z-ADD     WR7POO        paif(IPA)
296100060418    5C                   END
296200061017     c*Sommo 24 ore di inizio giornata
296300061017     c*            : non ho capito perchè ma ci vogliono
296400061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
296500090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
296600090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
296700160404     c                   if        wVP='S'
296800160404     c                   sub       24            paii(IPA)
296900160404     c                   endif
297000061017     c* Se tempi di consegna inferiori alle 24 ore , devo
297100061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
297200061017     c*  partire prima
297300061017     c****               if        d98tcc<24
297400061017     c****               sub       12            paii(ipa)
297500061017     c****               endif
297600060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
297700060418     c* Imposto nuovo giro
297800060420     c                   eval      wgiroepa='0'
297900060418     c                   eval      wfine=' '
298000060420     c                   clear                   numggcepa         3 0
298100060418     c                   ENDSR
298200930511     C********************************************************************
298300930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
298400930511     C********************************************************************
298500010822     C     CALASS        BEGSR
298600010821     C*
298700011211     C                   EXSR      REPMAXCSB
298800930511     C*
298900930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
299000010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
299100011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
299200930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
299300930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
299400930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
299500930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
299600930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
299700930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
299800930511     C                   ELSE                                                   <-- 4 -->
2999009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
300000930512     C                   END                                                    <-- 4 ---
300100930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
300200930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
300300930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
300400930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
300500930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
300600930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
300700930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
300800930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
300900930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
301000930413     C                   END                                                    <-- 2 ---
301100011211     C                   END                                                    <-- 1 ---
301200930406     C*
301300930325     C                   ENDSR
301400930608     C********************************************************************
301500930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
301600930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
3017009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
301800930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
301900930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
302000930608     C********************************************************************
302100930608     C     RDRASS        BEGSR
302200930608     C*
302300930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
3024009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
3025009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
3026009407151507 C*********************EXSR RO3ORE
3027009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
302800930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
3029009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
3030009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
3031009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
3032009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3033009407191907 C                   Z-ADD     WPOASI        WPOROU
3034009407191907 C                   EXSR      CHKFES
3035009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3036009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
3037009407191907 C                   ELSE                                                   *E' FERIALE
3038009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
3039009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3040009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
3041009407191907 C                   ENDIF
304200930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
304300930608     C                   MOVEL     'ASS1 '       WR3TAB
304400930608     C                   Z-ADD     *ZEROS        WR3HHI
304500930608     C                   EXSR      RO3ORE
304600930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
304700930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
304800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
304900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
305000930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
305100930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
305200930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
305300930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
305400930608     C*
305500930608     C                   ENDSR
305600930511     C********************************************************************
305700930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
305800930511     C********************************************************************
305900930511     C     CALFED        BEGSR
306000150331     c                   clear                   fddata
306100150331     c                   clear                   WPOFDdata
306200930511     C*
306300051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
306400931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
306500051108     C     D98FFD        oreq      'M'                                          --- 1 -->
306600930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
306700930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
306800930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
306900930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
307000150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
307100150331     c                   if        §ar5dfd>*zeros
307200150331     c                   movel     §ar5dfd       fddata            8 0
307300150331     c                   endif
307400150331     c                   if        fddata>0 and ((d98dti>0 and
307500150331     c                             fddata>d98dti) or d98dti=0)
307600150331     C                   Z-ADD     fddata        WDAT
307700150331     C                   EXSR      DATORE
307800150331     C                   Z-ADD     WORE          WPOfddata
307900150331     c                   endif
308000150331     c
308100930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
308200930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
308300930511     C                   ELSE                                                   <-- 2 -->
3084009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
308500930512     C                   END                                                    <-- 2 ---
308600150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3087001504081507 C                   Z-ADD     WPOfddata     WPOFDI
308800150331     c                   endif
308900150331
309000930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
309100930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
309200930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
309300930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
309400930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
309500930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
309600930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
309700930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
309800930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
309900930511     C                   END                                                    <-- 1 ---
310000930511     C*
310100930511     C                   ENDSR
310200930608     C********************************************************************
310300930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
310400930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3105009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
310600930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
310700930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
310800930608     C********************************************************************
310900930608     C     RDRFED        BEGSR
311000930608     C*
311100930608     C* CALCOLO ORE DEL FERMO DEPOSITO
311200930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3113009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3114001503311907 C                   Z-ADD     WPOFDI        WPOROU
3115001503311907 C                   EXSR      CHKFES
3116001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3117001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3118001503311907 C                   ELSE                                                   *E' FERIALE
3119001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3120001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3121001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3122001503311907 C                   ENDIF
312300150331     c* Se presente la particolarità consegna con più giorni di fermo depo
312400150331     c*  uso questi
312500150331     c                   if        §7rggfd>*zeros
312600150424     c                   movel     §7rggfd       wggfd             2 0
312700150331     c     wggfd         mult      24            Wnore
312800150331     c                   add       wnore         worefd
312900150331     c                   else
313000930608     C                   MOVEL     'FED1 '       WR3TAB
313100930608     C                   Z-ADD     *ZEROS        WR3HHI
313200930608     C                   EXSR      RO3ORE
313300940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
313400150331     c                   endif
313500930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
313600931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
313700931129     C                   MOVEL     'C'           WIC                            *CLIENTE
313800930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
313900930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
314000930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
314100930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
314200930608     C*
314300930608     C                   ENDSR
314400931201     C********************************************************************
314500931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
314600931201     C********************************************************************
314700931201     C     CALDTE        BEGSR
314800140825     c
314900140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
315000140825     c*  che tiene conto solo dei festivi per restituire in output
315100140825     c*  la data teorica di consegna
315200140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
315300140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
315400140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
315500140825     C                   ELSE
315600140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
315700140825     C                   END
315800140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
315900141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
316000141106     c*  quindi passo un flag
316100141106     c                   eval      nosomma='N'
316200140825     C                   EXSR      RO4CDP
316300141106     c                   eval      nosomma=' '
316400140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
316500140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
316600140825     c*  di consegna
316700140825     c                   EXSR      TRAS_DEI
316800140825     c
316900140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
317000140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
317100140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
317200140825     c                             ='M' or %subst(d98gc2:2:1)='P')
317300140825     c                   exsr      Reper_tempi
317400140825     c                   endif
317500931201     C*
317600140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
317700140825     c*  che tiene conto di:
317800140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
317900150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
318000931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
318100931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
318200931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
318300931201     C                   ELSE
318400931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
318500931201     C                   END
318600090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
318700090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
318800931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
318900140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
319000931201     C*
319100150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
319200931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
319300931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
319400090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
319500931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
319600150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
319700150608     c
319800150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
319900150608     c*  che non tiene conto dei tempi del C/Ass
320000150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
320100150609     c*
320200150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
320300150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
320400150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
320500150630     c                   Z-ADD     WPODTI        WR6POI
320600150609     c                   else
320700150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
320800150609     c                   endif
320900150609     c
321000150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
321100150608     c                   eval      wdee='S'
321200150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
321300150608     c                   eval      wdee=' '
321400150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
321500141017
321600141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
321700150219     C**                 EVAL      WTC='C'
321800150219     C**                 EVAL      WIC='C'
321900150219     c**                 z-add     wpoattdata00  WPOROU
322000150219     c**                 z-add     wdaodi        wpodata
322100150219     c**                 exsr      chkfes
322200150219     c**                 if        wslitt='S'
322300150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
322400150219     c**                 z-add     24            wr6noi
322500150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
322600150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
322700150219     c**                 else
322800150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
322900150219     c**                 endif
323000141204     c* Ricalcolo WPOrifDatLav
323100141229     C**                 MOVEL     'OSC  '       WR3TAB
323200141229     C**                 Z-ADD     WHHODI        WR3HHI
323300141229     C**                 EXSR      RO3ORE
323400141229     c**
323500141229     C**                 EVAL      WTC='C'
323600141229     C**                 EVAL      WIC='C'
323700141229     c**                 z-add     wpoattdata00  WPOROU
323800141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
323900141229     c**                 z-add     wr3hho        wr6noi
324000141229     c**                 add       12            wr6noi
324100141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
324200141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
324300141229     c
324400141017     c
324500141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
324600141017     c*  ma sempre devo slittare i festivi
324700141017     C* CONSEGNABILITA'
324800150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
324900150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
325000150219     C**                 EVAL      WTC='C'
325100150219     C**                 EVAL      WIC='C'
325200150219     c**                 z-add     d98dfc        WPOdata
325300150219     c**                 exsr      chkfes
325400150219     c**                 if        wslitt='S'
325500150219     c**                 z-add     wpodfc_00     Wr6poi
325600150219     c**                 z-add     24            wr6noi
325700150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
325800150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
325900150219     c**                 else
326000150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
326100150219     c**                 endif
326200150219     c**                 endif
326300931201     C*
326400931201     C                   ENDSR
326500930511     C********************************************************************
326600930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
326700930511     C********************************************************************
326800930511     C     CALDPC        BEGSR
326900930325     C*
327000930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
327100930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
327200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
327300930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
327400930325     C                   ELSE
327500930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
327600930407     C                   END
327700090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
327800090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
327900930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
328000090902     c
328100931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
328200090901
328300930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
328400930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
328500930423     C* A QUELLA DI RICHIESTA CONSEGNA,
328600930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
328700931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
328800931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
328900931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
329000931206     C                   END
329100930423     C                   END
329200930325     C*
329300930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
329400930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
329500930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
329600090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
329700090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
329800930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
329900931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
330000930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
330100930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
330200930423     C* A QUELLA DI RICHIESTA CONSEGNA,
330300930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
330400931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
330500931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
330600931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
330700931206     C                   END
330800930423     C                   END
330900140822     C*
331000930324     C                   ENDSR
331100150113     C********************************************************************
331200150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
331300150113     C********************************************************************
331400150113     c     Cal_DEE       BEGSR
331500150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
331600150508     c                   clear                   wtempi00C         1
331700151007     c                   clear                   wtempi12C         1
331800150722     c                   clear                   woreOLP
331900151009     c                   clear                   wdiff
332000150219     c
332100150219     c* 1) Se presente l'arrivo della trazione lo uso
332200150219    1c                   select
332300150219     c                   when      wmancatraz=' '  and d98dti>0
332400150630     c                   Z-ADD     WPODTI        WR1POI
332500150219     c
332600150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
332700150219    1c                   when      IDdata>0
332800150325     c                   exsr      CalcoloIMP
332900151008     c* Passo in output il terminal di disguido
333000151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
333100151008     c                   eval      d98flp=%editc(LTP(2):'X')
333200151008     c                   endif
333300151008
333400150528     c* stato IMP/disguido solo se >  alla data di oggi
333500151006     c                   if        wr1poi > wpoattdata00 and
333600151006     c                             (wr1poi-wpoattdata00)>12
333700150427     c                   eval      d98spcdee='IMP'
333800151008     c                   endif
333900151006     c                   if        wr1poi > wpoattdata00 and
334000151006     c                             (wr1poi-wpoattdata00)=12
334100151006     c                             and w991In>12
334200151006     c                   eval      d98spcdee='IMP'
334300151008     c                   endif
334400150219     c
334500150219     c                   other
334600150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
334700150225     c* devo far finta che sia arrivato.
334800150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
334900150508     c*  che tiene conto della data partenza PRIMO  collo
335000150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
335100150508     c*
335200150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
335300150508     c                   if        wr1poi >=wpoattdata00
335400150508     c                              and WPOPTC_DEE > WPOPTC
335500150508     c                   eval      d98spcdee='IMP'
335600150508     c                   endif
335700150219     c                   endsl
335800150113     c
335900150219     c* se il calcolo è < di oggi --> imposto oggi
336000150113     c                   if        wr1poi < wpoattdata00
336100150113     c                   eval      wr1poi= wpoattdata00
336200150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
336300150508     c                   eval      wTempi00C='S'
336400151009     c                   clear                   wdiff
336500150113     c                   endif
336600170208     c
336700170208     c* Calcolo la data di arrivo reale/teorica dei colli da passare in outout
336800170208     C                   Z-ADD     wr1poi        WORE
336900170208     C                   EXSR      OREDAT
337000170208     C                   Z-ADD     WDAT          D98dam_RTC
337100150217     c
337200150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
337300150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
337400150630     c                   if        wr1poi>wpoattdata00
337500150630     c                   eval      wdiff=wr1poi-wpoattdata00
337600150630     c                   endif
337700151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
337800151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
337900151006     c                   eval      wTempi12C='S'
338000151006     c                   endif
338100160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
338200151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
338300151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
338400160404     c                             and idfgs>0
338500151009     C     wdiff         DIV       24            WDIV
338600151009     C                   MVR                     WRES
338700151009     C                   IF        wres>0
338800151009     c                   eval      wTempi12C='S'
338900151009     c                   else
339000151009     c                   eval      wTempi00C='S'
339100151009     c                   endif
339200151009     c                   endif
339300151009     c
339400151006     c
339500151006     c                   select
339600151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
339700151006     c                             w991in>12 and
339800151006     c                             (deeindist<>'S' or d98cons<>'S'
339900151006     c                             or d98ddc>wdaodi)
340000151006     c
340100151006     c* Se stato IMP lo mantengo per avere messaggio diverso
340200151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
340300151006     c                   if        d98spcdee<>'IMP'
340400151006     c                   eval      d98spcdee='PRV'
340500151006     c                   endif
340600151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
340700150330     c                             (deeindist<>'S' or d98cons<>'S'
340800150330     c                             or d98ddc>wdaodi)
340900150427     c
341000150427     c* Se stato IMP lo mantengo per avere messaggio diverso
341100150427     c                   if        d98spcdee<>'IMP'
341200150304     c                   eval      d98spcdee='PRV'
341300150427     c                   endif
341400150427     c
341500151006   x1c                   other
341600150330     c                   exsr      CAL_OLP
341700150217     c
341800150528     c* Se passato l'orario oppure data distinta < data di oggi
341900150508     c* 1) spedizione in distinta          --> in consegna
342000150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
342100150528     c                             d98ddc<wdaodi)
342200150508    3c                   IF        deeindist='S' and d98cons='S'
342300150508     c                   eval      d98spcdee= 'DDC'
342400150508
342500150508   x3c                   else
342600150507     c
342700150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
342800150722     c*  entro sera
342900150630     c                   if        wr1poi>wpoattdata00
343000150722     c                   z-add     12            woreOLP           3 0
343100150722     c***                add       12            wr1poi
343200150630    4c                   else
343300150507    4c                   if        wr1poi<=wpoattdata00
343400150722     c                   add       24            woreOLP
343500150722     c***                add       24            wr1poi
343600150507    4c                   endif
343700150630    4c                   endif
343800150507     c
343900150722     c****               eval      d98spcdee='OLP'
344000150507     c* memorizzo orario limite picking
344100150507     c                   eval      d98picklim=%editc(picklim:'X')
344200150507    3c                   endif
344300150507    2c                   endif
344400150507     c
344500151006    1c                   endsl
344600150217     c
344700150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
344800150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
344900150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
345000150113     C                   Z-ADD     Wr1poi        W10POI            9 0
345100150113     C                   EXSR      R10FES
345200150113     C                   Z-ADD     W10POO        wr1poi
345300150113     C
345400151006     c                   select
345500151006    1c                   when      wtempi12C='S'
345600151006     c                   z-add     W991IN        wr1noi
345700151006     c                   if        woreOLP>0
345800151006     c                   add       woreOLP       wr1noi
345900151006     c                   eval      d98spcdee='OLP'
346000160404     c                   else
346100160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
346200160404     c**                 if        wVP='S'
346300160404     c**                 add       24            wr1noi
346400160404     c**                 endif
346500151006     c                   endif
346600160404
346700151006    1c                   when      wtempi00C='S'
346800150630     c***                z-add     W990CN        wr1noi
346900150630     c                   z-add     W990IN        wr1noi
347000150722     c
347100150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
347200150722     c*  quindi sottraggo 24
347300151009     c                   if        wr1noi>24  and wdiff=0
347400150722     c                   sub       24            wr1noi
347500150722     c                   endif
347600150722     c
347700150722     c* se passato orario limite picking, slitto in avanti
347800150722     c                   if        woreOLP>0
347900150722     c                   add       woreOLP       wr1noi
348000150722     c                   eval      d98spcdee='OLP'
348100160404     c                   else
348200160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
348300160404     c**                 if        wVP='S'
348400160404     c**                 add       24            wr1noi
348500160404     c**                 endif
348600150722     c                   endif
348700150722     c
348800151006   x1c                   other
348900150722     c
349000150714     c                   if        wmancatraz=' '  and d98dti>0
349100150630     c                   z-add     w99tci        wr1noi
349200150714     c                   else
349300150714     c                   z-add     w99tcc        wr1noi
349400150714     c                   endif
349500160404
349600160401     c
349700150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
349800150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
349900150722     c                   if        woreOLP=24 and wr1noi<=24
350000150722     c                   add       woreOLP       wr1noi
350100150722     c                   eval      d98spcdee='OLP'
350200150722     c                   endif
350300150722     c                   if        woreOLP=12 and wr1noi<24
350400150722     c                   add       woreOLP       wr1noi
350500150722     c                   eval      d98spcdee='OLP'
350600150722     c                   endif
350700150722     c
350800151006    1c                   endsl
350900150629     c
351000150722    1c**                 if        WR1NOI=10
351100150722     c**                 z-add     12            wr1noi
351200150722     c**                 endif
351300150722     c
351400150722     c     wr1noi        div       12            wdiv
351500150722     c                   mvr                     wres
351600150722     c                   if        wres >0
351700150722     c                   add       2             wr1noi
351800150722     c                   endif
351900150722     c
352000150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
352100150629    1c******             endif
352200150508     c
352300150605     c                   eval      wdee='S'
352400150113     C                   EXSR      RO1CDP
352500150605     c                   eval      wdee=' '
352600150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
352700150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
352800150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
352900150113     C* A QUELLA DI RICHIESTA CONSEGNA,
353000150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
353100150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
353200150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
353300150331     c*  2) IL        -> è il giorno indicato
353400150331     C     D98DCR        IFNE      *ZEROS
353500150331     C     D98tcr        andne     'P'
353600150331     C***  WPOdpcee      IFLE      WPOCR2
353700150331     C***  WPOCR2        SUB       12            WPOdpcee
353800150330     c
353900150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
354000150330     c*   dare la prima data utile di consegna
354100150330     C     WPOdpcee      IFLE      WPOCRF_dee
354200150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
354300150330     C                   z-add     WPOCRF_dee    WPOdpcee
354400150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
354500150409     c                   if        wpodpcee>wpoattdata00
354600150409     c                   eval      d98spcdee='PRV'
354700150409     c                   endif
354800150113     C                   END
354900150331     c
355000150220     c
355100150113     C                   END
355200150220     c
355300150113     c                   ENDSR
355400150330     C********************************************************************
355500150330     C*   Calcolo orario da tabella OLP
355600150330     C********************************************************************
355700150330     c     Cal_OLP       BEGSR
355800150330     c                   eval      picklim=0900
355900150330     c* Dopo l'ora limite picking, passo al giorno dopo
356000150330     C                   MOVEL(P)  'OLP'         TBECOD
356100150330     C                   MOVEL(P)  d98lna        TBEKE1
356200150330     C                   MOVEL(P)  wdaodi        TBEKE2
356300150330     C     KTBE          CHAIN     TNTBE01L
356400150330     C                   IF        not %found(TNTBE01L)
356500150330     c                   clear                   tbeke2
356600150330     C     KTBE          CHAIN     TNTBE01L
356700150330     c                   endif
356800150330     C                   IF        not %found(TNTBE01L)
356900150330     C                   MOVEL(P)  '000'         TBEKE1
357000150330     C     KTBE          CHAIN     TNTBE01L
357100150330     c                   endif
357200150330     c
357300150330     C                   IF        %found(TNTBE01L) and
357400150330     c                             %subst(tbeuni:1:4)>*zeros
357500150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
357600150330     c                   endif
357700150330     c                   ENDSR
357800150325     C********************************************************************
357900150325     C*   Riclcolo data se colli in IMP/disguido
358000150325     C********************************************************************
358100150325     c     CalcoloIMP    BEGSR
358200150325     C                   Z-ADD     IDdata        WDAT
358300150325     C                   EXSR      DATORE
358400150325     C                   Z-ADD     WORE          WR4POI
358500150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
358600151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
358700150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
358800150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
358900150325     c* calcolo il terminal di partenza
359000150325     c                   clear                   fnlv55ds
359100150325     c                   eval      d55tla='L'
359200150325     c                   eval      d55tpt='P'
359300150325     c                   eval      d55lin=IDfgs
359400150325     c                   eval      d55drf=IDdata
359500150325     c                   call      'FNLV55R'
359600150325     c                   parm                    fnlv55ds
359700150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
359800150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
359900150325     c                   EXSR      RTTRAZ
360000150325    3c                   if        d98err<>'1'
360100150325     C                   Z-ADD     CASTcn        wr4noi
360200150325     c                   else
360300150325     C                   Z-ADD     d98ttc        wr4noi
360400150325    3C                   endif
360500150325
360600150325   x2c                   else
360700150325     C                   Z-ADD     d98ttc        wr4noi
360800150325    2C                   endif
360900150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
361000150325     c*  perchè partirà un giorno dopo
361100150325    3c                   if        IDnpg='P'
361200150325     c                   add       24            WR4noi
361300150325    3c                   endif
361400150325     c
361500150325     c                   exsr      ro4cdp
361600150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
361700150325     c                   ENDSR
361800930604     C********************************************************************
361900930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
362000930604     C********************************************************************
362100930604     C     TPODAT        BEGSR
362200960229     C*
362300960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
362400960229     C*
362500960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
362600960229     C     WPOPTI        IFEQ      *ZEROS
362700960229     C                   Z-ADD     *ZEROS        D98DTI
362800960229     C                   Z-ADD     *ZEROS        D98HTI
362900960229     C                   ELSE
363000960229     C                   Z-ADD     WPOPTI        WORE
363100960229     C                   EXSR      OREDAT
363200960229     C                   Z-ADD     WDAT          D98DTI
363300960229     C     WRES          MULT      100           D98HTI
363400960229     C                   END
363500960229     C*
363600960229     C                   ELSE                                                   <-- 1 -->
363700930604     C*
363800931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
363900140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
364000140825     c*******            exsr      Tras_DEI
364100140825     c
364200930604     C*
364300150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
364400140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
364500140820     c* è data da :
364600140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
364700140820     c*  Tenere  conto, ovvero slittare in avanti, per :
364800140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
364900140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
365000140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
365100140911     c*
365200140820     c*  Da azzerare nei seguenti casi:
365300150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
365400150324     c* 4) bolla export    7) Cod manca consegna
365500140911     c* 8) bolla con DCR DOPOil o PRIMAdel
365600150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
365700150119     c*10) se completamente non arrivata e collo in IMP
365800150119     c
365900140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
366000150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
366100140925     C*  - FD   - export -
366200150217     c                   movel     'S'           DEEinDIST         1
366300150113     c                   eval      wcaldpcee='S'
366400150304     c                   eval      D98SPCDEE='PRV'
366500150324     c                   clear                   avvcmc            1
366600150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
366700150323     c                   if        d98cmc<>*blanks
366800150324     C                   Z-ADD     1             II
366900150326     C     d98cmc        LOOKUP    C2A(II)                                99
367000150324     C                   MOVEL     D2A(II)       usa2A
367100170915     C                   MOVEL     D2Z(II)       usa2Z
367200150324    4C     §2AFTC        IFEQ      'A'
367300150326     c                   eval      avvcmc='A'
367400170915     c* se non è prevista telefonata significa che è un "rimanda svincolo"
367500170915     c                   if        §2ztel=' '
367600170915     c                   eval      avvcmc='R'
367700170915     c                   endif
367800150323     c                   endif
367900150324    4C     §2AFTC        IFEQ      'G'
368000150326     c                   eval      avvcmc='G'
368100150324     c                   endif
368200150324     c                   endif
368300140820     c
368400140911     c                   z-add     d98lna        EX
368500150113     c*  la azzero nei casi previsti                 0
368600150601    1c                   select
368700150119     c* 1) Se la bolla e consegnata
368800140925     c                   when      d98dcm>0
368900150113     c                   clear                   wcaldpcee
369000150216     c                   eval      d98spcdee='CON'
369100150217     c                   movel     ' '           DEEinDIST         1
369200150323     c* 2) Se pratica di danno
369300150603     c                   when      flgdan='S'
369400150323     c                   clear                   wcaldpcee
369500150323     c                   eval      d98spcdee='CA '
369600150323     c                   eval      deeindist='S'
369700150324     c* 3) Se bloccata/rimpallo
369800150114     c                   when      wd98fbc='B'
369900150113     c                   clear                   wcaldpcee
370000150216     c                   eval      d98spcdee='BLO'
370100150217     c                   movel     ' '           DEEinDIST         1
370200150324     c* 4) Se bolla export
370300170406     c                   when      ex>0 and fl1(EX)='E'
370400150324     c                   clear                   wcaldpcee
370500150324     c                   movel     ' '           DEEinDIST         1
370600150324     c                   eval      d98spcdee='EXP'
370700150324     c* 5) Se giacente  o in attesa di apertura giacenza
370800150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
370900150324     c                   clear                   wcaldpcee
371000150324     c                   eval      d98spcdee='GIA'
371100150324     c                   eval      deeindist='S'
371200170915     c* 6a) Se codice manca consegna "A" e prevista telefonata
371300170915     c*     singifica che sono al secondo tentativo di cons.
371400150326     c                   when      d98cmc<>*blanks and avvcmc='A'
371500150324     c                   clear                   wpodpcee
371600150324     c                   clear                   wcaldpcee
371700150324     c                   movel     ' '           DEEinDIST         1
371800150324     c                   eval      d98spcdee='ATT'
371900170915     c* 6b) Se codice manca consegna "A"  e non prevista telefonata,
372000170915     c*     singifica che è un "rimanda svincolo"
372100170915     c                   when      d98cmc<>*blanks and avvcmc='R'
372200170915     c                             and d98dcr>0
372300170915     c                   clear                   wpodpcee
372400170915     c                   clear                   wcaldpcee
372500170915     c                   movel     ' '           DEEinDIST         1
372600170915     c                   eval      d98spcdee='AT1'
372700170915     c* imposto la data consegna richeista . Se < di oggi
372800170915     c* imposto oggi
372900170915     C     wpoattdata00  IFLE      WPOCRF_dee
373000170915     C                   z-add     WPOCRF_dee    WPOdpcee
373100170915     c                   else
373200170915     C                   z-add     wpoattdata00  WPOdpcee
373300170915     c                   endif
373400170915     c* 6c) Se codice manca consegna "A"  e non prevista telefonata,
373500170915     c*     singifica che è un "rimanda svincolo" ma non c'è cons richiesta
373600170915     c*     (non dovrebbe succedere): metto stato generico
373700170915     c                   when      d98cmc<>*blanks and avvcmc='R'
373800170915     c                             and d98dcr=0
373900170915     c                   clear                   wpodpcee
374000170915     c                   clear                   wcaldpcee
374100170915     c                   movel     ' '           DEEinDIST         1
374200170915     c                   eval      d98spcdee='SOS'
374300150410     c* 7) Bolla in sospensione consegna
374400150410     c                   when      §B4LBGE='S'
374500150410     c                   clear                   wpodpcee
374600150410     c                   clear                   wcaldpcee
374700150410     c                   movel     ' '           DEEinDIST         1
374800150410     c                   eval      d98spcdee='SOS'
374900150616     c
375000150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
375100161031     c                   when      wrbl='R'  or d98cbo='M '
375200150616     c                   clear                   wcaldpcee
375300150616     c                   eval      d98spcdee='   '
375400150616     c                   eval      deeindist='S'
375500150601   x1c                   other
375600150325     c*
375700150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
375800150331     c                   if        d98ffd='S'
375900150601    2c                   select
376000150403     c* FD escluso dai contatti
376100150601    2c                   when      §B4LTFD= 'N'
376200150403     c                   eval      d98spcdee='FD3'
376300150403     c                   clear                   wcaldpcee
376400150403     c                   movel     ' '           DEEinDIST         1
376500150403
376600150403     c* FD non contattato ( o da ricontattare)
376700170821    2c                   when      §B4LTFD<>' '  and  (olv82sts<>'0'  and
376800170821     c                             olv82sts<>'1' or oLRJ6FALL=' ')
376900150323     c                   clear                   wcaldpcee
377000150323     c                   movel     ' '           DEEinDIST         1
377100150323     c                   eval      d98spcdee='FFD'
377200150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
377300150424     c                   eval      wpodpcee=wpoffi
377400150403
377500150403     c* FD contattato e     IN distinta
377600150601    2c                   when      d98cons='S'
377700150401     c                   eval      d98spcdee='FD2'
377800150401     c                   clear                   wcaldpcee
377900150401     c                   movel     ' '           DEEinDIST         1
378000150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
378100150424     c                   eval      wpodpcee=wpoffi
378200150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
378300150427     c*  ritirare la merce
378400150427     C                   MOVEL     'C'           WTC                            *PARTENZA
378500150427     C                   MOVEL     'C'           WIC
378600150427     C                   Z-ADD     D98ddc        WDAT
378700150427     C                   EXSR      DATORE
378800150427     c                   z-add     wore          wr4poi
378900150427     c                   z-add     24            wr4noi
379000150427     C                   EXSR      RO4CDP
379100150427     c                   z-add     wr4poo        wore
379200150427     c                   exsr      OREDAT
379300150427     c                   movel     wdat          D98DTCTD
379400150601   x2c                   other
379500150403     c* FD contattato e NON in distinta
379600150331     c                   eval      d98spcdee='FD1'
379700150408     c                   eval      wpodpcee=wpoffi
379800150331     c                   clear                   wcaldpcee
379900150331     c                   movel     ' '           DEEinDIST         1
380000150601    2c                   endsl
380100150331     c
380200150331     c                   endif
380300150403     c
380400150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
380500150601    2c                   if        (d98tc1='A' or d98tc2='A' or
380600150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
380700150601     c                             and d98ffd= ' '
380800150325     c                   clear                   wcaldpcee
380900150325     c                   movel     ' '           DEEinDIST         1
381000150601    3c                   if        §B4LBAS= 'N'
381100150427     c                   eval      d98spcdee='AP3'
381200150403     c                   else
381300150403     c                   eval      d98spcdee='APP'
381400150601    3c                   endif
381500150601    2c                   endif
381600150325     c*   se la bolla ha il C/Assegno verifico se da contattare
381700150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
381800150325     c                   clear                   wcaldpcee
381900150325     c                   movel     ' '           DEEinDIST         1
382000170208    3c                   if        §B4LTCA= 'N'
382100150427     c                   eval      d98spcdee='CA3'
382200150403     c                   else
382300150403     c                   eval      d98spcdee='CAS'
382400150601    3c                   endif
382500150601    2c                   endif
382600150325     c* Per questi 3 casi devo impostare la data contatto destinatario
382700150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
382800150325     c                             d98spcdee='CAS'
382900170208     c
383000170208     c                   exsr      Caldam_rtc
383100170208     c                   endif
383200170208    1c                   endsl
383300140820     c
383400150113     c* Se la bolla non è in distinta calcolo la data se previsto
383500150220     c                   if        wcaldpcee='S' and
383600150220     c                             (d98cons<>'S'  or (d98cons='S' and
383700150220     c                             evemic<>'SI'))
383800150219     c
383900150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
384000150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
384100150219     c                   if        d98nsp>0 and wtrasm<>'D' and
384200150219     c                             d98dam=0
384300150219     c                   exsr      letBRV
384400150219     c                   endif
384500150219     c
384600150217     c                   eval      d98spcdee='PRG'
384700150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
384800150320     c                             d98tc1='S' or d98tc2='S')
384900150320     c                   eval      d98spcdee='APP'
385000150320     c                   endif
385100150320     c
385200150113     c                   exsr      cal_DEE
385300150113     c                   endif
385400150113     c
385500140820     C     WPOdpcee      IFEQ      *ZEROS
385600931202     C                   Z-ADD     *ZEROS        D98DEE
385700931202     C                   Z-ADD     *ZEROS        D98OEE
385800930604     C                   ELSE
385900140820     C                   Z-ADD     WPOdpcee      WORE
386000141021     C******             SUB       12            WORE
386100930604     C                   EXSR      OREDAT
386200931202     C                   Z-ADD     WDAT          D98DEE
386300090903     c                   if        d98tcc<>10
386400090903     C                   ADD       12            WRES
386500090903     c                   else
386600090903     C                   ADD       10            WRES
386700090903     c                   endif
386800090903
386900931202     C     WRES          MULT      100           D98OEE
387000090903     c
387100090903     c                   if        d98oee=1000
387200090903     c                   eval      d98oee=1030
387300090903     c                   endif
387400140925     C                   END
387500150311     c
387600150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
387700150311     c                   select
387800150311     c                   when      d98spcdee='OLP'
387900140820     c
388000150220     c* Se bolla in distinta --> data prevista = data distinta
388100150311     c                   when      deeindist='S'  and
388200150113     c                             d98cons='S'
388300150220
388400150225     c*  non la propongo senza MIC e se presente disguido o IMP
388500150220     c                   if        d98dee>d98ddc and evemic<>'SI'
388600150220     c                             and IDdata>0
388700150220     c                   else
388800150220
388900140820     c                   eval      d98dee=d98ddc
389000140925     c                   clear                   d98oee
389100150320     c* se la data è > di oggi stato PRV se non è in IMP
389200150320     c                   if        d98dee>wdaodi
389300150320     c                   if        d98spcdee<>'IMP'
389400150311     c                   eval      d98spcdee='PRV'
389500150320     c                   endif
389600150320     c
389700150225     c                   else
389800150217     c* se c'è il MIC
389900150217     c                   if        evemic='SI'
390000150217     c                   eval      d98spcdee='DDC'
390100150225     c                   endif
390200150217     c                   endif
390300150220     c                   endif
390400150217     c
390500150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
390600150722     c                   eval      d98spcdee='PRV'
390700150311     c                   endsl
390800170208     c
390900170208     c* se non ho ancora calcolato la data arrivo reale o teorica tempi cliente, lo faccio ora
391000170208     c                   if        d98dam_rtc=0
391100170208     c                   eval      wsolodam='S'
391200170208     c                   exsr      Caldam_rtc
391300170208     c                   eval      wsolodam=' '
391400170208     c                   endif
391500931202     C*
391600931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
391700931202     C     WPODCI        IFEQ      *ZEROS
391800931202     C                   Z-ADD     *ZEROS        D98DCI
391900931202     C                   Z-ADD     *ZEROS        D98HCI
392000931202     C                   ELSE
392100931202     C                   Z-ADD     WPODCI        WORE
392200931202     C                   EXSR      OREDAT
392300931202     C                   Z-ADD     WDAT          D98DCI
392400090902     c                   if        d98tci<>10
392500931202     C                   ADD       12            WRES
392600090902     c                   else
392700090902     C                   ADD       10            WRES
392800090902     c                   endif
392900090902
393000931202     C     WRES          MULT      100           D98HCI
393100090901     c
393200090903     c                   if        d98hci=1000
393300090901     c                   eval      d98hci=1030
393400090901     c                   endif
393500090901     c
393600931202     C                   END
393700931202     C*
393800931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
393900931202     C     WPODCE        IFEQ      *ZEROS
394000931202     C                   Z-ADD     *ZEROS        D98DCE
394100931202     C                   Z-ADD     *ZEROS        D98HCE
394200931202     C                   ELSE
394300931202     C                   Z-ADD     WPODCE        WORE
394400931202//// C**************       SUB  12        WORE
394500931202     C                   EXSR      OREDAT
394600931202     C                   Z-ADD     WDAT          D98DCE
394700090902     c                   if        d98tcc >10
394800931202     C                   ADD       12            WRES
394900090902     c                   else
395000090902     C                   ADD       10            WRES
395100090902     c                   endif
395200090902     c
395300931202     C     WRES          MULT      100           D98HCE
395400090902     c
395500090903     c                   if        d98hce=1000
395600090902     c                   eval      d98hce=1030
395700090902     c                   endif
395800090902     c
395900931202     C                   END
396000960229     C*
396100960229     C                   ENDIF                                                  <-- 1 ---
396200930604     C*
396300930604     C                   ENDSR
396400170208     C********************************************************************
396500170208     C* Calcolo data di arrivo teorica o reale cliente
396600170208     C********************************************************************
396700170208     c     Caldam_rtc    BEGSR
396800170208     c
396900170208    2c                   select
397000170220    2c                   when      d98dti>0 and wmancatraz= ' ' and wpodti>0
397100170220     c
397200170220     c* questa specifica non serve: se è piena la data è pieno wpodti e viceversa
397300170220     c*****              movel     d98dti        d98dtctd
397400170208     c* se piene le ore già portate ad un giorno lavorativo
397500170220     c*******            if        wpodti>0
397600170208     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
397700170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
397800170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
397900170208     C                   Z-ADD     WPODTI        W10POI            9 0
398000170208     C                   EXSR      R10FES
398100170208     c                   z-add     w10poo        wore
398200170208     C                   EXSR      OREDAT
398300170208     c                   if        wsolodam<>'S'
398400170208     C                   movel     WDAT          D98dtctd
398500170208     c                   endif
398600170208     C                   movel     WDAT          D98dam_rtc
398700170220     c*****              endif
398800170208     c
398900170208    2c                   when      d98dam>0
399000170208     C                   Z-ADD     D98dam        WDAT
399100170208     C                   EXSR      DATORE
399200170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
399300170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
399400170208     C                   Z-ADD     Wore          W10POI            9 0
399500170208     C                   EXSR      R10FES
399600170208     c                   z-add     w10poo        wore
399700170208     C                   EXSR      OREDAT
399800170208     c                   if        wsolodam<>'S'
399900170208     C                   movel     WDAT          D98dtctd
400000170208     c                   endif
400100170208     C                   movel     WDAT          D98dam_rtc
400200170208   x2c                   other
400300170208     c* data teorica di arrivo tenendo conto dell'IMP
400400170208     c                   if        d98nsp>0
400500170208     c                   exsr      letbrv
400600170208     c                   endif
400700170208     c                   if        idDATA>0
400800170208     c                   exsr      CalcoloIMP
400900170208     c                   z-add     wr1poi        WORE
401000170208     c                   else
401100170208     c                   z-add     wpoptc_dee    WORE
401200170208     c                   endif
401300170208     C                   EXSR      OREDAT
401400170208     c                   if        wsolodam<>'S'
401500170208     c                   movel     wdat          d98dtctd
401600170208     c                   endif
401700170208     C                   movel     WDAT          D98dam_rtc
401800170208    2c                   endsl
401900170208     c
402000170208     c
402100170208     c                   ENDSR
402200150218     C********************************************************************
402300150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
402400150218     C********************************************************************
402500150218     c     LetBRV        BEGSR
402600150219     c                   clear                   bollaOK           1
402700150219     c                   clear                   IDnpg
402800150219     c                   clear                   IDdata
402900150219     c                   clear                   IDora
403000150219     c                   clear                   IDfgs
403100150219     c
403200150218     C     Kart          SETLL     FNART01L
403300150218     C     Kart          READE     FNART01L
403400151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
403500150219     c                   clear                   DISfgs
403600150219     c                   clear                   DISdata
403700150219     c                   clear                   DISnpg
403800150219     c                   clear                   DISora
403900150219     c                   clear                   IMPdata
404000150219     c                   clear                   IMPfgs
404100150508     c* per ogni collo verifico se in IMP o disguidato
404200150218     c* al primo che NON trovo, esco come --> OK
404300150218     c     kbrv          setll     fnbrv07l
404400150218     c     kbrv          reade     fnbrv07l
404500150219    2c                   dow       not %eof(fnbrv07l)
404600150219    3c                   if        brvatr='D'  or brvnpg=3
404700150218     c* prendo la data della spunta
404800150218     c                   clear                   fnlv53ds
404900150219     c                   eval      d53tla='L'
405000150218     C                   MOVEL     BRvNPG        D53NPG
405100150218     C                   Z-ADD     BRvNFV        D53NFV
405200150218     C                   MOVEL     BRvFGS        D53FGS
405300150218     C                   CALL      'FNLV53R'
405400150218     C                   PARM                    fnlv53ds
405500150218    4c                   if        d53err<>*blanks
405600150219    5c                   if        brvdcs>0
405700150218     c                   eval      d53dfv=brvdcs
405800150218     c                   else
405900150219    6c                   if        brvdfs>0
406000150218     c                   eval      d53dfv=brvdfs
406100150219    6c                   endif
406200150219    5c                   endif
406300150219    4c                   endif
406400150218     c
406500150219    4c                   select
406600150219     c* se si tratta di DISGUIDO
406700150218     c                   when      brvatr='D'
406800150219     c
406900150219    5c                   if        d53dfv=DISdata
407000150219     c* Verifico l'ora
407100150219    6c                   if        brvhcs>0 and brvhcs>DISora
407200150219     c                   eval      DISdata=d53dfv
407300150219     c                   eval      DISfgs =brvfgs
407400150219     c                   eval      DISnpg =brvnpg
407500150219     c                   eval      DISora =brvhcs
407600150219   x6c                   else
407700150219    7c                   if        brvhms>0 and brvhms>DISora
407800150219     c                   eval      DISdata=d53dfv
407900150219     c                   eval      DISfgs =brvfgs
408000150219     c                   eval      DISnpg =brvnpg
408100150219     c                   eval      DISora =brvhms
408200150219    7c                   endif
408300150219    6c                   endif
408400150219    5c                   endif
408500150219     c
408600150219    5c                   if        d53dfv>DISdata
408700150219     c                   eval      DISdata=d53dfv
408800150219     c                   eval      DISfgs =brvfgs
408900150219     c                   eval      DISnpg =brvnpg
409000150219     c                   eval      DISora =brvhcs
409100150219     c                   if        disora=0
409200150219     c                   eval      DISora =brvhms
409300150219     c                   endif
409400150219    5c                   endif
409500150219     c
409600151009     c* se si tratta di IMP
409700151009    4c                   when      d53spg='P' and d53dfv>impdata
409800151009     c                   eval      IMPdata=d53dfv
409900151009     c                   eval      IMPfgs =brvfgs
410000150219    4c                   endsl
410100150219    3c                   endif
410200151009
410300150219     c     kbrv          reade     fnbrv07l
410400150219    2c                   enddo
410500150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
410600150219     c*  esco dal ciclo: la bolla è OK
410700150219    2c                   if        impdata=0 and disdata=0
410800150219     c                   eval      bollaOK='S'
410900150219     c                   clear                   IDdata
411000150219     c                   clear                   IDfgs
411100150219     c                   clear                   IDora
411200150219     c                   clear                   IDnpg
411300150219   x2c                   else
411400150219     c* memorizzo la data più alta
411500150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
411600150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
411700150219     c                   eval      IDdata=IMPdata
411800150219     c                   eval      IDfgs=IMPfgs
411900150219     c                   eval      IDora=0
412000150219     c                   eval      IDnpg='P'
412100150219    3c                   endif
412200150219    3c                   if        (DISdata>0 and DISdata=IDdata
412300150219     c                             and IDnpg<>'P' and DISora>IDora) or
412400150219     c                             (DISdata>0 and DISdata>IDdata)
412500150219     c                   eval      IDdata=DISdata
412600150219     c                   eval      IDfgs=DISfgs
412700150219     c                   eval      IDora=DISora
412800150219     c                   eval      IDnpg=%char(DISnpg)
412900150219    3c                   endif
413000150219    2c                   endif
413100150218     c
413200150218     C     Kart          READE     FNART01L
413300150219    1c                   enddo
413400150219     c
413500150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
413600150508     c**                 if        d98duc> 0 and d98duc>IDdata
413700150508     c**                 eval      bollaOK='S'
413800150508     c**                 clear                   IDdata
413900150508     c**                 clear                   IDfgs
414000150508     c**                 clear                   IDora
414100150508     c**                 clear                   IDnpg
414200150508     c**                 endif
414300150219     c
414400150218     c
414500150218     C                   ENDSR
414600140825     C********************************************************************
414700140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
414800140825     C********************************************************************
414900140825     c     TRAS_DEI      BEGSR
415000140825     C     WPODEI        IFEQ      *ZEROS
415100140825     C                   Z-ADD     *ZEROS        D98DEI
415200140825     C                   Z-ADD     *ZEROS        D98OEI
415300140825     C                   ELSE
415400140825     C                   Z-ADD     WPODEI        WORE
415500140825     C                   SUB       12            WORE
415600140825     C                   EXSR      OREDAT
415700140825     c
415800140825     C                   Z-ADD     WDAT          D98DEI
415900140825     c                   if        d98tci<>10
416000140825     C                   ADD       12            WRES
416100140825     c                   else
416200140825     C                   ADD       10            WRES
416300140825     c                   endif
416400140825     c
416500140825     C     WRES          MULT      100           D98OEI
416600140825     c
416700140825     c                   if        d98oei=1000
416800140825     c                   eval      d98oei=1030
416900140825     c                   endif
417000140825     c
417100140825     C                   END
417200140825     c                   ENDSR
417300930604     C********************************************************************
417400930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
417500930604     C********************************************************************
417600930604     C     CHKFOR        BEGSR
417700930604     C*
417800930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
417900930604     C* E QUINDI FORZO LA CONFORMITA' A SI
418000941227     C     D98ERR        IFEQ      '0'
418100930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
418200930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
418300930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
418400930604     C                   MOVEL     '1'           D98ERR
418500930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
418600930604     C                   END
418700930604     C                   END
418800990212     C*
418900990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
419000990212     C* E QUINDI FORZO LA CONFORMITA' A SI
419100990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
419200990212     C     D98ERR        IFEQ      '0'
419300990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
419400141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
419500990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
419600990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
419700990212     C                   MOVEL     '1'           D98ERR
419800990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
419900990212     C                   END
420000990212     C                   END
420100990308     C*
420200990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
420300990308     C* E QUINDI FORZO LA CONFORMITA' A SI
420400100614     C**   D98ERR        IFEQ      '0'
420500100614     C**   WFBR          IFEQ      'S'                                          *RESO
420600100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
420700100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
420800100614     C**                 MOVEL     '1'           D98ERR
420900100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
421000100614     C**                 END
421100100614     C**                 END
421200930604     C*
421300930604     C                   ENDSR
421400990212     C********************************************************************
421500990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
421600990212     C********************************************************************
421700990212     C     CHKDAN        BEGSR
421800990212     C*
421900990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
422000150603     c                   movel     'N'           opendan           1
422100040809      *
422200040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
422300040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
422400040809      * legate alla spedizione che passo
422500040809     c                   clear                   fidn12ds
422600040809     c                   eval      i12aas = D98AAS
422700040809     c                   eval      i12lnp = D98LNP
422800040809     c                   eval      i12nrs = D98NRS
422900040809     c                   eval      i12nsp = D98NSP
423000040809     c                   eval      i12fel = 'E'
423100040809     c                   eval      i12fan = 'E'
423200040809     c*
423300040809     c                   call      'FIDN12R'
423400040809     c                   parm                    fidn12ds
423500040809     c*
423600040809     c* se non ci sono errori
423700040809     c                   if        o12err = ' '
423800040809     c* se numero di CA trovate maggiore di zero
423900040809     c                   if        o12nca > 0
424000040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
424100150603     c
424200161215     c                   z-add     1             aa                2 0
424300161215     c                   dow       aa <= o12nca
424400150603     C* APERTA
424500161215     C***  skdch(aa)     IFgt      0
424600161215     c****               movel     'S'           opendan
424700161215     c***                endif
424800161215     c                   if        skdca(aa)>0 and skdca(aa)>d98dac
424900161215     c                   eval      d98dac =skdca(aa)
425000161215     c                   eval      d98dcc =skdch(aa)
425100161215     c                   endif
425200150603     c*
425300161215     c                   add       1             aa
425400161215     c                   enddo
425500150603     c
425600040809     c                   endif
425700040809     c                   endif
425800990212     C*
425900990212     C                   ENDSR
426000941228     C********************************************************************
426100941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
426200941228     C********************************************************************
426300941228     C     CALORP        BEGSR
426400941228     C*
426500941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
426600941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
426700941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
426800960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
426900950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
427000950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
427100950110     C                   ELSE
427200950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
427300950110     C                   ENDIF
427400941228     C                   EXSR      RO2COP
427500941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
427600941228     C* SPED. IL GIORNO DOPO
427700941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
427800941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
427900941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
428000941228     C*
428100941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
428200941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
428300941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
428400941228     C                   Z-ADD     WPODSP        WR2POI
428500950110     C                   Z-ADD     WPOPPC        WR2POF
428600941228     C                   EXSR      RO2COP
428700941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
428800941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
428900941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
429000941228     C*
429100941228     C                   ENDSR
429200930414     C********************************************************************
429300930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
429400930414     C********************************************************************
429500930414     C     CALORT        BEGSR
429600930414     C*
429700930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
429800931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
429900930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
430000931126     C                   Z-ADD     *ZEROS        W98HF
430100950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
430200950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
430300950110     C                   ELSE
430400950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
430500950110     C                   ENDIF
430600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
430700931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
430800930608     C                   ELSE
430900931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
431000930608     C                   END
431100931126     C                   EXSR      RO2COV
431200931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
431300930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
431400930414     C*
431500930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
431600930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
431700930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
431800931126     C                   Z-ADD     *ZEROS        W98HF
431900950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
432000931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
432100931126     C                   EXSR      RO2COV
432200931126     C                   Z-ADD     WR2NOV        W98RTC
432300930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
432400930414     C*
432500930414     C                   ENDSR
432600930414     C********************************************************************
432700930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
432800930414     C********************************************************************
432900930414     C     CALORC        BEGSR
433000930511     C*
433100930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
433200930511     C* I TEMPI DI RITARDO AD OGGI
433300930511     C     WPODCM        IFEQ      *ZEROS
433400960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
433500930511     C                   END
433600940117     C*
433700940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
433800940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
433900940117     C* ALLA DATA DI FINE CONSEGNABILITA'
434000940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
434100940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
434200940118££££ C     WPODFC        IFNE      *ZEROS
434300940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
434400940118££££ C                   END
434500930414     C*
434600930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
434700930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
434800930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
434900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
435000931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
435100930608     C                   ELSE
435200931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
435300930608     C                   END
435400931126     C                   Z-ADD     WPODCM        WR5POF            9 0
435500931126     C                   EXSR      RO5COV
435600931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
435700930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
435800930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
435900060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
436000060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
436100930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
436200930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
436300930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
436400930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
436500930414     C*
436600930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
436700930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
436800930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
436900931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
437000931126     C                   Z-ADD     WPODCM        WR5POF            9 0
437100931126     C                   EXSR      RO5COV
437200931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
437300930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
437400930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
437500060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
437600060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
437700930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
437800930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
437900930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
438000930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
438100930414     C*
438200930414     C                   ENDSR
438300930414     C********************************************************************
438400930414     C*   CALORR - CALCOLA ORE REALI DI RESA
438500930414     C********************************************************************
438600930414     C     CALORR        BEGSR
438700930414     C*
438800950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
438900950110     C                   ADD       W98RTI        W98RRI
439000941228     C                   ADD       W98RCI        W98RRI
439100950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
439200950110     C                   ADD       W98RTC        W98RRC
439300941228     C                   ADD       W98RCC        W98RRC
439400930414     C*
439500930414     C                   ENDSR
439600930420     C********************************************************************
439700930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
439800930420     C********************************************************************
439900930420     C     CALDIF        BEGSR
440000930420     C*
440100941228     C* PARTENZA
440200941228     C     W98RPI        SUB       D98TPI        W98FPI
440300941228     C     W98RPC        SUB       D98TPC        W98FPC
440400941228     C*
440500930420     C* TRAZIONE
440600930423     C     W98RTI        SUB       D98TTI        W98FTI
440700930423     C     W98RTC        SUB       D98TTC        W98FTC
440800930420     C*
440900930420     C* CONSEGNA
441000090827     C***  W98RCI        SUB       D98TCI        W98FCI
441100090827     C     W98RCI        SUB       w99TCI        W98FCI
441200090827     C***  W98RCC        SUB       D98TCC        W98FCC
441300090827     C     W98RCC        SUB       w99TCC        W98FCC
441400930420     C*
441500930420     C* RESA
441600090921     C***  W98RRI        SUB       D98TRI        W98FRI
441700090921     C     W98RRI        SUB       w99TRI        W98FRI
441800090921     c
441900090921     C***  W98RRC        SUB       D98TRC        W98FRC
442000170104     c
442100170104     c*** se le ore reali interne di RESA usate sono < delle ore reali cliente, uso le
442200170104     c*    reali interne
442300170104     c                   if        w98rri<w98rrc
442400170104     C     W98RRI        SUB       w99TRC        W98FRC
442500170104     c                   else
442600170104     C     W98RRC        SUB       w99TRC        W98FRC
442700170104     c                   endif
442800930420     C*
442900930420     C                   ENDSR
443000930420     C********************************************************************
443100930420     C*   CALCNF - CALCOLA LE CONFORMITA'
443200930420     C********************************************************************
443300930420     C     CALCNF        BEGSR
443400941228     C*
443500941228     C*-------------------------------------------------------------------
443600941228     C* CONFORMITA' PARTENZA  INTERNA
443700941228     C*-------------------------------------------------------------------
443800941228     C                   Z-ADD     W98FPI        W98NPI
443900941228     C     W98FPI        IFLE      *ZEROS
444000941228     C                   MOVEL     'S'           D98CPI
444100941228     C                   ELSE
444200941228     C                   MOVEL     'N'           D98CPI
444300941228     C                   END
444400941228     C*
444500941228     C*-------------------------------------------------------------------
444600941228     C* CONFORMITA' PARTENZA  CLIENTE
444700941228     C*-------------------------------------------------------------------
444800941228     C                   Z-ADD     W98FPC        W98NPC
444900941228     C     W98FPC        IFLE      *ZEROS
445000941228     C                   MOVEL     'S'           D98CPC
445100941228     C                   ELSE
445200941228     C                   MOVEL     'N'           D98CPC
445300941228     C                   END
445400930420     C*
445500930420     C*-------------------------------------------------------------------
445600930420     C* CONFORMITA' TRAZIONE  INTERNA
445700930420     C*-------------------------------------------------------------------
445800930510     C                   Z-ADD     W98FTI        W98NTI
445900930423     C     W98FTI        IFLE      *ZEROS
446000930420     C                   MOVEL     'S'           D98CTI
446100930420     C                   ELSE
446200930420     C                   MOVEL     'N'           D98CTI
446300930420     C                   END
446400930420     C*
446500930420     C*-------------------------------------------------------------------
446600930420     C* CONFORMITA' TRAZIONE  CLIENTE
446700930420     C*-------------------------------------------------------------------
446800930510     C                   Z-ADD     W98FTC        W98NTC
446900930423     C     W98FTC        IFLE      *ZEROS
447000930420     C                   MOVEL     'S'           D98CTC
447100930420     C                   ELSE
447200930420     C                   MOVEL     'N'           D98CTC
447300930420     C                   END
447400930420     C*
447500930420     C*-------------------------------------------------------------------
447600930420     C* CONFORMITA' CONSEGNA  INTERNA
447700930420     C*-------------------------------------------------------------------
447800150226     c                   if        wmancatraz='S'
447900150226     c                   clear                   d98dti
448000150226     c                   clear                   d98hti
448100150226     c                   endif
448200150226     c
448300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
448400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
448500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
448600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
448700050914     c                   if        consideraora='S'
448800050914     c     w98fci        sub       24            w98nci
448900050914     c                   add       24            d98hrc
449000050914     c                   else
449100930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
449200050914     c                   endif
449300080116     c
449400110923     c                   select
449500080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
449600080116     c*  --> trasformo il ritardo in 0
449700090827     c                   when      d98cca='2'
449800080116     c                   z-add     0             w98nci
449900090827
450000080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
450100080116     c*  e sto leggendo la bolla in arrivo
450200080116     c*  --> trasformo il ritardo = 0
450300090827     c                   when      d98cca='1' and d98tbo='A'
450400080116     c                   z-add     0             w98nci
450500090902     c
450600090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
450700090902     c                   when      wmancaTRAZ='S'
450800090902     c                   z-add     0             w98nci
450900090902     c                   clear                   d98dti
451000090902     c                   clear                   d98hti
451100110923     c
451200110923     c* Ignoro ritardo se pefect order
451300130221     c                   when      d98xco >*zeros
451400110923     c                   z-add     0             w98nci
451500090827     c
451600090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
451700091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
451800091014     c*  controllo ora soglia  consegna da tabella OSC)
451900091009     c                   when      d98tci=10   and
452000091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
452100091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
452200091014     c*
452300091014     c                             d98hci=1030
452400091014     C                   MOVEL     'OSC  '       WR3TAB
452500091014     C                   eval      WR3HHI=d98hmc
452600091014     C                   EXSR      RO3ORE
452700131218
452800091014     c                   if        wr3matpom='M' and d98hmc>1030
452900131218     c                   exsr      ctrLAVH1030
453000131218
453100131218     c                   if        wlav1030<>'S'
453200091014     c                   add       12            w98nci
453300091014     c                   endif
453400131218     c                   endif
453500090928     c
453600090827     c                   endsl
453700050914     c
453800930524     C                   ELSE                                                   <-- 1 -->
453900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
454000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
454100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
454200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
454300930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
454400930608     C                   ELSE
454500930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
454600930608     C                   END
454700930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
454800930525     C                   EXSR      RO2COV
454900930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
455000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
455100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
455200930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
455300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
455400930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
455500930608     C                   ELSE
455600930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
455700930608     C                   END
455800931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
455900930525     C                   EXSR      RO2COV
456000930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
456100930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
456200930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
456300930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
456400930525     C* E' IL RITARDO
456500930610     C     WNOCRI        SUB       W98RCI        W98NCI
456600050901     c
456700050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
456800050901     c*  --> trasformo il ritardo in anticipo
456900050901     c                   if        d98cca='2'
457000080116     c***                z-sub     w98nci        w98nci
457100080116     c                   z-add     0             w98nci
457200050901     c                   endif
457300111018     c* Ignoro ritardo se pefect order
457400130221     c                   if        d98xco >*zeros
457500111018     c                   z-add     0             w98nci
457600111018     c                   endif
457700061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
457800061121     c*  e sto leggendo la bolla in arrivo
457900061121     c*  --> trasformo il ritardo in anticipo
458000061121     c                   if        d98cca='1' and d98tbo='A'
458100080116     c***                z-sub     w98nci        w98nci
458200080116     c                   z-add     0             w98nci
458300061121     c                   endif
458400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
458500070827     c*  mittente, --> consegnato in anticipo
458600070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
458700140403     c                   if        Callar5=' '
458800070827     c                   exsr      LeggiAR5
458900140403     c                   endif
459000070827     c
459100070827     c                   if        dcrpart<>'S'
459200070824     c                   z-sub     w98nci        w98nci
459300070824     c                   endif
459400070827     c
459500070824     c                   endif
459600050907     c
459700050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
459800050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
459900050907     c*  lo trasformo in anticipo perchè non sono in ritardo
460000050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
460100050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
460200050916     c***                if        d98dfc>0
460300070828     c                   if        d98dcm=0  and w98nci>0
460400050907     c                   z-sub     w98nci        w98nci
460500050907     c                   sub       oreILoDOPO    w98nci
460600050907     c                   endif
460700050901     c
460800930525     C                   ELSE                                                   <-- 2 -->
460900070521     c* non do ritardo se la consegna reale cade nel giorno
461000070521     c*  della consegna richiesta se presente IL
461100100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
461200070521     c                             wpodcm<=wpocr1
461300070521     c                   clear                   w98nci
461400100224     c                   else                                                   ---2a -->
461500930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
461600090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
461700090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
461800930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
461900930525     C* E' IL RITARDO O L'ANTICIPO
462000930525     C     W98RCI        SUB       WNOCRF        W98NCI
462100070115     c
462200070115     c* non do ritardo se comunque la consegna reale cade nel giorno
462300070115     c*  della consegna richiesta se presente IL
462400070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
462500070521     c***                          wpodcm<=wpocr1  and w98nci>0
462600070521     c***                clear                   w98nci
462700070521     c***                endif
462800070115     c
462900930525     C                   ELSE                                                   <-- 3 -->
463000930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
463100930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
463200930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
463300930525     C                   END                                                    <-- 3 ---
463400100224     C                   END                                                    <-- 2a---
463500100224
463600100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
463700100224     c                   select
463800100224     c                   when      d98cca='2'
463900100224     c                   z-add     0             w98nci
464000100224
464100100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
464200100224     c*  e sto leggendo la bolla in arrivo
464300100224     c                   when      d98cca='1' and d98tbo='A'
464400100224     c                   z-add     0             w98nci
464500100224     c                   endsl
464600100224     c
464700100224     C                   END                                                    <-- 2 ---
464800070521     C                   END                                                    <-- 1 ---
464900070521     c
465000970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
465100970915     C*/         WAZNCI    IFEQ 'S'
465200970915     C*/         W98NCI    ANDLT*ZEROS
465300970915     C*/                   Z-ADD*ZEROS    W98NCI
465400970915     C*/                   ENDIF
465500970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
465600050922     c* lo faccio solo se non c'e' la data cons richiesta
465700970915     C     D98DTI        IFEQ      *ZEROS
465800050922     c     d98dcr        andeq     *zeros
465900970915     C                   Z-ADD     *ZEROS        W98NCI
466000970915     C                   ENDIF
466100930524     C*
466200930524     C* CONTROLLO CONFORMITA
466300930423     C     W98NCI        IFLE      *ZEROS
466400930524     C                   MOVEL     'S'           D98CCI
466500930420     C                   ELSE
466600930524     C                   MOVEL     'N'           D98CCI
466700930420     C                   END
466800930420     C*
466900930420     C*-------------------------------------------------------------------
467000930420     C* CONFORMITA' CONSEGNA  CLIENTE
467100930420     C*-------------------------------------------------------------------
467200930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
467300930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
467400930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
467500050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
467600050914     c                   if        consideraora='S'
467700050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
467800050914     c                   else
467900050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
468000050914     c                   endif
468100080116     c
468200080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
468300080116     c*  --> trasformo il ritardo =0
468400090827     c                   select
468500090827     c                   when      d98cca='2'
468600080116     c                   z-add     0             w98ncc
468700090827
468800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
468900080116     c*  e sto leggendo la bolla in arrivo
469000080116     c*  --> trasformo il ritardo =0
469100090827     c                   when      d98cca='1' and d98tbo='A'
469200080116     c                   z-add     0             w98ncc
469300090902     c
469400090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
469500090902     c                   when      wmancaTRAZ='S'
469600090902     c                   z-add     0             w98ncc
469700110923     c* Ignoro ritardo se Perfect Order
469800130221     c                   when      d98xco >*zeros
469900110923     c                   z-add     0             w98ncc
470000090902     c
470100090827     c
470200090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
470300090827     c* sono  in ritardo di 12 ore
470400091009     c                   when      d98tcc=10  and
470500091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
470600091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
470700091014     c*
470800091014     c                             d98hce=1030
470900091014     C                   MOVEL     'OSC  '       WR3TAB
471000091014     C                   eval      WR3HHI=d98hmc
471100091014     C                   EXSR      RO3ORE
471200091014     c                   if        wr3matpom='M' and d98hmc>1030
471300131218     c                   exsr      ctrLAVH1030
471400131218
471500131218     c                   if        wlav1030<>'S'
471600131218     c                   add       12            w98ncc
471700131218     c                   endif
471800131218     c****               add       12            w98ncc
471900091014     c                   endif
472000131218     c
472100091014     c***                z-add     12            w98ncc
472200090827     c                   endsl
472300050914     c
472400930524     C                   ELSE                                                   <-- 1 -->
472500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
472600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
472700930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
472800930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
472900930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
473000930525     C                   EXSR      RO2COV
473100930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
473200930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
473300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
473400930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
473500930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
473600931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
473700930525     C                   EXSR      RO2COV
473800930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
473900930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
474000930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
474100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
474200930525     C* E' IL RITARDO
474300930525     C     WNOCRI        SUB       W98RCC        W98NCC
474400050901     c
474500050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
474600050901     c*  --> trasformo il ritardo in anticipo
474700050901     c                   if        d98cca='2'
474800080116     c***                z-sub     w98ncc        w98ncc
474900080116     c                   z-add     0             w98ncc
475000050901     c                   endif
475100111018     c* Ignoro ritardo se pefect order
475200130221     c                   if        d98xco >*zeros
475300111018     c                   z-add     0             w98ncc
475400111018     c                   endif
475500061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
475600061121     c*  e sto leggendo la bolla in arrivo
475700061121     c*  --> trasformo il ritardo in anticipo
475800061121     c                   if        d98cca='1' and d98tbo='A'
475900080116     c***                z-sub     w98ncc        w98ncc
476000080116     c                   z-add     0             w98ncc
476100061121     c                   endif
476200070824     c
476300070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
476400070827     c*  mittente, --> consegnato in anticipo
476500070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
476600070827     c* SBR per vedere se tassativa
476700070827     c                   if        Callar5=' '
476800070827     c                   EXSR      LeggiAR5
476900070827     c                   endif
477000070827     c                   if        dcrpart<>'S'
477100070824     c                   z-sub     w98ncc        w98ncc
477200070824     c                   endif
477300070824     c                   endif
477400050907     c
477500050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
477600050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
477700050907     c*  lo trasformo in anticipo perchè non sono in ritardo
477800050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
477900050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
478000050916     c***                if        d98dfc>0
478100070828     c                   if        d98dcm=0 and w98ncc>0
478200050907     c                   z-sub     w98ncc        w98ncc
478300050907     c                   sub       oreILoDOPO    w98ncc
478400050907     c                   endif
478500050901     c
478600930525     C                   ELSE                                                   <-- 2 -->
478700070521     c* Non do ritardo se la consegna reale cade nel giorno
478800070521     c*  della consegna richiesta se presente IL
478900100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
479000070521     c                             wpodcm<=wpocr2
479100070521     c                   clear                   w98ncc
479200100224     c                   else                                                   <--2a -->
479300070521     c
479400930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
479500090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
479600090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
479700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
479800930525     C* E' IL RITARDO O L'ANTICIPO
479900930525     C     W98RCC        SUB       WNOCRF        W98NCC
480000070115     c
480100070115     c* non do ritardo se comunque la consegna reale cade nel giorno
480200070115     c*  della consegna richiesta se presente IL
480300070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
480400070521     c***                          wpodcm<=wpocr2 and w98ncc>0
480500070521     c***                clear                   w98ncc
480600070521     c***                endif
480700070115     c
480800930525     C                   ELSE                                                   <-- 3 -->
480900930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
481000930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
481100930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
481200930525     C                   END                                                    <-- 3 ---
481300100224     C                   END                                                    <-- 2a---
481400100224
481500100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
481600100224     c                   select
481700100224     c                   when      d98cca='2'
481800100224     c                   z-add     0             w98nci
481900100224
482000100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
482100100224     c*  e sto leggendo la bolla in arrivo
482200100224     c                   when      d98cca='1' and d98tbo='A'
482300100224     c                   z-add     0             w98nci
482400100224     c                   endsl
482500100224     c
482600100224     C                   END                                                    <-- 2 ---
482700070521     C                   END                                                    <-- 1 ---
482800930524     C*
482900930524     C* CONTROLLO CONFORMITA
483000930423     C     W98NCC        IFLE      *ZEROS
483100930524     C                   MOVEL     'S'           D98CCC
483200930420     C                   ELSE
483300930524     C                   MOVEL     'N'           D98CCC
483400930420     C                   END
483500930420     C*
483600930420     C*-------------------------------------------------------------------
483700930420     C* CONFORMITA' RESA INTERNA
483800930420     C*-------------------------------------------------------------------
483900930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
484000930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
484100930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
484200050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
484300050914     c                   if        consideraora='S'
484400050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
484500050914     c                   else
484600050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
484700050914     c                   endif
484800080116     c
484900080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
485000090827     c                   select
485100090827     c                   when      d98cca='2' and w98nri>0
485200080116     c                   z-add     0             w98nri
485300090827     c
485400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
485500080116     c*  e sto leggendo la bolla in arrivo
485600090827     c                   when      d98cca='1' and d98tbo='A' and
485700080116     c                                            w98nri>0
485800080116     c                   z-add     0             w98nri
485900110923     c
486000110923     c* Ignoro ritardo se Perfect Order
486100130221     c                   when      d98xco >*zeros
486200110923     c                   z-add     0             w98nri
486300110923     c
486400090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
486500090827     c* sono  in ritardo di 12 ore
486600091009     c                   when      d98tci=10  and
486700091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
486800091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
486900091014     c*
487000091014     c                             d98hci=1030
487100091014     C                   MOVEL     'OSC  '       WR3TAB
487200091014     C                   eval      WR3HHI=d98hmc
487300091014     C                   EXSR      RO3ORE
487400091014     c                   if        wr3matpom='M' and d98hmc>1030
487500131218     c                   exsr      ctrLAVH1030
487600131218
487700131218     c                   if        wlav1030<>'S'
487800091014     c                   add       12            w98nri
487900091014     c                   endif
488000131218     c                   endif
488100091014     c
488200091014     c***                z-add     12            w98nri
488300090827     c                   endsl
488400090827
488500930524     C                   ELSE                                                   <-- 1 -->
488600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
488700930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
488800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
488900931206     C     WPODCM        ANDLE     WPOCR1
489000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
489100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
489200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
489300931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
489400930525     C                   EXSR      RO2COV
489500930525     C     WR2NOV        IFNE      *ZEROS
489600930525     C     WR2NOV        MULT      -1            W98NRI
489700930525     C                   END
489800930524     C                   ELSE                                                   <-- 2 -->
489900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
490000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
490100930524     C     WPODCM        IFLT      WPOCRI
490200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
490300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
490400930524     C                   ELSE
490500931206     C                   Z-ADD     WPOCR1        WPORFI
490600930524     C                   Z-ADD     WPODCM        WPORFF
490700930524     C                   END
490800930524     C* CALCOLO ORE DI DIFFERENZA
490900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
491000930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
491100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
491200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
491300930524     C                   EXSR      RO2COV
491400930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
491500050902     c
491600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
491700100224     c                   if        d98cca='2' and w98nri>0
491800100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
491900080116     c***                z-sub     w98nri        w98nri
492000080116     c                   z-add     0             w98nri
492100050902     c                   endif
492200111018     c* Ignoro ritardo se pefect order
492300130221     c                   if        d98xco >*zeros
492400111018     c                   z-add     0             w98nri
492500111018     c                   endif
492600061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
492700061121     c*  e sto leggendo la bolla in arrivo
492800100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
492900100224     c***                if        d98cca='1' and d98tbo='A' and
493000100224     c***                                         w98nri>0 and wpodcm<wpocri
493100080116     c***                z-sub     w98nri        w98nri
493200080116     c                   z-add     0             w98nri
493300061121     c                   endif
493400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
493500070827     c*  mittente, --> consegnato in anticipo
493600070828     c                   if        d98tcr=' ' and d98nsp>0  and
493700070828     c                                            w98nri>0 and wpodcm<wpocri
493800070827     c                   if        CallAR5=' '
493900070827     c                   EXSR      LeggiAR5
494000070824     c                   endif
494100070827     c                   if        dcrpart<>'S'
494200070824     c                   z-sub     w98nri        w98nri
494300070824     c                   endif
494400070824     c                   endif
494500050907     c
494600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
494700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
494800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
494900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
495000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
495100050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
495200050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
495300050907     c                   z-sub     w98nri        w98nri
495400050907     c                   sub       oreILoDOPO    w98nri
495500050907     c                   endif
495600050907     c
495700050907     C                   END                                                    <-- 2 ---
495800930524     C                   END                                                    <-- 1 ---
495900930420     C* CALCOLO LE ORE DI RESA
496000930420     C* CONTROLLO CONFORMITA
496100930423     C     W98NRI        IFLE      *ZEROS
496200930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
496300930420     C                   ELSE
496400930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
496500930420     C                   END
496600930420     C*
496700930420     C*-------------------------------------------------------------------
496800930420     C* CONFORMITA' RESA CLIENTE
496900930420     C*-------------------------------------------------------------------
497000930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
497100930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
497200930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
497300050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
497400050914     c                   if        consideraora='S'
497500050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
497600050914     c                   else
497700050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
497800050914     c                   endif
497900090827     c
498000080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
498100090827     c                   select
498200090827     c                   when      d98cca='2' and w98nrc>0
498300080116     c                   z-add     0             w98nrc
498400090827
498500080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
498600080116     c*  e sto leggendo la bolla in arrivo
498700090827     c                   when      d98cca='1' and d98tbo='A' and
498800080116     c                                            w98nrc>0
498900080116     c                   z-add     0             w98nrc
499000110923     c
499100110923     c* Ignoro ritardo se Perfect Order
499200130221     c                   when      d98xco >*zeros
499300110923     c                   z-add     0             w98nrc
499400090827     c
499500090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
499600090827     c* sono  in ritardo di 12 ore
499700091009     c                   when      d98tcc=10  and
499800091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
499900091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
500000091014     c                             d98hce=1030
500100091014     C                   MOVEL     'OSC  '       WR3TAB
500200091014     C                   eval      WR3HHI=d98hmc
500300091014     C                   EXSR      RO3ORE
500400091014     c                   if        wr3matpom='M' and d98hmc>1030
500500131218     c                   exsr      ctrLAVH1030
500600131218
500700131218     c                   if        wlav1030<>'S'
500800091014     c                   add       12            w98nrc
500900091014     c                   endif
501000131218     c                   endif
501100091014     c
501200091014     c**                 z-add     12            w98nrc
501300090827     c                   endsl
501400090827     c
501500930524     C                   ELSE                                                   <-- 1 -->
501600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
501700930524     C* DI DIFFERENZA SONO UGUALI A ZERO
501800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
501900931206     C     WPODCM        ANDLE     WPOCR2
502000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
502100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
502200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
502300931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
502400930525     C                   EXSR      RO2COV
502500930525     C     WR2NOV        IFNE      *ZEROS
502600930525     C     WR2NOV        MULT      -1            W98NRC
502700930525     C                   END
502800930524     C                   ELSE                                                   <-- 2 -->
502900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
503000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
503100930524     C     WPODCM        IFLT      WPOCRI
503200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
503300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
503400930524     C                   ELSE
503500931206     C                   Z-ADD     WPOCR2        WPORFI
503600930524     C                   Z-ADD     WPODCM        WPORFF
503700930524     C                   END
503800930524     C* CALCOLO ORE DI DIFFERENZA
503900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
504000930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
504100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
504200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
504300930524     C                   EXSR      RO2COV
504400930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
504500050902     c
504600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
504700100224     c                   if        d98cca='2' and w98nrc>0
504800100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
504900080116     c***                z-sub     w98nrc        w98nrc
505000080116     c                   z-add     0             w98nrc
505100050902     c                   endif
505200111018     c* Ignoro ritardo se pefect order
505300130221     c                   if        d98xco >*zeros
505400111018     c                   z-add     0             w98nrc
505500111018     c                   endif
505600070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
505700061121     c*  e sto leggendo la bolla in arrivo
505800100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
505900100224     c***                if        d98cca='1' and d98tbo='A' and
506000100224     c***                                         w98nrc>0 and wpodcm<wpocri
506100080116     c***                z-sub     w98nrc        w98nrc
506200080116     c                   z-add     0             w98nrc
506300061121     c                   endif
506400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
506500070827     c*  mittente, --> consegnato in anticipo
506600070828     c                   if        d98tcr=' ' and d98nsp>0 and
506700070828     c                                            w98nrc>0 and wpodcm<wpocri
506800070827     c                   if        CallAR5=' '
506900070827     c                   EXSR      LeggiAR5
507000070827     c                   endif
507100070827     c                   if        dcrpart<>'S'
507200070824     c                   z-sub     w98nrc        w98nrc
507300070824     c                   endif
507400070824     c                   endif
507500050907     c
507600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
507700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
507800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
507900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
508000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
508100050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
508200050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
508300050907     c                   z-sub     w98nrc        w98nrc
508400050907     c                   sub       oreILoDOPO    w98nrc
508500050907     c                   endif
508600050907     c
508700050907     C                   END                                                    <-- 2 ---
508800930524     C                   END                                                    <-- 1 ---
508900930420     C* CALCOLO LE ORE DI RESA
509000930420     C* CONTROLLO CONFORMITA
509100930423     C     W98NRC        IFLE      *ZEROS
509200930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
509300930420     C                   ELSE
509400930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
509500930420     C                   END
509600930420     C*
509700930420     C                   ENDSR
509800070827     c
509900131218     c********************************************************************
510000131218     c     CtrlavH1030   BEGSR
510100131218     c********************************************************************
510200131218     c                   clear                   wlav1030          1
510300140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
510400131218     c                   z-add     1             xx                4 0
510500131218     c                   dow       evc(xx)<>*blanks
510600140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
510700140127     c                   if        evd(xx)=d98dcm
510800131218     c                   if        evo(xx)<=1030
510900131218     c                   eval      wlav1030='S'
511000131218     c                   leave
511100131218     c                   endif
511200131218     c                   endif
511300131218     c                   add       1             xx
511400131218     c                   enddo
511500131218     c                   ENDSR
511600070827     c********************************************************************
511700070827     c* Controllo record GEN di FIAR5
511800070827     c********************************************************************
511900070827     c     LeggiAR5      BEGSR
512000070827     c                   clear                   DCRPART           1
512100141104     c                   clear                   dar5gen
512200141104     c                   clear                   gc1PART           2
512300141104     c                   clear                   gc2PART           2
512400070827     c     kar5          chain     fiar501l
512500070827    1c                   if        not %found
512600070827     c                   eval      dcrpart='S'
512700141104     c                   eval      gc1part=d98gc1
512800141104     c                   eval      gc2part=d98gc2
512900070827   x1c                   else
513000070827     c* i dati devono essere quelli immessi dalla partenza
513100070827     c                   movel     ar5uni        dar5gen
513200070827     c
513300070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
513400070827     c*  ma il record GEN è stato scritto per altri motivi
513500070827    2c                   if        §ar5mdp='S'
513600070827     c                   movel     d98dcr        alfadcr           8
513700141104     c                   eval      gc1part=§ar5gc1
513800141104     c                   eval      gc2part=§ar5gc2
513900070827     c
514000070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
514100070827     c                   eval      dcrpart='S'
514200070827    3c                   endif
514300070827     c
514400070827   x2c                   else
514500070827     c                   eval      dcrpart='S'
514600141106     c                   eval      gc1part=d98gc1
514700141106     c                   eval      gc2part=d98gc2
514800070827    2c                   endif
514900070827    1c                   endif
515000070827     c
515100140402     c* Memorizzo il flag pickup 2
515200140402     c                   eval      wpick2=§AR5PK2
515300140402     c
515400150401     c*
515500150414     c**                 if        d98cas>0 or d98ffd<>' ' or
515600150414     c**                           d98tc1='A'  or d98tc2='A' or
515700150414     c**                           d98tc1='S'  or d98tc2='S'
515800160331     c                   eval      kar4trc='L'
515900150401     c     kar4          chain     fiar401l
516000160420    1c                   if        %found(fiar401l)
516100150401     c                   eval      dsbl4L=ar4not
516200160420   x1c                   else
516300160420    1c                   endif
516400160420
516500150401     c* Solo per fermo deposito verifico se inviato alert
516600150401     c                   if        d98ffd<>' '
516700150401     c                   clear                   fnlv82ds
516800150401     c                   eval      ILV82AAS=d98aas
516900150401     c                   eval      ILV82LNP=d98lnp
517000150401     c                   eval      ILV82NRS=d98nrs
517100150401     c                   eval      ILV82NSP=d98nsp
517200150401     c                   eval      ILV82TIPA='FD'
517300150401     c                   call      'FNLV82R'
517400150401     c                   parm                    fnlv82ds
517500170821     c* inoltre se stato '0' o '1', verifico se sono arrivati tutti i colli
517600170821     c*  alla lna finale
517700170821     c                   if        olv82sts= '0' or olv82sts='1'
517800170822     c                   clear                   fnlrj6ds
517900170821     c                   if        d98dam>0
518000170821     c                   eval      ilrj6tla ='L'
518100170822     c                   eval      ilrj6tric= 'A'
518200170821     c                   eval      ilrj6aas=d98aas
518300170821     c                   eval      ilrj6lnp=d98lnp
518400170821     c                   eval      ilrj6nrs=d98nrs
518500170821     c                   eval      ilrj6nsp=d98nsp
518600170821     c                   call      'FNLRJ6R'
518700170821     c                   parm                    fnlrj6ds
518800150401     c                   endif
518900170821     c                   endif
519000170821     c
519100170821     c                   endif
519200150414     c**                 endif
519300150401
519400150401     c                   eval      callar5='S'
519500160331     c
519600160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
519700160331     c                   if        %lookup(d98lnp:fildpd)>0
519800170925     c**                 eval      kar4trc='I'
519900170925     c**   kar4          chain     fiar401l
520000170925     c**                 if        %found(fiar401l)
520100170925     c**                 eval      dsbl4I=ar4not
520200170925     c**                 endif
520300170925     c**                 if        §b4trd='1' or §b4trd='2'
520400170925     c**                 eval      wVP='S'
520500170925     c**                 endif
520600170925
520700170925     c** nuovo file dati DPD
520800170925     c     kart          chain     fipnd01l
520900170925     c                   if        %found(fipnd01l)
521000170925     c                   eval      dpndflo=pndflo
521100170925     c                   if        §PNDTRD='1' or §PNDTRD='2'
521200170925     c                   eval      wVP='S'
521300170925     c                   endif
521400170925     c                   endif
521500160401     c
521600160331     c                   endif
521700150401
521800070827     c                   ENDSR
521900930913     C********************************************************************
522000930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
522100930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
522200930913     C*            DATA/ORA (FINE RANGE)
522300930913     C********************************************************************
522400930913     C     CALCNS        BEGSR
522500930914     C*
522600930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
522700930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
522800930914     C     WPODTI        IFEQ      *ZEROS
522900960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
523000930914     C                   ELSE
523100930914     C                   Z-ADD     WPODTI        WPOIRC
523200930914     C                   END
523300930914     C*
523400930914     C* SE NON ANCORA CONSEGNATA
523500930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
523600050922     c* in realtà nella data consegna c'e' la data fine calcolo
523700050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
523800930914     C     WPODCM        IFEQ      *ZEROS
523900050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
524000930914     C                   ELSE
524100930916     C                   Z-ADD     WPODCM        WPOFRC
524200930914     C                   END
524300930914     C*
524400930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
524500930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
524600930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
524700930914     C     WPODFC        ANDLT     WPOIRC
524800930914     C                   ELSE
524900930914     C*
525000930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
525100930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
525200930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
525300930914     C     WPODFC        ANDGT     WPOFRC
525400930914     C                   ELSE
525500930916     C*
525600930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
525700930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
525800930916     C     WPODFC        ANDGT     WPOFRC
525900930916     C                   ELSE
526000930916     C*
526100930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
526200930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
526300930916     C     WPODFC        ANDLT     WPOIRC
526400930916     C                   ELSE
526500930914     C*
526600930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
526700930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
526800930914     C     WPODIC        IFNE      *ZEROS
526900930914     C     WPODIC        ANDGT     WPOFRC
527000930914     C                   Z-ADD     WPOFRC        WPODIC
527100930914     C                   ENDIF
527200930914     C*
527300930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
527400930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
527500930914     C     WPODFC        IFGT      WPOFRC
527600930914     C                   Z-ADD     WPOFRC        WPODFC
527700930914     C                   ENDIF
527800930914     C*
527900930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
528000930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
528100930914     C     WPODIC        IFNE      *ZEROS
528200930914     C     WPODIC        ANDLT     WPOIRC
528300930914     C                   Z-ADD     WPOIRC        WPODIC
528400930914     C                   ENDIF
528500930914     C*
528600930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
528700930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
528800930914     C     WPODFC        IFLT      WPOIRC
528900930914     C                   Z-ADD     WPOIRC        WPODFC
529000930914     C                   ENDIF
529100930914     C*
529200930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
529300931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
529400930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
529500930916     C*
529600931210     C***********WPODIC    IFNE *ZEROS
529700931210     C***********WPODFC    ANDNE*ZEROS
529800931210     C*********************ADD  12        WPODFC
529900931210     C*********************ENDIF
530000930914     C     WPODIC        IFEQ      *ZEROS
530100930914     C     WPODFC        ANDNE     *ZEROS
530200931130     C                   Z-ADD     WPODFC        WPODIC
530300931210     C*********************ADD  12        WPODFC
530400930916     C                   ENDIF
530500930913     C*
530600930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
530700930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
530800930914     C* INTERNA
530900050908     c                   eval      Usacons='S'
531000930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
531100930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
531200931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
531300931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
531400940324     C                   EXSR      RO9COV
531500931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
531600930914     C* CLIENTE
531700930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
531800931210     C                   EXSR      RO9COV
531900931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
532000050908     c                   eval      Usacons=' '
532100930914     C*
532200930916     C                   ENDIF                                                  <-- 5 ---
532300930916     C                   ENDIF                                                  <-- 4 ---
532400930916     C                   ENDIF                                                  <-- 3 ---
532500930914     C                   ENDIF                                                  <-- 2 ---
532600930914     C                   ENDIF                                                  <-- 1 ---
532700930913     C*
532800930913     C                   ENDSR
532900930325     C********************************************************************
533000930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
533100930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
533200930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
533300930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
533400930325     C********************************************************************
533500930325     C     RO1CDP        BEGSR
533600930414     C*
533700931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
533800931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
533900931126     C*
534000931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
534100931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
534200150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
534300150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
534400931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
534500060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
534600930326     C*
534700930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
534800931130     C                   Z-ADD     WR1POI        WR1POO            9 0
534900930325     C*
535000930325     C* CICLO FINO A CHE:
535100930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
535200930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
535300930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
535400930406     C     WR1POO        ANDLT     WPOLIM
535500930414     C*
535600930407     C                   Z-ADD     WR1POO        WPOROU            9 0
535700930325     C                   MOVEL     'N'           WSLITT            1
535800930414     C*
535900930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
536000060420     C*
536100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
536200150605     c     wdee          andeq     ' '
536300930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
536400930414     C                   END
536500930511     C*
536600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
536700150608     c     wdee          andeq     ' '
536800930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
536900930511     C                   END
537000930414     C*
537100930325     C     WSLITT        IFEQ      'N'
537200930423     C                   SUB       12            WR1NOI            5 0
537300930325     C                   END
537400931130     C                   ADD       12            WR1POO            9 0
537500930414     C*
537600931223     C                   ENDDO                                                  <-- 1 ---
537700931126     C*
537800931126     C                   ELSE                                                   <-- 0 -->
537900931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
538000931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
538100931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
538200960129     C     WPOLAF        IFGT      WPODCG
538300960129     C                   Z-ADD     WPOLAF        WTM090            9 0
538400960129     C                   ELSE
538500960129     C                   Z-ADD     WPODCG        WTM090
538600960129     C                   ENDIF
538700960129     C     WPOGIF        IFGT      WTM090
538800960129     C                   Z-ADD     WPOGIF        WTM090
538900960129     C                   ENDIF
539000060420     C     WPOpaif       IFGT      WTM090
539100060420     C                   Z-ADD     WPOpaif       WTM090
539200060420     C                   ENDIF
539300960129     C*
539400960129     C     12            ADD       WTM090        WR1POO
539500960129     C                   ENDIF                                                  <-- 0 ---
539600930512     C*
539700931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
539800931202     C     WR1POO        SUB       12            WR7POI
539900931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
540000931202     C                   ADD       12            WR7POO
540100931202     C     WR7POO        IFGT      WR1POO
540200931202     C                   Z-ADD     WR7POO        WR1POO
540300931202     C                   END
540400930406     C*
540500930325     C                   ENDSR
540600930414     C********************************************************************
540700930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
540800930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
540900930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
541000930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
541100930414     C********************************************************************
541200930414     C     RO2COV        BEGSR
541300091014     c                   clear                   salvaEveL
541400091014     c                   clear                   salvaEveG
541500931130     C*
541600930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
541700930414     C                   Z-ADD     *ZEROS        WR2NOV
541800930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
541900931130     C                   Z-ADD     WR2POI        WR2POW
542000930414     C*
542100930414     C* CICLO FINO A CHE:
542200930414     C*   - DATA INIZIO SUPERATO DATA FINE
542300930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
542400931223     C     WR2POW        ANDLT     WPOLIM
542500930414     C*
542600930414     C                   Z-ADD     WR2POW        WPOROU
542700930414     C                   MOVEL     'N'           WSLITT            1
542800930414     C*
542900930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
543000930414     C*
543100930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
543200930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
543300930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
543400930414     C                   END
543500930414     C*
543600930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
543700930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
543800930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
543900930618     C                   END
544000060420     c
544100060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
544200060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
544300060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
544400060420     C                   END
544500060420     c
544600931201     C*
544700931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
544800931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
544900931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
545000931201     C                   END
545100060418     c
545200060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
545300060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
545400060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
545500060418     C                   END
545600930414     C*
545700930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
545800930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
545900930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
546000930414     C                   END
546100930511     C*
546200930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
546300930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
546400930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
546500930511     C                   END
546600930414     C*
546700930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
546800930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
546900930414     C     WSLITT        IFEQ      'N'
547000930423     C                   ADD       12            WR2NOV            5 0
547100930414     C                   END
547200931130     C                   ADD       12            WR2POW            9 0
547300930414     C*
547400931223     C                   ENDDO                                                  <-- 1 ---
547500930414     C*
547600930414     C                   ENDSR
547700941228     C********************************************************************
547800941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
547900941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
548000941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
548100941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
548200941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
548300941228     C********************************************************************
548400941228     C     RO2COP        BEGSR
548500941228     C*
548600941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
548700941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
548800941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
548900941228     C                   Z-ADD     *ZEROS        WR2NOV
549000941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
549100941228     C                   Z-ADD     WR2POI        WR2POW
549200941228     C*
549300941228     C* CICLO FINO A CHE:
549400941228     C*   - DATA INIZIO SUPERATO DATA FINE
549500941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
549600941228     C*
549700950215     C                   Z-ADD     WR2POW        WPOROU
549800941228     C                   MOVEL     'N'           WSLITT
549900941228     C* TRASFORMA IL PROGR. ORE IN DATA
550000950215     C*******              Z-ADDWR2POW    WORE
550100950215     C*******              EXSR OREDAT
550200950215     C*******              Z-ADDWDAT      W12DAT  80
550300950215     C*******              Z-ADDWRES      W12RES  30
550400941228     C*
550500941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
550600941228     C*
550700941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
550800941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
550900941228     C     WSLITT        IFEQ      'N'
551000941228     C                   ADD       12            WR2NOV            5 0
551100941228     C                   END
551200941228     C                   ADD       12            WR2POW            9 0
551300941228     C*
551400941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
551500941228     C* ENTRI IN LOOP
551600941228     C     WR2NOV        IFGT      999
551700941228     C                   LEAVE                                                  *VA A ENDDO
551800941228     C                   END
551900941228     C*
552000941228     C                   ENDDO                                                  <-- 1 ---
552100941228     C*
552200941228     C                   ENDSR
552300930510     C********************************************************************
552400930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
552500930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
552600930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
552700930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
552800050905     c*                     il num ore deve essere un multiplo di 12
552900050905     c*                     oppure 1 ora
553000930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
553100930510     C********************************************************************
553200930510     C     RO4CDP        BEGSR
553300930510     C*
553400930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
553500930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
553600050905     c                   movel     '1'           wgiro             1
553700930510     C*
553800930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
553900930510     C                   Z-ADD     WR4POI        WR4POO            9 0
554000930510     C*
554100930510     C* CICLO FINO A CHE:
554200930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
554300930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
554400930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
554500930510     C     WR4POO        ANDLT     WPOLIM
554600930510     C*
554700141106     c                   if        nosomma<>'N'
554800931130     C                   ADD       12            WR4POO            9 0
554900141106     c                   endif
555000930510     C                   Z-ADD     WR4POO        WPOROU            9 0
555100930510     C                   MOVEL     'N'           WSLITT            1
555200930510     C*
555300930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
555400050905     c
555500050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
555600930510     C     WSLITT        IFEQ      'N'
555700050905     c
555800050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
555900050905     c*  se cado in giorno non festivo
556000050905     c                   if        wr4noi=1 and wgiro='1'
556100050905     c                   sub       12            wr4poo
556200050905     c                   add       wr4noi        wr4poo
556300050905     c                   endif
556400050905     c*
556500050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
556600050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
556700050905     c*  calcolate dal pgm
556800050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
556900050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
557000050905     c*  del giorno sucessivo non festivo
557100050905     c                   if        wr4noi=1 and wgiro<>'1'
557200050905     c                   add       wr4noi        wr4poo
557300050905     c                   endif
557400050905     c
557500930510     C                   SUB       12            WR4NOI            5 0
557600930510     C                   END
557700141106     c
557800141106     c                   if        nosomma= 'N'
557900141106     C                   ADD       12            WR4POO            9 0
558000141106     c                   endif
558100930510     C*
558200050905     c                   eval      wgiro='2'
558300931223     C                   ENDDO                                                  <-- 1 ---
558400930510     C*
558500930510     C                   ENDSR
558600931130     C********************************************************************
558700931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
558800931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
558900931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
559000931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
559100931130     C********************************************************************
559200931130     C     RO5COV        BEGSR
559300931130     C*
559400931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
559500060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
559600060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
559700060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
559800931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
559900931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
560000931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
560100931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
560200931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
560300931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
560400931130     C                   Z-ADD     *ZEROS        WR5NOV
560500940714     C*
560600940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
560700940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
560800931207     C*
560900931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
561000940908     C     WIC           IFEQ      'I'
561100090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
561200090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
561300931207     C                   ELSE
561400090827     C*****              Z-ADD     D98TCC        WPOTCT
561500090827     C                   Z-ADD     w99TCC        WPOTCT
561600931207     C                   END
561700931130     C*
561800931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
561900060420     C                   clear                   COV
562000931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
562100931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
562200931207     C                   Z-ADD     2             CO                3 0
562300931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
562400931207     C                   EXSR      RO5SUB
562500981102     C                   DO        99            IC                3 0
562600931207     C     LAF(IC)       IFNE      *ZEROS
562700931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
562800931207     C                   EXSR      RO5SUB
562900931207     C                   ENDIF
563000931207     C                   ENDDO
563100060420     c
563200060420     C                   DO        99            IC                3 0
563300060420     C     paif(IC)      IFNE      *ZEROS
563400060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
563500060420     C                   EXSR      RO5SUB
563600060420     C                   ENDIF
563700060420     C                   ENDDO
563800931207     C* ORDINA LA SCHIERA COV
563900931207     C                   SORTA     COV
564000931130     C*
564100931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
564200931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
564300981102     C                   DO        98            IC                             --- 1 -->
564400931207     C     IC            ADD       1             I1C               3 0
564500931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
564600931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
564700931207     C     COV(I1C)      OREQ      *ZEROS
564800931207     C                   ELSE
564900931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
565000931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
565100931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
565200931130     C                   EXSR      RO2COV
565300931207     C                   ADD       WR2NOV        WR5NOV            9 0
565400931209     C* SE NON SONO IN DATA CONSEGNA
565500931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
565600931207     C     WR5NOV        IFLT      WPOTCT
565700931209     C     COV(I1C)      ANDNE     WPODCM
5658009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5659009709111009 C     WPOTCT        SUB       12            WR5NOV
566000931130     C                   END
566100931207     C                   ENDIF                                                  <-- 2 ---
566200931207     C                   ENDDO                                                  <-- 1 ---
566300940714     C                   ENDIF                                                  <-- 0 ---
566400931130     C*
566500931130     C                   ENDSR
566600931207     C********************************************************************
566700931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
566800931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
566900931207     C********************************************************************
567000931207     C     RO5SUB        BEGSR
567100931207     C*
567200931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
567300931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
567400931207     C     WR5COV        ANDLT     WR5POF
567500931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
567600931207     C                   MOVEL     'N'           WR5PRE            1
567700981102     C*---                 DO   30        I
567800981102     C                   DO        99            I
567900931207     C     WR5COV        IFEQ      COV(I)
568000931207     C                   MOVEL     'S'           WR5PRE
568100931207     C                   LEAVE
568200931207     C                   ENDIF
568300931207     C                   ENDDO
568400931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
568500931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
568600931207     C                   ADD       1             CO                3 0
568700931207     C                   Z-ADD     WR5COV        COV(CO)
568800931207     C                   ENDIF                                                  <-- 2 ---
568900931207     C                   ENDIF                                                  <-- 1 ---
569000931207     C*
569100931207     C                   ENDSR
569200931201     C********************************************************************
569300060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
569400931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
569500931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
569600931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
569700931201     C********************************************************************
569800931201     C     RO6CDT        BEGSR
569900931201     C*
570000931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
570100931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
570200931201     C*
570300931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
570400931201     C                   Z-ADD     WR6POI        WR6POO            9 0
570500931201     C*
570600931201     C* CICLO FINO A CHE:
570700931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
570800931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
570900931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
571000931201     C     WR6POO        ANDLT     WPOLIM
571100931201     C*
571200931201     C                   Z-ADD     WR6POO        WPOROU            9 0
571300931201     C                   MOVEL     'N'           WSLITT            1
571400931201     C*
571500931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
571600931201     C*
571700931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
571800150605     c     wdee          andeq     ' '
571900931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
572000931201     C                   END
572100931201     C*
572200931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
572300150608     c     wdee          andeq     ' '
572400931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
572500931201     C                   END
572600931201     C*
572700931201     C     WSLITT        IFEQ      'N'
572800931201     C                   SUB       12            WR6NOI            5 0
572900931201     C                   END
573000931201     C                   ADD       12            WR6POO            9 0
573100931201     C*
573200931223     C                   ENDDO                                                  <-- 1 ---
573300931201     C*
573400931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
573500931202     C     WR6POO        SUB       12            WR7POI
573600931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
573700931207     C                   ADD       12            WR7POO
573800931202     C     WR7POO        IFGT      WR6POO
573900931201     C                   Z-ADD     WR7POO        WR6POO
574000931202     C                   END
574100931201     C*
574200931201     C                   ENDSR
574300931201     C********************************************************************
574400060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
574500060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
574600060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
574700060407     C*            INPUT:  -WR7POI (PROGR.ORE)
574800931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
574900931201     C********************************************************************
575000931201     C     RO7TCF        BEGSR
575100060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
575200060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
575300060407     C                   MOVEL     'N'           WSLITT
575400931201     C*
575500931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
575600931201     C                   Z-ADD     WR7POI        WR7POO            9 0
575700931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
575800931201     C* COINCIDA CON UN TURNO DI CHIUSURA
575900060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
576000931201     C     D98GC2        ANDEQ     *BLANKS
576100060407   x1C                   ELSE                                                   <-- 1 -->
576200931214     C*
576300931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
576400931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
576500931214     C*
576600931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
576700931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
576800931223     C     WPOROU        ANDLT     WPOLIM
576900931214     C* MEMORIZZO INIZIO E FINE RANGE
577000931202     C     WPOTI         IFEQ      *ZEROS
577100931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
577200931202     C                   END
577300931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
577400931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
577500931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
577600931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
577700931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
577800931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
577900931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
578000931201     C                   MOVEL     'N'           WSLITT
578100931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
578200060407     c* Se non trovat, controllo se c'e' evento part che giustifica
578300060407     c                   if        wslitt='N'
578400060407     c                   exsr      datcepg
578500060407     c                   endif
578600060407     c
578700931201     C                   ENDDO                                                  <-- 2 ---
578800931214     C*
578900931201     C                   Z-ADD     WPOROU        WR7POO
579000931214     C*
579100931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
579200931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
579300931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
579400931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
579500931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
579600931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
579700931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
579800931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
579900931209     C                   ELSE                                                   <-- 3 -->
580000931209     C     WIC           IFEQ      'I'
580100931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
580200931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
580300931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
580400931201     C                   ELSE
580500931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
580600931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
580700931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
580800931201     C                   ENDIF
580900931209     C                   ENDIF                                                  <-- 3 ---
581000931209     C                   ENDIF                                                  <-- 2 ---
581100931214     C*
581200060407    1C                   ENDIF                                                  <-- 1 ---
581300060407     c
581400060407     c* Se non trovato turno di chiusura, verifico se c'e'
581500060407     c*  evento particolare che GIUSTIFICA
581600060407     c                   if        wpoti=0
581700060418     C                   EXSR      RO7CEPG
581800060407     c                   endif
581900931201     C*
582000931201     C                   ENDSR
582100060406     C********************************************************************
582200060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
582300060406     c*                        che giustifica
582400060407     C*            INPUT:-WR7POI (PROGR.ORE)
582500060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
582600060406     C********************************************************************
582700060407     C     RO7CEPG       BEGSR
582800060406     C*
582900060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
583000060407     C                   Z-ADD     WR7POI        WR7POO            9 0
583100060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
583200060406     C* coincide
583300060406    1C                   if        wevpag='S'                                   <-- 1 -->
583400060406     C*
583500060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
583600060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
583700060406     C                   MOVEL     'N'           WSLITT
583800060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
583900060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
584000060406     C*
584100060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
584200060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
584300060406     C     WPOROU        ANDLT     WPOLIM
584400060406     C* MEMORIZZO INIZIO E FINE RANGE
584500060406    3C     WPOTI         IFEQ      *ZEROS
584600060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
584700060406    3C                   END
584800060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
584900060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
585000060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
585100060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
585200060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
585300060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
585400060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
585500060406     C                   MOVEL     'N'           WSLITT
585600060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
585700060406     C* Se non trovato,
585800060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
585900060406    3c                   if        wslitt='N'
586000060406     c                   exsr      DATTUC
586100060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
586200060406    2C                   ENDDO                                                  <-- 2 ---
586300060406     C*
586400060407     C                   Z-ADD     WPOROU        WR7POO
586500060406     C*
586600060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
586700060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
586800060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
586900060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
587000060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
587100060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
587200060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
587300060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
587400060406   x3C                   ELSE                                                   <-- 3 -->
587500060406    4C     WIC           IFEQ      'I'
587600060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
587700060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
587800060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
587900060406   x4C                   ELSE
588000060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
588100060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
588200060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
588300060406    4C                   ENDIF
588400060406    3C                   ENDIF                                                  <-- 3 ---
588500060406    2C                   ENDIF                                                  <-- 2 ---
588600060406     C*
588700060406    1C                   ENDIF                                                  <-- 1 ---
588800060406     C*
588900060406     C                   ENDSR
589000931207     C********************************************************************
589100931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
589200931207     C*            INPUT:  -WR8POI (PROGR.ORE)
589300931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
589400931207     C********************************************************************
589500931207     C     RO8FES        BEGSR
589600130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
589700130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
589800931207     C*
589900931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
590000130411     C     WR8POI        SUB       12            WR8POO            9 0
590100931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
590200931207     C                   MOVEL     'N'           WSLITT
590300130411     c
590400130411     c* Non salto i festivi ???
590500130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
590600130411     c
590700931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
590800130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
590900130411     C     WPOROU        ANDGT     WPODSP
591000130411     C                   SUB       12            WPOROU
591100130411     C                   MOVEL     'N'           WSLITT
591200130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
591300130411     C                   ENDDO                                                  <-- 2 ---
591400130411     c
591500130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
591600130411     C     D98GC2        ANDEQ     *BLANKS
591700130411   x1C                   ELSE                                                   <-- 1 -->
591800130411     c* eseguo controllo per i turni di chiusura
591900130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
592000130411     C*
592100130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
592200130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
592300130411     C     WPOROU        ANDgT     WPOdsp
592400130411     C* MEMORIZZO INIZIO E FINE RANGE
592500130411     C     WPOTf         IFEQ      *ZEROS
592600130411     C                   z-add     wporou        WPOTf
592700130411     C                   END
592800140826     C     WPOROU        SUB       wnoadd        WPOROU
592900130411     c
593000130411     C                   MOVEL     'N'           WSLITT
593100130411     c                   exsr      dattuc
593200130411     c                   if        wslitt='N'
593300130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
593400140826     c* Se slitta per festivo --> tolgo sempre 12 ore
593500140826     c                   z-add     12            WNOADD
593600130411     c                   endif
593700130411     C* SLITTA
593800140826     C     wporou        add       12            WPOTi
593900130411    2C                   ENDDO                                                  <-- 2 ---
594000130411     c
594100130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
594200130411     c
594300130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
594400130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
594500130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
594600130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
594700130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
594800130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
594900130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
595000130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
595100130411   x3C                   ELSE                                                   <-- 3 -->
595200130411    4C     WIC           IFEQ      'I'
595300130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
595400130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
595500130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
595600130411   x4C                   ELSE
595700130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
595800130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
595900130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
596000130411    4C                   ENDIF
596100130411    3C                   ENDIF                                                  <-- 3 ---
596200130411    2C                   ENDIF                                                  <-- 3 ---
596300130411     C*
596400130411    1C                   ENDIF                                                  <-- 1 ---
596500130411
596600130411     C**   WPOROU        ADD       12            WR8POO
596700130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
596800130411     c                   z-add     wr8poi        wr8poo
596900931207     C*
597000931207     C                   ENDSR
597100931210     C********************************************************************
597200931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
597300940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
597400931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
597500931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
597600931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
597700931210     C********************************************************************
597800931210     C     RO9COV        BEGSR
597900931210     C*
598000931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
598100931210     C                   Z-ADD     *ZEROS        WR9NOV
598200931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
598300931210     C                   Z-ADD     WR9POI        WR9POW
598400931210     C* CICLO FINO A CHE:
598500931210     C*   - DATA INIZIO SUPERATO DATA FINE
598600931223     C*   - RAGGIUNTO LIMITE MASSIMO
598700931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
598800931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
598900931223     C     WR9POW        ANDLT     WPOLIM
599000931210     C*
599100931210     C                   Z-ADD     WR9POW        WPOROU
599200931210     C                   MOVEL     'N'           WSLITT            1
599300940324     C* FESTIVI
599400931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
599500940324     C* GIACENZA
599600931214     C     WSLITT        IFEQ      'N'
599700941229£$   C     WPOAGC        IFNE      *ZEROS
599800940324£$   C     WPOCGC        ORNE      *ZEROS
599900941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
600000940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
600100940324£$   C                   MOVEL     'S'           WSLITT
600200940324£$   C                   END
600300940324£$   C                   END
600400940324£$   C                   END
600500960129     C* EVENTI GIACENZA
600600960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
600700960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
600800960129     C                   Z-ADD     *ZEROS        G                 3 0
600900960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
601000960129     C     G             ANDLT     IG
601100960129     C                   ADD       1             G
601200960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
601300960209     C     WPOROU        ANDLE     GCF(G)
601400960129     C                   MOVEL     'S'           WSLITT
601500960129     C                   ENDIF                                                  <-- 3 ---
601600960129     C                   ENDDO                                                  <-- 2 ---
601700960129     C                   ENDIF                                                  <-- 1 ---
601800960129     C                   ENDIF                                                  <-- 0 ---
601900940324     C* LASCIATO AVVISO
602000931214     C     WSLITT        IFEQ      'N'
602100941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
602200940324£$   C                   Z-ADD     *ZEROS        L                 3 0
602300940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
602400940324£$   C     L             ANDLT     IL
602500940324£$   C                   ADD       1             L
602600941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
602700941229£$   C     WPOROU        ANDLE     LCF(L)
602800050520     c     LCon(L)       andeq     'S'
602900940324£$   C                   MOVEL     'S'           WSLITT
603000940324£$   C                   END                                                    <-- 3 ---
603100940324£$   C                   ENDDO                                                  <-- 2 ---
603200940324£$   C                   END                                                    <-- 1 ---
603300931210     C                   END
603400940324     C* TURNO CHIUSURA
603500140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
603600140826     c*  di sistemare le ore da sommare 12 o 24...
603700931214     C     WSLITT        IFEQ      'N'
603800931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
603900931210     C                   END
604000940324     C* DATA CONSEGNA RICHIESTA
604100050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
604200050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
604300050907     c
604400931214     C     WSLITT        IFEQ      'N'
604500050907     c                   add       1             wporou
604600940324£$   C     WPOCRI        IFNE      *ZEROS
604700940324£$   C     WPOROU        IFLT      WPOCRC
604800940324£$   C                   MOVEL     'S'           WSLITT
604900940324£$   C                   END
605000940324£4   C                   END
605100050907     c                   sub       1             wporou
605200931210     C                   END
605300931210     C*
605400931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
605500931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
605600931210     C     WSLITT        IFEQ      'N'
605700931210     C                   ADD       12            WR9NOV            5 0
605800931210     C                   END
605900931210     C                   ADD       12            WR9POW            9 0
606000931210     C*
606100931223     C                   ENDDO                                                  <-- 1 ---
606200931210     C*
606300931210     C                   ENDSR
606400940628%%%  C********************************************************************
606500940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
606600940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
606700940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
606800940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
606900940628%%%  C********************************************************************
607000940628%%%  C     RO0GES        BEGSR
607100940628%%%  C*
607200940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
607300940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
607400940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
607500940628%%%  C*
607600940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
607700940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
607800940628%%%  C*
607900940628%%%  C* CICLO FINO A CHE:
608000940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
608100940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
608200940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
608300050922%%%  C     WR0POO        ANDGe     WPODSP
608400940628%%%  C*
608500940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
608600940628%%%  C                   MOVEL     'N'           WSLITT            1
608700940628%%%  C*
608800940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
608900940628%%%  C*
609000940628%%%  C     WSLITT        IFEQ      'N'
609100940628%%%  C                   SUB       12            WR0NOI            5 0
609200940628%%%  C                   END
609300940628%%%  C                   SUB       12            WR0POO            9 0
609400940628%%%  C*
609500940628%%%  C                   ENDDO                                                  <-- 1 ---
609600940628%%%  C*
609700940628%%%  C                   ADD       12            WR0POO            9 0
609800940628%%%  C*
609900940628%%%  C                   ENDSR
610000960911     C********************************************************************
610100960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
610200960911     C*            INPUT:  -W10POI (PROGR.ORE)
610300960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
610400960911     C********************************************************************
610500960911     C     R10FES        BEGSR
610600960911     C*
610700960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
610800960911     C                   Z-ADD     W10POI        W10POO            9 0
610900960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
611000960911     C                   MOVEL     'N'           WSLITT
611100960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
611200960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
611300960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
611400960911     C                   ADD       12            WPOROU
611500960911     C                   MOVEL     'N'           WSLITT
611600960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
611700960911     C                   ENDDO                                                  <-- 2 ---
611800960911     C                   Z-ADD     WPOROU        W10POO
611900960911     C*
612000960911     C                   ENDSR
612100930325     C********************************************************************
612200930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
612300930325     C********************************************************************
612400930325     C     CHKGIA        BEGSR
612500930406     C*
612600960129     C* APERTURA/CHIUSURA GIACENZA
612700930326     C     WPODAG        IFNE      *ZEROS
612800930406     C     WPODCG        ORNE      *ZEROS
612900930407     C     WPOROU        IFGE      WPODAG
613000930407     C     WPOROU        ANDLE     WPODCG
613100930325     C                   MOVEL     'S'           WSLITT
613200930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
613300930325     C                   END
613400930326     C                   END
613500960129     C*
613600960129     C* EVENTI GIACENZA
613700960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
613800960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
613900960129     C                   Z-ADD     *ZEROS        G                 3 0
614000960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
614100960129     C     G             ANDLT     IG
614200960129     C                   ADD       1             G
614300960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
614400960129     C     WPOROU        ANDLE     GIF(G)
614500091009     c
614600091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
614700091009     c*  l'orario dell'evento
614800091009     c                   select
614900091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
615000091009     c* Se orario entro le 10.30  va bene
615100091014     c*  solo la prima volta per evento cioè quando controllo
615200091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
615300091014     c*  e non devo dare errore
615400091014     C                   MOVEL     'OSC  '       WR3TAB
615500091014     C                   eval      WR3HHI=evo(gip(G))
615600091014     C                   EXSR      RO3ORE
615700091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
615800091014     c*  in ritardo come per "E")
615900091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
616000091014     c                              wr3matpom='P'
616100091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
616200091009     C                   MOVEL     'S'           WSLITT
616300091009     c                   endif
616400091014     c                   eval      SalvaEveG=gip(G)
616500091009     c
616600091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
616700091009     c* Se orario entro le 10.30  va bene
616800091014     c*  solo la prima volta per evento cioè quando controllo
616900091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
617000091014     c*  e non devo dare errore
617100091014     C                   MOVEL     'OSC  '       WR3TAB
617200091014     C                   eval      WR3HHI=evo(gip(G))
617300091014     C                   EXSR      RO3ORE
617400091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
617500091014     c*  in ritardo come per "E")
617600091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
617700091014     c                              wr3matpom='P'
617800091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
617900091009     C                   MOVEL     'S'           WSLITT
618000091009     c                   endif
618100091014     c                   eval      SalvaEveG=gip(G)
618200091009     c
618300091009     c                   other
618400960129     C                   MOVEL     'S'           WSLITT
618500960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
618600091009     c                   endsl
618700091009
618800960129     C                   ENDIF                                                  <-- 3 ---
618900960129     C                   ENDDO                                                  <-- 2 ---
619000960129     C                   ENDIF                                                  <-- 1 ---
619100960129     C                   ENDIF                                                  <-- 0 ---
619200930406     C*
619300930325     C                   ENDSR
619400060420     C********************************************************************
619500930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
619600930325     C********************************************************************
619700930325     C     CHKLAV        BEGSR
619800930325     C*
619900941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
620000930421     C                   Z-ADD     *ZEROS        L                 3 0
620100930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
620200930421     C     L             ANDLT     IL
620300930421     C                   ADD       1             L
620400930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
620500930421     C     WPOROU        ANDLE     LAF(L)
620600090928     c
620700090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
620800091009     c*  l'orario dell'evento
620900091009     c                   select
621000091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
621100091009     c* Se orario entro le 10.30  va bene
621200091014     c*  solo la prima volta per evento cioè quando controllo
621300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
621400091014     c*  e non devo dare errore
621500091014     C                   MOVEL     'OSC  '       WR3TAB
621600091014     C                   eval      WR3HHI=evo(lap(L))
621700091014     C                   EXSR      RO3ORE
621800091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
621900091014     c*  in ritardo come per "E")
622000091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
622100091014     c                              wr3matpom='P'
622200091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
622300091009     C                   MOVEL     'S'           WSLITT
622400091009     c                   endif
622500091014     c                   eval      SalvaEveL=lap(L)
622600091009     c
622700091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
622800091009     c* Se orario entro le 10.30  va bene
622900091014     c*  solo la prima volta per evento cioè quando controllo
623000091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
623100091014     c*  e non devo dare errore
623200091014     C                   MOVEL     'OSC  '       WR3TAB
623300091014     C                   eval      WR3HHI=evo(lap(L))
623400091014     C                   EXSR      RO3ORE
623500091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
623600091014     c*  in ritardo come per "E")
623700091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
623800091014     c                              wr3matpom='P'
623900091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
624000091009     C                   MOVEL     'S'           WSLITT
624100091009     c                   endif
624200091014     c                   eval      SalvaEveL=lap(L)
624300090928     c
624400091009     c                   other
624500930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
624600091009     C                   MOVEL     'S'           WSLITT
624700091009     c                   endsl
624800090928     c
624900930421     C                   END                                                    <-- 3 ---
625000931223     C                   ENDDO                                                  <-- 2 ---
625100930421     C                   END                                                    <-- 1 ---
625200930325     C*
625300930325     C                   ENDSR
625400060420     C********************************************************************
625500060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
625600060420     c*                   che Allungano
625700060420     C********************************************************************
625800060420     C     CHKCEPA       BEGSR
625900060420     C*
626000060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
626100060420     C                   Z-ADD     *ZEROS        p                 3 0
626200060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
626300060420     C     p             ANDLT     IpA
626400060420     C                   ADD       1             P
626500060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
626600060420     C     WPOROU        ANDLE     paif(P)
626700060420     C                   MOVEL     'S'           WSLITT
626800060420     C                   ADD       12            W98EA
626900060420     C                   END                                                    <-- 3 ---
627000060420     C                   ENDDO                                                  <-- 2 ---
627100060420     C                   END                                                    <-- 1 ---
627200060420     C*
627300060420     C                   ENDSR
627400930414     C********************************************************************
627500930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
627600930414     C********************************************************************
627700930414     C     CHKCNT        BEGSR
627800930511     C*
627900930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
628000930611     C     WIC           IFEQ      'I'                                          *INTERNO
628100930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
628200930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
628300930611     C                   ELSE
628400930611     C     WPOROU        IFGE      WPOAII
628500930611     C     WPOROU        ANDLE     WPOAFI
628600930611     C                   MOVEL     'S'           WSLITT
628700930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
628800930611     C                   END
628900930611     C                   END
629000930611     C                   ELSE                                                   *CLIENTE
629100930611     C     WPOAIC        IFEQ      *ZEROS
629200930611     C     WPOAFC        ANDEQ     *ZEROS
629300930611     C                   ELSE
629400930611     C     WPOROU        IFGE      WPOAIC
629500930611     C     WPOROU        ANDLE     WPOAFC
629600930611     C                   MOVEL     'S'           WSLITT
629700930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
629800930611     C                   END
629900930611     C                   END
630000930611     C                   END
630100930414     C*
630200930414     C                   ENDSR
630300930511     C********************************************************************
630400930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
630500930511     C********************************************************************
630600930511     C     CHKFED        BEGSR
630700930511     C*
630800930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
630900930611     C     WIC           IFEQ      'I'                                          *INTERNO
631000930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
631100930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
631200930611     C                   ELSE
631300930611     C     WPOROU        IFGE      WPOFII
631400930611     C     WPOROU        ANDLE     WPOFFI
631500930611     C                   MOVEL     'S'           WSLITT
631600930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
631700930611     C                   END
631800930611     C                   END
631900930611     C                   ELSE                                                   *CLIENTE
632000930611     C     WPOFIC        IFEQ      *ZEROS
632100930611     C     WPOFFC        ANDEQ     *ZEROS
632200930611     C                   ELSE
632300930611     C     WPOROU        IFGE      WPOFIC
632400930611     C     WPOROU        ANDLE     WPOFFC
632500930611     C                   MOVEL     'S'           WSLITT
632600930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
632700930611     C                   END
632800930611     C                   END
632900930611     C                   END
633000930511     C*
633100930511     C                   ENDSR
633200930325     C********************************************************************
633300930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
633400930325     C********************************************************************
633500930325     C     CHKFES        BEGSR
633600060526     c* se già passata la data, non controllo wporou
633700060526     c                   if        wpodata>0
633800060526     c                   z-add     wpodata       w12dat
633900060526     c                   clear                   w12res
634000060526     c                   else
634100931130     C*
634200931130     C* TRASFORMA IL  PROGR.ORE IN DATA
634300931130     C                   Z-ADD     WPOROU        WORE
634400931130     C                   EXSR      OREDAT
634500941227     C                   Z-ADD     WDAT          W12DAT            8 0
634600931130     C                   Z-ADD     WRES          W12RES            3 0
634700060526     c                   endif
634800930325     C*
634900941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
635000930325     C* E' FESTIVO
635100170117     c* l'affidabilità partenza ora fa un doppio tentativo
635200170117    1C     WTC           IFEQ      'P'                                          *PARTENZA
635300170117     C                   Z-ADD     D98LNP        CLNTFP
635400170117     C                   Z-ADD     *ZEROS        CLNTFA
635500170117     c                   exsr      azcln_fes
635600170117     c* se non è festivo guardo anche la trazione
635700170117     c                   if        wslitt<>'S'
635800170117     C                   Z-ADD     D98TFP        CLNTFP
635900170117     C                   Z-ADD     D98TFA        CLNTFA
636000170117     c                   exsr      azcln_fes
636100170117     C                   ENDIF
636200170117     c
636300170117   x1C                   Else
636400170117     c
636500090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
636600090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
636700090108     c**   è possibile rendere festivo in terminal per un altro terminal
636800090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
636900170117     c
637000170117     c** ES 17/01/17 - Faccio doppio tentativo: prima con la filiale e poi con
637100170117     c**               la trazione , se filiale non festiva
637200930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
637300170117     C***  WTC           oreq      'P'                                          *TRAZIONE
637400941227     C                   Z-ADD     D98TFP        CLNTFP
637500941227     C                   Z-ADD     D98TFA        CLNTFA
637600941228     C                   ENDIF
637700941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
637800941227     C                   Z-ADD     *ZEROS        CLNTFP
637900941227     C                   Z-ADD     D98LNA        CLNTFA
638000941228     C                   ENDIF
638100170117     c
638200170117     c                   exsr      azcln_fes
638300170117    1c                   endif
638400930325     C*
638500060526     c                   clear                   wpodata
638600930325     C                   ENDSR
638700170117
638800170117     C********************************************************************
638900170117     c* Verifica se giorno festivo su calendario
639000170117     C********************************************************************
639100170117     c     Azcln_fes     BEGSR
639200170117     C                   Z-ADD     W12DAT        DSDAT8
639300170117     C                   Z-ADD     DSAAS         CLNANN
639400170117     C                   Z-ADD     DSMES         CLNMES
639500170117     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
639600170117     C     D98ERR        IFEQ      '0'                                          --- 1 -->
639700170117     C                   Z-ADD     DSGIO         G
639800170117     C     W12RES        IFEQ      *ZEROS
639900170117     C                   MOVEL     CLNMAT        GFEDS
640000170117     C                   ELSE
640100170117     C                   MOVEL     CLNPOM        GFEDS
640200170117     C                   END
640300170117     c
640400170117     c* se sto verificando i tempi partenza o trazione e
640500170117     c*  la data che sto controllando corrisponde alla data spedizione,
640600170117     c*  la considero sempre lavorativa
640700170117     c                   if        (wtc='P' or wtc='T') and w12dat=d98dfv
640800170117     c                   else
640900170117     c
641000170117     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
641100170117     C                   MOVEL     *BLANKS       WC1Y
641200170117     C                   MOVEL     GFE(G)        WC1Y              8
641300170117     C                   Z-ADD     1             I
641400170117     C     WC1Y          LOOKUP    C1Y(I)                                 99
641500170117     C     *IN99         IFEQ      '1'                                          --- 3 -->
641600170117     C     WIC           IFEQ      'I'
641700170117     C     I1Y(I)        ANDEQ     'S'
641800170117     C                   MOVEL     'S'           WSLITT
641900170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
642000170117     C                   END
642100170117     C     WIC           IFEQ      'C'
642200170117     C     E1Y(I)        ANDEQ     'S'
642300170117     C                   MOVEL     'S'           WSLITT
642400170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
642500170117     C                   END
642600170117     C     WIC           IFEQ      'X'
642700170117     C     E1Y(I)        ANDEQ     'S'
642800170117     C                   MOVEL     'S'           WSLITT
642900170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
643000170117     C                   END
643100170117     C                   ELSE                                                   <-- 3 -->
643200170117     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
643300170117     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
643400170117     C                   END                                                    <-- 3 ---
643500170117     C                   END                                                    <-- 2 ---
643600170117     C                   END                                                    <-- 1 ---
643700170117     c
643800170117     c                   endif
643900170117     c                   ENDSR
644000931201     C********************************************************************
644100931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
644200931201     C********************************************************************
644300931201     C     CHKTUC        BEGSR
644400931201     C*
644500931201     C     WIC           IFEQ      'I'                                          --- 1 -->
644600931201     C                   DO        TI            T                 3 0          --- 2 -->
644700931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
644800931201     C     WPOROU        ANDLE     TIF(T)
644900931201     C                   MOVEL     'S'           WSLITT
645000931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
645100931201     C                   LEAVE                                                  *VA A FINE DO
645200931201     C                   ENDIF                                                  <-- 3 ---
645300931201     C                   ENDDO                                                  <-- 2 ---
645400931201     C                   ELSE                                                   <-- 1 -->
645500931201     C                   DO        TC            T                 3 0          --- 2 -->
645600931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
645700931201     C     WPOROU        ANDLE     TCF(T)
645800931201     C                   MOVEL     'S'           WSLITT
645900931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
646000931201     C                   LEAVE                                                  *VA A FINE DO
646100931201     C                   ENDIF                                                  <-- 3 ---
646200931201     C                   ENDDO                                                  <-- 2 ---
646300931201     C                   ENDIF                                                  <-- 1 ---
646400931201     C*
646500931201     C                   ENDSR
646600060418     C********************************************************************
646700060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
646800060418     c*          - eventi particolari che giustificano
646900060418     C********************************************************************
647000060418     C     CHKCEPG       BEGSR
647100060418     C*
647200060418     C     WIC           IFEQ      'I'                                          --- 1 -->
647300060420     C                   DO        PGI           p                 3 0          --- 2 -->
647400060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
647500060420     C     WPOROU        ANDLE     PGIF(p)
647600060418     C                   MOVEL     'S'           WSLITT
647700060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
647800060418     C                   LEAVE                                                  *VA A FINE DO
647900060418     C                   ENDIF                                                  <-- 3 ---
648000060418     C                   ENDDO                                                  <-- 2 ---
648100060418     C                   ELSE                                                   <-- 1 -->
648200060420     C                   DO        PGC           p                 3 0          --- 2 -->
648300060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
648400060420     C     WPOROU        ANDLE     PGCF(p)
648500060418     C                   MOVEL     'S'           WSLITT
648600060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
648700060418     C                   LEAVE                                                  *VA A FINE DO
648800060418     C                   ENDIF                                                  <-- 3 ---
648900060418     C                   ENDDO                                                  <-- 2 ---
649000060418     C                   ENDIF                                                  <-- 1 ---
649100060418     C*
649200060418     C                   ENDSR
649300930512     C********************************************************************
649400931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
649500930512     C********************************************************************
649600931130     C     DATTUC        BEGSR
649700931214     C*
649800931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
649900931214     C                   Z-ADD     12            WNOADD            9 0
650000930325     C*
650100931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
650200931125     C     D98GC2        ORNE      *BLANKS
650300931130     C*
650400931213     C                   Z-ADD     WPOROU        WORE
650500931130     C                   EXSR      OREDAT                                       *ORE->DATA
650600931130     C                   Z-ADD     WDAT          W12DAT
650700931130     C                   Z-ADD     WRES          W12RES
650800930405     C*
650900941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
651000930405     C* DELLA SETTIMANA
651100941227     C                   Z-ADD     W12DAT        DSDAT8
651200941227     C                   Z-ADD     DSAAS         CLNANN
651300941227     C                   Z-ADD     DSMES         CLNMES
651400941227     C                   Z-ADD     *ZEROS        CLNTFP
651500941227     C                   Z-ADD     *ZEROS        CLNTFA
651600930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
651700930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
651800941227     C                   MOVEL     CLNMAT        GSEDS
651900960129     C                   Z-ADD     DSGIO         GI                6 0
652000960129     C                   MOVEL     GSE(GI)       WGSE              1
652100140911     c
652200140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
652300140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
652400140911     c                   if        wtuctut='S'
652500140911     c                   eval      wtuctut='P'
652600140911     c                   endif
652700930326     C*
652800931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
652900931213     C                   DO        4             IT                3 0          --- 3 -->
653000931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
653100931213     C                   MOVEL     TUC(IT)       W93GSE            1
653200931213     C                   MOVE      TUC(IT)       W93GMP            1
653300930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
653400930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
653500931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
653600930512     C     W93GSE        OREQ      ' '
653700140911     c     WTUCTUT       andeq     ' '
653800140911     c
653900930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
654000931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
654100930401     C     W93GMP        OREQ      ' '
654200930325     C                   MOVEL     'S'           WSLITT
654300931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
654400931213     C                   ELSE                                                   <-- 6 -->
654500001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
654600001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
654700001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
654800050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
654900050908     C     Usacons       IFNE      'S'                                          --- 7 -->
655000141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
655100141107     c*    orari giorno per giorno
655200141107     c                   if        OOR2FCSGL<>' '
655300141108     c                             and w12dat<>sav_12dat
655400141107     c                   eval      or_datarif=w12dat
655500141107     c                   eval      or_DDC    =w12dat
655600141107     c                   exsr      Reper_tempi
655700141108     c                   eval      sav_12dat=w12dat
655800141107     c                   endif
655900141107     c
656000930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
656100930325     C     W93GMP        IFEQ      'M'
656200930326     C     W12RES        ANDEQ     *ZEROS
656300141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
656400141111     c                             or OOR2STfS =0
656500141105     c                             or (tuc(IT)<>gc1part and
656600170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
656700170207     c
656800140825     C                   MOVEL     'S'           WSLITT
656900140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
657000140911     c* slitto solo una volta  se chiuso tutti " M"
657100140911     c                   if        W93GSE=' '
657200140911     c                   eval      wTUCTUT='S'
657300140911     c                   endif
657400140911     c
657500140825     c                   endif
657600930325     C                   END
657700930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
657800930325     C     W93GMP        IFEQ      'P'
657900930326     C     W12RES        ANDNE     *ZEROS
658000140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
658100140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
658200141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
658300141111     c                             or OOR2STiS =0
658400141105     c                             or (tuc(IT)<>gc1part and
658500170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
658600930325     C                   MOVEL     'S'           WSLITT
658700140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
658800140911     c* slitto solo una volta  se chiuso tutti " P"
658900140911     c                   if        W93GSE=' '
659000140911     c                   eval      wTUCTUT='S'
659100140911     c                   endif
659200140825     c                   endif
659300140825     c
659400931213     C                   ENDIF
659500001128     C                   ENDIF                                                  <-- 7 ---
659600931213     C                   ENDIF                                                  <-- 6 ---
659700931213     C                   ENDIF                                                  <-- 5 ---
659800931213     C                   ENDIF                                                  <-- 4 ---
659900931213     C                   ENDDO                                                  <-- 3 ---
660000930405     C*
660100931213     C                   ENDIF                                                  <-- 2 ---
660200931213     C                   ENDIF                                                  <-- 1 ---
660300140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
660400140911     c                   if        wtuctut='P'  and wslitt='N'
660500140911     c                   clear                   wtuctut
660600140911     c                   endif
660700140911     c
660800930325     C                   ENDSR
660900060406     C********************************************************************
661000060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
661100060406     C********************************************************************
661200060406     C     DATCEPG       BEGSR
661300060406     C*
661400060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
661500060406     C                   Z-ADD     12            WNOADD            9 0
661600060406     C*
661700060406    1C                   if        wevpag='S'
661800060406     C*
661900060406     C                   Z-ADD     WPOROU        WORE
662000060406     C                   EXSR      OREDAT                                       *ORE->DATA
662100060406     C                   Z-ADD     WDAT          W12DAT
662200060406     C                   Z-ADD     WRES          W12RES
662300060418     c                   z-add     1             P
662400060406     c* Verifico se nella data esiste un evento Part che giustifica
662500060418     c     w12dat        lookup    pvd(P)                                 30
662600060418    2c                   dow       *in30 and pvf(P)<>'G'
662700060418     c                   add       1             P
662800060420     c     w12dat        lookup    pvd(P)                                 30
662900060406    2c                   enddo
663000060406     c
663100060406     c* 30 on --> TROVATO !!
663200060406    2c                   if        *in30
663300060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
663400060406     C                   MOVEL     'S'           WSLITT
663500060406     c                   z-add     24            wnoadd
663600060406    3C                   ENDIF
663700060406     C*
663800060406    2C                   ENDIF                                                  <-- 2 ---
663900060406    1C                   ENDIF                                                  <-- 1 ---
664000060406     C*
664100060406     C                   ENDSR
664200931210     C********************************************************************
664300931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
664400931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
664500931210     C********************************************************************
664600050805     C***  CHKDCR        BEGSR
664700931210     C*
664800050805     C***  WPOCRI        IFNE      *ZEROS
664900050805     C***  WPOROU        IFGE      WPOCRI
665000050805     C***  WPOROU        ANDLT     WPOCRF
665100050805     C***                ELSE
665200050805     C***                MOVEL     'S'           WSLITT
665300050805     C***                END
665400050805     C***                END
665500931210     C*
665600050805     C***                ENDSR
665700140822     C********************************************************************
665800140822     C*  Reperimento tempi di consegna per decidere se slittare
665900140822     C********************************************************************
666000140822     C     Reper_tempi   BEGSR
666100140822
666200140822     c                   clear                   trulorSds
666300140825     c                   clear                   trulor2ds
666400140822     c                   eval       IOREtfp = d98tfp
666500140822     c                   eval       IOREtfa = d98tfa
666600140825     c                   eval       IOREdti = d98dti
666700140825     c                   eval       IOREhti = d98hti
666800140825     c                   eval       IOREdcr = s98dcr
666900140825     c                   eval       IOREhcr = s98hcr
667000140825     c                   eval       IOREtcr = s98tcr
667100140825     c                   eval       IOREDEI = d98dei
667200140825     c                   eval       IOREOEI = d98oei
667300140825     c                   eval       IOREfil = d98lna
667400140825     c                   eval       IOREcap = d98cad
667500140825     c                   eval       IOREloc = d98lod
667600140825     c                   eval       IOREnar = d98nzd
667700140825     c
667800140825     c* Data di Riferimento:
667900140825     c* Se impostata la consegnabilità --> prendo questa
668000140822     c                   select
668100140825     c                   when      d98dfc>0
668200141107     c                   if        or_datarif<d98dfc and or_datarif>0
668300141107     c                   eval      ioredta=or_datarif
668400141107     c                   else
668500141107     c                   eval      ioredta=d98dfc
668600141107     c                   endif
668700140825     c* altrimenti data di consegna
668800140825     c                   when      d98dcm>0
668900141107     c                   if        or_datarif<d98dcm and or_datarif>0
669000141107     c                   eval      ioredta=or_datarif
669100141107     c                   else
669200140825     c                   eval      ioredta=d98dcm
669300141107     c                   endif
669400140825     c
669500140822     c                   other
669600140825     c* altrimenti data teorica consegna se > = oggi
669700140825     C                   time                    wrkoggiiso
669800140825     C     *iso          move      wrkoggiiso    ioredta
669900140822     c* se < di oggi, imposto oggi
670000140825     c                   if        d98dei>ioredta
670100140825     c                   eval      ioredta=d98dei
670200140822     c                   endif
670300141107     c
670400141107     c                   if        or_datarif<ioredta and or_datarif>0
670500141107     c                   eval      ioredta=or_datarif
670600141107     c                   endif
670700141107     c
670800140822     c                   endsl
670900140822     c
671000140825     c                   eval       IOREtser = 'C'
671100141107     c                   if         or_ddc>0 and or_ddc<d98ddc
671200141107     c                   eval       IOREDDC=or_ddc
671300141107     c                   eval       IOREnDC=8888888
671400141107     c                   eval       IOREcons = 'S'
671500141107     c                   else
671600140825     c                   eval       IOREDDC=d98ddc
671700140825     c                   eval       IOREnDC=wd98ndc
671800140825     c                   eval       IOREcons = d98cons
671900141107     c                   endif
672000141017     c                   eval       IOREtsp = d98tsp
672100140822     c
672200140822     c                   call      'TRULORSR'
672300140822     c                   parm                    kpjba
672400140822     c                   parm                    trulorSds
672500140825     c                   parm                    trulor2ds
672600140825     c
672700140825     c                   eval      wtrulor= 'S'
672800141107     c                   clear                   or_datarif        8 0
672900141107     c                   clear                   or_ddc            8 0
673000140822     C                   ENDSR
673100950406     C********************************************************************
673200950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
673300950406     C********************************************************************
673400950406     C     DATORE        BEGSR
673500950406     C*
673600950406     C                   Z-ADD     *ZEROS        WORE
673700950406     C     WDAT          IFNE      0
673800950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
673900950406     C     WAA           DIV       4             WQUA              4 0
674000950406     C                   MVR                     WRES
674100950406     C     WAA           MULT      365           WGIO              9 0
674200950406     C                   ADD       WQUA          WGIO
674300950406     C                   ADD       WGG           WGIO
674400950406     C     WMM           SUB       1             MC                3 0
674500950406     C                   DO        MC            WX                3 0
674600950406     C                   ADD       MES(WX)       WGIO
674700950406     C                   END
674800950406     C     WRES          IFEQ      0
674900950406     C     WMM           ANDLE     2
675000950406     C                   SUB       1             WGIO
675100950406     C                   END
675200950406     C     WGIO          MULT      24            WORE              9 0
675300950406     C                   ENDIF
675400950406     C*
675500950406     C                   ENDSR
675600930323     C********************************************************************
675700930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
675800930323     C********************************************************************
675900930323     C     OREDAT        BEGSR
676000930323     C*
676100950407     C                   SETOFF                                       717273
676200950407     C                   SETOFF                                       7475
676300930323     C     WORE          DIV       24            WGIO
676400930323     C                   MVR                     WRES              3 0
676500950406     C*
676600950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
676700950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
676800950406     C*  TROVO ANNO
676900950406     C     WGIO          DIV       365           WAA
677000950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
677100950406     C                   MVR                     WREST                71
677200950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
677300950406     C     WGIO          DIV       365           WAA
677400950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
677500950406     C*                                                    0 = 31/12/PR
677600950406     C   75
677700950406     CAN 71              DO                                                     BIS. E NON ERA
677800950406     C     WAA           SUB       1             WAA
677900950406     C                   Z-ADD     365           WREST
678000950406     C                   END
678100950406     C*
678200950406     C     WAA           DIV       4             WNBIS                                      T.
678300950406     C                   MVR                     WRBIS             3 073
678400950406     C   73
678500950406     CANN71                                                                     NON PIU' BISEST
678600950406     CORN73              ADD       1             WREST                          E'BISESTILE
678700950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
678800950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
678900950406     C  N73              SUB       1             $PM(1)
679000950406     C                   Z-ADD     1             WI                2 0
679100950406     C* TROVO IL MESE E GIORNI
679200950406     C     WREST         LOOKUP    $PM(WI)                            74  73
679300950406     C                   MOVE      WI            WMM
679400950406     C     WI            COMP      1                                      72
679500950406     C  N72
679600950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
679700950406     C   74              SUB       1             WI
679800950406     C  N72
679900950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
680000950406     C   72              MOVE      WREST         WGG
680100950406     C                   ADD       1900          WAA
680200950406     C*
680300930323     C*
680400930323     C                   ENDSR
6805009407151507 C********************************************************************
6806009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6807009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6808009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6809009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6810009407151507 C********************************************************************
6811009407151507 C     FINGIO        BEGSR
6812009407151507 C*
6813009407151507 C     WPORE         DIV       24            WGIO
6814009407151507 C                   MVR                     WRES
6815009407151507 C     WRES          IFEQ      *ZEROS
6816009407151507 C                   Z-ADD     12            WNORE             3 0
6817009407151507 C                   ELSE
6818009407151507 C                   Z-ADD     *ZEROS        WNORE
6819009407151507 C                   ENDIF
6820009407151507 C*
6821009407151507 C                   ENDSR
682200930517     C********************************************************************
682300971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
682400930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
682500930517     C********************************************************************
682600930517     C     CERCAL        BEGSR
682700930517     C*
682800930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
682900941227     C                   MOVE      CLNTFP        DCATFP
683000941227     C                   MOVE      CLNTFA        DCATFA
683100941227     C                   MOVE      CLNANN        DCAANN
683200941227     C                   MOVE      CLNMES        DCAMES
683300930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
683400930517     C                   Z-ADD     1             I
683500930517     C     DCALEN        LOOKUP    CAK(I)                                 99
683600930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
683700930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
683800941227     C     KEYCLN        CHAIN     AZCLN01L                           99
683900930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
684000930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
684100930517     C                   MOVEL     '1'           D98ERR
684200930517     C                   MOVEL     MSG(3)        D98MSG
684300930517     C                   ELSE                                                   <-- 2 -->
684400970401     C* CONTROLLA SE ANNULLATO
684500970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
684600970401     C                   MOVEL     '1'           D98ERR
684700970401     C                   MOVEL     MSG(3)        D98MSG
684800970401     C                   ELSE                                                   <-- 3 -->
684900930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
685000930517     C                   ADD       1             WINDCA            6 0
685100970401     C     WINDCA        IFLE      200                                          --- 4 -->
685200970401     C     WINDCA        ANDGT     *ZEROS
685300930517     C                   Z-ADD     WINDCA        I
685400930517     C                   MOVEL     DCALEN        CAK(I)
685500941227     C                   MOVEL     CLNMAT        CAM(I)
685600941227     C                   MOVEL     CLNPOM        CAP(I)
685700970401     C                   END                                                    <-- 4 ---
685800970401     C                   ENDIF                                                  <-- 3 ---
685900930517     C                   END                                                    <-- 2 ---
686000930517     C                   ELSE                                                   <-- 1 -->
686100930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
686200941227     C                   MOVEL     CAM(I)        CLNMAT
686300941227     C                   MOVEL     CAP(I)        CLNPOM
686400930517     C                   END                                                    <-- 1 ---
686500930517     C*
686600930517     C                   ENDSR
686700010821     C********************************************************************
686800010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
686900010821     C********************************************************************
687000010821     C     CHKVCA        BEGSR
687100010821     C*
687200010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
687300010821     C     D98CAS        IFGT      *ZEROS
687400010821     C     D98VCA        ANDEQ     *BLANKS
687500010821     C                   MOVEL     'ITL'         D98VCA
687600010821     C                   ENDIF
687700010822     C*
687800010822     C     D98VCA        IFNE      'ITL'
687900010822     C     D98VCA        ANDNE     'EUR'
688000010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
688100010822     C                   Z-ADD     D98CAS        INPIMP
688200010822     C                   MOVEL     D98VCA        INPDIV
688300010822     C                   EXSR      EXECNV
688400010822     C                   Z-ADD     OUTIMP        D98CAS
688500010822     C                   MOVEL     §GEDCN        D98VCA
688600010822     C                   ENDIF
688700010821     C*
688800010821     C                   ENDSR
688900010821     C********************************************************************
689000010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
689100010821     C********************************************************************
689200010821     C     EXECNV        BEGSR
689300010821     C*
689400010821     C                   CLEAR                   YEURDS
689500010821     C*
689600010821     C                   MOVEL     INPDIV        YECDVI
689700010821     C                   Z-ADD     INPIMP        YECIMI
689800010821     C                   MOVEL     §GEDCN        YECDVO
689900010821     C                   MOVEL     '3'           YECDCO
690000010821     C                   MOVEL     'H'           YECTAR
690100010821     C                   CALL      'YEURCO'
690200010821     C                   PARM                    YEURDS
690300010821     C*
690400010821     C                   Z-ADD     YECIMO        OUTIMP
690500010821     C*
690600010821     C                   ENDSR
690700011211     C*------------------------------------------------------------------------*
690800011211     C* Controllo importo massimo contrassegno.
690900011211     C*------------------------------------------------------------------------*
691000011211     C     REPMAXCSB     BEGSR
691100011211     C*
691200011211     C                   CLEAR                   Trul21DS
691300011211     C*
691400160420     c                   eval      i21cbo = d98cbo
691500011211     C                   EVAL      I21TSp = d98TSP
691600011211     C                   EVAL      I21NzD = d98NZD
691700011211     C                   EVAL      I21LnP = d98LNP
691800011211     C                   EVAL      I21LnA = d98LNA
691900011211     C                   EVAL      I21Ksc = d98KSC
692000011211     C                   EVAL      I21TiC = d98TIC
692100011211     C                   EVAL      I21Imp = d98CAS
692200011211     C                   EVAL      I21Div = d98VCA
692300160420     c                   eval      i21cbo = d98cbo
692400011211     C*
692500011211     C                   CALL      'TRUL21R'
692600011211     C                   PARM                    Trul21DS
692700160420
692800160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
692900160420     c                   if        wtrasm='N'
693000160420     c                   if        o21fca <>*blanks
693100160420     c                   eval      §b4ltca = 'S'
693200160420     c                   endif
693300160420     c                   endif
693400011211     C*
693500011211     C                   ENDSR
693600011211     C*------------------------------------------------------------------------*
693700011211
693800930414     C********************************************************************
693900930414     C*   RUTINZ - ROUTINE INIZIALE
694000930414     C********************************************************************
694100930414     C     RUTINZ        BEGSR
694200930414     C*
694300930414     C* PARAMETRI IN INPUT ALLA ROUTINE
694400930414     C     *ENTRY        PLIST
694500970731     C                   PARM                    DS99
694600941227     C*
694700941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
694800941227     C     KEYBLP        KLIST
694900941227     C                   KFLD                    BLPAAS
695000941227     C                   KFLD                    BLPLNP
695100941227     C                   KFLD                    BLPNRS
695200941227     C                   KFLD                    BLPNSP
695300050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
695400941227     C     KEYGCP        KLIST
695500050405     C                   KFLD                    KWGCPAAS
695600050405     C                   KFLD                    KWGCPLNP
695700050405     C                   KFLD                    KWGCPNRS
695800050405     C                   KFLD                    KWGCPNSP
695900050405     C                   KFLD                    KWGCPFRG
696000050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
696100031125     C     KEYGC_P       KLIST
696200050405     C                   KFLD                    KWGCPAAS
696300050405     C                   KFLD                    KWGCPLNP
696400050405     C                   KFLD                    KWGCPNRS
696500050405     C                   KFLD                    KWGCPNSP
696600970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
696700150120     C**   KEYBLT        KLIST
696800150120     C**                 KFLD                    BLTAAS
696900150120     C**                 KFLD                    BLTLNP
697000150120     C**                 KFLD                    BLTNRS
697100150120     C**                 KFLD                    BLTNSP
697200941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
697300941227     C     KEYARB        KLIST
697400941227     C                   KFLD                    ARBAAS
697500941227     C                   KFLD                    ARBLNP
697600941227     C                   KFLD                    ARBNRS
697700941227     C                   KFLD                    ARBNSP
697800051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
697900941228     C     KEYAR9        KLIST
698000941228     C                   KFLD                    AR9AAS
698100941228     C                   KFLD                    AR9LNP
698200941228     C                   KFLD                    AR9NRS
698300941228     C                   KFLD                    AR9NSP
698400941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
698500050405     C     KEYGCP21      KLIST
698600050405     C                   KFLD                    KWGCPAAS
698700050405     C                   KFLD                    KWGCPLNP
698800050405     C                   KFLD                    KWGCPNRS
698900050405     C                   KFLD                    KWGCPNSP
699000050405     C                   KFLD                    KWGCPFRG
699100970912     C* CHIAVE LETTURA FNART01L - PARZIALE
699200150120     C***  KEYART        KLIST
699300150120     C***                KFLD                    ARTAAS
699400150120     C***                KFLD                    ARTLNP
699500150120     C***                KFLD                    ARTNRS
699600150120     C***                KFLD                    ARTNSP
699700950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
699800950102     C     KEYLBL        KLIST
699900950102     C                   KFLD                    LBLAAN
700000950102     C                   KFLD                    LBLLPN
700100950102     C                   KFLD                    LBLNRN
700200950102     C                   KFLD                    LBLNSN
700300930414     C*
700400070827     c     kAR5          Klist
700500070827     c                   Kfld                    d98Aas
700600070827     c                   Kfld                    d98Lnp
700700070827     c                   Kfld                    d98nrs
700800070827     c                   Kfld                    d98nsp
700900070827     c                   Kfld                    kAr5Trd
701000150325     c     kAR4          Klist
701100150325     c                   Kfld                    d98Aas
701200150325     c                   Kfld                    d98Lnp
701300150325     c                   Kfld                    d98nrs
701400150325     c                   Kfld                    d98nsp
701500150325     c                   Kfld                    kAr4Trc
701600150218     c*
701700150218     c     kart          Klist
701800150218     c                   Kfld                    d98Aas
701900150218     c                   Kfld                    d98Lnp
702000150218     c                   Kfld                    d98nrs
702100150218     c                   Kfld                    d98nsp
702200150218     c     kbrv          Klist
702300150218     c                   Kfld                    artfls
702400150218     c                   Kfld                    artlna
702500150218     c                   Kfld                    artnrs
702600150218     c                   Kfld                    artnsc
702700150325     C     Kfapd         klist
702800150325     C                   kfld                    kpdtip
702900150325     C                   kfld                    kpdpdr
703000930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
703100930414     C     KEYTAB        KLIST
703200930414     C                   KFLD                    KTBUTE
703300930414     C                   KFLD                    KTBCOD
703400930414     C                   KFLD                    KTBKEY
703500060406     C     Ktab          KLIST
703600060406     C                   KFLD                    KTBUTE
703700060406     C                   KFLD                    KTBCOD
703800930414     C*
703900941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
704000941227     C     KEYCLN        KLIST
704100941227     C                   KFLD                    CLNTFP
704200941227     C                   KFLD                    CLNTFA
704300941227     C                   KFLD                    CLNANN
704400941227     C                   KFLD                    CLNMES
704500941227     C*
704600941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
704700941227     C     KEYCAS        KLIST
704800941227     C                   KFLD                    CASPTA
704900941227     C                   KFLD                    CASLNP
705000941227     C                   KFLD                    CASLNA
705100941227     C                   KFLD                    CASTSP
705200941227     C                   KFLD                    CASDDE
705300930420     C*
705400990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
705500930420     C     KEYEVB        KLIST
705600930420     C                   KFLD                    KEBAAS
705700930420     C                   KFLD                    KEBLNP
705800930420     C                   KFLD                    KEBNRS
705900930420     C                   KFLD                    KEBNSP
706000010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
706100010821     C     KTBE          KLIST
706200010821     C                   KFLD                    TBECOD
706300010821     C                   KFLD                    TBEKE1
706400010821     C                   KFLD                    TBEKE2
706500930414     C*
706600010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
706700930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
706800930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
706900930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
707000930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
707100930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
707200930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
707300930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
707400930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
707500930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
707600010821     C     *LIKE         DEFINE    D98CAS        INPIMP
707700010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
707800010821     C     *LIKE         DEFINE    D98VCA        INPDIV
707900930414     C*
708000930414     C* RECUPERO IL CODICE DELL'AZIENDA
708100930414     C                   Z-ADD     1             CODUT             1 0
708200970701     C                   Z-ADD     *ZEROS        I                 6 0
708300930414     C*
708400930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
708500930414     C                   EXSR      AZZERA
708600930414     C*
708700930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
708800941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
708900941227     C                   MOVEL     'S'           CTABSN            1
709000930414     C                   EXSR      CARTAB
709100941227     C                   ENDIF
709200041116     c* Se passato umero di spedizioni apro i files , altrimenti no
709300041116     c                   if        d98nsp>0 and wapri=' '
709400041116     c                   open      fnblp01l
709500050405     c                   open      tigcp01l
709600150120     c***                open      fnblt01l
709700041116     c                   open      fnarb01l
709800051115     c                   open      fiar901l
709900150325     c                   open      fiar401l
710000050405     c                   open      tigcp21l
710100150218     c                   open      fnart01l
710200150218     c                   open      fnbrv07l
710300041116     c                   open      fnlbl01l
710400041116     c                   open      fnevb01l
710500070827     c                   open      fiar501l
710600041116     c                   eval      wapri='S'
710700041116     c                   endif
710800930414     C*
710900930414     C                   ENDSR
711000941227     C*------------------------------------------------------------------------*
711100941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
711200941227     C*------------------------------------------------------------------------*
711300941227     C     SUBDAT        BEGSR
711400941227     C*
711500941227     C                   TIME                    DTTIME
711600941227     C                   Z-ADD     DTANN         DSANN
711700941227     C                   Z-ADD     DTMES         DSMES
711800941227     C                   Z-ADD     DTGIO         DSGIO
711900941227     C                   Z-ADD     DTSEC         DSSEC
712000941227     C*
712100941227     C                   ENDSR
712200930414     C********************************************************************
712300930414     C*   CARTAB - CARICA LE TABELLE
712400930414     C********************************************************************
712500930414     C     CARTAB        BEGSR
712600930414     C*
712700930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
712800930414     C     D98ERR        IFEQ      '0'
712900930421     C                   Z-ADD     0             I
713000930414     C                   Z-ADD     CODUT         KTBUTE
713100930414     C                   MOVEL     '1Y'          KTBCOD
713200930414     C                   MOVEL     *BLANKS       KTBKEY
713300930414     C     KEYTAB        SETLL     TABEL00F                           99
713400930414     C  N99              READ      TABEL00F                               99
713500930414     C     *IN99         IFEQ      '1'
713600930414     C                   MOVEL     '1'           D98ERR
713700930414     C                   MOVEL     MSG(5)        D98MSG
713800930414     C                   END
713900930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
714000930414     C     TBLCOD        ANDEQ     '1Y'
714100930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
714200930421     C                   ADD       1             I
714300930421     C                   MOVEL     TBLKEY        C1Y(I)
714400930414     C                   MOVEL     TBLUNI        DS1Y
714500930421     C                   MOVEL     §1YFEI        I1Y(I)
714600930421     C                   MOVEL     §1YFEC        E1Y(I)
714700930414     C                   END                                                    <-- 3 ---
714800930414     C                   READ      TABEL00F                               99
714900930414     C                   END                                                    <-- 2 ---
715000930414     C                   END
715100930414     C*
715200930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
715300930414     C     D98ERR        IFEQ      '0'
715400930421     C                   Z-ADD     0             I
715500930414     C                   Z-ADD     CODUT         KTBUTE
715600930414     C                   MOVEL     '1X'          KTBCOD
715700930414     C                   MOVEL     *BLANKS       KTBKEY
715800930414     C     KEYTAB        SETLL     TABEL00F                           99
715900930414     C  N99              READ      TABEL00F                               99
716000930414     C     *IN99         IFEQ      '1'
716100930414     C                   MOVEL     '1'           D98ERR
716200930414     C                   MOVEL     MSG(1)        D98MSG
716300930414     C                   END
716400930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
716500930414     C     TBLCOD        ANDEQ     '1X'
716600930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
716700930421     C                   ADD       1             I
716800930421     C                   MOVEL     TBLKEY        C1X(I)
716900930414     C                   MOVEL     TBLUNI        DS1X
717000930421     C                   Z-ADD     §1XHHG        G1X(I)
717100930421     C                   Z-ADD     §1XHHO        O1X(I)
717200930421     C                   Z-ADD     §1XHHM        M1X(I)
717300930421     C                   Z-ADD     §1XHHP        P1X(I)
717400930421     C                   Z-ADD     §1XHHI        I1X(I)
717500930421     C                   Z-ADD     §1XHHS        S1X(I)
717600930414     C                   END                                                    <-- 3 ---
717700930414     C                   READ      TABEL00F                               99
717800931223     C                   ENDDO                                                  <-- 2 ---
717900050523     c
718000050523     c* controllo riempimento skiere
718100050523     c                   clear                   trul0sds
718200050523     c                   eval      i0sski='C1X/G1X/O1X/...'
718300050523     c                   eval      i0sele=%elem(c1x)
718400050523     c                   eval      i0spie=i
718500050523     c                   eval      i0sfile='TABEL00F  '
718600050523     c                   eval      i0scod=' 1X  '
718700050523     c                   eval      i0spgm='TNSD99R'
718800050523     c                   movel     trul0sds      kpjbu
718900050523     c                   call      'TRUL0SR'
719000050523     c                   parm                    kpjba
719100050523     C                   END
719200930421     C*
719300930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
719400930421     C     D98ERR        IFEQ      '0'
719500930421     C                   Z-ADD     0             I
719600930421     C                   Z-ADD     CODUT         KTBUTE
719700930421     C                   MOVEL     '2A'          KTBCOD
719800060406     C     Ktab          SETLL     TABEL00F                           99
719900060406     C  N99ktab          READe     TABEL00F                               99
720000930421     C     *IN99         IFEQ      '1'
720100930421     C                   MOVEL     '1'           D98ERR
720200930421     C                   MOVEL     MSG(8)        D98MSG
720300930421     C                   END
720400930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
720500930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
720600930421     C                   ADD       1             I
720700930421     C                   MOVEL     TBLKEY        C2A(I)
720800060406     C                   MOVEL     TBLUNI        ds2a
720900060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
721000060406     C                   MOVEL     w003a         D2A(I)
721100930421     C                   END                                                    <-- 3 ---
721200060406     C     ktab          READe     TABEL00F                               99
721300931223     C                   ENDDO                                                  <-- 2 ---
721400050523     c* controllo riempimento skiere
721500050523     c                   clear                   trul0sds
721600050523     c                   eval      i0sski='C2A/D2A'
721700050523     c                   eval      i0sele=%elem(c2a)
721800050523     c                   eval      i0spie=i
721900050523     c                   eval      i0sfile='TABEL00F  '
722000060406     c                   eval      i0scod='2A/2Z'
722100050523     c                   eval      i0spgm='TNSD99R'
722200050523     c                   movel     trul0sds      kpjbu
722300050523     c                   call      'TRUL0SR'
722400050523     c                   parm                    kpjba
722500930421     C                   END
722600060406     c* Tabella  2Z
722700060406     C     D98ERR        IFEQ      '0'
722800060406     C                   Z-ADD     CODUT         KTBUTE
722900060406     C                   MOVEL     '2Z'          KTBCOD
723000060406     C     Ktab          SETLL     TABEL00F                           99
723100060406     C  N99ktab          READe     TABEL00F                               99
723200060406     C     *IN99         IFEQ      '1'
723300060406     C                   MOVEL     '1'           D98ERR
723400060406     C                   MOVEL     MSG(33)       D98MSG
723500060406     C                   END
723600060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
723700060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
723800060406     C                   Z-ADD     1             I
723900060406     C                   MOVEL     TBLKEY        w003a
724000060406     c     w003a         lookup    c2a(i)                                 99
724100060406     C     *IN99         IFEQ      *off
724200060406     C                   MOVEL     '1'           D98ERR
724300060406     C                   MOVEL     MSG(34)       D98MSG
724400060406     C                   Else
724500060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
724600170915     c                   eval      ds2z=tbluni
724700170915     c                   eval      %subst(d2z(i):6:1)=w2ztel
724800060406     C                   END                                                    <-- 3 ---
724900060406     C                   END                                                    <-- 3 ---
725000060406     C     ktab          READe     TABEL00F                               99
725100060406     C                   ENDDO                                                  <-- 2 ---
725200060406     C                   END
725300941227     C*
725400941227     C* CARICA TABELLA CODICI BOLLA -3A-
725500941227     C     D98ERR        IFEQ      '0'
725600941228     C                   Z-ADD     0             I
725700941227     C                   Z-ADD     CODUT         KTBUTE
725800941227     C                   MOVEL     '3A'          KTBCOD
725900941227     C                   MOVEL     *BLANKS       KTBKEY
726000941227     C     KEYTAB        SETLL     TABEL00F                           99
726100941227     C  N99              READ      TABEL00F                               99
726200941227     C     *IN99         IFEQ      '1'
726300941227     C                   MOVEL     '1'           D98ERR
726400941227     C                   MOVEL     MSG(22)       D98MSG
726500941227     C                   END
726600941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
726700941227     C     TBLCOD        ANDEQ     '3A'
726800941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
726900941227     C                   ADD       1             I
727000941227     C                   MOVEL     TBLKEY        C3A(I)
727100941227     C                   MOVEL     TBLUNI        DS3A
727200941227     C                   MOVEL     §3ATB1        T3A(I)
727300941228     C                   Z-ADD     §3AFCA        F3A(I)
727400990308     C                   MOVEL     §3ARBL        R3A(I)
727500941227     C                   END                                                    <-- 3 ---
727600941227     C                   READ      TABEL00F                               99
727700941227     C                   END                                                    <-- 2 ---
727800050523     c* controllo riempimento skiere
727900050523     c                   clear                   trul0sds
728000050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
728100050523     c                   eval      i0sele=%elem(c3a)
728200050523     c                   eval      i0spie=i
728300050523     c                   eval      i0sfile='TABEL00F  '
728400050523     c                   eval      i0scod=' 3A  '
728500050523     c                   eval      i0spgm='TNSD99R'
728600050523     c                   movel     trul0sds      kpjbu
728700050523     c                   call      'TRUL0SR'
728800050523     c                   parm                    kpjba
728900941227     C                   END
729000971210     C*
729100020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
729200020226     C                   Z-ADD     *ZEROS        J                 3 0
729300160331     C                   Z-ADD     *ZEROS        D                 3 0
729400971210IF  0C     D98ERR        IFEQ      '0'
729500971210     C     *LOVAL        SETLL     AZORG01L
729600971210     C                   READ      AZORG01L                               98
729700971210DO  1C     *IN98         DOWEQ     *OFF
729800971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
729900971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
730000971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
730100020627     C                   MOVEL     ORGDE3        OG143
730200020702     C*
730300020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
730400020702     C                   CLEAR                   DNTW
730500020702     C*
730600020702     C                   MOVEL(P)  'NTW'         TBECOD
730700020702     C                   MOVEL(P)  §OGNTW        TBEKE1
730800020702     C     KTBE          CHAIN     TNTBE01L
730900020702     C                   IF        %found(TNTBE01L)
731000020702     C                   MOVEL     TBEUNI        DNTW
731100020702     C                   ENDIF
731200020702     C*
731300020702     C                   MOVEL     §NTWFIE       FL1(F)
731400020627     C*
731500020226     C* VERIFICO SE FILIALE FEDEX
731600020226     C                   IF        §OGNTW = 'FED'
731700020226     C                   ADD       1             J
731800020226     C                   Z-ADD     ORGFIL        FILFED(J)
731900020226     C                   ENDIF
732000160331     C* VERIFICO SE FILIALE DPD
732100160331     C                   IF        §OGNTW = 'DPD'
732200160331     C                   ADD       1             D
732300160331     C                   Z-ADD     ORGFIL        FILDPD(D)
732400160331     C                   ENDIF
732500020226     C*
732600971210E   2C                   ENDIF
732700971210     C                   READ      AZORG01L                               98
732800971210E   1C                   ENDDO
732900971210E   0C                   ENDIF
733000010821     C*
733100010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
733200010821     C                   MOVEL     'GED'         TBECOD
733300070514     C                   MOVEL     *BLANKS       TBEKE1
733400070514     C                   MOVEL     '1'           TBEKE1
733500010821     C                   MOVEL     *BLANKS       TBEKE2
733600010821     C     KTBE          CHAIN     TNTBE01L                           21
733700070514     C  N21              MOVEL     TBEUNI        DGED
733800150326     c
733900150326     C* REPERISCO LA tabella flag operativi per tabella OLP
734000150326     c                   clear                   dvpo
734100150326     C                   MOVEL     'VPO'         TBECOD
734200150326     C                   MOVEL     *BLANKS       TBEKE2
734300150326     C                   MOVEL(P)  'VPO'         TBEKE1
734400150326     C     KTBE          CHAIN     TNTBE01L                           21
734500150326     C  N21              MOVEL     TBEUNI        DVPO
734600150326     C*
734700031201     C*
734800031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
734900031201     C     D98ERR        IFEQ      '0'
735000060403     C                   Z-ADD     0             I7QW              3 0
735100031201     C                   Z-ADD     CODUT         KTBUTE
735200031201     C                   MOVEL     '7Q'          KTBCOD
735300031201     C                   MOVEL     *BLANKS       KTBKEY
735400031201     C     KEYTAB        SETLL     TABEL00F                           99
735500031201     C  N99              READ      TABEL00F                               99
735600031201     C     *IN99         IFEQ      '1'
735700031201     C                   MOVEL     '1'           D98ERR
735800031201     C                   MOVEL     MSG(8)        D98MSG
735900031201     C                   ENDIF
736000031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
736100031201     C     TBLCOD        ANDEQ     '7Q'
736200031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
736300031201     C                   MOVEL     TBLUNI        DS7Q
736400031201     C                   IF        §7Q4D1 = 'R' OR
736500031201     C                             §7Q4D2 = 'R' OR
736600031201     C                             §7Q4D3 = 'R' OR
736700031201     C                             §7Q4D4 = 'R' OR
736800031201     C                             §7Q4D5 = 'R'
736900031201     C                   ADD       1             I7QW
737000031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
737100031201     C                   ENDIF                                                  <-- 3 ---
737200031201     C                   ENDIF                                                  <-- 3 ---
737300031201     C                   READ      TABEL00F                               99
737400031201     C                   ENDDO                                                  <-- 2 ---
737500031201     C                   ENDIF
737600031201     C*
737700031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
737800031201     C     D98ERR        IFEQ      '0'
737900031201     C                   Z-ADD     CODUT         KTBUTE
738000031201     C                   MOVEL     '7W'          KTBCOD
738100031201     C                   MOVEL     *BLANKS       KTBKEY
738200031201     C     KEYTAB        SETLL     TABEL00F                           99
738300031201     C  N99              READ      TABEL00F                               99
738400031201     C     *IN99         IFEQ      '1'
738500031201     C                   MOVEL     '1'           D98ERR
738600031201     C                   MOVEL     MSG(8)        D98MSG
738700031201     C                   ENDIF
738800031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
738900031201     C     TBLCOD        ANDEQ     '7W'
739000031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
739100031201     C                   MOVEL     TBLUNI        DS7W
739200031201     C                   IF        §7W6D1 = 'C' OR
739300031201     C                             §7W6D2 = 'C' OR
739400031201     C                             §7W6D3 = 'C' OR
739500031201     C                             §7W6D4 = 'C' OR
739600031201     C                             §7W6D5 = 'C'
739700031201     C                   ADD       1             I7QW
739800031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
739900031201     C                   ENDIF                                                  <-- 3 ---
740000031201     C                   ENDIF                                                  <-- 3 ---
740100031201     C                   READ      TABEL00F                               99
740200031201     C                   ENDDO                                                  <-- 2 ---
740300050523     c* controllo riempimento skiere
740400050523     c                   clear                   trul0sds
740500050523     c                   eval      i0sski='SK7QW'
740600050523     c                   eval      i0sele=%elem(sk7qw)
740700050523     c                   eval      i0spie=i7qw
740800050523     c                   eval      i0sfile='TABEL00F  '
740900050523     c                   eval      i0scod='7Q-7W'
741000050523     c                   eval      i0spgm='TNSD99R'
741100050523     c                   movel     trul0sds      kpjbu
741200050523     c                   call      'TRUL0SR'
741300050523     c                   parm                    kpjba
741400031201     C                   ENDIF
741500930414     C*
741600930414     C                   ENDSR
741700930414     C********************************************************************
741800930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
741900930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
742000930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
742100930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
742200930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
742300930414     C********************************************************************
742400930414     C     RO3ORE        BEGSR
742500930414     C*
742600930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
742700930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
742800930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
742900050902     c                   movel     ' '           WR3MATPOM         1
743000931209     C                   Z-ADD     *ZEROS        WR3HHO
743100931209     C                   Z-ADD     *ZEROS        WR3HHA
743200930414     C                   MOVE      WR3TAB        WR3COD
743300930604     C                   MOVEL     D98LNA        WR3COD            8
743400930421     C                   Z-ADD     1             I
743500930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
743600930414     C     *IN99         IFEQ      '0'
743700930414     C                   MOVE      WR3TAB        WR3COD
743800930414     C                   MOVEL     '   '         WR3COD            8
743900930421     C                   Z-ADD     1             I
744000930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
744100930414     C                   END
744200930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
744300930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
744400930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
744500930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
744600930414     C                   ELSE                                                   <-- 2 -->
744700930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
744800930421     C                   Z-ADD     M1X(I)        WR3HHO
744900930510     C                   Z-ADD     I1X(I)        WR3HHA
745000050902     c                   movel     'M'           WR3MATPOM         1
745100930414     C                   END
745200930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
745300930421     C                   Z-ADD     P1X(I)        WR3HHO
745400930510     C                   Z-ADD     S1X(I)        WR3HHA
745500050902     c                   movel     'P'           WR3MATPOM
745600930414     C                   END
745700930414     C                   END                                                    <-- 2 ---
745800930414     C                   END                                                    <-- 1 ---
745900930414     C*
746000930414     C                   ENDSR
746100930423     C********************************************************************
746200930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
746300930423     C********************************************************************
746400930423     C     W98D98        BEGSR
746500930423     C*
746600941228     C*
746700941228     C* ORE REALI PARTENZA INTERNE
746800941228     C                   Z-ADD     W98RPI        W98               3 0
746900941228     C                   EXSR      W98999
747000941228     C                   Z-ADD     D98           D98RPI
747100941228     C* ORE REALI PARTENZA CLIENTE
747200941228     C                   Z-ADD     W98RPC        W98
747300941228     C                   EXSR      W98999
747400941228     C                   Z-ADD     D98           D98RPC
747500930423     C* ORE REALI TRAZIONE INTERNE
747600941228     C                   Z-ADD     W98RTI        W98
747700941228     C                   EXSR      W98999
747800941228     C                   Z-ADD     D98           D98RTI
747900930423     C* ORE REALI TRAZIONE CLIENTE
748000941228     C                   Z-ADD     W98RTC        W98
748100941228     C                   EXSR      W98999
748200941228     C                   Z-ADD     D98           D98RTC
748300930423     C* ORE REALI CONSEGNA INTERNE
748400941228     C                   Z-ADD     W98RCI        W98
748500941228     C                   EXSR      W98999
748600941228     C                   Z-ADD     D98           D98RCI
748700930423     C* ORE REALI CONSEGNA CLIENTE
748800941228     C                   Z-ADD     W98RCC        W98
748900941228     C                   EXSR      W98999
749000941228     C                   Z-ADD     D98           D98RCC
749100930423     C* ORE REALI RESA INTERNE
749200941228     C                   Z-ADD     W98RRI        W98
749300941228     C                   EXSR      W98999
749400941228     C                   Z-ADD     D98           D98RRI
749500930423     C* ORE REALI RESA CLIENTE
749600941228     C                   Z-ADD     W98RRC        W98
749700941228     C                   EXSR      W98999
749800941228     C                   Z-ADD     D98           D98RRC
749900941228     C* ORE DIFF. PARTENZA INTERNE
750000941228     C                   Z-ADD     W98FPI        W98
750100941228     C                   EXSR      W98999
750200941228     C                   Z-ADD     D98           D98FPI
750300941228     C* ORE DIFF. PARTENZA CLIENTE
750400941228     C                   Z-ADD     W98FPC        W98
750500941228     C                   EXSR      W98999
750600941228     C                   Z-ADD     D98           D98FPC
750700930423     C* ORE DIFF. TRAZIONE INTERNE
750800941228     C                   Z-ADD     W98FTI        W98
750900941228     C                   EXSR      W98999
751000941228     C                   Z-ADD     D98           D98FTI
751100930423     C* ORE DIFF. TRAZIONE CLIENTE
751200941228     C                   Z-ADD     W98FTC        W98
751300941228     C                   EXSR      W98999
751400941228     C                   Z-ADD     D98           D98FTC
751500930423     C* ORE DIFF. CONSEGNA INTERNE
751600941228     C                   Z-ADD     W98FCI        W98
751700941228     C                   EXSR      W98999
751800941228     C                   Z-ADD     D98           D98FCI
751900930423     C* ORE DIFF. CONSEGNA CLIENTE
752000941228     C                   Z-ADD     W98FCC        W98
752100941228     C                   EXSR      W98999
752200941228     C                   Z-ADD     D98           D98FCC
752300930423     C* ORE DIFF. RESA INTERNE
752400941228     C                   Z-ADD     W98FRI        W98
752500941228     C                   EXSR      W98999
752600941228     C                   Z-ADD     D98           D98FRI
752700930423     C* ORE DIFF. RESA CLIENTE
752800941228     C                   Z-ADD     W98FRC        W98
752900941228     C                   EXSR      W98999
753000941228     C                   Z-ADD     D98           D98FRC
753100941228     C* ORE NON CONF. PARTENZA INTERNE
753200941228     C     W98NPI        IFGT      999
753300941228     C                   Z-ADD     999           D98NPI
753400941228     C                   ELSE
753500941228     C                   Z-ADD     W98NPI        D98NPI
753600941228     C                   END
753700941228     C* ORE NON CONF. PARTENZA CLIENTE
753800941228     C     W98NPC        IFGT      999
753900941228     C                   Z-ADD     999           D98NPC
754000941228     C                   ELSE
754100941228     C                   Z-ADD     W98NPC        D98NPC
754200941228     C                   END
754300930423     C* ORE NON CONF. TRAZIONE INTERNE
754400930423     C     W98NTI        IFGT      999
754500930423     C                   Z-ADD     999           D98NTI
754600930423     C                   ELSE
754700930423     C                   Z-ADD     W98NTI        D98NTI
754800930423     C                   END
754900930423     C* ORE NON CONF. TRAZIONE CLIENTE
755000930423     C     W98NTC        IFGT      999
755100930423     C                   Z-ADD     999           D98NTC
755200930423     C                   ELSE
755300930423     C                   Z-ADD     W98NTC        D98NTC
755400930423     C                   END
755500930423     C* ORE NON CONF. CONSEGNA INTERNE
755600930423     C     W98NCI        IFGT      999
755700930423     C                   Z-ADD     999           D98NCI
755800930423     C                   ELSE
755900930423     C                   Z-ADD     W98NCI        D98NCI
756000930423     C                   END
756100930423     C* ORE NON CONF. CONSEGNA CLIENTE
756200930423     C     W98NCC        IFGT      999
756300930423     C                   Z-ADD     999           D98NCC
756400930423     C                   ELSE
756500930423     C                   Z-ADD     W98NCC        D98NCC
756600930423     C                   END
756700930423     C* ORE NON CONF. RESA INTERNE
756800930423     C     W98NRI        IFGT      999
756900930423     C                   Z-ADD     999           D98NRI
757000930423     C                   ELSE
757100930423     C                   Z-ADD     W98NRI        D98NRI
757200930423     C                   END
757300930423     C* ORE NON CONF. RESA CLIENTE
757400930423     C     W98NRC        IFGT      999
757500930423     C                   Z-ADD     999           D98NRC
757600930423     C                   ELSE
757700930423     C                   Z-ADD     W98NRC        D98NRC
757800930423     C                   END
757900930423     C* ORE X TURNO CHIUSURA INTERNE
758000941228     C                   Z-ADD     W98HHI        W98
758100941228     C                   EXSR      W98999
758200941228     C                   Z-ADD     D98           D98HHI
758300930423     C* ORE X TURNO CHIUSURA CLIENTE
758400941228     C                   Z-ADD     W98HHC        W98
758500941228     C                   EXSR      W98999
758600941228     C                   Z-ADD     D98           D98HHC
758700060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
758800060418     C                   Z-ADD     W98EGI        W98
758900060418     C                   EXSR      W98999
759000060418     C                   Z-ADD     D98           D98hEGI
759100060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
759200060418     C                   Z-ADD     W98EGC        W98
759300060418     C                   EXSR      W98999
759400060418     C                   Z-ADD     D98           D98hEGC
759500060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
759600060420     C                   Z-ADD     W98EAI        W98
759700060420     C                   EXSR      W98999
759800060420     C                   Z-ADD     D98           D98hEAI
759900060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
760000060420     C                   Z-ADD     W98EAC        W98
760100060420     C                   EXSR      W98999
760200060420     C                   Z-ADD     D98           D98hEAC
760300930423     C* ORE X LASC. AVVISO INTERNE
760400941228     C                   Z-ADD     W98HLI        W98
760500941228     C                   EXSR      W98999
760600941228     C                   Z-ADD     D98           D98HLI
760700930423     C* ORE X LASC. AVVISO CLIENTE
760800941228     C                   Z-ADD     W98HLC        W98
760900941228     C                   EXSR      W98999
761000941228     C                   Z-ADD     D98           D98HLC
761100930423     C* ORE X GIACENZA INTERNE
761200941228     C                   Z-ADD     W98HGI        W98
761300941228     C                   EXSR      W98999
761400941228     C                   Z-ADD     D98           D98HGI
761500930423     C* ORE X GIACENZA CLIENTE
761600941228     C                   Z-ADD     W98HGC        W98
761700941228     C                   EXSR      W98999
761800941228     C                   Z-ADD     D98           D98HGC
761900941228     C* ORE X FEST. PART. INTERNE
762000941228     C                   Z-ADD     W98HPI        W98
762100941228     C                   EXSR      W98999
762200941228     C                   Z-ADD     D98           D98HPI
762300941228     C* ORE X FEST. PART. CLIENTE
762400941228     C                   Z-ADD     W98HPC        W98
762500941228     C                   EXSR      W98999
762600941228     C                   Z-ADD     D98           D98HPC
762700930423     C* ORE X FEST. TRAZ. INTERNE
762800941228     C                   Z-ADD     W98HFI        W98
762900941228     C                   EXSR      W98999
763000941228     C                   Z-ADD     D98           D98HFI
763100930423     C* ORE X FEST. TRAZ. CLIENTE
763200941228     C                   Z-ADD     W98HFC        W98
763300941228     C                   EXSR      W98999
763400941228     C                   Z-ADD     D98           D98HFC
763500930423     C* ORE X FEST. CONS. INTERNE
763600941228     C                   Z-ADD     W98HEI        W98
763700941228     C                   EXSR      W98999
763800941228     C                   Z-ADD     D98           D98HEI
763900930423     C* ORE X FEST. CONS. CLIENTE
764000941228     C                   Z-ADD     W98HEC        W98
764100941228     C                   EXSR      W98999
764200941228     C                   Z-ADD     D98           D98HEC
764300930511     C* ORE X F.DEPOSITO INTERNE
764400941228     C                   Z-ADD     W98HDI        W98
764500941228     C                   EXSR      W98999
764600941228     C                   Z-ADD     D98           D98HDI
764700930511     C* ORE X F.DEPOSITO CLIENTE
764800941228     C                   Z-ADD     W98HDC        W98
764900941228     C                   EXSR      W98999
765000941228     C                   Z-ADD     D98           D98HDC
765100930511     C* ORE X CONTRASSEGNO INTERNE
765200941228     C                   Z-ADD     W98HOI        W98
765300941228     C                   EXSR      W98999
765400941228     C                   Z-ADD     D98           D98HOI
765500930511     C* ORE X CONTRASSEGNO CLIENTE
765600941228     C                   Z-ADD     W98HOC        W98
765700941228     C                   EXSR      W98999
765800941228     C                   Z-ADD     D98           D98HOC
765900930914     C* ORE DI CONSEGNABILITA' INTERNE
766000941228     C                   Z-ADD     W98OCI        W98
766100941228     C                   EXSR      W98000
766200941228     C                   Z-ADD     D98           D98OCI
766300930914     C* ORE DI CONSEGNABILITA' CLIENTE
766400941228     C                   Z-ADD     W98OCC        W98
766500941228     C                   EXSR      W98000
766600941228     C                   Z-ADD     D98           D98OCC
766700071122     C
766800071122     c* Per i codici bolla M prepagati annullati, azzero tutto
766900071122     c                   if        d98cbo='M '
767000071122     c                   clear                   d98rpi
767100071122     c                   clear                   d98rpc
767200071122     c                   clear                   d98rti
767300071122     c                   clear                   d98rtc
767400071122     c                   clear                   d98rci
767500071122     c                   clear                   d98rcc
767600071122     c                   clear                   d98rri
767700071122     c                   clear                   d98rrc
767800071122     c                   clear                   d98fpi
767900071122     c                   clear                   d98fpc
768000071122     c                   clear                   d98fti
768100071122     c                   clear                   d98ftc
768200071122     c                   clear                   d98fci
768300071122     c                   clear                   d98fcc
768400071122     c                   clear                   d98fri
768500071122     c                   clear                   d98frc
768600071122     c                   clear                   d98npi
768700071122     c                   clear                   d98npc
768800071122     c                   clear                   d98nti
768900071122     c                   clear                   d98ntc
769000071122     c                   clear                   d98nci
769100071122     c                   clear                   d98ncc
769200071122     c                   clear                   d98nri
769300071122     c                   clear                   d98nrc
769400071122     c                   clear                   d98OCi
769500071122     c                   clear                   d98OCC
769600071122     c                   endif
769700930423     C*
769800100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
769900100308     c*  calcolo
770000100308     c                   eval      d98dcr=s98dcr
770100100308     c                   eval      d98hcr=s98hcr
770200100308     c                   eval      d98tcr=s98tcr
770300160405     c
770400160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
770500160405     c*  perchè il pgm usa il campo verificado se =10 )
770600160405     c                   if        wVP='S'
770700160405     c                   add       24            d98tci
770800160405     c                   add       24            d98tcc
770900160405     c                   add       24            d98tri
771000160405     c                   add       24            d98trc
771100160405     c                   endif
771200100308     c
771300930423     C                   ENDSR
771400941228     C***********************************************************************
771500941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
771600941228     C***********************************************************************
771700941228     C     W98999        BEGSR
771800941228     C     W98           IFLT      -999
771900941228     C                   Z-ADD     -999          D98               3 0
772000941228     C                   ELSE
772100941228     C     W98           IFGT      999
772200941228     C                   Z-ADD     999           D98
772300941228     C                   ELSE
772400941228     C                   Z-ADD     W98           D98
772500941228     C                   END
772600941228     C                   END
772700941228     C                   ENDSR
772800941228     C***********************************************************************
772900941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
773000941228     C***********************************************************************
773100941228     C     W98000        BEGSR
773200941228     C     W98           IFLE      *ZEROS
773300941228     C                   Z-ADD     *ZEROS        D98
773400941228     C                   ELSE
773500941228     C     W98           IFGT      999
773600941228     C                   Z-ADD     999           D98
773700941228     C                   ELSE
773800941228     C                   Z-ADD     W98           D98
773900941228     C                   END
774000941228     C                   END
774100941228     C                   ENDSR
774200930305     C***********************************************************************
774300930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
774400930305     C***********************************************************************
774500930305     C     AZZERA        BEGSR
774600150219     c* data e ora del momento
774700150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
774800150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
774900150219     C                   Z-ADD     DTORA4        WHHODI            4 0
775000930305     C*
775100141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
775200150603     C                   MOVEL     'N'           opendan
775300141107     c                   clear                   or_ddc
775400141108     c                   clear                   sav_12dat         8 0
775500141107     c                   clear                   or_datarif
775600140402     c                   clear                   wpick2            1
775700140402     c                   clear                   Primotipo
775800090928     c                   clear                   Primodev
775900090928     c                   clear                   Primohev
776000070827     c                   clear                   CallAR5
776100071122     c                   clear                   d98cbo
776200100308     c                   clear                   s98dcr
776300100308     c                   clear                   s98hcr
776400100308     c                   clear                   s98tcr
776500141106     c                   clear                   nosomma           1
776600141229     c                   clear                   wtrasm
776700150324     c                   clear                   avvcmc
776800150325     c                   clear                   DSbl4L
776900170925     c**                 clear                   dsbl4I
777000170925     c                   clear                   dpndflo
777100150401     c                   clear                   fnlv82ds
777200150331     c                   clear                   DS7R
777300150605     c                   clear                   wDEE              1
777400150629     c                   clear                   wtsogliaE
777500930323     C                   MOVEL     '0'           D98ERR
777600930323     C                   MOVEL     *BLANKS       D98MSG
777700931124     C                   Z-ADD     *ZEROS        D98TPI
777800931124     C                   Z-ADD     *ZEROS        D98TPC
777900930325     C                   Z-ADD     *ZEROS        D98TTI
778000930325     C                   Z-ADD     *ZEROS        D98TTC
778100930325     C                   Z-ADD     *ZEROS        D98TCI
778200930325     C                   Z-ADD     *ZEROS        D98TCC
778300090827     C                   Z-ADD     *ZEROS        w99TCI
778400090827     C                   Z-ADD     *ZEROS        w99TCC
778500930325     C                   Z-ADD     *ZEROS        D98TRI
778600930325     C                   Z-ADD     *ZEROS        D98TRC
778700090921     C                   Z-ADD     *ZEROS        w99TRI
778800090921     C                   Z-ADD     *ZEROS        w99TRC
778900931124     C                   Z-ADD     *ZEROS        D98RPI
779000931124     C                   Z-ADD     *ZEROS        D98RPC
779100930405     C                   Z-ADD     *ZEROS        D98RTI
779200930405     C                   Z-ADD     *ZEROS        D98RTC
779300930405     C                   Z-ADD     *ZEROS        D98RCI
779400930405     C                   Z-ADD     *ZEROS        D98RCC
779500930405     C                   Z-ADD     *ZEROS        D98RRI
779600930405     C                   Z-ADD     *ZEROS        D98RRC
779700931124     C                   Z-ADD     *ZEROS        D98FPI
779800931124     C                   Z-ADD     *ZEROS        D98FPC
779900930325     C                   Z-ADD     *ZEROS        D98FTI
780000930325     C                   Z-ADD     *ZEROS        D98FTC
780100930325     C                   Z-ADD     *ZEROS        D98FCI
780200930325     C                   Z-ADD     *ZEROS        D98FCC
780300930325     C                   Z-ADD     *ZEROS        D98FRI
780400930325     C                   Z-ADD     *ZEROS        D98FRC
780500931124     C                   MOVEL     *BLANKS       D98CPI
780600931124     C                   MOVEL     *BLANKS       D98CPC
780700930325     C                   MOVEL     *BLANKS       D98CTI
780800930325     C                   MOVEL     *BLANKS       D98CTC
780900930325     C                   MOVEL     *BLANKS       D98CCI
781000930325     C                   MOVEL     *BLANKS       D98CCC
781100930325     C                   MOVEL     *BLANKS       D98CRI
781200930325     C                   MOVEL     *BLANKS       D98CRC
781300931124     C                   Z-ADD     *ZEROS        D98NPI
781400931124     C                   Z-ADD     *ZEROS        D98NPC
781500930325     C                   Z-ADD     *ZEROS        D98NTI
781600930325     C                   Z-ADD     *ZEROS        D98NTC
781700930325     C                   Z-ADD     *ZEROS        D98NCI
781800930325     C                   Z-ADD     *ZEROS        D98NCC
781900930325     C                   Z-ADD     *ZEROS        D98NRI
782000930325     C                   Z-ADD     *ZEROS        D98NRC
782100930414     C                   Z-ADD     *ZEROS        D98HHI
782200930414     C                   Z-ADD     *ZEROS        D98HHC
782300930414     C                   Z-ADD     *ZEROS        D98HOI
782400930414     C                   Z-ADD     *ZEROS        D98HOC
782500930414     C                   Z-ADD     *ZEROS        D98HLI
782600930414     C                   Z-ADD     *ZEROS        D98HLC
782700930414     C                   Z-ADD     *ZEROS        D98HGI
782800930414     C                   Z-ADD     *ZEROS        D98HGC
782900931124     C                   Z-ADD     *ZEROS        D98HPI
783000931124     C                   Z-ADD     *ZEROS        D98HPC
783100930414     C                   Z-ADD     *ZEROS        D98HFI
783200930414     C                   Z-ADD     *ZEROS        D98HFC
783300930416     C                   Z-ADD     *ZEROS        D98HEI
783400930416     C                   Z-ADD     *ZEROS        D98HEC
783500930414     C                   Z-ADD     *ZEROS        D98HRI
783600930414     C                   Z-ADD     *ZEROS        D98HRC
783700930511     C                   Z-ADD     *ZEROS        D98HDI
783800930511     C                   Z-ADD     *ZEROS        D98HDC
783900930325     C                   Z-ADD     *ZEROS        D98DCI
784000930325     C                   Z-ADD     *ZEROS        D98HCI
784100930325     C                   Z-ADD     *ZEROS        D98DCE
784200930325     C                   Z-ADD     *ZEROS        D98HCE
784300931125     C                   Z-ADD     *ZEROS        D98OCI
784400931125     C                   Z-ADD     *ZEROS        D98OCC
784500931202     C                   Z-ADD     *ZEROS        D98DEI
784600931202     C                   Z-ADD     *ZEROS        D98OEI
784700931202     C                   Z-ADD     *ZEROS        D98DEE
784800931202     C                   Z-ADD     *ZEROS        D98OEE
784900151019     c                   CLEAR                   D98FLP
785000161107     c                   CLEAR                   D98Gfas
785100150325     C                   clear                   D98dtctd
785200150326     C                   clear                   D98picklim
785300150325     C                   clear                   D98spcdee
785400150331     C                   clear                   fddata
785500170208     C                   clear                   D98dam_rtc
785600140825     c
785700141017     c                   clear                   d98ddc
785800141017     c                   clear                   d98cons
785900160506     c                   clear                   d98disf
786000141017     c                   clear                   wd98ndc
786100140910     c                   clear                   wd98fbc
786200150119     c                   clear                   wd98acc
786300140825     c                   clear                   wtrulor
786400160401     c                   clear                   wVP               1
786500170208     c                   clear                   wsolodam
786600150219     c
786700150219     c                   clear                   IMPdata
786800150219     c                   clear                   IMPfgs
786900150219     c                   clear                   DISdata
787000150219     c                   clear                   DISfgs
787100150219     c                   clear                   DISora
787200150219     c                   clear                   DISnpg
787300150219     c                   clear                   IDnpg
787400150219     c                   clear                   IDdata
787500150219     c                   clear                   IDora
787600150219     c                   clear                   IDfgs
787700931125     C* CAMPI VARI
787800931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
787900140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
788000140825      *   DALLA LETTURA DI QUESTI FILE                               *
788100931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
788200931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
788300931125     C     D98NSP        IFNE      *ZEROS
788400930525     C                   MOVEL     *BLANKS       D98TSP
788500941228     C                   MOVEL     *BLANKS       D98CAD
788600970728     C                   MOVEL     *BLANKS       D98LOD
788700930525     C                   Z-ADD     *ZEROS        D98TFP
788800930525     C                   Z-ADD     *ZEROS        D98TFA
788900930525     C                   Z-ADD     *ZEROS        D98LNA
789000930525     C                   Z-ADD     *ZEROS        D98DSP
789100941228     C                   Z-ADD     *ZEROS        D98MGS
789200930525     C                   Z-ADD     *ZEROS        D98DTI
789300930525     C                   Z-ADD     *ZEROS        D98HTI
789400930525     C                   Z-ADD     *ZEROS        D98DCM
789500930525     C                   Z-ADD     *ZEROS        D98HMC
789600930525     C                   MOVEL     *BLANKS       D98GC1
789700930525     C                   MOVEL     *BLANKS       D98GC2
789800010820     C                   Z-ADD     *ZEROS        D98CAS
789900010821     C                   MOVEL     *BLANKS       D98VCA
790000930525     C                   MOVEL     *BLANKS       D98TIC
790100930525     C                   Z-ADD     *ZEROS        D98DCR
790200930525     C                   Z-ADD     *ZEROS        D98HCR
790300930525     C                   MOVEL     *BLANKS       D98TCR
790400050914     C                   MOVEL     *BLANKS       Consideraora      1
790500930525     C                   MOVEL     *BLANKS       D98CMC
790600170818     C                   clear                   wD98ntc_con       1 0
790700930525     C                   Z-ADD     *ZEROS        D98DAG
790800161215     C                   Z-ADD     *ZEROS        D98DCc
790900161215     C                   Z-ADD     *ZEROS        D98DAc
791000161215     C                   Z-ADD     *ZEROS        D98DCG
791100930525     C                   MOVEL     *BLANKS       D98FFD
791200930525     C                   Z-ADD     *ZEROS        D98NCD
791300930525     C                   MOVEL     *BLANKS       D98FNS
791400931124     C                   Z-ADD     *ZEROS        D98DAM
791500170105     C                   Z-ADD     *ZEROS        D98Dfv
791600940628     C                   MOVEL     *BLANKS       D98FIN
791700940628     C                   Z-ADD     *ZEROS        D98KSC
791800940628     C                   MOVEL     *BLANKS       D98TBL
791900931125     C                   END
792000960229     C     D98TLA        IFEQ      'T'
792100960229     C                   Z-ADD     *ZEROS        D98DTI
792200960229     C                   Z-ADD     *ZEROS        D98HTI
792300960229     C                   ENDIF
792400931125     C* CAMPI WORK
792500930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
792600930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
792700150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
792800930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
792900941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
793000960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
793100930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
793200930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
793300940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
793400930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
793500150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
793600931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
793700931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
793800150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
793900930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
794000930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
794100930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
794200930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
794300930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
794400940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
794500941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
794600930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
794700050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
794800050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
794900140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
795000930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
795100930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
795200930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
795300930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
795400930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
795500930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
795600930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
795700930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
795800930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
795900930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
796000930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
796100930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
796200930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
796300930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
796400140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
796500931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
796600060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
796700960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
796800931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
796900140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
797000140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
797100150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
797200140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
797300941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
797400970304     C******************** Z-ADD*ZEROS    W98DBR  80
797500970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
797600150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
797700050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
797800930414     C*
797900941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
798000941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
798100930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
798200930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
798300930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
798400930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
798500930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
798600930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
798700941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
798800941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
798900930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
799000930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
799100930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
799200930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
799300930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
799400930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
799500941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
799600941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
799700930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
799800930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
799900930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
800000930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
800100930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
800200930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
800300930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
800400930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
800500060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
800600060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
800700060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
800800060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
800900930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
801000930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
801100930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
801200930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
801300930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
801400930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
801500930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
801600930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
801700930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
801800930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
801900930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
802000930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
802100930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
802200930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
802300941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
802400941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
802500930423     C*
802600930511     C                   Z-ADD     *ZEROS        W98HF             5 0
802700930423     C                   Z-ADD     *ZEROS        W98HH             5 0
802800060418     C                   Z-ADD     *ZEROS        W98EG             5 0
802900060420     C                   Z-ADD     *ZEROS        W98EA             5 0
803000930423     C                   Z-ADD     *ZEROS        W98HG             5 0
803100930423     C                   Z-ADD     *ZEROS        W98HL             5 0
803200930423     C                   Z-ADD     *ZEROS        W98HO             5 0
803300930511     C                   Z-ADD     *ZEROS        W98HD             5 0
803400930611     C*
803500930611     C                   Z-ADD     *ZEROS        WORELA            3 0
803600960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
803700930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
803800930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
803900931207     C                   Z-ADD     *ZEROS        WORECR            3 0
804000090902     c
804100090902     c                   clear                   wmancaTRAZ
804200150113     c                   clear                   wcaldpcee         1
804300150220     c                   clear                   evemic            2
804400930305     C*
804500060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
804600060406     C                   clear                   LAF                            *LASC.AVVISO FINE
804700091014     C                   clear                   LAp                            *posizione l.avviso
804800060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
804900060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
805000060406c    c                   clear                   LCon
805100060406     C                   clear                   EVD                            *DATA EVENTO
805200060406     C                   clear                   EVO                            *ORA EVENTO
805300060406     C                   clear                   EVC                            *CAUSALE EVENTO
805400131218     C                   clear                   EVT
805500060406     C                   clear                   pVD                            *DATA EVENTO
805600060406     C                   clear                   pVO                            *ORA EVENTO
805700060406     C                   clear                   pVC                            *CAUSALE EVENTO
805800060406     C                   clear                   pVF                            *CAUSALE EVENTO
805900060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
806000060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
806100060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
806200060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
806300060406     C                   clear                   COV                            *DATE FINE L.A. E GI
806400060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
806500060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
806600091014     C                   clear                   GIp                            *posizione vento gia
806700060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
806800060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
806900060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
807000931213     C*
807100060406     C                   clear                   TUC                            *TURNI CHIUSURA
807200931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
807300931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
807400060406     c
807500060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
807600060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
807700060406     c                   clear                   pgii
807800060406     c                   clear                   pgif
807900060406     c                   clear                   pgci
808000060406     c                   clear                   pgcf
808100060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
808200060420     c                   clear                   paii
808300060420     c                   clear                   paif
808400930420     C*
808500930305     C                   ENDSR
808600930512     C***********************************************************************
808700930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
808800930512     C***********************************************************************
808900930512     C     FORCON        BEGSR
809000930512     C*
809100931124     C                   Z-ADD     *ZEROS        D98RPI
809200931124     C                   Z-ADD     *ZEROS        D98RPC
809300930512     C                   Z-ADD     *ZEROS        D98RTI
809400930512     C                   Z-ADD     *ZEROS        D98RTC
809500930512     C                   Z-ADD     *ZEROS        D98RCI
809600930512     C                   Z-ADD     *ZEROS        D98RCC
809700930512     C                   Z-ADD     *ZEROS        D98RRI
809800930512     C                   Z-ADD     *ZEROS        D98RRC
809900931124     C                   Z-ADD     *ZEROS        D98FPI
810000931124     C                   Z-ADD     *ZEROS        D98FPC
810100930512     C                   Z-ADD     *ZEROS        D98FTI
810200930512     C                   Z-ADD     *ZEROS        D98FTC
810300930512     C                   Z-ADD     *ZEROS        D98FCI
810400930512     C                   Z-ADD     *ZEROS        D98FCC
810500930512     C                   Z-ADD     *ZEROS        D98FRI
810600930512     C                   Z-ADD     *ZEROS        D98FRC
810700931124     C                   MOVEL     'S'           D98CPI
810800931124     C                   MOVEL     'S'           D98CPC
810900930512     C                   MOVEL     'S'           D98CTI
811000930512     C                   MOVEL     'S'           D98CTC
811100930512     C                   MOVEL     'S'           D98CCI
811200930512     C                   MOVEL     'S'           D98CCC
811300930512     C                   MOVEL     'S'           D98CRI
811400930512     C                   MOVEL     'S'           D98CRC
811500931124     C                   Z-ADD     *ZEROS        D98NPI
811600931124     C                   Z-ADD     *ZEROS        D98NPC
811700930512     C                   Z-ADD     *ZEROS        D98NTI
811800930512     C                   Z-ADD     *ZEROS        D98NTC
811900930512     C                   Z-ADD     *ZEROS        D98NCI
812000930512     C                   Z-ADD     *ZEROS        D98NCC
812100930512     C                   Z-ADD     *ZEROS        D98NRI
812200930512     C                   Z-ADD     *ZEROS        D98NRC
812300930512     C                   Z-ADD     *ZEROS        D98HHI
812400930512     C                   Z-ADD     *ZEROS        D98HHC
812500930512     C                   Z-ADD     *ZEROS        D98HOI
812600930512     C                   Z-ADD     *ZEROS        D98HOC
812700930512     C                   Z-ADD     *ZEROS        D98HLI
812800930512     C                   Z-ADD     *ZEROS        D98HLC
812900930512     C                   Z-ADD     *ZEROS        D98HGI
813000930512     C                   Z-ADD     *ZEROS        D98HGC
813100931124     C                   Z-ADD     *ZEROS        D98HPI
813200931124     C                   Z-ADD     *ZEROS        D98HPC
813300930512     C                   Z-ADD     *ZEROS        D98HFI
813400930512     C                   Z-ADD     *ZEROS        D98HFC
813500930512     C                   Z-ADD     *ZEROS        D98HEI
813600930512     C                   Z-ADD     *ZEROS        D98HEC
813700930512     C                   Z-ADD     *ZEROS        D98HRI
813800930512     C                   Z-ADD     *ZEROS        D98HRC
813900930512     C                   Z-ADD     *ZEROS        D98HDI
814000930512     C                   Z-ADD     *ZEROS        D98HDC
814100930520     C*
814200930512     C                   ENDSR
814300930305     C********************************************************************
814400971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
814500950406031059090120151181212243273304334365
814600950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
814700950406312831303130313130313031
814800930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
814900971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
815000971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
815100971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
815200971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
815300971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
815400971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
815500971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
815600971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
815700971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
815800971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
815900971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
816000971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
816100971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
816200971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
816300971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
816400971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
816500971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
816600971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
816700971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
816800971127TNSD99R- Bolla inesistente.                                               BOIN20
816900971127TNSD99R-     Cap inesistente.                                             CAPI21
817000971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
817100971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
817200971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
817300971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
817400971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
817500971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
817600971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
817700971127TNSD99R- Contrassegno non trovato.                                        CONT29
817800971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
817900990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
818000990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
818100060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
818200060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
818300971127TNSD99R- 35                                                                   35
818400971127TNSD99R- 36                                                                   36
818500971127TNSD99R- 37                                                                   37
818600971127TNSD99R- 38                                                                   38
818700971127TNSD99R- 39                                                                   39
818800971127TNSD99R- 40                                                                   40
