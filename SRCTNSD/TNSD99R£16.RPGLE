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
005200950406     D*****************************************************************
005300950406     D* SCHIERE PER ROUTINE GIORNO>DATA E DATA>GIORNO
005400950406     D GPM             S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND     GG. PROGR. MESI
005500950406     D MES             S              2  0 DIM(12) CTDATA PERRCD(12)            GG. DEL MESE
005600930304     D*****************************************************************
005700930304     D* MESSAGGI
005800941227     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
005900950406     D*****************************************************************
006000950406     D* SCHIERE DI LAVORO PER ROUTINE GIORNO>DATA E DATA>GIORNO
006100950406     D $PM             S              3  0 DIM(12) ASCEND                       GG. PROGR. MESI
006200950406     D $GM             S              2  0 DIM(12)                              GG. DEL MESE
006300941227     D*****************************************************************
006400941227     D* SCHIERA TABELLA CODICI BOLLA -> TIPO BOLLA (FORMATO SEDE)
006500941228     D C3A             S              2    DIM(50)                              *KEY
006600941227     D T3A             S              2    DIM(50)                              *TIPO BOLLA
006700941228     D F3A             S              1  0 DIM(50)                              *FLAG CONTRASSEGNO
006800990308     D R3A             S              1    DIM(50)                              *RAGGRUPP.BOLLE
006900930316     D*****************************************************************
007000930405     D* SCHIERA TABELLA TEMPI FISSI DI DELIVERY (1X)  (PER FILIALE)
007100080513     d c_dim_1X        c                   const(500)
007200080513     D C1X             S              8    DIM(c_dim_1X)                        *KEY
007300080513     D G1X             S              3  0 DIM(c_dim_1X)                        *TEMPO GIORNO
007400080513     D O1X             S              4  0 DIM(c_dim_1X)                        *ORA SOGLIA
007500080513     D M1X             S              3  0 DIM(c_dim_1X)                        *TEMPO MATTINO
007600080513     D P1X             S              3  0 DIM(c_dim_1X)                        *TEMPO POMERIGGIO
007700080513     D I1X             S              3  0 DIM(c_dim_1X)                        *ORE INF.CONFORM.
007800080513     D S1X             S              3  0 DIM(c_dim_1X)                        *ORE SUP.CONFORM.
007900930405     D*****************************************************************
008000930405     D* SCHIERA TABELLA SIMBOLI CALENDARIO (1Y)
008100930421     D C1Y             S              8    DIM(30)                              *KEY
008200930421     D I1Y             S              1    DIM(30)                              *FEST.INTERNA ?
008300930421     D E1Y             S              1    DIM(30)                              *FEST.CLIENTE ?
008400930421     D*****************************************************************
008500930421     D* SCHIERA TABELLA CAUSALI EVENTI BOLLE (2A)
008600060403     D C2A             S              3    DIM(199)                             *KEY
008700060406     D D2A             S              3    DIM(199)                             *RECORD
008800060418     D D2Z             S              5    DIM(199)                             *RECORD
008900031201     D*****************************************************************
009000031201     D* SCHIERA TABELLA CODICI GESTIONE PARTICOLARITA' GIACENZE (7Q) + SUA ESTENSIONE (7W)
009100060403     D SK7QW           S              2    DIM(199)
009200930421     D*****************************************************************
009300060406     D* SCHIERA EVENTI BOLLE di l.avviso e giacenza
009400060403     D EVD             S              8  0 DIM(199)                             *DATA EVENTO
009500060403     D EVO             S              4  0 DIM(199)                             *ORA EVENTO
009600060403     D EVC             S              3    DIM(199)                             *CAUSALE EVENTO
009700131218     D EVT             S              1    DIM(199)                             *tipo G/A
009800031125     D*****************************************************************
009900031125     D* SCHIERA GIACENZE BOLLE
010000031125     D GCPD            S              8  0 DIM(99)                              *DATA GIACENZA
010100930421     D*****************************************************************
010200930421     D* SCHIERA RANGE DATE INIZIO/FINE PER LASCIATO AVVISO
010300981029     D LAI             S              9  0 DIM(99)                              *INIZIO
010400981029     D LAF             S              9  0 DIM(99)                              *FINE
010500091014     D LAp             S              3  0 DIM(99)                              *FINE
010600981029£$   D LCI             S              9  0 DIM(99)                              *INIZ X CONSEGNABILI
010700981029     D LCF             S              9  0 DIM(99)                              *FINE X CONSEGNABILI
010800050520     D LCon            S              1    DIM(99)                              *FINE X CONSEGNABILI
010900960129$$£  D*****************************************************************
011000960129$$£  D* SCHIERA RANGE DATE INIZIO/FINE PER EVENTI GIACENZA
011100981029$$£  D GII             S              9  0 DIM(99)                              *INIZIO
011200981029$$£  D GIF             S              9  0 DIM(99)                              *FINE
011300091014$$£  D GIp             S              3  0 DIM(99)                              *FINE
011400981029$$£  D GCI             S              9  0 DIM(99)                              *INIZ X CONSEGNABILI
011500981029$$£  D GCF             S              9  0 DIM(99)                              *FINE X CONSEGNABILI
011600931213     D*****************************************************************
011700931213     D* SCHIERA TURNI CHIUSURA
011800931213     D TUC             S              2    DIM(4)                               *TURNI CHIUSURA
011900931201     D*****************************************************************
012000931201     D* SCHIERA RANGE DATE INIZIO/FINE PER TURNO CHIUSURA
012100981029     D TII             S              9  0 DIM(99)                              *INTERNO/INIZIO
012200981029     D TIF             S              9  0 DIM(99)                              *INTERNO/FINE
012300981029     D TCI             S              9  0 DIM(99)                              *CLIENTE/INIZIO
012400981029     D TCF             S              9  0 DIM(99)                              *CLIENTE/FINE
012500060406     D*****************************************************************
012600060406     D* SCHIERA EVENTI particolari bolle
012700060406     D pVD             S              8  0 DIM(199)                             *DATA EVENTO
012800060406     D pVO             S              4  0 DIM(199)                             *ORA EVENTO
012900060406     D pVC             S              3    DIM(199)                             *CAUSALE EVENTO
013000060406     D pVF             S              1    DIM(199)                             *CAUSALE EVENTO
013100060406     D* SCHIERA RANGE DATE INIZIO/FINE PER Eventi particolari bolle "G"
013200060406     d*  che  Giustificano l'affidabilità
013300060406     D pGII            S              9  0 DIM(99)                              *INTERNO/INIZIO
013400060406     D pGIF            S              9  0 DIM(99)                              *INTERNO/FINE
013500060406     D pGCI            S              9  0 DIM(99)                              *CLIENTE/INIZIO
013600060406     D pGCF            S              9  0 DIM(99)                              *CLIENTE/FINE
013700060418     d*  che  Allungano    l'affidabilità
013800060418     D pAII            S              9  0 DIM(99)                              *INTERNO/INIZIO
013900060418     D pAIF            S              9  0 DIM(99)                              *INTERNO/FINE
014000060418     D pACI            S              9  0 DIM(99)                              *CLIENTE/INIZIO
014100060418     D pACF            S              9  0 DIM(99)                              *CLIENTE/FINE
014200931207     D*****************************************************************
014300931207     D* SCHIERA DATE PER CALCOLO ORE VALIDE TRA DUE DATE (DATE FINE L.A. E GIAC)
014400981029     D COV             S              9  0 DIM(99)
014500930325     D*****************************************************************
014600930405     D* SCHIERA GIORNI SETTIMANA
014700930405     D*****************************************************************
014800930405     D* SCHIERA GIORNI FESTIVI
014900930517     D*****************************************************************
015000971113     D* SCHIERA RECORD DEL CALENDARIO PIU' UTILIZZATI
015100050523     D CAK             S             12    DIM(600)                             *KEY CALENDARIO
015200050523     D CAM             S             31    DIM(600)                             *MATTINA
015300050523     D CAP             S             31    DIM(600)                             *POMERIGGIO
015400941227     D*****************************************************************
015500941227     D* SCHIERA POSSIBILI COMBINAZIONI LINEE/TERMINAL
015600941227     D LTP             S              3  0 DIM(4)
015700941227     D LTA             S              3  0 DIM(4)
015800971210     D*****************************************************************
015900971210     D* INDICE=COD.FILIALE ,   FL1 => BLANK=ITALIA,'E'=ESTERO
016000971210     D FL1             S              1    DIM(999)                             *FLAG ITALIA/ESTERO
016100020226     D*****************************************************************
016200160331     D* SCHIERA FILIALI FEDEX   e DPD
016300060406     D FILFED          S              3  0 DIM(99)
016400160331     D FILDPD          S              3  0 DIM(99)
016500970701     D*****************************************************************
016600970701     D* PARAMETRI CHIAMATA PGM TISI95R (CONTROLLO CAP)
016700970701     D SI95          E DS                  EXTNAME(TISI95DS)
016800930325     D*****************************************************************
016900930405     D* DS PER RIDEFINIRE SCHIERA GIORNI SETTIMANA
017000930405     D GSEDS           DS
017100930405     D  GSE                    1     31
017200930405     D                                     DIM(31)
017300930405     D*****************************************************************
017400930405     D* DS PER RIDEFINIRE SCHIERA GIORNI FESTIVI
017500930405     D GFEDS           DS
017600930405     D  GFE                    1     31
017700930405     D                                     DIM(31)
017800150325     D*****************************************************************
017900150325     D* DS per record L del FIAR4
018000150325     D DSbl4L        E DS
018100160331     d
018200160331     D DSbl4I        E DS
018300930323     D*****************************************************************
018400930323     D* DS ESTERNA IN INPUT AL PGM
018500970731     D DS99          E DS                  EXTNAME(TNSD99DS)
018600930323     D*****************************************************************
018700930323     D* DS ESTERNA TABELLA 1X TEMPI FISSI DI DELIVERY
018800930326     D DS1X          E DS
018900930405     D*****************************************************************
019000971113     D* DS ESTERNA TABELLA 1Y SIMBOLI CALENDARIO
019100930405     D DS1Y          E DS
019200941227     D*****************************************************************
019300941227     D* DS ESTERNA TABELLA 3A (CODICI BOLLA)
019400941227     D DS3A          E DS
019500930421     D*****************************************************************
019600060406     D* DS ESTERNA TABELLA 2A CAUSALI EVENTI BOLLE
019700060406     D DS2A          E DS                  prefix(W:1)
019800020226     D*****************************************************************
019900020226     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
020000020226     D OG143         E DS
020100020702     D*****************************************************************
020200020702     D* DS PER LETTURA TABELLA "NTW"
020300020702     D DNTW          E DS
020400150326     D* DS PER LETTURA TABELLA "VPO" flag operativi
020500150326     D Dvpo          E DS
020600031201     D*****************************************************************
020700150331     D* DS ESTERNA TABELLA 7Q/7W/7R
020800031201     D DS7Q          E DS
020900031201     D DS7W          E DS
021000150331     D DS7R          E DS
021100070514     D*****************************************************************
021200140825     D* DS per reperimento orari di consegna
021300140825     D TRULORSDS     E DS
021400140825     D TRULOR2DS     E DS
021500150331     D TRULOR3DS     E DS
021600140825     D*****************************************************************
021700150219     D FNLV53DS      E DS
021800150219     D FNLV55DS      E DS
021900150401     D*****************************************************************
022000150401     D fnlv82ds      E DS
022100150219     D*****************************************************************
022200060406     D usa2a           DS
022300060406     D  §2aftc                 1      1
022400060406     D  §2agla                 2      2  0
022500060406     D  §2aFTG                 3      3
022600060406     D usa2Z           DS
022700060406     D  §2Zcep                 1      1
022800060406     D  §2Znep                 2      3
022900060406     D  §2Zdep                 4      4
023000060406     D  §2Zfep                 5      5
023100930517     D***************************************************************
023200971113     D* DS PER SCOMPORRE CHIAVE CALENDARIO
023300930517     D                 DS
023400941227     D  DCATFP                 1      3
023500941227     D  DCATFA                 4      6
023600941227     D  DCAANN                 7     10
023700941227     D  DCAMES                11     12
023800941227     D  DCALEN                 1     12
023900950406     D***************************************************************
024000950406     D* DS PER SCOMPORRE WDAT IN AAAA/MM/GG
024100950406     D                 DS
024200950406     D  WDAT                   1      8  0
024300950406     D  WAA                    1      4  0
024400950406     D  WMM                    5      6  0
024500950406     D  WGG                    7      8  0
024600970304     D***************************************************************
024700970304     D* DS PER CONFRONTI TRA DATE
024800970304     D                 DS
024900970304     D  WDO1                   1     12  0
025000970304     D  WD1                    1      8  0
025100970304     D  WO1                    9     12  0
025200970304     D                 DS
025300970304     D  WDO2                   1     12  0
025400970304     D  WD2                    1      8  0
025500970304     D  WO2                    9     12  0
025600150330     D***************************************************************
025700150330     D                 DS
025800150330     D  who                    1      2  0
025900150330     D  wmn                    3      4  0
026000150330     D  WORA                   1      4  0
026100941227     D***************************************************************
026200941227     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
026300941227     D DS§DAT          DS
026400941227     D  DSSEC                  1      2  0
026500941227     D  DSANN                  3      4  0
026600941227     D  DSMES                  5      6  0
026700941227     D  DSGIO                  7      8  0
026800941227     D  DSAAS                  1      4  0
026900941228     D  DSMGS                  5      8  0
027000941227     D  DSDAT6                 3      8  0
027100941227     D  DSDAT8                 1      8  0
027200941227     D DS§TIM          DS
027300941227     D  DTORA                  1      2  0
027400941227     D  DTMIN                  3      4  0
027500941227     D  DTSCN                  5      6  0
027600941227     D  DTGIO                  7      8  0
027700941227     D  DTMES                  9     10  0
027800941227     D  DTSEC                 11     12  0
027900941227     D  DTANN                 13     14  0
028000941227     D  DTINV                  7     14  0
028100941227     D  DTORA6                 1      6  0
028200941227     D  DTORA4                 1      4  0
028300941227     D  DTTIME                 1     14  0
028400010821     D***************************************************************
028500050523     D kpjba         E DS
028600010821     D*  CAMBIO DIVISA
028700010821     D YEURDS        E DS                  EXTNAME(YEURCODS)
028800010821     D***************************************************************
028900010821     D*  REPERIMENTO DIVISE DI CONTO
029000010821     D DGED          E DS
029100070827     D*------------------
029200070827     D DAR5GEN       E DS
029300011211     D*------------------
029400011211     D* DS REPERIMENTO IMPORTO MASSIMO CONTRASSEGNO
029500011211     D*------------------
029600011211     D TRUL21DS      E DS
029700050523     d
029800050523     d* DS x controllo riempimento schiera
029900050523     D TRUL0sDS      E DS
030000040809     D*------------------
030100040809     D* ds per il controllo della presenza di CA per la spedizione richiesta
030200040809     D*------------------
030300040809     d FIDN12DS      e ds
030400040809     d  skKey                 26    305    dim(20)
030500040809     d  skAnn                306    325    dim(20)
030600040809     d  skDca                326    485  0 dim(20)
030700040809     d  skFca                486    545  0 dim(20)
030800040809     d  skDch                546    705  0 dim(20)
030900040809     d  skCch                706    745    dim(20)
031000040809     d*
031100040809     d dsKey           ds
031200040809     d  dsaac                         4  0
031300040809     d  dsfil                         3  0
031400040809     d  dsnca                         7  0
031500041028     d wesarb          s              1
031600141216     d wtrasm          s              1
031700041116     d wapri           s              1    inz(' ')
031800050908     d Usacons         s              1    inz(' ')
031900050405     D*------------------
032000050405     D* VARIABILI D WRK RIFERITE AL DATA BASE
032100050405     D*------------------
032200050405     D KWGCPAAS        s                   like(GCPAAS)
032300050405     D KWGCPLNP        s                   like(GCPLNP)
032400050405     D KWGCPNRS        s                   like(GCPNRS)
032500050405     D KWGCPNSP        s                   like(GCPNSP)
032600050405     D KWGCPFRG        s                   like(GCPFRG)
032700050901     d
032800050901     D salvaoreHMC     s                   like(wr3hho)
032900050901     D salvaoreHCR     s                   like(wr3hho)
033000050907     D oreILoDOPO      s                   like(worecr)
033100060526     D wpodata         s                   like(w12dat)
033200070514     D wdata8          s                   like(w12dat)
033300060406     D w003a           s              3
033400060406     D w11poi          s              9  0
033500150217     D picklim         s              4  0
033600060420     D wgiroepa        s              1
033700140911     D wtuctut         s              1    inz
033800060420     D wfine           s              1
033900070827     D CallAR5         s              1    inz
034000140825     D Wtrulor         s              1    inz
034100070827     d kAr5Trd         s                   Like(Ar5Trd) inz('GEN')
034200150325     d kAr4Trc         s                   Like(Ar4Trc) inz('L')
034300071122     d d98cbo          s                   Like(arbcbo)
034400090827     d w99tci          s                   Like(d98tci)
034500090827     d w99tcc          s                   Like(d98tcc)
034600090921     d w99tri          s                   Like(d98tri)
034700090921     d w99trc          s                   Like(d98trc)
034800090928     d Primotipo       s                   Like(§2aftc)
034900090928     d Primodev        s                   Like(evbdev)
035000090928     d Primohev        s                   Like(evbhev)
035100091014     d SalvaEveL       s              3  0
035200091014     d SalvaEveG       s              3  0
035300140911     d EX              s              3  0
035400150325     d II              s              3  0
035500150630     d wdiff           s              9  0
035600100308     d s98dcr          s                   Like(d98dcr)
035700100308     d s98hcr          s                   Like(d98hcr)
035800100308     d s98tcr          s                   Like(d98tcr)
035900140825     d wd98ndc         s                   Like(arbndc)
036000140911     d wd98fbc         s                   Like(arbfbc)
036100150119     d wd98acc         s                   Like(arbacc)
036200150219     d
036300150219     d IMPdata         s                   like(d53dfv)
036400150219     d IMPfgs          s                   like(brvfgs)
036500150219     d DISfgs          s                   like(brvfgs)
036600150219     d DISdata         s                   like(d53dfv)
036700150219     d DISnpg          s                   like(brvnpg)
036800150219     d DISora          s                   like(brvhcs)
036900150219     d IDdata          s                   like(d53dfv)
037000150219     d IDora           s                   like(brvhcs)
037100150219     d IDfgs           s                   like(brvfgs)
037200150219     d IDnpg           s              1
037300150325     D kpdpdr          S                   LIKE(APDpdr)
037400150325     D kpdtip          S                   LIKE(APDtip)
037500170208     d wsolodam        s              1
037600140825     d
037700140825     D wrkoggiiso      s               d
037800930304     C*****************************************************************
037900930323     C*/ INIZIO MAIN DEL PGM
038000170208     C* ESEGUE ROUTINE wdaodiLE
038100930304     C                   EXSR      RUTINZ
038200930528     C*
038300930528     C     D98ERR        IFEQ      '0'                                          --- 1 -->
038400930602     C*
038500930602     C* SE LANCIATO CON TIPO LANCIO='C' (CHIUDI FILE) VADO A FINE PGM
038600930602     C     D98TLA        IFNE      'C'                                          --- 2 -->
038700960729     C*
038800960729     C* CONTROLLO SE MI HANNO LANCIATO PER DETERMINARE LA CONSEGNABILITA'
038900960729     C                   MOVEL     'N'           FLCONS            1            *FLAG ESISTE CONSEGN
039000960729     C     D98DIC        IFGT      *ZEROS
039100960729     C     D98DFC        ORGT      *ZEROS
039200960729     C                   MOVEL     'S'           FLCONS                         *SI ESISTE CONSEGNAB
039300960729     C                   ENDIF
039400930323     C*
039500930511     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
039600931125     C* SE NON E' STATA IMPOSTATA DS DAL PGM CHIAMANTE (NSP=ZERO)
039700930420     C     D98ERR        IFEQ      '0'
039800010821     C     D98NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
039900930415     C     D98TBO        IFEQ      'A'
040000941227     C                   EXSR      LETARB                                       *LEGGE BOLLA ARRIVO
040100930323     C                   ELSE
040200941227     C                   EXSR      LETBLP                                       *LEGGE BOLLA PART.
040300930323     C                   END
040400140402     c
040500150401     c* Leggo fiar5 recod GEN  e FIAR4
040600140402     c                   exsr      leggiar5
040700140402     c
040800931206     C                   ELSE
040900931206     C* DS IMPOSTATA DAL PGM CHIAMANTE IMPOSTO CAMPI LAVORO
041000931206     C                   Z-ADD     D98DCG        W98DCG
041100990308     C                   MOVEL     *BLANKS       WRBL              1            *RAGGRUPP.BOLLE
041200990308     C                   MOVEL     *BLANKS       WFBR              1            *BOLLA DI RESO
041300031201     C                   MOVEL     *BLANKS       WGGA              2            *PARTICOLARIT' GIAC.
041400140822     C                   Z-ADD     d98DCR        s98DCR
041500140822     C                   Z-ADD     d98HCR        s98HCR
041600140822     C                   MOVEL     d98TCR        s98TCR
041700141104     c                   eval      gc1part=d98gc1
041800141104     c                   eval      gc2part=d98gc2
041900150303     c* imposto flag trasmissione bolla = 'N'
042000150303     c                   eval      wtrasm='N'
042100931125     C                   END
042200930420     C                   END
042300930528     C*
042400010821     C     D98ERR        IFEQ      '0'                                          --- 3 -->
042500160420
042600160420     c* se bolla in partenza, devo ricostruire il record "L"
042700160420    2c                   if        wtrasm='N'
042800160420    3c                   if        d98tc1 = 'A' or d98tc1 = 'S' or d98tc2 = 'A'
042900160420     c                             or d98tc2 = 'S'
043000160420     c                   eval      §b4lbas = 'S'
043100160420      * se consegna tassativa sblocco
043200160420    4c                   if        d98dcr <> *zeros and d98tcr = *blanks
043300160420     c                   clear                   §b4lbas
043400160420    4c                   endif
043500160420    3c                   endif
043600160420
043700160420    3c                   if        d98ffd<>' '
043800170206
043900160420     c                   eval      §b4ltfd = 'S'
044000170206     c* se Fermo deposito tolgo il contatto per l'appuntamento
044100170206    4c                   if        §b4lbas = 'S' and
044200170206     c                             (d98tc1 = 'A' or d98tc2= 'A')
044300170206     c                   clear                   §b4lbas
044400170206    4c                   endif
044500160420    3c                   endif
044600160420
044700160420    2c                   endif
044800160420
044900050908     c* Se esiste la data consegnabilità, faccio finta che la bolla
045000050908     c*  non si consegnata, azzerando proprio la dta di consegna
045100050908     c*  perchè può trarre in inganno
045200050908     c                   if        d98dfc>0 and d98dcm>0
045300050908     c                   clear                   d98dcm
045400050908     c                   clear                   d98hmc
045500050908     c                   clear                   d98cca
045600050908     c                   endif
045700090902     c*
045800090902     c* Se non c'e' nè data consegna, ne data arrivo trazione
045900090902     c*  faccio finta che data arrivo trazione= udate utime ai fini dei calcoli
046000090902     c* in fondo al pgm dovrò azzerare i tempi di consegna interni e cliente
046100090902     c* perchè non calcolabili
046200090902     c                   clear                   wmancaTRAZ        1
046300090902     c
046400010821     C*
046500010821     C* EFFETTUO SUBITO CONSIDERAZIONI SU DIVISE (X QUESTIONE EURO)
046600010821     C                   EXSR      CHKVCA
046700141204     c
046800141204     c* Controllo se ha pratica di DANNO aperta
046900141204     c                   EXSR      CHKDAN
047000010108     C*
047100010108     C* SE BOLLA POSTA, UTILIZZA LA DATA BORDERO' ANZICHE' LA DATA SPEDIZIONE
047200091009     C**   D98TSP        IFEQ      'P'                                          *BOLLA POSTA
047300091009     C**   D98DBR        IFGT      *ZEROS                                       *DATA BORDERO' PRESE
047400091009     C**                 Z-ADD     D98DBR        D98DSP                         *DATA BORDERO' --> D
047500091009     C**                 ENDIF
047600091009     C**                 ENDIF
047700930304     C*
047800950110     C* RECUPERO I TEMPI TEORICI DI PARTENZA, TRAZIONE E DI CONSEGNA
047900930420     C     D98ERR        IFEQ      '0'
048000930323     C                   EXSR      RTTEOR
048100930420     C                   END
048200930528     C*
048300930602     C     D98ERR        IFEQ      '0'                                          --- 4 -->
048400970304     C*
048500970304     C* SE RICHIESTO LANCIO CON DATA RIFERIMENTO CONTROLLO LE DATE DA AZZERARE
048600970304     C     D98ERR        IFEQ      '0'
048700970304     C                   EXSR      DATRIF
048800970304     C                   END
048900940901     C*
049000940901     C* TRASF. DATA ARRIVO TRAZIONE E RICONSIDERO TEMPO CONSEGNA SE RITARDO
049100940901     C     D98ERR        IFEQ      '0'
049200940901     C                   EXSR      TDTIPO
049300940901     C                   END
049400930420     C*
049500930511     C* CARICO IL FILE DEGLI EVENTI BOLLA
049600930420     C     D98ERR        IFEQ      '0'
049700041116     c     d98nsp        andgt     0
049800930420     C                   EXSR      CAREVB
049900930420     C                   END
050000931213     C*
050100931213     C* CARICO LA SCHIERA DEI TURNI DI CHIUSURA
050200931213     C     D98ERR        IFEQ      '0'
050300931213     C                   EXSR      CARTUC
050400931213     C                   END
050500930528     C*
050600930602     C     D98ERR        IFEQ      '0'                                          --- 5 -->
050700930405     C*
050800940901     C* TRASFORMO LE DATE (SPED,CONS,ECC.) IN PROGRESSIVI ORE
050900930420     C     D98ERR        IFEQ      '0'
051000930405     C                   EXSR      TDATPO
051100930420     C                   END
051200930528     C*
051300930602     C     D98ERR        IFEQ      '0'                                          --- 6 -->
051400950110     C*
051500950110     C* CALCOLA LE DATE DI PREVISTA PARTENZA DELL'ULTIMO COLLO
051600950110     C     D98ERR        IFEQ      '0'
051700950110     C                   EXSR      CALDPP
051800950110     C                   END
051900930405     C*
052000930511     C* CALCOLA LE DATE DI PREVISTO ARRIVO DELLA TRAZIONE
052100930420     C     D98ERR        IFEQ      '0'
052200930511     C                   EXSR      CALDPT
052300090902     c*
052400090902     c* Ricalcolo le date facendo finta che la trazione stia arrivando
052500090903     c*  in questo momento se non presente
052600090902     c                   if        ricalcoloTRAZ='S'
052700090902     c                   eval      d98dti=WDAODI
052800090902     c                   eval      d98hti=WHHODI
052900090902     c                   eval      wmancaTRAZ='S'
053000090902     c
053100090902     C                   EXSR      TDTIPO
053200090902     C                   EXSR      CALDPP
053300090902     C                   EXSR      CALDPT
053400090902     c                   endif
053500090902     c
053600930420     C                   END
053700960229     C*
053800091009     d* D98TLA = 'T' --> in output solo i tempi di trazione
053900960229     C     D98TLA        IFNE      'T'                                          --- 6A ->
054000060407     C*
054100931206     C* CALCOLA IL RANGE PER IL CONTRASSEGNO
054200931206     C     D98ERR        IFEQ      '0'
054300931206     C                   EXSR      CALASS
054400931206     C                   END
054500931206     C*
054600931206     C* CALCOLA IL RANGE PER IL FERMO DEPOSITO
054700931206     C     D98ERR        IFEQ      '0'
054800931206     C                   EXSR      CALFED
054900931206     C                   END
055000931206     C*
055100931206     C* CALCOLA LE DATE TEORICHE DI CONSEGNA
055200931206     C     D98ERR        IFEQ      '0'
055300931206     C                   EXSR      CALDTE
055400931206     C                   END
055500930511     C*
055600930511     C* CALCOLA IL RANGE PER DATA CONSEGNA RICHIESTA
055700930511     C     D98ERR        IFEQ      '0'
055800930511     C                   EXSR      CALDCR
055900930511     C                   END
056000930511     C*
056100930511     C* CALCOLA I RANGE PER LASCIATO AVVISO
056200930511     C     D98ERR        IFEQ      '0'
056300930511     C                   EXSR      CALLAV
056400930511     C                   END
056500930511     C*
056600930511     C* CALCOLA IL RANGE PER LA GIACENZA
056700930511     C     D98ERR        IFEQ      '0'
056800930511     C                   EXSR      CALGIA
056900930511     C                   END
057000060420     C* CALCOLA IL RANGE PER Eventi Particolari che Allungano
057100060420     C     D98ERR        IFEQ      '0'
057200060420     C                   EXSR      CALcepa
057300060420     C                   END
057400930511     C*
057500930511     C* CALCOLA LE DATE DI PREVISTA CONSEGNA
057600930511     C     D98ERR        IFEQ      '0'
057700930511     C                   EXSR      CALDPC
057800930511     C                   END
057900960229     C*
058000960229     C                   ENDIF                                                  <-- 6A --
058100960229     C*
058200930602     C* TRASFORMO I PROGRESSIVI ORE IN DATE
058300930602     C                   EXSR      TPODAT
058400930602     C*
058500930914     C* CONTROLLO SE BOLLA RIENTRA IN UNO DEI CASI IN CUI NON RIESCO A CALCOLA
058600930602     C* RE I RITARDI PER CUI FORZO CONFORMITA' A SI
058700930602     C     D98ERR        IFEQ      '0'
058800960229     C     D98TLA        ANDNE     'T'
058900930602     C                   EXSR      CHKFOR
059000930602     C                   END
059100930602     C*
059200930602     C     D98ERR        IFEQ      '0'                                          --- 7 -->
059300960229     C     D98TLA        ANDNE     'T'
059400941228     C*
059500941228     C* CALCOLA LE ORE REALI DI PARTENZA
059600941228     C     D98ERR        IFEQ      '0'
059700941228     C                   EXSR      CALORP
059800941228     C                   END
059900930405     C*
060000930511     C* CALCOLA LE ORE REALI DI TRAZIONE
060100930420     C     D98ERR        IFEQ      '0'
060200930407     C                   EXSR      CALORT
060300930420     C                   END
060400930405     C*
060500930511     C* CALCOLA LE ORE REALI DI CONSEGNA
060600930420     C     D98ERR        IFEQ      '0'
060700930416     C                   EXSR      CALORC
060800930420     C                   END
060900930416     C*
061000930511     C* CALCOLA LE ORE REALI DI RESA
061100930420     C     D98ERR        IFEQ      '0'
061200930416     C                   EXSR      CALORR
061300930420     C                   END
061400930420     C*
061500930511     C* CALCOLA LE ORE DI DIFFERENZA REALI/TEORICHE
061600930420     C     D98ERR        IFEQ      '0'
061700930420     C                   EXSR      CALDIF
061800930420     C                   END
061900930420     C*
062000930511     C* CALCOLA LE CONFORMITA'
062100930420     C     D98ERR        IFEQ      '0'
062200930420     C                   EXSR      CALCNF
062300930420     C                   END
062400930913     C*
062500930913     C* CALCOLA LE ORE DI CONSEGNABILITA'
062600930913     C     D98ERR        IFEQ      '0'
062700950407     C     FLCONS        ANDEQ     'S'
062800930913     C                   EXSR      CALCNS
062900930913     C                   END
063000930423     C*
063100930511     C* PORTO I VALORI DAI CAMPI DI LAVORO AI CAMPI IN OUTPUT DELLA DS
063200930423     C     D98ERR        IFEQ      '0'
063300930423     C                   EXSR      W98D98
063400930423     C                   END
063500930528     C*
063600930602     C                   END                                                    <-- 7 ---
063700930602     C                   END                                                    <-- 6 ---
063800930528     C                   END                                                    <-- 5 ---
063900930528     C                   END                                                    <-- 4 ---
064000930528     C                   END                                                    <-- 3 ---
064100930528     C                   END                                                    <-- 2 ---
064200930528     C                   END                                                    <-- 1 ---
064300930323     C*
064400930324     C* SE PGM LANCIATO CON 'C' (LANCI MULTIPLI) CHIUDE CON LR (CHIUDE FILE)
064500930527     C* E LANCIA TUTTI I PGM CHIAMATI PER CHIUDERE I FILE DA LORO APERTI
064600930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
064700930324     C     D98TLA        IFEQ      'C'
064800970714     C                   CLEAR                   SI95
064900970701     C                   MOVEL     'C'           I95TLA
065000020226     C                   CALL      'TISI95R'
065100970701     C                   PARM                    SI95
065200040809     c*
065300040809     c                   clear                   fidn12ds
065400040809     c                   eval      i12tla = 'C'
065500040809     c                   call      'FIDN12R'
065600040809     c                   parm                    fidn12ds
065700050523     c**
065800050523     c* controllo riempimento skiera CAK solo alla fine
065900050523     c**                 clear                   trul0sds
066000050523     c**                 eval      i0sski='CAK/CAM/CAP'
066100050523     c**                 eval      i0sele=%elem(CAK)
066200050523     c**                 if        windca>i0sele
066300050523     c**                 eval      i0spie=i0sele
066400050523     c**                 else
066500050523     c**                 eval      i0spie=windca
066600050523     c**                 endif
066700050523     c**                 eval      i0sfile='AZCLN00F  '
066800050523     c**                 eval      i0scod='     '
066900050523     c**                 eval      i0spgm='TNSD99R'
067000050523     c**                 movel     trul0sds      kpjbu
067100050523     c**                 call      'TRUL0SR'
067200050523     c**                 parm                    kpjba
067300050523     c
067400930323     C                   SETON                                        LR
067500930324     C                   ELSE
067600930324     C                   SETON                                        RT
067700930323     C                   END
067800941227     C********************************************************************
067900941227     C*   LETARB - LEGGE IL FILE FNARB (BOLLE ARRIVI)
068000941227     C********************************************************************
068100941227     C     LETARB        BEGSR
068200941227     C*
068300140820     C* LEGGO IL FILE FNARB01L
068400941227     C                   Z-ADD     D98AAS        ARBAAS
068500941227     C                   Z-ADD     D98LNP        ARBLNP
068600941227     C                   Z-ADD     D98NRS        ARBNRS
068700941227     C                   Z-ADD     D98NSP        ARBNSP
068800941228     C     KEYARB        CHAIN     FNARB01L                           99
068900941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
069000941227     C                   MOVEL     '1'           D98ERR
069100941227     C                   MOVEL     MSG(20)       D98MSG                         *BOLLA INESISTENTE
069200941227     C                   END                                                    <-- 1 ---
069300941228     C     D98ERR        IFEQ      '0'                                          --- 1 --> *TROVATA
069400941227     C                   MOVEL     ARBTSP        D98TSP
069500140820     c* Se forzato da Ds lo imposto al posto di quello della bolla
069600130529     c                   if        i98tsp_for<>' '
069700130529     C                   MOVEL     i98tsp_for    D98TSP
069800130529     c                   endif
069900941228     C                   MOVEL     ARBNZD        D98NZD
070000941227     C                   MOVEL     ARBCAD        D98CAD
070100970701     C                   MOVEL     ARBLOD        D98LOD
070200941227     C                   Z-ADD     ARBTFP        D98TFP
070300941227     C                   Z-ADD     ARBTFA        D98TFA
070400941227     C                   Z-ADD     ARBLNA        D98LNA
070500941227     C                   Z-ADD     ARBAAS        DSAAS
070600941227     C                   Z-ADD     ARBMGS        DSMGS
070700941227     C                   Z-ADD     DSDAT8        D98DSP
070800941227     C                   Z-ADD     ARBMGS        D98MGS
070900941227     C                   Z-ADD     ARBDTI        D98DTI
071000941227     C                   Z-ADD     ARBHTI        D98HTI
071100941227     C                   Z-ADD     ARBDCM        D98DCM
071200941227     C                   Z-ADD     ARBHMC        D98HMC
071300941227     C                   MOVEL     ARBGC1        D98GC1
071400941227     C                   MOVEL     ARBGC2        D98GC2
071500941227     C                   Z-ADD     ARBDCR        D98DCR
071600941227     C                   Z-ADD     ARBHCR        D98HCR
071700100308     C                   MOVEL     ARBTCR        D98TCR
071800100308     c* salvo i campi perchè in alcuni casi li clearo
071900100308     C                   Z-ADD     ARBDCR        s98DCR
072000100308     C                   Z-ADD     ARBHCR        s98HCR
072100100308     C                   MOVEL     ARBTCR        s98TCR
072200100308     c
072300941227     C                   MOVEL     ARBCMC        D98CMC
072400941227     C                   MOVEL     ARBFFD        D98FFD
072500941227     C                   Z-ADD     ARBNCD        D98NCD
072600941227     C                   MOVEL     ARBFNS        D98FNS
072700941227     C                   Z-ADD     ARBDUC        D98DUC
072800150508     C                   Z-ADD     ARBDPC        W98DPC
072900970310     C                   Z-ADD     *ZEROS        W98HUC
073000071122     c
073100071122     c                   movel     arbcbo        d98cbo
073200140820     c                   z-add     arbddc        d98ddc
073300140825     c                   z-add     arbndc        wd98ndc
073400140911     c                   movel     arbfbc        wd98fbc
073500150119     c                   movel     arbacc        wd98acc
073600150325     c                   exsr      IN_dist
073700150331     c* Se in fermo deposito vedo se presente particlarità consegna
073800150331     c                   if        arbffd='S' and arbgma<>*blanks
073900150331     C                   Z-ADD     CODUT         KTBUTE
074000150331     C                   MOVEL     '7R'          KTBCOD
074100150331     C                   MOVEL(P)  arbgma        KTBKEY
074200150331     C     KEYTAB        chain     TABEL00F
074300150331     c                   if        %found(tabel00f)
074400150331     c                   movel     tbluni        ds7r
074500150331     c                   endif
074600150331     c                   endif
074700150325     c
074800070511     c
074900141215     C                   Z-ADD     ARBDBR        D98DBR
075000170104     c
075100170105     C                   Z-ADD     ARBDBR        d98dfv
075200141215     c                   eval      d98fdrt=' '
075300070511     c
075400141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
075500070511     c* La data ritiro è presente solo su fnblp
075600070511     C     KEYARB        CHAIN     FNblp01L                           99
075700141215     c                   if        %found(fnblp01l)
075800070511     C                   Z-ADD     blpDrt        D98DBR
075900070511     c                   eval      d98fdrt='S'
076000070511     c                   endif
076100141215     c******             endif
076200070511     c
076300110923     C                   movel     ARBxco        D98xco
076400110923     C                   Z-ADD     ARBDAM        D98DAM
076500941227     C                   MOVEL     ARBFIN        D98FIN
076600990308     C                   MOVEL     ARBFBR        WFBR
076700031201     C                   MOVEL     ARBGGA        WGGA
076800050901     C                   MOVEL     ARBcca        d98cca
076900051124     C                   MOVEL     ARBTC1        d98TC1
077000051124     C                   MOVEL     ARBTC2        d98TC2
077100941227     C                   Z-ADD     1             I
077200941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
077300941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
077400941227     C                   MOVEL     '1'           D98ERR
077500941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
077600941227     C                   ELSE                                                   <-- 2 -->
077700941227     C                   MOVEL     T3A(I)        D98TBL
077800941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
077900990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
078000941227     C                   END                                                    <-- 2 ---
078100130826
078200130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
078300130826     c                   if        %subst(d98tbl:1:1) ='F'
078400130826     C                   Z-ADD     ARBKSC        D98KSC
078500130826     c                   else
078600130826     c
078700130826     c                   if        arbccm>0
078800130826     C                   Z-ADD     ARBccm        D98KSC
078900130826     c                   endif
079000130826     c                   if        d98ksc=0
079100130826     c                   z-add     8888          d98ksc
079200130826     c                   movel     d98lnp        d98ksc
079300130826     c                   endif
079400130826     c                   endif
079500130826     c
079600941227     C                   END                                                    <-- 1 ---
079700941228     C*
079800051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
079900950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
080000941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
080100941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
080200941228     C                   Z-ADD     D98AAS        AR9AAS
080300941228     C                   Z-ADD     D98LNP        AR9LNP
080400941228     C                   Z-ADD     D98NRS        AR9NRS
080500941228     C                   Z-ADD     D98NSP        AR9NSP
080600051115     C     KEYAR9        CHAIN     FiAR901L                           99
080700950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
080800950102     C                   MOVEL     'S'           WFLASS
080900950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
081000950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
081100950102     C                   Z-ADD     D98AAS        LBLAAN
081200950102     C                   Z-ADD     D98LNP        LBLLPN
081300950102     C                   Z-ADD     D98NRS        LBLNRN
081400950102     C                   Z-ADD     D98NSP        LBLNSN
081500950102     C     KEYLBL        CHAIN     FNLBL01L                           99
081600950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
081700950102     C                   Z-ADD     LBLAAP        AR9AAS
081800950102     C                   Z-ADD     LBLLPP        AR9LNP
081900950102     C                   Z-ADD     LBLNRP        AR9NRS
082000950102     C                   Z-ADD     LBLNSP        AR9NSP
082100051115     C     KEYAR9        CHAIN     FiAR901L                           99
082200950102     C     *IN99         IFEQ      '0'
082300950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
082400950102     C                   ENDIF
082500950102     C                   ENDIF                                                  <-- 3 ---
082600950102     C                   ENDIF                                                  <-- 2 ---
082700950102     C*
082800950102     C* CONTRASSEGNO NON TROVATO
082900950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
083000941228     C                   ELSE                                                   <-- 2 -->
083100950102     C* CONTRASSEGNO TROVATO
083200941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
083300010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
083400010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
083500941228     C                   ENDIF                                                  <-- 2 ---
083600941228     C                   ENDIF                                                  <-- 1 ---
083700150325     c
083800941227     C*
083900050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
084000941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
084100050405     C                   Z-ADD     D98AAS        KWGCPAAS
084200050405     C                   Z-ADD     D98LNP        KWGCPLNP
084300050405     C                   Z-ADD     D98NRS        KWGCPNRS
084400050405     C                   Z-ADD     D98NSP        KWGCPNSP
084500050405     C                   Z-ADD     *ZEROS        KWGCPFRG
084600050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
084700941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
084800941227     C     GCPATB        ANDNE     'A'
084900960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
085000960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
085100960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
085200161107$$$$ C                   movel     gcpfas        D98gfas
085300050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
085400960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
085500960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
085600050405     C* O UGUALE A 35 INVECE DI 40)
085700960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
085800050405     C     GCPFAS        ANDGE     40
085900941227     C                   Z-ADD     GCPDCG        D98DCG
086000950112     C                   Z-ADD     GCPDCG        W98DCG
086100960123     C                   ENDIF
086200960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
086300960729     C     GCPFAS        ANDGE     35
086400070202     C     GCPFAS        ANDne     36
086500960729     C                   Z-ADD     GCPDCG        D98DCG
086600960729     C                   Z-ADD     GCPDCG        W98DCG
086700960729     C                   ENDIF
086800941228     C                   ENDIF
086900941228     C                   ENDIF                                                  <-- 1 ---
087000970911     C*
087100970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
087200970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
087300970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
087400050922     C*****              Z-ADD     D98DTI        W98DTI
087500971210     C                   Z-ADD     D98LNP        F
087600971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
087700970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
087800970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
087900150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
088000150120     c                   if        arbdcp=0 and d98dfc=0
088100150120     C                   Z-ADD     d98dam        D98DTI
088200150120     c                   endif
088300150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
088400150120     C                   Z-ADD     d98dam        D98DTI
088500150120     c                   endif
088600150120     c                   ENDIF
088700150120     c***
088800150120     C***                Z-ADD     D98AAS        ARTAAS
088900150120     C***                Z-ADD     D98LNP        ARTLNP
089000150120     C***                Z-ADD     D98NRS        ARTNRS
089100150120     C***                Z-ADD     D98NSP        ARTNSP
089200150120     C***  KEYART        SETLL     FNART01L                           99
089300150120     C**N99KEYART        READE     FNART01L                               99
089400150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
089500150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
089600970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
089700150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
089800970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
089900970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
090000150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
090100150120     C***  ARTDCM        ANDEQ     *ZEROS
090200970912     C* SE PRIMO REC. TROVATO, MEM. DATA
090300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
090400150120     C***                Z-ADD     ARTDAM        D98DTI
090500150120     C***                ELSE                                                   <-- 6 -->
090600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
090700150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
090800150120     C***                Z-ADD     ARTDAM        D98DTI
090900150120     C***                ENDIF                                                  <-- 7 ---
091000150120     C***                ENDIF                                                  <-- 6 ---
091100150120     C***                ENDIF                                                  <-- 5 ---
091200970911     C* C'E' DATA CONSEGNAB.
091300150120     C***                ELSE                                                   <-- 4 -->
091400970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
091500970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
091600150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
091700150120     C***  ARTDAM        ANDLE     D98DFC
091800970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
091900970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
092000150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
092100150120     C***  ARTDCM        ORGT      D98DFC
092200970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
092300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
092400150120     C***                Z-ADD     ARTDAM        D98DTI
092500150120     C***                ELSE                                                   <-- 7 -->
092600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
092700150120     C***  ARTDAM        IFLT      D98DTI
092800150120     C***                Z-ADD     ARTDAM        D98DTI
092900150120     C***                ENDIF
093000150120     C***                ENDIF                                                  <-- 7 ---
093100150120     C***                ENDIF                                                  <-- 6 ---
093200150120     C***                ENDIF                                                  <-- 5 ---
093300150120     C***                ENDIF                                                  <-- 4 ---
093400150120     C***                ENDIF                                                  <-- 3 ---
093500150120     C***  KEYART        READE     FNART01L                               99
093600150120     C***                ENDDO                                                  <-- 2 ---
093700150120     C***                ENDIF                                                  <-- 1 ---
093800941227     C*
093900941227     C                   ENDSR
094000150325     C********************************************************************
094100150325     C*   Verifico se bolla in distinta
094200150325     C********************************************************************
094300150325     c     IN_dist       BEGSR
094400150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
094500150325     c                   if         ARBfbc = *blanks and
094600150325     c                               ARBddc > 0 and
094700150325     c                               ARBndc > 0 and ARBdcm=0
094800150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
094900150325     c                   if        not %open(fiapd01l)
095000150325     c                   open      fiapd01l
095100150325     c                   endif
095200150325     c
095300150325     C                   move      'A'           kpdtip
095400150325     c                   eval      kpdpdr=arbpdc
095500150325     C     kfapd         chain     fiapd01l
095600150325     c
095700150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
095800150325     c                   eval       d98cons = 'S'
095900160506     c                   else
096000160506     c                   eval       d98disf = 'F'
096100150325     c                   endif
096200150325     c                   endif
096300150325     c                   ENDSR
096400941227     C********************************************************************
096500941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
096600941227     C********************************************************************
096700941227     C     LETBLP        BEGSR
096800941227     C*
096900941227     C                   Z-ADD     D98AAS        BLPAAS
097000941227     C                   Z-ADD     D98LNP        BLPLNP
097100941227     C                   Z-ADD     D98NRS        BLPNRS
097200941227     C                   Z-ADD     D98NSP        BLPNSP
097300941227     C     KEYBLP        CHAIN     FNBLP01L                           99
097400941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
097500941227     C                   MOVEL     '1'           D98ERR
097600941227     C                   MOVEL     MSG(20)       D98MSG
097700941227     C                   END                                                    <-- 1 ---
097800941228     C     *IN99         IFEQ      '0'                                                    *TROV.
097900041028     C     KEYBLP        CHAIN     FNarb01L
098000041028     c                   if        %found(fnarb01l)
098100041028     c                   eval      wesarb='S'
098200041028     c                   else
098300041028     c                   eval      wesarb='N'
098400150219     c                   eval      wtrasm='D'
098500041028     c                   endif
098600141216     c
098700141216     c* se non trasmessa la bolla in arrivo memorizzo
098800150219     c                   if        blpft1= ' '
098900141229     c                   eval      wtrasm='N'
099000141216     c                   endif
099100141216     c
099200110923     C                   MOVEL     BLPxco        D98xco
099300110923     C                   MOVEL     BLPTSP        D98TSP
099400130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
099500130529     c                   if        i98tsp_for<>' '
099600130529     C                   MOVEL     i98tsp_for    D98TSP
099700130529     c                   endif
099800960209     C                   MOVEL     BLPNZD        D98NZD
099900941228     C                   MOVEL     BLPCAD        D98CAD
100000970701     C                   MOVEL     BLPLOD        D98LOD
100100941227     C                   Z-ADD     BLPTFP        D98TFP
100200941227     C                   Z-ADD     BLPTFA        D98TFA
100300941227     C                   Z-ADD     BLPLNA        D98LNA
100400941227     C                   Z-ADD     BLPAAS        DSAAS
100500941227     C                   Z-ADD     BLPMGS        DSMGS
100600941227     C                   Z-ADD     DSDAT8        D98DSP
100700941227     C                   Z-ADD     BLPMGS        D98MGS
100800170104
100900170104     c* se si tratta di prepagato NON confermato, devo fare finta che sia bollettato oggi
101000170104     c                   if        wtrasm='N'  and blppdr=0
101100170104     c                   movel     wdaodi        d98dsp
101200170104     c                   z-add     wdaodi        d98mgs
101300170104     c                   endif
101400170104     c
101500051124     C                   movel     BLPTC1        D98TC1
101600051124     C                   movel     BLPTC2        D98TC2
101700041028     c                   if        wesarb='S'
101800041028     C                   Z-ADD     arbDTI        D98DTI
101900041028     C                   Z-ADD     arbHTI        D98HTI
102000041028     C                   Z-ADD     arbDCM        D98DCM
102100041028     C                   Z-ADD     arbHMC        D98HMC
102200041028     C                   Z-ADD     arbDAM        D98DAM
102300050901     C                   MOVEL     arbcca        d98cca
102400140820     C                   MOVEL     arbddc        d98ddc
102500140911     C                   z-add     arbndc        wd98ndc
102600140910     C                   MOVEL     arbfbc        wd98fbc
102700150119     c                   movel     arbacc        wd98acc
102800150325     c                   exsr      IN_DIST
102900140820     c*
103000041028     c                   else
103100941227     C                   Z-ADD     BLPDTI        D98DTI
103200941227     C                   Z-ADD     BLPHTI        D98HTI
103300941227     C                   Z-ADD     BLPDCM        D98DCM
103400941227     C                   Z-ADD     BLPHMC        D98HMC
103500041028     C                   Z-ADD     BLPDAM        D98DAM
103600050901     C                   MOVEL     BLPcca        d98cca
103700140820     C                   MOVEL     BLPddc        d98ddc
103800140825     C                   MOVEL     BLPndc        wd98ndc
103900140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
104000140820     c                   clear                   d98cons
104100041028     c                   endif
104200941227     C                   MOVEL     BLPGC1        D98GC1
104300941227     C                   MOVEL     BLPGC2        D98GC2
104400941227     C                   Z-ADD     BLPDCR        D98DCR
104500941227     C                   Z-ADD     BLPHCR        D98HCR
104600941227     C                   MOVEL     BLPTCR        D98TCR
104700100308     c* Salvo i campi perchè in alcuni casi li clearo
104800100308     C                   Z-ADD     blpDCR        s98DCR
104900100308     C                   Z-ADD     blpHCR        s98HCR
105000100308     C                   MOVEL     blpTCR        s98TCR
105100100308     c
105200941227     C                   MOVEL     *BLANKS       D98CMC
105300941227     C                   MOVEL     BLPFFD        D98FFD
105400941227     C                   Z-ADD     BLPNCD        D98NCD
105500941227     C                   MOVEL     BLPFNS        D98FNS
105600941228     C                   Z-ADD     BLPDUC        D98DUC
105700150508     C                   Z-ADD     BLPDPC        w98DPC
105800970310     C                   Z-ADD     *ZEROS        W98HUC
105900071122     c                   movel     blpcbo        d98cbo
106000150331     c*
106100150331     c* Se in fermo deposito vedo se presente particlarità consegna
106200150331     c                   if        blpffd='S' and blpgma<>*blanks
106300150331     C                   Z-ADD     CODUT         KTBUTE
106400150331     C                   MOVEL     '7R'          KTBCOD
106500150331     C                   MOVEL(P)  blpgma        KTBKEY
106600150331     C     KEYTAB        chain     TABEL00F
106700150331     c                   if        %found(tabel00f)
106800150331     c                   movel     tbluni        ds7r
106900150331     c                   endif
107000150331     c                   endif
107100150331     c
107200970304     C                   Z-ADD     BLPDBR        D98DBR
107300170105     C                   Z-ADD     BLPDBR        d98dfv
107400070514     c                   eval      d98fdrt=' '
107500070514     c
107600141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
107700070514     C                   Z-ADD     blpDrt        D98DBR
107800070514     c                   eval      d98fdrt='S'
107900141215     c********           endif
108000070514     c*
108100941227     C                   MOVEL     BLPFIN        D98FIN
108200130826     C****               Z-ADD     BLPKSC        D98KSC
108300990308     C                   MOVEL     BLPFBR        WFBR
108400031201     C                   MOVEL     BLPGGA        WGGA
108500941227     C                   Z-ADD     1             I
108600941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
108700941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
108800941227     C                   MOVEL     '1'           D98ERR
108900941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
109000941227     C                   ELSE                                                   <-- 2 -->
109100941227     C                   MOVEL     T3A(I)        D98TBL
109200941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
109300990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
109400941227     C                   END                                                    <-- 2 ---
109500130826     c                   if        %subst(d98tbl:1:1) ='F'
109600130826     C                   Z-ADD     blpKSC        D98KSC
109700130826     c                   else
109800130826     c
109900130826     c                   if        blpccm>0
110000130826     C                   Z-ADD     blpccm        D98KSC
110100130826     c                   endif
110200130826     c                   if        d98ksc=0
110300130826     c                   z-add     8888          d98ksc
110400130826     c                   movel     d98lnp        d98ksc
110500130826     c                   endif
110600130826     c                   endif
110700130826     c
110800941227     C                   END
110900941228     C*
111000051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
111100950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
111200941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
111300941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
111400051115     C                   Z-ADD     D98AAS        ar9AAS
111500051115     C                   Z-ADD     D98LNP        ar9LNP
111600051115     C                   Z-ADD     D98NRS        ar9NRS
111700051115     C                   Z-ADD     D98NSP        ar9NSP
111800051115     C     KEYar9        CHAIN     Fiar901L                           99
111900950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
112000950102     C                   MOVEL     'S'           WFLASS
112100950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
112200950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
112300950102     C                   Z-ADD     D98AAS        LBLAAN
112400950102     C                   Z-ADD     D98LNP        LBLLPN
112500950102     C                   Z-ADD     D98NRS        LBLNRN
112600950102     C                   Z-ADD     D98NSP        LBLNSN
112700950102     C     KEYLBL        CHAIN     FNLBL01L                           99
112800950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
112900051115     C                   Z-ADD     LBLAAP        ar9AAS
113000051115     C                   Z-ADD     LBLLPP        ar9LNP
113100051115     C                   Z-ADD     LBLNRP        ar9NRS
113200051115     C                   Z-ADD     LBLNSP        ar9NSP
113300051115     C     KEYar9        CHAIN     Fiar901L                           99
113400950102     C     *IN99         IFEQ      '0'
113500950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
113600950102     C                   ENDIF
113700950102     C                   ENDIF                                                  <-- 3 ---
113800950102     C                   ENDIF                                                  <-- 2 ---
113900950102     C*
114000950102     C* CONTRASSEGNO TROVATO
114100150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
114200051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
114300051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
114400051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
114500941228     C                   ENDIF                                                  <-- 2 ---
114600941228     C                   ENDIF                                                  <-- 1 ---
114700941227     C*
114800050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
114900941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
115000050405     C                   Z-ADD     D98AAS        KWGCPAAS
115100050405     C                   Z-ADD     D98LNP        KWGCPLNP
115200050405     C                   Z-ADD     D98NRS        KWGCPNRS
115300050405     C                   Z-ADD     D98NSP        KWGCPNSP
115400050405     C                   Z-ADD     *ZEROS        KWGCPFRG
115500050405     C     KEYGCP        CHAIN     TIGCP01L                           99
115600941228     C     *IN99         IFEQ      '0'                                          *TROV.
115700941227     C     GCPATB        ANDNE     'A'
115800960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
115900960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
116000960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
116100960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
116200161107$$$$ C                   movel     gcpfas        D98Gfas
116300050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
116400960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
116500960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
116600050405     C* O UGUALE A 35 INVECE DI 40)
116700960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
116800050405     C     GCPFAS        ANDGE     40
116900941227     C                   Z-ADD     GCPDCG        D98DCG
117000950112     C                   Z-ADD     GCPDCG        W98DCG
117100960123     C                   ENDIF
117200960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
117300960729     C     GCPFAS        ANDGE     35
117400070202     C     GCPFAS        ANDne     36
117500960729     C                   Z-ADD     GCPDCG        D98DCG
117600960729     C                   Z-ADD     GCPDCG        W98DCG
117700960729     C                   ENDIF
117800941228     C                   ENDIF
117900941228     C                   ENDIF                                                  <-- 1 ---
118000970912     C*
118100970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
118200970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
118300970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
118400050922     C******             Z-ADD     D98DTI        W98DTI
118500971210     C                   Z-ADD     D98LNP        F
118600971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
118700970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
118800970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
118900150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
119000150120     c                   if        blpdcp=0 and d98dfc=0
119100150120     C                   Z-ADD     d98dam        D98DTI
119200150120     c                   endif
119300150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
119400150120     C                   Z-ADD     d98dam        D98DTI
119500150120     c                   endif
119600150120     c                   ENDIF
119700150120     c*
119800150120     C***                Z-ADD     D98AAS        BLTAAS
119900150120     C****               Z-ADD     D98LNP        BLTLNP
120000150120     C***                Z-ADD     D98NRS        BLTNRS
120100150120     C***                Z-ADD     D98NSP        BLTNSP
120200150120     C***  KEYBLT        SETLL     FNBLT01L                           99
120300150120     C**N99KEYBLT        READE     FNBLT01L                               99
120400150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
120500150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
120600970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
120700150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
120800970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
120900970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
121000150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
121100150120     C**   BLTDCM        ANDEQ     *ZEROS
121200970912     C* SE PRIMO REC. TROVATO, MEM. DATA
121300150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
121400150120     C**                 Z-ADD     BLTDAM        D98DTI
121500150120     C**                 ELSE                                                   <-- 6 -->
121600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
121700150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
121800150120     C**                 Z-ADD     BLTDAM        D98DTI
121900150120     C**                 ENDIF                                                  <-- 7 ---
122000150120     C**                 ENDIF                                                  <-- 6 ---
122100150120     C**                 ENDIF                                                  <-- 5 ---
122200970912     C* C'E' DATA CONSEGNAB.
122300150120     **                  ELSE                                                   <-- 4 -->
122400970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
122500970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
122600150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
122700150120     C**   BLTDAM        ANDLT     D98DFC
122800970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
122900970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
123000150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
123100150120     C**   BLTDCM        ORGT      D98DFC
123200970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
123300150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
123400150120     C**                 Z-ADD     BLTDAM        D98DTI
123500150120     C**                 ELSE                                                   <-- 7 -->
123600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
123700150120     C**   BLTDAM        IFLT      D98DTI
123800150120     C**                 Z-ADD     BLTDAM        D98DTI
123900150120     C**                 ENDIF
124000150120     C**                 ENDIF                                                  <-- 7 ---
124100150120     C**                 ENDIF                                                  <-- 6 ---
124200150120     C**                 ENDIF                                                  <-- 5 ---
124300150120     C**                 ENDIF                                                  <-- 4 ---
124400150120     C**                 ENDIF                                                  <-- 3 ---
124500150120     C**   KEYBLT        READE     FNBLT01L                               99
124600150120     C**                 ENDDO                                                  <-- 2 ---
124700150120     C**                 ENDIF                                                  <-- 1 ---
124800941227     C*
124900941227     C                   ENDSR
125000930323     C********************************************************************
125100941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
125200930323     C********************************************************************
125300930323     C     RTTEOR        BEGSR
125400941227     C*
125500941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
125600941228     C     D98ERR        IFEQ      '0'
125700941227     C                   EXSR      RTPART
125800941227     C                   ENDIF
125900941227     C*
126000941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
126100941228     C     D98ERR        IFEQ      '0'
126200150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
126300150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
126400150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
126500150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
126600150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
126700941227     C                   EXSR      RTTRAZ
126800150219     c                   if        d98err<>'1'
126900150219     C                   Z-ADD     CASTIN        D98TTI
127000150219     C                   Z-ADD     CASTCN        D98TTC
127100150219     C                   endif
127200150219     C                   ENDIF
127300941227     C*
127400941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
127500941228     C     D98ERR        IFEQ      '0'
127600941227     C                   EXSR      RTCONS
127700941227     C                   ENDIF
127800941227     C*
127900941228     C     D98ERR        IFEQ      '0'
128000941227     C*
128100941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
128200941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
128300941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
128400090921     c                   z-add     d98tri        w99tri
128500090921     c* per la DS
128600090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
128700090921     c* per i calcoli interi
128800090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
128900090921     c
129000941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
129100941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
129200941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
129300090921     c                   z-add     d98trc        w99trc
129400090921     c* per la DS
129500090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
129600090921     c* per i calcoli interi
129700090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
129800941227     C*
129900941227     C                   ENDIF
130000930323     C*
130100930323     C                   ENDSR
130200941227     C********************************************************************
130300941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
130400941227     C********************************************************************
130500941227     C     RTPART        BEGSR
130600941228     C*
130700941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
130800941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
130900941228     C                   MOVEL     D98TSP        WTSP              1
131000130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
131100130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
131200050301     C*******
131300050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
131400050301     C*
131500050301     C*    W001          IFEQ      'A'
131600050301     C*    W0040         OREQ      8888
131700050301     C*                  MOVEL     'E'           WTSP
131800050301     C*                  ENDIF
131900050301     C*******
132000941227     C*
132100941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
132200941227     C                   MOVEL     'N'           WTRAOK            1
132300941227     C                   MOVEL     'P'           CASPTA
132400941227     C                   Z-ADD     D98LNP        CASLNP
132500941227     C                   Z-ADD     *ZEROS        CASLNA
132600941228     C                   MOVEL     WTSP          CASTSP
132700941228     C                   Z-ADD     99999999      CASDDE
132800941227     C     KEYCAS        SETGT     AZCAS01L                             99
132900941227     C  N99              READP     AZCAS01L                               99
133000941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
133100941227     C     WTRAOK        ANDEQ     'N'
133200941227     C     CASLNP        ANDEQ     D98LNP
133300950102     C     CASTSP        ANDEQ     WTSP
133400941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
133500941227     C     CASATB        ANDEQ     *BLANKS
133600941227     C                   MOVEL     'S'           WTRAOK
133700941227     C                   ELSE                                                   <-- 2 -->
133800941227     C                   READP     AZCAS01L                               99
133900941227     C                   ENDIF                                                  <-- 2 ---
134000941227     C                   ENDDO                                                  <-- 1 ---
134100941227     C*
134200941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
134300941227     C     WTRAOK        IFEQ      'N'
134400941227     C                   MOVEL     '1'           D98ERR
134500941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
134600941227     C                   ELSE
134700941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
134800941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
134900941227     C                   END
135000970214     C*
135100140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
135200140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
135300140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
135400140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
135500140403     C**                 Z-ADD     D98LNA        F
135600140403     C**   D98ERR        IFEQ      '0'
135700140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
135800140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
135900140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
136000140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
136100140403     C**                 ENDIF
136200140403     C**                 ENDIF
136300140402     c
136400140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
136500140402     c*  se tempi  = 0
136600140402     c                   if        wpick2='S' and d98tpi<=0
136700140402     c                   add       24            D98TPI
136800140402     c                   endif
136900941227     C*
137000941227     C                   ENDSR
137100941227     C********************************************************************
137200941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
137300941227     C********************************************************************
137400941227     C     RTTRAZ        BEGSR
137500941227     C*
137600941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
137700150219     c
137800150219     C                   Z-ADD     D98LNA        LTA(1)
137900150219     C                   Z-ADD     D98LNA        LTA(2)
138000150219     C                   Z-ADD     D98TFA        LTA(3)
138100941227     C                   Z-ADD     D98TFA        LTA(4)
138200941227     C*
138300941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
138400941227     C                   MOVEL     'N'           WTRAOK            1
138500941227     C                   Z-ADD     1             K                 3 0
138600941227     C*
138700941227     C* CICLO FINO A:
138800941227     C*    - FINE SCHIERA
138900941227     C*    - TROVATO TEMPI VALIDI
139000941227     C     K             DOUGT     4                                            --- 1 -->
139100941227     C     WTRAOK        OREQ      'S'
139200941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
139300941227     C     LTA(K)        ORNE      *ZEROS
139400941227     C                   MOVEL     'T'           CASPTA
139500941227     C                   Z-ADD     LTP(K)        CASLNP
139600941227     C                   Z-ADD     LTA(K)        CASLNA
139700941227     C                   MOVEL     D98TSP        CASTSP
139800941228     C                   Z-ADD     99999999      CASDDE
139900941227     C     KEYCAS        SETGT     AZCAS01L                             99
140000941227     C  N99              READP     AZCAS01L                               99
140100941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
140200941227     C     WTRAOK        ANDEQ     'N'
140300941227     C     LTP(K)        ANDEQ     CASLNP
140400941227     C     LTA(K)        ANDEQ     CASLNA
140500941227     C     D98TSP        ANDEQ     CASTSP
140600960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
140700941227     C     CASATB        ANDEQ     *BLANKS
140800941227     C                   MOVEL     'S'           WTRAOK
140900941227     C                   ELSE                                                   <-- 4 -->
141000941227     C                   READP     AZCAS01L                               99
141100941227     C                   ENDIF                                                  <-- 4 ---
141200941227     C                   ENDDO                                                  <-- 3 ---
141300941227     C                   ENDIF                                                  <-- 2 ---
141400941227     C                   ADD       1             K
141500941227     C                   ENDDO                                                  <-- 1 ---
141600941227     C*
141700941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
141800941227     C     WTRAOK        IFEQ      'N'
141900941227     C                   MOVEL     '1'           D98ERR
142000941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
142100150219     C***                ELSE
142200150219     C***                Z-ADD     CASTIN        D98TTI
142300150219     C***                Z-ADD     CASTCN        D98TTC
142400941227     C                   END
142500941227     C*
142600941227     C                   ENDSR
142700941227     C********************************************************************
142800941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
142900941227     C********************************************************************
143000941227     C     RTCONS        BEGSR
143100150629     c                   clear                   WTsogliaE         1
143200970701     C*
143300970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
143400970701     C                   CLEAR                   SI95
143500970701     C                   MOVEL     '3'           I95TCN
143600970701     C                   MOVEL     D98NZD        I95NAR
143700970701     C                   MOVEL     D98CAD        I95CAP
143800970701     C                   MOVEL     D98LOD        I95LOC
143900970701     C                   MOVEL     D98DSP        I95DAT
144000970701     C                   MOVEL     D98TSP        I95TSP
144100980602     C                   MOVEL     'S'           I95FRT
144200150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
144300150511     c                   if        d98ffd='S'
144400150511     c                   eval      i95tsp='C'
144500150511     c                   endif
144600020226     C*
144700020226     C* VERIFICO SE LNA E' FILIALE FEDEX
144800020226     C                   IF        D98LNA > *ZEROS
144900020226     C     D98LNA        LOOKUP    FILFED                                 55
145000020226     C                   IF        *IN55 = *ON
145100020226     C                   EVAL      I95FI1 = 'S'
145200020226     C                   ENDIF
145300020226     C                   ENDIF
145400020226     C*
145500020226     C                   CALL      'TISI95R'
145600970701     C                   PARM                    SI95
145700941227     C*
145800941228     C* SE NON TROVATO, ERRORE
145900970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
146000941227     C                   MOVEL     '1'           D98ERR
146100970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
146200941227     C                   ELSE                                                   <-- 1 -->
146300941227     C*
146400941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
146500941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
146600941227     C     CASTIN        DIV       24            WDIV              3 0
146700941227     C                   MVR                     WRES              3 0
146800941227     C     WRES          IFEQ      *ZEROS
146900970701     C                   Z-ADD     O950IN        D98TCI
147000941227     C                   ELSE
147100970701     C                   Z-ADD     O951IN        D98TCI
147200941227     C                   END
147300941227     C*
147400941227     C     CASTCN        DIV       24            WDIV                     99
147500941227     C                   MVR                     WRES
147600941227     C     WRES          IFEQ      *ZEROS
147700970701     C                   Z-ADD     O950CN        D98TCC
147800150629     c                   eval      wTSogliaE='0'
147900941227     C                   ELSE
148000970701     C                   Z-ADD     O951CN        D98TCC
148100150629     c                   eval      wTSogliaE='1'
148200941227     C                   END
148300941227     C*
148400150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
148500941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
148600150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
148700150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
148800150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
148900150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
149000150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
149100941227     C*
149200941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
149300941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
149400970701     C     O95ISO        IFEQ      'I'
149500941227     C     D98FIN        OREQ      'I'
149600970701     C                   ADD       O95TIS        D98TCI
149700970701     C                   ADD       O95TIS        D98TCC
149800941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
149900970701     C                   ADD       O95TIS        W990IN
150000970701     C                   ADD       O95TIS        W991IN
150100150629     C                   ADD       O95TIS        W990CN
150200150629     C                   ADD       O95TIS        W991CN
150300941227     C                   END
150400090827     c
150500090827     c* Sistemo i tempi di consegna se non sono multipli di 12
150600090827     c                   exsr      SisCONS
150700160404     c
150800160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
150900160404     c                   if        wVP='S'
151000160404     c                   add       24            w991IN
151100160404     c                   add       24            w991CN
151200160404     c                   add       24            w990IN
151300160404     c                   add       24            w990CN
151400160404     c                   endif
151500941228     C*
151600941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
151700941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
151800941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
151900941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
152000941228     C                   Z-ADD     D98TCC        W99CCE            3 0
152100941228     C                   ELSE                                                   <-- 3 -->
152200970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
152300970701     C                   CLEAR                   SI95
152400970701     C                   MOVEL     '3'           I95TCN
152500970701     C                   MOVEL     D98NZD        I95NAR
152600970701     C                   MOVEL     D98CAD        I95CAP
152700970701     C                   MOVEL     D98LOD        I95LOC
152800970701     C                   MOVEL     D98DSP        I95DAT
152900970701     C                   MOVEL     'E'           I95TSP
153000980602     C                   MOVEL     'S'           I95FRT
153100020226     C*
153200020226     C* VERIFICO SE LNA E' FILIALE FEDEX
153300020226     C                   IF        D98LNA > *ZEROS
153400020226     C     D98LNA        LOOKUP    FILFED                                 55
153500020226     C                   IF        *IN55 = *ON
153600020226     C                   EVAL      I95FI1 = 'S'
153700020226     C                   ENDIF
153800020226     C                   ENDIF
153900020226     C*
154000020226     C                   CALL      'TISI95R'
154100970701     C                   PARM                    SI95
154200970701     C*
154300941228     C* SE NON TROVATO, ERRORE
154400970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
154500941228     C                   MOVEL     '1'           D98ERR
154600970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
154700941228     C                   ELSE                                                   <-- 4 -->
154800941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
154900941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
155000941228     C     CASTCN        DIV       24            WDIV                     99
155100941228     C                   MVR                     WRES
155200941228     C     WRES          IFEQ      *ZEROS
155300970701     C                   Z-ADD     O950CN        W99CCE
155400941228     C                   ELSE
155500970701     C                   Z-ADD     O951CN        W99CCE
155600941228     C                   END
155700941228     C                   ENDIF                                                  <-- 4 ---
155800941228     C                   ENDIF                                                  <-- 3 ---
155900941228     C                   ENDIF                                                  <-- 2 ---
156000941227     C*
156100941227     C                   END                                                    <-- 1 ---
156200941227     C*
156300941227     C                   ENDSR
156400090827     C********************************************************************
156500090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
156600090827     C********************************************************************
156700090827     C     SisCONS       BEGSR
156800090827     c                   eval      w99tci=d98tci
156900090827     c                   if        d98tci=10
157000090827     c                   z-add     12            w99tci
157100090827     c                   endif
157200090827     c
157300090827     c                   eval      w99tcc=d98tcc
157400090827     c                   if        d98tcc=10
157500090827     c                   z-add     12            w99tcc
157600090827     c                   endif
157700160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
157800160404     c                   if        wVP='S'
157900160404     c                   add       24            w99tcc
158000160404     c                   add       24            w99tci
158100160404     c                   endif
158200090827     c
158300090827     C                   ENDSR
158400930420     C********************************************************************
158500930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
158600930420     C********************************************************************
158700930420     C     CAREVB        BEGSR
158800930420     C*
158900930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
159000930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
159100060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
159200930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
159300930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
159400060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
159500060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
159600930420     C                   Z-ADD     D98AAS        KEBAAS
159700930420     C                   Z-ADD     D98LNP        KEBLNP
159800930420     C                   Z-ADD     D98NRS        KEBNRS
159900930420     C                   Z-ADD     D98NSP        KEBNSP
160000990810     C     KEYEVB        SETLL     FNEVB01L                           99
160100060406     C  N99keyevb        READe     FNEVB01L                               99
160200060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
160300930421     C     D98ERR        ANDEQ     '0'
160400060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
160500930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
160600930421     C                   Z-ADD     1             I
160700930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
160800060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
160900930421     C                   MOVEL     '1'           D98ERR
161000930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
161100060406   x3C                   ELSE                                                   <-- 3 -->
161200060406     C                   MOVEL     D2A(I)        usa2A
161300060406     C                   MOVEL     D2Z(I)        usa2Z
161400930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
161500060406    4C     §2AFTC        IFEQ      'A'
161600930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
161700060406    4C                   END
161800060406    4C     §2AFTC        IFEQ      'G'
161900930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
162000060406    4C                   END
162100150216     c* Se si tratta di MIC verifico se uguale alla sped.
162200150303    4c                   if        evbcev='MIC' or evbcev='NIC'
162300150303    5c                   if        %subst(evbnot:1:7) >*zeros
162400150303     c
162500150303    6c                   if        evbcev='MIC' and
162600150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
162700150216     c                   eval      evemic='SI'
162800150303    6c                   endif
162900150303     c
163000150303    6c                   if        evbcev='NIC' and
163100150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
163200150216     c                   eval      evemic='  '
163300150303    6c                   endif
163400150303     c
163500150303   x5c                   else
163600150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
163700150303     c                   eval      evemic='SI'
163800150303    6c                   endif
163900150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
164000150303     c                   eval      evemic='  '
164100150303    6c                   endif
164200150303
164300150303    5c                   endif
164400150303    4c                   endif
164500150216     c
164600060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
164700930421     C     §2AFTC        OREQ      'G'
164800930421     C                   ADD       1             IE
164900990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
165000930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
165100930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
165200131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
165300090928     c
165400090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
165500091014     c**                 if        ie=1
165600091014     c**                 eval      Primotipo=§2aftc
165700091014     c**                 eval      Primodev=evbdev
165800091014     c**                 eval      Primohev=evbhev
165900091014     c**                 endif
166000090928     c
166100060406    4C                   END                                                    <-- 4 ---
166200060406     c* Verifico se si tratta di evento particolare
166300060406    4c                   if        §2zCEP<>' '
166400060526     c* non carico l'evento se cade in giorno festivo
166500060526     c                   z-add     evbdev        wpodata
166600060526     C                   EVAL      WTC='C'
166700060526     C                   EVAL      WIC='C'
166800060526     c                   eval      wslitt='N'
166900060526     c                   exsr      chkfes
167000060526     c
167100060526     c* Wslitt='N' non festivo
167200060526   4ac                   if        wslitt='N'
167300060526     c
167400060406    5c                   if        §2zfep='A'
167500060406     c                   eval      wevpaA='S'
167600060406    5C                   END                                                    <-- 3 ---
167700060406    5c                   if        §2zfep='G'
167800060406     c                   eval      wevpaG='S'
167900060406    5C                   END                                                    <-- 3 ---
168000060406     c                   add       1             ip
168100060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
168200060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
168300060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
168400060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
168500060526   4aC                   ENDif                                                  <-- 3 ---
168600060526    4C                   END                                                    <-- 3 ---
168700060406    3C                   END                                                    <-- 3 ---
168800060406    2C                   END                                                    <-- 2 ---
168900060406     C     keyevb        READe     FNEVB01L                               99
169000060406    1C                   ENDDO                                                  <-- 1 ---
169100050523     c* controllo riempimento skiere
169200050523     c                   clear                   trul0sds
169300050523     c                   eval      i0sski='EVD/EVO/EVC'
169400050523     c                   eval      i0sele=%elem(EVD)
169500050523     c                   eval      i0spie=ie
169600050523     c                   eval      i0sfile='FNEVB01L  '
169700050523     c                   eval      i0scod='     '
169800050523     c                   eval      i0spgm='TNSD99R'
169900050523     c                   movel     trul0sds      kpjbu
170000050523     c                   call      'TRUL0SR'
170100050523     c                   parm                    kpjba
170200930420     C*
170300060406     c* controllo riempimento skiere
170400060406     c                   clear                   trul0sds
170500060406     c                   eval      i0sski='PVD/PVO/PVC'
170600060406     c                   eval      i0sele=%elem(PVD)
170700060406     c                   eval      i0spie=ie
170800060406     c                   eval      i0sfile='FNEVB01L  '
170900060406     c                   eval      i0scod='     '
171000060406     c                   eval      i0spgm='TNSD99R'
171100060406     c                   movel     trul0sds      kpjbu
171200060406     c                   call      'TRUL0SR'
171300060406     c                   parm                    kpjba
171400060406     C*
171500930420     C                   ENDSR
171600931213     C********************************************************************
171700931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
171800931213     C********************************************************************
171900931213     C     CARTUC        BEGSR
172000931213     C*
172100931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
172200931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
172300931214     C     D98GC2        ANDEQ     *BLANKS
172400931213     C                   ELSE                                                   <-- 1 -->
172500931213     C*
172600931213     C                   MOVE      D98GC1        TUC(1)
172700931214     C                   MOVE      D98GC2        TUC(2)
172800931213     C*
172900931213     C                   END                                                    <-- 1 ---
173000931213     C*
173100931213     C                   ENDSR
173200970304     C********************************************************************
173300970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
173400970304     C********************************************************************
173500970304     C     DATRIF        BEGSR
173600970304     C*
173700050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
173800050922     c*  partenza
173900970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
174000970304     C*
174100970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
174200070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
174300070605     C***                MOVEL     '1'           D98ERR
174400070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
174500070605     C***                ELSE                                                   <-- 2 -->
174600970304     C*
174700970304     C                   Z-ADD     D98DRF        WD1
174800970304     C                   Z-ADD     D98ORF        WO1
174900970310     C* DATA PARTENZA ULTIMO COLLO
175000970310     C     D98DRF        IFLE      D98DUC
175100050928     c     d98duc        oreq      0
175200970310     C                   Z-ADD     D98DRF        D98DUC
175300970310     C                   Z-ADD     D98ORF        W98HUC
175400070605     C                   ENDIF
175500970304     C* DATA ARRIVO TRAZIONE
175600050922     C***                Z-ADD     D98DTI        WD2
175700050922     C***                Z-ADD     D98HTI        WO2
175800050922     C***  WDO1          IFLE      WDO2
175900050922     C***                Z-ADD     D98DRF        D98DTI
176000050922     C***                Z-ADD     D98ORF        D98HTI
176100050922     C***                ENDIF
176200970304     C* DATA CONSEGNA
176300050922     C***                Z-ADD     D98DCM        WD2
176400050922     C***                Z-ADD     D98HMC        WO2
176500050922     C***  WDO1          IFLE      WDO2
176600050922     C***                Z-ADD     D98DRF        D98DCM
176700050922     C***                Z-ADD     D98ORF        D98HMC
176800050922     C***                ENDIF
176900970304     C*
177000070605     C***                ENDIF                                                  <-- 2 ---
177100970304     C                   ENDIF                                                  <-- 1 ---
177200970304     C*
177300970304     C                   ENDSR
177400940901     C********************************************************************
177500940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
177600940901     C********************************************************************
177700940901     C     TDTIPO        BEGSR
177800940901     C*
177900940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
178000940902     C                   Z-ADD     D98TTI        W98TTI            3 0
178100941011     C*
178200941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
178300941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
178400941011     C     W98TTI        DIV       24            WDIV              3 0
178500941011     C                   MVR                     WRES              3 0
178600941011     C     WRES          IFEQ      *ZEROS
178700941011     C                   Z-ADD     *ZEROS        WASTA             9 0
178800941011     C                   ELSE
178900941011     C                   Z-ADD     12            WASTA
179000941011     C                   END
179100940901     C*
179200940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
179300940901     C     D98DTI        IFNE      *ZEROS
179400940901     C                   Z-ADD     D98DTI        WDAT
179500940901     C                   EXSR      DATORE
179600960911     C                   Z-ADD     WORE          WPODTI
179700960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
179800960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
179900960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
180000960911     C                   Z-ADD     WPODTI        W10POI            9 0
180100960911     C                   EXSR      R10FES
180200960911     C                   Z-ADD     W10POO        WPODTI
180300960911     C* AGGIUNGO LE ORA D'ASTA
180400960911     C                   ADD       WASTA         WPODTI
180500960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
180600960911     C     W10POI        IFEQ      W10POO
180700940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
180800940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
180900940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
181000940901     C                   EXSR      RO3ORE
181100940901     C                   ADD       WR3HHO        WPODTI
181200940901     C                   ADD       WR3HHO        W98TTI
181300940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
181400940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
181500940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
181600940901     C                   EXSR      RO3ORE
181700940901     C                   ADD       WR3HHO        WPODTI
181800940901     C                   ADD       WR3HHO        W98TTI
181900960911     C                   ENDIF                                                  *SE NON FESTIVO
182000940901     C                   END
182100940901     C*
182200940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
182300940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
182400940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
182500940901     C     W98TTI        DIV       24            WDIV              3 0
182600940901     C                   MVR                     WRES              3 0
182700940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
182800941228     C                   Z-ADD     W990IN        D98TCI
182900160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
183000160405     c*  per cui le sottraggo, per riagguingerle alla fine
183100160405     c                   if        wVP='S'
183200160405     c                   sub       24            D98TCI
183300160405     c                   endif
183400160405     c
183500150630     c                   eval      wTSogliaE='0'
183600940901     C                   ELSE                                                   <-- 2 -->
183700941228     C                   Z-ADD     W991IN        D98TCI
183800160405     c                   if        wVP='S'
183900160405     c                   sub       24            D98TCI
184000160405     c                   endif
184100150630     c                   eval      wTSogliaE='1'
184200940908     C                   ENDIF                                                  <-- 2 ---
184300090827     c* Sistemo i tempi di consegna se non sono multipli di 12
184400090827     c                   exsr      SisCONS
184500090827     c*
184600940908     C* RICALCOLO TEMPO DI RESA INTERNO
184700950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
184800950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
184900090921     c                   z-add     d98tri        w99tri
185000090921     c* per la DS
185100090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
185200090921     c* per i calcoli interni
185300090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
185400940908     C                   ENDIF                                                  <-- 1 ---
185500940901     C*
185600940901     C                   ENDSR
185700930325     C********************************************************************
185800930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
185900930325     C********************************************************************
186000930325     C     TDATPO        BEGSR
186100930406     C*
186200930420     C* TRASFORMO LA DATA ODIERNA
186300930416     C                   Z-ADD     WDAODI        WDAT
186400930416     C                   EXSR      DATORE
186500050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
186600960911     C                   MOVEL     'OSC  '       WR3TAB
186700960911     C                   Z-ADD     WHHODI        WR3HHI
186800960911     C                   EXSR      RO3ORE
186900050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
187000050922     c                   if        wr3hho=12
187100050922     c     wpoatt        add       12            wpoattdata24
187200050922     c                   else
187300050922     c                   z-add     wpoatt        wpoattdata24
187400050922     c                   endif
187500930325     C*
187600930420     C* TRASFORMO LA DATA DI SPEDIZIONE
187700931125     C     D98DSP        IFEQ      *ZEROS
187800930326     C                   MOVEL     '1'           D98ERR
187900930326     C                   MOVEL     MSG(2)        D98MSG
188000930326     C                   ELSE
188100931125     C                   Z-ADD     D98DSP        WDAT
188200930325     C                   EXSR      DATORE
188300930406     C                   Z-ADD     WORE          WPODSP
188400930326     C                   END
188500930414     C*
188600930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
188700930414     C                   MOVEL     'LIM  '       WR3TAB
188800930414     C                   Z-ADD     *ZEROS        WR3HHI
188900930414     C                   EXSR      RO3ORE
189000931222     C     WR3HHO        MULT      10            WTM060            6 0
189100931222     C     WPODSP        ADD       WTM060        WPOLIM
189200140822     c
189300930325     C*
189400930420     C* TRASFORMO LA DATA DI CONSEGNA
189500931125     C     D98DCM        IFNE      *ZEROS
189600931125     C                   Z-ADD     D98DCM        WDAT
189700930325     C                   EXSR      DATORE
189800930326     C                   Z-ADD     WORE          WPODCM
189900930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
190000930326     C                   MOVEL     'OSC  '       WR3TAB
190100931125     C                   Z-ADD     D98HMC        WR3HHI
190200930326     C                   EXSR      RO3ORE
190300930326     C                   ADD       WR3HHO        WPODCM
190400930326     C                   END
190500941228     C*
190600941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
190700941228     C     D98DUC        IFNE      *ZEROS
190800941228     C                   Z-ADD     D98DUC        WDAT
190900941228     C                   EXSR      DATORE
191000941228     C                   Z-ADD     WORE          WPODUC
191100970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
191200970310     C                   MOVEL     'OSP  '       WR3TAB
191300970310     C                   Z-ADD     W98HUC        WR3HHI
191400970310     C                   EXSR      RO3ORE
191500970310     C     D98DRF        IFNE      *ZEROS
191600050928     c     w98huc        andgt     1200
191700970310     C                   SUB       12            WR3HHO
191800970310     C                   ENDIF
191900970310     C                   ADD       WR3HHO        WPODUC
192000941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
192100070605     C***  WPODSP        IFGT      WPODUC
192200070605     C***                Z-ADD     WPODSP        WPODUC
192300070605     C***                ENDIF
192400941228     C                   ENDIF
192500960402     C*
192600141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
192700970304     C     D98DBR        IFNE      *ZEROS
192800970304     C                   Z-ADD     D98DBR        WDAT
192900960402     C                   EXSR      DATORE
193000960402     C                   Z-ADD     WORE          WPODBR
193100070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
193200960402     C                   ELSE
193300960402     C                   Z-ADD     WPODSP        WPODBR
193400960402     C                   ENDIF
193500930913     C*
193600960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
193700930913     C* CONSEGNABILITA'
193800960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
193900950407     C*
194000930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
194100931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
194200931125     C                   Z-ADD     D98DIC        WDAT
194300930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
194400930913     C                   Z-ADD     WORE          WPODIC
194500930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
194600931125     C                   Z-ADD     D98OIC        WR3HHI
194700930913     C                   EXSR      RO3ORE
194800930913     C                   ADD       WR3HHO        WPODIC
194900931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
195000950407     C*
195100930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
195200931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
195300931125     C                   Z-ADD     D98DFC        WDAT
195400930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
195500930913     C                   Z-ADD     WORE          WPODFC
195600140822     C                   Z-ADD     WORE          WPODFC_00
195700930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
195800931125     C                   Z-ADD     D98OFC        WR3HHI
195900930913     C                   EXSR      RO3ORE
196000930913     C                   ADD       WR3HHO        WPODFC
196100931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
196200930913     C*
196300930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
196400930913     C     D98ERR        IFEQ      '0'
196500930913     C     WPODFC        IFEQ      *ZEROS
196600930913     C     WPODIC        ANDNE     *ZEROS
196700930913     C                   MOVEL     '1'           D98ERR
196800930913     C                   MOVEL     MSG(17)       D98MSG
196900930913     C                   ENDIF
197000930913     C                   ENDIF
197100930907     C*
197200930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
197300930913     C     D98ERR        IFEQ      '0'
197400930913     C     WPODIC        IFGT      WPODFC
197500930907     C                   MOVEL     '1'           D98ERR
197600930913     C                   MOVEL     MSG(18)       D98MSG
197700930913     C                   ENDIF
197800930913     C                   ENDIF
197900950407     C*
198000950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
198100930511     C*
198200930511     C                   ENDSR
198300950110     C********************************************************************
198400950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
198500950110     C********************************************************************
198600950110     C     CALDPP        BEGSR
198700950110     C*
198800950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
198900950110     C                   MOVEL     'P'           WTC                            *PARTENZA
199000950110     C                   MOVEL     'I'           WIC                            *INTERNA
199100960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
199200950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
199300950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
199400950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
199500950110     C*
199600950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
199700950110     C                   MOVEL     'P'           WTC                            *PARTENZA
199800950110     C                   MOVEL     'C'           WIC                            *CLIENTE
199900950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
200000170104     C* SE TEMPO PARTENZA =0 sommo comunque 1 ore per slittare dal
200100170104     c*  festivo se fosse data festiva la data sped e non bortderizzata
200200170104     c*  in questa data
200300170104     c                   if        d98tpc=0
200400170104     c                   z-add     1             WR4NOI
200500170104     c                   else
200600950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
200700170104     c                   endif
200800950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
200900950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
201000170104     c
201100170104     c                   if        d98tpc=0
201200170104     c                   sub       1             wpoppc
201300170104     c                   endif
201400960229     C*
201500960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
201600050922     c
201700050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
201800050922     c
201900050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
202000090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
202100050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
202200090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
202300050922%%%  C*  ritardo alla partenza.
202400050922     c* Il tutto solo se la data di oggi > della
202500050922     c*  data teorica di partenza determinata
202600050922     c*
202700950110%%%  C     D98ERR        IFEQ      '0'
202800950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
202900950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
203000950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
203100050922     c     wpoattData24  andgt     wpoppi
203200960911%%%  C********             MOVEL'OSC  '   WR3TAB
203300960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
203400960911%%%  C********             EXSR RO3ORE
203500960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
203600050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
203700090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
203800090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
203900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
204000050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
204100050922     c* La nuova data calcolata non può essere < della minima prevista
204200050922     c                   if        wr0poo>wpoppi
204300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
204400050922     c                   endif
204500050922     c
204600950110%%%  C                   ENDIF
204700950110%%%  C                   ENDIF
204800950110%%%  C*
204900950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
205000050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
205100050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
205200050915%%%  C*  alla prtenza.
205300950110%%%  C     D98ERR        IFEQ      '0'
205400950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
205500950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
205600950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
205700950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
205800950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
205900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
206000050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
206100950110%%%  C                   ENDIF
206200950110%%%  C                   ENDIF
206300950110%%%  C*
206400950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
206500950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
206600950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
206700950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
206800950110%%%  C     D98ERR        IFEQ      '0'
206900950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
207000950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
207100950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
207200950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
207300090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
207400090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
207500950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
207600050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
207700950110%%%  C                   ENDIF
207800950110%%%  C                   ENDIF
207900960229     C*
208000960229     C                   ENDIF                                                  <-- 1 ---
208100950110     C*
208200950110     C                   ENDSR
208300930604     C********************************************************************
208400930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
208500930604     C********************************************************************
208600930604     C     CALDPT        BEGSR
208700090902     c                   clear                   RicalcoloTRAZ     1
208800930604     C*
208900930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
209000931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
209100931129     C                   MOVEL     'I'           WIC                            *INTERNA
209200050915     c* se non presente la data partenza ultimo collo, uso la teorica
209300950110     C     D98DUC        IFEQ      *ZEROS
209400950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
209500950110     C                   ELSE
209600950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
209700950110     C                   ENDIF
209800930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
209900930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
210000930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
210100930604     C*
210200930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
210300931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
210400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
210500950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
210600930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
210700930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
210800930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
210900150508
211000150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
211100150508     c* se presente la data partenza PRIMO  collo --> la uso
211200150508     c                   if        w98dpc=0
211300150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
211400150508     c                   else
211500150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
211600150508     C                   MOVEL     'C'           WIC                            *INTERNA
211700150508     C                   Z-ADD     W98DPC        WDAT
211800150508     C                   EXSR      DATORE
211900150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
212000150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
212100150508     C                   EXSR      RO4CDP
212200150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
212300150508     c                   endif
212400960229     C*
212500960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
212600940624%%%  C*
212700050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
212800050915
212900940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
213000050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
213100050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
213200050915     c*   attribuito alla trazione
213300940624%%%  C     D98ERR        IFEQ      '0'
213400940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
213500940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
213600090903     c     wpoatt        andge     wpopti
213700050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
213800090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
213900090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
214000940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
214100050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
214200050922     c                   if        wr0poo>wpopti
214300050922     c                   eval      wpopti=wr0poo
214400050922%%%  C                   endif
214500090902     c*
214600090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
214700090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
214800090902     c*  della trazione udate e utime
214900090902     c                   eval      RicalcoloTRAZ='S'
215000090902     c
215100940624%%%  C                   ENDIF
215200940624%%%  C                   ENDIF
215300940628%%%  C*
215400050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
215500050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
215600050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
215700940628%%%  C     D98ERR        IFEQ      '0'
215800940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
215900940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
216000940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
216100090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
216200090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
216300940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
216400050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
216500940628%%%  C                   ENDIF
216600940628%%%  C                   ENDIF
216700960229     C*
216800960229     C                   ENDIF                                                  <-- 1 ---
216900930604     C*
217000930604     C                   ENDSR
217100930511     C********************************************************************
217200930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
217300930511     C********************************************************************
217400930511     C     CALDCR        BEGSR
217500050914     c                   clear                   consideraora
217600100308     c
217700100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
217800100308     c*
217900100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
218000100308     c* 1) se presente solo ORA --> ignoro sempre
218100100308     c* 2) se presente data e ora IL --> ignoro l'ora
218200100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
218300100309     c***                if        d98tsp='E'
218400100309     c***                clear                   d98dcr
218500100309     c***                clear                   d98hcr
218600100309     c**+                clear                   d98tcr
218700100309     c***                endif
218800100308     c
218900100308     c                   if        d98hcr>0
219000100308     c
219100100308     c                   if        d98dcr=0
219200100308     c                   clear                   d98hcr
219300100308     c                   endif
219400100308
219500100308     c                   if        d98dcr>0 and d98tcr=' '
219600100308     c                   clear                   d98hcr
219700100308     c                   endif
219800100308     c                   endif
219900941228     C*
220000941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
220100941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
220200050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
220300050901     c*  (per uguale si intende entrambe nel mattino o entrambe
220400050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
220500050901     c*  (EDPES 1/9/05)
220600050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
220700050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
220800050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
220900050831     C***                Z-ADD     D98DCM        D98DCR
221000050831     C***                ENDIF
221100050901     c*
221200070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
221300050914     c
221400100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
221500100308     c***                clear                   d98hcr
221600100308     c***                clear                   d98tcr
221700100308     c***                endif
221800050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
221900050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
222000070521     c*  richiesta dal cliente
222100100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
222200100308     C***                MOVEL     'OSC  '       WR3TAB
222300100308     C***                Z-ADD     D98HMC        WR3HHI
222400100308     C***                EXSR      RO3ORE
222500100308     c***                z-add     wr3hho        salvaoreHMC
222600050901     c
222700100308     C***                Z-ADD     D98Hcr        WR3HHI
222800100308     C***                EXSR      RO3ORE
222900100308     c***                z-add     wr3hho        salvaoreHCR
223000100308     c***
223100050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
223200100308    2c***                if        salvaoreHMC=salvaoreHCR
223300100308     C***                eval      Consideraora='S'
223400100308     c***                else
223500100308     C***                eval      Consideraora=' '
223600100308     c***                clear                   d98hcr
223700100308    2c***                endif
223800100308    1c***                endif
223900930511     C*
224000930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
224100931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
224200931125     C                   Z-ADD     D98DCR        WDAT
224300930325     C                   EXSR      DATORE
224400930326     C                   Z-ADD     WORE          WPODCR
224500930420     C*
224600930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
224700930420     C*
224800931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
224900930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
225000930326     C                   MOVEL     'CRD  '       WR3TAB
225100931125     C                   Z-ADD     D98HCR        WR3HHI
225200930419     C                   EXSR      RO3ORE
225300930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
225400940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
225500050902     c
225600931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
225700930420     C                   MOVEL     'CRD1 '       WR3TAB
225800930420     C                   Z-ADD     *ZEROS        WR3HHI
225900930420     C                   EXSR      RO3ORE
226000931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
226100090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
226200090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
226300050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
226400060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
226500050907     c                   z-add     worecr        oreILoDOPO
226600931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
226700931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
226800931207     C                   MOVEL     'C'           WIC                            *CLIENTE
226900931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
227000150330     C                   Z-ADD     WORECR        WR4NOI
227100931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
227200931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
227300931209     C     WPOCRF        SUB       12            WR7POI            9 0
227400931209     C                   MOVEL     'X'           WIC
227500931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
227600931209     C                   ADD       12            WR7POO
227700931207     C     WR7POO        IFGT      WPOCRF
227800931207     C                   Z-ADD     WR7POO        WPOCRF
227900931207     C                   END
228000150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
228100150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
228200150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
228300150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
228400150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
228500150330     C                   MOVEL     'C'           WIC                            *CLIENTE
228600150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
228700150330     C                   Z-ADD     w99TCC        WR4NOI
228800150330     c                   sub       12            wr4noi
228900150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
229000150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
229100150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
229200150330     c
229300150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
229400150330     C                   MOVEL     'X'           WIC
229500150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
229600150330     C******             ADD       12            WR7POO
229700150330     C     WR7POO        IFGT      WPOCRF_dee
229800150330     C                   Z-ADD     WR7POO        WPOCRF_dee
229900150330     C                   END
230000050902     c
230100050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
230200050902     c*  sono le ore di fine della non consegnabilità
230300050905     c* Se aggiungendo 1 finisco in giorno festivo
230400050905     c*  cerco primo giorno non festivo
230500050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
230600050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
230700050907     C****               MOVEL     'C'           WIC                            *CLIENTE
230800050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
230900050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
231000050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
231100050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
231200050907     c
231300050907     c                   add       1             wpocri
231400050902     c
231500050902   FFC                   Z-ADD     WPOCRI        WPOCRC
231600050902     c
231700930325     C                   END
231800150330     C*
231900931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
232000930420     C* DATA SPED. = DATA INIZIO RANGE
232100930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
232200940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
232300940324£$   C                   Z-ADD     WPOCRI        WPOCRC
232400930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
232500930420     C                   MOVEL     'CRP  '       WR3TAB
232600931125     C                   Z-ADD     D98HCR        WR3HHI
232700930420     C                   EXSR      RO3ORE
232800931207     C     WPODCR        ADD       WR3HHO        WPOCRF
232900931207     C                   SUB       24            WPOCRF                         *FINE RANGE
233000931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
233100931207     C* ANDANDO INDIETRO NEL TEMPO
233200050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
233300050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
233400050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
233500050907     c* data utile venedì sera
233600130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
233700130411     C                   MOVEL     'X'           WIC                            *CLIENTE
233800931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
233900931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
234000931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
234100930326     C                   END
234200930526     C*
234300931125     C     D98TCR        IFEQ      'P'
234400931125     C     D98TCR        OREQ      'D'
234500930526     C                   ELSE                                                   *CONS.RICH.IL ..
234600940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
234700940324£$   C                   Z-ADD     WPODCR        WPOCRC
234800930526     C* DATA SPED. = DATA INIZIO RANGE
234900050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
235000050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
235100050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
235200930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
235300930526     C                   MOVEL     'CRI  '       WR3TAB
235400931125     C                   Z-ADD     D98HCR        WR3HHI
235500930526     C                   EXSR      RO3ORE
235600050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
235700050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
235800050907     c                   z-add     wr3hho        oreILoDOPO
235900930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
236000050902     c
236100050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
236200050902     c                   if        wr3MATPOM='P' and d98hcr>0
236300050902     c                   add       12            wpocri
236400100308     c* tolgo 12 ore dalle ore conseganbili
236500050907     c                   sub       12            oreILoDOPO
236600050902     c                   endif
236700050902     c
236800931209     C     WPOCRF        SUB       12            WR7POI            9 0
236900931209     C                   MOVEL     'X'           WIC
237000931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
237100931209     C                   ADD       12            WR7POO
237200931209     C     WR7POO        IFGT      WPOCRF
237300931209     C                   Z-ADD     WR7POO        WPOCRF
237400931209     C                   END
237500050905     c
237600050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
237700050905     c*  sono le ore di fine della non consegnabilità
237800050905     c* Se aggiungendo 1 finisco in giorno festivo
237900050905     c*  cerco primo giorno non festivo
238000050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
238100050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
238200050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
238300050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
238400050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
238500050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
238600050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
238700050907     c
238800050907     c                   add       1             wpocri
238900050905     c
239000050905   FFC                   Z-ADD     WPOCRI        WPOCRC
239100050905     c
239200930526     C                   END
239300931206     C*
239400931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
239500931206     C* LA IMPOSTO COME LA CONS.TEORICA
239600140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
239700140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
239800931206     C                   ELSE
239900931206     C                   Z-ADD     WPOCRF        WPOCR1
240000931206     C                   END
240100150608     c* ai fini del calcolo affidabilità
240200150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
240300150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
240400931206     C                   ELSE
240500931206     C                   Z-ADD     WPOCRF        WPOCR2
240600931206     C                   END
240700150608     c* ai fini della data prevista consegna cliente
240800150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
240900150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
241000150608     C                   ELSE
241100150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
241200150608     C                   END
241300150331     c
241400150331     c                   if        wpocrf_dee>0
241500150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
241600150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
241700150330     C                   END
241800150331     C                   END
241900150331     C*
242000150331     c                   if        wpocrf_dee=0
242100150608     C     wpocr2_dee    sub       24            WPOCRF_dee
242200150331     c                   endif
242300150331     c
242400930526     C                   END                                                    <-- 1 ---
242500930511     C*
242600930511     C                   ENDSR
242700930511     C********************************************************************
242800930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
242900930511     C********************************************************************
243000930511     C     CALLAV        BEGSR
243100930511     C*
243200941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
243300930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
243400930421     C*
243500930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
243600930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
243700930421     C     1             DO        IE            E                 3 0          --- 2 -->
243800930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
243900930421     C                   Z-ADD     1             I
244000930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
244100060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
244200930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
244300930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
244400930421     C                   ADD       1             IL
244500930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
244600930421     C                   EXSR      DATORE
244700930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
244800091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
244900091014     c* Mi salvo la posizione dell'evento
245000091014     C                   Z-ADD     E             LAp(IL)
245100930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
245200930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
245300930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
245400930421     C                   EXSR      RO3ORE
245500930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
245600930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
245700930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
245800930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
245900091015     c**                 if        §2agla>1
246000091015     c**                 sub       1             §2agla
246100930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
246200091015     c**                 else
246300091015     c**                 clear                   whh
246400091015     c**                 endif
246500930510     C                   ADD       WHH           WORELA
246600940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
246700930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
246800090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
246900150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
247000930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
247100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
247200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
247300930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
247400930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
247500930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
247600930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
247700931209     C                   Z-ADD     LAF(IL)       WR7POI
247800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
247900931202     C     WR7POO        IFGT      LAF(IL)
248000931201     C                   Z-ADD     WR7POO        LAF(IL)
248100931202     C                   END
248200930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
248300930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
248400091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
248500930510     C                   END                                                    <-- 3 ---
248600931223     C                   ENDDO                                                  <-- 2 ---
248700930421     C*
248800930421     C                   END                                                    <-- 1 ---
248900930618££$$ C*
249000930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
249100931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
249200941228££$$ C     1             DO        IL            I
249300930618££$$ C     LAF(I)        IFGT      WPOLAF
249400930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
249500930618££$$ C                   END
249600931223££$$ C                   ENDDO
249700930511     C*
249800930511     C                   ENDSR
249900940324£$   C********************************************************************
250000940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
250100940324£$   C********************************************************************
250200940324£$   C     CALLAC        BEGSR
250300941229     C*
250400950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
250500950407     C*
250600950210     C                   Z-ADD     WORELA        WTM060
250700941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
250800941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
250900941229     C     D98DAG        IFEQ      *ZEROS
251000050520     c     §2aftg        andeq     'S'
251100090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
251200090827     C     WORELA        ADD       w99TCC        WTM060            6 0
251300160404     c                   if        wVP='S'
251400160404     c                   sub       24            wTM060
251500160404     c                   endif
251600941229     C                   ENDIF
251700941229     C*
251800940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
251900940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
252000941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
252100941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
252200941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
252300940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
252400941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
252500050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
252600050520     c                   movel     §2aftg        LCON(IL)
252700950407     C*
252800950407     C                   ENDIF
252900960129     C*
253000940324£$   C                   ENDSR
253100930511     C********************************************************************
253200930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
253300930511     C********************************************************************
253400930511     C     CALGIA        BEGSR
253500960129     C*
253600960129     C*-------------------------------------------------------------------
253700960129     C* EVENTI GIACENZA
253800960129     C*-------------------------------------------------------------------
253900960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
254000960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
254100960129     C*
254200050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
254300041116     c     d98nsp        ifgt      0
254400031125     C                   EXSR      CARGIAC
254500041116     c                   endif
254600031125     C*
254700960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
254800960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
254900960129     C     1             DO        IE            E                 3 0          --- 2 -->
255000960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
255100960129     C                   Z-ADD     1             I
255200960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
255300060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
255400960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
255500960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
255600960129     C                   ADD       1             IG
255700960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
255800960129     C                   EXSR      DATORE
255900960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
256000140606     c* Mi salvo la posizione dell'evento
256100140606     C                   Z-ADD     E             gip(IG)
256200960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
256300960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
256400960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
256500960129     C                   EXSR      RO3ORE
256600960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
256700960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
256800960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
256900960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
257000960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
257100960129     C                   ADD       WHH           WOREGI
257200960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
257300960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
257400090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
257500090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
257600160404     c                   if        wVP='S'
257700160404     c                   sub       24            wOREGI
257800160404     c                   endif
257900960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
258000960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
258100960129     C                   MOVEL     'C'           WIC                            *CLIENTE
258200960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
258300960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
258400960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
258500960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
258600960129     C                   Z-ADD     GIF(IG)       WR7POI
258700960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
258800031125     C     WR7POO        IFGT      GIF(IG)
258900031125     C                   Z-ADD     WR7POO        GIF(IG)
259000031125     C                   ENDIF
259100031201     C*
259200031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
259300031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
259400031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
259500031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
259600031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
259700031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
259800031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
259900031201     C                   IF        WPOATT > GIF(IG)
260000031201     C*
260100031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
260200031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
260300031201     C                   IF        WGGA = *BLANKS
260400031201     C                   SETOFF                                       54
260500031201     C                   ELSE
260600031201     C     WGGA          LOOKUP    SK7QW                                  54
260700031201     C                   ENDIF
260800031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
260900031201     C                   MOVEL     '1'           FLGGIACOK
261000031201     C                   ELSE
261100140127     c
261200031125     C*
261300031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
261400031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
261500031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
261600031125     C                   MOVEL     '0'           FLGGIACOK         1
261700031125     C*
261800031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
261900031125     C     1             DO        IAG           EG                3 0
262000031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
262100031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
262200031125     C                   EXSR      DATORE
262300031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
262400031125     C*
262500031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
262600031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
262700031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
262800031125     C                   EXSR      DATORE
262900031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
263000031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
263100031229     C* => NN LO CONSIDERO
263200031229     C                   IF        EVC(E+1) = 'G' AND
263300031229     C                             WPROXGII = WGCPDORE
263400031229     C                   MOVEL     '1'           FLGGIACOK
263500031229     C                   ENDIF
263600031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
263700031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
263800031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
263900031125     C                   EXSR      RO3ORE
264000031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
264100031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
264200031125     C* DELL'EVENTO GIACENZA CORRENTE
264300031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
264400060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
264500031125     C                   MOVEL     '1'           FLGGIACOK
264600031125     C                   LEAVE
264700031125     C                   ENDIF
264800031125     C                   ELSE
264900031229     C                   IF        EVC(E)  = 'G' AND
265000031229     C                             GII(IG) = WGCPDORE
265100031229     C                   MOVEL     '1'           FLGGIACOK
265200031229     C                   LEAVE
265300031229     C                   ENDIF
265400031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
265500031125     C                   MOVEL     '1'           FLGGIACOK
265600031125     C                   LEAVE
265700031125     C                   ENDIF
265800031125     C                   ENDIF
265900031125     C                   ENDDO
266000031201     C                   ENDIF
266100140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
266200140127     C                   IF        FLGGIACOK = '0'  and
266300140127     c                             d98dcm=evd(E)
266400140127     C                   MOVEL     '1'           FLGGIACOK
266500140127     c                   endif
266600031125     C*
266700031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
266800031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
266900031125     C* DELL'EVENTO GIACENZA CORRENTE
267000031125     C                   IF        FLGGIACOK = '0'
267100031125     C                   Z-ADD     *ZEROS        GIF(IG)
267200031125     C                   ELSE
267300031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
267400031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
267500031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
267600031125     C                   ENDIF
267700031125     C                   ELSE
267800031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
267900031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
268000031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
268100031125     C                   ENDIF
268200031125     C*
268300960129     C                   ENDIF                                                  <-- 3 ---
268400960129     C                   ENDDO                                                  <-- 2 ---
268500960129     C*
268600960129     C                   ENDIF                                                  <-- 1 ---
268700960129££$$ C*
268800960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
268900960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
269000960129££$$ C     1             DO        IG            I
269100960129££$$ C     GIF(I)        IFGT      WPOGIF
269200960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
269300960129££$$ C                   ENDIF
269400960129££$$ C                   ENDDO
269500941229     C*
269600941229     C*-------------------------------------------------------------------
269700941229     C* APERTURA GIACENZA
269800941229     C*-------------------------------------------------------------------
269900960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
270000960129     C*
270100960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
270200931125     C                   Z-ADD     D98DAG        WDAT
270300930325     C                   EXSR      DATORE
270400950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
270500960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
270600950413£££  C                   Z-ADD     WORE          WPODAG
270700960129$$£  C*********************ENDIF
270800930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
270900930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
271000930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
271100930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
271200930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
271300941229     C*
271400941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
271500941229     C                   Z-ADD     WPODAG        WPOAGC
271600960129     C*
271700960129     C                   ENDIF                                                  <-- 1 ---
271800941229     C*
271900941229     C*-------------------------------------------------------------------
272000930511     C* CHIUSURA GIACENZA
272100941229     C*-------------------------------------------------------------------
272200930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
272300930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
272400931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
272500931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
272600931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
272700931201     C                   Z-ADD     D98DCM        W98DCG
272800930416     C                   ELSE
272900930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
273000930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
273100931201     C                   Z-ADD     WDAODI        W98DCG
273200061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
273300061123     c*  chiusura giacenza, come già fa in altri posti
273400061123     c                   if        d98dfc>0
273500061123     C                   Z-ADD     d98dfc        W98DCG
273600061123     c                   endif
273700061123     c
273800930409     C                   END
273900930409     C                   END
274000930409     C                   END
274100930414     C* TRASF.DATA CHIUSURA
274200931201     C     W98DCG        IFNE      *ZEROS
274300931201     C                   Z-ADD     W98DCG        WDAT
274400930325     C                   EXSR      DATORE
274500930326     C                   Z-ADD     WORE          WPODCG
274600930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
274700930407     C* GIORNO DA ESCLUDERE
274800150216     C                   ADD       12            WPODCG
274900150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
275000150123     c*  il giorno di chiusura dovrebe già essere protetto
275100941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
275200941229     C     WPOCGC        IFEQ      *ZEROS
275300941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
275400941229     C                   ENDIF
275500930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
275600930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
275700931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
275800931129     C                   MOVEL     'C'           WIC                            *CLIENTE
275900150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
276000150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
276100160404     c                   if        wVP='S'
276200160404     c                   sub       24            wr4NOI
276300160404     c                   endif
276400930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
276500930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
276600930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
276700931209     C                   Z-ADD     WPODCG        WR7POI
276800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
276900931202     C     WR7POO        IFGT      WPODCG
277000931201     C                   Z-ADD     WR7POO        WPODCG
277100930326     C                   END
277200931202     C                   END
277300930511     C*
277400930511     C                   ENDSR
277500031125     C********************************************************************
277600031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
277700031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
277800031125     C*             DEGLI EVENTI DI TIPO GIACENZA
277900031125     C********************************************************************
278000031125     C     CARGIAC       BEGSR
278100031125     C*
278200031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
278300031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
278400031125     C*
278500050405     C                   Z-ADD     D98AAS        KWGCPAAS
278600050405     C                   Z-ADD     D98LNP        KWGCPLNP
278700050405     C                   Z-ADD     D98NRS        KWGCPNRS
278800050405     C                   Z-ADD     D98NSP        KWGCPNSP
278900031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
279000050405     C     KEYGC_P       SETLL     TIGCP01L
279100050405     C                   IF        %equal(TIGCP01L)
279200050405     C     KEYGC_P       READE     TIGCP01L
279300050405     C                   DOW       not %eof(TIGCP01L)
279400031125     C                   ADD       1             IAG
279500031125     C                   EVAL      GCPD(IAG) = GCPDUR
279600050405     C     KEYGC_P       READE     TIGCP01L
279700031125     C                   ENDDO
279800031125     C                   ENDIF
279900031125     C                   ELSE
280000050405     C     KEYGC_P       SETLL     TIGCP21L
280100050405     C                   IF        %equal(TIGCP21L)
280200050405     C     KEYGC_P       READE     TIGCP21L
280300050405     C                   DOW       not %eof(TIGCP21L)
280400031125     C                   ADD       1             IAG
280500031125     C                   EVAL      GCPD(IAG) = GCPDUR
280600050405     C     KEYGC_P       READE     TIGCP21L
280700031125     C                   ENDDO
280800031125     C                   ENDIF
280900031125     C                   ENDIF
281000031125     C*
281100031125     C                   ENDSR
281200941229     C********************************************************************
281300960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
281400941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
281500941229     C********************************************************************
281600960129     C     CALGIC        BEGSR
281700941229     C*
281800960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
281900960129     C*
282000960129     C                   Z-ADD     WOREGI        WTM060
282100960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
282200960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
282300960129     C     D98DAG        IFEQ      *ZEROS
282400960129$$£  C     WOREGI        ADD       12            WTM060            6 0
282500960129     C                   ENDIF
282600960129     C*
282700960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
282800960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
282900960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
283000960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
283100960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
283200960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
283300960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
283400960129     C*
283500960129     C                   ENDIF
283600941229     C*
283700941229     C                   ENDSR
283800060418     C********************************************************************
283900060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
284000060418     c*            l'affidabilità
284100060418     C********************************************************************
284200060418     C     CALCEPA       BEGSR
284300060418     C*
284400060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
284500060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
284600060420     c                   clear                   numggcepa         3 0
284700060418     C*
284800060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
284900060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
285000060420     c                   eval      wgiroepa='0'
285100060418     c                   eval      wfine=' '
285200060418     c
285300060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
285400060420    2C     W             DOwle     IP                                           --- 2 -->
285500060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
285600060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
285700060418     c
285800060420    4c                   if        wgiroepa='0'
285900060420     c                   exsr      ElaInizCEPA
286000060418   x4c                   else
286100060420    5c                   if        pvd(W)>wdat
286200060418     c                   eval      wfine='S'
286300060418    5c                   endif
286400060418    4c                   endif
286500060418     c
286600060420    4c                   if        wfine=' ' and pvd(W)=wdat
286700060420     C*
286800060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
286900060420     c*  finale
287000060420     c                   add       1             numggcepa
287100060420     c
287200060420     C                   z-add     24            WORELA
287300060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
287400060418     c
287500060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
287600060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
287700060418     C                   MOVEL     'C'           WIC                            *CLIENTE
287800060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
287900060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
288000060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
288100060418     c
288200060418     c* Trasformo le ore in data e verifico se c'e' altro evento
288300060418     c*  in sta data che allunga ancora
288400060420     c                   z-add     wr4poo        wore
288500060418     c                   EXSR      OREDAT
288600060418    4c                   endif
288700060418     c
288800060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
288900060418    4c                   if        wfine='S'
289000060418     c                   EXSR      ELAFINECEPA
289100060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
289200060420     c*  inizio range
289300060420     c                   else
289400060420     c                   add       1             w
289500060418    4C                   ENDif                                                  <-- 3 ---
289600060420     c
289700060420     c                   else
289800060420     c                   add       1             w
289900060418    3C                   ENDif                                                  <-- 3 ---
290000060418    2C                   ENDDO                                                  <-- 2 ---
290100060418     c
290200060420     c                   if        wgiroepa='1'
290300060418     c                   EXSR      ELAFINECEPA
290400060418     c                   endif
290500060418     C*
290600060418    1C                   END                                                    <-- 1 ---
290700060418££$$ C*
290800060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
290900060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
291000060420££$$ C     1             DO        Ipa           w
291100060420££$$ C     paif(w)       IFGT      WPOpaif
291200060420££$$ C                   Z-ADD     paif(w)       WPOpaif
291300060418££$$ C                   END
291400060418££$$ C                   ENDDO
291500060418     C*
291600060418     C                   ENDSR
291700060420     C********************************************************************
291800060420     c* Elabora Inizio range eventi particolari che allungano
291900060420     C********************************************************************
292000060420     c     ElaInizCEPA   BEGSR
292100060420     C                   ADD       1             IPA
292200060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
292300060420     C                   EXSR      DATORE
292400060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
292500061017     c
292600060420     c                   eval      wgiroepa='1'
292700060420     c                   eval      wdat=pvd(w)
292800060420     C                   ENDSR
292900060418     C********************************************************************
293000060418     c* Elabora fine range eventi particolari che allungano
293100060418     C********************************************************************
293200060418     c     ElafineCEPA   BEGSR
293300060420     c     numggcepa     mult      24            worela
293400060420     c                   sub       12            worela
293500090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
293600090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
293700160404     c                   if        wVP='S'
293800160404     c                   sub       24            WORELA
293900160404     c                   endif
294000060418     c
294100060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
294200060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
294300060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
294400060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
294500060418     C                   Z-ADD     paif(IPA)     WR7POI
294600060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
294700060418    5C     WR7POO        IFGT      paif(IPA)
294800060418     C                   Z-ADD     WR7POO        paif(IPA)
294900060418    5C                   END
295000061017     c*Sommo 24 ore di inizio giornata
295100061017     c*            : non ho capito perchè ma ci vogliono
295200061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
295300090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
295400090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
295500160404     c                   if        wVP='S'
295600160404     c                   sub       24            paii(IPA)
295700160404     c                   endif
295800061017     c* Se tempi di consegna inferiori alle 24 ore , devo
295900061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
296000061017     c*  partire prima
296100061017     c****               if        d98tcc<24
296200061017     c****               sub       12            paii(ipa)
296300061017     c****               endif
296400060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
296500060418     c* Imposto nuovo giro
296600060420     c                   eval      wgiroepa='0'
296700060418     c                   eval      wfine=' '
296800060420     c                   clear                   numggcepa         3 0
296900060418     c                   ENDSR
297000930511     C********************************************************************
297100930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
297200930511     C********************************************************************
297300010822     C     CALASS        BEGSR
297400010821     C*
297500011211     C                   EXSR      REPMAXCSB
297600930511     C*
297700930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
297800010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
297900011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
298000930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
298100930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
298200930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
298300930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
298400930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
298500930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
298600930511     C                   ELSE                                                   <-- 4 -->
2987009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
298800930512     C                   END                                                    <-- 4 ---
298900930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
299000930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
299100930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
299200930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
299300930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
299400930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
299500930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
299600930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
299700930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
299800930413     C                   END                                                    <-- 2 ---
299900011211     C                   END                                                    <-- 1 ---
300000930406     C*
300100930325     C                   ENDSR
300200930608     C********************************************************************
300300930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
300400930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
3005009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
300600930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
300700930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
300800930608     C********************************************************************
300900930608     C     RDRASS        BEGSR
301000930608     C*
301100930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
3012009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
3013009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
3014009407151507 C*********************EXSR RO3ORE
3015009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
301600930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
3017009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
3018009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
3019009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
3020009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3021009407191907 C                   Z-ADD     WPOASI        WPOROU
3022009407191907 C                   EXSR      CHKFES
3023009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3024009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
3025009407191907 C                   ELSE                                                   *E' FERIALE
3026009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
3027009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3028009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
3029009407191907 C                   ENDIF
303000930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
303100930608     C                   MOVEL     'ASS1 '       WR3TAB
303200930608     C                   Z-ADD     *ZEROS        WR3HHI
303300930608     C                   EXSR      RO3ORE
303400930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
303500930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
303600931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
303700931129     C                   MOVEL     'C'           WIC                            *CLIENTE
303800930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
303900930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
304000930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
304100930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
304200930608     C*
304300930608     C                   ENDSR
304400930511     C********************************************************************
304500930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
304600930511     C********************************************************************
304700930511     C     CALFED        BEGSR
304800150331     c                   clear                   fddata
304900150331     c                   clear                   WPOFDdata
305000930511     C*
305100051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
305200931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
305300051108     C     D98FFD        oreq      'M'                                          --- 1 -->
305400930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
305500930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
305600930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
305700930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
305800150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
305900150331     c                   if        §ar5dfd>*zeros
306000150331     c                   movel     §ar5dfd       fddata            8 0
306100150331     c                   endif
306200150331     c                   if        fddata>0 and ((d98dti>0 and
306300150331     c                             fddata>d98dti) or d98dti=0)
306400150331     C                   Z-ADD     fddata        WDAT
306500150331     C                   EXSR      DATORE
306600150331     C                   Z-ADD     WORE          WPOfddata
306700150331     c                   endif
306800150331     c
306900930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
307000930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
307100930511     C                   ELSE                                                   <-- 2 -->
3072009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
307300930512     C                   END                                                    <-- 2 ---
307400150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3075001504081507 C                   Z-ADD     WPOfddata     WPOFDI
307600150331     c                   endif
307700150331
307800930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
307900930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
308000930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
308100930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
308200930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
308300930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
308400930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
308500930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
308600930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
308700930511     C                   END                                                    <-- 1 ---
308800930511     C*
308900930511     C                   ENDSR
309000930608     C********************************************************************
309100930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
309200930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3093009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
309400930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
309500930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
309600930608     C********************************************************************
309700930608     C     RDRFED        BEGSR
309800930608     C*
309900930608     C* CALCOLO ORE DEL FERMO DEPOSITO
310000930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3101009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3102001503311907 C                   Z-ADD     WPOFDI        WPOROU
3103001503311907 C                   EXSR      CHKFES
3104001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3105001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3106001503311907 C                   ELSE                                                   *E' FERIALE
3107001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3108001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3109001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3110001503311907 C                   ENDIF
311100150331     c* Se presente la particolarità consegna con più giorni di fermo depo
311200150331     c*  uso questi
311300150331     c                   if        §7rggfd>*zeros
311400150424     c                   movel     §7rggfd       wggfd             2 0
311500150331     c     wggfd         mult      24            Wnore
311600150331     c                   add       wnore         worefd
311700150331     c                   else
311800930608     C                   MOVEL     'FED1 '       WR3TAB
311900930608     C                   Z-ADD     *ZEROS        WR3HHI
312000930608     C                   EXSR      RO3ORE
312100940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
312200150331     c                   endif
312300930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
312400931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
312500931129     C                   MOVEL     'C'           WIC                            *CLIENTE
312600930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
312700930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
312800930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
312900930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
313000930608     C*
313100930608     C                   ENDSR
313200931201     C********************************************************************
313300931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
313400931201     C********************************************************************
313500931201     C     CALDTE        BEGSR
313600140825     c
313700140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
313800140825     c*  che tiene conto solo dei festivi per restituire in output
313900140825     c*  la data teorica di consegna
314000140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
314100140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
314200140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
314300140825     C                   ELSE
314400140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
314500140825     C                   END
314600140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
314700141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
314800141106     c*  quindi passo un flag
314900141106     c                   eval      nosomma='N'
315000140825     C                   EXSR      RO4CDP
315100141106     c                   eval      nosomma=' '
315200140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
315300140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
315400140825     c*  di consegna
315500140825     c                   EXSR      TRAS_DEI
315600140825     c
315700140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
315800140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
315900140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
316000140825     c                             ='M' or %subst(d98gc2:2:1)='P')
316100140825     c                   exsr      Reper_tempi
316200140825     c                   endif
316300931201     C*
316400140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
316500140825     c*  che tiene conto di:
316600140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
316700150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
316800931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
316900931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
317000931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
317100931201     C                   ELSE
317200931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
317300931201     C                   END
317400090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
317500090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
317600931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
317700140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
317800931201     C*
317900150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
318000931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
318100931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
318200090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
318300931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
318400150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
318500150608     c
318600150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
318700150608     c*  che non tiene conto dei tempi del C/Ass
318800150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
318900150609     c*
319000150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
319100150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
319200150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
319300150630     c                   Z-ADD     WPODTI        WR6POI
319400150609     c                   else
319500150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
319600150609     c                   endif
319700150609     c
319800150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
319900150608     c                   eval      wdee='S'
320000150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
320100150608     c                   eval      wdee=' '
320200150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
320300141017
320400141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
320500150219     C**                 EVAL      WTC='C'
320600150219     C**                 EVAL      WIC='C'
320700150219     c**                 z-add     wpoattdata00  WPOROU
320800150219     c**                 z-add     wdaodi        wpodata
320900150219     c**                 exsr      chkfes
321000150219     c**                 if        wslitt='S'
321100150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
321200150219     c**                 z-add     24            wr6noi
321300150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
321400150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
321500150219     c**                 else
321600150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
321700150219     c**                 endif
321800141204     c* Ricalcolo WPOrifDatLav
321900141229     C**                 MOVEL     'OSC  '       WR3TAB
322000141229     C**                 Z-ADD     WHHODI        WR3HHI
322100141229     C**                 EXSR      RO3ORE
322200141229     c**
322300141229     C**                 EVAL      WTC='C'
322400141229     C**                 EVAL      WIC='C'
322500141229     c**                 z-add     wpoattdata00  WPOROU
322600141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
322700141229     c**                 z-add     wr3hho        wr6noi
322800141229     c**                 add       12            wr6noi
322900141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
323000141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
323100141229     c
323200141017     c
323300141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
323400141017     c*  ma sempre devo slittare i festivi
323500141017     C* CONSEGNABILITA'
323600150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
323700150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
323800150219     C**                 EVAL      WTC='C'
323900150219     C**                 EVAL      WIC='C'
324000150219     c**                 z-add     d98dfc        WPOdata
324100150219     c**                 exsr      chkfes
324200150219     c**                 if        wslitt='S'
324300150219     c**                 z-add     wpodfc_00     Wr6poi
324400150219     c**                 z-add     24            wr6noi
324500150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
324600150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
324700150219     c**                 else
324800150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
324900150219     c**                 endif
325000150219     c**                 endif
325100931201     C*
325200931201     C                   ENDSR
325300930511     C********************************************************************
325400930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
325500930511     C********************************************************************
325600930511     C     CALDPC        BEGSR
325700930325     C*
325800930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
325900930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
326000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
326100930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
326200930325     C                   ELSE
326300930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
326400930407     C                   END
326500090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
326600090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
326700930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
326800090902     c
326900931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
327000090901
327100930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
327200930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
327300930423     C* A QUELLA DI RICHIESTA CONSEGNA,
327400930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
327500931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
327600931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
327700931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
327800931206     C                   END
327900930423     C                   END
328000930325     C*
328100930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
328200930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
328300930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
328400090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
328500090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
328600930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
328700931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
328800930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
328900930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
329000930423     C* A QUELLA DI RICHIESTA CONSEGNA,
329100930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
329200931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
329300931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
329400931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
329500931206     C                   END
329600930423     C                   END
329700140822     C*
329800930324     C                   ENDSR
329900150113     C********************************************************************
330000150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
330100150113     C********************************************************************
330200150113     c     Cal_DEE       BEGSR
330300150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
330400150508     c                   clear                   wtempi00C         1
330500151007     c                   clear                   wtempi12C         1
330600150722     c                   clear                   woreOLP
330700151009     c                   clear                   wdiff
330800150219     c
330900150219     c* 1) Se presente l'arrivo della trazione lo uso
331000150219    1c                   select
331100150219     c                   when      wmancatraz=' '  and d98dti>0
331200150630     c                   Z-ADD     WPODTI        WR1POI
331300150219     c
331400150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
331500150219    1c                   when      IDdata>0
331600150325     c                   exsr      CalcoloIMP
331700151008     c* Passo in output il terminal di disguido
331800151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
331900151008     c                   eval      d98flp=%editc(LTP(2):'X')
332000151008     c                   endif
332100151008
332200150528     c* stato IMP/disguido solo se >  alla data di oggi
332300151006     c                   if        wr1poi > wpoattdata00 and
332400151006     c                             (wr1poi-wpoattdata00)>12
332500150427     c                   eval      d98spcdee='IMP'
332600151008     c                   endif
332700151006     c                   if        wr1poi > wpoattdata00 and
332800151006     c                             (wr1poi-wpoattdata00)=12
332900151006     c                             and w991In>12
333000151006     c                   eval      d98spcdee='IMP'
333100151008     c                   endif
333200150219     c
333300150219     c                   other
333400150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
333500150225     c* devo far finta che sia arrivato.
333600150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
333700150508     c*  che tiene conto della data partenza PRIMO  collo
333800150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
333900150508     c*
334000150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
334100150508     c                   if        wr1poi >=wpoattdata00
334200150508     c                              and WPOPTC_DEE > WPOPTC
334300150508     c                   eval      d98spcdee='IMP'
334400150508     c                   endif
334500150219     c                   endsl
334600150113     c
334700150219     c* se il calcolo è < di oggi --> imposto oggi
334800150113     c                   if        wr1poi < wpoattdata00
334900150113     c                   eval      wr1poi= wpoattdata00
335000150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
335100150508     c                   eval      wTempi00C='S'
335200151009     c                   clear                   wdiff
335300150113     c                   endif
335400170208     c
335500170208     c* Calcolo la data di arrivo reale/teorica dei colli da passare in outout
335600170208     C                   Z-ADD     wr1poi        WORE
335700170208     C                   EXSR      OREDAT
335800170208     C                   Z-ADD     WDAT          D98dam_RTC
335900150217     c
336000150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
336100150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
336200150630     c                   if        wr1poi>wpoattdata00
336300150630     c                   eval      wdiff=wr1poi-wpoattdata00
336400150630     c                   endif
336500151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
336600151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
336700151006     c                   eval      wTempi12C='S'
336800151006     c                   endif
336900160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
337000151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
337100151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
337200160404     c                             and idfgs>0
337300151009     C     wdiff         DIV       24            WDIV
337400151009     C                   MVR                     WRES
337500151009     C                   IF        wres>0
337600151009     c                   eval      wTempi12C='S'
337700151009     c                   else
337800151009     c                   eval      wTempi00C='S'
337900151009     c                   endif
338000151009     c                   endif
338100151009     c
338200151006     c
338300151006     c                   select
338400151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
338500151006     c                             w991in>12 and
338600151006     c                             (deeindist<>'S' or d98cons<>'S'
338700151006     c                             or d98ddc>wdaodi)
338800151006     c
338900151006     c* Se stato IMP lo mantengo per avere messaggio diverso
339000151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
339100151006     c                   if        d98spcdee<>'IMP'
339200151006     c                   eval      d98spcdee='PRV'
339300151006     c                   endif
339400151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
339500150330     c                             (deeindist<>'S' or d98cons<>'S'
339600150330     c                             or d98ddc>wdaodi)
339700150427     c
339800150427     c* Se stato IMP lo mantengo per avere messaggio diverso
339900150427     c                   if        d98spcdee<>'IMP'
340000150304     c                   eval      d98spcdee='PRV'
340100150427     c                   endif
340200150427     c
340300151006   x1c                   other
340400150330     c                   exsr      CAL_OLP
340500150217     c
340600150528     c* Se passato l'orario oppure data distinta < data di oggi
340700150508     c* 1) spedizione in distinta          --> in consegna
340800150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
340900150528     c                             d98ddc<wdaodi)
341000150508    3c                   IF        deeindist='S' and d98cons='S'
341100150508     c                   eval      d98spcdee= 'DDC'
341200150508
341300150508   x3c                   else
341400150507     c
341500150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
341600150722     c*  entro sera
341700150630     c                   if        wr1poi>wpoattdata00
341800150722     c                   z-add     12            woreOLP           3 0
341900150722     c***                add       12            wr1poi
342000150630    4c                   else
342100150507    4c                   if        wr1poi<=wpoattdata00
342200150722     c                   add       24            woreOLP
342300150722     c***                add       24            wr1poi
342400150507    4c                   endif
342500150630    4c                   endif
342600150507     c
342700150722     c****               eval      d98spcdee='OLP'
342800150507     c* memorizzo orario limite picking
342900150507     c                   eval      d98picklim=%editc(picklim:'X')
343000150507    3c                   endif
343100150507    2c                   endif
343200150507     c
343300151006    1c                   endsl
343400150217     c
343500150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
343600150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
343700150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
343800150113     C                   Z-ADD     Wr1poi        W10POI            9 0
343900150113     C                   EXSR      R10FES
344000150113     C                   Z-ADD     W10POO        wr1poi
344100150113     C
344200151006     c                   select
344300151006    1c                   when      wtempi12C='S'
344400151006     c                   z-add     W991IN        wr1noi
344500151006     c                   if        woreOLP>0
344600151006     c                   add       woreOLP       wr1noi
344700151006     c                   eval      d98spcdee='OLP'
344800160404     c                   else
344900160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
345000160404     c**                 if        wVP='S'
345100160404     c**                 add       24            wr1noi
345200160404     c**                 endif
345300151006     c                   endif
345400160404
345500151006    1c                   when      wtempi00C='S'
345600150630     c***                z-add     W990CN        wr1noi
345700150630     c                   z-add     W990IN        wr1noi
345800150722     c
345900150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
346000150722     c*  quindi sottraggo 24
346100151009     c                   if        wr1noi>24  and wdiff=0
346200150722     c                   sub       24            wr1noi
346300150722     c                   endif
346400150722     c
346500150722     c* se passato orario limite picking, slitto in avanti
346600150722     c                   if        woreOLP>0
346700150722     c                   add       woreOLP       wr1noi
346800150722     c                   eval      d98spcdee='OLP'
346900160404     c                   else
347000160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
347100160404     c**                 if        wVP='S'
347200160404     c**                 add       24            wr1noi
347300160404     c**                 endif
347400150722     c                   endif
347500150722     c
347600151006   x1c                   other
347700150722     c
347800150714     c                   if        wmancatraz=' '  and d98dti>0
347900150630     c                   z-add     w99tci        wr1noi
348000150714     c                   else
348100150714     c                   z-add     w99tcc        wr1noi
348200150714     c                   endif
348300160404
348400160401     c
348500150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
348600150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
348700150722     c                   if        woreOLP=24 and wr1noi<=24
348800150722     c                   add       woreOLP       wr1noi
348900150722     c                   eval      d98spcdee='OLP'
349000150722     c                   endif
349100150722     c                   if        woreOLP=12 and wr1noi<24
349200150722     c                   add       woreOLP       wr1noi
349300150722     c                   eval      d98spcdee='OLP'
349400150722     c                   endif
349500150722     c
349600151006    1c                   endsl
349700150629     c
349800150722    1c**                 if        WR1NOI=10
349900150722     c**                 z-add     12            wr1noi
350000150722     c**                 endif
350100150722     c
350200150722     c     wr1noi        div       12            wdiv
350300150722     c                   mvr                     wres
350400150722     c                   if        wres >0
350500150722     c                   add       2             wr1noi
350600150722     c                   endif
350700150722     c
350800150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
350900150629    1c******             endif
351000150508     c
351100150605     c                   eval      wdee='S'
351200150113     C                   EXSR      RO1CDP
351300150605     c                   eval      wdee=' '
351400150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
351500150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
351600150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
351700150113     C* A QUELLA DI RICHIESTA CONSEGNA,
351800150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
351900150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
352000150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
352100150331     c*  2) IL        -> è il giorno indicato
352200150331     C     D98DCR        IFNE      *ZEROS
352300150331     C     D98tcr        andne     'P'
352400150331     C***  WPOdpcee      IFLE      WPOCR2
352500150331     C***  WPOCR2        SUB       12            WPOdpcee
352600150330     c
352700150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
352800150330     c*   dare la prima data utile di consegna
352900150330     C     WPOdpcee      IFLE      WPOCRF_dee
353000150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
353100150330     C                   z-add     WPOCRF_dee    WPOdpcee
353200150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
353300150409     c                   if        wpodpcee>wpoattdata00
353400150409     c                   eval      d98spcdee='PRV'
353500150409     c                   endif
353600150113     C                   END
353700150331     c
353800150220     c
353900150113     C                   END
354000150220     c
354100150113     c                   ENDSR
354200150330     C********************************************************************
354300150330     C*   Calcolo orario da tabella OLP
354400150330     C********************************************************************
354500150330     c     Cal_OLP       BEGSR
354600150330     c                   eval      picklim=0900
354700150330     c* Dopo l'ora limite picking, passo al giorno dopo
354800150330     C                   MOVEL(P)  'OLP'         TBECOD
354900150330     C                   MOVEL(P)  d98lna        TBEKE1
355000150330     C                   MOVEL(P)  wdaodi        TBEKE2
355100150330     C     KTBE          CHAIN     TNTBE01L
355200150330     C                   IF        not %found(TNTBE01L)
355300150330     c                   clear                   tbeke2
355400150330     C     KTBE          CHAIN     TNTBE01L
355500150330     c                   endif
355600150330     C                   IF        not %found(TNTBE01L)
355700150330     C                   MOVEL(P)  '000'         TBEKE1
355800150330     C     KTBE          CHAIN     TNTBE01L
355900150330     c                   endif
356000150330     c
356100150330     C                   IF        %found(TNTBE01L) and
356200150330     c                             %subst(tbeuni:1:4)>*zeros
356300150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
356400150330     c                   endif
356500150330     c                   ENDSR
356600150325     C********************************************************************
356700150325     C*   Riclcolo data se colli in IMP/disguido
356800150325     C********************************************************************
356900150325     c     CalcoloIMP    BEGSR
357000150325     C                   Z-ADD     IDdata        WDAT
357100150325     C                   EXSR      DATORE
357200150325     C                   Z-ADD     WORE          WR4POI
357300150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
357400151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
357500150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
357600150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
357700150325     c* calcolo il terminal di partenza
357800150325     c                   clear                   fnlv55ds
357900150325     c                   eval      d55tla='L'
358000150325     c                   eval      d55tpt='P'
358100150325     c                   eval      d55lin=IDfgs
358200150325     c                   eval      d55drf=IDdata
358300150325     c                   call      'FNLV55R'
358400150325     c                   parm                    fnlv55ds
358500150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
358600150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
358700150325     c                   EXSR      RTTRAZ
358800150325    3c                   if        d98err<>'1'
358900150325     C                   Z-ADD     CASTcn        wr4noi
359000150325     c                   else
359100150325     C                   Z-ADD     d98ttc        wr4noi
359200150325    3C                   endif
359300150325
359400150325   x2c                   else
359500150325     C                   Z-ADD     d98ttc        wr4noi
359600150325    2C                   endif
359700150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
359800150325     c*  perchè partirà un giorno dopo
359900150325    3c                   if        IDnpg='P'
360000150325     c                   add       24            WR4noi
360100150325    3c                   endif
360200150325     c
360300150325     c                   exsr      ro4cdp
360400150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
360500150325     c                   ENDSR
360600930604     C********************************************************************
360700930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
360800930604     C********************************************************************
360900930604     C     TPODAT        BEGSR
361000960229     C*
361100960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
361200960229     C*
361300960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
361400960229     C     WPOPTI        IFEQ      *ZEROS
361500960229     C                   Z-ADD     *ZEROS        D98DTI
361600960229     C                   Z-ADD     *ZEROS        D98HTI
361700960229     C                   ELSE
361800960229     C                   Z-ADD     WPOPTI        WORE
361900960229     C                   EXSR      OREDAT
362000960229     C                   Z-ADD     WDAT          D98DTI
362100960229     C     WRES          MULT      100           D98HTI
362200960229     C                   END
362300960229     C*
362400960229     C                   ELSE                                                   <-- 1 -->
362500930604     C*
362600931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
362700140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
362800140825     c*******            exsr      Tras_DEI
362900140825     c
363000930604     C*
363100150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
363200140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
363300140820     c* è data da :
363400140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
363500140820     c*  Tenere  conto, ovvero slittare in avanti, per :
363600140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
363700140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
363800140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
363900140911     c*
364000140820     c*  Da azzerare nei seguenti casi:
364100150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
364200150324     c* 4) bolla export    7) Cod manca consegna
364300140911     c* 8) bolla con DCR DOPOil o PRIMAdel
364400150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
364500150119     c*10) se completamente non arrivata e collo in IMP
364600150119     c
364700140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
364800150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
364900140925     C*  - FD   - export -
365000150217     c                   movel     'S'           DEEinDIST         1
365100150113     c                   eval      wcaldpcee='S'
365200150304     c                   eval      D98SPCDEE='PRV'
365300150324     c                   clear                   avvcmc            1
365400150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
365500150323     c                   if        d98cmc<>*blanks
365600150324     C                   Z-ADD     1             II
365700150326     C     d98cmc        LOOKUP    C2A(II)                                99
365800150324     C                   MOVEL     D2A(II)       usa2A
365900150324    4C     §2AFTC        IFEQ      'A'
366000150326     c                   eval      avvcmc='A'
366100150323     c                   endif
366200150324    4C     §2AFTC        IFEQ      'G'
366300150326     c                   eval      avvcmc='G'
366400150324     c                   endif
366500150324     c                   endif
366600140820     c
366700140911     c                   z-add     d98lna        EX
366800150113     c*  la azzero nei casi previsti                 0
366900150601    1c                   select
367000150119     c* 1) Se la bolla e consegnata
367100140925     c                   when      d98dcm>0
367200150113     c                   clear                   wcaldpcee
367300150216     c                   eval      d98spcdee='CON'
367400150217     c                   movel     ' '           DEEinDIST         1
367500150323     c* 2) Se pratica di danno
367600150603     c                   when      flgdan='S'
367700150323     c                   clear                   wcaldpcee
367800150323     c                   eval      d98spcdee='CA '
367900150323     c                   eval      deeindist='S'
368000150324     c* 3) Se bloccata/rimpallo
368100150114     c                   when      wd98fbc='B'
368200150113     c                   clear                   wcaldpcee
368300150216     c                   eval      d98spcdee='BLO'
368400150217     c                   movel     ' '           DEEinDIST         1
368500150324     c* 4) Se bolla export
368600150324     c                   when      fl1(EX)='E'
368700150324     c                   clear                   wcaldpcee
368800150324     c                   movel     ' '           DEEinDIST         1
368900150324     c                   eval      d98spcdee='EXP'
369000150324     c* 5) Se giacente  o in attesa di apertura giacenza
369100150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
369200150324     c                   clear                   wcaldpcee
369300150324     c                   eval      d98spcdee='GIA'
369400150324     c                   eval      deeindist='S'
369500150119     c* 7) Se codice manca consegna
369600150326     c                   when      d98cmc<>*blanks and avvcmc='A'
369700150324     c                   clear                   wpodpcee
369800150324     c                   clear                   wcaldpcee
369900150324     c                   movel     ' '           DEEinDIST         1
370000150324     c                   eval      d98spcdee='ATT'
370100150410     c* 7) Bolla in sospensione consegna
370200150410     c                   when      §B4LBGE='S'
370300150410     c                   clear                   wpodpcee
370400150410     c                   clear                   wcaldpcee
370500150410     c                   movel     ' '           DEEinDIST         1
370600150410     c                   eval      d98spcdee='SOS'
370700150616     c
370800150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
370900161031     c                   when      wrbl='R'  or d98cbo='M '
371000150616     c                   clear                   wcaldpcee
371100150616     c                   eval      d98spcdee='   '
371200150616     c                   eval      deeindist='S'
371300150601   x1c                   other
371400150325     c*
371500150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
371600150331     c                   if        d98ffd='S'
371700150601    2c                   select
371800150403     c* FD escluso dai contatti
371900150601    2c                   when      §B4LTFD= 'N'
372000150403     c                   eval      d98spcdee='FD3'
372100150403     c                   clear                   wcaldpcee
372200150403     c                   movel     ' '           DEEinDIST         1
372300150403
372400150403     c* FD non contattato ( o da ricontattare)
372500150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
372600150401     c                             olv82sts<>'1'
372700150323     c                   clear                   wcaldpcee
372800150323     c                   movel     ' '           DEEinDIST         1
372900150323     c                   eval      d98spcdee='FFD'
373000150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
373100150424     c                   eval      wpodpcee=wpoffi
373200150403
373300150403     c* FD contattato e     IN distinta
373400150601    2c                   when      d98cons='S'
373500150401     c                   eval      d98spcdee='FD2'
373600150401     c                   clear                   wcaldpcee
373700150401     c                   movel     ' '           DEEinDIST         1
373800150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
373900150424     c                   eval      wpodpcee=wpoffi
374000150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
374100150427     c*  ritirare la merce
374200150427     C                   MOVEL     'C'           WTC                            *PARTENZA
374300150427     C                   MOVEL     'C'           WIC
374400150427     C                   Z-ADD     D98ddc        WDAT
374500150427     C                   EXSR      DATORE
374600150427     c                   z-add     wore          wr4poi
374700150427     c                   z-add     24            wr4noi
374800150427     C                   EXSR      RO4CDP
374900150427     c                   z-add     wr4poo        wore
375000150427     c                   exsr      OREDAT
375100150427     c                   movel     wdat          D98DTCTD
375200150601   x2c                   other
375300150403     c* FD contattato e NON in distinta
375400150331     c                   eval      d98spcdee='FD1'
375500150408     c                   eval      wpodpcee=wpoffi
375600150331     c                   clear                   wcaldpcee
375700150331     c                   movel     ' '           DEEinDIST         1
375800150601    2c                   endsl
375900150331     c
376000150331     c                   endif
376100150403     c
376200150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
376300150601    2c                   if        (d98tc1='A' or d98tc2='A' or
376400150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
376500150601     c                             and d98ffd= ' '
376600150325     c                   clear                   wcaldpcee
376700150325     c                   movel     ' '           DEEinDIST         1
376800150601    3c                   if        §B4LBAS= 'N'
376900150427     c                   eval      d98spcdee='AP3'
377000150403     c                   else
377100150403     c                   eval      d98spcdee='APP'
377200150601    3c                   endif
377300150601    2c                   endif
377400150325     c*   se la bolla ha il C/Assegno verifico se da contattare
377500150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
377600150325     c                   clear                   wcaldpcee
377700150325     c                   movel     ' '           DEEinDIST         1
377800170208    3c                   if        §B4LTCA= 'N'
377900150427     c                   eval      d98spcdee='CA3'
378000150403     c                   else
378100150403     c                   eval      d98spcdee='CAS'
378200150601    3c                   endif
378300150601    2c                   endif
378400150325     c* Per questi 3 casi devo impostare la data contatto destinatario
378500150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
378600150325     c                             d98spcdee='CAS'
378700170208     c
378800170208     c                   exsr      Caldam_rtc
378900170208     c                   endif
379000170208    1c                   endsl
379100140820     c
379200150113     c* Se la bolla non è in distinta calcolo la data se previsto
379300150220     c                   if        wcaldpcee='S' and
379400150220     c                             (d98cons<>'S'  or (d98cons='S' and
379500150220     c                             evemic<>'SI'))
379600150219     c
379700150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
379800150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
379900150219     c                   if        d98nsp>0 and wtrasm<>'D' and
380000150219     c                             d98dam=0
380100150219     c                   exsr      letBRV
380200150219     c                   endif
380300150219     c
380400150217     c                   eval      d98spcdee='PRG'
380500150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
380600150320     c                             d98tc1='S' or d98tc2='S')
380700150320     c                   eval      d98spcdee='APP'
380800150320     c                   endif
380900150320     c
381000150113     c                   exsr      cal_DEE
381100150113     c                   endif
381200150113     c
381300140820     C     WPOdpcee      IFEQ      *ZEROS
381400931202     C                   Z-ADD     *ZEROS        D98DEE
381500931202     C                   Z-ADD     *ZEROS        D98OEE
381600930604     C                   ELSE
381700140820     C                   Z-ADD     WPOdpcee      WORE
381800141021     C******             SUB       12            WORE
381900930604     C                   EXSR      OREDAT
382000931202     C                   Z-ADD     WDAT          D98DEE
382100090903     c                   if        d98tcc<>10
382200090903     C                   ADD       12            WRES
382300090903     c                   else
382400090903     C                   ADD       10            WRES
382500090903     c                   endif
382600090903
382700931202     C     WRES          MULT      100           D98OEE
382800090903     c
382900090903     c                   if        d98oee=1000
383000090903     c                   eval      d98oee=1030
383100090903     c                   endif
383200140925     C                   END
383300150311     c
383400150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
383500150311     c                   select
383600150311     c                   when      d98spcdee='OLP'
383700140820     c
383800150220     c* Se bolla in distinta --> data prevista = data distinta
383900150311     c                   when      deeindist='S'  and
384000150113     c                             d98cons='S'
384100150220
384200150225     c*  non la propongo senza MIC e se presente disguido o IMP
384300150220     c                   if        d98dee>d98ddc and evemic<>'SI'
384400150220     c                             and IDdata>0
384500150220     c                   else
384600150220
384700140820     c                   eval      d98dee=d98ddc
384800140925     c                   clear                   d98oee
384900150320     c* se la data è > di oggi stato PRV se non è in IMP
385000150320     c                   if        d98dee>wdaodi
385100150320     c                   if        d98spcdee<>'IMP'
385200150311     c                   eval      d98spcdee='PRV'
385300150320     c                   endif
385400150320     c
385500150225     c                   else
385600150217     c* se c'è il MIC
385700150217     c                   if        evemic='SI'
385800150217     c                   eval      d98spcdee='DDC'
385900150225     c                   endif
386000150217     c                   endif
386100150220     c                   endif
386200150217     c
386300150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
386400150722     c                   eval      d98spcdee='PRV'
386500150311     c                   endsl
386600170208     c
386700170208     c* se non ho ancora calcolato la data arrivo reale o teorica tempi cliente, lo faccio ora
386800170208     c                   if        d98dam_rtc=0
386900170208     c                   eval      wsolodam='S'
387000170208     c                   exsr      Caldam_rtc
387100170208     c                   eval      wsolodam=' '
387200170208     c                   endif
387300931202     C*
387400931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
387500931202     C     WPODCI        IFEQ      *ZEROS
387600931202     C                   Z-ADD     *ZEROS        D98DCI
387700931202     C                   Z-ADD     *ZEROS        D98HCI
387800931202     C                   ELSE
387900931202     C                   Z-ADD     WPODCI        WORE
388000931202     C                   EXSR      OREDAT
388100931202     C                   Z-ADD     WDAT          D98DCI
388200090902     c                   if        d98tci<>10
388300931202     C                   ADD       12            WRES
388400090902     c                   else
388500090902     C                   ADD       10            WRES
388600090902     c                   endif
388700090902
388800931202     C     WRES          MULT      100           D98HCI
388900090901     c
389000090903     c                   if        d98hci=1000
389100090901     c                   eval      d98hci=1030
389200090901     c                   endif
389300090901     c
389400931202     C                   END
389500931202     C*
389600931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
389700931202     C     WPODCE        IFEQ      *ZEROS
389800931202     C                   Z-ADD     *ZEROS        D98DCE
389900931202     C                   Z-ADD     *ZEROS        D98HCE
390000931202     C                   ELSE
390100931202     C                   Z-ADD     WPODCE        WORE
390200931202//// C**************       SUB  12        WORE
390300931202     C                   EXSR      OREDAT
390400931202     C                   Z-ADD     WDAT          D98DCE
390500090902     c                   if        d98tcc >10
390600931202     C                   ADD       12            WRES
390700090902     c                   else
390800090902     C                   ADD       10            WRES
390900090902     c                   endif
391000090902     c
391100931202     C     WRES          MULT      100           D98HCE
391200090902     c
391300090903     c                   if        d98hce=1000
391400090902     c                   eval      d98hce=1030
391500090902     c                   endif
391600090902     c
391700931202     C                   END
391800960229     C*
391900960229     C                   ENDIF                                                  <-- 1 ---
392000930604     C*
392100930604     C                   ENDSR
392200170208     C********************************************************************
392300170208     C* Calcolo data di arrivo teorica o reale cliente
392400170208     C********************************************************************
392500170208     c     Caldam_rtc    BEGSR
392600170208     c
392700170208    2c                   select
392800170220    2c                   when      d98dti>0 and wmancatraz= ' ' and wpodti>0
392900170220     c
393000170220     c* questa specifica non serve: se è piena la data è pieno wpodti e viceversa
393100170220     c*****              movel     d98dti        d98dtctd
393200170208     c* se piene le ore già portate ad un giorno lavorativo
393300170220     c*******            if        wpodti>0
393400170208     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
393500170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
393600170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
393700170208     C                   Z-ADD     WPODTI        W10POI            9 0
393800170208     C                   EXSR      R10FES
393900170208     c                   z-add     w10poo        wore
394000170208     C                   EXSR      OREDAT
394100170208     c                   if        wsolodam<>'S'
394200170208     C                   movel     WDAT          D98dtctd
394300170208     c                   endif
394400170208     C                   movel     WDAT          D98dam_rtc
394500170220     c*****              endif
394600170208     c
394700170208    2c                   when      d98dam>0
394800170208     C                   Z-ADD     D98dam        WDAT
394900170208     C                   EXSR      DATORE
395000170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
395100170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
395200170208     C                   Z-ADD     Wore          W10POI            9 0
395300170208     C                   EXSR      R10FES
395400170208     c                   z-add     w10poo        wore
395500170208     C                   EXSR      OREDAT
395600170208     c                   if        wsolodam<>'S'
395700170208     C                   movel     WDAT          D98dtctd
395800170208     c                   endif
395900170208     C                   movel     WDAT          D98dam_rtc
396000170208   x2c                   other
396100170208     c* data teorica di arrivo tenendo conto dell'IMP
396200170208     c                   if        d98nsp>0
396300170208     c                   exsr      letbrv
396400170208     c                   endif
396500170208     c                   if        idDATA>0
396600170208     c                   exsr      CalcoloIMP
396700170208     c                   z-add     wr1poi        WORE
396800170208     c                   else
396900170208     c                   z-add     wpoptc_dee    WORE
397000170208     c                   endif
397100170208     C                   EXSR      OREDAT
397200170208     c                   if        wsolodam<>'S'
397300170208     c                   movel     wdat          d98dtctd
397400170208     c                   endif
397500170208     C                   movel     WDAT          D98dam_rtc
397600170208    2c                   endsl
397700170208     c
397800170208     c
397900170208     c                   ENDSR
398000150218     C********************************************************************
398100150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
398200150218     C********************************************************************
398300150218     c     LetBRV        BEGSR
398400150219     c                   clear                   bollaOK           1
398500150219     c                   clear                   IDnpg
398600150219     c                   clear                   IDdata
398700150219     c                   clear                   IDora
398800150219     c                   clear                   IDfgs
398900150219     c
399000150218     C     Kart          SETLL     FNART01L
399100150218     C     Kart          READE     FNART01L
399200151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
399300150219     c                   clear                   DISfgs
399400150219     c                   clear                   DISdata
399500150219     c                   clear                   DISnpg
399600150219     c                   clear                   DISora
399700150219     c                   clear                   IMPdata
399800150219     c                   clear                   IMPfgs
399900150508     c* per ogni collo verifico se in IMP o disguidato
400000150218     c* al primo che NON trovo, esco come --> OK
400100150218     c     kbrv          setll     fnbrv07l
400200150218     c     kbrv          reade     fnbrv07l
400300150219    2c                   dow       not %eof(fnbrv07l)
400400150219    3c                   if        brvatr='D'  or brvnpg=3
400500150218     c* prendo la data della spunta
400600150218     c                   clear                   fnlv53ds
400700150219     c                   eval      d53tla='L'
400800150218     C                   MOVEL     BRvNPG        D53NPG
400900150218     C                   Z-ADD     BRvNFV        D53NFV
401000150218     C                   MOVEL     BRvFGS        D53FGS
401100150218     C                   CALL      'FNLV53R'
401200150218     C                   PARM                    fnlv53ds
401300150218    4c                   if        d53err<>*blanks
401400150219    5c                   if        brvdcs>0
401500150218     c                   eval      d53dfv=brvdcs
401600150218     c                   else
401700150219    6c                   if        brvdfs>0
401800150218     c                   eval      d53dfv=brvdfs
401900150219    6c                   endif
402000150219    5c                   endif
402100150219    4c                   endif
402200150218     c
402300150219    4c                   select
402400150219     c* se si tratta di DISGUIDO
402500150218     c                   when      brvatr='D'
402600150219     c
402700150219    5c                   if        d53dfv=DISdata
402800150219     c* Verifico l'ora
402900150219    6c                   if        brvhcs>0 and brvhcs>DISora
403000150219     c                   eval      DISdata=d53dfv
403100150219     c                   eval      DISfgs =brvfgs
403200150219     c                   eval      DISnpg =brvnpg
403300150219     c                   eval      DISora =brvhcs
403400150219   x6c                   else
403500150219    7c                   if        brvhms>0 and brvhms>DISora
403600150219     c                   eval      DISdata=d53dfv
403700150219     c                   eval      DISfgs =brvfgs
403800150219     c                   eval      DISnpg =brvnpg
403900150219     c                   eval      DISora =brvhms
404000150219    7c                   endif
404100150219    6c                   endif
404200150219    5c                   endif
404300150219     c
404400150219    5c                   if        d53dfv>DISdata
404500150219     c                   eval      DISdata=d53dfv
404600150219     c                   eval      DISfgs =brvfgs
404700150219     c                   eval      DISnpg =brvnpg
404800150219     c                   eval      DISora =brvhcs
404900150219     c                   if        disora=0
405000150219     c                   eval      DISora =brvhms
405100150219     c                   endif
405200150219    5c                   endif
405300150219     c
405400151009     c* se si tratta di IMP
405500151009    4c                   when      d53spg='P' and d53dfv>impdata
405600151009     c                   eval      IMPdata=d53dfv
405700151009     c                   eval      IMPfgs =brvfgs
405800150219    4c                   endsl
405900150219    3c                   endif
406000151009
406100150219     c     kbrv          reade     fnbrv07l
406200150219    2c                   enddo
406300150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
406400150219     c*  esco dal ciclo: la bolla è OK
406500150219    2c                   if        impdata=0 and disdata=0
406600150219     c                   eval      bollaOK='S'
406700150219     c                   clear                   IDdata
406800150219     c                   clear                   IDfgs
406900150219     c                   clear                   IDora
407000150219     c                   clear                   IDnpg
407100150219   x2c                   else
407200150219     c* memorizzo la data più alta
407300150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
407400150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
407500150219     c                   eval      IDdata=IMPdata
407600150219     c                   eval      IDfgs=IMPfgs
407700150219     c                   eval      IDora=0
407800150219     c                   eval      IDnpg='P'
407900150219    3c                   endif
408000150219    3c                   if        (DISdata>0 and DISdata=IDdata
408100150219     c                             and IDnpg<>'P' and DISora>IDora) or
408200150219     c                             (DISdata>0 and DISdata>IDdata)
408300150219     c                   eval      IDdata=DISdata
408400150219     c                   eval      IDfgs=DISfgs
408500150219     c                   eval      IDora=DISora
408600150219     c                   eval      IDnpg=%char(DISnpg)
408700150219    3c                   endif
408800150219    2c                   endif
408900150218     c
409000150218     C     Kart          READE     FNART01L
409100150219    1c                   enddo
409200150219     c
409300150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
409400150508     c**                 if        d98duc> 0 and d98duc>IDdata
409500150508     c**                 eval      bollaOK='S'
409600150508     c**                 clear                   IDdata
409700150508     c**                 clear                   IDfgs
409800150508     c**                 clear                   IDora
409900150508     c**                 clear                   IDnpg
410000150508     c**                 endif
410100150219     c
410200150218     c
410300150218     C                   ENDSR
410400140825     C********************************************************************
410500140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
410600140825     C********************************************************************
410700140825     c     TRAS_DEI      BEGSR
410800140825     C     WPODEI        IFEQ      *ZEROS
410900140825     C                   Z-ADD     *ZEROS        D98DEI
411000140825     C                   Z-ADD     *ZEROS        D98OEI
411100140825     C                   ELSE
411200140825     C                   Z-ADD     WPODEI        WORE
411300140825     C                   SUB       12            WORE
411400140825     C                   EXSR      OREDAT
411500140825     c
411600140825     C                   Z-ADD     WDAT          D98DEI
411700140825     c                   if        d98tci<>10
411800140825     C                   ADD       12            WRES
411900140825     c                   else
412000140825     C                   ADD       10            WRES
412100140825     c                   endif
412200140825     c
412300140825     C     WRES          MULT      100           D98OEI
412400140825     c
412500140825     c                   if        d98oei=1000
412600140825     c                   eval      d98oei=1030
412700140825     c                   endif
412800140825     c
412900140825     C                   END
413000140825     c                   ENDSR
413100930604     C********************************************************************
413200930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
413300930604     C********************************************************************
413400930604     C     CHKFOR        BEGSR
413500930604     C*
413600930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
413700930604     C* E QUINDI FORZO LA CONFORMITA' A SI
413800941227     C     D98ERR        IFEQ      '0'
413900930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
414000930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
414100930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
414200930604     C                   MOVEL     '1'           D98ERR
414300930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
414400930604     C                   END
414500930604     C                   END
414600990212     C*
414700990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
414800990212     C* E QUINDI FORZO LA CONFORMITA' A SI
414900990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
415000990212     C     D98ERR        IFEQ      '0'
415100990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
415200141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
415300990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
415400990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
415500990212     C                   MOVEL     '1'           D98ERR
415600990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
415700990212     C                   END
415800990212     C                   END
415900990308     C*
416000990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
416100990308     C* E QUINDI FORZO LA CONFORMITA' A SI
416200100614     C**   D98ERR        IFEQ      '0'
416300100614     C**   WFBR          IFEQ      'S'                                          *RESO
416400100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
416500100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
416600100614     C**                 MOVEL     '1'           D98ERR
416700100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
416800100614     C**                 END
416900100614     C**                 END
417000930604     C*
417100930604     C                   ENDSR
417200990212     C********************************************************************
417300990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
417400990212     C********************************************************************
417500990212     C     CHKDAN        BEGSR
417600990212     C*
417700990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
417800150603     c                   movel     'N'           opendan           1
417900040809      *
418000040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
418100040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
418200040809      * legate alla spedizione che passo
418300040809     c                   clear                   fidn12ds
418400040809     c                   eval      i12aas = D98AAS
418500040809     c                   eval      i12lnp = D98LNP
418600040809     c                   eval      i12nrs = D98NRS
418700040809     c                   eval      i12nsp = D98NSP
418800040809     c                   eval      i12fel = 'E'
418900040809     c                   eval      i12fan = 'E'
419000040809     c*
419100040809     c                   call      'FIDN12R'
419200040809     c                   parm                    fidn12ds
419300040809     c*
419400040809     c* se non ci sono errori
419500040809     c                   if        o12err = ' '
419600040809     c* se numero di CA trovate maggiore di zero
419700040809     c                   if        o12nca > 0
419800040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
419900150603     c
420000161215     c                   z-add     1             aa                2 0
420100161215     c                   dow       aa <= o12nca
420200150603     C* APERTA
420300161215     C***  skdch(aa)     IFgt      0
420400161215     c****               movel     'S'           opendan
420500161215     c***                endif
420600161215     c                   if        skdca(aa)>0 and skdca(aa)>d98dac
420700161215     c                   eval      d98dac =skdca(aa)
420800161215     c                   eval      d98dcc =skdch(aa)
420900161215     c                   endif
421000150603     c*
421100161215     c                   add       1             aa
421200161215     c                   enddo
421300150603     c
421400040809     c                   endif
421500040809     c                   endif
421600990212     C*
421700990212     C                   ENDSR
421800941228     C********************************************************************
421900941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
422000941228     C********************************************************************
422100941228     C     CALORP        BEGSR
422200941228     C*
422300941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
422400941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
422500941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
422600960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
422700950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
422800950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
422900950110     C                   ELSE
423000950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
423100950110     C                   ENDIF
423200941228     C                   EXSR      RO2COP
423300941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
423400941228     C* SPED. IL GIORNO DOPO
423500941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
423600941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
423700941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
423800941228     C*
423900941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
424000941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
424100941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
424200941228     C                   Z-ADD     WPODSP        WR2POI
424300950110     C                   Z-ADD     WPOPPC        WR2POF
424400941228     C                   EXSR      RO2COP
424500941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
424600941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
424700941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
424800941228     C*
424900941228     C                   ENDSR
425000930414     C********************************************************************
425100930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
425200930414     C********************************************************************
425300930414     C     CALORT        BEGSR
425400930414     C*
425500930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
425600931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
425700930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
425800931126     C                   Z-ADD     *ZEROS        W98HF
425900950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
426000950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
426100950110     C                   ELSE
426200950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
426300950110     C                   ENDIF
426400931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
426500931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
426600930608     C                   ELSE
426700931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
426800930608     C                   END
426900931126     C                   EXSR      RO2COV
427000931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
427100930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
427200930414     C*
427300930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
427400930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
427500930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
427600931126     C                   Z-ADD     *ZEROS        W98HF
427700950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
427800931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
427900931126     C                   EXSR      RO2COV
428000931126     C                   Z-ADD     WR2NOV        W98RTC
428100930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
428200930414     C*
428300930414     C                   ENDSR
428400930414     C********************************************************************
428500930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
428600930414     C********************************************************************
428700930414     C     CALORC        BEGSR
428800930511     C*
428900930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
429000930511     C* I TEMPI DI RITARDO AD OGGI
429100930511     C     WPODCM        IFEQ      *ZEROS
429200960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
429300930511     C                   END
429400940117     C*
429500940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
429600940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
429700940117     C* ALLA DATA DI FINE CONSEGNABILITA'
429800940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
429900940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
430000940118££££ C     WPODFC        IFNE      *ZEROS
430100940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
430200940118££££ C                   END
430300930414     C*
430400930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
430500930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
430600930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
430700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
430800931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
430900930608     C                   ELSE
431000931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
431100930608     C                   END
431200931126     C                   Z-ADD     WPODCM        WR5POF            9 0
431300931126     C                   EXSR      RO5COV
431400931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
431500930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
431600930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
431700060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
431800060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
431900930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
432000930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
432100930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
432200930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
432300930414     C*
432400930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
432500930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
432600930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
432700931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
432800931126     C                   Z-ADD     WPODCM        WR5POF            9 0
432900931126     C                   EXSR      RO5COV
433000931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
433100930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
433200930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
433300060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
433400060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
433500930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
433600930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
433700930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
433800930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
433900930414     C*
434000930414     C                   ENDSR
434100930414     C********************************************************************
434200930414     C*   CALORR - CALCOLA ORE REALI DI RESA
434300930414     C********************************************************************
434400930414     C     CALORR        BEGSR
434500930414     C*
434600950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
434700950110     C                   ADD       W98RTI        W98RRI
434800941228     C                   ADD       W98RCI        W98RRI
434900950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
435000950110     C                   ADD       W98RTC        W98RRC
435100941228     C                   ADD       W98RCC        W98RRC
435200930414     C*
435300930414     C                   ENDSR
435400930420     C********************************************************************
435500930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
435600930420     C********************************************************************
435700930420     C     CALDIF        BEGSR
435800930420     C*
435900941228     C* PARTENZA
436000941228     C     W98RPI        SUB       D98TPI        W98FPI
436100941228     C     W98RPC        SUB       D98TPC        W98FPC
436200941228     C*
436300930420     C* TRAZIONE
436400930423     C     W98RTI        SUB       D98TTI        W98FTI
436500930423     C     W98RTC        SUB       D98TTC        W98FTC
436600930420     C*
436700930420     C* CONSEGNA
436800090827     C***  W98RCI        SUB       D98TCI        W98FCI
436900090827     C     W98RCI        SUB       w99TCI        W98FCI
437000090827     C***  W98RCC        SUB       D98TCC        W98FCC
437100090827     C     W98RCC        SUB       w99TCC        W98FCC
437200930420     C*
437300930420     C* RESA
437400090921     C***  W98RRI        SUB       D98TRI        W98FRI
437500090921     C     W98RRI        SUB       w99TRI        W98FRI
437600090921     c
437700090921     C***  W98RRC        SUB       D98TRC        W98FRC
437800170104     c
437900170104     c*** se le ore reali interne di RESA usate sono < delle ore reali cliente, uso le
438000170104     c*    reali interne
438100170104     c                   if        w98rri<w98rrc
438200170104     C     W98RRI        SUB       w99TRC        W98FRC
438300170104     c                   else
438400170104     C     W98RRC        SUB       w99TRC        W98FRC
438500170104     c                   endif
438600930420     C*
438700930420     C                   ENDSR
438800930420     C********************************************************************
438900930420     C*   CALCNF - CALCOLA LE CONFORMITA'
439000930420     C********************************************************************
439100930420     C     CALCNF        BEGSR
439200941228     C*
439300941228     C*-------------------------------------------------------------------
439400941228     C* CONFORMITA' PARTENZA  INTERNA
439500941228     C*-------------------------------------------------------------------
439600941228     C                   Z-ADD     W98FPI        W98NPI
439700941228     C     W98FPI        IFLE      *ZEROS
439800941228     C                   MOVEL     'S'           D98CPI
439900941228     C                   ELSE
440000941228     C                   MOVEL     'N'           D98CPI
440100941228     C                   END
440200941228     C*
440300941228     C*-------------------------------------------------------------------
440400941228     C* CONFORMITA' PARTENZA  CLIENTE
440500941228     C*-------------------------------------------------------------------
440600941228     C                   Z-ADD     W98FPC        W98NPC
440700941228     C     W98FPC        IFLE      *ZEROS
440800941228     C                   MOVEL     'S'           D98CPC
440900941228     C                   ELSE
441000941228     C                   MOVEL     'N'           D98CPC
441100941228     C                   END
441200930420     C*
441300930420     C*-------------------------------------------------------------------
441400930420     C* CONFORMITA' TRAZIONE  INTERNA
441500930420     C*-------------------------------------------------------------------
441600930510     C                   Z-ADD     W98FTI        W98NTI
441700930423     C     W98FTI        IFLE      *ZEROS
441800930420     C                   MOVEL     'S'           D98CTI
441900930420     C                   ELSE
442000930420     C                   MOVEL     'N'           D98CTI
442100930420     C                   END
442200930420     C*
442300930420     C*-------------------------------------------------------------------
442400930420     C* CONFORMITA' TRAZIONE  CLIENTE
442500930420     C*-------------------------------------------------------------------
442600930510     C                   Z-ADD     W98FTC        W98NTC
442700930423     C     W98FTC        IFLE      *ZEROS
442800930420     C                   MOVEL     'S'           D98CTC
442900930420     C                   ELSE
443000930420     C                   MOVEL     'N'           D98CTC
443100930420     C                   END
443200930420     C*
443300930420     C*-------------------------------------------------------------------
443400930420     C* CONFORMITA' CONSEGNA  INTERNA
443500930420     C*-------------------------------------------------------------------
443600150226     c                   if        wmancatraz='S'
443700150226     c                   clear                   d98dti
443800150226     c                   clear                   d98hti
443900150226     c                   endif
444000150226     c
444100930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
444200930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
444300930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
444400050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
444500050914     c                   if        consideraora='S'
444600050914     c     w98fci        sub       24            w98nci
444700050914     c                   add       24            d98hrc
444800050914     c                   else
444900930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
445000050914     c                   endif
445100080116     c
445200110923     c                   select
445300080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
445400080116     c*  --> trasformo il ritardo in 0
445500090827     c                   when      d98cca='2'
445600080116     c                   z-add     0             w98nci
445700090827
445800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
445900080116     c*  e sto leggendo la bolla in arrivo
446000080116     c*  --> trasformo il ritardo = 0
446100090827     c                   when      d98cca='1' and d98tbo='A'
446200080116     c                   z-add     0             w98nci
446300090902     c
446400090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
446500090902     c                   when      wmancaTRAZ='S'
446600090902     c                   z-add     0             w98nci
446700090902     c                   clear                   d98dti
446800090902     c                   clear                   d98hti
446900110923     c
447000110923     c* Ignoro ritardo se pefect order
447100130221     c                   when      d98xco >*zeros
447200110923     c                   z-add     0             w98nci
447300090827     c
447400090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
447500091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
447600091014     c*  controllo ora soglia  consegna da tabella OSC)
447700091009     c                   when      d98tci=10   and
447800091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
447900091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
448000091014     c*
448100091014     c                             d98hci=1030
448200091014     C                   MOVEL     'OSC  '       WR3TAB
448300091014     C                   eval      WR3HHI=d98hmc
448400091014     C                   EXSR      RO3ORE
448500131218
448600091014     c                   if        wr3matpom='M' and d98hmc>1030
448700131218     c                   exsr      ctrLAVH1030
448800131218
448900131218     c                   if        wlav1030<>'S'
449000091014     c                   add       12            w98nci
449100091014     c                   endif
449200131218     c                   endif
449300090928     c
449400090827     c                   endsl
449500050914     c
449600930524     C                   ELSE                                                   <-- 1 -->
449700930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
449800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
449900930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
450000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
450100930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
450200930608     C                   ELSE
450300930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
450400930608     C                   END
450500930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
450600930525     C                   EXSR      RO2COV
450700930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
450800930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
450900930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
451000930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
451100931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
451200930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
451300930608     C                   ELSE
451400930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
451500930608     C                   END
451600931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
451700930525     C                   EXSR      RO2COV
451800930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
451900930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
452000930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
452100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
452200930525     C* E' IL RITARDO
452300930610     C     WNOCRI        SUB       W98RCI        W98NCI
452400050901     c
452500050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
452600050901     c*  --> trasformo il ritardo in anticipo
452700050901     c                   if        d98cca='2'
452800080116     c***                z-sub     w98nci        w98nci
452900080116     c                   z-add     0             w98nci
453000050901     c                   endif
453100111018     c* Ignoro ritardo se pefect order
453200130221     c                   if        d98xco >*zeros
453300111018     c                   z-add     0             w98nci
453400111018     c                   endif
453500061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
453600061121     c*  e sto leggendo la bolla in arrivo
453700061121     c*  --> trasformo il ritardo in anticipo
453800061121     c                   if        d98cca='1' and d98tbo='A'
453900080116     c***                z-sub     w98nci        w98nci
454000080116     c                   z-add     0             w98nci
454100061121     c                   endif
454200070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
454300070827     c*  mittente, --> consegnato in anticipo
454400070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
454500140403     c                   if        Callar5=' '
454600070827     c                   exsr      LeggiAR5
454700140403     c                   endif
454800070827     c
454900070827     c                   if        dcrpart<>'S'
455000070824     c                   z-sub     w98nci        w98nci
455100070824     c                   endif
455200070827     c
455300070824     c                   endif
455400050907     c
455500050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
455600050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
455700050907     c*  lo trasformo in anticipo perchè non sono in ritardo
455800050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
455900050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
456000050916     c***                if        d98dfc>0
456100070828     c                   if        d98dcm=0  and w98nci>0
456200050907     c                   z-sub     w98nci        w98nci
456300050907     c                   sub       oreILoDOPO    w98nci
456400050907     c                   endif
456500050901     c
456600930525     C                   ELSE                                                   <-- 2 -->
456700070521     c* non do ritardo se la consegna reale cade nel giorno
456800070521     c*  della consegna richiesta se presente IL
456900100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
457000070521     c                             wpodcm<=wpocr1
457100070521     c                   clear                   w98nci
457200100224     c                   else                                                   ---2a -->
457300930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
457400090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
457500090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
457600930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
457700930525     C* E' IL RITARDO O L'ANTICIPO
457800930525     C     W98RCI        SUB       WNOCRF        W98NCI
457900070115     c
458000070115     c* non do ritardo se comunque la consegna reale cade nel giorno
458100070115     c*  della consegna richiesta se presente IL
458200070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
458300070521     c***                          wpodcm<=wpocr1  and w98nci>0
458400070521     c***                clear                   w98nci
458500070521     c***                endif
458600070115     c
458700930525     C                   ELSE                                                   <-- 3 -->
458800930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
458900930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
459000930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
459100930525     C                   END                                                    <-- 3 ---
459200100224     C                   END                                                    <-- 2a---
459300100224
459400100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
459500100224     c                   select
459600100224     c                   when      d98cca='2'
459700100224     c                   z-add     0             w98nci
459800100224
459900100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
460000100224     c*  e sto leggendo la bolla in arrivo
460100100224     c                   when      d98cca='1' and d98tbo='A'
460200100224     c                   z-add     0             w98nci
460300100224     c                   endsl
460400100224     c
460500100224     C                   END                                                    <-- 2 ---
460600070521     C                   END                                                    <-- 1 ---
460700070521     c
460800970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
460900970915     C*/         WAZNCI    IFEQ 'S'
461000970915     C*/         W98NCI    ANDLT*ZEROS
461100970915     C*/                   Z-ADD*ZEROS    W98NCI
461200970915     C*/                   ENDIF
461300970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
461400050922     c* lo faccio solo se non c'e' la data cons richiesta
461500970915     C     D98DTI        IFEQ      *ZEROS
461600050922     c     d98dcr        andeq     *zeros
461700970915     C                   Z-ADD     *ZEROS        W98NCI
461800970915     C                   ENDIF
461900930524     C*
462000930524     C* CONTROLLO CONFORMITA
462100930423     C     W98NCI        IFLE      *ZEROS
462200930524     C                   MOVEL     'S'           D98CCI
462300930420     C                   ELSE
462400930524     C                   MOVEL     'N'           D98CCI
462500930420     C                   END
462600930420     C*
462700930420     C*-------------------------------------------------------------------
462800930420     C* CONFORMITA' CONSEGNA  CLIENTE
462900930420     C*-------------------------------------------------------------------
463000930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
463100930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
463200930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
463300050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
463400050914     c                   if        consideraora='S'
463500050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
463600050914     c                   else
463700050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
463800050914     c                   endif
463900080116     c
464000080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
464100080116     c*  --> trasformo il ritardo =0
464200090827     c                   select
464300090827     c                   when      d98cca='2'
464400080116     c                   z-add     0             w98ncc
464500090827
464600080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
464700080116     c*  e sto leggendo la bolla in arrivo
464800080116     c*  --> trasformo il ritardo =0
464900090827     c                   when      d98cca='1' and d98tbo='A'
465000080116     c                   z-add     0             w98ncc
465100090902     c
465200090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
465300090902     c                   when      wmancaTRAZ='S'
465400090902     c                   z-add     0             w98ncc
465500110923     c* Ignoro ritardo se Perfect Order
465600130221     c                   when      d98xco >*zeros
465700110923     c                   z-add     0             w98ncc
465800090902     c
465900090827     c
466000090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
466100090827     c* sono  in ritardo di 12 ore
466200091009     c                   when      d98tcc=10  and
466300091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
466400091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
466500091014     c*
466600091014     c                             d98hce=1030
466700091014     C                   MOVEL     'OSC  '       WR3TAB
466800091014     C                   eval      WR3HHI=d98hmc
466900091014     C                   EXSR      RO3ORE
467000091014     c                   if        wr3matpom='M' and d98hmc>1030
467100131218     c                   exsr      ctrLAVH1030
467200131218
467300131218     c                   if        wlav1030<>'S'
467400131218     c                   add       12            w98ncc
467500131218     c                   endif
467600131218     c****               add       12            w98ncc
467700091014     c                   endif
467800131218     c
467900091014     c***                z-add     12            w98ncc
468000090827     c                   endsl
468100050914     c
468200930524     C                   ELSE                                                   <-- 1 -->
468300930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
468400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
468500930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
468600930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
468700930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
468800930525     C                   EXSR      RO2COV
468900930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
469000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
469100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
469200930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
469300930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
469400931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
469500930525     C                   EXSR      RO2COV
469600930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
469700930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
469800930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
469900930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
470000930525     C* E' IL RITARDO
470100930525     C     WNOCRI        SUB       W98RCC        W98NCC
470200050901     c
470300050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
470400050901     c*  --> trasformo il ritardo in anticipo
470500050901     c                   if        d98cca='2'
470600080116     c***                z-sub     w98ncc        w98ncc
470700080116     c                   z-add     0             w98ncc
470800050901     c                   endif
470900111018     c* Ignoro ritardo se pefect order
471000130221     c                   if        d98xco >*zeros
471100111018     c                   z-add     0             w98ncc
471200111018     c                   endif
471300061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
471400061121     c*  e sto leggendo la bolla in arrivo
471500061121     c*  --> trasformo il ritardo in anticipo
471600061121     c                   if        d98cca='1' and d98tbo='A'
471700080116     c***                z-sub     w98ncc        w98ncc
471800080116     c                   z-add     0             w98ncc
471900061121     c                   endif
472000070824     c
472100070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
472200070827     c*  mittente, --> consegnato in anticipo
472300070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
472400070827     c* SBR per vedere se tassativa
472500070827     c                   if        Callar5=' '
472600070827     c                   EXSR      LeggiAR5
472700070827     c                   endif
472800070827     c                   if        dcrpart<>'S'
472900070824     c                   z-sub     w98ncc        w98ncc
473000070824     c                   endif
473100070824     c                   endif
473200050907     c
473300050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
473400050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
473500050907     c*  lo trasformo in anticipo perchè non sono in ritardo
473600050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
473700050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
473800050916     c***                if        d98dfc>0
473900070828     c                   if        d98dcm=0 and w98ncc>0
474000050907     c                   z-sub     w98ncc        w98ncc
474100050907     c                   sub       oreILoDOPO    w98ncc
474200050907     c                   endif
474300050901     c
474400930525     C                   ELSE                                                   <-- 2 -->
474500070521     c* Non do ritardo se la consegna reale cade nel giorno
474600070521     c*  della consegna richiesta se presente IL
474700100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
474800070521     c                             wpodcm<=wpocr2
474900070521     c                   clear                   w98ncc
475000100224     c                   else                                                   <--2a -->
475100070521     c
475200930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
475300090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
475400090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
475500930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
475600930525     C* E' IL RITARDO O L'ANTICIPO
475700930525     C     W98RCC        SUB       WNOCRF        W98NCC
475800070115     c
475900070115     c* non do ritardo se comunque la consegna reale cade nel giorno
476000070115     c*  della consegna richiesta se presente IL
476100070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
476200070521     c***                          wpodcm<=wpocr2 and w98ncc>0
476300070521     c***                clear                   w98ncc
476400070521     c***                endif
476500070115     c
476600930525     C                   ELSE                                                   <-- 3 -->
476700930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
476800930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
476900930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
477000930525     C                   END                                                    <-- 3 ---
477100100224     C                   END                                                    <-- 2a---
477200100224
477300100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
477400100224     c                   select
477500100224     c                   when      d98cca='2'
477600100224     c                   z-add     0             w98nci
477700100224
477800100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
477900100224     c*  e sto leggendo la bolla in arrivo
478000100224     c                   when      d98cca='1' and d98tbo='A'
478100100224     c                   z-add     0             w98nci
478200100224     c                   endsl
478300100224     c
478400100224     C                   END                                                    <-- 2 ---
478500070521     C                   END                                                    <-- 1 ---
478600930524     C*
478700930524     C* CONTROLLO CONFORMITA
478800930423     C     W98NCC        IFLE      *ZEROS
478900930524     C                   MOVEL     'S'           D98CCC
479000930420     C                   ELSE
479100930524     C                   MOVEL     'N'           D98CCC
479200930420     C                   END
479300930420     C*
479400930420     C*-------------------------------------------------------------------
479500930420     C* CONFORMITA' RESA INTERNA
479600930420     C*-------------------------------------------------------------------
479700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
479800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
479900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
480000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
480100050914     c                   if        consideraora='S'
480200050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
480300050914     c                   else
480400050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
480500050914     c                   endif
480600080116     c
480700080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
480800090827     c                   select
480900090827     c                   when      d98cca='2' and w98nri>0
481000080116     c                   z-add     0             w98nri
481100090827     c
481200080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
481300080116     c*  e sto leggendo la bolla in arrivo
481400090827     c                   when      d98cca='1' and d98tbo='A' and
481500080116     c                                            w98nri>0
481600080116     c                   z-add     0             w98nri
481700110923     c
481800110923     c* Ignoro ritardo se Perfect Order
481900130221     c                   when      d98xco >*zeros
482000110923     c                   z-add     0             w98nri
482100110923     c
482200090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
482300090827     c* sono  in ritardo di 12 ore
482400091009     c                   when      d98tci=10  and
482500091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
482600091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
482700091014     c*
482800091014     c                             d98hci=1030
482900091014     C                   MOVEL     'OSC  '       WR3TAB
483000091014     C                   eval      WR3HHI=d98hmc
483100091014     C                   EXSR      RO3ORE
483200091014     c                   if        wr3matpom='M' and d98hmc>1030
483300131218     c                   exsr      ctrLAVH1030
483400131218
483500131218     c                   if        wlav1030<>'S'
483600091014     c                   add       12            w98nri
483700091014     c                   endif
483800131218     c                   endif
483900091014     c
484000091014     c***                z-add     12            w98nri
484100090827     c                   endsl
484200090827
484300930524     C                   ELSE                                                   <-- 1 -->
484400930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
484500930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
484600930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
484700931206     C     WPODCM        ANDLE     WPOCR1
484800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
484900930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
485000930525     C                   Z-ADD     WPODCM        WR2POI            9 0
485100931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
485200930525     C                   EXSR      RO2COV
485300930525     C     WR2NOV        IFNE      *ZEROS
485400930525     C     WR2NOV        MULT      -1            W98NRI
485500930525     C                   END
485600930524     C                   ELSE                                                   <-- 2 -->
485700930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
485800930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
485900930524     C     WPODCM        IFLT      WPOCRI
486000930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
486100930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
486200930524     C                   ELSE
486300931206     C                   Z-ADD     WPOCR1        WPORFI
486400930524     C                   Z-ADD     WPODCM        WPORFF
486500930524     C                   END
486600930524     C* CALCOLO ORE DI DIFFERENZA
486700930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
486800930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
486900930524     C                   Z-ADD     WPORFI        WR2POI            9 0
487000930524     C                   Z-ADD     WPORFF        WR2POF            9 0
487100930524     C                   EXSR      RO2COV
487200930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
487300050902     c
487400050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
487500100224     c                   if        d98cca='2' and w98nri>0
487600100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
487700080116     c***                z-sub     w98nri        w98nri
487800080116     c                   z-add     0             w98nri
487900050902     c                   endif
488000111018     c* Ignoro ritardo se pefect order
488100130221     c                   if        d98xco >*zeros
488200111018     c                   z-add     0             w98nri
488300111018     c                   endif
488400061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
488500061121     c*  e sto leggendo la bolla in arrivo
488600100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
488700100224     c***                if        d98cca='1' and d98tbo='A' and
488800100224     c***                                         w98nri>0 and wpodcm<wpocri
488900080116     c***                z-sub     w98nri        w98nri
489000080116     c                   z-add     0             w98nri
489100061121     c                   endif
489200070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
489300070827     c*  mittente, --> consegnato in anticipo
489400070828     c                   if        d98tcr=' ' and d98nsp>0  and
489500070828     c                                            w98nri>0 and wpodcm<wpocri
489600070827     c                   if        CallAR5=' '
489700070827     c                   EXSR      LeggiAR5
489800070824     c                   endif
489900070827     c                   if        dcrpart<>'S'
490000070824     c                   z-sub     w98nri        w98nri
490100070824     c                   endif
490200070824     c                   endif
490300050907     c
490400050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
490500050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
490600050907     c*  lo trasformo in anticipo perchè non sono in ritardo
490700050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
490800050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
490900050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
491000050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
491100050907     c                   z-sub     w98nri        w98nri
491200050907     c                   sub       oreILoDOPO    w98nri
491300050907     c                   endif
491400050907     c
491500050907     C                   END                                                    <-- 2 ---
491600930524     C                   END                                                    <-- 1 ---
491700930420     C* CALCOLO LE ORE DI RESA
491800930420     C* CONTROLLO CONFORMITA
491900930423     C     W98NRI        IFLE      *ZEROS
492000930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
492100930420     C                   ELSE
492200930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
492300930420     C                   END
492400930420     C*
492500930420     C*-------------------------------------------------------------------
492600930420     C* CONFORMITA' RESA CLIENTE
492700930420     C*-------------------------------------------------------------------
492800930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
492900930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
493000930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
493100050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
493200050914     c                   if        consideraora='S'
493300050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
493400050914     c                   else
493500050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
493600050914     c                   endif
493700090827     c
493800080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
493900090827     c                   select
494000090827     c                   when      d98cca='2' and w98nrc>0
494100080116     c                   z-add     0             w98nrc
494200090827
494300080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
494400080116     c*  e sto leggendo la bolla in arrivo
494500090827     c                   when      d98cca='1' and d98tbo='A' and
494600080116     c                                            w98nrc>0
494700080116     c                   z-add     0             w98nrc
494800110923     c
494900110923     c* Ignoro ritardo se Perfect Order
495000130221     c                   when      d98xco >*zeros
495100110923     c                   z-add     0             w98nrc
495200090827     c
495300090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
495400090827     c* sono  in ritardo di 12 ore
495500091009     c                   when      d98tcc=10  and
495600091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
495700091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
495800091014     c                             d98hce=1030
495900091014     C                   MOVEL     'OSC  '       WR3TAB
496000091014     C                   eval      WR3HHI=d98hmc
496100091014     C                   EXSR      RO3ORE
496200091014     c                   if        wr3matpom='M' and d98hmc>1030
496300131218     c                   exsr      ctrLAVH1030
496400131218
496500131218     c                   if        wlav1030<>'S'
496600091014     c                   add       12            w98nrc
496700091014     c                   endif
496800131218     c                   endif
496900091014     c
497000091014     c**                 z-add     12            w98nrc
497100090827     c                   endsl
497200090827     c
497300930524     C                   ELSE                                                   <-- 1 -->
497400930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
497500930524     C* DI DIFFERENZA SONO UGUALI A ZERO
497600930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
497700931206     C     WPODCM        ANDLE     WPOCR2
497800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
497900930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
498000930525     C                   Z-ADD     WPODCM        WR2POI            9 0
498100931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
498200930525     C                   EXSR      RO2COV
498300930525     C     WR2NOV        IFNE      *ZEROS
498400930525     C     WR2NOV        MULT      -1            W98NRC
498500930525     C                   END
498600930524     C                   ELSE                                                   <-- 2 -->
498700930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
498800930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
498900930524     C     WPODCM        IFLT      WPOCRI
499000930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
499100930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
499200930524     C                   ELSE
499300931206     C                   Z-ADD     WPOCR2        WPORFI
499400930524     C                   Z-ADD     WPODCM        WPORFF
499500930524     C                   END
499600930524     C* CALCOLO ORE DI DIFFERENZA
499700930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
499800930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
499900930524     C                   Z-ADD     WPORFI        WR2POI            9 0
500000930524     C                   Z-ADD     WPORFF        WR2POF            9 0
500100930524     C                   EXSR      RO2COV
500200930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
500300050902     c
500400050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
500500100224     c                   if        d98cca='2' and w98nrc>0
500600100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
500700080116     c***                z-sub     w98nrc        w98nrc
500800080116     c                   z-add     0             w98nrc
500900050902     c                   endif
501000111018     c* Ignoro ritardo se pefect order
501100130221     c                   if        d98xco >*zeros
501200111018     c                   z-add     0             w98nrc
501300111018     c                   endif
501400070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
501500061121     c*  e sto leggendo la bolla in arrivo
501600100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
501700100224     c***                if        d98cca='1' and d98tbo='A' and
501800100224     c***                                         w98nrc>0 and wpodcm<wpocri
501900080116     c***                z-sub     w98nrc        w98nrc
502000080116     c                   z-add     0             w98nrc
502100061121     c                   endif
502200070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
502300070827     c*  mittente, --> consegnato in anticipo
502400070828     c                   if        d98tcr=' ' and d98nsp>0 and
502500070828     c                                            w98nrc>0 and wpodcm<wpocri
502600070827     c                   if        CallAR5=' '
502700070827     c                   EXSR      LeggiAR5
502800070827     c                   endif
502900070827     c                   if        dcrpart<>'S'
503000070824     c                   z-sub     w98nrc        w98nrc
503100070824     c                   endif
503200070824     c                   endif
503300050907     c
503400050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
503500050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
503600050907     c*  lo trasformo in anticipo perchè non sono in ritardo
503700050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
503800050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
503900050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
504000050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
504100050907     c                   z-sub     w98nrc        w98nrc
504200050907     c                   sub       oreILoDOPO    w98nrc
504300050907     c                   endif
504400050907     c
504500050907     C                   END                                                    <-- 2 ---
504600930524     C                   END                                                    <-- 1 ---
504700930420     C* CALCOLO LE ORE DI RESA
504800930420     C* CONTROLLO CONFORMITA
504900930423     C     W98NRC        IFLE      *ZEROS
505000930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
505100930420     C                   ELSE
505200930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
505300930420     C                   END
505400930420     C*
505500930420     C                   ENDSR
505600070827     c
505700131218     c********************************************************************
505800131218     c     CtrlavH1030   BEGSR
505900131218     c********************************************************************
506000131218     c                   clear                   wlav1030          1
506100140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
506200131218     c                   z-add     1             xx                4 0
506300131218     c                   dow       evc(xx)<>*blanks
506400140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
506500140127     c                   if        evd(xx)=d98dcm
506600131218     c                   if        evo(xx)<=1030
506700131218     c                   eval      wlav1030='S'
506800131218     c                   leave
506900131218     c                   endif
507000131218     c                   endif
507100131218     c                   add       1             xx
507200131218     c                   enddo
507300131218     c                   ENDSR
507400070827     c********************************************************************
507500070827     c* Controllo record GEN di FIAR5
507600070827     c********************************************************************
507700070827     c     LeggiAR5      BEGSR
507800070827     c                   clear                   DCRPART           1
507900141104     c                   clear                   dar5gen
508000141104     c                   clear                   gc1PART           2
508100141104     c                   clear                   gc2PART           2
508200070827     c     kar5          chain     fiar501l
508300070827    1c                   if        not %found
508400070827     c                   eval      dcrpart='S'
508500141104     c                   eval      gc1part=d98gc1
508600141104     c                   eval      gc2part=d98gc2
508700070827   x1c                   else
508800070827     c* i dati devono essere quelli immessi dalla partenza
508900070827     c                   movel     ar5uni        dar5gen
509000070827     c
509100070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
509200070827     c*  ma il record GEN è stato scritto per altri motivi
509300070827    2c                   if        §ar5mdp='S'
509400070827     c                   movel     d98dcr        alfadcr           8
509500141104     c                   eval      gc1part=§ar5gc1
509600141104     c                   eval      gc2part=§ar5gc2
509700070827     c
509800070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
509900070827     c                   eval      dcrpart='S'
510000070827    3c                   endif
510100070827     c
510200070827   x2c                   else
510300070827     c                   eval      dcrpart='S'
510400141106     c                   eval      gc1part=d98gc1
510500141106     c                   eval      gc2part=d98gc2
510600070827    2c                   endif
510700070827    1c                   endif
510800070827     c
510900140402     c* Memorizzo il flag pickup 2
511000140402     c                   eval      wpick2=§AR5PK2
511100140402     c
511200150401     c*
511300150414     c**                 if        d98cas>0 or d98ffd<>' ' or
511400150414     c**                           d98tc1='A'  or d98tc2='A' or
511500150414     c**                           d98tc1='S'  or d98tc2='S'
511600160331     c                   eval      kar4trc='L'
511700150401     c     kar4          chain     fiar401l
511800160420    1c                   if        %found(fiar401l)
511900150401     c                   eval      dsbl4L=ar4not
512000160420   x1c                   else
512100160420    1c                   endif
512200160420
512300150401     c* Solo per fermo deposito verifico se inviato alert
512400150401     c                   if        d98ffd<>' '
512500150401     c                   clear                   fnlv82ds
512600150401     c                   eval      ILV82AAS=d98aas
512700150401     c                   eval      ILV82LNP=d98lnp
512800150401     c                   eval      ILV82NRS=d98nrs
512900150401     c                   eval      ILV82NSP=d98nsp
513000150401     c                   eval      ILV82TIPA='FD'
513100150401     c                   call      'FNLV82R'
513200150401     c                   parm                    fnlv82ds
513300150401     c                   endif
513400150414     c**                 endif
513500150401
513600150401     c                   eval      callar5='S'
513700160331     c
513800160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
513900160331     c                   if        %lookup(d98lnp:fildpd)>0
514000160331     c                   eval      kar4trc='I'
514100160331     c     kar4          chain     fiar401l
514200160331     c                   if        %found(fiar401l)
514300160331     c                   eval      dsbl4I=ar4not
514400160331     c                   endif
514500160401     c                   if        §b4trd='1' or §b4trd='2'
514600160401     c                   eval      wVP='S'
514700160401     c                   endif
514800160401     c
514900160331     c                   endif
515000150401
515100070827     c                   ENDSR
515200930913     C********************************************************************
515300930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
515400930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
515500930913     C*            DATA/ORA (FINE RANGE)
515600930913     C********************************************************************
515700930913     C     CALCNS        BEGSR
515800930914     C*
515900930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
516000930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
516100930914     C     WPODTI        IFEQ      *ZEROS
516200960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
516300930914     C                   ELSE
516400930914     C                   Z-ADD     WPODTI        WPOIRC
516500930914     C                   END
516600930914     C*
516700930914     C* SE NON ANCORA CONSEGNATA
516800930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
516900050922     c* in realtà nella data consegna c'e' la data fine calcolo
517000050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
517100930914     C     WPODCM        IFEQ      *ZEROS
517200050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
517300930914     C                   ELSE
517400930916     C                   Z-ADD     WPODCM        WPOFRC
517500930914     C                   END
517600930914     C*
517700930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
517800930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
517900930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
518000930914     C     WPODFC        ANDLT     WPOIRC
518100930914     C                   ELSE
518200930914     C*
518300930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
518400930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
518500930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
518600930914     C     WPODFC        ANDGT     WPOFRC
518700930914     C                   ELSE
518800930916     C*
518900930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
519000930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
519100930916     C     WPODFC        ANDGT     WPOFRC
519200930916     C                   ELSE
519300930916     C*
519400930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
519500930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
519600930916     C     WPODFC        ANDLT     WPOIRC
519700930916     C                   ELSE
519800930914     C*
519900930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
520000930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
520100930914     C     WPODIC        IFNE      *ZEROS
520200930914     C     WPODIC        ANDGT     WPOFRC
520300930914     C                   Z-ADD     WPOFRC        WPODIC
520400930914     C                   ENDIF
520500930914     C*
520600930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
520700930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
520800930914     C     WPODFC        IFGT      WPOFRC
520900930914     C                   Z-ADD     WPOFRC        WPODFC
521000930914     C                   ENDIF
521100930914     C*
521200930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
521300930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
521400930914     C     WPODIC        IFNE      *ZEROS
521500930914     C     WPODIC        ANDLT     WPOIRC
521600930914     C                   Z-ADD     WPOIRC        WPODIC
521700930914     C                   ENDIF
521800930914     C*
521900930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
522000930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
522100930914     C     WPODFC        IFLT      WPOIRC
522200930914     C                   Z-ADD     WPOIRC        WPODFC
522300930914     C                   ENDIF
522400930914     C*
522500930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
522600931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
522700930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
522800930916     C*
522900931210     C***********WPODIC    IFNE *ZEROS
523000931210     C***********WPODFC    ANDNE*ZEROS
523100931210     C*********************ADD  12        WPODFC
523200931210     C*********************ENDIF
523300930914     C     WPODIC        IFEQ      *ZEROS
523400930914     C     WPODFC        ANDNE     *ZEROS
523500931130     C                   Z-ADD     WPODFC        WPODIC
523600931210     C*********************ADD  12        WPODFC
523700930916     C                   ENDIF
523800930913     C*
523900930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
524000930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
524100930914     C* INTERNA
524200050908     c                   eval      Usacons='S'
524300930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
524400930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
524500931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
524600931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
524700940324     C                   EXSR      RO9COV
524800931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
524900930914     C* CLIENTE
525000930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
525100931210     C                   EXSR      RO9COV
525200931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
525300050908     c                   eval      Usacons=' '
525400930914     C*
525500930916     C                   ENDIF                                                  <-- 5 ---
525600930916     C                   ENDIF                                                  <-- 4 ---
525700930916     C                   ENDIF                                                  <-- 3 ---
525800930914     C                   ENDIF                                                  <-- 2 ---
525900930914     C                   ENDIF                                                  <-- 1 ---
526000930913     C*
526100930913     C                   ENDSR
526200930325     C********************************************************************
526300930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
526400930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
526500930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
526600930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
526700930325     C********************************************************************
526800930325     C     RO1CDP        BEGSR
526900930414     C*
527000931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
527100931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
527200931126     C*
527300931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
527400931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
527500150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
527600150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
527700931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
527800060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
527900930326     C*
528000930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
528100931130     C                   Z-ADD     WR1POI        WR1POO            9 0
528200930325     C*
528300930325     C* CICLO FINO A CHE:
528400930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
528500930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
528600930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
528700930406     C     WR1POO        ANDLT     WPOLIM
528800930414     C*
528900930407     C                   Z-ADD     WR1POO        WPOROU            9 0
529000930325     C                   MOVEL     'N'           WSLITT            1
529100930414     C*
529200930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
529300060420     C*
529400931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
529500150605     c     wdee          andeq     ' '
529600930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
529700930414     C                   END
529800930511     C*
529900931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
530000150608     c     wdee          andeq     ' '
530100930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
530200930511     C                   END
530300930414     C*
530400930325     C     WSLITT        IFEQ      'N'
530500930423     C                   SUB       12            WR1NOI            5 0
530600930325     C                   END
530700931130     C                   ADD       12            WR1POO            9 0
530800930414     C*
530900931223     C                   ENDDO                                                  <-- 1 ---
531000931126     C*
531100931126     C                   ELSE                                                   <-- 0 -->
531200931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
531300931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
531400931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
531500960129     C     WPOLAF        IFGT      WPODCG
531600960129     C                   Z-ADD     WPOLAF        WTM090            9 0
531700960129     C                   ELSE
531800960129     C                   Z-ADD     WPODCG        WTM090
531900960129     C                   ENDIF
532000960129     C     WPOGIF        IFGT      WTM090
532100960129     C                   Z-ADD     WPOGIF        WTM090
532200960129     C                   ENDIF
532300060420     C     WPOpaif       IFGT      WTM090
532400060420     C                   Z-ADD     WPOpaif       WTM090
532500060420     C                   ENDIF
532600960129     C*
532700960129     C     12            ADD       WTM090        WR1POO
532800960129     C                   ENDIF                                                  <-- 0 ---
532900930512     C*
533000931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
533100931202     C     WR1POO        SUB       12            WR7POI
533200931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
533300931202     C                   ADD       12            WR7POO
533400931202     C     WR7POO        IFGT      WR1POO
533500931202     C                   Z-ADD     WR7POO        WR1POO
533600931202     C                   END
533700930406     C*
533800930325     C                   ENDSR
533900930414     C********************************************************************
534000930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
534100930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
534200930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
534300930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
534400930414     C********************************************************************
534500930414     C     RO2COV        BEGSR
534600091014     c                   clear                   salvaEveL
534700091014     c                   clear                   salvaEveG
534800931130     C*
534900930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
535000930414     C                   Z-ADD     *ZEROS        WR2NOV
535100930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
535200931130     C                   Z-ADD     WR2POI        WR2POW
535300930414     C*
535400930414     C* CICLO FINO A CHE:
535500930414     C*   - DATA INIZIO SUPERATO DATA FINE
535600930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
535700931223     C     WR2POW        ANDLT     WPOLIM
535800930414     C*
535900930414     C                   Z-ADD     WR2POW        WPOROU
536000930414     C                   MOVEL     'N'           WSLITT            1
536100930414     C*
536200930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
536300930414     C*
536400930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
536500930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
536600930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
536700930414     C                   END
536800930414     C*
536900930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
537000930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
537100930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
537200930618     C                   END
537300060420     c
537400060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
537500060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
537600060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
537700060420     C                   END
537800060420     c
537900931201     C*
538000931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
538100931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
538200931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
538300931201     C                   END
538400060418     c
538500060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
538600060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
538700060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
538800060418     C                   END
538900930414     C*
539000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
539100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
539200930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
539300930414     C                   END
539400930511     C*
539500930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
539600930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
539700930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
539800930511     C                   END
539900930414     C*
540000930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
540100930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
540200930414     C     WSLITT        IFEQ      'N'
540300930423     C                   ADD       12            WR2NOV            5 0
540400930414     C                   END
540500931130     C                   ADD       12            WR2POW            9 0
540600930414     C*
540700931223     C                   ENDDO                                                  <-- 1 ---
540800930414     C*
540900930414     C                   ENDSR
541000941228     C********************************************************************
541100941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
541200941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
541300941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
541400941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
541500941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
541600941228     C********************************************************************
541700941228     C     RO2COP        BEGSR
541800941228     C*
541900941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
542000941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
542100941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
542200941228     C                   Z-ADD     *ZEROS        WR2NOV
542300941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
542400941228     C                   Z-ADD     WR2POI        WR2POW
542500941228     C*
542600941228     C* CICLO FINO A CHE:
542700941228     C*   - DATA INIZIO SUPERATO DATA FINE
542800941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
542900941228     C*
543000950215     C                   Z-ADD     WR2POW        WPOROU
543100941228     C                   MOVEL     'N'           WSLITT
543200941228     C* TRASFORMA IL PROGR. ORE IN DATA
543300950215     C*******              Z-ADDWR2POW    WORE
543400950215     C*******              EXSR OREDAT
543500950215     C*******              Z-ADDWDAT      W12DAT  80
543600950215     C*******              Z-ADDWRES      W12RES  30
543700941228     C*
543800941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
543900941228     C*
544000941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
544100941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
544200941228     C     WSLITT        IFEQ      'N'
544300941228     C                   ADD       12            WR2NOV            5 0
544400941228     C                   END
544500941228     C                   ADD       12            WR2POW            9 0
544600941228     C*
544700941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
544800941228     C* ENTRI IN LOOP
544900941228     C     WR2NOV        IFGT      999
545000941228     C                   LEAVE                                                  *VA A ENDDO
545100941228     C                   END
545200941228     C*
545300941228     C                   ENDDO                                                  <-- 1 ---
545400941228     C*
545500941228     C                   ENDSR
545600930510     C********************************************************************
545700930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
545800930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
545900930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
546000930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
546100050905     c*                     il num ore deve essere un multiplo di 12
546200050905     c*                     oppure 1 ora
546300930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
546400930510     C********************************************************************
546500930510     C     RO4CDP        BEGSR
546600930510     C*
546700930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
546800930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
546900050905     c                   movel     '1'           wgiro             1
547000930510     C*
547100930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
547200930510     C                   Z-ADD     WR4POI        WR4POO            9 0
547300930510     C*
547400930510     C* CICLO FINO A CHE:
547500930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
547600930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
547700930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
547800930510     C     WR4POO        ANDLT     WPOLIM
547900930510     C*
548000141106     c                   if        nosomma<>'N'
548100931130     C                   ADD       12            WR4POO            9 0
548200141106     c                   endif
548300930510     C                   Z-ADD     WR4POO        WPOROU            9 0
548400930510     C                   MOVEL     'N'           WSLITT            1
548500930510     C*
548600930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
548700050905     c
548800050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
548900930510     C     WSLITT        IFEQ      'N'
549000050905     c
549100050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
549200050905     c*  se cado in giorno non festivo
549300050905     c                   if        wr4noi=1 and wgiro='1'
549400050905     c                   sub       12            wr4poo
549500050905     c                   add       wr4noi        wr4poo
549600050905     c                   endif
549700050905     c*
549800050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
549900050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
550000050905     c*  calcolate dal pgm
550100050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
550200050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
550300050905     c*  del giorno sucessivo non festivo
550400050905     c                   if        wr4noi=1 and wgiro<>'1'
550500050905     c                   add       wr4noi        wr4poo
550600050905     c                   endif
550700050905     c
550800930510     C                   SUB       12            WR4NOI            5 0
550900930510     C                   END
551000141106     c
551100141106     c                   if        nosomma= 'N'
551200141106     C                   ADD       12            WR4POO            9 0
551300141106     c                   endif
551400930510     C*
551500050905     c                   eval      wgiro='2'
551600931223     C                   ENDDO                                                  <-- 1 ---
551700930510     C*
551800930510     C                   ENDSR
551900931130     C********************************************************************
552000931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
552100931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
552200931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
552300931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
552400931130     C********************************************************************
552500931130     C     RO5COV        BEGSR
552600931130     C*
552700931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
552800060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
552900060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
553000060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
553100931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
553200931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
553300931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
553400931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
553500931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
553600931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
553700931130     C                   Z-ADD     *ZEROS        WR5NOV
553800940714     C*
553900940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
554000940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
554100931207     C*
554200931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
554300940908     C     WIC           IFEQ      'I'
554400090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
554500090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
554600931207     C                   ELSE
554700090827     C*****              Z-ADD     D98TCC        WPOTCT
554800090827     C                   Z-ADD     w99TCC        WPOTCT
554900931207     C                   END
555000931130     C*
555100931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
555200060420     C                   clear                   COV
555300931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
555400931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
555500931207     C                   Z-ADD     2             CO                3 0
555600931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
555700931207     C                   EXSR      RO5SUB
555800981102     C                   DO        99            IC                3 0
555900931207     C     LAF(IC)       IFNE      *ZEROS
556000931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
556100931207     C                   EXSR      RO5SUB
556200931207     C                   ENDIF
556300931207     C                   ENDDO
556400060420     c
556500060420     C                   DO        99            IC                3 0
556600060420     C     paif(IC)      IFNE      *ZEROS
556700060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
556800060420     C                   EXSR      RO5SUB
556900060420     C                   ENDIF
557000060420     C                   ENDDO
557100931207     C* ORDINA LA SCHIERA COV
557200931207     C                   SORTA     COV
557300931130     C*
557400931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
557500931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
557600981102     C                   DO        98            IC                             --- 1 -->
557700931207     C     IC            ADD       1             I1C               3 0
557800931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
557900931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
558000931207     C     COV(I1C)      OREQ      *ZEROS
558100931207     C                   ELSE
558200931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
558300931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
558400931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
558500931130     C                   EXSR      RO2COV
558600931207     C                   ADD       WR2NOV        WR5NOV            9 0
558700931209     C* SE NON SONO IN DATA CONSEGNA
558800931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
558900931207     C     WR5NOV        IFLT      WPOTCT
559000931209     C     COV(I1C)      ANDNE     WPODCM
5591009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5592009709111009 C     WPOTCT        SUB       12            WR5NOV
559300931130     C                   END
559400931207     C                   ENDIF                                                  <-- 2 ---
559500931207     C                   ENDDO                                                  <-- 1 ---
559600940714     C                   ENDIF                                                  <-- 0 ---
559700931130     C*
559800931130     C                   ENDSR
559900931207     C********************************************************************
560000931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
560100931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
560200931207     C********************************************************************
560300931207     C     RO5SUB        BEGSR
560400931207     C*
560500931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
560600931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
560700931207     C     WR5COV        ANDLT     WR5POF
560800931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
560900931207     C                   MOVEL     'N'           WR5PRE            1
561000981102     C*---                 DO   30        I
561100981102     C                   DO        99            I
561200931207     C     WR5COV        IFEQ      COV(I)
561300931207     C                   MOVEL     'S'           WR5PRE
561400931207     C                   LEAVE
561500931207     C                   ENDIF
561600931207     C                   ENDDO
561700931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
561800931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
561900931207     C                   ADD       1             CO                3 0
562000931207     C                   Z-ADD     WR5COV        COV(CO)
562100931207     C                   ENDIF                                                  <-- 2 ---
562200931207     C                   ENDIF                                                  <-- 1 ---
562300931207     C*
562400931207     C                   ENDSR
562500931201     C********************************************************************
562600060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
562700931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
562800931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
562900931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
563000931201     C********************************************************************
563100931201     C     RO6CDT        BEGSR
563200931201     C*
563300931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
563400931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
563500931201     C*
563600931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
563700931201     C                   Z-ADD     WR6POI        WR6POO            9 0
563800931201     C*
563900931201     C* CICLO FINO A CHE:
564000931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
564100931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
564200931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
564300931201     C     WR6POO        ANDLT     WPOLIM
564400931201     C*
564500931201     C                   Z-ADD     WR6POO        WPOROU            9 0
564600931201     C                   MOVEL     'N'           WSLITT            1
564700931201     C*
564800931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
564900931201     C*
565000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
565100150605     c     wdee          andeq     ' '
565200931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
565300931201     C                   END
565400931201     C*
565500931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
565600150608     c     wdee          andeq     ' '
565700931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
565800931201     C                   END
565900931201     C*
566000931201     C     WSLITT        IFEQ      'N'
566100931201     C                   SUB       12            WR6NOI            5 0
566200931201     C                   END
566300931201     C                   ADD       12            WR6POO            9 0
566400931201     C*
566500931223     C                   ENDDO                                                  <-- 1 ---
566600931201     C*
566700931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
566800931202     C     WR6POO        SUB       12            WR7POI
566900931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
567000931207     C                   ADD       12            WR7POO
567100931202     C     WR7POO        IFGT      WR6POO
567200931201     C                   Z-ADD     WR7POO        WR6POO
567300931202     C                   END
567400931201     C*
567500931201     C                   ENDSR
567600931201     C********************************************************************
567700060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
567800060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
567900060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
568000060407     C*            INPUT:  -WR7POI (PROGR.ORE)
568100931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
568200931201     C********************************************************************
568300931201     C     RO7TCF        BEGSR
568400060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
568500060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
568600060407     C                   MOVEL     'N'           WSLITT
568700931201     C*
568800931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
568900931201     C                   Z-ADD     WR7POI        WR7POO            9 0
569000931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
569100931201     C* COINCIDA CON UN TURNO DI CHIUSURA
569200060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
569300931201     C     D98GC2        ANDEQ     *BLANKS
569400060407   x1C                   ELSE                                                   <-- 1 -->
569500931214     C*
569600931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
569700931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
569800931214     C*
569900931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
570000931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
570100931223     C     WPOROU        ANDLT     WPOLIM
570200931214     C* MEMORIZZO INIZIO E FINE RANGE
570300931202     C     WPOTI         IFEQ      *ZEROS
570400931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
570500931202     C                   END
570600931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
570700931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
570800931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
570900931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
571000931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
571100931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
571200931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
571300931201     C                   MOVEL     'N'           WSLITT
571400931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
571500060407     c* Se non trovat, controllo se c'e' evento part che giustifica
571600060407     c                   if        wslitt='N'
571700060407     c                   exsr      datcepg
571800060407     c                   endif
571900060407     c
572000931201     C                   ENDDO                                                  <-- 2 ---
572100931214     C*
572200931201     C                   Z-ADD     WPOROU        WR7POO
572300931214     C*
572400931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
572500931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
572600931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
572700931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
572800931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
572900931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
573000931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
573100931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
573200931209     C                   ELSE                                                   <-- 3 -->
573300931209     C     WIC           IFEQ      'I'
573400931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
573500931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
573600931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
573700931201     C                   ELSE
573800931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
573900931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
574000931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
574100931201     C                   ENDIF
574200931209     C                   ENDIF                                                  <-- 3 ---
574300931209     C                   ENDIF                                                  <-- 2 ---
574400931214     C*
574500060407    1C                   ENDIF                                                  <-- 1 ---
574600060407     c
574700060407     c* Se non trovato turno di chiusura, verifico se c'e'
574800060407     c*  evento particolare che GIUSTIFICA
574900060407     c                   if        wpoti=0
575000060418     C                   EXSR      RO7CEPG
575100060407     c                   endif
575200931201     C*
575300931201     C                   ENDSR
575400060406     C********************************************************************
575500060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
575600060406     c*                        che giustifica
575700060407     C*            INPUT:-WR7POI (PROGR.ORE)
575800060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
575900060406     C********************************************************************
576000060407     C     RO7CEPG       BEGSR
576100060406     C*
576200060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
576300060407     C                   Z-ADD     WR7POI        WR7POO            9 0
576400060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
576500060406     C* coincide
576600060406    1C                   if        wevpag='S'                                   <-- 1 -->
576700060406     C*
576800060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
576900060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
577000060406     C                   MOVEL     'N'           WSLITT
577100060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
577200060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
577300060406     C*
577400060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
577500060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
577600060406     C     WPOROU        ANDLT     WPOLIM
577700060406     C* MEMORIZZO INIZIO E FINE RANGE
577800060406    3C     WPOTI         IFEQ      *ZEROS
577900060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
578000060406    3C                   END
578100060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
578200060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
578300060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
578400060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
578500060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
578600060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
578700060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
578800060406     C                   MOVEL     'N'           WSLITT
578900060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
579000060406     C* Se non trovato,
579100060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
579200060406    3c                   if        wslitt='N'
579300060406     c                   exsr      DATTUC
579400060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
579500060406    2C                   ENDDO                                                  <-- 2 ---
579600060406     C*
579700060407     C                   Z-ADD     WPOROU        WR7POO
579800060406     C*
579900060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
580000060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
580100060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
580200060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
580300060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
580400060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
580500060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
580600060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
580700060406   x3C                   ELSE                                                   <-- 3 -->
580800060406    4C     WIC           IFEQ      'I'
580900060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
581000060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
581100060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
581200060406   x4C                   ELSE
581300060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
581400060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
581500060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
581600060406    4C                   ENDIF
581700060406    3C                   ENDIF                                                  <-- 3 ---
581800060406    2C                   ENDIF                                                  <-- 2 ---
581900060406     C*
582000060406    1C                   ENDIF                                                  <-- 1 ---
582100060406     C*
582200060406     C                   ENDSR
582300931207     C********************************************************************
582400931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
582500931207     C*            INPUT:  -WR8POI (PROGR.ORE)
582600931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
582700931207     C********************************************************************
582800931207     C     RO8FES        BEGSR
582900130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
583000130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
583100931207     C*
583200931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
583300130411     C     WR8POI        SUB       12            WR8POO            9 0
583400931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
583500931207     C                   MOVEL     'N'           WSLITT
583600130411     c
583700130411     c* Non salto i festivi ???
583800130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
583900130411     c
584000931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
584100130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
584200130411     C     WPOROU        ANDGT     WPODSP
584300130411     C                   SUB       12            WPOROU
584400130411     C                   MOVEL     'N'           WSLITT
584500130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
584600130411     C                   ENDDO                                                  <-- 2 ---
584700130411     c
584800130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
584900130411     C     D98GC2        ANDEQ     *BLANKS
585000130411   x1C                   ELSE                                                   <-- 1 -->
585100130411     c* eseguo controllo per i turni di chiusura
585200130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
585300130411     C*
585400130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
585500130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
585600130411     C     WPOROU        ANDgT     WPOdsp
585700130411     C* MEMORIZZO INIZIO E FINE RANGE
585800130411     C     WPOTf         IFEQ      *ZEROS
585900130411     C                   z-add     wporou        WPOTf
586000130411     C                   END
586100140826     C     WPOROU        SUB       wnoadd        WPOROU
586200130411     c
586300130411     C                   MOVEL     'N'           WSLITT
586400130411     c                   exsr      dattuc
586500130411     c                   if        wslitt='N'
586600130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
586700140826     c* Se slitta per festivo --> tolgo sempre 12 ore
586800140826     c                   z-add     12            WNOADD
586900130411     c                   endif
587000130411     C* SLITTA
587100140826     C     wporou        add       12            WPOTi
587200130411    2C                   ENDDO                                                  <-- 2 ---
587300130411     c
587400130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
587500130411     c
587600130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
587700130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
587800130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
587900130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
588000130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
588100130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
588200130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
588300130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
588400130411   x3C                   ELSE                                                   <-- 3 -->
588500130411    4C     WIC           IFEQ      'I'
588600130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
588700130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
588800130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
588900130411   x4C                   ELSE
589000130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
589100130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
589200130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
589300130411    4C                   ENDIF
589400130411    3C                   ENDIF                                                  <-- 3 ---
589500130411    2C                   ENDIF                                                  <-- 3 ---
589600130411     C*
589700130411    1C                   ENDIF                                                  <-- 1 ---
589800130411
589900130411     C**   WPOROU        ADD       12            WR8POO
590000130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
590100130411     c                   z-add     wr8poi        wr8poo
590200931207     C*
590300931207     C                   ENDSR
590400931210     C********************************************************************
590500931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
590600940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
590700931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
590800931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
590900931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
591000931210     C********************************************************************
591100931210     C     RO9COV        BEGSR
591200931210     C*
591300931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
591400931210     C                   Z-ADD     *ZEROS        WR9NOV
591500931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
591600931210     C                   Z-ADD     WR9POI        WR9POW
591700931210     C* CICLO FINO A CHE:
591800931210     C*   - DATA INIZIO SUPERATO DATA FINE
591900931223     C*   - RAGGIUNTO LIMITE MASSIMO
592000931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
592100931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
592200931223     C     WR9POW        ANDLT     WPOLIM
592300931210     C*
592400931210     C                   Z-ADD     WR9POW        WPOROU
592500931210     C                   MOVEL     'N'           WSLITT            1
592600940324     C* FESTIVI
592700931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
592800940324     C* GIACENZA
592900931214     C     WSLITT        IFEQ      'N'
593000941229£$   C     WPOAGC        IFNE      *ZEROS
593100940324£$   C     WPOCGC        ORNE      *ZEROS
593200941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
593300940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
593400940324£$   C                   MOVEL     'S'           WSLITT
593500940324£$   C                   END
593600940324£$   C                   END
593700940324£$   C                   END
593800960129     C* EVENTI GIACENZA
593900960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
594000960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
594100960129     C                   Z-ADD     *ZEROS        G                 3 0
594200960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
594300960129     C     G             ANDLT     IG
594400960129     C                   ADD       1             G
594500960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
594600960209     C     WPOROU        ANDLE     GCF(G)
594700960129     C                   MOVEL     'S'           WSLITT
594800960129     C                   ENDIF                                                  <-- 3 ---
594900960129     C                   ENDDO                                                  <-- 2 ---
595000960129     C                   ENDIF                                                  <-- 1 ---
595100960129     C                   ENDIF                                                  <-- 0 ---
595200940324     C* LASCIATO AVVISO
595300931214     C     WSLITT        IFEQ      'N'
595400941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
595500940324£$   C                   Z-ADD     *ZEROS        L                 3 0
595600940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
595700940324£$   C     L             ANDLT     IL
595800940324£$   C                   ADD       1             L
595900941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
596000941229£$   C     WPOROU        ANDLE     LCF(L)
596100050520     c     LCon(L)       andeq     'S'
596200940324£$   C                   MOVEL     'S'           WSLITT
596300940324£$   C                   END                                                    <-- 3 ---
596400940324£$   C                   ENDDO                                                  <-- 2 ---
596500940324£$   C                   END                                                    <-- 1 ---
596600931210     C                   END
596700940324     C* TURNO CHIUSURA
596800140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
596900140826     c*  di sistemare le ore da sommare 12 o 24...
597000931214     C     WSLITT        IFEQ      'N'
597100931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
597200931210     C                   END
597300940324     C* DATA CONSEGNA RICHIESTA
597400050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
597500050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
597600050907     c
597700931214     C     WSLITT        IFEQ      'N'
597800050907     c                   add       1             wporou
597900940324£$   C     WPOCRI        IFNE      *ZEROS
598000940324£$   C     WPOROU        IFLT      WPOCRC
598100940324£$   C                   MOVEL     'S'           WSLITT
598200940324£$   C                   END
598300940324£4   C                   END
598400050907     c                   sub       1             wporou
598500931210     C                   END
598600931210     C*
598700931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
598800931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
598900931210     C     WSLITT        IFEQ      'N'
599000931210     C                   ADD       12            WR9NOV            5 0
599100931210     C                   END
599200931210     C                   ADD       12            WR9POW            9 0
599300931210     C*
599400931223     C                   ENDDO                                                  <-- 1 ---
599500931210     C*
599600931210     C                   ENDSR
599700940628%%%  C********************************************************************
599800940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
599900940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
600000940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
600100940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
600200940628%%%  C********************************************************************
600300940628%%%  C     RO0GES        BEGSR
600400940628%%%  C*
600500940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
600600940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
600700940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
600800940628%%%  C*
600900940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
601000940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
601100940628%%%  C*
601200940628%%%  C* CICLO FINO A CHE:
601300940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
601400940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
601500940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
601600050922%%%  C     WR0POO        ANDGe     WPODSP
601700940628%%%  C*
601800940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
601900940628%%%  C                   MOVEL     'N'           WSLITT            1
602000940628%%%  C*
602100940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
602200940628%%%  C*
602300940628%%%  C     WSLITT        IFEQ      'N'
602400940628%%%  C                   SUB       12            WR0NOI            5 0
602500940628%%%  C                   END
602600940628%%%  C                   SUB       12            WR0POO            9 0
602700940628%%%  C*
602800940628%%%  C                   ENDDO                                                  <-- 1 ---
602900940628%%%  C*
603000940628%%%  C                   ADD       12            WR0POO            9 0
603100940628%%%  C*
603200940628%%%  C                   ENDSR
603300960911     C********************************************************************
603400960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
603500960911     C*            INPUT:  -W10POI (PROGR.ORE)
603600960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
603700960911     C********************************************************************
603800960911     C     R10FES        BEGSR
603900960911     C*
604000960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
604100960911     C                   Z-ADD     W10POI        W10POO            9 0
604200960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
604300960911     C                   MOVEL     'N'           WSLITT
604400960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
604500960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
604600960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
604700960911     C                   ADD       12            WPOROU
604800960911     C                   MOVEL     'N'           WSLITT
604900960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
605000960911     C                   ENDDO                                                  <-- 2 ---
605100960911     C                   Z-ADD     WPOROU        W10POO
605200960911     C*
605300960911     C                   ENDSR
605400930325     C********************************************************************
605500930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
605600930325     C********************************************************************
605700930325     C     CHKGIA        BEGSR
605800930406     C*
605900960129     C* APERTURA/CHIUSURA GIACENZA
606000930326     C     WPODAG        IFNE      *ZEROS
606100930406     C     WPODCG        ORNE      *ZEROS
606200930407     C     WPOROU        IFGE      WPODAG
606300930407     C     WPOROU        ANDLE     WPODCG
606400930325     C                   MOVEL     'S'           WSLITT
606500930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
606600930325     C                   END
606700930326     C                   END
606800960129     C*
606900960129     C* EVENTI GIACENZA
607000960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
607100960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
607200960129     C                   Z-ADD     *ZEROS        G                 3 0
607300960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
607400960129     C     G             ANDLT     IG
607500960129     C                   ADD       1             G
607600960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
607700960129     C     WPOROU        ANDLE     GIF(G)
607800091009     c
607900091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
608000091009     c*  l'orario dell'evento
608100091009     c                   select
608200091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
608300091009     c* Se orario entro le 10.30  va bene
608400091014     c*  solo la prima volta per evento cioè quando controllo
608500091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
608600091014     c*  e non devo dare errore
608700091014     C                   MOVEL     'OSC  '       WR3TAB
608800091014     C                   eval      WR3HHI=evo(gip(G))
608900091014     C                   EXSR      RO3ORE
609000091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
609100091014     c*  in ritardo come per "E")
609200091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
609300091014     c                              wr3matpom='P'
609400091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
609500091009     C                   MOVEL     'S'           WSLITT
609600091009     c                   endif
609700091014     c                   eval      SalvaEveG=gip(G)
609800091009     c
609900091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
610000091009     c* Se orario entro le 10.30  va bene
610100091014     c*  solo la prima volta per evento cioè quando controllo
610200091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
610300091014     c*  e non devo dare errore
610400091014     C                   MOVEL     'OSC  '       WR3TAB
610500091014     C                   eval      WR3HHI=evo(gip(G))
610600091014     C                   EXSR      RO3ORE
610700091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
610800091014     c*  in ritardo come per "E")
610900091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
611000091014     c                              wr3matpom='P'
611100091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
611200091009     C                   MOVEL     'S'           WSLITT
611300091009     c                   endif
611400091014     c                   eval      SalvaEveG=gip(G)
611500091009     c
611600091009     c                   other
611700960129     C                   MOVEL     'S'           WSLITT
611800960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
611900091009     c                   endsl
612000091009
612100960129     C                   ENDIF                                                  <-- 3 ---
612200960129     C                   ENDDO                                                  <-- 2 ---
612300960129     C                   ENDIF                                                  <-- 1 ---
612400960129     C                   ENDIF                                                  <-- 0 ---
612500930406     C*
612600930325     C                   ENDSR
612700060420     C********************************************************************
612800930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
612900930325     C********************************************************************
613000930325     C     CHKLAV        BEGSR
613100930325     C*
613200941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
613300930421     C                   Z-ADD     *ZEROS        L                 3 0
613400930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
613500930421     C     L             ANDLT     IL
613600930421     C                   ADD       1             L
613700930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
613800930421     C     WPOROU        ANDLE     LAF(L)
613900090928     c
614000090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
614100091009     c*  l'orario dell'evento
614200091009     c                   select
614300091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
614400091009     c* Se orario entro le 10.30  va bene
614500091014     c*  solo la prima volta per evento cioè quando controllo
614600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
614700091014     c*  e non devo dare errore
614800091014     C                   MOVEL     'OSC  '       WR3TAB
614900091014     C                   eval      WR3HHI=evo(lap(L))
615000091014     C                   EXSR      RO3ORE
615100091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
615200091014     c*  in ritardo come per "E")
615300091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
615400091014     c                              wr3matpom='P'
615500091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
615600091009     C                   MOVEL     'S'           WSLITT
615700091009     c                   endif
615800091014     c                   eval      SalvaEveL=lap(L)
615900091009     c
616000091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
616100091009     c* Se orario entro le 10.30  va bene
616200091014     c*  solo la prima volta per evento cioè quando controllo
616300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
616400091014     c*  e non devo dare errore
616500091014     C                   MOVEL     'OSC  '       WR3TAB
616600091014     C                   eval      WR3HHI=evo(lap(L))
616700091014     C                   EXSR      RO3ORE
616800091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
616900091014     c*  in ritardo come per "E")
617000091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
617100091014     c                              wr3matpom='P'
617200091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
617300091009     C                   MOVEL     'S'           WSLITT
617400091009     c                   endif
617500091014     c                   eval      SalvaEveL=lap(L)
617600090928     c
617700091009     c                   other
617800930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
617900091009     C                   MOVEL     'S'           WSLITT
618000091009     c                   endsl
618100090928     c
618200930421     C                   END                                                    <-- 3 ---
618300931223     C                   ENDDO                                                  <-- 2 ---
618400930421     C                   END                                                    <-- 1 ---
618500930325     C*
618600930325     C                   ENDSR
618700060420     C********************************************************************
618800060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
618900060420     c*                   che Allungano
619000060420     C********************************************************************
619100060420     C     CHKCEPA       BEGSR
619200060420     C*
619300060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
619400060420     C                   Z-ADD     *ZEROS        p                 3 0
619500060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
619600060420     C     p             ANDLT     IpA
619700060420     C                   ADD       1             P
619800060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
619900060420     C     WPOROU        ANDLE     paif(P)
620000060420     C                   MOVEL     'S'           WSLITT
620100060420     C                   ADD       12            W98EA
620200060420     C                   END                                                    <-- 3 ---
620300060420     C                   ENDDO                                                  <-- 2 ---
620400060420     C                   END                                                    <-- 1 ---
620500060420     C*
620600060420     C                   ENDSR
620700930414     C********************************************************************
620800930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
620900930414     C********************************************************************
621000930414     C     CHKCNT        BEGSR
621100930511     C*
621200930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
621300930611     C     WIC           IFEQ      'I'                                          *INTERNO
621400930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
621500930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
621600930611     C                   ELSE
621700930611     C     WPOROU        IFGE      WPOAII
621800930611     C     WPOROU        ANDLE     WPOAFI
621900930611     C                   MOVEL     'S'           WSLITT
622000930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
622100930611     C                   END
622200930611     C                   END
622300930611     C                   ELSE                                                   *CLIENTE
622400930611     C     WPOAIC        IFEQ      *ZEROS
622500930611     C     WPOAFC        ANDEQ     *ZEROS
622600930611     C                   ELSE
622700930611     C     WPOROU        IFGE      WPOAIC
622800930611     C     WPOROU        ANDLE     WPOAFC
622900930611     C                   MOVEL     'S'           WSLITT
623000930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
623100930611     C                   END
623200930611     C                   END
623300930611     C                   END
623400930414     C*
623500930414     C                   ENDSR
623600930511     C********************************************************************
623700930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
623800930511     C********************************************************************
623900930511     C     CHKFED        BEGSR
624000930511     C*
624100930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
624200930611     C     WIC           IFEQ      'I'                                          *INTERNO
624300930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
624400930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
624500930611     C                   ELSE
624600930611     C     WPOROU        IFGE      WPOFII
624700930611     C     WPOROU        ANDLE     WPOFFI
624800930611     C                   MOVEL     'S'           WSLITT
624900930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
625000930611     C                   END
625100930611     C                   END
625200930611     C                   ELSE                                                   *CLIENTE
625300930611     C     WPOFIC        IFEQ      *ZEROS
625400930611     C     WPOFFC        ANDEQ     *ZEROS
625500930611     C                   ELSE
625600930611     C     WPOROU        IFGE      WPOFIC
625700930611     C     WPOROU        ANDLE     WPOFFC
625800930611     C                   MOVEL     'S'           WSLITT
625900930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
626000930611     C                   END
626100930611     C                   END
626200930611     C                   END
626300930511     C*
626400930511     C                   ENDSR
626500930325     C********************************************************************
626600930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
626700930325     C********************************************************************
626800930325     C     CHKFES        BEGSR
626900060526     c* se già passata la data, non controllo wporou
627000060526     c                   if        wpodata>0
627100060526     c                   z-add     wpodata       w12dat
627200060526     c                   clear                   w12res
627300060526     c                   else
627400931130     C*
627500931130     C* TRASFORMA IL  PROGR.ORE IN DATA
627600931130     C                   Z-ADD     WPOROU        WORE
627700931130     C                   EXSR      OREDAT
627800941227     C                   Z-ADD     WDAT          W12DAT            8 0
627900931130     C                   Z-ADD     WRES          W12RES            3 0
628000060526     c                   endif
628100930325     C*
628200941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
628300930325     C* E' FESTIVO
628400170117     c* l'affidabilità partenza ora fa un doppio tentativo
628500170117    1C     WTC           IFEQ      'P'                                          *PARTENZA
628600170117     C                   Z-ADD     D98LNP        CLNTFP
628700170117     C                   Z-ADD     *ZEROS        CLNTFA
628800170117     c                   exsr      azcln_fes
628900170117     c* se non è festivo guardo anche la trazione
629000170117     c                   if        wslitt<>'S'
629100170117     C                   Z-ADD     D98TFP        CLNTFP
629200170117     C                   Z-ADD     D98TFA        CLNTFA
629300170117     c                   exsr      azcln_fes
629400170117     C                   ENDIF
629500170117     c
629600170117   x1C                   Else
629700170117     c
629800090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
629900090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
630000090108     c**   è possibile rendere festivo in terminal per un altro terminal
630100090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
630200170117     c
630300170117     c** ES 17/01/17 - Faccio doppio tentativo: prima con la filiale e poi con
630400170117     c**               la trazione , se filiale non festiva
630500930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
630600170117     C***  WTC           oreq      'P'                                          *TRAZIONE
630700941227     C                   Z-ADD     D98TFP        CLNTFP
630800941227     C                   Z-ADD     D98TFA        CLNTFA
630900941228     C                   ENDIF
631000941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
631100941227     C                   Z-ADD     *ZEROS        CLNTFP
631200941227     C                   Z-ADD     D98LNA        CLNTFA
631300941228     C                   ENDIF
631400170117     c
631500170117     c                   exsr      azcln_fes
631600170117    1c                   endif
631700930325     C*
631800060526     c                   clear                   wpodata
631900930325     C                   ENDSR
632000170117
632100170117     C********************************************************************
632200170117     c* Verifica se giorno festivo su calendario
632300170117     C********************************************************************
632400170117     c     Azcln_fes     BEGSR
632500170117     C                   Z-ADD     W12DAT        DSDAT8
632600170117     C                   Z-ADD     DSAAS         CLNANN
632700170117     C                   Z-ADD     DSMES         CLNMES
632800170117     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
632900170117     C     D98ERR        IFEQ      '0'                                          --- 1 -->
633000170117     C                   Z-ADD     DSGIO         G
633100170117     C     W12RES        IFEQ      *ZEROS
633200170117     C                   MOVEL     CLNMAT        GFEDS
633300170117     C                   ELSE
633400170117     C                   MOVEL     CLNPOM        GFEDS
633500170117     C                   END
633600170117     c
633700170117     c* se sto verificando i tempi partenza o trazione e
633800170117     c*  la data che sto controllando corrisponde alla data spedizione,
633900170117     c*  la considero sempre lavorativa
634000170117     c                   if        (wtc='P' or wtc='T') and w12dat=d98dfv
634100170117     c                   else
634200170117     c
634300170117     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
634400170117     C                   MOVEL     *BLANKS       WC1Y
634500170117     C                   MOVEL     GFE(G)        WC1Y              8
634600170117     C                   Z-ADD     1             I
634700170117     C     WC1Y          LOOKUP    C1Y(I)                                 99
634800170117     C     *IN99         IFEQ      '1'                                          --- 3 -->
634900170117     C     WIC           IFEQ      'I'
635000170117     C     I1Y(I)        ANDEQ     'S'
635100170117     C                   MOVEL     'S'           WSLITT
635200170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
635300170117     C                   END
635400170117     C     WIC           IFEQ      'C'
635500170117     C     E1Y(I)        ANDEQ     'S'
635600170117     C                   MOVEL     'S'           WSLITT
635700170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
635800170117     C                   END
635900170117     C     WIC           IFEQ      'X'
636000170117     C     E1Y(I)        ANDEQ     'S'
636100170117     C                   MOVEL     'S'           WSLITT
636200170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
636300170117     C                   END
636400170117     C                   ELSE                                                   <-- 3 -->
636500170117     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
636600170117     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
636700170117     C                   END                                                    <-- 3 ---
636800170117     C                   END                                                    <-- 2 ---
636900170117     C                   END                                                    <-- 1 ---
637000170117     c
637100170117     c                   endif
637200170117     c                   ENDSR
637300931201     C********************************************************************
637400931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
637500931201     C********************************************************************
637600931201     C     CHKTUC        BEGSR
637700931201     C*
637800931201     C     WIC           IFEQ      'I'                                          --- 1 -->
637900931201     C                   DO        TI            T                 3 0          --- 2 -->
638000931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
638100931201     C     WPOROU        ANDLE     TIF(T)
638200931201     C                   MOVEL     'S'           WSLITT
638300931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
638400931201     C                   LEAVE                                                  *VA A FINE DO
638500931201     C                   ENDIF                                                  <-- 3 ---
638600931201     C                   ENDDO                                                  <-- 2 ---
638700931201     C                   ELSE                                                   <-- 1 -->
638800931201     C                   DO        TC            T                 3 0          --- 2 -->
638900931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
639000931201     C     WPOROU        ANDLE     TCF(T)
639100931201     C                   MOVEL     'S'           WSLITT
639200931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
639300931201     C                   LEAVE                                                  *VA A FINE DO
639400931201     C                   ENDIF                                                  <-- 3 ---
639500931201     C                   ENDDO                                                  <-- 2 ---
639600931201     C                   ENDIF                                                  <-- 1 ---
639700931201     C*
639800931201     C                   ENDSR
639900060418     C********************************************************************
640000060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
640100060418     c*          - eventi particolari che giustificano
640200060418     C********************************************************************
640300060418     C     CHKCEPG       BEGSR
640400060418     C*
640500060418     C     WIC           IFEQ      'I'                                          --- 1 -->
640600060420     C                   DO        PGI           p                 3 0          --- 2 -->
640700060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
640800060420     C     WPOROU        ANDLE     PGIF(p)
640900060418     C                   MOVEL     'S'           WSLITT
641000060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
641100060418     C                   LEAVE                                                  *VA A FINE DO
641200060418     C                   ENDIF                                                  <-- 3 ---
641300060418     C                   ENDDO                                                  <-- 2 ---
641400060418     C                   ELSE                                                   <-- 1 -->
641500060420     C                   DO        PGC           p                 3 0          --- 2 -->
641600060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
641700060420     C     WPOROU        ANDLE     PGCF(p)
641800060418     C                   MOVEL     'S'           WSLITT
641900060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
642000060418     C                   LEAVE                                                  *VA A FINE DO
642100060418     C                   ENDIF                                                  <-- 3 ---
642200060418     C                   ENDDO                                                  <-- 2 ---
642300060418     C                   ENDIF                                                  <-- 1 ---
642400060418     C*
642500060418     C                   ENDSR
642600930512     C********************************************************************
642700931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
642800930512     C********************************************************************
642900931130     C     DATTUC        BEGSR
643000931214     C*
643100931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
643200931214     C                   Z-ADD     12            WNOADD            9 0
643300930325     C*
643400931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
643500931125     C     D98GC2        ORNE      *BLANKS
643600931130     C*
643700931213     C                   Z-ADD     WPOROU        WORE
643800931130     C                   EXSR      OREDAT                                       *ORE->DATA
643900931130     C                   Z-ADD     WDAT          W12DAT
644000931130     C                   Z-ADD     WRES          W12RES
644100930405     C*
644200941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
644300930405     C* DELLA SETTIMANA
644400941227     C                   Z-ADD     W12DAT        DSDAT8
644500941227     C                   Z-ADD     DSAAS         CLNANN
644600941227     C                   Z-ADD     DSMES         CLNMES
644700941227     C                   Z-ADD     *ZEROS        CLNTFP
644800941227     C                   Z-ADD     *ZEROS        CLNTFA
644900930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
645000930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
645100941227     C                   MOVEL     CLNMAT        GSEDS
645200960129     C                   Z-ADD     DSGIO         GI                6 0
645300960129     C                   MOVEL     GSE(GI)       WGSE              1
645400140911     c
645500140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
645600140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
645700140911     c                   if        wtuctut='S'
645800140911     c                   eval      wtuctut='P'
645900140911     c                   endif
646000930326     C*
646100931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
646200931213     C                   DO        4             IT                3 0          --- 3 -->
646300931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
646400931213     C                   MOVEL     TUC(IT)       W93GSE            1
646500931213     C                   MOVE      TUC(IT)       W93GMP            1
646600930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
646700930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
646800931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
646900930512     C     W93GSE        OREQ      ' '
647000140911     c     WTUCTUT       andeq     ' '
647100140911     c
647200930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
647300931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
647400930401     C     W93GMP        OREQ      ' '
647500930325     C                   MOVEL     'S'           WSLITT
647600931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
647700931213     C                   ELSE                                                   <-- 6 -->
647800001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
647900001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
648000001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
648100050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
648200050908     C     Usacons       IFNE      'S'                                          --- 7 -->
648300141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
648400141107     c*    orari giorno per giorno
648500141107     c                   if        OOR2FCSGL<>' '
648600141108     c                             and w12dat<>sav_12dat
648700141107     c                   eval      or_datarif=w12dat
648800141107     c                   eval      or_DDC    =w12dat
648900141107     c                   exsr      Reper_tempi
649000141108     c                   eval      sav_12dat=w12dat
649100141107     c                   endif
649200141107     c
649300930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
649400930325     C     W93GMP        IFEQ      'M'
649500930326     C     W12RES        ANDEQ     *ZEROS
649600141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
649700141111     c                             or OOR2STfS =0
649800141105     c                             or (tuc(IT)<>gc1part and
649900170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
650000170207     c
650100140825     C                   MOVEL     'S'           WSLITT
650200140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
650300140911     c* slitto solo una volta  se chiuso tutti " M"
650400140911     c                   if        W93GSE=' '
650500140911     c                   eval      wTUCTUT='S'
650600140911     c                   endif
650700140911     c
650800140825     c                   endif
650900930325     C                   END
651000930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
651100930325     C     W93GMP        IFEQ      'P'
651200930326     C     W12RES        ANDNE     *ZEROS
651300140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
651400140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
651500141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
651600141111     c                             or OOR2STiS =0
651700141105     c                             or (tuc(IT)<>gc1part and
651800170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
651900930325     C                   MOVEL     'S'           WSLITT
652000140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
652100140911     c* slitto solo una volta  se chiuso tutti " P"
652200140911     c                   if        W93GSE=' '
652300140911     c                   eval      wTUCTUT='S'
652400140911     c                   endif
652500140825     c                   endif
652600140825     c
652700931213     C                   ENDIF
652800001128     C                   ENDIF                                                  <-- 7 ---
652900931213     C                   ENDIF                                                  <-- 6 ---
653000931213     C                   ENDIF                                                  <-- 5 ---
653100931213     C                   ENDIF                                                  <-- 4 ---
653200931213     C                   ENDDO                                                  <-- 3 ---
653300930405     C*
653400931213     C                   ENDIF                                                  <-- 2 ---
653500931213     C                   ENDIF                                                  <-- 1 ---
653600140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
653700140911     c                   if        wtuctut='P'  and wslitt='N'
653800140911     c                   clear                   wtuctut
653900140911     c                   endif
654000140911     c
654100930325     C                   ENDSR
654200060406     C********************************************************************
654300060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
654400060406     C********************************************************************
654500060406     C     DATCEPG       BEGSR
654600060406     C*
654700060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
654800060406     C                   Z-ADD     12            WNOADD            9 0
654900060406     C*
655000060406    1C                   if        wevpag='S'
655100060406     C*
655200060406     C                   Z-ADD     WPOROU        WORE
655300060406     C                   EXSR      OREDAT                                       *ORE->DATA
655400060406     C                   Z-ADD     WDAT          W12DAT
655500060406     C                   Z-ADD     WRES          W12RES
655600060418     c                   z-add     1             P
655700060406     c* Verifico se nella data esiste un evento Part che giustifica
655800060418     c     w12dat        lookup    pvd(P)                                 30
655900060418    2c                   dow       *in30 and pvf(P)<>'G'
656000060418     c                   add       1             P
656100060420     c     w12dat        lookup    pvd(P)                                 30
656200060406    2c                   enddo
656300060406     c
656400060406     c* 30 on --> TROVATO !!
656500060406    2c                   if        *in30
656600060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
656700060406     C                   MOVEL     'S'           WSLITT
656800060406     c                   z-add     24            wnoadd
656900060406    3C                   ENDIF
657000060406     C*
657100060406    2C                   ENDIF                                                  <-- 2 ---
657200060406    1C                   ENDIF                                                  <-- 1 ---
657300060406     C*
657400060406     C                   ENDSR
657500931210     C********************************************************************
657600931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
657700931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
657800931210     C********************************************************************
657900050805     C***  CHKDCR        BEGSR
658000931210     C*
658100050805     C***  WPOCRI        IFNE      *ZEROS
658200050805     C***  WPOROU        IFGE      WPOCRI
658300050805     C***  WPOROU        ANDLT     WPOCRF
658400050805     C***                ELSE
658500050805     C***                MOVEL     'S'           WSLITT
658600050805     C***                END
658700050805     C***                END
658800931210     C*
658900050805     C***                ENDSR
659000140822     C********************************************************************
659100140822     C*  Reperimento tempi di consegna per decidere se slittare
659200140822     C********************************************************************
659300140822     C     Reper_tempi   BEGSR
659400140822
659500140822     c                   clear                   trulorSds
659600140825     c                   clear                   trulor2ds
659700140822     c                   eval       IOREtfp = d98tfp
659800140822     c                   eval       IOREtfa = d98tfa
659900140825     c                   eval       IOREdti = d98dti
660000140825     c                   eval       IOREhti = d98hti
660100140825     c                   eval       IOREdcr = s98dcr
660200140825     c                   eval       IOREhcr = s98hcr
660300140825     c                   eval       IOREtcr = s98tcr
660400140825     c                   eval       IOREDEI = d98dei
660500140825     c                   eval       IOREOEI = d98oei
660600140825     c                   eval       IOREfil = d98lna
660700140825     c                   eval       IOREcap = d98cad
660800140825     c                   eval       IOREloc = d98lod
660900140825     c                   eval       IOREnar = d98nzd
661000140825     c
661100140825     c* Data di Riferimento:
661200140825     c* Se impostata la consegnabilità --> prendo questa
661300140822     c                   select
661400140825     c                   when      d98dfc>0
661500141107     c                   if        or_datarif<d98dfc and or_datarif>0
661600141107     c                   eval      ioredta=or_datarif
661700141107     c                   else
661800141107     c                   eval      ioredta=d98dfc
661900141107     c                   endif
662000140825     c* altrimenti data di consegna
662100140825     c                   when      d98dcm>0
662200141107     c                   if        or_datarif<d98dcm and or_datarif>0
662300141107     c                   eval      ioredta=or_datarif
662400141107     c                   else
662500140825     c                   eval      ioredta=d98dcm
662600141107     c                   endif
662700140825     c
662800140822     c                   other
662900140825     c* altrimenti data teorica consegna se > = oggi
663000140825     C                   time                    wrkoggiiso
663100140825     C     *iso          move      wrkoggiiso    ioredta
663200140822     c* se < di oggi, imposto oggi
663300140825     c                   if        d98dei>ioredta
663400140825     c                   eval      ioredta=d98dei
663500140822     c                   endif
663600141107     c
663700141107     c                   if        or_datarif<ioredta and or_datarif>0
663800141107     c                   eval      ioredta=or_datarif
663900141107     c                   endif
664000141107     c
664100140822     c                   endsl
664200140822     c
664300140825     c                   eval       IOREtser = 'C'
664400141107     c                   if         or_ddc>0 and or_ddc<d98ddc
664500141107     c                   eval       IOREDDC=or_ddc
664600141107     c                   eval       IOREnDC=8888888
664700141107     c                   eval       IOREcons = 'S'
664800141107     c                   else
664900140825     c                   eval       IOREDDC=d98ddc
665000140825     c                   eval       IOREnDC=wd98ndc
665100140825     c                   eval       IOREcons = d98cons
665200141107     c                   endif
665300141017     c                   eval       IOREtsp = d98tsp
665400140822     c
665500140822     c                   call      'TRULORSR'
665600140822     c                   parm                    kpjba
665700140822     c                   parm                    trulorSds
665800140825     c                   parm                    trulor2ds
665900140825     c
666000140825     c                   eval      wtrulor= 'S'
666100141107     c                   clear                   or_datarif        8 0
666200141107     c                   clear                   or_ddc            8 0
666300140822     C                   ENDSR
666400950406     C********************************************************************
666500950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
666600950406     C********************************************************************
666700950406     C     DATORE        BEGSR
666800950406     C*
666900950406     C                   Z-ADD     *ZEROS        WORE
667000950406     C     WDAT          IFNE      0
667100950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
667200950406     C     WAA           DIV       4             WQUA              4 0
667300950406     C                   MVR                     WRES
667400950406     C     WAA           MULT      365           WGIO              9 0
667500950406     C                   ADD       WQUA          WGIO
667600950406     C                   ADD       WGG           WGIO
667700950406     C     WMM           SUB       1             MC                3 0
667800950406     C                   DO        MC            WX                3 0
667900950406     C                   ADD       MES(WX)       WGIO
668000950406     C                   END
668100950406     C     WRES          IFEQ      0
668200950406     C     WMM           ANDLE     2
668300950406     C                   SUB       1             WGIO
668400950406     C                   END
668500950406     C     WGIO          MULT      24            WORE              9 0
668600950406     C                   ENDIF
668700950406     C*
668800950406     C                   ENDSR
668900930323     C********************************************************************
669000930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
669100930323     C********************************************************************
669200930323     C     OREDAT        BEGSR
669300930323     C*
669400950407     C                   SETOFF                                       717273
669500950407     C                   SETOFF                                       7475
669600930323     C     WORE          DIV       24            WGIO
669700930323     C                   MVR                     WRES              3 0
669800950406     C*
669900950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
670000950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
670100950406     C*  TROVO ANNO
670200950406     C     WGIO          DIV       365           WAA
670300950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
670400950406     C                   MVR                     WREST                71
670500950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
670600950406     C     WGIO          DIV       365           WAA
670700950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
670800950406     C*                                                    0 = 31/12/PR
670900950406     C   75
671000950406     CAN 71              DO                                                     BIS. E NON ERA
671100950406     C     WAA           SUB       1             WAA
671200950406     C                   Z-ADD     365           WREST
671300950406     C                   END
671400950406     C*
671500950406     C     WAA           DIV       4             WNBIS                                      T.
671600950406     C                   MVR                     WRBIS             3 073
671700950406     C   73
671800950406     CANN71                                                                     NON PIU' BISEST
671900950406     CORN73              ADD       1             WREST                          E'BISESTILE
672000950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
672100950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
672200950406     C  N73              SUB       1             $PM(1)
672300950406     C                   Z-ADD     1             WI                2 0
672400950406     C* TROVO IL MESE E GIORNI
672500950406     C     WREST         LOOKUP    $PM(WI)                            74  73
672600950406     C                   MOVE      WI            WMM
672700950406     C     WI            COMP      1                                      72
672800950406     C  N72
672900950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
673000950406     C   74              SUB       1             WI
673100950406     C  N72
673200950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
673300950406     C   72              MOVE      WREST         WGG
673400950406     C                   ADD       1900          WAA
673500950406     C*
673600930323     C*
673700930323     C                   ENDSR
6738009407151507 C********************************************************************
6739009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6740009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6741009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6742009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6743009407151507 C********************************************************************
6744009407151507 C     FINGIO        BEGSR
6745009407151507 C*
6746009407151507 C     WPORE         DIV       24            WGIO
6747009407151507 C                   MVR                     WRES
6748009407151507 C     WRES          IFEQ      *ZEROS
6749009407151507 C                   Z-ADD     12            WNORE             3 0
6750009407151507 C                   ELSE
6751009407151507 C                   Z-ADD     *ZEROS        WNORE
6752009407151507 C                   ENDIF
6753009407151507 C*
6754009407151507 C                   ENDSR
675500930517     C********************************************************************
675600971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
675700930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
675800930517     C********************************************************************
675900930517     C     CERCAL        BEGSR
676000930517     C*
676100930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
676200941227     C                   MOVE      CLNTFP        DCATFP
676300941227     C                   MOVE      CLNTFA        DCATFA
676400941227     C                   MOVE      CLNANN        DCAANN
676500941227     C                   MOVE      CLNMES        DCAMES
676600930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
676700930517     C                   Z-ADD     1             I
676800930517     C     DCALEN        LOOKUP    CAK(I)                                 99
676900930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
677000930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
677100941227     C     KEYCLN        CHAIN     AZCLN01L                           99
677200930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
677300930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
677400930517     C                   MOVEL     '1'           D98ERR
677500930517     C                   MOVEL     MSG(3)        D98MSG
677600930517     C                   ELSE                                                   <-- 2 -->
677700970401     C* CONTROLLA SE ANNULLATO
677800970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
677900970401     C                   MOVEL     '1'           D98ERR
678000970401     C                   MOVEL     MSG(3)        D98MSG
678100970401     C                   ELSE                                                   <-- 3 -->
678200930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
678300930517     C                   ADD       1             WINDCA            6 0
678400970401     C     WINDCA        IFLE      200                                          --- 4 -->
678500970401     C     WINDCA        ANDGT     *ZEROS
678600930517     C                   Z-ADD     WINDCA        I
678700930517     C                   MOVEL     DCALEN        CAK(I)
678800941227     C                   MOVEL     CLNMAT        CAM(I)
678900941227     C                   MOVEL     CLNPOM        CAP(I)
679000970401     C                   END                                                    <-- 4 ---
679100970401     C                   ENDIF                                                  <-- 3 ---
679200930517     C                   END                                                    <-- 2 ---
679300930517     C                   ELSE                                                   <-- 1 -->
679400930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
679500941227     C                   MOVEL     CAM(I)        CLNMAT
679600941227     C                   MOVEL     CAP(I)        CLNPOM
679700930517     C                   END                                                    <-- 1 ---
679800930517     C*
679900930517     C                   ENDSR
680000010821     C********************************************************************
680100010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
680200010821     C********************************************************************
680300010821     C     CHKVCA        BEGSR
680400010821     C*
680500010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
680600010821     C     D98CAS        IFGT      *ZEROS
680700010821     C     D98VCA        ANDEQ     *BLANKS
680800010821     C                   MOVEL     'ITL'         D98VCA
680900010821     C                   ENDIF
681000010822     C*
681100010822     C     D98VCA        IFNE      'ITL'
681200010822     C     D98VCA        ANDNE     'EUR'
681300010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
681400010822     C                   Z-ADD     D98CAS        INPIMP
681500010822     C                   MOVEL     D98VCA        INPDIV
681600010822     C                   EXSR      EXECNV
681700010822     C                   Z-ADD     OUTIMP        D98CAS
681800010822     C                   MOVEL     §GEDCN        D98VCA
681900010822     C                   ENDIF
682000010821     C*
682100010821     C                   ENDSR
682200010821     C********************************************************************
682300010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
682400010821     C********************************************************************
682500010821     C     EXECNV        BEGSR
682600010821     C*
682700010821     C                   CLEAR                   YEURDS
682800010821     C*
682900010821     C                   MOVEL     INPDIV        YECDVI
683000010821     C                   Z-ADD     INPIMP        YECIMI
683100010821     C                   MOVEL     §GEDCN        YECDVO
683200010821     C                   MOVEL     '3'           YECDCO
683300010821     C                   MOVEL     'H'           YECTAR
683400010821     C                   CALL      'YEURCO'
683500010821     C                   PARM                    YEURDS
683600010821     C*
683700010821     C                   Z-ADD     YECIMO        OUTIMP
683800010821     C*
683900010821     C                   ENDSR
684000011211     C*------------------------------------------------------------------------*
684100011211     C* Controllo importo massimo contrassegno.
684200011211     C*------------------------------------------------------------------------*
684300011211     C     REPMAXCSB     BEGSR
684400011211     C*
684500011211     C                   CLEAR                   Trul21DS
684600011211     C*
684700160420     c                   eval      i21cbo = d98cbo
684800011211     C                   EVAL      I21TSp = d98TSP
684900011211     C                   EVAL      I21NzD = d98NZD
685000011211     C                   EVAL      I21LnP = d98LNP
685100011211     C                   EVAL      I21LnA = d98LNA
685200011211     C                   EVAL      I21Ksc = d98KSC
685300011211     C                   EVAL      I21TiC = d98TIC
685400011211     C                   EVAL      I21Imp = d98CAS
685500011211     C                   EVAL      I21Div = d98VCA
685600160420     c                   eval      i21cbo = d98cbo
685700011211     C*
685800011211     C                   CALL      'TRUL21R'
685900011211     C                   PARM                    Trul21DS
686000160420
686100160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
686200160420     c                   if        wtrasm='N'
686300160420     c                   if        o21fca <>*blanks
686400160420     c                   eval      §b4ltca = 'S'
686500160420     c                   endif
686600160420     c                   endif
686700011211     C*
686800011211     C                   ENDSR
686900011211     C*------------------------------------------------------------------------*
687000011211
687100930414     C********************************************************************
687200930414     C*   RUTINZ - ROUTINE INIZIALE
687300930414     C********************************************************************
687400930414     C     RUTINZ        BEGSR
687500930414     C*
687600930414     C* PARAMETRI IN INPUT ALLA ROUTINE
687700930414     C     *ENTRY        PLIST
687800970731     C                   PARM                    DS99
687900941227     C*
688000941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
688100941227     C     KEYBLP        KLIST
688200941227     C                   KFLD                    BLPAAS
688300941227     C                   KFLD                    BLPLNP
688400941227     C                   KFLD                    BLPNRS
688500941227     C                   KFLD                    BLPNSP
688600050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
688700941227     C     KEYGCP        KLIST
688800050405     C                   KFLD                    KWGCPAAS
688900050405     C                   KFLD                    KWGCPLNP
689000050405     C                   KFLD                    KWGCPNRS
689100050405     C                   KFLD                    KWGCPNSP
689200050405     C                   KFLD                    KWGCPFRG
689300050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
689400031125     C     KEYGC_P       KLIST
689500050405     C                   KFLD                    KWGCPAAS
689600050405     C                   KFLD                    KWGCPLNP
689700050405     C                   KFLD                    KWGCPNRS
689800050405     C                   KFLD                    KWGCPNSP
689900970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
690000150120     C**   KEYBLT        KLIST
690100150120     C**                 KFLD                    BLTAAS
690200150120     C**                 KFLD                    BLTLNP
690300150120     C**                 KFLD                    BLTNRS
690400150120     C**                 KFLD                    BLTNSP
690500941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
690600941227     C     KEYARB        KLIST
690700941227     C                   KFLD                    ARBAAS
690800941227     C                   KFLD                    ARBLNP
690900941227     C                   KFLD                    ARBNRS
691000941227     C                   KFLD                    ARBNSP
691100051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
691200941228     C     KEYAR9        KLIST
691300941228     C                   KFLD                    AR9AAS
691400941228     C                   KFLD                    AR9LNP
691500941228     C                   KFLD                    AR9NRS
691600941228     C                   KFLD                    AR9NSP
691700941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
691800050405     C     KEYGCP21      KLIST
691900050405     C                   KFLD                    KWGCPAAS
692000050405     C                   KFLD                    KWGCPLNP
692100050405     C                   KFLD                    KWGCPNRS
692200050405     C                   KFLD                    KWGCPNSP
692300050405     C                   KFLD                    KWGCPFRG
692400970912     C* CHIAVE LETTURA FNART01L - PARZIALE
692500150120     C***  KEYART        KLIST
692600150120     C***                KFLD                    ARTAAS
692700150120     C***                KFLD                    ARTLNP
692800150120     C***                KFLD                    ARTNRS
692900150120     C***                KFLD                    ARTNSP
693000950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
693100950102     C     KEYLBL        KLIST
693200950102     C                   KFLD                    LBLAAN
693300950102     C                   KFLD                    LBLLPN
693400950102     C                   KFLD                    LBLNRN
693500950102     C                   KFLD                    LBLNSN
693600930414     C*
693700070827     c     kAR5          Klist
693800070827     c                   Kfld                    d98Aas
693900070827     c                   Kfld                    d98Lnp
694000070827     c                   Kfld                    d98nrs
694100070827     c                   Kfld                    d98nsp
694200070827     c                   Kfld                    kAr5Trd
694300150325     c     kAR4          Klist
694400150325     c                   Kfld                    d98Aas
694500150325     c                   Kfld                    d98Lnp
694600150325     c                   Kfld                    d98nrs
694700150325     c                   Kfld                    d98nsp
694800150325     c                   Kfld                    kAr4Trc
694900150218     c*
695000150218     c     kart          Klist
695100150218     c                   Kfld                    d98Aas
695200150218     c                   Kfld                    d98Lnp
695300150218     c                   Kfld                    d98nrs
695400150218     c                   Kfld                    d98nsp
695500150218     c     kbrv          Klist
695600150218     c                   Kfld                    artfls
695700150218     c                   Kfld                    artlna
695800150218     c                   Kfld                    artnrs
695900150218     c                   Kfld                    artnsc
696000150325     C     Kfapd         klist
696100150325     C                   kfld                    kpdtip
696200150325     C                   kfld                    kpdpdr
696300930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
696400930414     C     KEYTAB        KLIST
696500930414     C                   KFLD                    KTBUTE
696600930414     C                   KFLD                    KTBCOD
696700930414     C                   KFLD                    KTBKEY
696800060406     C     Ktab          KLIST
696900060406     C                   KFLD                    KTBUTE
697000060406     C                   KFLD                    KTBCOD
697100930414     C*
697200941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
697300941227     C     KEYCLN        KLIST
697400941227     C                   KFLD                    CLNTFP
697500941227     C                   KFLD                    CLNTFA
697600941227     C                   KFLD                    CLNANN
697700941227     C                   KFLD                    CLNMES
697800941227     C*
697900941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
698000941227     C     KEYCAS        KLIST
698100941227     C                   KFLD                    CASPTA
698200941227     C                   KFLD                    CASLNP
698300941227     C                   KFLD                    CASLNA
698400941227     C                   KFLD                    CASTSP
698500941227     C                   KFLD                    CASDDE
698600930420     C*
698700990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
698800930420     C     KEYEVB        KLIST
698900930420     C                   KFLD                    KEBAAS
699000930420     C                   KFLD                    KEBLNP
699100930420     C                   KFLD                    KEBNRS
699200930420     C                   KFLD                    KEBNSP
699300010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
699400010821     C     KTBE          KLIST
699500010821     C                   KFLD                    TBECOD
699600010821     C                   KFLD                    TBEKE1
699700010821     C                   KFLD                    TBEKE2
699800930414     C*
699900010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
700000930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
700100930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
700200930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
700300930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
700400930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
700500930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
700600930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
700700930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
700800930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
700900010821     C     *LIKE         DEFINE    D98CAS        INPIMP
701000010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
701100010821     C     *LIKE         DEFINE    D98VCA        INPDIV
701200930414     C*
701300930414     C* RECUPERO IL CODICE DELL'AZIENDA
701400930414     C                   Z-ADD     1             CODUT             1 0
701500970701     C                   Z-ADD     *ZEROS        I                 6 0
701600930414     C*
701700930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
701800930414     C                   EXSR      AZZERA
701900930414     C*
702000930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
702100941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
702200941227     C                   MOVEL     'S'           CTABSN            1
702300930414     C                   EXSR      CARTAB
702400941227     C                   ENDIF
702500041116     c* Se passato umero di spedizioni apro i files , altrimenti no
702600041116     c                   if        d98nsp>0 and wapri=' '
702700041116     c                   open      fnblp01l
702800050405     c                   open      tigcp01l
702900150120     c***                open      fnblt01l
703000041116     c                   open      fnarb01l
703100051115     c                   open      fiar901l
703200150325     c                   open      fiar401l
703300050405     c                   open      tigcp21l
703400150218     c                   open      fnart01l
703500150218     c                   open      fnbrv07l
703600041116     c                   open      fnlbl01l
703700041116     c                   open      fnevb01l
703800070827     c                   open      fiar501l
703900041116     c                   eval      wapri='S'
704000041116     c                   endif
704100930414     C*
704200930414     C                   ENDSR
704300941227     C*------------------------------------------------------------------------*
704400941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
704500941227     C*------------------------------------------------------------------------*
704600941227     C     SUBDAT        BEGSR
704700941227     C*
704800941227     C                   TIME                    DTTIME
704900941227     C                   Z-ADD     DTANN         DSANN
705000941227     C                   Z-ADD     DTMES         DSMES
705100941227     C                   Z-ADD     DTGIO         DSGIO
705200941227     C                   Z-ADD     DTSEC         DSSEC
705300941227     C*
705400941227     C                   ENDSR
705500930414     C********************************************************************
705600930414     C*   CARTAB - CARICA LE TABELLE
705700930414     C********************************************************************
705800930414     C     CARTAB        BEGSR
705900930414     C*
706000930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
706100930414     C     D98ERR        IFEQ      '0'
706200930421     C                   Z-ADD     0             I
706300930414     C                   Z-ADD     CODUT         KTBUTE
706400930414     C                   MOVEL     '1Y'          KTBCOD
706500930414     C                   MOVEL     *BLANKS       KTBKEY
706600930414     C     KEYTAB        SETLL     TABEL00F                           99
706700930414     C  N99              READ      TABEL00F                               99
706800930414     C     *IN99         IFEQ      '1'
706900930414     C                   MOVEL     '1'           D98ERR
707000930414     C                   MOVEL     MSG(5)        D98MSG
707100930414     C                   END
707200930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
707300930414     C     TBLCOD        ANDEQ     '1Y'
707400930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
707500930421     C                   ADD       1             I
707600930421     C                   MOVEL     TBLKEY        C1Y(I)
707700930414     C                   MOVEL     TBLUNI        DS1Y
707800930421     C                   MOVEL     §1YFEI        I1Y(I)
707900930421     C                   MOVEL     §1YFEC        E1Y(I)
708000930414     C                   END                                                    <-- 3 ---
708100930414     C                   READ      TABEL00F                               99
708200930414     C                   END                                                    <-- 2 ---
708300930414     C                   END
708400930414     C*
708500930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
708600930414     C     D98ERR        IFEQ      '0'
708700930421     C                   Z-ADD     0             I
708800930414     C                   Z-ADD     CODUT         KTBUTE
708900930414     C                   MOVEL     '1X'          KTBCOD
709000930414     C                   MOVEL     *BLANKS       KTBKEY
709100930414     C     KEYTAB        SETLL     TABEL00F                           99
709200930414     C  N99              READ      TABEL00F                               99
709300930414     C     *IN99         IFEQ      '1'
709400930414     C                   MOVEL     '1'           D98ERR
709500930414     C                   MOVEL     MSG(1)        D98MSG
709600930414     C                   END
709700930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
709800930414     C     TBLCOD        ANDEQ     '1X'
709900930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
710000930421     C                   ADD       1             I
710100930421     C                   MOVEL     TBLKEY        C1X(I)
710200930414     C                   MOVEL     TBLUNI        DS1X
710300930421     C                   Z-ADD     §1XHHG        G1X(I)
710400930421     C                   Z-ADD     §1XHHO        O1X(I)
710500930421     C                   Z-ADD     §1XHHM        M1X(I)
710600930421     C                   Z-ADD     §1XHHP        P1X(I)
710700930421     C                   Z-ADD     §1XHHI        I1X(I)
710800930421     C                   Z-ADD     §1XHHS        S1X(I)
710900930414     C                   END                                                    <-- 3 ---
711000930414     C                   READ      TABEL00F                               99
711100931223     C                   ENDDO                                                  <-- 2 ---
711200050523     c
711300050523     c* controllo riempimento skiere
711400050523     c                   clear                   trul0sds
711500050523     c                   eval      i0sski='C1X/G1X/O1X/...'
711600050523     c                   eval      i0sele=%elem(c1x)
711700050523     c                   eval      i0spie=i
711800050523     c                   eval      i0sfile='TABEL00F  '
711900050523     c                   eval      i0scod=' 1X  '
712000050523     c                   eval      i0spgm='TNSD99R'
712100050523     c                   movel     trul0sds      kpjbu
712200050523     c                   call      'TRUL0SR'
712300050523     c                   parm                    kpjba
712400050523     C                   END
712500930421     C*
712600930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
712700930421     C     D98ERR        IFEQ      '0'
712800930421     C                   Z-ADD     0             I
712900930421     C                   Z-ADD     CODUT         KTBUTE
713000930421     C                   MOVEL     '2A'          KTBCOD
713100060406     C     Ktab          SETLL     TABEL00F                           99
713200060406     C  N99ktab          READe     TABEL00F                               99
713300930421     C     *IN99         IFEQ      '1'
713400930421     C                   MOVEL     '1'           D98ERR
713500930421     C                   MOVEL     MSG(8)        D98MSG
713600930421     C                   END
713700930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
713800930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
713900930421     C                   ADD       1             I
714000930421     C                   MOVEL     TBLKEY        C2A(I)
714100060406     C                   MOVEL     TBLUNI        ds2a
714200060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
714300060406     C                   MOVEL     w003a         D2A(I)
714400930421     C                   END                                                    <-- 3 ---
714500060406     C     ktab          READe     TABEL00F                               99
714600931223     C                   ENDDO                                                  <-- 2 ---
714700050523     c* controllo riempimento skiere
714800050523     c                   clear                   trul0sds
714900050523     c                   eval      i0sski='C2A/D2A'
715000050523     c                   eval      i0sele=%elem(c2a)
715100050523     c                   eval      i0spie=i
715200050523     c                   eval      i0sfile='TABEL00F  '
715300060406     c                   eval      i0scod='2A/2Z'
715400050523     c                   eval      i0spgm='TNSD99R'
715500050523     c                   movel     trul0sds      kpjbu
715600050523     c                   call      'TRUL0SR'
715700050523     c                   parm                    kpjba
715800930421     C                   END
715900060406     c* Tabella  2Z
716000060406     C     D98ERR        IFEQ      '0'
716100060406     C                   Z-ADD     CODUT         KTBUTE
716200060406     C                   MOVEL     '2Z'          KTBCOD
716300060406     C     Ktab          SETLL     TABEL00F                           99
716400060406     C  N99ktab          READe     TABEL00F                               99
716500060406     C     *IN99         IFEQ      '1'
716600060406     C                   MOVEL     '1'           D98ERR
716700060406     C                   MOVEL     MSG(33)       D98MSG
716800060406     C                   END
716900060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
717000060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
717100060406     C                   Z-ADD     1             I
717200060406     C                   MOVEL     TBLKEY        w003a
717300060406     c     w003a         lookup    c2a(i)                                 99
717400060406     C     *IN99         IFEQ      *off
717500060406     C                   MOVEL     '1'           D98ERR
717600060406     C                   MOVEL     MSG(34)       D98MSG
717700060406     C                   Else
717800060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
717900060406     C                   END                                                    <-- 3 ---
718000060406     C                   END                                                    <-- 3 ---
718100060406     C     ktab          READe     TABEL00F                               99
718200060406     C                   ENDDO                                                  <-- 2 ---
718300060406     C                   END
718400941227     C*
718500941227     C* CARICA TABELLA CODICI BOLLA -3A-
718600941227     C     D98ERR        IFEQ      '0'
718700941228     C                   Z-ADD     0             I
718800941227     C                   Z-ADD     CODUT         KTBUTE
718900941227     C                   MOVEL     '3A'          KTBCOD
719000941227     C                   MOVEL     *BLANKS       KTBKEY
719100941227     C     KEYTAB        SETLL     TABEL00F                           99
719200941227     C  N99              READ      TABEL00F                               99
719300941227     C     *IN99         IFEQ      '1'
719400941227     C                   MOVEL     '1'           D98ERR
719500941227     C                   MOVEL     MSG(22)       D98MSG
719600941227     C                   END
719700941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
719800941227     C     TBLCOD        ANDEQ     '3A'
719900941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
720000941227     C                   ADD       1             I
720100941227     C                   MOVEL     TBLKEY        C3A(I)
720200941227     C                   MOVEL     TBLUNI        DS3A
720300941227     C                   MOVEL     §3ATB1        T3A(I)
720400941228     C                   Z-ADD     §3AFCA        F3A(I)
720500990308     C                   MOVEL     §3ARBL        R3A(I)
720600941227     C                   END                                                    <-- 3 ---
720700941227     C                   READ      TABEL00F                               99
720800941227     C                   END                                                    <-- 2 ---
720900050523     c* controllo riempimento skiere
721000050523     c                   clear                   trul0sds
721100050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
721200050523     c                   eval      i0sele=%elem(c3a)
721300050523     c                   eval      i0spie=i
721400050523     c                   eval      i0sfile='TABEL00F  '
721500050523     c                   eval      i0scod=' 3A  '
721600050523     c                   eval      i0spgm='TNSD99R'
721700050523     c                   movel     trul0sds      kpjbu
721800050523     c                   call      'TRUL0SR'
721900050523     c                   parm                    kpjba
722000941227     C                   END
722100971210     C*
722200020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
722300020226     C                   Z-ADD     *ZEROS        J                 3 0
722400160331     C                   Z-ADD     *ZEROS        D                 3 0
722500971210IF  0C     D98ERR        IFEQ      '0'
722600971210     C     *LOVAL        SETLL     AZORG01L
722700971210     C                   READ      AZORG01L                               98
722800971210DO  1C     *IN98         DOWEQ     *OFF
722900971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
723000971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
723100971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
723200020627     C                   MOVEL     ORGDE3        OG143
723300020702     C*
723400020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
723500020702     C                   CLEAR                   DNTW
723600020702     C*
723700020702     C                   MOVEL(P)  'NTW'         TBECOD
723800020702     C                   MOVEL(P)  §OGNTW        TBEKE1
723900020702     C     KTBE          CHAIN     TNTBE01L
724000020702     C                   IF        %found(TNTBE01L)
724100020702     C                   MOVEL     TBEUNI        DNTW
724200020702     C                   ENDIF
724300020702     C*
724400020702     C                   MOVEL     §NTWFIE       FL1(F)
724500020627     C*
724600020226     C* VERIFICO SE FILIALE FEDEX
724700020226     C                   IF        §OGNTW = 'FED'
724800020226     C                   ADD       1             J
724900020226     C                   Z-ADD     ORGFIL        FILFED(J)
725000020226     C                   ENDIF
725100160331     C* VERIFICO SE FILIALE DPD
725200160331     C                   IF        §OGNTW = 'DPD'
725300160331     C                   ADD       1             D
725400160331     C                   Z-ADD     ORGFIL        FILDPD(D)
725500160331     C                   ENDIF
725600020226     C*
725700971210E   2C                   ENDIF
725800971210     C                   READ      AZORG01L                               98
725900971210E   1C                   ENDDO
726000971210E   0C                   ENDIF
726100010821     C*
726200010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
726300010821     C                   MOVEL     'GED'         TBECOD
726400070514     C                   MOVEL     *BLANKS       TBEKE1
726500070514     C                   MOVEL     '1'           TBEKE1
726600010821     C                   MOVEL     *BLANKS       TBEKE2
726700010821     C     KTBE          CHAIN     TNTBE01L                           21
726800070514     C  N21              MOVEL     TBEUNI        DGED
726900150326     c
727000150326     C* REPERISCO LA tabella flag operativi per tabella OLP
727100150326     c                   clear                   dvpo
727200150326     C                   MOVEL     'VPO'         TBECOD
727300150326     C                   MOVEL     *BLANKS       TBEKE2
727400150326     C                   MOVEL(P)  'VPO'         TBEKE1
727500150326     C     KTBE          CHAIN     TNTBE01L                           21
727600150326     C  N21              MOVEL     TBEUNI        DVPO
727700150326     C*
727800031201     C*
727900031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
728000031201     C     D98ERR        IFEQ      '0'
728100060403     C                   Z-ADD     0             I7QW              3 0
728200031201     C                   Z-ADD     CODUT         KTBUTE
728300031201     C                   MOVEL     '7Q'          KTBCOD
728400031201     C                   MOVEL     *BLANKS       KTBKEY
728500031201     C     KEYTAB        SETLL     TABEL00F                           99
728600031201     C  N99              READ      TABEL00F                               99
728700031201     C     *IN99         IFEQ      '1'
728800031201     C                   MOVEL     '1'           D98ERR
728900031201     C                   MOVEL     MSG(8)        D98MSG
729000031201     C                   ENDIF
729100031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
729200031201     C     TBLCOD        ANDEQ     '7Q'
729300031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
729400031201     C                   MOVEL     TBLUNI        DS7Q
729500031201     C                   IF        §7Q4D1 = 'R' OR
729600031201     C                             §7Q4D2 = 'R' OR
729700031201     C                             §7Q4D3 = 'R' OR
729800031201     C                             §7Q4D4 = 'R' OR
729900031201     C                             §7Q4D5 = 'R'
730000031201     C                   ADD       1             I7QW
730100031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
730200031201     C                   ENDIF                                                  <-- 3 ---
730300031201     C                   ENDIF                                                  <-- 3 ---
730400031201     C                   READ      TABEL00F                               99
730500031201     C                   ENDDO                                                  <-- 2 ---
730600031201     C                   ENDIF
730700031201     C*
730800031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
730900031201     C     D98ERR        IFEQ      '0'
731000031201     C                   Z-ADD     CODUT         KTBUTE
731100031201     C                   MOVEL     '7W'          KTBCOD
731200031201     C                   MOVEL     *BLANKS       KTBKEY
731300031201     C     KEYTAB        SETLL     TABEL00F                           99
731400031201     C  N99              READ      TABEL00F                               99
731500031201     C     *IN99         IFEQ      '1'
731600031201     C                   MOVEL     '1'           D98ERR
731700031201     C                   MOVEL     MSG(8)        D98MSG
731800031201     C                   ENDIF
731900031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
732000031201     C     TBLCOD        ANDEQ     '7W'
732100031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
732200031201     C                   MOVEL     TBLUNI        DS7W
732300031201     C                   IF        §7W6D1 = 'C' OR
732400031201     C                             §7W6D2 = 'C' OR
732500031201     C                             §7W6D3 = 'C' OR
732600031201     C                             §7W6D4 = 'C' OR
732700031201     C                             §7W6D5 = 'C'
732800031201     C                   ADD       1             I7QW
732900031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
733000031201     C                   ENDIF                                                  <-- 3 ---
733100031201     C                   ENDIF                                                  <-- 3 ---
733200031201     C                   READ      TABEL00F                               99
733300031201     C                   ENDDO                                                  <-- 2 ---
733400050523     c* controllo riempimento skiere
733500050523     c                   clear                   trul0sds
733600050523     c                   eval      i0sski='SK7QW'
733700050523     c                   eval      i0sele=%elem(sk7qw)
733800050523     c                   eval      i0spie=i7qw
733900050523     c                   eval      i0sfile='TABEL00F  '
734000050523     c                   eval      i0scod='7Q-7W'
734100050523     c                   eval      i0spgm='TNSD99R'
734200050523     c                   movel     trul0sds      kpjbu
734300050523     c                   call      'TRUL0SR'
734400050523     c                   parm                    kpjba
734500031201     C                   ENDIF
734600930414     C*
734700930414     C                   ENDSR
734800930414     C********************************************************************
734900930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
735000930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
735100930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
735200930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
735300930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
735400930414     C********************************************************************
735500930414     C     RO3ORE        BEGSR
735600930414     C*
735700930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
735800930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
735900930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
736000050902     c                   movel     ' '           WR3MATPOM         1
736100931209     C                   Z-ADD     *ZEROS        WR3HHO
736200931209     C                   Z-ADD     *ZEROS        WR3HHA
736300930414     C                   MOVE      WR3TAB        WR3COD
736400930604     C                   MOVEL     D98LNA        WR3COD            8
736500930421     C                   Z-ADD     1             I
736600930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
736700930414     C     *IN99         IFEQ      '0'
736800930414     C                   MOVE      WR3TAB        WR3COD
736900930414     C                   MOVEL     '   '         WR3COD            8
737000930421     C                   Z-ADD     1             I
737100930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
737200930414     C                   END
737300930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
737400930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
737500930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
737600930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
737700930414     C                   ELSE                                                   <-- 2 -->
737800930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
737900930421     C                   Z-ADD     M1X(I)        WR3HHO
738000930510     C                   Z-ADD     I1X(I)        WR3HHA
738100050902     c                   movel     'M'           WR3MATPOM         1
738200930414     C                   END
738300930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
738400930421     C                   Z-ADD     P1X(I)        WR3HHO
738500930510     C                   Z-ADD     S1X(I)        WR3HHA
738600050902     c                   movel     'P'           WR3MATPOM
738700930414     C                   END
738800930414     C                   END                                                    <-- 2 ---
738900930414     C                   END                                                    <-- 1 ---
739000930414     C*
739100930414     C                   ENDSR
739200930423     C********************************************************************
739300930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
739400930423     C********************************************************************
739500930423     C     W98D98        BEGSR
739600930423     C*
739700941228     C*
739800941228     C* ORE REALI PARTENZA INTERNE
739900941228     C                   Z-ADD     W98RPI        W98               3 0
740000941228     C                   EXSR      W98999
740100941228     C                   Z-ADD     D98           D98RPI
740200941228     C* ORE REALI PARTENZA CLIENTE
740300941228     C                   Z-ADD     W98RPC        W98
740400941228     C                   EXSR      W98999
740500941228     C                   Z-ADD     D98           D98RPC
740600930423     C* ORE REALI TRAZIONE INTERNE
740700941228     C                   Z-ADD     W98RTI        W98
740800941228     C                   EXSR      W98999
740900941228     C                   Z-ADD     D98           D98RTI
741000930423     C* ORE REALI TRAZIONE CLIENTE
741100941228     C                   Z-ADD     W98RTC        W98
741200941228     C                   EXSR      W98999
741300941228     C                   Z-ADD     D98           D98RTC
741400930423     C* ORE REALI CONSEGNA INTERNE
741500941228     C                   Z-ADD     W98RCI        W98
741600941228     C                   EXSR      W98999
741700941228     C                   Z-ADD     D98           D98RCI
741800930423     C* ORE REALI CONSEGNA CLIENTE
741900941228     C                   Z-ADD     W98RCC        W98
742000941228     C                   EXSR      W98999
742100941228     C                   Z-ADD     D98           D98RCC
742200930423     C* ORE REALI RESA INTERNE
742300941228     C                   Z-ADD     W98RRI        W98
742400941228     C                   EXSR      W98999
742500941228     C                   Z-ADD     D98           D98RRI
742600930423     C* ORE REALI RESA CLIENTE
742700941228     C                   Z-ADD     W98RRC        W98
742800941228     C                   EXSR      W98999
742900941228     C                   Z-ADD     D98           D98RRC
743000941228     C* ORE DIFF. PARTENZA INTERNE
743100941228     C                   Z-ADD     W98FPI        W98
743200941228     C                   EXSR      W98999
743300941228     C                   Z-ADD     D98           D98FPI
743400941228     C* ORE DIFF. PARTENZA CLIENTE
743500941228     C                   Z-ADD     W98FPC        W98
743600941228     C                   EXSR      W98999
743700941228     C                   Z-ADD     D98           D98FPC
743800930423     C* ORE DIFF. TRAZIONE INTERNE
743900941228     C                   Z-ADD     W98FTI        W98
744000941228     C                   EXSR      W98999
744100941228     C                   Z-ADD     D98           D98FTI
744200930423     C* ORE DIFF. TRAZIONE CLIENTE
744300941228     C                   Z-ADD     W98FTC        W98
744400941228     C                   EXSR      W98999
744500941228     C                   Z-ADD     D98           D98FTC
744600930423     C* ORE DIFF. CONSEGNA INTERNE
744700941228     C                   Z-ADD     W98FCI        W98
744800941228     C                   EXSR      W98999
744900941228     C                   Z-ADD     D98           D98FCI
745000930423     C* ORE DIFF. CONSEGNA CLIENTE
745100941228     C                   Z-ADD     W98FCC        W98
745200941228     C                   EXSR      W98999
745300941228     C                   Z-ADD     D98           D98FCC
745400930423     C* ORE DIFF. RESA INTERNE
745500941228     C                   Z-ADD     W98FRI        W98
745600941228     C                   EXSR      W98999
745700941228     C                   Z-ADD     D98           D98FRI
745800930423     C* ORE DIFF. RESA CLIENTE
745900941228     C                   Z-ADD     W98FRC        W98
746000941228     C                   EXSR      W98999
746100941228     C                   Z-ADD     D98           D98FRC
746200941228     C* ORE NON CONF. PARTENZA INTERNE
746300941228     C     W98NPI        IFGT      999
746400941228     C                   Z-ADD     999           D98NPI
746500941228     C                   ELSE
746600941228     C                   Z-ADD     W98NPI        D98NPI
746700941228     C                   END
746800941228     C* ORE NON CONF. PARTENZA CLIENTE
746900941228     C     W98NPC        IFGT      999
747000941228     C                   Z-ADD     999           D98NPC
747100941228     C                   ELSE
747200941228     C                   Z-ADD     W98NPC        D98NPC
747300941228     C                   END
747400930423     C* ORE NON CONF. TRAZIONE INTERNE
747500930423     C     W98NTI        IFGT      999
747600930423     C                   Z-ADD     999           D98NTI
747700930423     C                   ELSE
747800930423     C                   Z-ADD     W98NTI        D98NTI
747900930423     C                   END
748000930423     C* ORE NON CONF. TRAZIONE CLIENTE
748100930423     C     W98NTC        IFGT      999
748200930423     C                   Z-ADD     999           D98NTC
748300930423     C                   ELSE
748400930423     C                   Z-ADD     W98NTC        D98NTC
748500930423     C                   END
748600930423     C* ORE NON CONF. CONSEGNA INTERNE
748700930423     C     W98NCI        IFGT      999
748800930423     C                   Z-ADD     999           D98NCI
748900930423     C                   ELSE
749000930423     C                   Z-ADD     W98NCI        D98NCI
749100930423     C                   END
749200930423     C* ORE NON CONF. CONSEGNA CLIENTE
749300930423     C     W98NCC        IFGT      999
749400930423     C                   Z-ADD     999           D98NCC
749500930423     C                   ELSE
749600930423     C                   Z-ADD     W98NCC        D98NCC
749700930423     C                   END
749800930423     C* ORE NON CONF. RESA INTERNE
749900930423     C     W98NRI        IFGT      999
750000930423     C                   Z-ADD     999           D98NRI
750100930423     C                   ELSE
750200930423     C                   Z-ADD     W98NRI        D98NRI
750300930423     C                   END
750400930423     C* ORE NON CONF. RESA CLIENTE
750500930423     C     W98NRC        IFGT      999
750600930423     C                   Z-ADD     999           D98NRC
750700930423     C                   ELSE
750800930423     C                   Z-ADD     W98NRC        D98NRC
750900930423     C                   END
751000930423     C* ORE X TURNO CHIUSURA INTERNE
751100941228     C                   Z-ADD     W98HHI        W98
751200941228     C                   EXSR      W98999
751300941228     C                   Z-ADD     D98           D98HHI
751400930423     C* ORE X TURNO CHIUSURA CLIENTE
751500941228     C                   Z-ADD     W98HHC        W98
751600941228     C                   EXSR      W98999
751700941228     C                   Z-ADD     D98           D98HHC
751800060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
751900060418     C                   Z-ADD     W98EGI        W98
752000060418     C                   EXSR      W98999
752100060418     C                   Z-ADD     D98           D98hEGI
752200060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
752300060418     C                   Z-ADD     W98EGC        W98
752400060418     C                   EXSR      W98999
752500060418     C                   Z-ADD     D98           D98hEGC
752600060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
752700060420     C                   Z-ADD     W98EAI        W98
752800060420     C                   EXSR      W98999
752900060420     C                   Z-ADD     D98           D98hEAI
753000060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
753100060420     C                   Z-ADD     W98EAC        W98
753200060420     C                   EXSR      W98999
753300060420     C                   Z-ADD     D98           D98hEAC
753400930423     C* ORE X LASC. AVVISO INTERNE
753500941228     C                   Z-ADD     W98HLI        W98
753600941228     C                   EXSR      W98999
753700941228     C                   Z-ADD     D98           D98HLI
753800930423     C* ORE X LASC. AVVISO CLIENTE
753900941228     C                   Z-ADD     W98HLC        W98
754000941228     C                   EXSR      W98999
754100941228     C                   Z-ADD     D98           D98HLC
754200930423     C* ORE X GIACENZA INTERNE
754300941228     C                   Z-ADD     W98HGI        W98
754400941228     C                   EXSR      W98999
754500941228     C                   Z-ADD     D98           D98HGI
754600930423     C* ORE X GIACENZA CLIENTE
754700941228     C                   Z-ADD     W98HGC        W98
754800941228     C                   EXSR      W98999
754900941228     C                   Z-ADD     D98           D98HGC
755000941228     C* ORE X FEST. PART. INTERNE
755100941228     C                   Z-ADD     W98HPI        W98
755200941228     C                   EXSR      W98999
755300941228     C                   Z-ADD     D98           D98HPI
755400941228     C* ORE X FEST. PART. CLIENTE
755500941228     C                   Z-ADD     W98HPC        W98
755600941228     C                   EXSR      W98999
755700941228     C                   Z-ADD     D98           D98HPC
755800930423     C* ORE X FEST. TRAZ. INTERNE
755900941228     C                   Z-ADD     W98HFI        W98
756000941228     C                   EXSR      W98999
756100941228     C                   Z-ADD     D98           D98HFI
756200930423     C* ORE X FEST. TRAZ. CLIENTE
756300941228     C                   Z-ADD     W98HFC        W98
756400941228     C                   EXSR      W98999
756500941228     C                   Z-ADD     D98           D98HFC
756600930423     C* ORE X FEST. CONS. INTERNE
756700941228     C                   Z-ADD     W98HEI        W98
756800941228     C                   EXSR      W98999
756900941228     C                   Z-ADD     D98           D98HEI
757000930423     C* ORE X FEST. CONS. CLIENTE
757100941228     C                   Z-ADD     W98HEC        W98
757200941228     C                   EXSR      W98999
757300941228     C                   Z-ADD     D98           D98HEC
757400930511     C* ORE X F.DEPOSITO INTERNE
757500941228     C                   Z-ADD     W98HDI        W98
757600941228     C                   EXSR      W98999
757700941228     C                   Z-ADD     D98           D98HDI
757800930511     C* ORE X F.DEPOSITO CLIENTE
757900941228     C                   Z-ADD     W98HDC        W98
758000941228     C                   EXSR      W98999
758100941228     C                   Z-ADD     D98           D98HDC
758200930511     C* ORE X CONTRASSEGNO INTERNE
758300941228     C                   Z-ADD     W98HOI        W98
758400941228     C                   EXSR      W98999
758500941228     C                   Z-ADD     D98           D98HOI
758600930511     C* ORE X CONTRASSEGNO CLIENTE
758700941228     C                   Z-ADD     W98HOC        W98
758800941228     C                   EXSR      W98999
758900941228     C                   Z-ADD     D98           D98HOC
759000930914     C* ORE DI CONSEGNABILITA' INTERNE
759100941228     C                   Z-ADD     W98OCI        W98
759200941228     C                   EXSR      W98000
759300941228     C                   Z-ADD     D98           D98OCI
759400930914     C* ORE DI CONSEGNABILITA' CLIENTE
759500941228     C                   Z-ADD     W98OCC        W98
759600941228     C                   EXSR      W98000
759700941228     C                   Z-ADD     D98           D98OCC
759800071122     C
759900071122     c* Per i codici bolla M prepagati annullati, azzero tutto
760000071122     c                   if        d98cbo='M '
760100071122     c                   clear                   d98rpi
760200071122     c                   clear                   d98rpc
760300071122     c                   clear                   d98rti
760400071122     c                   clear                   d98rtc
760500071122     c                   clear                   d98rci
760600071122     c                   clear                   d98rcc
760700071122     c                   clear                   d98rri
760800071122     c                   clear                   d98rrc
760900071122     c                   clear                   d98fpi
761000071122     c                   clear                   d98fpc
761100071122     c                   clear                   d98fti
761200071122     c                   clear                   d98ftc
761300071122     c                   clear                   d98fci
761400071122     c                   clear                   d98fcc
761500071122     c                   clear                   d98fri
761600071122     c                   clear                   d98frc
761700071122     c                   clear                   d98npi
761800071122     c                   clear                   d98npc
761900071122     c                   clear                   d98nti
762000071122     c                   clear                   d98ntc
762100071122     c                   clear                   d98nci
762200071122     c                   clear                   d98ncc
762300071122     c                   clear                   d98nri
762400071122     c                   clear                   d98nrc
762500071122     c                   clear                   d98OCi
762600071122     c                   clear                   d98OCC
762700071122     c                   endif
762800930423     C*
762900100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
763000100308     c*  calcolo
763100100308     c                   eval      d98dcr=s98dcr
763200100308     c                   eval      d98hcr=s98hcr
763300100308     c                   eval      d98tcr=s98tcr
763400160405     c
763500160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
763600160405     c*  perchè il pgm usa il campo verificado se =10 )
763700160405     c                   if        wVP='S'
763800160405     c                   add       24            d98tci
763900160405     c                   add       24            d98tcc
764000160405     c                   add       24            d98tri
764100160405     c                   add       24            d98trc
764200160405     c                   endif
764300100308     c
764400930423     C                   ENDSR
764500941228     C***********************************************************************
764600941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
764700941228     C***********************************************************************
764800941228     C     W98999        BEGSR
764900941228     C     W98           IFLT      -999
765000941228     C                   Z-ADD     -999          D98               3 0
765100941228     C                   ELSE
765200941228     C     W98           IFGT      999
765300941228     C                   Z-ADD     999           D98
765400941228     C                   ELSE
765500941228     C                   Z-ADD     W98           D98
765600941228     C                   END
765700941228     C                   END
765800941228     C                   ENDSR
765900941228     C***********************************************************************
766000941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
766100941228     C***********************************************************************
766200941228     C     W98000        BEGSR
766300941228     C     W98           IFLE      *ZEROS
766400941228     C                   Z-ADD     *ZEROS        D98
766500941228     C                   ELSE
766600941228     C     W98           IFGT      999
766700941228     C                   Z-ADD     999           D98
766800941228     C                   ELSE
766900941228     C                   Z-ADD     W98           D98
767000941228     C                   END
767100941228     C                   END
767200941228     C                   ENDSR
767300930305     C***********************************************************************
767400930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
767500930305     C***********************************************************************
767600930305     C     AZZERA        BEGSR
767700150219     c* data e ora del momento
767800150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
767900150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
768000150219     C                   Z-ADD     DTORA4        WHHODI            4 0
768100930305     C*
768200141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
768300150603     C                   MOVEL     'N'           opendan
768400141107     c                   clear                   or_ddc
768500141108     c                   clear                   sav_12dat         8 0
768600141107     c                   clear                   or_datarif
768700140402     c                   clear                   wpick2            1
768800140402     c                   clear                   Primotipo
768900090928     c                   clear                   Primodev
769000090928     c                   clear                   Primohev
769100070827     c                   clear                   CallAR5
769200071122     c                   clear                   d98cbo
769300100308     c                   clear                   s98dcr
769400100308     c                   clear                   s98hcr
769500100308     c                   clear                   s98tcr
769600141106     c                   clear                   nosomma           1
769700141229     c                   clear                   wtrasm
769800150324     c                   clear                   avvcmc
769900150325     c                   clear                   DSbl4L
770000160401     c                   clear                   dsbl4I
770100150401     c                   clear                   fnlv82ds
770200150331     c                   clear                   DS7R
770300150605     c                   clear                   wDEE              1
770400150629     c                   clear                   wtsogliaE
770500930323     C                   MOVEL     '0'           D98ERR
770600930323     C                   MOVEL     *BLANKS       D98MSG
770700931124     C                   Z-ADD     *ZEROS        D98TPI
770800931124     C                   Z-ADD     *ZEROS        D98TPC
770900930325     C                   Z-ADD     *ZEROS        D98TTI
771000930325     C                   Z-ADD     *ZEROS        D98TTC
771100930325     C                   Z-ADD     *ZEROS        D98TCI
771200930325     C                   Z-ADD     *ZEROS        D98TCC
771300090827     C                   Z-ADD     *ZEROS        w99TCI
771400090827     C                   Z-ADD     *ZEROS        w99TCC
771500930325     C                   Z-ADD     *ZEROS        D98TRI
771600930325     C                   Z-ADD     *ZEROS        D98TRC
771700090921     C                   Z-ADD     *ZEROS        w99TRI
771800090921     C                   Z-ADD     *ZEROS        w99TRC
771900931124     C                   Z-ADD     *ZEROS        D98RPI
772000931124     C                   Z-ADD     *ZEROS        D98RPC
772100930405     C                   Z-ADD     *ZEROS        D98RTI
772200930405     C                   Z-ADD     *ZEROS        D98RTC
772300930405     C                   Z-ADD     *ZEROS        D98RCI
772400930405     C                   Z-ADD     *ZEROS        D98RCC
772500930405     C                   Z-ADD     *ZEROS        D98RRI
772600930405     C                   Z-ADD     *ZEROS        D98RRC
772700931124     C                   Z-ADD     *ZEROS        D98FPI
772800931124     C                   Z-ADD     *ZEROS        D98FPC
772900930325     C                   Z-ADD     *ZEROS        D98FTI
773000930325     C                   Z-ADD     *ZEROS        D98FTC
773100930325     C                   Z-ADD     *ZEROS        D98FCI
773200930325     C                   Z-ADD     *ZEROS        D98FCC
773300930325     C                   Z-ADD     *ZEROS        D98FRI
773400930325     C                   Z-ADD     *ZEROS        D98FRC
773500931124     C                   MOVEL     *BLANKS       D98CPI
773600931124     C                   MOVEL     *BLANKS       D98CPC
773700930325     C                   MOVEL     *BLANKS       D98CTI
773800930325     C                   MOVEL     *BLANKS       D98CTC
773900930325     C                   MOVEL     *BLANKS       D98CCI
774000930325     C                   MOVEL     *BLANKS       D98CCC
774100930325     C                   MOVEL     *BLANKS       D98CRI
774200930325     C                   MOVEL     *BLANKS       D98CRC
774300931124     C                   Z-ADD     *ZEROS        D98NPI
774400931124     C                   Z-ADD     *ZEROS        D98NPC
774500930325     C                   Z-ADD     *ZEROS        D98NTI
774600930325     C                   Z-ADD     *ZEROS        D98NTC
774700930325     C                   Z-ADD     *ZEROS        D98NCI
774800930325     C                   Z-ADD     *ZEROS        D98NCC
774900930325     C                   Z-ADD     *ZEROS        D98NRI
775000930325     C                   Z-ADD     *ZEROS        D98NRC
775100930414     C                   Z-ADD     *ZEROS        D98HHI
775200930414     C                   Z-ADD     *ZEROS        D98HHC
775300930414     C                   Z-ADD     *ZEROS        D98HOI
775400930414     C                   Z-ADD     *ZEROS        D98HOC
775500930414     C                   Z-ADD     *ZEROS        D98HLI
775600930414     C                   Z-ADD     *ZEROS        D98HLC
775700930414     C                   Z-ADD     *ZEROS        D98HGI
775800930414     C                   Z-ADD     *ZEROS        D98HGC
775900931124     C                   Z-ADD     *ZEROS        D98HPI
776000931124     C                   Z-ADD     *ZEROS        D98HPC
776100930414     C                   Z-ADD     *ZEROS        D98HFI
776200930414     C                   Z-ADD     *ZEROS        D98HFC
776300930416     C                   Z-ADD     *ZEROS        D98HEI
776400930416     C                   Z-ADD     *ZEROS        D98HEC
776500930414     C                   Z-ADD     *ZEROS        D98HRI
776600930414     C                   Z-ADD     *ZEROS        D98HRC
776700930511     C                   Z-ADD     *ZEROS        D98HDI
776800930511     C                   Z-ADD     *ZEROS        D98HDC
776900930325     C                   Z-ADD     *ZEROS        D98DCI
777000930325     C                   Z-ADD     *ZEROS        D98HCI
777100930325     C                   Z-ADD     *ZEROS        D98DCE
777200930325     C                   Z-ADD     *ZEROS        D98HCE
777300931125     C                   Z-ADD     *ZEROS        D98OCI
777400931125     C                   Z-ADD     *ZEROS        D98OCC
777500931202     C                   Z-ADD     *ZEROS        D98DEI
777600931202     C                   Z-ADD     *ZEROS        D98OEI
777700931202     C                   Z-ADD     *ZEROS        D98DEE
777800931202     C                   Z-ADD     *ZEROS        D98OEE
777900151019     c                   CLEAR                   D98FLP
778000161107     c                   CLEAR                   D98Gfas
778100150325     C                   clear                   D98dtctd
778200150326     C                   clear                   D98picklim
778300150325     C                   clear                   D98spcdee
778400150331     C                   clear                   fddata
778500170208     C                   clear                   D98dam_rtc
778600140825     c
778700141017     c                   clear                   d98ddc
778800141017     c                   clear                   d98cons
778900160506     c                   clear                   d98disf
779000141017     c                   clear                   wd98ndc
779100140910     c                   clear                   wd98fbc
779200150119     c                   clear                   wd98acc
779300140825     c                   clear                   wtrulor
779400160401     c                   clear                   wVP               1
779500170208     c                   clear                   wsolodam
779600150219     c
779700150219     c                   clear                   IMPdata
779800150219     c                   clear                   IMPfgs
779900150219     c                   clear                   DISdata
780000150219     c                   clear                   DISfgs
780100150219     c                   clear                   DISora
780200150219     c                   clear                   DISnpg
780300150219     c                   clear                   IDnpg
780400150219     c                   clear                   IDdata
780500150219     c                   clear                   IDora
780600150219     c                   clear                   IDfgs
780700931125     C* CAMPI VARI
780800931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
780900140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
781000140825      *   DALLA LETTURA DI QUESTI FILE                               *
781100931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
781200931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
781300931125     C     D98NSP        IFNE      *ZEROS
781400930525     C                   MOVEL     *BLANKS       D98TSP
781500941228     C                   MOVEL     *BLANKS       D98CAD
781600970728     C                   MOVEL     *BLANKS       D98LOD
781700930525     C                   Z-ADD     *ZEROS        D98TFP
781800930525     C                   Z-ADD     *ZEROS        D98TFA
781900930525     C                   Z-ADD     *ZEROS        D98LNA
782000930525     C                   Z-ADD     *ZEROS        D98DSP
782100941228     C                   Z-ADD     *ZEROS        D98MGS
782200930525     C                   Z-ADD     *ZEROS        D98DTI
782300930525     C                   Z-ADD     *ZEROS        D98HTI
782400930525     C                   Z-ADD     *ZEROS        D98DCM
782500930525     C                   Z-ADD     *ZEROS        D98HMC
782600930525     C                   MOVEL     *BLANKS       D98GC1
782700930525     C                   MOVEL     *BLANKS       D98GC2
782800010820     C                   Z-ADD     *ZEROS        D98CAS
782900010821     C                   MOVEL     *BLANKS       D98VCA
783000930525     C                   MOVEL     *BLANKS       D98TIC
783100930525     C                   Z-ADD     *ZEROS        D98DCR
783200930525     C                   Z-ADD     *ZEROS        D98HCR
783300930525     C                   MOVEL     *BLANKS       D98TCR
783400050914     C                   MOVEL     *BLANKS       Consideraora      1
783500930525     C                   MOVEL     *BLANKS       D98CMC
783600930525     C                   Z-ADD     *ZEROS        D98DAG
783700161215     C                   Z-ADD     *ZEROS        D98DCc
783800161215     C                   Z-ADD     *ZEROS        D98DAc
783900161215     C                   Z-ADD     *ZEROS        D98DCG
784000930525     C                   MOVEL     *BLANKS       D98FFD
784100930525     C                   Z-ADD     *ZEROS        D98NCD
784200930525     C                   MOVEL     *BLANKS       D98FNS
784300931124     C                   Z-ADD     *ZEROS        D98DAM
784400170105     C                   Z-ADD     *ZEROS        D98Dfv
784500940628     C                   MOVEL     *BLANKS       D98FIN
784600940628     C                   Z-ADD     *ZEROS        D98KSC
784700940628     C                   MOVEL     *BLANKS       D98TBL
784800931125     C                   END
784900960229     C     D98TLA        IFEQ      'T'
785000960229     C                   Z-ADD     *ZEROS        D98DTI
785100960229     C                   Z-ADD     *ZEROS        D98HTI
785200960229     C                   ENDIF
785300931125     C* CAMPI WORK
785400930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
785500930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
785600150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
785700930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
785800941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
785900960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
786000930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
786100930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
786200940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
786300930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
786400150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
786500931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
786600931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
786700150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
786800930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
786900930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
787000930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
787100930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
787200930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
787300940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
787400941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
787500930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
787600050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
787700050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
787800140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
787900930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
788000930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
788100930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
788200930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
788300930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
788400930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
788500930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
788600930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
788700930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
788800930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
788900930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
789000930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
789100930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
789200930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
789300140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
789400931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
789500060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
789600960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
789700931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
789800140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
789900140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
790000150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
790100140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
790200941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
790300970304     C******************** Z-ADD*ZEROS    W98DBR  80
790400970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
790500150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
790600050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
790700930414     C*
790800941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
790900941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
791000930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
791100930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
791200930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
791300930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
791400930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
791500930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
791600941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
791700941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
791800930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
791900930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
792000930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
792100930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
792200930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
792300930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
792400941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
792500941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
792600930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
792700930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
792800930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
792900930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
793000930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
793100930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
793200930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
793300930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
793400060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
793500060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
793600060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
793700060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
793800930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
793900930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
794000930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
794100930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
794200930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
794300930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
794400930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
794500930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
794600930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
794700930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
794800930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
794900930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
795000930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
795100930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
795200941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
795300941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
795400930423     C*
795500930511     C                   Z-ADD     *ZEROS        W98HF             5 0
795600930423     C                   Z-ADD     *ZEROS        W98HH             5 0
795700060418     C                   Z-ADD     *ZEROS        W98EG             5 0
795800060420     C                   Z-ADD     *ZEROS        W98EA             5 0
795900930423     C                   Z-ADD     *ZEROS        W98HG             5 0
796000930423     C                   Z-ADD     *ZEROS        W98HL             5 0
796100930423     C                   Z-ADD     *ZEROS        W98HO             5 0
796200930511     C                   Z-ADD     *ZEROS        W98HD             5 0
796300930611     C*
796400930611     C                   Z-ADD     *ZEROS        WORELA            3 0
796500960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
796600930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
796700930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
796800931207     C                   Z-ADD     *ZEROS        WORECR            3 0
796900090902     c
797000090902     c                   clear                   wmancaTRAZ
797100150113     c                   clear                   wcaldpcee         1
797200150220     c                   clear                   evemic            2
797300930305     C*
797400060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
797500060406     C                   clear                   LAF                            *LASC.AVVISO FINE
797600091014     C                   clear                   LAp                            *posizione l.avviso
797700060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
797800060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
797900060406c    c                   clear                   LCon
798000060406     C                   clear                   EVD                            *DATA EVENTO
798100060406     C                   clear                   EVO                            *ORA EVENTO
798200060406     C                   clear                   EVC                            *CAUSALE EVENTO
798300131218     C                   clear                   EVT
798400060406     C                   clear                   pVD                            *DATA EVENTO
798500060406     C                   clear                   pVO                            *ORA EVENTO
798600060406     C                   clear                   pVC                            *CAUSALE EVENTO
798700060406     C                   clear                   pVF                            *CAUSALE EVENTO
798800060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
798900060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
799000060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
799100060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
799200060406     C                   clear                   COV                            *DATE FINE L.A. E GI
799300060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
799400060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
799500091014     C                   clear                   GIp                            *posizione vento gia
799600060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
799700060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
799800060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
799900931213     C*
800000060406     C                   clear                   TUC                            *TURNI CHIUSURA
800100931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
800200931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
800300060406     c
800400060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
800500060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
800600060406     c                   clear                   pgii
800700060406     c                   clear                   pgif
800800060406     c                   clear                   pgci
800900060406     c                   clear                   pgcf
801000060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
801100060420     c                   clear                   paii
801200060420     c                   clear                   paif
801300930420     C*
801400930305     C                   ENDSR
801500930512     C***********************************************************************
801600930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
801700930512     C***********************************************************************
801800930512     C     FORCON        BEGSR
801900930512     C*
802000931124     C                   Z-ADD     *ZEROS        D98RPI
802100931124     C                   Z-ADD     *ZEROS        D98RPC
802200930512     C                   Z-ADD     *ZEROS        D98RTI
802300930512     C                   Z-ADD     *ZEROS        D98RTC
802400930512     C                   Z-ADD     *ZEROS        D98RCI
802500930512     C                   Z-ADD     *ZEROS        D98RCC
802600930512     C                   Z-ADD     *ZEROS        D98RRI
802700930512     C                   Z-ADD     *ZEROS        D98RRC
802800931124     C                   Z-ADD     *ZEROS        D98FPI
802900931124     C                   Z-ADD     *ZEROS        D98FPC
803000930512     C                   Z-ADD     *ZEROS        D98FTI
803100930512     C                   Z-ADD     *ZEROS        D98FTC
803200930512     C                   Z-ADD     *ZEROS        D98FCI
803300930512     C                   Z-ADD     *ZEROS        D98FCC
803400930512     C                   Z-ADD     *ZEROS        D98FRI
803500930512     C                   Z-ADD     *ZEROS        D98FRC
803600931124     C                   MOVEL     'S'           D98CPI
803700931124     C                   MOVEL     'S'           D98CPC
803800930512     C                   MOVEL     'S'           D98CTI
803900930512     C                   MOVEL     'S'           D98CTC
804000930512     C                   MOVEL     'S'           D98CCI
804100930512     C                   MOVEL     'S'           D98CCC
804200930512     C                   MOVEL     'S'           D98CRI
804300930512     C                   MOVEL     'S'           D98CRC
804400931124     C                   Z-ADD     *ZEROS        D98NPI
804500931124     C                   Z-ADD     *ZEROS        D98NPC
804600930512     C                   Z-ADD     *ZEROS        D98NTI
804700930512     C                   Z-ADD     *ZEROS        D98NTC
804800930512     C                   Z-ADD     *ZEROS        D98NCI
804900930512     C                   Z-ADD     *ZEROS        D98NCC
805000930512     C                   Z-ADD     *ZEROS        D98NRI
805100930512     C                   Z-ADD     *ZEROS        D98NRC
805200930512     C                   Z-ADD     *ZEROS        D98HHI
805300930512     C                   Z-ADD     *ZEROS        D98HHC
805400930512     C                   Z-ADD     *ZEROS        D98HOI
805500930512     C                   Z-ADD     *ZEROS        D98HOC
805600930512     C                   Z-ADD     *ZEROS        D98HLI
805700930512     C                   Z-ADD     *ZEROS        D98HLC
805800930512     C                   Z-ADD     *ZEROS        D98HGI
805900930512     C                   Z-ADD     *ZEROS        D98HGC
806000931124     C                   Z-ADD     *ZEROS        D98HPI
806100931124     C                   Z-ADD     *ZEROS        D98HPC
806200930512     C                   Z-ADD     *ZEROS        D98HFI
806300930512     C                   Z-ADD     *ZEROS        D98HFC
806400930512     C                   Z-ADD     *ZEROS        D98HEI
806500930512     C                   Z-ADD     *ZEROS        D98HEC
806600930512     C                   Z-ADD     *ZEROS        D98HRI
806700930512     C                   Z-ADD     *ZEROS        D98HRC
806800930512     C                   Z-ADD     *ZEROS        D98HDI
806900930512     C                   Z-ADD     *ZEROS        D98HDC
807000930520     C*
807100930512     C                   ENDSR
807200930305     C********************************************************************
807300971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
807400950406031059090120151181212243273304334365
807500950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
807600950406312831303130313130313031
807700930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
807800971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
807900971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
808000971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
808100971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
808200971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
808300971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
808400971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
808500971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
808600971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
808700971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
808800971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
808900971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
809000971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
809100971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
809200971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
809300971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
809400971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
809500971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
809600971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
809700971127TNSD99R- Bolla inesistente.                                               BOIN20
809800971127TNSD99R-     Cap inesistente.                                             CAPI21
809900971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
810000971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
810100971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
810200971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
810300971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
810400971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
810500971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
810600971127TNSD99R- Contrassegno non trovato.                                        CONT29
810700971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
810800990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
810900990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
811000060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
811100060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
811200971127TNSD99R- 35                                                                   35
811300971127TNSD99R- 36                                                                   36
811400971127TNSD99R- 37                                                                   37
811500971127TNSD99R- 38                                                                   38
811600971127TNSD99R- 39                                                                   39
811700971127TNSD99R- 40                                                                   40
