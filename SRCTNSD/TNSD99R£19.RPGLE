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
018300170821
018400170822     D fnlrj6ds      E DS                  inz
018500930323     D*****************************************************************
018600930323     D* DS ESTERNA IN INPUT AL PGM
018700970731     D DS99          E DS                  EXTNAME(TNSD99DS)
018800930323     D*****************************************************************
018900930323     D* DS ESTERNA TABELLA 1X TEMPI FISSI DI DELIVERY
019000930326     D DS1X          E DS
019100930405     D*****************************************************************
019200971113     D* DS ESTERNA TABELLA 1Y SIMBOLI CALENDARIO
019300930405     D DS1Y          E DS
019400941227     D*****************************************************************
019500941227     D* DS ESTERNA TABELLA 3A (CODICI BOLLA)
019600941227     D DS3A          E DS
019700930421     D*****************************************************************
019800060406     D* DS ESTERNA TABELLA 2A CAUSALI EVENTI BOLLE
019900060406     D DS2A          E DS                  prefix(W:1)
020000020226     D*****************************************************************
020100020226     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
020200020226     D OG143         E DS
020300020702     D*****************************************************************
020400020702     D* DS PER LETTURA TABELLA "NTW"
020500020702     D DNTW          E DS
020600150326     D* DS PER LETTURA TABELLA "VPO" flag operativi
020700150326     D Dvpo          E DS
020800031201     D*****************************************************************
020900150331     D* DS ESTERNA TABELLA 7Q/7W/7R
021000031201     D DS7Q          E DS
021100031201     D DS7W          E DS
021200150331     D DS7R          E DS
021300070514     D*****************************************************************
021400140825     D* DS per reperimento orari di consegna
021500140825     D TRULORSDS     E DS
021600140825     D TRULOR2DS     E DS
021700150331     D TRULOR3DS     E DS
021800140825     D*****************************************************************
021900150219     D FNLV53DS      E DS
022000150219     D FNLV55DS      E DS
022100150401     D*****************************************************************
022200150401     D fnlv82ds      E DS
022300150219     D*****************************************************************
022400060406     D usa2a           DS
022500060406     D  §2aftc                 1      1
022600060406     D  §2agla                 2      2  0
022700060406     D  §2aFTG                 3      3
022800060406     D usa2Z           DS
022900060406     D  §2Zcep                 1      1
023000060406     D  §2Znep                 2      3
023100060406     D  §2Zdep                 4      4
023200060406     D  §2Zfep                 5      5
023300930517     D***************************************************************
023400971113     D* DS PER SCOMPORRE CHIAVE CALENDARIO
023500930517     D                 DS
023600941227     D  DCATFP                 1      3
023700941227     D  DCATFA                 4      6
023800941227     D  DCAANN                 7     10
023900941227     D  DCAMES                11     12
024000941227     D  DCALEN                 1     12
024100950406     D***************************************************************
024200950406     D* DS PER SCOMPORRE WDAT IN AAAA/MM/GG
024300950406     D                 DS
024400950406     D  WDAT                   1      8  0
024500950406     D  WAA                    1      4  0
024600950406     D  WMM                    5      6  0
024700950406     D  WGG                    7      8  0
024800970304     D***************************************************************
024900970304     D* DS PER CONFRONTI TRA DATE
025000970304     D                 DS
025100970304     D  WDO1                   1     12  0
025200970304     D  WD1                    1      8  0
025300970304     D  WO1                    9     12  0
025400970304     D                 DS
025500970304     D  WDO2                   1     12  0
025600970304     D  WD2                    1      8  0
025700970304     D  WO2                    9     12  0
025800150330     D***************************************************************
025900150330     D                 DS
026000150330     D  who                    1      2  0
026100150330     D  wmn                    3      4  0
026200150330     D  WORA                   1      4  0
026300941227     D***************************************************************
026400941227     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
026500941227     D DS§DAT          DS
026600941227     D  DSSEC                  1      2  0
026700941227     D  DSANN                  3      4  0
026800941227     D  DSMES                  5      6  0
026900941227     D  DSGIO                  7      8  0
027000941227     D  DSAAS                  1      4  0
027100941228     D  DSMGS                  5      8  0
027200941227     D  DSDAT6                 3      8  0
027300941227     D  DSDAT8                 1      8  0
027400941227     D DS§TIM          DS
027500941227     D  DTORA                  1      2  0
027600941227     D  DTMIN                  3      4  0
027700941227     D  DTSCN                  5      6  0
027800941227     D  DTGIO                  7      8  0
027900941227     D  DTMES                  9     10  0
028000941227     D  DTSEC                 11     12  0
028100941227     D  DTANN                 13     14  0
028200941227     D  DTINV                  7     14  0
028300941227     D  DTORA6                 1      6  0
028400941227     D  DTORA4                 1      4  0
028500941227     D  DTTIME                 1     14  0
028600010821     D***************************************************************
028700050523     D kpjba         E DS
028800010821     D*  CAMBIO DIVISA
028900010821     D YEURDS        E DS                  EXTNAME(YEURCODS)
029000010821     D***************************************************************
029100010821     D*  REPERIMENTO DIVISE DI CONTO
029200010821     D DGED          E DS
029300070827     D*------------------
029400070827     D DAR5GEN       E DS
029500011211     D*------------------
029600011211     D* DS REPERIMENTO IMPORTO MASSIMO CONTRASSEGNO
029700011211     D*------------------
029800011211     D TRUL21DS      E DS
029900050523     d
030000050523     d* DS x controllo riempimento schiera
030100050523     D TRUL0sDS      E DS
030200040809     D*------------------
030300040809     D* ds per il controllo della presenza di CA per la spedizione richiesta
030400040809     D*------------------
030500040809     d FIDN12DS      e ds
030600040809     d  skKey                 26    305    dim(20)
030700040809     d  skAnn                306    325    dim(20)
030800040809     d  skDca                326    485  0 dim(20)
030900040809     d  skFca                486    545  0 dim(20)
031000040809     d  skDch                546    705  0 dim(20)
031100040809     d  skCch                706    745    dim(20)
031200040809     d*
031300040809     d dsKey           ds
031400040809     d  dsaac                         4  0
031500040809     d  dsfil                         3  0
031600040809     d  dsnca                         7  0
031700041028     d wesarb          s              1
031800141216     d wtrasm          s              1
031900041116     d wapri           s              1    inz(' ')
032000050908     d Usacons         s              1    inz(' ')
032100050405     D*------------------
032200050405     D* VARIABILI D WRK RIFERITE AL DATA BASE
032300050405     D*------------------
032400050405     D KWGCPAAS        s                   like(GCPAAS)
032500050405     D KWGCPLNP        s                   like(GCPLNP)
032600050405     D KWGCPNRS        s                   like(GCPNRS)
032700050405     D KWGCPNSP        s                   like(GCPNSP)
032800050405     D KWGCPFRG        s                   like(GCPFRG)
032900050901     d
033000050901     D salvaoreHMC     s                   like(wr3hho)
033100050901     D salvaoreHCR     s                   like(wr3hho)
033200050907     D oreILoDOPO      s                   like(worecr)
033300060526     D wpodata         s                   like(w12dat)
033400070514     D wdata8          s                   like(w12dat)
033500060406     D w003a           s              3
033600060406     D w11poi          s              9  0
033700150217     D picklim         s              4  0
033800060420     D wgiroepa        s              1
033900140911     D wtuctut         s              1    inz
034000060420     D wfine           s              1
034100070827     D CallAR5         s              1    inz
034200140825     D Wtrulor         s              1    inz
034300070827     d kAr5Trd         s                   Like(Ar5Trd) inz('GEN')
034400150325     d kAr4Trc         s                   Like(Ar4Trc) inz('L')
034500071122     d d98cbo          s                   Like(arbcbo)
034600090827     d w99tci          s                   Like(d98tci)
034700090827     d w99tcc          s                   Like(d98tcc)
034800090921     d w99tri          s                   Like(d98tri)
034900090921     d w99trc          s                   Like(d98trc)
035000090928     d Primotipo       s                   Like(§2aftc)
035100090928     d Primodev        s                   Like(evbdev)
035200090928     d Primohev        s                   Like(evbhev)
035300091014     d SalvaEveL       s              3  0
035400091014     d SalvaEveG       s              3  0
035500140911     d EX              s              3  0
035600150325     d II              s              3  0
035700150630     d wdiff           s              9  0
035800100308     d s98dcr          s                   Like(d98dcr)
035900100308     d s98hcr          s                   Like(d98hcr)
036000100308     d s98tcr          s                   Like(d98tcr)
036100140825     d wd98ndc         s                   Like(arbndc)
036200140911     d wd98fbc         s                   Like(arbfbc)
036300150119     d wd98acc         s                   Like(arbacc)
036400150219     d
036500150219     d IMPdata         s                   like(d53dfv)
036600150219     d IMPfgs          s                   like(brvfgs)
036700150219     d DISfgs          s                   like(brvfgs)
036800150219     d DISdata         s                   like(d53dfv)
036900150219     d DISnpg          s                   like(brvnpg)
037000150219     d DISora          s                   like(brvhcs)
037100150219     d IDdata          s                   like(d53dfv)
037200150219     d IDora           s                   like(brvhcs)
037300150219     d IDfgs           s                   like(brvfgs)
037400150219     d IDnpg           s              1
037500150325     D kpdpdr          S                   LIKE(APDpdr)
037600150325     D kpdtip          S                   LIKE(APDtip)
037700170208     d wsolodam        s              1
037800140825     d
037900140825     D wrkoggiiso      s               d
038000930304     C*****************************************************************
038100930323     C*/ INIZIO MAIN DEL PGM
038200170208     C* ESEGUE ROUTINE wdaodiLE
038300930304     C                   EXSR      RUTINZ
038400930528     C*
038500930528     C     D98ERR        IFEQ      '0'                                          --- 1 -->
038600930602     C*
038700930602     C* SE LANCIATO CON TIPO LANCIO='C' (CHIUDI FILE) VADO A FINE PGM
038800930602     C     D98TLA        IFNE      'C'                                          --- 2 -->
038900960729     C*
039000960729     C* CONTROLLO SE MI HANNO LANCIATO PER DETERMINARE LA CONSEGNABILITA'
039100960729     C                   MOVEL     'N'           FLCONS            1            *FLAG ESISTE CONSEGN
039200960729     C     D98DIC        IFGT      *ZEROS
039300960729     C     D98DFC        ORGT      *ZEROS
039400960729     C                   MOVEL     'S'           FLCONS                         *SI ESISTE CONSEGNAB
039500960729     C                   ENDIF
039600930323     C*
039700930511     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
039800931125     C* SE NON E' STATA IMPOSTATA DS DAL PGM CHIAMANTE (NSP=ZERO)
039900930420     C     D98ERR        IFEQ      '0'
040000010821     C     D98NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
040100930415     C     D98TBO        IFEQ      'A'
040200941227     C                   EXSR      LETARB                                       *LEGGE BOLLA ARRIVO
040300930323     C                   ELSE
040400941227     C                   EXSR      LETBLP                                       *LEGGE BOLLA PART.
040500930323     C                   END
040600140402     c
040700150401     c* Leggo fiar5 recod GEN  e FIAR4
040800140402     c                   exsr      leggiar5
040900140402     c
041000931206     C                   ELSE
041100931206     C* DS IMPOSTATA DAL PGM CHIAMANTE IMPOSTO CAMPI LAVORO
041200931206     C                   Z-ADD     D98DCG        W98DCG
041300990308     C                   MOVEL     *BLANKS       WRBL              1            *RAGGRUPP.BOLLE
041400990308     C                   MOVEL     *BLANKS       WFBR              1            *BOLLA DI RESO
041500031201     C                   MOVEL     *BLANKS       WGGA              2            *PARTICOLARIT' GIAC.
041600140822     C                   Z-ADD     d98DCR        s98DCR
041700140822     C                   Z-ADD     d98HCR        s98HCR
041800140822     C                   MOVEL     d98TCR        s98TCR
041900141104     c                   eval      gc1part=d98gc1
042000141104     c                   eval      gc2part=d98gc2
042100150303     c* imposto flag trasmissione bolla = 'N'
042200150303     c                   eval      wtrasm='N'
042300931125     C                   END
042400930420     C                   END
042500930528     C*
042600010821     C     D98ERR        IFEQ      '0'                                          --- 3 -->
042700160420
042800160420     c* se bolla in partenza, devo ricostruire il record "L"
042900160420    2c                   if        wtrasm='N'
043000160420    3c                   if        d98tc1 = 'A' or d98tc1 = 'S' or d98tc2 = 'A'
043100160420     c                             or d98tc2 = 'S'
043200160420     c                   eval      §b4lbas = 'S'
043300160420      * se consegna tassativa sblocco
043400160420    4c                   if        d98dcr <> *zeros and d98tcr = *blanks
043500160420     c                   clear                   §b4lbas
043600160420    4c                   endif
043700160420    3c                   endif
043800160420
043900160420    3c                   if        d98ffd<>' '
044000170206
044100160420     c                   eval      §b4ltfd = 'S'
044200170206     c* se Fermo deposito tolgo il contatto per l'appuntamento
044300170206    4c                   if        §b4lbas = 'S' and
044400170206     c                             (d98tc1 = 'A' or d98tc2= 'A')
044500170206     c                   clear                   §b4lbas
044600170206    4c                   endif
044700160420    3c                   endif
044800160420
044900160420    2c                   endif
045000160420
045100050908     c* Se esiste la data consegnabilità, faccio finta che la bolla
045200050908     c*  non si consegnata, azzerando proprio la dta di consegna
045300050908     c*  perchè può trarre in inganno
045400050908     c                   if        d98dfc>0 and d98dcm>0
045500050908     c                   clear                   d98dcm
045600050908     c                   clear                   d98hmc
045700050908     c                   clear                   d98cca
045800050908     c                   endif
045900090902     c*
046000090902     c* Se non c'e' nè data consegna, ne data arrivo trazione
046100090902     c*  faccio finta che data arrivo trazione= udate utime ai fini dei calcoli
046200090902     c* in fondo al pgm dovrò azzerare i tempi di consegna interni e cliente
046300090902     c* perchè non calcolabili
046400090902     c                   clear                   wmancaTRAZ        1
046500090902     c
046600010821     C*
046700010821     C* EFFETTUO SUBITO CONSIDERAZIONI SU DIVISE (X QUESTIONE EURO)
046800010821     C                   EXSR      CHKVCA
046900141204     c
047000141204     c* Controllo se ha pratica di DANNO aperta
047100141204     c                   EXSR      CHKDAN
047200010108     C*
047300010108     C* SE BOLLA POSTA, UTILIZZA LA DATA BORDERO' ANZICHE' LA DATA SPEDIZIONE
047400091009     C**   D98TSP        IFEQ      'P'                                          *BOLLA POSTA
047500091009     C**   D98DBR        IFGT      *ZEROS                                       *DATA BORDERO' PRESE
047600091009     C**                 Z-ADD     D98DBR        D98DSP                         *DATA BORDERO' --> D
047700091009     C**                 ENDIF
047800091009     C**                 ENDIF
047900930304     C*
048000950110     C* RECUPERO I TEMPI TEORICI DI PARTENZA, TRAZIONE E DI CONSEGNA
048100930420     C     D98ERR        IFEQ      '0'
048200930323     C                   EXSR      RTTEOR
048300930420     C                   END
048400930528     C*
048500930602     C     D98ERR        IFEQ      '0'                                          --- 4 -->
048600970304     C*
048700970304     C* SE RICHIESTO LANCIO CON DATA RIFERIMENTO CONTROLLO LE DATE DA AZZERARE
048800970304     C     D98ERR        IFEQ      '0'
048900970304     C                   EXSR      DATRIF
049000970304     C                   END
049100940901     C*
049200940901     C* TRASF. DATA ARRIVO TRAZIONE E RICONSIDERO TEMPO CONSEGNA SE RITARDO
049300940901     C     D98ERR        IFEQ      '0'
049400940901     C                   EXSR      TDTIPO
049500940901     C                   END
049600930420     C*
049700930511     C* CARICO IL FILE DEGLI EVENTI BOLLA
049800930420     C     D98ERR        IFEQ      '0'
049900041116     c     d98nsp        andgt     0
050000930420     C                   EXSR      CAREVB
050100930420     C                   END
050200931213     C*
050300931213     C* CARICO LA SCHIERA DEI TURNI DI CHIUSURA
050400931213     C     D98ERR        IFEQ      '0'
050500931213     C                   EXSR      CARTUC
050600931213     C                   END
050700930528     C*
050800930602     C     D98ERR        IFEQ      '0'                                          --- 5 -->
050900930405     C*
051000940901     C* TRASFORMO LE DATE (SPED,CONS,ECC.) IN PROGRESSIVI ORE
051100930420     C     D98ERR        IFEQ      '0'
051200930405     C                   EXSR      TDATPO
051300930420     C                   END
051400930528     C*
051500930602     C     D98ERR        IFEQ      '0'                                          --- 6 -->
051600950110     C*
051700950110     C* CALCOLA LE DATE DI PREVISTA PARTENZA DELL'ULTIMO COLLO
051800950110     C     D98ERR        IFEQ      '0'
051900950110     C                   EXSR      CALDPP
052000950110     C                   END
052100930405     C*
052200930511     C* CALCOLA LE DATE DI PREVISTO ARRIVO DELLA TRAZIONE
052300930420     C     D98ERR        IFEQ      '0'
052400930511     C                   EXSR      CALDPT
052500090902     c*
052600090902     c* Ricalcolo le date facendo finta che la trazione stia arrivando
052700090903     c*  in questo momento se non presente
052800090902     c                   if        ricalcoloTRAZ='S'
052900090902     c                   eval      d98dti=WDAODI
053000090902     c                   eval      d98hti=WHHODI
053100090902     c                   eval      wmancaTRAZ='S'
053200090902     c
053300090902     C                   EXSR      TDTIPO
053400090902     C                   EXSR      CALDPP
053500090902     C                   EXSR      CALDPT
053600090902     c                   endif
053700090902     c
053800930420     C                   END
053900960229     C*
054000091009     d* D98TLA = 'T' --> in output solo i tempi di trazione
054100960229     C     D98TLA        IFNE      'T'                                          --- 6A ->
054200060407     C*
054300931206     C* CALCOLA IL RANGE PER IL CONTRASSEGNO
054400931206     C     D98ERR        IFEQ      '0'
054500931206     C                   EXSR      CALASS
054600931206     C                   END
054700931206     C*
054800931206     C* CALCOLA IL RANGE PER IL FERMO DEPOSITO
054900931206     C     D98ERR        IFEQ      '0'
055000931206     C                   EXSR      CALFED
055100931206     C                   END
055200931206     C*
055300931206     C* CALCOLA LE DATE TEORICHE DI CONSEGNA
055400931206     C     D98ERR        IFEQ      '0'
055500931206     C                   EXSR      CALDTE
055600931206     C                   END
055700930511     C*
055800930511     C* CALCOLA IL RANGE PER DATA CONSEGNA RICHIESTA
055900930511     C     D98ERR        IFEQ      '0'
056000930511     C                   EXSR      CALDCR
056100930511     C                   END
056200930511     C*
056300930511     C* CALCOLA I RANGE PER LASCIATO AVVISO
056400930511     C     D98ERR        IFEQ      '0'
056500930511     C                   EXSR      CALLAV
056600930511     C                   END
056700930511     C*
056800930511     C* CALCOLA IL RANGE PER LA GIACENZA
056900930511     C     D98ERR        IFEQ      '0'
057000930511     C                   EXSR      CALGIA
057100930511     C                   END
057200060420     C* CALCOLA IL RANGE PER Eventi Particolari che Allungano
057300060420     C     D98ERR        IFEQ      '0'
057400060420     C                   EXSR      CALcepa
057500060420     C                   END
057600930511     C*
057700930511     C* CALCOLA LE DATE DI PREVISTA CONSEGNA
057800930511     C     D98ERR        IFEQ      '0'
057900930511     C                   EXSR      CALDPC
058000930511     C                   END
058100960229     C*
058200960229     C                   ENDIF                                                  <-- 6A --
058300960229     C*
058400930602     C* TRASFORMO I PROGRESSIVI ORE IN DATE
058500930602     C                   EXSR      TPODAT
058600930602     C*
058700930914     C* CONTROLLO SE BOLLA RIENTRA IN UNO DEI CASI IN CUI NON RIESCO A CALCOLA
058800930602     C* RE I RITARDI PER CUI FORZO CONFORMITA' A SI
058900930602     C     D98ERR        IFEQ      '0'
059000960229     C     D98TLA        ANDNE     'T'
059100930602     C                   EXSR      CHKFOR
059200930602     C                   END
059300930602     C*
059400930602     C     D98ERR        IFEQ      '0'                                          --- 7 -->
059500960229     C     D98TLA        ANDNE     'T'
059600941228     C*
059700941228     C* CALCOLA LE ORE REALI DI PARTENZA
059800941228     C     D98ERR        IFEQ      '0'
059900941228     C                   EXSR      CALORP
060000941228     C                   END
060100930405     C*
060200930511     C* CALCOLA LE ORE REALI DI TRAZIONE
060300930420     C     D98ERR        IFEQ      '0'
060400930407     C                   EXSR      CALORT
060500930420     C                   END
060600930405     C*
060700930511     C* CALCOLA LE ORE REALI DI CONSEGNA
060800930420     C     D98ERR        IFEQ      '0'
060900930416     C                   EXSR      CALORC
061000930420     C                   END
061100930416     C*
061200930511     C* CALCOLA LE ORE REALI DI RESA
061300930420     C     D98ERR        IFEQ      '0'
061400930416     C                   EXSR      CALORR
061500930420     C                   END
061600930420     C*
061700930511     C* CALCOLA LE ORE DI DIFFERENZA REALI/TEORICHE
061800930420     C     D98ERR        IFEQ      '0'
061900930420     C                   EXSR      CALDIF
062000930420     C                   END
062100930420     C*
062200930511     C* CALCOLA LE CONFORMITA'
062300930420     C     D98ERR        IFEQ      '0'
062400930420     C                   EXSR      CALCNF
062500930420     C                   END
062600930913     C*
062700930913     C* CALCOLA LE ORE DI CONSEGNABILITA'
062800930913     C     D98ERR        IFEQ      '0'
062900950407     C     FLCONS        ANDEQ     'S'
063000930913     C                   EXSR      CALCNS
063100930913     C                   END
063200930423     C*
063300930511     C* PORTO I VALORI DAI CAMPI DI LAVORO AI CAMPI IN OUTPUT DELLA DS
063400930423     C     D98ERR        IFEQ      '0'
063500930423     C                   EXSR      W98D98
063600930423     C                   END
063700930528     C*
063800930602     C                   END                                                    <-- 7 ---
063900930602     C                   END                                                    <-- 6 ---
064000930528     C                   END                                                    <-- 5 ---
064100930528     C                   END                                                    <-- 4 ---
064200930528     C                   END                                                    <-- 3 ---
064300930528     C                   END                                                    <-- 2 ---
064400930528     C                   END                                                    <-- 1 ---
064500930323     C*
064600930324     C* SE PGM LANCIATO CON 'C' (LANCI MULTIPLI) CHIUDE CON LR (CHIUDE FILE)
064700930527     C* E LANCIA TUTTI I PGM CHIAMATI PER CHIUDERE I FILE DA LORO APERTI
064800930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
064900930324     C     D98TLA        IFEQ      'C'
065000970714     C                   CLEAR                   SI95
065100970701     C                   MOVEL     'C'           I95TLA
065200020226     C                   CALL      'TISI95R'
065300970701     C                   PARM                    SI95
065400040809     c*
065500040809     c                   clear                   fidn12ds
065600040809     c                   eval      i12tla = 'C'
065700040809     c                   call      'FIDN12R'
065800040809     c                   parm                    fidn12ds
065900050523     c**
066000050523     c* controllo riempimento skiera CAK solo alla fine
066100050523     c**                 clear                   trul0sds
066200050523     c**                 eval      i0sski='CAK/CAM/CAP'
066300050523     c**                 eval      i0sele=%elem(CAK)
066400050523     c**                 if        windca>i0sele
066500050523     c**                 eval      i0spie=i0sele
066600050523     c**                 else
066700050523     c**                 eval      i0spie=windca
066800050523     c**                 endif
066900050523     c**                 eval      i0sfile='AZCLN00F  '
067000050523     c**                 eval      i0scod='     '
067100050523     c**                 eval      i0spgm='TNSD99R'
067200050523     c**                 movel     trul0sds      kpjbu
067300050523     c**                 call      'TRUL0SR'
067400050523     c**                 parm                    kpjba
067500050523     c
067600930323     C                   SETON                                        LR
067700930324     C                   ELSE
067800930324     C                   SETON                                        RT
067900930323     C                   END
068000941227     C********************************************************************
068100941227     C*   LETARB - LEGGE IL FILE FNARB (BOLLE ARRIVI)
068200941227     C********************************************************************
068300941227     C     LETARB        BEGSR
068400941227     C*
068500140820     C* LEGGO IL FILE FNARB01L
068600941227     C                   Z-ADD     D98AAS        ARBAAS
068700941227     C                   Z-ADD     D98LNP        ARBLNP
068800941227     C                   Z-ADD     D98NRS        ARBNRS
068900941227     C                   Z-ADD     D98NSP        ARBNSP
069000941228     C     KEYARB        CHAIN     FNARB01L                           99
069100941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
069200941227     C                   MOVEL     '1'           D98ERR
069300941227     C                   MOVEL     MSG(20)       D98MSG                         *BOLLA INESISTENTE
069400941227     C                   END                                                    <-- 1 ---
069500941228     C     D98ERR        IFEQ      '0'                                          --- 1 --> *TROVATA
069600941227     C                   MOVEL     ARBTSP        D98TSP
069700140820     c* Se forzato da Ds lo imposto al posto di quello della bolla
069800130529     c                   if        i98tsp_for<>' '
069900130529     C                   MOVEL     i98tsp_for    D98TSP
070000130529     c                   endif
070100941228     C                   MOVEL     ARBNZD        D98NZD
070200941227     C                   MOVEL     ARBCAD        D98CAD
070300970701     C                   MOVEL     ARBLOD        D98LOD
070400941227     C                   Z-ADD     ARBTFP        D98TFP
070500941227     C                   Z-ADD     ARBTFA        D98TFA
070600941227     C                   Z-ADD     ARBLNA        D98LNA
070700941227     C                   Z-ADD     ARBAAS        DSAAS
070800941227     C                   Z-ADD     ARBMGS        DSMGS
070900941227     C                   Z-ADD     DSDAT8        D98DSP
071000941227     C                   Z-ADD     ARBMGS        D98MGS
071100941227     C                   Z-ADD     ARBDTI        D98DTI
071200941227     C                   Z-ADD     ARBHTI        D98HTI
071300941227     C                   Z-ADD     ARBDCM        D98DCM
071400941227     C                   Z-ADD     ARBHMC        D98HMC
071500941227     C                   MOVEL     ARBGC1        D98GC1
071600941227     C                   MOVEL     ARBGC2        D98GC2
071700941227     C                   Z-ADD     ARBDCR        D98DCR
071800941227     C                   Z-ADD     ARBHCR        D98HCR
071900100308     C                   MOVEL     ARBTCR        D98TCR
072000100308     c* salvo i campi perchè in alcuni casi li clearo
072100100308     C                   Z-ADD     ARBDCR        s98DCR
072200100308     C                   Z-ADD     ARBHCR        s98HCR
072300100308     C                   MOVEL     ARBTCR        s98TCR
072400100308     c
072500941227     C                   MOVEL     ARBCMC        D98CMC
072600170818     C                   MOVEL     ARBntc        wd98ntc_con
072700941227     C                   MOVEL     ARBFFD        D98FFD
072800941227     C                   Z-ADD     ARBNCD        D98NCD
072900941227     C                   MOVEL     ARBFNS        D98FNS
073000941227     C                   Z-ADD     ARBDUC        D98DUC
073100150508     C                   Z-ADD     ARBDPC        W98DPC
073200970310     C                   Z-ADD     *ZEROS        W98HUC
073300071122     c
073400071122     c                   movel     arbcbo        d98cbo
073500140820     c                   z-add     arbddc        d98ddc
073600140825     c                   z-add     arbndc        wd98ndc
073700140911     c                   movel     arbfbc        wd98fbc
073800150119     c                   movel     arbacc        wd98acc
073900150325     c                   exsr      IN_dist
074000150331     c* Se in fermo deposito vedo se presente particlarità consegna
074100150331     c                   if        arbffd='S' and arbgma<>*blanks
074200150331     C                   Z-ADD     CODUT         KTBUTE
074300150331     C                   MOVEL     '7R'          KTBCOD
074400150331     C                   MOVEL(P)  arbgma        KTBKEY
074500150331     C     KEYTAB        chain     TABEL00F
074600150331     c                   if        %found(tabel00f)
074700150331     c                   movel     tbluni        ds7r
074800150331     c                   endif
074900150331     c                   endif
075000150325     c
075100070511     c
075200141215     C                   Z-ADD     ARBDBR        D98DBR
075300170104     c
075400170105     C                   Z-ADD     ARBDBR        d98dfv
075500141215     c                   eval      d98fdrt=' '
075600070511     c
075700141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
075800070511     c* La data ritiro è presente solo su fnblp
075900070511     C     KEYARB        CHAIN     FNblp01L                           99
076000141215     c                   if        %found(fnblp01l)
076100070511     C                   Z-ADD     blpDrt        D98DBR
076200070511     c                   eval      d98fdrt='S'
076300070511     c                   endif
076400141215     c******             endif
076500070511     c
076600110923     C                   movel     ARBxco        D98xco
076700110923     C                   Z-ADD     ARBDAM        D98DAM
076800941227     C                   MOVEL     ARBFIN        D98FIN
076900990308     C                   MOVEL     ARBFBR        WFBR
077000031201     C                   MOVEL     ARBGGA        WGGA
077100050901     C                   MOVEL     ARBcca        d98cca
077200051124     C                   MOVEL     ARBTC1        d98TC1
077300051124     C                   MOVEL     ARBTC2        d98TC2
077400941227     C                   Z-ADD     1             I
077500941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
077600941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
077700941227     C                   MOVEL     '1'           D98ERR
077800941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
077900941227     C                   ELSE                                                   <-- 2 -->
078000941227     C                   MOVEL     T3A(I)        D98TBL
078100941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
078200990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
078300941227     C                   END                                                    <-- 2 ---
078400130826
078500130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
078600130826     c                   if        %subst(d98tbl:1:1) ='F'
078700130826     C                   Z-ADD     ARBKSC        D98KSC
078800130826     c                   else
078900130826     c
079000130826     c                   if        arbccm>0
079100130826     C                   Z-ADD     ARBccm        D98KSC
079200130826     c                   endif
079300130826     c                   if        d98ksc=0
079400130826     c                   z-add     8888          d98ksc
079500130826     c                   movel     d98lnp        d98ksc
079600130826     c                   endif
079700130826     c                   endif
079800130826     c
079900941227     C                   END                                                    <-- 1 ---
080000941228     C*
080100051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
080200950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
080300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
080400941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
080500941228     C                   Z-ADD     D98AAS        AR9AAS
080600941228     C                   Z-ADD     D98LNP        AR9LNP
080700941228     C                   Z-ADD     D98NRS        AR9NRS
080800941228     C                   Z-ADD     D98NSP        AR9NSP
080900051115     C     KEYAR9        CHAIN     FiAR901L                           99
081000950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
081100950102     C                   MOVEL     'S'           WFLASS
081200950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
081300950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
081400950102     C                   Z-ADD     D98AAS        LBLAAN
081500950102     C                   Z-ADD     D98LNP        LBLLPN
081600950102     C                   Z-ADD     D98NRS        LBLNRN
081700950102     C                   Z-ADD     D98NSP        LBLNSN
081800950102     C     KEYLBL        CHAIN     FNLBL01L                           99
081900950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
082000950102     C                   Z-ADD     LBLAAP        AR9AAS
082100950102     C                   Z-ADD     LBLLPP        AR9LNP
082200950102     C                   Z-ADD     LBLNRP        AR9NRS
082300950102     C                   Z-ADD     LBLNSP        AR9NSP
082400051115     C     KEYAR9        CHAIN     FiAR901L                           99
082500950102     C     *IN99         IFEQ      '0'
082600950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
082700950102     C                   ENDIF
082800950102     C                   ENDIF                                                  <-- 3 ---
082900950102     C                   ENDIF                                                  <-- 2 ---
083000950102     C*
083100950102     C* CONTRASSEGNO NON TROVATO
083200950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
083300941228     C                   ELSE                                                   <-- 2 -->
083400950102     C* CONTRASSEGNO TROVATO
083500941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
083600010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
083700010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
083800941228     C                   ENDIF                                                  <-- 2 ---
083900941228     C                   ENDIF                                                  <-- 1 ---
084000150325     c
084100941227     C*
084200050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
084300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
084400050405     C                   Z-ADD     D98AAS        KWGCPAAS
084500050405     C                   Z-ADD     D98LNP        KWGCPLNP
084600050405     C                   Z-ADD     D98NRS        KWGCPNRS
084700050405     C                   Z-ADD     D98NSP        KWGCPNSP
084800050405     C                   Z-ADD     *ZEROS        KWGCPFRG
084900050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
085000941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
085100941227     C     GCPATB        ANDNE     'A'
085200960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
085300960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
085400960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
085500161107$$$$ C                   movel     gcpfas        D98gfas
085600050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
085700960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
085800960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
085900050405     C* O UGUALE A 35 INVECE DI 40)
086000960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
086100050405     C     GCPFAS        ANDGE     40
086200941227     C                   Z-ADD     GCPDCG        D98DCG
086300950112     C                   Z-ADD     GCPDCG        W98DCG
086400960123     C                   ENDIF
086500960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
086600960729     C     GCPFAS        ANDGE     35
086700070202     C     GCPFAS        ANDne     36
086800960729     C                   Z-ADD     GCPDCG        D98DCG
086900960729     C                   Z-ADD     GCPDCG        W98DCG
087000960729     C                   ENDIF
087100941228     C                   ENDIF
087200941228     C                   ENDIF                                                  <-- 1 ---
087300970911     C*
087400970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
087500970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
087600970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
087700050922     C*****              Z-ADD     D98DTI        W98DTI
087800971210     C                   Z-ADD     D98LNP        F
087900971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
088000970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
088100970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
088200150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
088300150120     c                   if        arbdcp=0 and d98dfc=0
088400150120     C                   Z-ADD     d98dam        D98DTI
088500150120     c                   endif
088600150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
088700150120     C                   Z-ADD     d98dam        D98DTI
088800150120     c                   endif
088900150120     c                   ENDIF
089000150120     c***
089100150120     C***                Z-ADD     D98AAS        ARTAAS
089200150120     C***                Z-ADD     D98LNP        ARTLNP
089300150120     C***                Z-ADD     D98NRS        ARTNRS
089400150120     C***                Z-ADD     D98NSP        ARTNSP
089500150120     C***  KEYART        SETLL     FNART01L                           99
089600150120     C**N99KEYART        READE     FNART01L                               99
089700150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
089800150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
089900970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
090000150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
090100970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
090200970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
090300150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
090400150120     C***  ARTDCM        ANDEQ     *ZEROS
090500970912     C* SE PRIMO REC. TROVATO, MEM. DATA
090600150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
090700150120     C***                Z-ADD     ARTDAM        D98DTI
090800150120     C***                ELSE                                                   <-- 6 -->
090900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
091000150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
091100150120     C***                Z-ADD     ARTDAM        D98DTI
091200150120     C***                ENDIF                                                  <-- 7 ---
091300150120     C***                ENDIF                                                  <-- 6 ---
091400150120     C***                ENDIF                                                  <-- 5 ---
091500970911     C* C'E' DATA CONSEGNAB.
091600150120     C***                ELSE                                                   <-- 4 -->
091700970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
091800970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
091900150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
092000150120     C***  ARTDAM        ANDLE     D98DFC
092100970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
092200970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
092300150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
092400150120     C***  ARTDCM        ORGT      D98DFC
092500970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
092600150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
092700150120     C***                Z-ADD     ARTDAM        D98DTI
092800150120     C***                ELSE                                                   <-- 7 -->
092900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
093000150120     C***  ARTDAM        IFLT      D98DTI
093100150120     C***                Z-ADD     ARTDAM        D98DTI
093200150120     C***                ENDIF
093300150120     C***                ENDIF                                                  <-- 7 ---
093400150120     C***                ENDIF                                                  <-- 6 ---
093500150120     C***                ENDIF                                                  <-- 5 ---
093600150120     C***                ENDIF                                                  <-- 4 ---
093700150120     C***                ENDIF                                                  <-- 3 ---
093800150120     C***  KEYART        READE     FNART01L                               99
093900150120     C***                ENDDO                                                  <-- 2 ---
094000150120     C***                ENDIF                                                  <-- 1 ---
094100941227     C*
094200941227     C                   ENDSR
094300150325     C********************************************************************
094400150325     C*   Verifico se bolla in distinta
094500150325     C********************************************************************
094600150325     c     IN_dist       BEGSR
094700150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
094800150325     c                   if         ARBfbc = *blanks and
094900150325     c                               ARBddc > 0 and
095000150325     c                               ARBndc > 0 and ARBdcm=0
095100150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
095200150325     c                   if        not %open(fiapd01l)
095300150325     c                   open      fiapd01l
095400150325     c                   endif
095500150325     c
095600150325     C                   move      'A'           kpdtip
095700150325     c                   eval      kpdpdr=arbpdc
095800150325     C     kfapd         chain     fiapd01l
095900150325     c
096000150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
096100150325     c                   eval       d98cons = 'S'
096200160506     c                   else
096300160506     c                   eval       d98disf = 'F'
096400150325     c                   endif
096500150325     c                   endif
096600150325     c                   ENDSR
096700941227     C********************************************************************
096800941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
096900941227     C********************************************************************
097000941227     C     LETBLP        BEGSR
097100941227     C*
097200941227     C                   Z-ADD     D98AAS        BLPAAS
097300941227     C                   Z-ADD     D98LNP        BLPLNP
097400941227     C                   Z-ADD     D98NRS        BLPNRS
097500941227     C                   Z-ADD     D98NSP        BLPNSP
097600941227     C     KEYBLP        CHAIN     FNBLP01L                           99
097700941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
097800941227     C                   MOVEL     '1'           D98ERR
097900941227     C                   MOVEL     MSG(20)       D98MSG
098000941227     C                   END                                                    <-- 1 ---
098100941228     C     *IN99         IFEQ      '0'                                                    *TROV.
098200041028     C     KEYBLP        CHAIN     FNarb01L
098300041028     c                   if        %found(fnarb01l)
098400041028     c                   eval      wesarb='S'
098500041028     c                   else
098600041028     c                   eval      wesarb='N'
098700150219     c                   eval      wtrasm='D'
098800041028     c                   endif
098900141216     c
099000141216     c* se non trasmessa la bolla in arrivo memorizzo
099100150219     c                   if        blpft1= ' '
099200141229     c                   eval      wtrasm='N'
099300141216     c                   endif
099400141216     c
099500110923     C                   MOVEL     BLPxco        D98xco
099600110923     C                   MOVEL     BLPTSP        D98TSP
099700130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
099800130529     c                   if        i98tsp_for<>' '
099900130529     C                   MOVEL     i98tsp_for    D98TSP
100000130529     c                   endif
100100960209     C                   MOVEL     BLPNZD        D98NZD
100200941228     C                   MOVEL     BLPCAD        D98CAD
100300970701     C                   MOVEL     BLPLOD        D98LOD
100400941227     C                   Z-ADD     BLPTFP        D98TFP
100500941227     C                   Z-ADD     BLPTFA        D98TFA
100600941227     C                   Z-ADD     BLPLNA        D98LNA
100700941227     C                   Z-ADD     BLPAAS        DSAAS
100800941227     C                   Z-ADD     BLPMGS        DSMGS
100900941227     C                   Z-ADD     DSDAT8        D98DSP
101000941227     C                   Z-ADD     BLPMGS        D98MGS
101100170104
101200170104     c* se si tratta di prepagato NON confermato, devo fare finta che sia bollettato oggi
101300170104     c                   if        wtrasm='N'  and blppdr=0
101400170104     c                   movel     wdaodi        d98dsp
101500170104     c                   z-add     wdaodi        d98mgs
101600170104     c                   endif
101700170104     c
101800051124     C                   movel     BLPTC1        D98TC1
101900051124     C                   movel     BLPTC2        D98TC2
102000041028     c                   if        wesarb='S'
102100041028     C                   Z-ADD     arbDTI        D98DTI
102200041028     C                   Z-ADD     arbHTI        D98HTI
102300041028     C                   Z-ADD     arbDCM        D98DCM
102400041028     C                   Z-ADD     arbHMC        D98HMC
102500041028     C                   Z-ADD     arbDAM        D98DAM
102600050901     C                   MOVEL     arbcca        d98cca
102700140820     C                   MOVEL     arbddc        d98ddc
102800140911     C                   z-add     arbndc        wd98ndc
102900140910     C                   MOVEL     arbfbc        wd98fbc
103000150119     c                   movel     arbacc        wd98acc
103100170818     c                   movel     arbcmc        d98cmc
103200170818     C                   MOVEL     ARBntc        wd98ntc_con
103300150325     c                   exsr      IN_DIST
103400140820     c*
103500041028     c                   else
103600941227     C                   Z-ADD     BLPDTI        D98DTI
103700941227     C                   Z-ADD     BLPHTI        D98HTI
103800941227     C                   Z-ADD     BLPDCM        D98DCM
103900941227     C                   Z-ADD     BLPHMC        D98HMC
104000041028     C                   Z-ADD     BLPDAM        D98DAM
104100050901     C                   MOVEL     BLPcca        d98cca
104200140820     C                   MOVEL     BLPddc        d98ddc
104300140825     C                   MOVEL     BLPndc        wd98ndc
104400140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
104500140820     c                   clear                   d98cons
104600170818     C                   MOVEL     *BLANKS       D98CMC
104700170821     c* in partenza non posso determinale i tentivi di consegna
104800170818     C                   clear                   wd98ntc_con
104900041028     c                   endif
105000941227     C                   MOVEL     BLPGC1        D98GC1
105100941227     C                   MOVEL     BLPGC2        D98GC2
105200941227     C                   Z-ADD     BLPDCR        D98DCR
105300941227     C                   Z-ADD     BLPHCR        D98HCR
105400941227     C                   MOVEL     BLPTCR        D98TCR
105500100308     c* Salvo i campi perchè in alcuni casi li clearo
105600100308     C                   Z-ADD     blpDCR        s98DCR
105700100308     C                   Z-ADD     blpHCR        s98HCR
105800100308     C                   MOVEL     blpTCR        s98TCR
105900100308     c
106000941227     C                   MOVEL     BLPFFD        D98FFD
106100941227     C                   Z-ADD     BLPNCD        D98NCD
106200941227     C                   MOVEL     BLPFNS        D98FNS
106300941228     C                   Z-ADD     BLPDUC        D98DUC
106400150508     C                   Z-ADD     BLPDPC        w98DPC
106500970310     C                   Z-ADD     *ZEROS        W98HUC
106600071122     c                   movel     blpcbo        d98cbo
106700150331     c*
106800150331     c* Se in fermo deposito vedo se presente particlarità consegna
106900150331     c                   if        blpffd='S' and blpgma<>*blanks
107000150331     C                   Z-ADD     CODUT         KTBUTE
107100150331     C                   MOVEL     '7R'          KTBCOD
107200150331     C                   MOVEL(P)  blpgma        KTBKEY
107300150331     C     KEYTAB        chain     TABEL00F
107400150331     c                   if        %found(tabel00f)
107500150331     c                   movel     tbluni        ds7r
107600150331     c                   endif
107700150331     c                   endif
107800150331     c
107900970304     C                   Z-ADD     BLPDBR        D98DBR
108000170105     C                   Z-ADD     BLPDBR        d98dfv
108100070514     c                   eval      d98fdrt=' '
108200070514     c
108300141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
108400070514     C                   Z-ADD     blpDrt        D98DBR
108500070514     c                   eval      d98fdrt='S'
108600141215     c********           endif
108700070514     c*
108800941227     C                   MOVEL     BLPFIN        D98FIN
108900130826     C****               Z-ADD     BLPKSC        D98KSC
109000990308     C                   MOVEL     BLPFBR        WFBR
109100031201     C                   MOVEL     BLPGGA        WGGA
109200941227     C                   Z-ADD     1             I
109300941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
109400941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
109500941227     C                   MOVEL     '1'           D98ERR
109600941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
109700941227     C                   ELSE                                                   <-- 2 -->
109800941227     C                   MOVEL     T3A(I)        D98TBL
109900941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
110000990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
110100941227     C                   END                                                    <-- 2 ---
110200130826     c                   if        %subst(d98tbl:1:1) ='F'
110300130826     C                   Z-ADD     blpKSC        D98KSC
110400130826     c                   else
110500130826     c
110600130826     c                   if        blpccm>0
110700130826     C                   Z-ADD     blpccm        D98KSC
110800130826     c                   endif
110900130826     c                   if        d98ksc=0
111000130826     c                   z-add     8888          d98ksc
111100130826     c                   movel     d98lnp        d98ksc
111200130826     c                   endif
111300130826     c                   endif
111400130826     c
111500941227     C                   END
111600941228     C*
111700051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
111800950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
111900941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
112000941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
112100051115     C                   Z-ADD     D98AAS        ar9AAS
112200051115     C                   Z-ADD     D98LNP        ar9LNP
112300051115     C                   Z-ADD     D98NRS        ar9NRS
112400051115     C                   Z-ADD     D98NSP        ar9NSP
112500051115     C     KEYar9        CHAIN     Fiar901L                           99
112600950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
112700950102     C                   MOVEL     'S'           WFLASS
112800950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
112900950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
113000950102     C                   Z-ADD     D98AAS        LBLAAN
113100950102     C                   Z-ADD     D98LNP        LBLLPN
113200950102     C                   Z-ADD     D98NRS        LBLNRN
113300950102     C                   Z-ADD     D98NSP        LBLNSN
113400950102     C     KEYLBL        CHAIN     FNLBL01L                           99
113500950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
113600051115     C                   Z-ADD     LBLAAP        ar9AAS
113700051115     C                   Z-ADD     LBLLPP        ar9LNP
113800051115     C                   Z-ADD     LBLNRP        ar9NRS
113900051115     C                   Z-ADD     LBLNSP        ar9NSP
114000051115     C     KEYar9        CHAIN     Fiar901L                           99
114100950102     C     *IN99         IFEQ      '0'
114200950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
114300950102     C                   ENDIF
114400950102     C                   ENDIF                                                  <-- 3 ---
114500950102     C                   ENDIF                                                  <-- 2 ---
114600950102     C*
114700950102     C* CONTRASSEGNO TROVATO
114800150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
114900051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
115000051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
115100051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
115200941228     C                   ENDIF                                                  <-- 2 ---
115300941228     C                   ENDIF                                                  <-- 1 ---
115400941227     C*
115500050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
115600941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
115700050405     C                   Z-ADD     D98AAS        KWGCPAAS
115800050405     C                   Z-ADD     D98LNP        KWGCPLNP
115900050405     C                   Z-ADD     D98NRS        KWGCPNRS
116000050405     C                   Z-ADD     D98NSP        KWGCPNSP
116100050405     C                   Z-ADD     *ZEROS        KWGCPFRG
116200050405     C     KEYGCP        CHAIN     TIGCP01L                           99
116300941228     C     *IN99         IFEQ      '0'                                          *TROV.
116400941227     C     GCPATB        ANDNE     'A'
116500960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
116600960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
116700960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
116800960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
116900161107$$$$ C                   movel     gcpfas        D98Gfas
117000050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
117100960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
117200960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
117300050405     C* O UGUALE A 35 INVECE DI 40)
117400960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
117500050405     C     GCPFAS        ANDGE     40
117600941227     C                   Z-ADD     GCPDCG        D98DCG
117700950112     C                   Z-ADD     GCPDCG        W98DCG
117800960123     C                   ENDIF
117900960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
118000960729     C     GCPFAS        ANDGE     35
118100070202     C     GCPFAS        ANDne     36
118200960729     C                   Z-ADD     GCPDCG        D98DCG
118300960729     C                   Z-ADD     GCPDCG        W98DCG
118400960729     C                   ENDIF
118500941228     C                   ENDIF
118600941228     C                   ENDIF                                                  <-- 1 ---
118700970912     C*
118800970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
118900970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
119000970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
119100050922     C******             Z-ADD     D98DTI        W98DTI
119200971210     C                   Z-ADD     D98LNP        F
119300971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
119400970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
119500970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
119600150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
119700150120     c                   if        blpdcp=0 and d98dfc=0
119800150120     C                   Z-ADD     d98dam        D98DTI
119900150120     c                   endif
120000150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
120100150120     C                   Z-ADD     d98dam        D98DTI
120200150120     c                   endif
120300150120     c                   ENDIF
120400150120     c*
120500150120     C***                Z-ADD     D98AAS        BLTAAS
120600150120     C****               Z-ADD     D98LNP        BLTLNP
120700150120     C***                Z-ADD     D98NRS        BLTNRS
120800150120     C***                Z-ADD     D98NSP        BLTNSP
120900150120     C***  KEYBLT        SETLL     FNBLT01L                           99
121000150120     C**N99KEYBLT        READE     FNBLT01L                               99
121100150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
121200150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
121300970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
121400150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
121500970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
121600970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
121700150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
121800150120     C**   BLTDCM        ANDEQ     *ZEROS
121900970912     C* SE PRIMO REC. TROVATO, MEM. DATA
122000150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
122100150120     C**                 Z-ADD     BLTDAM        D98DTI
122200150120     C**                 ELSE                                                   <-- 6 -->
122300970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
122400150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
122500150120     C**                 Z-ADD     BLTDAM        D98DTI
122600150120     C**                 ENDIF                                                  <-- 7 ---
122700150120     C**                 ENDIF                                                  <-- 6 ---
122800150120     C**                 ENDIF                                                  <-- 5 ---
122900970912     C* C'E' DATA CONSEGNAB.
123000150120     **                  ELSE                                                   <-- 4 -->
123100970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
123200970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
123300150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
123400150120     C**   BLTDAM        ANDLT     D98DFC
123500970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
123600970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
123700150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
123800150120     C**   BLTDCM        ORGT      D98DFC
123900970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
124000150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
124100150120     C**                 Z-ADD     BLTDAM        D98DTI
124200150120     C**                 ELSE                                                   <-- 7 -->
124300970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
124400150120     C**   BLTDAM        IFLT      D98DTI
124500150120     C**                 Z-ADD     BLTDAM        D98DTI
124600150120     C**                 ENDIF
124700150120     C**                 ENDIF                                                  <-- 7 ---
124800150120     C**                 ENDIF                                                  <-- 6 ---
124900150120     C**                 ENDIF                                                  <-- 5 ---
125000150120     C**                 ENDIF                                                  <-- 4 ---
125100150120     C**                 ENDIF                                                  <-- 3 ---
125200150120     C**   KEYBLT        READE     FNBLT01L                               99
125300150120     C**                 ENDDO                                                  <-- 2 ---
125400150120     C**                 ENDIF                                                  <-- 1 ---
125500941227     C*
125600941227     C                   ENDSR
125700930323     C********************************************************************
125800941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
125900930323     C********************************************************************
126000930323     C     RTTEOR        BEGSR
126100941227     C*
126200941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
126300941228     C     D98ERR        IFEQ      '0'
126400941227     C                   EXSR      RTPART
126500941227     C                   ENDIF
126600941227     C*
126700941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
126800941228     C     D98ERR        IFEQ      '0'
126900150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
127000150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
127100150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
127200150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
127300150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
127400941227     C                   EXSR      RTTRAZ
127500150219     c                   if        d98err<>'1'
127600150219     C                   Z-ADD     CASTIN        D98TTI
127700150219     C                   Z-ADD     CASTCN        D98TTC
127800150219     C                   endif
127900150219     C                   ENDIF
128000941227     C*
128100941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
128200941228     C     D98ERR        IFEQ      '0'
128300941227     C                   EXSR      RTCONS
128400941227     C                   ENDIF
128500941227     C*
128600941228     C     D98ERR        IFEQ      '0'
128700941227     C*
128800941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
128900941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
129000941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
129100090921     c                   z-add     d98tri        w99tri
129200090921     c* per la DS
129300090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
129400090921     c* per i calcoli interi
129500090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
129600090921     c
129700941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
129800941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
129900941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
130000090921     c                   z-add     d98trc        w99trc
130100090921     c* per la DS
130200090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
130300090921     c* per i calcoli interi
130400090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
130500941227     C*
130600941227     C                   ENDIF
130700930323     C*
130800930323     C                   ENDSR
130900941227     C********************************************************************
131000941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
131100941227     C********************************************************************
131200941227     C     RTPART        BEGSR
131300941228     C*
131400941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
131500941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
131600941228     C                   MOVEL     D98TSP        WTSP              1
131700130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
131800130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
131900050301     C*******
132000050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
132100050301     C*
132200050301     C*    W001          IFEQ      'A'
132300050301     C*    W0040         OREQ      8888
132400050301     C*                  MOVEL     'E'           WTSP
132500050301     C*                  ENDIF
132600050301     C*******
132700941227     C*
132800941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
132900941227     C                   MOVEL     'N'           WTRAOK            1
133000941227     C                   MOVEL     'P'           CASPTA
133100941227     C                   Z-ADD     D98LNP        CASLNP
133200941227     C                   Z-ADD     *ZEROS        CASLNA
133300941228     C                   MOVEL     WTSP          CASTSP
133400941228     C                   Z-ADD     99999999      CASDDE
133500941227     C     KEYCAS        SETGT     AZCAS01L                             99
133600941227     C  N99              READP     AZCAS01L                               99
133700941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
133800941227     C     WTRAOK        ANDEQ     'N'
133900941227     C     CASLNP        ANDEQ     D98LNP
134000950102     C     CASTSP        ANDEQ     WTSP
134100941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
134200941227     C     CASATB        ANDEQ     *BLANKS
134300941227     C                   MOVEL     'S'           WTRAOK
134400941227     C                   ELSE                                                   <-- 2 -->
134500941227     C                   READP     AZCAS01L                               99
134600941227     C                   ENDIF                                                  <-- 2 ---
134700941227     C                   ENDDO                                                  <-- 1 ---
134800941227     C*
134900941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
135000941227     C     WTRAOK        IFEQ      'N'
135100941227     C                   MOVEL     '1'           D98ERR
135200941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
135300941227     C                   ELSE
135400941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
135500941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
135600941227     C                   END
135700970214     C*
135800140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
135900140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
136000140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
136100140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
136200140403     C**                 Z-ADD     D98LNA        F
136300140403     C**   D98ERR        IFEQ      '0'
136400140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
136500140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
136600140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
136700140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
136800140403     C**                 ENDIF
136900140403     C**                 ENDIF
137000140402     c
137100140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
137200140402     c*  se tempi  = 0
137300140402     c                   if        wpick2='S' and d98tpi<=0
137400140402     c                   add       24            D98TPI
137500140402     c                   endif
137600941227     C*
137700941227     C                   ENDSR
137800941227     C********************************************************************
137900941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
138000941227     C********************************************************************
138100941227     C     RTTRAZ        BEGSR
138200941227     C*
138300941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
138400150219     c
138500150219     C                   Z-ADD     D98LNA        LTA(1)
138600150219     C                   Z-ADD     D98LNA        LTA(2)
138700150219     C                   Z-ADD     D98TFA        LTA(3)
138800941227     C                   Z-ADD     D98TFA        LTA(4)
138900941227     C*
139000941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
139100941227     C                   MOVEL     'N'           WTRAOK            1
139200941227     C                   Z-ADD     1             K                 3 0
139300941227     C*
139400941227     C* CICLO FINO A:
139500941227     C*    - FINE SCHIERA
139600941227     C*    - TROVATO TEMPI VALIDI
139700941227     C     K             DOUGT     4                                            --- 1 -->
139800941227     C     WTRAOK        OREQ      'S'
139900941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
140000941227     C     LTA(K)        ORNE      *ZEROS
140100941227     C                   MOVEL     'T'           CASPTA
140200941227     C                   Z-ADD     LTP(K)        CASLNP
140300941227     C                   Z-ADD     LTA(K)        CASLNA
140400941227     C                   MOVEL     D98TSP        CASTSP
140500941228     C                   Z-ADD     99999999      CASDDE
140600941227     C     KEYCAS        SETGT     AZCAS01L                             99
140700941227     C  N99              READP     AZCAS01L                               99
140800941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
140900941227     C     WTRAOK        ANDEQ     'N'
141000941227     C     LTP(K)        ANDEQ     CASLNP
141100941227     C     LTA(K)        ANDEQ     CASLNA
141200941227     C     D98TSP        ANDEQ     CASTSP
141300960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
141400941227     C     CASATB        ANDEQ     *BLANKS
141500941227     C                   MOVEL     'S'           WTRAOK
141600941227     C                   ELSE                                                   <-- 4 -->
141700941227     C                   READP     AZCAS01L                               99
141800941227     C                   ENDIF                                                  <-- 4 ---
141900941227     C                   ENDDO                                                  <-- 3 ---
142000941227     C                   ENDIF                                                  <-- 2 ---
142100941227     C                   ADD       1             K
142200941227     C                   ENDDO                                                  <-- 1 ---
142300941227     C*
142400941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
142500941227     C     WTRAOK        IFEQ      'N'
142600941227     C                   MOVEL     '1'           D98ERR
142700941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
142800150219     C***                ELSE
142900150219     C***                Z-ADD     CASTIN        D98TTI
143000150219     C***                Z-ADD     CASTCN        D98TTC
143100941227     C                   END
143200941227     C*
143300941227     C                   ENDSR
143400941227     C********************************************************************
143500941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
143600941227     C********************************************************************
143700941227     C     RTCONS        BEGSR
143800150629     c                   clear                   WTsogliaE         1
143900970701     C*
144000970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
144100970701     C                   CLEAR                   SI95
144200970701     C                   MOVEL     '3'           I95TCN
144300970701     C                   MOVEL     D98NZD        I95NAR
144400970701     C                   MOVEL     D98CAD        I95CAP
144500970701     C                   MOVEL     D98LOD        I95LOC
144600970701     C                   MOVEL     D98DSP        I95DAT
144700970701     C                   MOVEL     D98TSP        I95TSP
144800980602     C                   MOVEL     'S'           I95FRT
144900150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
145000150511     c                   if        d98ffd='S'
145100150511     c                   eval      i95tsp='C'
145200150511     c                   endif
145300020226     C*
145400020226     C* VERIFICO SE LNA E' FILIALE FEDEX
145500020226     C                   IF        D98LNA > *ZEROS
145600020226     C     D98LNA        LOOKUP    FILFED                                 55
145700020226     C                   IF        *IN55 = *ON
145800020226     C                   EVAL      I95FI1 = 'S'
145900020226     C                   ENDIF
146000020226     C                   ENDIF
146100020226     C*
146200020226     C                   CALL      'TISI95R'
146300970701     C                   PARM                    SI95
146400941227     C*
146500941228     C* SE NON TROVATO, ERRORE
146600970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
146700941227     C                   MOVEL     '1'           D98ERR
146800970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
146900941227     C                   ELSE                                                   <-- 1 -->
147000941227     C*
147100941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
147200941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
147300941227     C     CASTIN        DIV       24            WDIV              3 0
147400941227     C                   MVR                     WRES              3 0
147500941227     C     WRES          IFEQ      *ZEROS
147600970701     C                   Z-ADD     O950IN        D98TCI
147700941227     C                   ELSE
147800970701     C                   Z-ADD     O951IN        D98TCI
147900941227     C                   END
148000941227     C*
148100941227     C     CASTCN        DIV       24            WDIV                     99
148200941227     C                   MVR                     WRES
148300941227     C     WRES          IFEQ      *ZEROS
148400970701     C                   Z-ADD     O950CN        D98TCC
148500150629     c                   eval      wTSogliaE='0'
148600941227     C                   ELSE
148700970701     C                   Z-ADD     O951CN        D98TCC
148800150629     c                   eval      wTSogliaE='1'
148900941227     C                   END
149000941227     C*
149100150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
149200941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
149300150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
149400150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
149500150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
149600150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
149700150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
149800941227     C*
149900941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
150000941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
150100970701     C     O95ISO        IFEQ      'I'
150200941227     C     D98FIN        OREQ      'I'
150300970701     C                   ADD       O95TIS        D98TCI
150400970701     C                   ADD       O95TIS        D98TCC
150500941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
150600970701     C                   ADD       O95TIS        W990IN
150700970701     C                   ADD       O95TIS        W991IN
150800150629     C                   ADD       O95TIS        W990CN
150900150629     C                   ADD       O95TIS        W991CN
151000941227     C                   END
151100090827     c
151200090827     c* Sistemo i tempi di consegna se non sono multipli di 12
151300090827     c                   exsr      SisCONS
151400160404     c
151500160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
151600160404     c                   if        wVP='S'
151700160404     c                   add       24            w991IN
151800160404     c                   add       24            w991CN
151900160404     c                   add       24            w990IN
152000160404     c                   add       24            w990CN
152100160404     c                   endif
152200941228     C*
152300941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
152400941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
152500941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
152600941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
152700941228     C                   Z-ADD     D98TCC        W99CCE            3 0
152800941228     C                   ELSE                                                   <-- 3 -->
152900970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
153000970701     C                   CLEAR                   SI95
153100970701     C                   MOVEL     '3'           I95TCN
153200970701     C                   MOVEL     D98NZD        I95NAR
153300970701     C                   MOVEL     D98CAD        I95CAP
153400970701     C                   MOVEL     D98LOD        I95LOC
153500970701     C                   MOVEL     D98DSP        I95DAT
153600970701     C                   MOVEL     'E'           I95TSP
153700980602     C                   MOVEL     'S'           I95FRT
153800020226     C*
153900020226     C* VERIFICO SE LNA E' FILIALE FEDEX
154000020226     C                   IF        D98LNA > *ZEROS
154100020226     C     D98LNA        LOOKUP    FILFED                                 55
154200020226     C                   IF        *IN55 = *ON
154300020226     C                   EVAL      I95FI1 = 'S'
154400020226     C                   ENDIF
154500020226     C                   ENDIF
154600020226     C*
154700020226     C                   CALL      'TISI95R'
154800970701     C                   PARM                    SI95
154900970701     C*
155000941228     C* SE NON TROVATO, ERRORE
155100970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
155200941228     C                   MOVEL     '1'           D98ERR
155300970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
155400941228     C                   ELSE                                                   <-- 4 -->
155500941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
155600941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
155700941228     C     CASTCN        DIV       24            WDIV                     99
155800941228     C                   MVR                     WRES
155900941228     C     WRES          IFEQ      *ZEROS
156000970701     C                   Z-ADD     O950CN        W99CCE
156100941228     C                   ELSE
156200970701     C                   Z-ADD     O951CN        W99CCE
156300941228     C                   END
156400941228     C                   ENDIF                                                  <-- 4 ---
156500941228     C                   ENDIF                                                  <-- 3 ---
156600941228     C                   ENDIF                                                  <-- 2 ---
156700941227     C*
156800941227     C                   END                                                    <-- 1 ---
156900941227     C*
157000941227     C                   ENDSR
157100090827     C********************************************************************
157200090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
157300090827     C********************************************************************
157400090827     C     SisCONS       BEGSR
157500090827     c                   eval      w99tci=d98tci
157600090827     c                   if        d98tci=10
157700090827     c                   z-add     12            w99tci
157800090827     c                   endif
157900090827     c
158000090827     c                   eval      w99tcc=d98tcc
158100090827     c                   if        d98tcc=10
158200090827     c                   z-add     12            w99tcc
158300090827     c                   endif
158400160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
158500160404     c                   if        wVP='S'
158600160404     c                   add       24            w99tcc
158700160404     c                   add       24            w99tci
158800160404     c                   endif
158900090827     c
159000090827     C                   ENDSR
159100930420     C********************************************************************
159200930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
159300930420     C********************************************************************
159400930420     C     CAREVB        BEGSR
159500930420     C*
159600930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
159700930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
159800060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
159900930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
160000930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
160100060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
160200060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
160300930420     C                   Z-ADD     D98AAS        KEBAAS
160400930420     C                   Z-ADD     D98LNP        KEBLNP
160500930420     C                   Z-ADD     D98NRS        KEBNRS
160600930420     C                   Z-ADD     D98NSP        KEBNSP
160700990810     C     KEYEVB        SETLL     FNEVB01L                           99
160800060406     C  N99keyevb        READe     FNEVB01L                               99
160900060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
161000930421     C     D98ERR        ANDEQ     '0'
161100060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
161200930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
161300930421     C                   Z-ADD     1             I
161400930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
161500060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
161600930421     C                   MOVEL     '1'           D98ERR
161700930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
161800060406   x3C                   ELSE                                                   <-- 3 -->
161900060406     C                   MOVEL     D2A(I)        usa2A
162000060406     C                   MOVEL     D2Z(I)        usa2Z
162100930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
162200060406    4C     §2AFTC        IFEQ      'A'
162300930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
162400060406    4C                   END
162500060406    4C     §2AFTC        IFEQ      'G'
162600930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
162700060406    4C                   END
162800150216     c* Se si tratta di MIC verifico se uguale alla sped.
162900150303    4c                   if        evbcev='MIC' or evbcev='NIC'
163000150303    5c                   if        %subst(evbnot:1:7) >*zeros
163100150303     c
163200150303    6c                   if        evbcev='MIC' and
163300150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
163400150216     c                   eval      evemic='SI'
163500150303    6c                   endif
163600150303     c
163700150303    6c                   if        evbcev='NIC' and
163800150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
163900150216     c                   eval      evemic='  '
164000150303    6c                   endif
164100150303     c
164200150303   x5c                   else
164300150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
164400150303     c                   eval      evemic='SI'
164500150303    6c                   endif
164600150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
164700150303     c                   eval      evemic='  '
164800150303    6c                   endif
164900150303
165000150303    5c                   endif
165100150303    4c                   endif
165200150216     c
165300060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
165400930421     C     §2AFTC        OREQ      'G'
165500930421     C                   ADD       1             IE
165600990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
165700930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
165800930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
165900131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
166000090928     c
166100090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
166200091014     c**                 if        ie=1
166300091014     c**                 eval      Primotipo=§2aftc
166400091014     c**                 eval      Primodev=evbdev
166500091014     c**                 eval      Primohev=evbhev
166600091014     c**                 endif
166700090928     c
166800060406    4C                   END                                                    <-- 4 ---
166900060406     c* Verifico se si tratta di evento particolare
167000060406    4c                   if        §2zCEP<>' '
167100060526     c* non carico l'evento se cade in giorno festivo
167200060526     c                   z-add     evbdev        wpodata
167300060526     C                   EVAL      WTC='C'
167400060526     C                   EVAL      WIC='C'
167500060526     c                   eval      wslitt='N'
167600060526     c                   exsr      chkfes
167700060526     c
167800060526     c* Wslitt='N' non festivo
167900060526   4ac                   if        wslitt='N'
168000060526     c
168100060406    5c                   if        §2zfep='A'
168200060406     c                   eval      wevpaA='S'
168300060406    5C                   END                                                    <-- 3 ---
168400060406    5c                   if        §2zfep='G'
168500060406     c                   eval      wevpaG='S'
168600060406    5C                   END                                                    <-- 3 ---
168700060406     c                   add       1             ip
168800060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
168900060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
169000060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
169100060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
169200060526   4aC                   ENDif                                                  <-- 3 ---
169300060526    4C                   END                                                    <-- 3 ---
169400060406    3C                   END                                                    <-- 3 ---
169500060406    2C                   END                                                    <-- 2 ---
169600060406     C     keyevb        READe     FNEVB01L                               99
169700060406    1C                   ENDDO                                                  <-- 1 ---
169800050523     c* controllo riempimento skiere
169900050523     c                   clear                   trul0sds
170000050523     c                   eval      i0sski='EVD/EVO/EVC'
170100050523     c                   eval      i0sele=%elem(EVD)
170200050523     c                   eval      i0spie=ie
170300050523     c                   eval      i0sfile='FNEVB01L  '
170400050523     c                   eval      i0scod='     '
170500050523     c                   eval      i0spgm='TNSD99R'
170600050523     c                   movel     trul0sds      kpjbu
170700050523     c                   call      'TRUL0SR'
170800050523     c                   parm                    kpjba
170900930420     C*
171000060406     c* controllo riempimento skiere
171100060406     c                   clear                   trul0sds
171200060406     c                   eval      i0sski='PVD/PVO/PVC'
171300060406     c                   eval      i0sele=%elem(PVD)
171400060406     c                   eval      i0spie=ie
171500060406     c                   eval      i0sfile='FNEVB01L  '
171600060406     c                   eval      i0scod='     '
171700060406     c                   eval      i0spgm='TNSD99R'
171800060406     c                   movel     trul0sds      kpjbu
171900060406     c                   call      'TRUL0SR'
172000060406     c                   parm                    kpjba
172100060406     C*
172200930420     C                   ENDSR
172300931213     C********************************************************************
172400931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
172500931213     C********************************************************************
172600931213     C     CARTUC        BEGSR
172700931213     C*
172800931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
172900931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
173000931214     C     D98GC2        ANDEQ     *BLANKS
173100931213     C                   ELSE                                                   <-- 1 -->
173200931213     C*
173300931213     C                   MOVE      D98GC1        TUC(1)
173400931214     C                   MOVE      D98GC2        TUC(2)
173500931213     C*
173600931213     C                   END                                                    <-- 1 ---
173700931213     C*
173800931213     C                   ENDSR
173900970304     C********************************************************************
174000970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
174100970304     C********************************************************************
174200970304     C     DATRIF        BEGSR
174300970304     C*
174400050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
174500050922     c*  partenza
174600970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
174700970304     C*
174800970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
174900070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
175000070605     C***                MOVEL     '1'           D98ERR
175100070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
175200070605     C***                ELSE                                                   <-- 2 -->
175300970304     C*
175400970304     C                   Z-ADD     D98DRF        WD1
175500970304     C                   Z-ADD     D98ORF        WO1
175600970310     C* DATA PARTENZA ULTIMO COLLO
175700970310     C     D98DRF        IFLE      D98DUC
175800050928     c     d98duc        oreq      0
175900970310     C                   Z-ADD     D98DRF        D98DUC
176000970310     C                   Z-ADD     D98ORF        W98HUC
176100070605     C                   ENDIF
176200970304     C* DATA ARRIVO TRAZIONE
176300050922     C***                Z-ADD     D98DTI        WD2
176400050922     C***                Z-ADD     D98HTI        WO2
176500050922     C***  WDO1          IFLE      WDO2
176600050922     C***                Z-ADD     D98DRF        D98DTI
176700050922     C***                Z-ADD     D98ORF        D98HTI
176800050922     C***                ENDIF
176900970304     C* DATA CONSEGNA
177000050922     C***                Z-ADD     D98DCM        WD2
177100050922     C***                Z-ADD     D98HMC        WO2
177200050922     C***  WDO1          IFLE      WDO2
177300050922     C***                Z-ADD     D98DRF        D98DCM
177400050922     C***                Z-ADD     D98ORF        D98HMC
177500050922     C***                ENDIF
177600970304     C*
177700070605     C***                ENDIF                                                  <-- 2 ---
177800970304     C                   ENDIF                                                  <-- 1 ---
177900970304     C*
178000970304     C                   ENDSR
178100940901     C********************************************************************
178200940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
178300940901     C********************************************************************
178400940901     C     TDTIPO        BEGSR
178500940901     C*
178600940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
178700940902     C                   Z-ADD     D98TTI        W98TTI            3 0
178800941011     C*
178900941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
179000941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
179100941011     C     W98TTI        DIV       24            WDIV              3 0
179200941011     C                   MVR                     WRES              3 0
179300941011     C     WRES          IFEQ      *ZEROS
179400941011     C                   Z-ADD     *ZEROS        WASTA             9 0
179500941011     C                   ELSE
179600941011     C                   Z-ADD     12            WASTA
179700941011     C                   END
179800940901     C*
179900940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
180000940901     C     D98DTI        IFNE      *ZEROS
180100940901     C                   Z-ADD     D98DTI        WDAT
180200940901     C                   EXSR      DATORE
180300960911     C                   Z-ADD     WORE          WPODTI
180400960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
180500960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
180600960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
180700960911     C                   Z-ADD     WPODTI        W10POI            9 0
180800960911     C                   EXSR      R10FES
180900960911     C                   Z-ADD     W10POO        WPODTI
181000960911     C* AGGIUNGO LE ORA D'ASTA
181100960911     C                   ADD       WASTA         WPODTI
181200960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
181300960911     C     W10POI        IFEQ      W10POO
181400940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
181500940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
181600940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
181700940901     C                   EXSR      RO3ORE
181800940901     C                   ADD       WR3HHO        WPODTI
181900940901     C                   ADD       WR3HHO        W98TTI
182000940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
182100940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
182200940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
182300940901     C                   EXSR      RO3ORE
182400940901     C                   ADD       WR3HHO        WPODTI
182500940901     C                   ADD       WR3HHO        W98TTI
182600960911     C                   ENDIF                                                  *SE NON FESTIVO
182700940901     C                   END
182800940901     C*
182900940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
183000940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
183100940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
183200940901     C     W98TTI        DIV       24            WDIV              3 0
183300940901     C                   MVR                     WRES              3 0
183400940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
183500941228     C                   Z-ADD     W990IN        D98TCI
183600160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
183700160405     c*  per cui le sottraggo, per riagguingerle alla fine
183800160405     c                   if        wVP='S'
183900160405     c                   sub       24            D98TCI
184000160405     c                   endif
184100160405     c
184200150630     c                   eval      wTSogliaE='0'
184300940901     C                   ELSE                                                   <-- 2 -->
184400941228     C                   Z-ADD     W991IN        D98TCI
184500160405     c                   if        wVP='S'
184600160405     c                   sub       24            D98TCI
184700160405     c                   endif
184800150630     c                   eval      wTSogliaE='1'
184900940908     C                   ENDIF                                                  <-- 2 ---
185000090827     c* Sistemo i tempi di consegna se non sono multipli di 12
185100090827     c                   exsr      SisCONS
185200090827     c*
185300940908     C* RICALCOLO TEMPO DI RESA INTERNO
185400950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
185500950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
185600090921     c                   z-add     d98tri        w99tri
185700090921     c* per la DS
185800090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
185900090921     c* per i calcoli interni
186000090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
186100940908     C                   ENDIF                                                  <-- 1 ---
186200940901     C*
186300940901     C                   ENDSR
186400930325     C********************************************************************
186500930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
186600930325     C********************************************************************
186700930325     C     TDATPO        BEGSR
186800930406     C*
186900930420     C* TRASFORMO LA DATA ODIERNA
187000930416     C                   Z-ADD     WDAODI        WDAT
187100930416     C                   EXSR      DATORE
187200050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
187300960911     C                   MOVEL     'OSC  '       WR3TAB
187400960911     C                   Z-ADD     WHHODI        WR3HHI
187500960911     C                   EXSR      RO3ORE
187600050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
187700050922     c                   if        wr3hho=12
187800050922     c     wpoatt        add       12            wpoattdata24
187900050922     c                   else
188000050922     c                   z-add     wpoatt        wpoattdata24
188100050922     c                   endif
188200930325     C*
188300930420     C* TRASFORMO LA DATA DI SPEDIZIONE
188400931125     C     D98DSP        IFEQ      *ZEROS
188500930326     C                   MOVEL     '1'           D98ERR
188600930326     C                   MOVEL     MSG(2)        D98MSG
188700930326     C                   ELSE
188800931125     C                   Z-ADD     D98DSP        WDAT
188900930325     C                   EXSR      DATORE
189000930406     C                   Z-ADD     WORE          WPODSP
189100930326     C                   END
189200930414     C*
189300930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
189400930414     C                   MOVEL     'LIM  '       WR3TAB
189500930414     C                   Z-ADD     *ZEROS        WR3HHI
189600930414     C                   EXSR      RO3ORE
189700931222     C     WR3HHO        MULT      10            WTM060            6 0
189800931222     C     WPODSP        ADD       WTM060        WPOLIM
189900140822     c
190000930325     C*
190100930420     C* TRASFORMO LA DATA DI CONSEGNA
190200931125     C     D98DCM        IFNE      *ZEROS
190300931125     C                   Z-ADD     D98DCM        WDAT
190400930325     C                   EXSR      DATORE
190500930326     C                   Z-ADD     WORE          WPODCM
190600930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
190700930326     C                   MOVEL     'OSC  '       WR3TAB
190800931125     C                   Z-ADD     D98HMC        WR3HHI
190900930326     C                   EXSR      RO3ORE
191000930326     C                   ADD       WR3HHO        WPODCM
191100930326     C                   END
191200941228     C*
191300941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
191400941228     C     D98DUC        IFNE      *ZEROS
191500941228     C                   Z-ADD     D98DUC        WDAT
191600941228     C                   EXSR      DATORE
191700941228     C                   Z-ADD     WORE          WPODUC
191800970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
191900970310     C                   MOVEL     'OSP  '       WR3TAB
192000970310     C                   Z-ADD     W98HUC        WR3HHI
192100970310     C                   EXSR      RO3ORE
192200970310     C     D98DRF        IFNE      *ZEROS
192300050928     c     w98huc        andgt     1200
192400970310     C                   SUB       12            WR3HHO
192500970310     C                   ENDIF
192600970310     C                   ADD       WR3HHO        WPODUC
192700941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
192800070605     C***  WPODSP        IFGT      WPODUC
192900070605     C***                Z-ADD     WPODSP        WPODUC
193000070605     C***                ENDIF
193100941228     C                   ENDIF
193200960402     C*
193300141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
193400970304     C     D98DBR        IFNE      *ZEROS
193500970304     C                   Z-ADD     D98DBR        WDAT
193600960402     C                   EXSR      DATORE
193700960402     C                   Z-ADD     WORE          WPODBR
193800070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
193900960402     C                   ELSE
194000960402     C                   Z-ADD     WPODSP        WPODBR
194100960402     C                   ENDIF
194200930913     C*
194300960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
194400930913     C* CONSEGNABILITA'
194500960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
194600950407     C*
194700930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
194800931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
194900931125     C                   Z-ADD     D98DIC        WDAT
195000930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
195100930913     C                   Z-ADD     WORE          WPODIC
195200930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
195300931125     C                   Z-ADD     D98OIC        WR3HHI
195400930913     C                   EXSR      RO3ORE
195500930913     C                   ADD       WR3HHO        WPODIC
195600931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
195700950407     C*
195800930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
195900931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
196000931125     C                   Z-ADD     D98DFC        WDAT
196100930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
196200930913     C                   Z-ADD     WORE          WPODFC
196300140822     C                   Z-ADD     WORE          WPODFC_00
196400930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
196500931125     C                   Z-ADD     D98OFC        WR3HHI
196600930913     C                   EXSR      RO3ORE
196700930913     C                   ADD       WR3HHO        WPODFC
196800931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
196900930913     C*
197000930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
197100930913     C     D98ERR        IFEQ      '0'
197200930913     C     WPODFC        IFEQ      *ZEROS
197300930913     C     WPODIC        ANDNE     *ZEROS
197400930913     C                   MOVEL     '1'           D98ERR
197500930913     C                   MOVEL     MSG(17)       D98MSG
197600930913     C                   ENDIF
197700930913     C                   ENDIF
197800930907     C*
197900930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
198000930913     C     D98ERR        IFEQ      '0'
198100930913     C     WPODIC        IFGT      WPODFC
198200930907     C                   MOVEL     '1'           D98ERR
198300930913     C                   MOVEL     MSG(18)       D98MSG
198400930913     C                   ENDIF
198500930913     C                   ENDIF
198600950407     C*
198700950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
198800930511     C*
198900930511     C                   ENDSR
199000950110     C********************************************************************
199100950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
199200950110     C********************************************************************
199300950110     C     CALDPP        BEGSR
199400950110     C*
199500950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
199600950110     C                   MOVEL     'P'           WTC                            *PARTENZA
199700950110     C                   MOVEL     'I'           WIC                            *INTERNA
199800960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
199900950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
200000950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
200100950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
200200950110     C*
200300950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
200400950110     C                   MOVEL     'P'           WTC                            *PARTENZA
200500950110     C                   MOVEL     'C'           WIC                            *CLIENTE
200600950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
200700170104     C* SE TEMPO PARTENZA =0 sommo comunque 1 ore per slittare dal
200800170104     c*  festivo se fosse data festiva la data sped e non bortderizzata
200900170104     c*  in questa data
201000170104     c                   if        d98tpc=0
201100170104     c                   z-add     1             WR4NOI
201200170104     c                   else
201300950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
201400170104     c                   endif
201500950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
201600950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
201700170104     c
201800170104     c                   if        d98tpc=0
201900170104     c                   sub       1             wpoppc
202000170104     c                   endif
202100960229     C*
202200960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
202300050922     c
202400050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
202500050922     c
202600050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
202700090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
202800050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
202900090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
203000050922%%%  C*  ritardo alla partenza.
203100050922     c* Il tutto solo se la data di oggi > della
203200050922     c*  data teorica di partenza determinata
203300050922     c*
203400950110%%%  C     D98ERR        IFEQ      '0'
203500950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
203600950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
203700950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
203800050922     c     wpoattData24  andgt     wpoppi
203900960911%%%  C********             MOVEL'OSC  '   WR3TAB
204000960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
204100960911%%%  C********             EXSR RO3ORE
204200960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
204300050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
204400090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
204500090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
204600950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
204700050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
204800050922     c* La nuova data calcolata non può essere < della minima prevista
204900050922     c                   if        wr0poo>wpoppi
205000050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
205100050922     c                   endif
205200050922     c
205300950110%%%  C                   ENDIF
205400950110%%%  C                   ENDIF
205500950110%%%  C*
205600950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
205700050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
205800050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
205900050915%%%  C*  alla prtenza.
206000950110%%%  C     D98ERR        IFEQ      '0'
206100950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
206200950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
206300950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
206400950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
206500950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
206600950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
206700050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
206800950110%%%  C                   ENDIF
206900950110%%%  C                   ENDIF
207000950110%%%  C*
207100950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
207200950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
207300950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
207400950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
207500950110%%%  C     D98ERR        IFEQ      '0'
207600950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
207700950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
207800950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
207900950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
208000090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
208100090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
208200950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
208300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
208400950110%%%  C                   ENDIF
208500950110%%%  C                   ENDIF
208600960229     C*
208700960229     C                   ENDIF                                                  <-- 1 ---
208800950110     C*
208900950110     C                   ENDSR
209000930604     C********************************************************************
209100930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
209200930604     C********************************************************************
209300930604     C     CALDPT        BEGSR
209400090902     c                   clear                   RicalcoloTRAZ     1
209500930604     C*
209600930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
209700931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
209800931129     C                   MOVEL     'I'           WIC                            *INTERNA
209900050915     c* se non presente la data partenza ultimo collo, uso la teorica
210000950110     C     D98DUC        IFEQ      *ZEROS
210100950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
210200950110     C                   ELSE
210300950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
210400950110     C                   ENDIF
210500930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
210600930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
210700930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
210800930604     C*
210900930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
211000931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
211100931129     C                   MOVEL     'C'           WIC                            *CLIENTE
211200950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
211300930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
211400930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
211500930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
211600150508
211700150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
211800150508     c* se presente la data partenza PRIMO  collo --> la uso
211900150508     c                   if        w98dpc=0
212000150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
212100150508     c                   else
212200150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
212300150508     C                   MOVEL     'C'           WIC                            *INTERNA
212400150508     C                   Z-ADD     W98DPC        WDAT
212500150508     C                   EXSR      DATORE
212600150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
212700150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
212800150508     C                   EXSR      RO4CDP
212900150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
213000150508     c                   endif
213100960229     C*
213200960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
213300940624%%%  C*
213400050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
213500050915
213600940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
213700050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
213800050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
213900050915     c*   attribuito alla trazione
214000940624%%%  C     D98ERR        IFEQ      '0'
214100940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
214200940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
214300090903     c     wpoatt        andge     wpopti
214400050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
214500090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
214600090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
214700940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
214800050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
214900050922     c                   if        wr0poo>wpopti
215000050922     c                   eval      wpopti=wr0poo
215100050922%%%  C                   endif
215200090902     c*
215300090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
215400090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
215500090902     c*  della trazione udate e utime
215600090902     c                   eval      RicalcoloTRAZ='S'
215700090902     c
215800940624%%%  C                   ENDIF
215900940624%%%  C                   ENDIF
216000940628%%%  C*
216100050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
216200050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
216300050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
216400940628%%%  C     D98ERR        IFEQ      '0'
216500940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
216600940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
216700940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
216800090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
216900090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
217000940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
217100050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
217200940628%%%  C                   ENDIF
217300940628%%%  C                   ENDIF
217400960229     C*
217500960229     C                   ENDIF                                                  <-- 1 ---
217600930604     C*
217700930604     C                   ENDSR
217800930511     C********************************************************************
217900930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
218000930511     C********************************************************************
218100930511     C     CALDCR        BEGSR
218200050914     c                   clear                   consideraora
218300100308     c
218400100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
218500100308     c*
218600100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
218700100308     c* 1) se presente solo ORA --> ignoro sempre
218800100308     c* 2) se presente data e ora IL --> ignoro l'ora
218900100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
219000100309     c***                if        d98tsp='E'
219100100309     c***                clear                   d98dcr
219200100309     c***                clear                   d98hcr
219300100309     c**+                clear                   d98tcr
219400100309     c***                endif
219500100308     c
219600100308     c                   if        d98hcr>0
219700100308     c
219800100308     c                   if        d98dcr=0
219900100308     c                   clear                   d98hcr
220000100308     c                   endif
220100100308
220200100308     c                   if        d98dcr>0 and d98tcr=' '
220300100308     c                   clear                   d98hcr
220400100308     c                   endif
220500100308     c                   endif
220600941228     C*
220700941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
220800941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
220900050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
221000050901     c*  (per uguale si intende entrambe nel mattino o entrambe
221100050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
221200050901     c*  (EDPES 1/9/05)
221300050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
221400050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
221500050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
221600050831     C***                Z-ADD     D98DCM        D98DCR
221700050831     C***                ENDIF
221800050901     c*
221900070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
222000050914     c
222100100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
222200100308     c***                clear                   d98hcr
222300100308     c***                clear                   d98tcr
222400100308     c***                endif
222500050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
222600050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
222700070521     c*  richiesta dal cliente
222800100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
222900100308     C***                MOVEL     'OSC  '       WR3TAB
223000100308     C***                Z-ADD     D98HMC        WR3HHI
223100100308     C***                EXSR      RO3ORE
223200100308     c***                z-add     wr3hho        salvaoreHMC
223300050901     c
223400100308     C***                Z-ADD     D98Hcr        WR3HHI
223500100308     C***                EXSR      RO3ORE
223600100308     c***                z-add     wr3hho        salvaoreHCR
223700100308     c***
223800050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
223900100308    2c***                if        salvaoreHMC=salvaoreHCR
224000100308     C***                eval      Consideraora='S'
224100100308     c***                else
224200100308     C***                eval      Consideraora=' '
224300100308     c***                clear                   d98hcr
224400100308    2c***                endif
224500100308    1c***                endif
224600930511     C*
224700930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
224800931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
224900931125     C                   Z-ADD     D98DCR        WDAT
225000930325     C                   EXSR      DATORE
225100930326     C                   Z-ADD     WORE          WPODCR
225200930420     C*
225300930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
225400930420     C*
225500931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
225600930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
225700930326     C                   MOVEL     'CRD  '       WR3TAB
225800931125     C                   Z-ADD     D98HCR        WR3HHI
225900930419     C                   EXSR      RO3ORE
226000930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
226100940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
226200050902     c
226300931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
226400930420     C                   MOVEL     'CRD1 '       WR3TAB
226500930420     C                   Z-ADD     *ZEROS        WR3HHI
226600930420     C                   EXSR      RO3ORE
226700931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
226800090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
226900090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
227000050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
227100060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
227200050907     c                   z-add     worecr        oreILoDOPO
227300931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
227400931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
227500931207     C                   MOVEL     'C'           WIC                            *CLIENTE
227600931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
227700150330     C                   Z-ADD     WORECR        WR4NOI
227800931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
227900931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
228000931209     C     WPOCRF        SUB       12            WR7POI            9 0
228100931209     C                   MOVEL     'X'           WIC
228200931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
228300931209     C                   ADD       12            WR7POO
228400931207     C     WR7POO        IFGT      WPOCRF
228500931207     C                   Z-ADD     WR7POO        WPOCRF
228600931207     C                   END
228700150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
228800150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
228900150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
229000150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
229100150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
229200150330     C                   MOVEL     'C'           WIC                            *CLIENTE
229300150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
229400150330     C                   Z-ADD     w99TCC        WR4NOI
229500150330     c                   sub       12            wr4noi
229600150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
229700150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
229800150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
229900150330     c
230000150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
230100150330     C                   MOVEL     'X'           WIC
230200150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
230300150330     C******             ADD       12            WR7POO
230400150330     C     WR7POO        IFGT      WPOCRF_dee
230500150330     C                   Z-ADD     WR7POO        WPOCRF_dee
230600150330     C                   END
230700050902     c
230800050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
230900050902     c*  sono le ore di fine della non consegnabilità
231000050905     c* Se aggiungendo 1 finisco in giorno festivo
231100050905     c*  cerco primo giorno non festivo
231200050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
231300050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
231400050907     C****               MOVEL     'C'           WIC                            *CLIENTE
231500050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
231600050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
231700050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
231800050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
231900050907     c
232000050907     c                   add       1             wpocri
232100050902     c
232200050902   FFC                   Z-ADD     WPOCRI        WPOCRC
232300050902     c
232400930325     C                   END
232500150330     C*
232600931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
232700930420     C* DATA SPED. = DATA INIZIO RANGE
232800930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
232900940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
233000940324£$   C                   Z-ADD     WPOCRI        WPOCRC
233100930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
233200930420     C                   MOVEL     'CRP  '       WR3TAB
233300931125     C                   Z-ADD     D98HCR        WR3HHI
233400930420     C                   EXSR      RO3ORE
233500931207     C     WPODCR        ADD       WR3HHO        WPOCRF
233600931207     C                   SUB       24            WPOCRF                         *FINE RANGE
233700931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
233800931207     C* ANDANDO INDIETRO NEL TEMPO
233900050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
234000050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
234100050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
234200050907     c* data utile venedì sera
234300130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
234400130411     C                   MOVEL     'X'           WIC                            *CLIENTE
234500931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
234600931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
234700931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
234800930326     C                   END
234900930526     C*
235000931125     C     D98TCR        IFEQ      'P'
235100931125     C     D98TCR        OREQ      'D'
235200930526     C                   ELSE                                                   *CONS.RICH.IL ..
235300940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
235400940324£$   C                   Z-ADD     WPODCR        WPOCRC
235500930526     C* DATA SPED. = DATA INIZIO RANGE
235600050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
235700050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
235800050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
235900930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
236000930526     C                   MOVEL     'CRI  '       WR3TAB
236100931125     C                   Z-ADD     D98HCR        WR3HHI
236200930526     C                   EXSR      RO3ORE
236300050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
236400050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
236500050907     c                   z-add     wr3hho        oreILoDOPO
236600930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
236700050902     c
236800050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
236900050902     c                   if        wr3MATPOM='P' and d98hcr>0
237000050902     c                   add       12            wpocri
237100100308     c* tolgo 12 ore dalle ore conseganbili
237200050907     c                   sub       12            oreILoDOPO
237300050902     c                   endif
237400050902     c
237500931209     C     WPOCRF        SUB       12            WR7POI            9 0
237600931209     C                   MOVEL     'X'           WIC
237700931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
237800931209     C                   ADD       12            WR7POO
237900931209     C     WR7POO        IFGT      WPOCRF
238000931209     C                   Z-ADD     WR7POO        WPOCRF
238100931209     C                   END
238200050905     c
238300050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
238400050905     c*  sono le ore di fine della non consegnabilità
238500050905     c* Se aggiungendo 1 finisco in giorno festivo
238600050905     c*  cerco primo giorno non festivo
238700050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
238800050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
238900050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
239000050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
239100050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
239200050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
239300050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
239400050907     c
239500050907     c                   add       1             wpocri
239600050905     c
239700050905   FFC                   Z-ADD     WPOCRI        WPOCRC
239800050905     c
239900930526     C                   END
240000931206     C*
240100931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
240200931206     C* LA IMPOSTO COME LA CONS.TEORICA
240300140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
240400140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
240500931206     C                   ELSE
240600931206     C                   Z-ADD     WPOCRF        WPOCR1
240700931206     C                   END
240800150608     c* ai fini del calcolo affidabilità
240900150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
241000150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
241100931206     C                   ELSE
241200931206     C                   Z-ADD     WPOCRF        WPOCR2
241300931206     C                   END
241400150608     c* ai fini della data prevista consegna cliente
241500150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
241600150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
241700150608     C                   ELSE
241800150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
241900150608     C                   END
242000150331     c
242100150331     c                   if        wpocrf_dee>0
242200150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
242300150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
242400150330     C                   END
242500150331     C                   END
242600150331     C*
242700150331     c                   if        wpocrf_dee=0
242800150608     C     wpocr2_dee    sub       24            WPOCRF_dee
242900150331     c                   endif
243000150331     c
243100930526     C                   END                                                    <-- 1 ---
243200930511     C*
243300930511     C                   ENDSR
243400930511     C********************************************************************
243500930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
243600930511     C********************************************************************
243700930511     C     CALLAV        BEGSR
243800930511     C*
243900941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
244000930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
244100930421     C*
244200930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
244300930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
244400930421     C     1             DO        IE            E                 3 0          --- 2 -->
244500930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
244600930421     C                   Z-ADD     1             I
244700930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
244800060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
244900930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
245000930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
245100930421     C                   ADD       1             IL
245200930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
245300930421     C                   EXSR      DATORE
245400930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
245500091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
245600091014     c* Mi salvo la posizione dell'evento
245700091014     C                   Z-ADD     E             LAp(IL)
245800930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
245900930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
246000930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
246100930421     C                   EXSR      RO3ORE
246200930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
246300930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
246400930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
246500930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
246600091015     c**                 if        §2agla>1
246700091015     c**                 sub       1             §2agla
246800930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
246900091015     c**                 else
247000091015     c**                 clear                   whh
247100091015     c**                 endif
247200930510     C                   ADD       WHH           WORELA
247300940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
247400930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
247500090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
247600150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
247700930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
247800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
247900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
248000930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
248100930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
248200930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
248300930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
248400931209     C                   Z-ADD     LAF(IL)       WR7POI
248500931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
248600931202     C     WR7POO        IFGT      LAF(IL)
248700931201     C                   Z-ADD     WR7POO        LAF(IL)
248800931202     C                   END
248900930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
249000930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
249100091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
249200930510     C                   END                                                    <-- 3 ---
249300931223     C                   ENDDO                                                  <-- 2 ---
249400930421     C*
249500930421     C                   END                                                    <-- 1 ---
249600930618££$$ C*
249700930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
249800931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
249900941228££$$ C     1             DO        IL            I
250000930618££$$ C     LAF(I)        IFGT      WPOLAF
250100930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
250200930618££$$ C                   END
250300931223££$$ C                   ENDDO
250400930511     C*
250500930511     C                   ENDSR
250600940324£$   C********************************************************************
250700940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
250800940324£$   C********************************************************************
250900940324£$   C     CALLAC        BEGSR
251000941229     C*
251100950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
251200950407     C*
251300950210     C                   Z-ADD     WORELA        WTM060
251400941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
251500941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
251600941229     C     D98DAG        IFEQ      *ZEROS
251700050520     c     §2aftg        andeq     'S'
251800090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
251900090827     C     WORELA        ADD       w99TCC        WTM060            6 0
252000160404     c                   if        wVP='S'
252100160404     c                   sub       24            wTM060
252200160404     c                   endif
252300941229     C                   ENDIF
252400941229     C*
252500940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
252600940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
252700941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
252800941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
252900941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
253000940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
253100941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
253200050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
253300050520     c                   movel     §2aftg        LCON(IL)
253400950407     C*
253500950407     C                   ENDIF
253600960129     C*
253700940324£$   C                   ENDSR
253800930511     C********************************************************************
253900930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
254000930511     C********************************************************************
254100930511     C     CALGIA        BEGSR
254200960129     C*
254300960129     C*-------------------------------------------------------------------
254400960129     C* EVENTI GIACENZA
254500960129     C*-------------------------------------------------------------------
254600960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
254700960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
254800960129     C*
254900050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
255000041116     c     d98nsp        ifgt      0
255100031125     C                   EXSR      CARGIAC
255200041116     c                   endif
255300031125     C*
255400960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
255500960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
255600960129     C     1             DO        IE            E                 3 0          --- 2 -->
255700960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
255800960129     C                   Z-ADD     1             I
255900960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
256000060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
256100960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
256200960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
256300960129     C                   ADD       1             IG
256400960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
256500960129     C                   EXSR      DATORE
256600960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
256700140606     c* Mi salvo la posizione dell'evento
256800140606     C                   Z-ADD     E             gip(IG)
256900960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
257000960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
257100960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
257200960129     C                   EXSR      RO3ORE
257300960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
257400960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
257500960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
257600960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
257700960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
257800960129     C                   ADD       WHH           WOREGI
257900960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
258000960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
258100090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
258200090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
258300160404     c                   if        wVP='S'
258400160404     c                   sub       24            wOREGI
258500160404     c                   endif
258600960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
258700960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
258800960129     C                   MOVEL     'C'           WIC                            *CLIENTE
258900960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
259000960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
259100960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
259200960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
259300960129     C                   Z-ADD     GIF(IG)       WR7POI
259400960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
259500031125     C     WR7POO        IFGT      GIF(IG)
259600031125     C                   Z-ADD     WR7POO        GIF(IG)
259700031125     C                   ENDIF
259800031201     C*
259900031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
260000031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
260100031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
260200031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
260300031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
260400031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
260500031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
260600031201     C                   IF        WPOATT > GIF(IG)
260700031201     C*
260800031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
260900031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
261000031201     C                   IF        WGGA = *BLANKS
261100031201     C                   SETOFF                                       54
261200031201     C                   ELSE
261300031201     C     WGGA          LOOKUP    SK7QW                                  54
261400031201     C                   ENDIF
261500031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
261600031201     C                   MOVEL     '1'           FLGGIACOK
261700031201     C                   ELSE
261800140127     c
261900031125     C*
262000031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
262100031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
262200031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
262300031125     C                   MOVEL     '0'           FLGGIACOK         1
262400031125     C*
262500031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
262600031125     C     1             DO        IAG           EG                3 0
262700031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
262800031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
262900031125     C                   EXSR      DATORE
263000031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
263100031125     C*
263200031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
263300031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
263400031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
263500031125     C                   EXSR      DATORE
263600031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
263700031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
263800031229     C* => NN LO CONSIDERO
263900031229     C                   IF        EVC(E+1) = 'G' AND
264000031229     C                             WPROXGII = WGCPDORE
264100031229     C                   MOVEL     '1'           FLGGIACOK
264200031229     C                   ENDIF
264300031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
264400031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
264500031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
264600031125     C                   EXSR      RO3ORE
264700031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
264800031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
264900031125     C* DELL'EVENTO GIACENZA CORRENTE
265000031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
265100060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
265200031125     C                   MOVEL     '1'           FLGGIACOK
265300031125     C                   LEAVE
265400031125     C                   ENDIF
265500031125     C                   ELSE
265600031229     C                   IF        EVC(E)  = 'G' AND
265700031229     C                             GII(IG) = WGCPDORE
265800031229     C                   MOVEL     '1'           FLGGIACOK
265900031229     C                   LEAVE
266000031229     C                   ENDIF
266100031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
266200031125     C                   MOVEL     '1'           FLGGIACOK
266300031125     C                   LEAVE
266400031125     C                   ENDIF
266500031125     C                   ENDIF
266600031125     C                   ENDDO
266700031201     C                   ENDIF
266800140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
266900140127     C                   IF        FLGGIACOK = '0'  and
267000140127     c                             d98dcm=evd(E)
267100140127     C                   MOVEL     '1'           FLGGIACOK
267200140127     c                   endif
267300031125     C*
267400031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
267500031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
267600031125     C* DELL'EVENTO GIACENZA CORRENTE
267700031125     C                   IF        FLGGIACOK = '0'
267800031125     C                   Z-ADD     *ZEROS        GIF(IG)
267900031125     C                   ELSE
268000031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
268100031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
268200031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
268300031125     C                   ENDIF
268400031125     C                   ELSE
268500031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
268600031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
268700031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
268800031125     C                   ENDIF
268900031125     C*
269000960129     C                   ENDIF                                                  <-- 3 ---
269100960129     C                   ENDDO                                                  <-- 2 ---
269200960129     C*
269300960129     C                   ENDIF                                                  <-- 1 ---
269400960129££$$ C*
269500960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
269600960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
269700960129££$$ C     1             DO        IG            I
269800960129££$$ C     GIF(I)        IFGT      WPOGIF
269900960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
270000960129££$$ C                   ENDIF
270100960129££$$ C                   ENDDO
270200941229     C*
270300941229     C*-------------------------------------------------------------------
270400941229     C* APERTURA GIACENZA
270500941229     C*-------------------------------------------------------------------
270600960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
270700960129     C*
270800960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
270900931125     C                   Z-ADD     D98DAG        WDAT
271000930325     C                   EXSR      DATORE
271100950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
271200960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
271300950413£££  C                   Z-ADD     WORE          WPODAG
271400960129$$£  C*********************ENDIF
271500930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
271600930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
271700930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
271800930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
271900930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
272000941229     C*
272100941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
272200941229     C                   Z-ADD     WPODAG        WPOAGC
272300960129     C*
272400960129     C                   ENDIF                                                  <-- 1 ---
272500941229     C*
272600941229     C*-------------------------------------------------------------------
272700930511     C* CHIUSURA GIACENZA
272800941229     C*-------------------------------------------------------------------
272900930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
273000930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
273100931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
273200931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
273300931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
273400931201     C                   Z-ADD     D98DCM        W98DCG
273500930416     C                   ELSE
273600930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
273700930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
273800931201     C                   Z-ADD     WDAODI        W98DCG
273900061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
274000061123     c*  chiusura giacenza, come già fa in altri posti
274100061123     c                   if        d98dfc>0
274200061123     C                   Z-ADD     d98dfc        W98DCG
274300061123     c                   endif
274400061123     c
274500930409     C                   END
274600930409     C                   END
274700930409     C                   END
274800930414     C* TRASF.DATA CHIUSURA
274900931201     C     W98DCG        IFNE      *ZEROS
275000931201     C                   Z-ADD     W98DCG        WDAT
275100930325     C                   EXSR      DATORE
275200930326     C                   Z-ADD     WORE          WPODCG
275300930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
275400930407     C* GIORNO DA ESCLUDERE
275500150216     C                   ADD       12            WPODCG
275600150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
275700150123     c*  il giorno di chiusura dovrebe già essere protetto
275800941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
275900941229     C     WPOCGC        IFEQ      *ZEROS
276000941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
276100941229     C                   ENDIF
276200930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
276300930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
276400931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
276500931129     C                   MOVEL     'C'           WIC                            *CLIENTE
276600150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
276700150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
276800160404     c                   if        wVP='S'
276900160404     c                   sub       24            wr4NOI
277000160404     c                   endif
277100930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
277200930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
277300930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
277400931209     C                   Z-ADD     WPODCG        WR7POI
277500931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
277600931202     C     WR7POO        IFGT      WPODCG
277700931201     C                   Z-ADD     WR7POO        WPODCG
277800930326     C                   END
277900931202     C                   END
278000930511     C*
278100930511     C                   ENDSR
278200031125     C********************************************************************
278300031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
278400031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
278500031125     C*             DEGLI EVENTI DI TIPO GIACENZA
278600031125     C********************************************************************
278700031125     C     CARGIAC       BEGSR
278800031125     C*
278900031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
279000031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
279100031125     C*
279200050405     C                   Z-ADD     D98AAS        KWGCPAAS
279300050405     C                   Z-ADD     D98LNP        KWGCPLNP
279400050405     C                   Z-ADD     D98NRS        KWGCPNRS
279500050405     C                   Z-ADD     D98NSP        KWGCPNSP
279600031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
279700050405     C     KEYGC_P       SETLL     TIGCP01L
279800050405     C                   IF        %equal(TIGCP01L)
279900050405     C     KEYGC_P       READE     TIGCP01L
280000050405     C                   DOW       not %eof(TIGCP01L)
280100031125     C                   ADD       1             IAG
280200031125     C                   EVAL      GCPD(IAG) = GCPDUR
280300050405     C     KEYGC_P       READE     TIGCP01L
280400031125     C                   ENDDO
280500031125     C                   ENDIF
280600031125     C                   ELSE
280700050405     C     KEYGC_P       SETLL     TIGCP21L
280800050405     C                   IF        %equal(TIGCP21L)
280900050405     C     KEYGC_P       READE     TIGCP21L
281000050405     C                   DOW       not %eof(TIGCP21L)
281100031125     C                   ADD       1             IAG
281200031125     C                   EVAL      GCPD(IAG) = GCPDUR
281300050405     C     KEYGC_P       READE     TIGCP21L
281400031125     C                   ENDDO
281500031125     C                   ENDIF
281600031125     C                   ENDIF
281700031125     C*
281800031125     C                   ENDSR
281900941229     C********************************************************************
282000960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
282100941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
282200941229     C********************************************************************
282300960129     C     CALGIC        BEGSR
282400941229     C*
282500960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
282600960129     C*
282700960129     C                   Z-ADD     WOREGI        WTM060
282800960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
282900960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
283000960129     C     D98DAG        IFEQ      *ZEROS
283100960129$$£  C     WOREGI        ADD       12            WTM060            6 0
283200960129     C                   ENDIF
283300960129     C*
283400960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
283500960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
283600960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
283700960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
283800960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
283900960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
284000960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
284100960129     C*
284200960129     C                   ENDIF
284300941229     C*
284400941229     C                   ENDSR
284500060418     C********************************************************************
284600060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
284700060418     c*            l'affidabilità
284800060418     C********************************************************************
284900060418     C     CALCEPA       BEGSR
285000060418     C*
285100060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
285200060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
285300060420     c                   clear                   numggcepa         3 0
285400060418     C*
285500060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
285600060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
285700060420     c                   eval      wgiroepa='0'
285800060418     c                   eval      wfine=' '
285900060418     c
286000060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
286100060420    2C     W             DOwle     IP                                           --- 2 -->
286200060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
286300060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
286400060418     c
286500060420    4c                   if        wgiroepa='0'
286600060420     c                   exsr      ElaInizCEPA
286700060418   x4c                   else
286800060420    5c                   if        pvd(W)>wdat
286900060418     c                   eval      wfine='S'
287000060418    5c                   endif
287100060418    4c                   endif
287200060418     c
287300060420    4c                   if        wfine=' ' and pvd(W)=wdat
287400060420     C*
287500060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
287600060420     c*  finale
287700060420     c                   add       1             numggcepa
287800060420     c
287900060420     C                   z-add     24            WORELA
288000060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
288100060418     c
288200060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
288300060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
288400060418     C                   MOVEL     'C'           WIC                            *CLIENTE
288500060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
288600060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
288700060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
288800060418     c
288900060418     c* Trasformo le ore in data e verifico se c'e' altro evento
289000060418     c*  in sta data che allunga ancora
289100060420     c                   z-add     wr4poo        wore
289200060418     c                   EXSR      OREDAT
289300060418    4c                   endif
289400060418     c
289500060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
289600060418    4c                   if        wfine='S'
289700060418     c                   EXSR      ELAFINECEPA
289800060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
289900060420     c*  inizio range
290000060420     c                   else
290100060420     c                   add       1             w
290200060418    4C                   ENDif                                                  <-- 3 ---
290300060420     c
290400060420     c                   else
290500060420     c                   add       1             w
290600060418    3C                   ENDif                                                  <-- 3 ---
290700060418    2C                   ENDDO                                                  <-- 2 ---
290800060418     c
290900060420     c                   if        wgiroepa='1'
291000060418     c                   EXSR      ELAFINECEPA
291100060418     c                   endif
291200060418     C*
291300060418    1C                   END                                                    <-- 1 ---
291400060418££$$ C*
291500060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
291600060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
291700060420££$$ C     1             DO        Ipa           w
291800060420££$$ C     paif(w)       IFGT      WPOpaif
291900060420££$$ C                   Z-ADD     paif(w)       WPOpaif
292000060418££$$ C                   END
292100060418££$$ C                   ENDDO
292200060418     C*
292300060418     C                   ENDSR
292400060420     C********************************************************************
292500060420     c* Elabora Inizio range eventi particolari che allungano
292600060420     C********************************************************************
292700060420     c     ElaInizCEPA   BEGSR
292800060420     C                   ADD       1             IPA
292900060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
293000060420     C                   EXSR      DATORE
293100060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
293200061017     c
293300060420     c                   eval      wgiroepa='1'
293400060420     c                   eval      wdat=pvd(w)
293500060420     C                   ENDSR
293600060418     C********************************************************************
293700060418     c* Elabora fine range eventi particolari che allungano
293800060418     C********************************************************************
293900060418     c     ElafineCEPA   BEGSR
294000060420     c     numggcepa     mult      24            worela
294100060420     c                   sub       12            worela
294200090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
294300090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
294400160404     c                   if        wVP='S'
294500160404     c                   sub       24            WORELA
294600160404     c                   endif
294700060418     c
294800060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
294900060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
295000060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
295100060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
295200060418     C                   Z-ADD     paif(IPA)     WR7POI
295300060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
295400060418    5C     WR7POO        IFGT      paif(IPA)
295500060418     C                   Z-ADD     WR7POO        paif(IPA)
295600060418    5C                   END
295700061017     c*Sommo 24 ore di inizio giornata
295800061017     c*            : non ho capito perchè ma ci vogliono
295900061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
296000090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
296100090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
296200160404     c                   if        wVP='S'
296300160404     c                   sub       24            paii(IPA)
296400160404     c                   endif
296500061017     c* Se tempi di consegna inferiori alle 24 ore , devo
296600061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
296700061017     c*  partire prima
296800061017     c****               if        d98tcc<24
296900061017     c****               sub       12            paii(ipa)
297000061017     c****               endif
297100060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
297200060418     c* Imposto nuovo giro
297300060420     c                   eval      wgiroepa='0'
297400060418     c                   eval      wfine=' '
297500060420     c                   clear                   numggcepa         3 0
297600060418     c                   ENDSR
297700930511     C********************************************************************
297800930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
297900930511     C********************************************************************
298000010822     C     CALASS        BEGSR
298100010821     C*
298200011211     C                   EXSR      REPMAXCSB
298300930511     C*
298400930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
298500010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
298600011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
298700930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
298800930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
298900930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
299000930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
299100930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
299200930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
299300930511     C                   ELSE                                                   <-- 4 -->
2994009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
299500930512     C                   END                                                    <-- 4 ---
299600930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
299700930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
299800930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
299900930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
300000930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
300100930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
300200930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
300300930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
300400930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
300500930413     C                   END                                                    <-- 2 ---
300600011211     C                   END                                                    <-- 1 ---
300700930406     C*
300800930325     C                   ENDSR
300900930608     C********************************************************************
301000930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
301100930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
3012009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
301300930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
301400930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
301500930608     C********************************************************************
301600930608     C     RDRASS        BEGSR
301700930608     C*
301800930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
3019009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
3020009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
3021009407151507 C*********************EXSR RO3ORE
3022009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
302300930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
3024009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
3025009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
3026009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
3027009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3028009407191907 C                   Z-ADD     WPOASI        WPOROU
3029009407191907 C                   EXSR      CHKFES
3030009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3031009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
3032009407191907 C                   ELSE                                                   *E' FERIALE
3033009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
3034009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3035009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
3036009407191907 C                   ENDIF
303700930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
303800930608     C                   MOVEL     'ASS1 '       WR3TAB
303900930608     C                   Z-ADD     *ZEROS        WR3HHI
304000930608     C                   EXSR      RO3ORE
304100930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
304200930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
304300931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
304400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
304500930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
304600930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
304700930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
304800930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
304900930608     C*
305000930608     C                   ENDSR
305100930511     C********************************************************************
305200930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
305300930511     C********************************************************************
305400930511     C     CALFED        BEGSR
305500150331     c                   clear                   fddata
305600150331     c                   clear                   WPOFDdata
305700930511     C*
305800051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
305900931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
306000051108     C     D98FFD        oreq      'M'                                          --- 1 -->
306100930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
306200930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
306300930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
306400930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
306500150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
306600150331     c                   if        §ar5dfd>*zeros
306700150331     c                   movel     §ar5dfd       fddata            8 0
306800150331     c                   endif
306900150331     c                   if        fddata>0 and ((d98dti>0 and
307000150331     c                             fddata>d98dti) or d98dti=0)
307100150331     C                   Z-ADD     fddata        WDAT
307200150331     C                   EXSR      DATORE
307300150331     C                   Z-ADD     WORE          WPOfddata
307400150331     c                   endif
307500150331     c
307600930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
307700930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
307800930511     C                   ELSE                                                   <-- 2 -->
3079009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
308000930512     C                   END                                                    <-- 2 ---
308100150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3082001504081507 C                   Z-ADD     WPOfddata     WPOFDI
308300150331     c                   endif
308400150331
308500930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
308600930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
308700930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
308800930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
308900930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
309000930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
309100930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
309200930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
309300930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
309400930511     C                   END                                                    <-- 1 ---
309500930511     C*
309600930511     C                   ENDSR
309700930608     C********************************************************************
309800930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
309900930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3100009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
310100930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
310200930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
310300930608     C********************************************************************
310400930608     C     RDRFED        BEGSR
310500930608     C*
310600930608     C* CALCOLO ORE DEL FERMO DEPOSITO
310700930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3108009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3109001503311907 C                   Z-ADD     WPOFDI        WPOROU
3110001503311907 C                   EXSR      CHKFES
3111001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3112001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3113001503311907 C                   ELSE                                                   *E' FERIALE
3114001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3115001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3116001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3117001503311907 C                   ENDIF
311800150331     c* Se presente la particolarità consegna con più giorni di fermo depo
311900150331     c*  uso questi
312000150331     c                   if        §7rggfd>*zeros
312100150424     c                   movel     §7rggfd       wggfd             2 0
312200150331     c     wggfd         mult      24            Wnore
312300150331     c                   add       wnore         worefd
312400150331     c                   else
312500930608     C                   MOVEL     'FED1 '       WR3TAB
312600930608     C                   Z-ADD     *ZEROS        WR3HHI
312700930608     C                   EXSR      RO3ORE
312800940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
312900150331     c                   endif
313000930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
313100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
313200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
313300930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
313400930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
313500930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
313600930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
313700930608     C*
313800930608     C                   ENDSR
313900931201     C********************************************************************
314000931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
314100931201     C********************************************************************
314200931201     C     CALDTE        BEGSR
314300140825     c
314400140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
314500140825     c*  che tiene conto solo dei festivi per restituire in output
314600140825     c*  la data teorica di consegna
314700140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
314800140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
314900140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
315000140825     C                   ELSE
315100140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
315200140825     C                   END
315300140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
315400141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
315500141106     c*  quindi passo un flag
315600141106     c                   eval      nosomma='N'
315700140825     C                   EXSR      RO4CDP
315800141106     c                   eval      nosomma=' '
315900140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
316000140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
316100140825     c*  di consegna
316200140825     c                   EXSR      TRAS_DEI
316300140825     c
316400140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
316500140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
316600140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
316700140825     c                             ='M' or %subst(d98gc2:2:1)='P')
316800140825     c                   exsr      Reper_tempi
316900140825     c                   endif
317000931201     C*
317100140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
317200140825     c*  che tiene conto di:
317300140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
317400150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
317500931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
317600931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
317700931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
317800931201     C                   ELSE
317900931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
318000931201     C                   END
318100090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
318200090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
318300931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
318400140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
318500931201     C*
318600150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
318700931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
318800931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
318900090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
319000931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
319100150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
319200150608     c
319300150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
319400150608     c*  che non tiene conto dei tempi del C/Ass
319500150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
319600150609     c*
319700150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
319800150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
319900150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
320000150630     c                   Z-ADD     WPODTI        WR6POI
320100150609     c                   else
320200150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
320300150609     c                   endif
320400150609     c
320500150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
320600150608     c                   eval      wdee='S'
320700150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
320800150608     c                   eval      wdee=' '
320900150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
321000141017
321100141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
321200150219     C**                 EVAL      WTC='C'
321300150219     C**                 EVAL      WIC='C'
321400150219     c**                 z-add     wpoattdata00  WPOROU
321500150219     c**                 z-add     wdaodi        wpodata
321600150219     c**                 exsr      chkfes
321700150219     c**                 if        wslitt='S'
321800150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
321900150219     c**                 z-add     24            wr6noi
322000150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
322100150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
322200150219     c**                 else
322300150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
322400150219     c**                 endif
322500141204     c* Ricalcolo WPOrifDatLav
322600141229     C**                 MOVEL     'OSC  '       WR3TAB
322700141229     C**                 Z-ADD     WHHODI        WR3HHI
322800141229     C**                 EXSR      RO3ORE
322900141229     c**
323000141229     C**                 EVAL      WTC='C'
323100141229     C**                 EVAL      WIC='C'
323200141229     c**                 z-add     wpoattdata00  WPOROU
323300141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
323400141229     c**                 z-add     wr3hho        wr6noi
323500141229     c**                 add       12            wr6noi
323600141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
323700141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
323800141229     c
323900141017     c
324000141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
324100141017     c*  ma sempre devo slittare i festivi
324200141017     C* CONSEGNABILITA'
324300150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
324400150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
324500150219     C**                 EVAL      WTC='C'
324600150219     C**                 EVAL      WIC='C'
324700150219     c**                 z-add     d98dfc        WPOdata
324800150219     c**                 exsr      chkfes
324900150219     c**                 if        wslitt='S'
325000150219     c**                 z-add     wpodfc_00     Wr6poi
325100150219     c**                 z-add     24            wr6noi
325200150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
325300150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
325400150219     c**                 else
325500150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
325600150219     c**                 endif
325700150219     c**                 endif
325800931201     C*
325900931201     C                   ENDSR
326000930511     C********************************************************************
326100930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
326200930511     C********************************************************************
326300930511     C     CALDPC        BEGSR
326400930325     C*
326500930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
326600930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
326700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
326800930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
326900930325     C                   ELSE
327000930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
327100930407     C                   END
327200090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
327300090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
327400930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
327500090902     c
327600931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
327700090901
327800930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
327900930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
328000930423     C* A QUELLA DI RICHIESTA CONSEGNA,
328100930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
328200931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
328300931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
328400931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
328500931206     C                   END
328600930423     C                   END
328700930325     C*
328800930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
328900930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
329000930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
329100090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
329200090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
329300930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
329400931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
329500930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
329600930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
329700930423     C* A QUELLA DI RICHIESTA CONSEGNA,
329800930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
329900931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
330000931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
330100931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
330200931206     C                   END
330300930423     C                   END
330400140822     C*
330500930324     C                   ENDSR
330600150113     C********************************************************************
330700150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
330800150113     C********************************************************************
330900150113     c     Cal_DEE       BEGSR
331000150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
331100150508     c                   clear                   wtempi00C         1
331200151007     c                   clear                   wtempi12C         1
331300150722     c                   clear                   woreOLP
331400151009     c                   clear                   wdiff
331500150219     c
331600150219     c* 1) Se presente l'arrivo della trazione lo uso
331700150219    1c                   select
331800150219     c                   when      wmancatraz=' '  and d98dti>0
331900150630     c                   Z-ADD     WPODTI        WR1POI
332000150219     c
332100150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
332200150219    1c                   when      IDdata>0
332300150325     c                   exsr      CalcoloIMP
332400151008     c* Passo in output il terminal di disguido
332500151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
332600151008     c                   eval      d98flp=%editc(LTP(2):'X')
332700151008     c                   endif
332800151008
332900150528     c* stato IMP/disguido solo se >  alla data di oggi
333000151006     c                   if        wr1poi > wpoattdata00 and
333100151006     c                             (wr1poi-wpoattdata00)>12
333200150427     c                   eval      d98spcdee='IMP'
333300151008     c                   endif
333400151006     c                   if        wr1poi > wpoattdata00 and
333500151006     c                             (wr1poi-wpoattdata00)=12
333600151006     c                             and w991In>12
333700151006     c                   eval      d98spcdee='IMP'
333800151008     c                   endif
333900150219     c
334000150219     c                   other
334100150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
334200150225     c* devo far finta che sia arrivato.
334300150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
334400150508     c*  che tiene conto della data partenza PRIMO  collo
334500150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
334600150508     c*
334700150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
334800150508     c                   if        wr1poi >=wpoattdata00
334900150508     c                              and WPOPTC_DEE > WPOPTC
335000150508     c                   eval      d98spcdee='IMP'
335100150508     c                   endif
335200150219     c                   endsl
335300150113     c
335400150219     c* se il calcolo è < di oggi --> imposto oggi
335500150113     c                   if        wr1poi < wpoattdata00
335600150113     c                   eval      wr1poi= wpoattdata00
335700150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
335800150508     c                   eval      wTempi00C='S'
335900151009     c                   clear                   wdiff
336000150113     c                   endif
336100170208     c
336200170208     c* Calcolo la data di arrivo reale/teorica dei colli da passare in outout
336300170208     C                   Z-ADD     wr1poi        WORE
336400170208     C                   EXSR      OREDAT
336500170208     C                   Z-ADD     WDAT          D98dam_RTC
336600150217     c
336700150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
336800150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
336900150630     c                   if        wr1poi>wpoattdata00
337000150630     c                   eval      wdiff=wr1poi-wpoattdata00
337100150630     c                   endif
337200151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
337300151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
337400151006     c                   eval      wTempi12C='S'
337500151006     c                   endif
337600160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
337700151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
337800151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
337900160404     c                             and idfgs>0
338000151009     C     wdiff         DIV       24            WDIV
338100151009     C                   MVR                     WRES
338200151009     C                   IF        wres>0
338300151009     c                   eval      wTempi12C='S'
338400151009     c                   else
338500151009     c                   eval      wTempi00C='S'
338600151009     c                   endif
338700151009     c                   endif
338800151009     c
338900151006     c
339000151006     c                   select
339100151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
339200151006     c                             w991in>12 and
339300151006     c                             (deeindist<>'S' or d98cons<>'S'
339400151006     c                             or d98ddc>wdaodi)
339500151006     c
339600151006     c* Se stato IMP lo mantengo per avere messaggio diverso
339700151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
339800151006     c                   if        d98spcdee<>'IMP'
339900151006     c                   eval      d98spcdee='PRV'
340000151006     c                   endif
340100151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
340200150330     c                             (deeindist<>'S' or d98cons<>'S'
340300150330     c                             or d98ddc>wdaodi)
340400150427     c
340500150427     c* Se stato IMP lo mantengo per avere messaggio diverso
340600150427     c                   if        d98spcdee<>'IMP'
340700150304     c                   eval      d98spcdee='PRV'
340800150427     c                   endif
340900150427     c
341000151006   x1c                   other
341100150330     c                   exsr      CAL_OLP
341200150217     c
341300150528     c* Se passato l'orario oppure data distinta < data di oggi
341400150508     c* 1) spedizione in distinta          --> in consegna
341500150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
341600150528     c                             d98ddc<wdaodi)
341700150508    3c                   IF        deeindist='S' and d98cons='S'
341800150508     c                   eval      d98spcdee= 'DDC'
341900150508
342000150508   x3c                   else
342100150507     c
342200150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
342300150722     c*  entro sera
342400150630     c                   if        wr1poi>wpoattdata00
342500150722     c                   z-add     12            woreOLP           3 0
342600150722     c***                add       12            wr1poi
342700150630    4c                   else
342800150507    4c                   if        wr1poi<=wpoattdata00
342900150722     c                   add       24            woreOLP
343000150722     c***                add       24            wr1poi
343100150507    4c                   endif
343200150630    4c                   endif
343300150507     c
343400150722     c****               eval      d98spcdee='OLP'
343500150507     c* memorizzo orario limite picking
343600150507     c                   eval      d98picklim=%editc(picklim:'X')
343700150507    3c                   endif
343800150507    2c                   endif
343900150507     c
344000151006    1c                   endsl
344100150217     c
344200150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
344300150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
344400150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
344500150113     C                   Z-ADD     Wr1poi        W10POI            9 0
344600150113     C                   EXSR      R10FES
344700150113     C                   Z-ADD     W10POO        wr1poi
344800150113     C
344900151006     c                   select
345000151006    1c                   when      wtempi12C='S'
345100151006     c                   z-add     W991IN        wr1noi
345200151006     c                   if        woreOLP>0
345300151006     c                   add       woreOLP       wr1noi
345400151006     c                   eval      d98spcdee='OLP'
345500160404     c                   else
345600160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
345700160404     c**                 if        wVP='S'
345800160404     c**                 add       24            wr1noi
345900160404     c**                 endif
346000151006     c                   endif
346100160404
346200151006    1c                   when      wtempi00C='S'
346300150630     c***                z-add     W990CN        wr1noi
346400150630     c                   z-add     W990IN        wr1noi
346500150722     c
346600150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
346700150722     c*  quindi sottraggo 24
346800151009     c                   if        wr1noi>24  and wdiff=0
346900150722     c                   sub       24            wr1noi
347000150722     c                   endif
347100150722     c
347200150722     c* se passato orario limite picking, slitto in avanti
347300150722     c                   if        woreOLP>0
347400150722     c                   add       woreOLP       wr1noi
347500150722     c                   eval      d98spcdee='OLP'
347600160404     c                   else
347700160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
347800160404     c**                 if        wVP='S'
347900160404     c**                 add       24            wr1noi
348000160404     c**                 endif
348100150722     c                   endif
348200150722     c
348300151006   x1c                   other
348400150722     c
348500150714     c                   if        wmancatraz=' '  and d98dti>0
348600150630     c                   z-add     w99tci        wr1noi
348700150714     c                   else
348800150714     c                   z-add     w99tcc        wr1noi
348900150714     c                   endif
349000160404
349100160401     c
349200150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
349300150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
349400150722     c                   if        woreOLP=24 and wr1noi<=24
349500150722     c                   add       woreOLP       wr1noi
349600150722     c                   eval      d98spcdee='OLP'
349700150722     c                   endif
349800150722     c                   if        woreOLP=12 and wr1noi<24
349900150722     c                   add       woreOLP       wr1noi
350000150722     c                   eval      d98spcdee='OLP'
350100150722     c                   endif
350200150722     c
350300151006    1c                   endsl
350400150629     c
350500150722    1c**                 if        WR1NOI=10
350600150722     c**                 z-add     12            wr1noi
350700150722     c**                 endif
350800150722     c
350900150722     c     wr1noi        div       12            wdiv
351000150722     c                   mvr                     wres
351100150722     c                   if        wres >0
351200150722     c                   add       2             wr1noi
351300150722     c                   endif
351400150722     c
351500150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
351600150629    1c******             endif
351700150508     c
351800150605     c                   eval      wdee='S'
351900150113     C                   EXSR      RO1CDP
352000150605     c                   eval      wdee=' '
352100150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
352200150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
352300150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
352400150113     C* A QUELLA DI RICHIESTA CONSEGNA,
352500150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
352600150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
352700150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
352800150331     c*  2) IL        -> è il giorno indicato
352900150331     C     D98DCR        IFNE      *ZEROS
353000150331     C     D98tcr        andne     'P'
353100150331     C***  WPOdpcee      IFLE      WPOCR2
353200150331     C***  WPOCR2        SUB       12            WPOdpcee
353300150330     c
353400150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
353500150330     c*   dare la prima data utile di consegna
353600150330     C     WPOdpcee      IFLE      WPOCRF_dee
353700150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
353800150330     C                   z-add     WPOCRF_dee    WPOdpcee
353900150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
354000150409     c                   if        wpodpcee>wpoattdata00
354100150409     c                   eval      d98spcdee='PRV'
354200150409     c                   endif
354300150113     C                   END
354400150331     c
354500150220     c
354600150113     C                   END
354700150220     c
354800150113     c                   ENDSR
354900150330     C********************************************************************
355000150330     C*   Calcolo orario da tabella OLP
355100150330     C********************************************************************
355200150330     c     Cal_OLP       BEGSR
355300150330     c                   eval      picklim=0900
355400150330     c* Dopo l'ora limite picking, passo al giorno dopo
355500150330     C                   MOVEL(P)  'OLP'         TBECOD
355600150330     C                   MOVEL(P)  d98lna        TBEKE1
355700150330     C                   MOVEL(P)  wdaodi        TBEKE2
355800150330     C     KTBE          CHAIN     TNTBE01L
355900150330     C                   IF        not %found(TNTBE01L)
356000150330     c                   clear                   tbeke2
356100150330     C     KTBE          CHAIN     TNTBE01L
356200150330     c                   endif
356300150330     C                   IF        not %found(TNTBE01L)
356400150330     C                   MOVEL(P)  '000'         TBEKE1
356500150330     C     KTBE          CHAIN     TNTBE01L
356600150330     c                   endif
356700150330     c
356800150330     C                   IF        %found(TNTBE01L) and
356900150330     c                             %subst(tbeuni:1:4)>*zeros
357000150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
357100150330     c                   endif
357200150330     c                   ENDSR
357300150325     C********************************************************************
357400150325     C*   Riclcolo data se colli in IMP/disguido
357500150325     C********************************************************************
357600150325     c     CalcoloIMP    BEGSR
357700150325     C                   Z-ADD     IDdata        WDAT
357800150325     C                   EXSR      DATORE
357900150325     C                   Z-ADD     WORE          WR4POI
358000150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
358100151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
358200150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
358300150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
358400150325     c* calcolo il terminal di partenza
358500150325     c                   clear                   fnlv55ds
358600150325     c                   eval      d55tla='L'
358700150325     c                   eval      d55tpt='P'
358800150325     c                   eval      d55lin=IDfgs
358900150325     c                   eval      d55drf=IDdata
359000150325     c                   call      'FNLV55R'
359100150325     c                   parm                    fnlv55ds
359200150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
359300150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
359400150325     c                   EXSR      RTTRAZ
359500150325    3c                   if        d98err<>'1'
359600150325     C                   Z-ADD     CASTcn        wr4noi
359700150325     c                   else
359800150325     C                   Z-ADD     d98ttc        wr4noi
359900150325    3C                   endif
360000150325
360100150325   x2c                   else
360200150325     C                   Z-ADD     d98ttc        wr4noi
360300150325    2C                   endif
360400150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
360500150325     c*  perchè partirà un giorno dopo
360600150325    3c                   if        IDnpg='P'
360700150325     c                   add       24            WR4noi
360800150325    3c                   endif
360900150325     c
361000150325     c                   exsr      ro4cdp
361100150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
361200150325     c                   ENDSR
361300930604     C********************************************************************
361400930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
361500930604     C********************************************************************
361600930604     C     TPODAT        BEGSR
361700960229     C*
361800960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
361900960229     C*
362000960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
362100960229     C     WPOPTI        IFEQ      *ZEROS
362200960229     C                   Z-ADD     *ZEROS        D98DTI
362300960229     C                   Z-ADD     *ZEROS        D98HTI
362400960229     C                   ELSE
362500960229     C                   Z-ADD     WPOPTI        WORE
362600960229     C                   EXSR      OREDAT
362700960229     C                   Z-ADD     WDAT          D98DTI
362800960229     C     WRES          MULT      100           D98HTI
362900960229     C                   END
363000960229     C*
363100960229     C                   ELSE                                                   <-- 1 -->
363200930604     C*
363300931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
363400140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
363500140825     c*******            exsr      Tras_DEI
363600140825     c
363700930604     C*
363800150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
363900140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
364000140820     c* è data da :
364100140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
364200140820     c*  Tenere  conto, ovvero slittare in avanti, per :
364300140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
364400140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
364500140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
364600140911     c*
364700140820     c*  Da azzerare nei seguenti casi:
364800150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
364900150324     c* 4) bolla export    7) Cod manca consegna
365000140911     c* 8) bolla con DCR DOPOil o PRIMAdel
365100150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
365200150119     c*10) se completamente non arrivata e collo in IMP
365300150119     c
365400140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
365500150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
365600140925     C*  - FD   - export -
365700150217     c                   movel     'S'           DEEinDIST         1
365800150113     c                   eval      wcaldpcee='S'
365900150304     c                   eval      D98SPCDEE='PRV'
366000150324     c                   clear                   avvcmc            1
366100150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
366200150323     c                   if        d98cmc<>*blanks
366300150324     C                   Z-ADD     1             II
366400150326     C     d98cmc        LOOKUP    C2A(II)                                99
366500150324     C                   MOVEL     D2A(II)       usa2A
366600150324    4C     §2AFTC        IFEQ      'A'
366700150326     c                   eval      avvcmc='A'
366800150323     c                   endif
366900150324    4C     §2AFTC        IFEQ      'G'
367000150326     c                   eval      avvcmc='G'
367100150324     c                   endif
367200150324     c                   endif
367300140820     c
367400140911     c                   z-add     d98lna        EX
367500150113     c*  la azzero nei casi previsti                 0
367600150601    1c                   select
367700150119     c* 1) Se la bolla e consegnata
367800140925     c                   when      d98dcm>0
367900150113     c                   clear                   wcaldpcee
368000150216     c                   eval      d98spcdee='CON'
368100150217     c                   movel     ' '           DEEinDIST         1
368200150323     c* 2) Se pratica di danno
368300150603     c                   when      flgdan='S'
368400150323     c                   clear                   wcaldpcee
368500150323     c                   eval      d98spcdee='CA '
368600150323     c                   eval      deeindist='S'
368700150324     c* 3) Se bloccata/rimpallo
368800150114     c                   when      wd98fbc='B'
368900150113     c                   clear                   wcaldpcee
369000150216     c                   eval      d98spcdee='BLO'
369100150217     c                   movel     ' '           DEEinDIST         1
369200150324     c* 4) Se bolla export
369300170406     c                   when      ex>0 and fl1(EX)='E'
369400150324     c                   clear                   wcaldpcee
369500150324     c                   movel     ' '           DEEinDIST         1
369600150324     c                   eval      d98spcdee='EXP'
369700150324     c* 5) Se giacente  o in attesa di apertura giacenza
369800150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
369900150324     c                   clear                   wcaldpcee
370000150324     c                   eval      d98spcdee='GIA'
370100150324     c                   eval      deeindist='S'
370200170818     c* 6a) Se codice manca consegna e tentativi > 1
370300150326     c                   when      d98cmc<>*blanks and avvcmc='A'
370400170821     c*****                        and wd98ntc_con>1
370500150324     c                   clear                   wpodpcee
370600150324     c                   clear                   wcaldpcee
370700150324     c                   movel     ' '           DEEinDIST         1
370800150324     c                   eval      d98spcdee='ATT'
370900170818     c* 6b) Se codice manca consegna e tentativi = 1  non deve uscire la scritta" due tentativ"
371000170821     c***                when      d98cmc<>*blanks and avvcmc='A'
371100170821     c***                          and wd98ntc_con<=1
371200170821     c***                clear                   wpodpcee
371300170821     c***                clear                   wcaldpcee
371400170821     c***                movel     ' '           DEEinDIST         1
371500170821     c***                eval      d98spcdee='AT1'
371600150410     c* 7) Bolla in sospensione consegna
371700150410     c                   when      §B4LBGE='S'
371800150410     c                   clear                   wpodpcee
371900150410     c                   clear                   wcaldpcee
372000150410     c                   movel     ' '           DEEinDIST         1
372100150410     c                   eval      d98spcdee='SOS'
372200150616     c
372300150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
372400161031     c                   when      wrbl='R'  or d98cbo='M '
372500150616     c                   clear                   wcaldpcee
372600150616     c                   eval      d98spcdee='   '
372700150616     c                   eval      deeindist='S'
372800150601   x1c                   other
372900150325     c*
373000150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
373100150331     c                   if        d98ffd='S'
373200150601    2c                   select
373300150403     c* FD escluso dai contatti
373400150601    2c                   when      §B4LTFD= 'N'
373500150403     c                   eval      d98spcdee='FD3'
373600150403     c                   clear                   wcaldpcee
373700150403     c                   movel     ' '           DEEinDIST         1
373800150403
373900150403     c* FD non contattato ( o da ricontattare)
374000170821    2c                   when      §B4LTFD<>' '  and  (olv82sts<>'0'  and
374100170821     c                             olv82sts<>'1' or oLRJ6FALL=' ')
374200150323     c                   clear                   wcaldpcee
374300150323     c                   movel     ' '           DEEinDIST         1
374400150323     c                   eval      d98spcdee='FFD'
374500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
374600150424     c                   eval      wpodpcee=wpoffi
374700150403
374800150403     c* FD contattato e     IN distinta
374900150601    2c                   when      d98cons='S'
375000150401     c                   eval      d98spcdee='FD2'
375100150401     c                   clear                   wcaldpcee
375200150401     c                   movel     ' '           DEEinDIST         1
375300150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
375400150424     c                   eval      wpodpcee=wpoffi
375500150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
375600150427     c*  ritirare la merce
375700150427     C                   MOVEL     'C'           WTC                            *PARTENZA
375800150427     C                   MOVEL     'C'           WIC
375900150427     C                   Z-ADD     D98ddc        WDAT
376000150427     C                   EXSR      DATORE
376100150427     c                   z-add     wore          wr4poi
376200150427     c                   z-add     24            wr4noi
376300150427     C                   EXSR      RO4CDP
376400150427     c                   z-add     wr4poo        wore
376500150427     c                   exsr      OREDAT
376600150427     c                   movel     wdat          D98DTCTD
376700150601   x2c                   other
376800150403     c* FD contattato e NON in distinta
376900150331     c                   eval      d98spcdee='FD1'
377000150408     c                   eval      wpodpcee=wpoffi
377100150331     c                   clear                   wcaldpcee
377200150331     c                   movel     ' '           DEEinDIST         1
377300150601    2c                   endsl
377400150331     c
377500150331     c                   endif
377600150403     c
377700150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
377800150601    2c                   if        (d98tc1='A' or d98tc2='A' or
377900150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
378000150601     c                             and d98ffd= ' '
378100150325     c                   clear                   wcaldpcee
378200150325     c                   movel     ' '           DEEinDIST         1
378300150601    3c                   if        §B4LBAS= 'N'
378400150427     c                   eval      d98spcdee='AP3'
378500150403     c                   else
378600150403     c                   eval      d98spcdee='APP'
378700150601    3c                   endif
378800150601    2c                   endif
378900150325     c*   se la bolla ha il C/Assegno verifico se da contattare
379000150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
379100150325     c                   clear                   wcaldpcee
379200150325     c                   movel     ' '           DEEinDIST         1
379300170208    3c                   if        §B4LTCA= 'N'
379400150427     c                   eval      d98spcdee='CA3'
379500150403     c                   else
379600150403     c                   eval      d98spcdee='CAS'
379700150601    3c                   endif
379800150601    2c                   endif
379900150325     c* Per questi 3 casi devo impostare la data contatto destinatario
380000150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
380100150325     c                             d98spcdee='CAS'
380200170208     c
380300170208     c                   exsr      Caldam_rtc
380400170208     c                   endif
380500170208    1c                   endsl
380600140820     c
380700150113     c* Se la bolla non è in distinta calcolo la data se previsto
380800150220     c                   if        wcaldpcee='S' and
380900150220     c                             (d98cons<>'S'  or (d98cons='S' and
381000150220     c                             evemic<>'SI'))
381100150219     c
381200150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
381300150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
381400150219     c                   if        d98nsp>0 and wtrasm<>'D' and
381500150219     c                             d98dam=0
381600150219     c                   exsr      letBRV
381700150219     c                   endif
381800150219     c
381900150217     c                   eval      d98spcdee='PRG'
382000150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
382100150320     c                             d98tc1='S' or d98tc2='S')
382200150320     c                   eval      d98spcdee='APP'
382300150320     c                   endif
382400150320     c
382500150113     c                   exsr      cal_DEE
382600150113     c                   endif
382700150113     c
382800140820     C     WPOdpcee      IFEQ      *ZEROS
382900931202     C                   Z-ADD     *ZEROS        D98DEE
383000931202     C                   Z-ADD     *ZEROS        D98OEE
383100930604     C                   ELSE
383200140820     C                   Z-ADD     WPOdpcee      WORE
383300141021     C******             SUB       12            WORE
383400930604     C                   EXSR      OREDAT
383500931202     C                   Z-ADD     WDAT          D98DEE
383600090903     c                   if        d98tcc<>10
383700090903     C                   ADD       12            WRES
383800090903     c                   else
383900090903     C                   ADD       10            WRES
384000090903     c                   endif
384100090903
384200931202     C     WRES          MULT      100           D98OEE
384300090903     c
384400090903     c                   if        d98oee=1000
384500090903     c                   eval      d98oee=1030
384600090903     c                   endif
384700140925     C                   END
384800150311     c
384900150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
385000150311     c                   select
385100150311     c                   when      d98spcdee='OLP'
385200140820     c
385300150220     c* Se bolla in distinta --> data prevista = data distinta
385400150311     c                   when      deeindist='S'  and
385500150113     c                             d98cons='S'
385600150220
385700150225     c*  non la propongo senza MIC e se presente disguido o IMP
385800150220     c                   if        d98dee>d98ddc and evemic<>'SI'
385900150220     c                             and IDdata>0
386000150220     c                   else
386100150220
386200140820     c                   eval      d98dee=d98ddc
386300140925     c                   clear                   d98oee
386400150320     c* se la data è > di oggi stato PRV se non è in IMP
386500150320     c                   if        d98dee>wdaodi
386600150320     c                   if        d98spcdee<>'IMP'
386700150311     c                   eval      d98spcdee='PRV'
386800150320     c                   endif
386900150320     c
387000150225     c                   else
387100150217     c* se c'è il MIC
387200150217     c                   if        evemic='SI'
387300150217     c                   eval      d98spcdee='DDC'
387400150225     c                   endif
387500150217     c                   endif
387600150220     c                   endif
387700150217     c
387800150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
387900150722     c                   eval      d98spcdee='PRV'
388000150311     c                   endsl
388100170208     c
388200170208     c* se non ho ancora calcolato la data arrivo reale o teorica tempi cliente, lo faccio ora
388300170208     c                   if        d98dam_rtc=0
388400170208     c                   eval      wsolodam='S'
388500170208     c                   exsr      Caldam_rtc
388600170208     c                   eval      wsolodam=' '
388700170208     c                   endif
388800931202     C*
388900931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
389000931202     C     WPODCI        IFEQ      *ZEROS
389100931202     C                   Z-ADD     *ZEROS        D98DCI
389200931202     C                   Z-ADD     *ZEROS        D98HCI
389300931202     C                   ELSE
389400931202     C                   Z-ADD     WPODCI        WORE
389500931202     C                   EXSR      OREDAT
389600931202     C                   Z-ADD     WDAT          D98DCI
389700090902     c                   if        d98tci<>10
389800931202     C                   ADD       12            WRES
389900090902     c                   else
390000090902     C                   ADD       10            WRES
390100090902     c                   endif
390200090902
390300931202     C     WRES          MULT      100           D98HCI
390400090901     c
390500090903     c                   if        d98hci=1000
390600090901     c                   eval      d98hci=1030
390700090901     c                   endif
390800090901     c
390900931202     C                   END
391000931202     C*
391100931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
391200931202     C     WPODCE        IFEQ      *ZEROS
391300931202     C                   Z-ADD     *ZEROS        D98DCE
391400931202     C                   Z-ADD     *ZEROS        D98HCE
391500931202     C                   ELSE
391600931202     C                   Z-ADD     WPODCE        WORE
391700931202//// C**************       SUB  12        WORE
391800931202     C                   EXSR      OREDAT
391900931202     C                   Z-ADD     WDAT          D98DCE
392000090902     c                   if        d98tcc >10
392100931202     C                   ADD       12            WRES
392200090902     c                   else
392300090902     C                   ADD       10            WRES
392400090902     c                   endif
392500090902     c
392600931202     C     WRES          MULT      100           D98HCE
392700090902     c
392800090903     c                   if        d98hce=1000
392900090902     c                   eval      d98hce=1030
393000090902     c                   endif
393100090902     c
393200931202     C                   END
393300960229     C*
393400960229     C                   ENDIF                                                  <-- 1 ---
393500930604     C*
393600930604     C                   ENDSR
393700170208     C********************************************************************
393800170208     C* Calcolo data di arrivo teorica o reale cliente
393900170208     C********************************************************************
394000170208     c     Caldam_rtc    BEGSR
394100170208     c
394200170208    2c                   select
394300170220    2c                   when      d98dti>0 and wmancatraz= ' ' and wpodti>0
394400170220     c
394500170220     c* questa specifica non serve: se è piena la data è pieno wpodti e viceversa
394600170220     c*****              movel     d98dti        d98dtctd
394700170208     c* se piene le ore già portate ad un giorno lavorativo
394800170220     c*******            if        wpodti>0
394900170208     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
395000170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
395100170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
395200170208     C                   Z-ADD     WPODTI        W10POI            9 0
395300170208     C                   EXSR      R10FES
395400170208     c                   z-add     w10poo        wore
395500170208     C                   EXSR      OREDAT
395600170208     c                   if        wsolodam<>'S'
395700170208     C                   movel     WDAT          D98dtctd
395800170208     c                   endif
395900170208     C                   movel     WDAT          D98dam_rtc
396000170220     c*****              endif
396100170208     c
396200170208    2c                   when      d98dam>0
396300170208     C                   Z-ADD     D98dam        WDAT
396400170208     C                   EXSR      DATORE
396500170208     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
396600170208     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
396700170208     C                   Z-ADD     Wore          W10POI            9 0
396800170208     C                   EXSR      R10FES
396900170208     c                   z-add     w10poo        wore
397000170208     C                   EXSR      OREDAT
397100170208     c                   if        wsolodam<>'S'
397200170208     C                   movel     WDAT          D98dtctd
397300170208     c                   endif
397400170208     C                   movel     WDAT          D98dam_rtc
397500170208   x2c                   other
397600170208     c* data teorica di arrivo tenendo conto dell'IMP
397700170208     c                   if        d98nsp>0
397800170208     c                   exsr      letbrv
397900170208     c                   endif
398000170208     c                   if        idDATA>0
398100170208     c                   exsr      CalcoloIMP
398200170208     c                   z-add     wr1poi        WORE
398300170208     c                   else
398400170208     c                   z-add     wpoptc_dee    WORE
398500170208     c                   endif
398600170208     C                   EXSR      OREDAT
398700170208     c                   if        wsolodam<>'S'
398800170208     c                   movel     wdat          d98dtctd
398900170208     c                   endif
399000170208     C                   movel     WDAT          D98dam_rtc
399100170208    2c                   endsl
399200170208     c
399300170208     c
399400170208     c                   ENDSR
399500150218     C********************************************************************
399600150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
399700150218     C********************************************************************
399800150218     c     LetBRV        BEGSR
399900150219     c                   clear                   bollaOK           1
400000150219     c                   clear                   IDnpg
400100150219     c                   clear                   IDdata
400200150219     c                   clear                   IDora
400300150219     c                   clear                   IDfgs
400400150219     c
400500150218     C     Kart          SETLL     FNART01L
400600150218     C     Kart          READE     FNART01L
400700151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
400800150219     c                   clear                   DISfgs
400900150219     c                   clear                   DISdata
401000150219     c                   clear                   DISnpg
401100150219     c                   clear                   DISora
401200150219     c                   clear                   IMPdata
401300150219     c                   clear                   IMPfgs
401400150508     c* per ogni collo verifico se in IMP o disguidato
401500150218     c* al primo che NON trovo, esco come --> OK
401600150218     c     kbrv          setll     fnbrv07l
401700150218     c     kbrv          reade     fnbrv07l
401800150219    2c                   dow       not %eof(fnbrv07l)
401900150219    3c                   if        brvatr='D'  or brvnpg=3
402000150218     c* prendo la data della spunta
402100150218     c                   clear                   fnlv53ds
402200150219     c                   eval      d53tla='L'
402300150218     C                   MOVEL     BRvNPG        D53NPG
402400150218     C                   Z-ADD     BRvNFV        D53NFV
402500150218     C                   MOVEL     BRvFGS        D53FGS
402600150218     C                   CALL      'FNLV53R'
402700150218     C                   PARM                    fnlv53ds
402800150218    4c                   if        d53err<>*blanks
402900150219    5c                   if        brvdcs>0
403000150218     c                   eval      d53dfv=brvdcs
403100150218     c                   else
403200150219    6c                   if        brvdfs>0
403300150218     c                   eval      d53dfv=brvdfs
403400150219    6c                   endif
403500150219    5c                   endif
403600150219    4c                   endif
403700150218     c
403800150219    4c                   select
403900150219     c* se si tratta di DISGUIDO
404000150218     c                   when      brvatr='D'
404100150219     c
404200150219    5c                   if        d53dfv=DISdata
404300150219     c* Verifico l'ora
404400150219    6c                   if        brvhcs>0 and brvhcs>DISora
404500150219     c                   eval      DISdata=d53dfv
404600150219     c                   eval      DISfgs =brvfgs
404700150219     c                   eval      DISnpg =brvnpg
404800150219     c                   eval      DISora =brvhcs
404900150219   x6c                   else
405000150219    7c                   if        brvhms>0 and brvhms>DISora
405100150219     c                   eval      DISdata=d53dfv
405200150219     c                   eval      DISfgs =brvfgs
405300150219     c                   eval      DISnpg =brvnpg
405400150219     c                   eval      DISora =brvhms
405500150219    7c                   endif
405600150219    6c                   endif
405700150219    5c                   endif
405800150219     c
405900150219    5c                   if        d53dfv>DISdata
406000150219     c                   eval      DISdata=d53dfv
406100150219     c                   eval      DISfgs =brvfgs
406200150219     c                   eval      DISnpg =brvnpg
406300150219     c                   eval      DISora =brvhcs
406400150219     c                   if        disora=0
406500150219     c                   eval      DISora =brvhms
406600150219     c                   endif
406700150219    5c                   endif
406800150219     c
406900151009     c* se si tratta di IMP
407000151009    4c                   when      d53spg='P' and d53dfv>impdata
407100151009     c                   eval      IMPdata=d53dfv
407200151009     c                   eval      IMPfgs =brvfgs
407300150219    4c                   endsl
407400150219    3c                   endif
407500151009
407600150219     c     kbrv          reade     fnbrv07l
407700150219    2c                   enddo
407800150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
407900150219     c*  esco dal ciclo: la bolla è OK
408000150219    2c                   if        impdata=0 and disdata=0
408100150219     c                   eval      bollaOK='S'
408200150219     c                   clear                   IDdata
408300150219     c                   clear                   IDfgs
408400150219     c                   clear                   IDora
408500150219     c                   clear                   IDnpg
408600150219   x2c                   else
408700150219     c* memorizzo la data più alta
408800150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
408900150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
409000150219     c                   eval      IDdata=IMPdata
409100150219     c                   eval      IDfgs=IMPfgs
409200150219     c                   eval      IDora=0
409300150219     c                   eval      IDnpg='P'
409400150219    3c                   endif
409500150219    3c                   if        (DISdata>0 and DISdata=IDdata
409600150219     c                             and IDnpg<>'P' and DISora>IDora) or
409700150219     c                             (DISdata>0 and DISdata>IDdata)
409800150219     c                   eval      IDdata=DISdata
409900150219     c                   eval      IDfgs=DISfgs
410000150219     c                   eval      IDora=DISora
410100150219     c                   eval      IDnpg=%char(DISnpg)
410200150219    3c                   endif
410300150219    2c                   endif
410400150218     c
410500150218     C     Kart          READE     FNART01L
410600150219    1c                   enddo
410700150219     c
410800150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
410900150508     c**                 if        d98duc> 0 and d98duc>IDdata
411000150508     c**                 eval      bollaOK='S'
411100150508     c**                 clear                   IDdata
411200150508     c**                 clear                   IDfgs
411300150508     c**                 clear                   IDora
411400150508     c**                 clear                   IDnpg
411500150508     c**                 endif
411600150219     c
411700150218     c
411800150218     C                   ENDSR
411900140825     C********************************************************************
412000140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
412100140825     C********************************************************************
412200140825     c     TRAS_DEI      BEGSR
412300140825     C     WPODEI        IFEQ      *ZEROS
412400140825     C                   Z-ADD     *ZEROS        D98DEI
412500140825     C                   Z-ADD     *ZEROS        D98OEI
412600140825     C                   ELSE
412700140825     C                   Z-ADD     WPODEI        WORE
412800140825     C                   SUB       12            WORE
412900140825     C                   EXSR      OREDAT
413000140825     c
413100140825     C                   Z-ADD     WDAT          D98DEI
413200140825     c                   if        d98tci<>10
413300140825     C                   ADD       12            WRES
413400140825     c                   else
413500140825     C                   ADD       10            WRES
413600140825     c                   endif
413700140825     c
413800140825     C     WRES          MULT      100           D98OEI
413900140825     c
414000140825     c                   if        d98oei=1000
414100140825     c                   eval      d98oei=1030
414200140825     c                   endif
414300140825     c
414400140825     C                   END
414500140825     c                   ENDSR
414600930604     C********************************************************************
414700930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
414800930604     C********************************************************************
414900930604     C     CHKFOR        BEGSR
415000930604     C*
415100930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
415200930604     C* E QUINDI FORZO LA CONFORMITA' A SI
415300941227     C     D98ERR        IFEQ      '0'
415400930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
415500930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
415600930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
415700930604     C                   MOVEL     '1'           D98ERR
415800930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
415900930604     C                   END
416000930604     C                   END
416100990212     C*
416200990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
416300990212     C* E QUINDI FORZO LA CONFORMITA' A SI
416400990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
416500990212     C     D98ERR        IFEQ      '0'
416600990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
416700141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
416800990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
416900990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
417000990212     C                   MOVEL     '1'           D98ERR
417100990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
417200990212     C                   END
417300990212     C                   END
417400990308     C*
417500990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
417600990308     C* E QUINDI FORZO LA CONFORMITA' A SI
417700100614     C**   D98ERR        IFEQ      '0'
417800100614     C**   WFBR          IFEQ      'S'                                          *RESO
417900100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
418000100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
418100100614     C**                 MOVEL     '1'           D98ERR
418200100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
418300100614     C**                 END
418400100614     C**                 END
418500930604     C*
418600930604     C                   ENDSR
418700990212     C********************************************************************
418800990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
418900990212     C********************************************************************
419000990212     C     CHKDAN        BEGSR
419100990212     C*
419200990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
419300150603     c                   movel     'N'           opendan           1
419400040809      *
419500040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
419600040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
419700040809      * legate alla spedizione che passo
419800040809     c                   clear                   fidn12ds
419900040809     c                   eval      i12aas = D98AAS
420000040809     c                   eval      i12lnp = D98LNP
420100040809     c                   eval      i12nrs = D98NRS
420200040809     c                   eval      i12nsp = D98NSP
420300040809     c                   eval      i12fel = 'E'
420400040809     c                   eval      i12fan = 'E'
420500040809     c*
420600040809     c                   call      'FIDN12R'
420700040809     c                   parm                    fidn12ds
420800040809     c*
420900040809     c* se non ci sono errori
421000040809     c                   if        o12err = ' '
421100040809     c* se numero di CA trovate maggiore di zero
421200040809     c                   if        o12nca > 0
421300040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
421400150603     c
421500161215     c                   z-add     1             aa                2 0
421600161215     c                   dow       aa <= o12nca
421700150603     C* APERTA
421800161215     C***  skdch(aa)     IFgt      0
421900161215     c****               movel     'S'           opendan
422000161215     c***                endif
422100161215     c                   if        skdca(aa)>0 and skdca(aa)>d98dac
422200161215     c                   eval      d98dac =skdca(aa)
422300161215     c                   eval      d98dcc =skdch(aa)
422400161215     c                   endif
422500150603     c*
422600161215     c                   add       1             aa
422700161215     c                   enddo
422800150603     c
422900040809     c                   endif
423000040809     c                   endif
423100990212     C*
423200990212     C                   ENDSR
423300941228     C********************************************************************
423400941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
423500941228     C********************************************************************
423600941228     C     CALORP        BEGSR
423700941228     C*
423800941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
423900941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
424000941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
424100960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
424200950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
424300950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
424400950110     C                   ELSE
424500950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
424600950110     C                   ENDIF
424700941228     C                   EXSR      RO2COP
424800941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
424900941228     C* SPED. IL GIORNO DOPO
425000941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
425100941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
425200941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
425300941228     C*
425400941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
425500941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
425600941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
425700941228     C                   Z-ADD     WPODSP        WR2POI
425800950110     C                   Z-ADD     WPOPPC        WR2POF
425900941228     C                   EXSR      RO2COP
426000941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
426100941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
426200941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
426300941228     C*
426400941228     C                   ENDSR
426500930414     C********************************************************************
426600930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
426700930414     C********************************************************************
426800930414     C     CALORT        BEGSR
426900930414     C*
427000930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
427100931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
427200930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
427300931126     C                   Z-ADD     *ZEROS        W98HF
427400950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
427500950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
427600950110     C                   ELSE
427700950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
427800950110     C                   ENDIF
427900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
428000931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
428100930608     C                   ELSE
428200931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
428300930608     C                   END
428400931126     C                   EXSR      RO2COV
428500931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
428600930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
428700930414     C*
428800930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
428900930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
429000930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
429100931126     C                   Z-ADD     *ZEROS        W98HF
429200950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
429300931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
429400931126     C                   EXSR      RO2COV
429500931126     C                   Z-ADD     WR2NOV        W98RTC
429600930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
429700930414     C*
429800930414     C                   ENDSR
429900930414     C********************************************************************
430000930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
430100930414     C********************************************************************
430200930414     C     CALORC        BEGSR
430300930511     C*
430400930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
430500930511     C* I TEMPI DI RITARDO AD OGGI
430600930511     C     WPODCM        IFEQ      *ZEROS
430700960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
430800930511     C                   END
430900940117     C*
431000940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
431100940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
431200940117     C* ALLA DATA DI FINE CONSEGNABILITA'
431300940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
431400940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
431500940118££££ C     WPODFC        IFNE      *ZEROS
431600940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
431700940118££££ C                   END
431800930414     C*
431900930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
432000930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
432100930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
432200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
432300931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
432400930608     C                   ELSE
432500931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
432600930608     C                   END
432700931126     C                   Z-ADD     WPODCM        WR5POF            9 0
432800931126     C                   EXSR      RO5COV
432900931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
433000930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
433100930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
433200060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
433300060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
433400930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
433500930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
433600930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
433700930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
433800930414     C*
433900930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
434000930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
434100930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
434200931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
434300931126     C                   Z-ADD     WPODCM        WR5POF            9 0
434400931126     C                   EXSR      RO5COV
434500931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
434600930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
434700930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
434800060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
434900060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
435000930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
435100930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
435200930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
435300930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
435400930414     C*
435500930414     C                   ENDSR
435600930414     C********************************************************************
435700930414     C*   CALORR - CALCOLA ORE REALI DI RESA
435800930414     C********************************************************************
435900930414     C     CALORR        BEGSR
436000930414     C*
436100950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
436200950110     C                   ADD       W98RTI        W98RRI
436300941228     C                   ADD       W98RCI        W98RRI
436400950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
436500950110     C                   ADD       W98RTC        W98RRC
436600941228     C                   ADD       W98RCC        W98RRC
436700930414     C*
436800930414     C                   ENDSR
436900930420     C********************************************************************
437000930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
437100930420     C********************************************************************
437200930420     C     CALDIF        BEGSR
437300930420     C*
437400941228     C* PARTENZA
437500941228     C     W98RPI        SUB       D98TPI        W98FPI
437600941228     C     W98RPC        SUB       D98TPC        W98FPC
437700941228     C*
437800930420     C* TRAZIONE
437900930423     C     W98RTI        SUB       D98TTI        W98FTI
438000930423     C     W98RTC        SUB       D98TTC        W98FTC
438100930420     C*
438200930420     C* CONSEGNA
438300090827     C***  W98RCI        SUB       D98TCI        W98FCI
438400090827     C     W98RCI        SUB       w99TCI        W98FCI
438500090827     C***  W98RCC        SUB       D98TCC        W98FCC
438600090827     C     W98RCC        SUB       w99TCC        W98FCC
438700930420     C*
438800930420     C* RESA
438900090921     C***  W98RRI        SUB       D98TRI        W98FRI
439000090921     C     W98RRI        SUB       w99TRI        W98FRI
439100090921     c
439200090921     C***  W98RRC        SUB       D98TRC        W98FRC
439300170104     c
439400170104     c*** se le ore reali interne di RESA usate sono < delle ore reali cliente, uso le
439500170104     c*    reali interne
439600170104     c                   if        w98rri<w98rrc
439700170104     C     W98RRI        SUB       w99TRC        W98FRC
439800170104     c                   else
439900170104     C     W98RRC        SUB       w99TRC        W98FRC
440000170104     c                   endif
440100930420     C*
440200930420     C                   ENDSR
440300930420     C********************************************************************
440400930420     C*   CALCNF - CALCOLA LE CONFORMITA'
440500930420     C********************************************************************
440600930420     C     CALCNF        BEGSR
440700941228     C*
440800941228     C*-------------------------------------------------------------------
440900941228     C* CONFORMITA' PARTENZA  INTERNA
441000941228     C*-------------------------------------------------------------------
441100941228     C                   Z-ADD     W98FPI        W98NPI
441200941228     C     W98FPI        IFLE      *ZEROS
441300941228     C                   MOVEL     'S'           D98CPI
441400941228     C                   ELSE
441500941228     C                   MOVEL     'N'           D98CPI
441600941228     C                   END
441700941228     C*
441800941228     C*-------------------------------------------------------------------
441900941228     C* CONFORMITA' PARTENZA  CLIENTE
442000941228     C*-------------------------------------------------------------------
442100941228     C                   Z-ADD     W98FPC        W98NPC
442200941228     C     W98FPC        IFLE      *ZEROS
442300941228     C                   MOVEL     'S'           D98CPC
442400941228     C                   ELSE
442500941228     C                   MOVEL     'N'           D98CPC
442600941228     C                   END
442700930420     C*
442800930420     C*-------------------------------------------------------------------
442900930420     C* CONFORMITA' TRAZIONE  INTERNA
443000930420     C*-------------------------------------------------------------------
443100930510     C                   Z-ADD     W98FTI        W98NTI
443200930423     C     W98FTI        IFLE      *ZEROS
443300930420     C                   MOVEL     'S'           D98CTI
443400930420     C                   ELSE
443500930420     C                   MOVEL     'N'           D98CTI
443600930420     C                   END
443700930420     C*
443800930420     C*-------------------------------------------------------------------
443900930420     C* CONFORMITA' TRAZIONE  CLIENTE
444000930420     C*-------------------------------------------------------------------
444100930510     C                   Z-ADD     W98FTC        W98NTC
444200930423     C     W98FTC        IFLE      *ZEROS
444300930420     C                   MOVEL     'S'           D98CTC
444400930420     C                   ELSE
444500930420     C                   MOVEL     'N'           D98CTC
444600930420     C                   END
444700930420     C*
444800930420     C*-------------------------------------------------------------------
444900930420     C* CONFORMITA' CONSEGNA  INTERNA
445000930420     C*-------------------------------------------------------------------
445100150226     c                   if        wmancatraz='S'
445200150226     c                   clear                   d98dti
445300150226     c                   clear                   d98hti
445400150226     c                   endif
445500150226     c
445600930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
445700930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
445800930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
445900050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
446000050914     c                   if        consideraora='S'
446100050914     c     w98fci        sub       24            w98nci
446200050914     c                   add       24            d98hrc
446300050914     c                   else
446400930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
446500050914     c                   endif
446600080116     c
446700110923     c                   select
446800080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
446900080116     c*  --> trasformo il ritardo in 0
447000090827     c                   when      d98cca='2'
447100080116     c                   z-add     0             w98nci
447200090827
447300080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
447400080116     c*  e sto leggendo la bolla in arrivo
447500080116     c*  --> trasformo il ritardo = 0
447600090827     c                   when      d98cca='1' and d98tbo='A'
447700080116     c                   z-add     0             w98nci
447800090902     c
447900090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
448000090902     c                   when      wmancaTRAZ='S'
448100090902     c                   z-add     0             w98nci
448200090902     c                   clear                   d98dti
448300090902     c                   clear                   d98hti
448400110923     c
448500110923     c* Ignoro ritardo se pefect order
448600130221     c                   when      d98xco >*zeros
448700110923     c                   z-add     0             w98nci
448800090827     c
448900090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
449000091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
449100091014     c*  controllo ora soglia  consegna da tabella OSC)
449200091009     c                   when      d98tci=10   and
449300091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
449400091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
449500091014     c*
449600091014     c                             d98hci=1030
449700091014     C                   MOVEL     'OSC  '       WR3TAB
449800091014     C                   eval      WR3HHI=d98hmc
449900091014     C                   EXSR      RO3ORE
450000131218
450100091014     c                   if        wr3matpom='M' and d98hmc>1030
450200131218     c                   exsr      ctrLAVH1030
450300131218
450400131218     c                   if        wlav1030<>'S'
450500091014     c                   add       12            w98nci
450600091014     c                   endif
450700131218     c                   endif
450800090928     c
450900090827     c                   endsl
451000050914     c
451100930524     C                   ELSE                                                   <-- 1 -->
451200930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
451300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
451400930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
451500931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
451600930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
451700930608     C                   ELSE
451800930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
451900930608     C                   END
452000930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
452100930525     C                   EXSR      RO2COV
452200930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
452300930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
452400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
452500930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
452600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
452700930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
452800930608     C                   ELSE
452900930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
453000930608     C                   END
453100931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
453200930525     C                   EXSR      RO2COV
453300930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
453400930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
453500930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
453600930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
453700930525     C* E' IL RITARDO
453800930610     C     WNOCRI        SUB       W98RCI        W98NCI
453900050901     c
454000050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
454100050901     c*  --> trasformo il ritardo in anticipo
454200050901     c                   if        d98cca='2'
454300080116     c***                z-sub     w98nci        w98nci
454400080116     c                   z-add     0             w98nci
454500050901     c                   endif
454600111018     c* Ignoro ritardo se pefect order
454700130221     c                   if        d98xco >*zeros
454800111018     c                   z-add     0             w98nci
454900111018     c                   endif
455000061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
455100061121     c*  e sto leggendo la bolla in arrivo
455200061121     c*  --> trasformo il ritardo in anticipo
455300061121     c                   if        d98cca='1' and d98tbo='A'
455400080116     c***                z-sub     w98nci        w98nci
455500080116     c                   z-add     0             w98nci
455600061121     c                   endif
455700070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
455800070827     c*  mittente, --> consegnato in anticipo
455900070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
456000140403     c                   if        Callar5=' '
456100070827     c                   exsr      LeggiAR5
456200140403     c                   endif
456300070827     c
456400070827     c                   if        dcrpart<>'S'
456500070824     c                   z-sub     w98nci        w98nci
456600070824     c                   endif
456700070827     c
456800070824     c                   endif
456900050907     c
457000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
457100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
457200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
457300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
457400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
457500050916     c***                if        d98dfc>0
457600070828     c                   if        d98dcm=0  and w98nci>0
457700050907     c                   z-sub     w98nci        w98nci
457800050907     c                   sub       oreILoDOPO    w98nci
457900050907     c                   endif
458000050901     c
458100930525     C                   ELSE                                                   <-- 2 -->
458200070521     c* non do ritardo se la consegna reale cade nel giorno
458300070521     c*  della consegna richiesta se presente IL
458400100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
458500070521     c                             wpodcm<=wpocr1
458600070521     c                   clear                   w98nci
458700100224     c                   else                                                   ---2a -->
458800930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
458900090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
459000090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
459100930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
459200930525     C* E' IL RITARDO O L'ANTICIPO
459300930525     C     W98RCI        SUB       WNOCRF        W98NCI
459400070115     c
459500070115     c* non do ritardo se comunque la consegna reale cade nel giorno
459600070115     c*  della consegna richiesta se presente IL
459700070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
459800070521     c***                          wpodcm<=wpocr1  and w98nci>0
459900070521     c***                clear                   w98nci
460000070521     c***                endif
460100070115     c
460200930525     C                   ELSE                                                   <-- 3 -->
460300930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
460400930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
460500930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
460600930525     C                   END                                                    <-- 3 ---
460700100224     C                   END                                                    <-- 2a---
460800100224
460900100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
461000100224     c                   select
461100100224     c                   when      d98cca='2'
461200100224     c                   z-add     0             w98nci
461300100224
461400100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
461500100224     c*  e sto leggendo la bolla in arrivo
461600100224     c                   when      d98cca='1' and d98tbo='A'
461700100224     c                   z-add     0             w98nci
461800100224     c                   endsl
461900100224     c
462000100224     C                   END                                                    <-- 2 ---
462100070521     C                   END                                                    <-- 1 ---
462200070521     c
462300970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
462400970915     C*/         WAZNCI    IFEQ 'S'
462500970915     C*/         W98NCI    ANDLT*ZEROS
462600970915     C*/                   Z-ADD*ZEROS    W98NCI
462700970915     C*/                   ENDIF
462800970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
462900050922     c* lo faccio solo se non c'e' la data cons richiesta
463000970915     C     D98DTI        IFEQ      *ZEROS
463100050922     c     d98dcr        andeq     *zeros
463200970915     C                   Z-ADD     *ZEROS        W98NCI
463300970915     C                   ENDIF
463400930524     C*
463500930524     C* CONTROLLO CONFORMITA
463600930423     C     W98NCI        IFLE      *ZEROS
463700930524     C                   MOVEL     'S'           D98CCI
463800930420     C                   ELSE
463900930524     C                   MOVEL     'N'           D98CCI
464000930420     C                   END
464100930420     C*
464200930420     C*-------------------------------------------------------------------
464300930420     C* CONFORMITA' CONSEGNA  CLIENTE
464400930420     C*-------------------------------------------------------------------
464500930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
464600930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
464700930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
464800050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
464900050914     c                   if        consideraora='S'
465000050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
465100050914     c                   else
465200050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
465300050914     c                   endif
465400080116     c
465500080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
465600080116     c*  --> trasformo il ritardo =0
465700090827     c                   select
465800090827     c                   when      d98cca='2'
465900080116     c                   z-add     0             w98ncc
466000090827
466100080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
466200080116     c*  e sto leggendo la bolla in arrivo
466300080116     c*  --> trasformo il ritardo =0
466400090827     c                   when      d98cca='1' and d98tbo='A'
466500080116     c                   z-add     0             w98ncc
466600090902     c
466700090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
466800090902     c                   when      wmancaTRAZ='S'
466900090902     c                   z-add     0             w98ncc
467000110923     c* Ignoro ritardo se Perfect Order
467100130221     c                   when      d98xco >*zeros
467200110923     c                   z-add     0             w98ncc
467300090902     c
467400090827     c
467500090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
467600090827     c* sono  in ritardo di 12 ore
467700091009     c                   when      d98tcc=10  and
467800091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
467900091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
468000091014     c*
468100091014     c                             d98hce=1030
468200091014     C                   MOVEL     'OSC  '       WR3TAB
468300091014     C                   eval      WR3HHI=d98hmc
468400091014     C                   EXSR      RO3ORE
468500091014     c                   if        wr3matpom='M' and d98hmc>1030
468600131218     c                   exsr      ctrLAVH1030
468700131218
468800131218     c                   if        wlav1030<>'S'
468900131218     c                   add       12            w98ncc
469000131218     c                   endif
469100131218     c****               add       12            w98ncc
469200091014     c                   endif
469300131218     c
469400091014     c***                z-add     12            w98ncc
469500090827     c                   endsl
469600050914     c
469700930524     C                   ELSE                                                   <-- 1 -->
469800930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
469900930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
470000930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
470100930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
470200930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
470300930525     C                   EXSR      RO2COV
470400930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
470500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
470600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
470700930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
470800930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
470900931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
471000930525     C                   EXSR      RO2COV
471100930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
471200930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
471300930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
471400930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
471500930525     C* E' IL RITARDO
471600930525     C     WNOCRI        SUB       W98RCC        W98NCC
471700050901     c
471800050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
471900050901     c*  --> trasformo il ritardo in anticipo
472000050901     c                   if        d98cca='2'
472100080116     c***                z-sub     w98ncc        w98ncc
472200080116     c                   z-add     0             w98ncc
472300050901     c                   endif
472400111018     c* Ignoro ritardo se pefect order
472500130221     c                   if        d98xco >*zeros
472600111018     c                   z-add     0             w98ncc
472700111018     c                   endif
472800061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
472900061121     c*  e sto leggendo la bolla in arrivo
473000061121     c*  --> trasformo il ritardo in anticipo
473100061121     c                   if        d98cca='1' and d98tbo='A'
473200080116     c***                z-sub     w98ncc        w98ncc
473300080116     c                   z-add     0             w98ncc
473400061121     c                   endif
473500070824     c
473600070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
473700070827     c*  mittente, --> consegnato in anticipo
473800070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
473900070827     c* SBR per vedere se tassativa
474000070827     c                   if        Callar5=' '
474100070827     c                   EXSR      LeggiAR5
474200070827     c                   endif
474300070827     c                   if        dcrpart<>'S'
474400070824     c                   z-sub     w98ncc        w98ncc
474500070824     c                   endif
474600070824     c                   endif
474700050907     c
474800050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
474900050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
475000050907     c*  lo trasformo in anticipo perchè non sono in ritardo
475100050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
475200050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
475300050916     c***                if        d98dfc>0
475400070828     c                   if        d98dcm=0 and w98ncc>0
475500050907     c                   z-sub     w98ncc        w98ncc
475600050907     c                   sub       oreILoDOPO    w98ncc
475700050907     c                   endif
475800050901     c
475900930525     C                   ELSE                                                   <-- 2 -->
476000070521     c* Non do ritardo se la consegna reale cade nel giorno
476100070521     c*  della consegna richiesta se presente IL
476200100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
476300070521     c                             wpodcm<=wpocr2
476400070521     c                   clear                   w98ncc
476500100224     c                   else                                                   <--2a -->
476600070521     c
476700930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
476800090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
476900090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
477000930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
477100930525     C* E' IL RITARDO O L'ANTICIPO
477200930525     C     W98RCC        SUB       WNOCRF        W98NCC
477300070115     c
477400070115     c* non do ritardo se comunque la consegna reale cade nel giorno
477500070115     c*  della consegna richiesta se presente IL
477600070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
477700070521     c***                          wpodcm<=wpocr2 and w98ncc>0
477800070521     c***                clear                   w98ncc
477900070521     c***                endif
478000070115     c
478100930525     C                   ELSE                                                   <-- 3 -->
478200930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
478300930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
478400930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
478500930525     C                   END                                                    <-- 3 ---
478600100224     C                   END                                                    <-- 2a---
478700100224
478800100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
478900100224     c                   select
479000100224     c                   when      d98cca='2'
479100100224     c                   z-add     0             w98nci
479200100224
479300100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
479400100224     c*  e sto leggendo la bolla in arrivo
479500100224     c                   when      d98cca='1' and d98tbo='A'
479600100224     c                   z-add     0             w98nci
479700100224     c                   endsl
479800100224     c
479900100224     C                   END                                                    <-- 2 ---
480000070521     C                   END                                                    <-- 1 ---
480100930524     C*
480200930524     C* CONTROLLO CONFORMITA
480300930423     C     W98NCC        IFLE      *ZEROS
480400930524     C                   MOVEL     'S'           D98CCC
480500930420     C                   ELSE
480600930524     C                   MOVEL     'N'           D98CCC
480700930420     C                   END
480800930420     C*
480900930420     C*-------------------------------------------------------------------
481000930420     C* CONFORMITA' RESA INTERNA
481100930420     C*-------------------------------------------------------------------
481200930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
481300930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
481400930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
481500050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
481600050914     c                   if        consideraora='S'
481700050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
481800050914     c                   else
481900050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
482000050914     c                   endif
482100080116     c
482200080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
482300090827     c                   select
482400090827     c                   when      d98cca='2' and w98nri>0
482500080116     c                   z-add     0             w98nri
482600090827     c
482700080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
482800080116     c*  e sto leggendo la bolla in arrivo
482900090827     c                   when      d98cca='1' and d98tbo='A' and
483000080116     c                                            w98nri>0
483100080116     c                   z-add     0             w98nri
483200110923     c
483300110923     c* Ignoro ritardo se Perfect Order
483400130221     c                   when      d98xco >*zeros
483500110923     c                   z-add     0             w98nri
483600110923     c
483700090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
483800090827     c* sono  in ritardo di 12 ore
483900091009     c                   when      d98tci=10  and
484000091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
484100091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
484200091014     c*
484300091014     c                             d98hci=1030
484400091014     C                   MOVEL     'OSC  '       WR3TAB
484500091014     C                   eval      WR3HHI=d98hmc
484600091014     C                   EXSR      RO3ORE
484700091014     c                   if        wr3matpom='M' and d98hmc>1030
484800131218     c                   exsr      ctrLAVH1030
484900131218
485000131218     c                   if        wlav1030<>'S'
485100091014     c                   add       12            w98nri
485200091014     c                   endif
485300131218     c                   endif
485400091014     c
485500091014     c***                z-add     12            w98nri
485600090827     c                   endsl
485700090827
485800930524     C                   ELSE                                                   <-- 1 -->
485900930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
486000930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
486100930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
486200931206     C     WPODCM        ANDLE     WPOCR1
486300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
486400930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
486500930525     C                   Z-ADD     WPODCM        WR2POI            9 0
486600931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
486700930525     C                   EXSR      RO2COV
486800930525     C     WR2NOV        IFNE      *ZEROS
486900930525     C     WR2NOV        MULT      -1            W98NRI
487000930525     C                   END
487100930524     C                   ELSE                                                   <-- 2 -->
487200930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
487300930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
487400930524     C     WPODCM        IFLT      WPOCRI
487500930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
487600930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
487700930524     C                   ELSE
487800931206     C                   Z-ADD     WPOCR1        WPORFI
487900930524     C                   Z-ADD     WPODCM        WPORFF
488000930524     C                   END
488100930524     C* CALCOLO ORE DI DIFFERENZA
488200930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
488300930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
488400930524     C                   Z-ADD     WPORFI        WR2POI            9 0
488500930524     C                   Z-ADD     WPORFF        WR2POF            9 0
488600930524     C                   EXSR      RO2COV
488700930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
488800050902     c
488900050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
489000100224     c                   if        d98cca='2' and w98nri>0
489100100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
489200080116     c***                z-sub     w98nri        w98nri
489300080116     c                   z-add     0             w98nri
489400050902     c                   endif
489500111018     c* Ignoro ritardo se pefect order
489600130221     c                   if        d98xco >*zeros
489700111018     c                   z-add     0             w98nri
489800111018     c                   endif
489900061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
490000061121     c*  e sto leggendo la bolla in arrivo
490100100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
490200100224     c***                if        d98cca='1' and d98tbo='A' and
490300100224     c***                                         w98nri>0 and wpodcm<wpocri
490400080116     c***                z-sub     w98nri        w98nri
490500080116     c                   z-add     0             w98nri
490600061121     c                   endif
490700070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
490800070827     c*  mittente, --> consegnato in anticipo
490900070828     c                   if        d98tcr=' ' and d98nsp>0  and
491000070828     c                                            w98nri>0 and wpodcm<wpocri
491100070827     c                   if        CallAR5=' '
491200070827     c                   EXSR      LeggiAR5
491300070824     c                   endif
491400070827     c                   if        dcrpart<>'S'
491500070824     c                   z-sub     w98nri        w98nri
491600070824     c                   endif
491700070824     c                   endif
491800050907     c
491900050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
492000050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
492100050907     c*  lo trasformo in anticipo perchè non sono in ritardo
492200050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
492300050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
492400050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
492500050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
492600050907     c                   z-sub     w98nri        w98nri
492700050907     c                   sub       oreILoDOPO    w98nri
492800050907     c                   endif
492900050907     c
493000050907     C                   END                                                    <-- 2 ---
493100930524     C                   END                                                    <-- 1 ---
493200930420     C* CALCOLO LE ORE DI RESA
493300930420     C* CONTROLLO CONFORMITA
493400930423     C     W98NRI        IFLE      *ZEROS
493500930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
493600930420     C                   ELSE
493700930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
493800930420     C                   END
493900930420     C*
494000930420     C*-------------------------------------------------------------------
494100930420     C* CONFORMITA' RESA CLIENTE
494200930420     C*-------------------------------------------------------------------
494300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
494400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
494500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
494600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
494700050914     c                   if        consideraora='S'
494800050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
494900050914     c                   else
495000050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
495100050914     c                   endif
495200090827     c
495300080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
495400090827     c                   select
495500090827     c                   when      d98cca='2' and w98nrc>0
495600080116     c                   z-add     0             w98nrc
495700090827
495800080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
495900080116     c*  e sto leggendo la bolla in arrivo
496000090827     c                   when      d98cca='1' and d98tbo='A' and
496100080116     c                                            w98nrc>0
496200080116     c                   z-add     0             w98nrc
496300110923     c
496400110923     c* Ignoro ritardo se Perfect Order
496500130221     c                   when      d98xco >*zeros
496600110923     c                   z-add     0             w98nrc
496700090827     c
496800090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
496900090827     c* sono  in ritardo di 12 ore
497000091009     c                   when      d98tcc=10  and
497100091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
497200091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
497300091014     c                             d98hce=1030
497400091014     C                   MOVEL     'OSC  '       WR3TAB
497500091014     C                   eval      WR3HHI=d98hmc
497600091014     C                   EXSR      RO3ORE
497700091014     c                   if        wr3matpom='M' and d98hmc>1030
497800131218     c                   exsr      ctrLAVH1030
497900131218
498000131218     c                   if        wlav1030<>'S'
498100091014     c                   add       12            w98nrc
498200091014     c                   endif
498300131218     c                   endif
498400091014     c
498500091014     c**                 z-add     12            w98nrc
498600090827     c                   endsl
498700090827     c
498800930524     C                   ELSE                                                   <-- 1 -->
498900930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
499000930524     C* DI DIFFERENZA SONO UGUALI A ZERO
499100930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
499200931206     C     WPODCM        ANDLE     WPOCR2
499300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
499400930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
499500930525     C                   Z-ADD     WPODCM        WR2POI            9 0
499600931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
499700930525     C                   EXSR      RO2COV
499800930525     C     WR2NOV        IFNE      *ZEROS
499900930525     C     WR2NOV        MULT      -1            W98NRC
500000930525     C                   END
500100930524     C                   ELSE                                                   <-- 2 -->
500200930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
500300930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
500400930524     C     WPODCM        IFLT      WPOCRI
500500930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
500600930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
500700930524     C                   ELSE
500800931206     C                   Z-ADD     WPOCR2        WPORFI
500900930524     C                   Z-ADD     WPODCM        WPORFF
501000930524     C                   END
501100930524     C* CALCOLO ORE DI DIFFERENZA
501200930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
501300930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
501400930524     C                   Z-ADD     WPORFI        WR2POI            9 0
501500930524     C                   Z-ADD     WPORFF        WR2POF            9 0
501600930524     C                   EXSR      RO2COV
501700930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
501800050902     c
501900050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
502000100224     c                   if        d98cca='2' and w98nrc>0
502100100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
502200080116     c***                z-sub     w98nrc        w98nrc
502300080116     c                   z-add     0             w98nrc
502400050902     c                   endif
502500111018     c* Ignoro ritardo se pefect order
502600130221     c                   if        d98xco >*zeros
502700111018     c                   z-add     0             w98nrc
502800111018     c                   endif
502900070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
503000061121     c*  e sto leggendo la bolla in arrivo
503100100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
503200100224     c***                if        d98cca='1' and d98tbo='A' and
503300100224     c***                                         w98nrc>0 and wpodcm<wpocri
503400080116     c***                z-sub     w98nrc        w98nrc
503500080116     c                   z-add     0             w98nrc
503600061121     c                   endif
503700070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
503800070827     c*  mittente, --> consegnato in anticipo
503900070828     c                   if        d98tcr=' ' and d98nsp>0 and
504000070828     c                                            w98nrc>0 and wpodcm<wpocri
504100070827     c                   if        CallAR5=' '
504200070827     c                   EXSR      LeggiAR5
504300070827     c                   endif
504400070827     c                   if        dcrpart<>'S'
504500070824     c                   z-sub     w98nrc        w98nrc
504600070824     c                   endif
504700070824     c                   endif
504800050907     c
504900050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
505000050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
505100050907     c*  lo trasformo in anticipo perchè non sono in ritardo
505200050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
505300050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
505400050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
505500050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
505600050907     c                   z-sub     w98nrc        w98nrc
505700050907     c                   sub       oreILoDOPO    w98nrc
505800050907     c                   endif
505900050907     c
506000050907     C                   END                                                    <-- 2 ---
506100930524     C                   END                                                    <-- 1 ---
506200930420     C* CALCOLO LE ORE DI RESA
506300930420     C* CONTROLLO CONFORMITA
506400930423     C     W98NRC        IFLE      *ZEROS
506500930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
506600930420     C                   ELSE
506700930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
506800930420     C                   END
506900930420     C*
507000930420     C                   ENDSR
507100070827     c
507200131218     c********************************************************************
507300131218     c     CtrlavH1030   BEGSR
507400131218     c********************************************************************
507500131218     c                   clear                   wlav1030          1
507600140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
507700131218     c                   z-add     1             xx                4 0
507800131218     c                   dow       evc(xx)<>*blanks
507900140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
508000140127     c                   if        evd(xx)=d98dcm
508100131218     c                   if        evo(xx)<=1030
508200131218     c                   eval      wlav1030='S'
508300131218     c                   leave
508400131218     c                   endif
508500131218     c                   endif
508600131218     c                   add       1             xx
508700131218     c                   enddo
508800131218     c                   ENDSR
508900070827     c********************************************************************
509000070827     c* Controllo record GEN di FIAR5
509100070827     c********************************************************************
509200070827     c     LeggiAR5      BEGSR
509300070827     c                   clear                   DCRPART           1
509400141104     c                   clear                   dar5gen
509500141104     c                   clear                   gc1PART           2
509600141104     c                   clear                   gc2PART           2
509700070827     c     kar5          chain     fiar501l
509800070827    1c                   if        not %found
509900070827     c                   eval      dcrpart='S'
510000141104     c                   eval      gc1part=d98gc1
510100141104     c                   eval      gc2part=d98gc2
510200070827   x1c                   else
510300070827     c* i dati devono essere quelli immessi dalla partenza
510400070827     c                   movel     ar5uni        dar5gen
510500070827     c
510600070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
510700070827     c*  ma il record GEN è stato scritto per altri motivi
510800070827    2c                   if        §ar5mdp='S'
510900070827     c                   movel     d98dcr        alfadcr           8
511000141104     c                   eval      gc1part=§ar5gc1
511100141104     c                   eval      gc2part=§ar5gc2
511200070827     c
511300070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
511400070827     c                   eval      dcrpart='S'
511500070827    3c                   endif
511600070827     c
511700070827   x2c                   else
511800070827     c                   eval      dcrpart='S'
511900141106     c                   eval      gc1part=d98gc1
512000141106     c                   eval      gc2part=d98gc2
512100070827    2c                   endif
512200070827    1c                   endif
512300070827     c
512400140402     c* Memorizzo il flag pickup 2
512500140402     c                   eval      wpick2=§AR5PK2
512600140402     c
512700150401     c*
512800150414     c**                 if        d98cas>0 or d98ffd<>' ' or
512900150414     c**                           d98tc1='A'  or d98tc2='A' or
513000150414     c**                           d98tc1='S'  or d98tc2='S'
513100160331     c                   eval      kar4trc='L'
513200150401     c     kar4          chain     fiar401l
513300160420    1c                   if        %found(fiar401l)
513400150401     c                   eval      dsbl4L=ar4not
513500160420   x1c                   else
513600160420    1c                   endif
513700160420
513800150401     c* Solo per fermo deposito verifico se inviato alert
513900150401     c                   if        d98ffd<>' '
514000150401     c                   clear                   fnlv82ds
514100150401     c                   eval      ILV82AAS=d98aas
514200150401     c                   eval      ILV82LNP=d98lnp
514300150401     c                   eval      ILV82NRS=d98nrs
514400150401     c                   eval      ILV82NSP=d98nsp
514500150401     c                   eval      ILV82TIPA='FD'
514600150401     c                   call      'FNLV82R'
514700150401     c                   parm                    fnlv82ds
514800170821     c* inoltre se stato '0' o '1', verifico se sono arrivati tutti i colli
514900170821     c*  alla lna finale
515000170821     c                   if        olv82sts= '0' or olv82sts='1'
515100170822     c                   clear                   fnlrj6ds
515200170821     c                   if        d98dam>0
515300170821     c                   eval      ilrj6tla ='L'
515400170822     c                   eval      ilrj6tric= 'A'
515500170821     c                   eval      ilrj6aas=d98aas
515600170821     c                   eval      ilrj6lnp=d98lnp
515700170821     c                   eval      ilrj6nrs=d98nrs
515800170821     c                   eval      ilrj6nsp=d98nsp
515900170821     c                   call      'FNLRJ6R'
516000170821     c                   parm                    fnlrj6ds
516100150401     c                   endif
516200170821     c                   endif
516300170821     c
516400170821     c                   endif
516500150414     c**                 endif
516600150401
516700150401     c                   eval      callar5='S'
516800160331     c
516900160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
517000160331     c                   if        %lookup(d98lnp:fildpd)>0
517100160331     c                   eval      kar4trc='I'
517200160331     c     kar4          chain     fiar401l
517300160331     c                   if        %found(fiar401l)
517400160331     c                   eval      dsbl4I=ar4not
517500160331     c                   endif
517600160401     c                   if        §b4trd='1' or §b4trd='2'
517700160401     c                   eval      wVP='S'
517800160401     c                   endif
517900160401     c
518000160331     c                   endif
518100150401
518200070827     c                   ENDSR
518300930913     C********************************************************************
518400930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
518500930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
518600930913     C*            DATA/ORA (FINE RANGE)
518700930913     C********************************************************************
518800930913     C     CALCNS        BEGSR
518900930914     C*
519000930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
519100930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
519200930914     C     WPODTI        IFEQ      *ZEROS
519300960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
519400930914     C                   ELSE
519500930914     C                   Z-ADD     WPODTI        WPOIRC
519600930914     C                   END
519700930914     C*
519800930914     C* SE NON ANCORA CONSEGNATA
519900930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
520000050922     c* in realtà nella data consegna c'e' la data fine calcolo
520100050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
520200930914     C     WPODCM        IFEQ      *ZEROS
520300050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
520400930914     C                   ELSE
520500930916     C                   Z-ADD     WPODCM        WPOFRC
520600930914     C                   END
520700930914     C*
520800930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
520900930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
521000930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
521100930914     C     WPODFC        ANDLT     WPOIRC
521200930914     C                   ELSE
521300930914     C*
521400930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
521500930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
521600930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
521700930914     C     WPODFC        ANDGT     WPOFRC
521800930914     C                   ELSE
521900930916     C*
522000930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
522100930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
522200930916     C     WPODFC        ANDGT     WPOFRC
522300930916     C                   ELSE
522400930916     C*
522500930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
522600930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
522700930916     C     WPODFC        ANDLT     WPOIRC
522800930916     C                   ELSE
522900930914     C*
523000930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
523100930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
523200930914     C     WPODIC        IFNE      *ZEROS
523300930914     C     WPODIC        ANDGT     WPOFRC
523400930914     C                   Z-ADD     WPOFRC        WPODIC
523500930914     C                   ENDIF
523600930914     C*
523700930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
523800930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
523900930914     C     WPODFC        IFGT      WPOFRC
524000930914     C                   Z-ADD     WPOFRC        WPODFC
524100930914     C                   ENDIF
524200930914     C*
524300930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
524400930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
524500930914     C     WPODIC        IFNE      *ZEROS
524600930914     C     WPODIC        ANDLT     WPOIRC
524700930914     C                   Z-ADD     WPOIRC        WPODIC
524800930914     C                   ENDIF
524900930914     C*
525000930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
525100930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
525200930914     C     WPODFC        IFLT      WPOIRC
525300930914     C                   Z-ADD     WPOIRC        WPODFC
525400930914     C                   ENDIF
525500930914     C*
525600930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
525700931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
525800930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
525900930916     C*
526000931210     C***********WPODIC    IFNE *ZEROS
526100931210     C***********WPODFC    ANDNE*ZEROS
526200931210     C*********************ADD  12        WPODFC
526300931210     C*********************ENDIF
526400930914     C     WPODIC        IFEQ      *ZEROS
526500930914     C     WPODFC        ANDNE     *ZEROS
526600931130     C                   Z-ADD     WPODFC        WPODIC
526700931210     C*********************ADD  12        WPODFC
526800930916     C                   ENDIF
526900930913     C*
527000930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
527100930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
527200930914     C* INTERNA
527300050908     c                   eval      Usacons='S'
527400930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
527500930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
527600931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
527700931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
527800940324     C                   EXSR      RO9COV
527900931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
528000930914     C* CLIENTE
528100930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
528200931210     C                   EXSR      RO9COV
528300931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
528400050908     c                   eval      Usacons=' '
528500930914     C*
528600930916     C                   ENDIF                                                  <-- 5 ---
528700930916     C                   ENDIF                                                  <-- 4 ---
528800930916     C                   ENDIF                                                  <-- 3 ---
528900930914     C                   ENDIF                                                  <-- 2 ---
529000930914     C                   ENDIF                                                  <-- 1 ---
529100930913     C*
529200930913     C                   ENDSR
529300930325     C********************************************************************
529400930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
529500930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
529600930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
529700930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
529800930325     C********************************************************************
529900930325     C     RO1CDP        BEGSR
530000930414     C*
530100931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
530200931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
530300931126     C*
530400931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
530500931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
530600150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
530700150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
530800931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
530900060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
531000930326     C*
531100930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
531200931130     C                   Z-ADD     WR1POI        WR1POO            9 0
531300930325     C*
531400930325     C* CICLO FINO A CHE:
531500930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
531600930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
531700930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
531800930406     C     WR1POO        ANDLT     WPOLIM
531900930414     C*
532000930407     C                   Z-ADD     WR1POO        WPOROU            9 0
532100930325     C                   MOVEL     'N'           WSLITT            1
532200930414     C*
532300930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
532400060420     C*
532500931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
532600150605     c     wdee          andeq     ' '
532700930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
532800930414     C                   END
532900930511     C*
533000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
533100150608     c     wdee          andeq     ' '
533200930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
533300930511     C                   END
533400930414     C*
533500930325     C     WSLITT        IFEQ      'N'
533600930423     C                   SUB       12            WR1NOI            5 0
533700930325     C                   END
533800931130     C                   ADD       12            WR1POO            9 0
533900930414     C*
534000931223     C                   ENDDO                                                  <-- 1 ---
534100931126     C*
534200931126     C                   ELSE                                                   <-- 0 -->
534300931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
534400931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
534500931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
534600960129     C     WPOLAF        IFGT      WPODCG
534700960129     C                   Z-ADD     WPOLAF        WTM090            9 0
534800960129     C                   ELSE
534900960129     C                   Z-ADD     WPODCG        WTM090
535000960129     C                   ENDIF
535100960129     C     WPOGIF        IFGT      WTM090
535200960129     C                   Z-ADD     WPOGIF        WTM090
535300960129     C                   ENDIF
535400060420     C     WPOpaif       IFGT      WTM090
535500060420     C                   Z-ADD     WPOpaif       WTM090
535600060420     C                   ENDIF
535700960129     C*
535800960129     C     12            ADD       WTM090        WR1POO
535900960129     C                   ENDIF                                                  <-- 0 ---
536000930512     C*
536100931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
536200931202     C     WR1POO        SUB       12            WR7POI
536300931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
536400931202     C                   ADD       12            WR7POO
536500931202     C     WR7POO        IFGT      WR1POO
536600931202     C                   Z-ADD     WR7POO        WR1POO
536700931202     C                   END
536800930406     C*
536900930325     C                   ENDSR
537000930414     C********************************************************************
537100930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
537200930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
537300930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
537400930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
537500930414     C********************************************************************
537600930414     C     RO2COV        BEGSR
537700091014     c                   clear                   salvaEveL
537800091014     c                   clear                   salvaEveG
537900931130     C*
538000930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
538100930414     C                   Z-ADD     *ZEROS        WR2NOV
538200930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
538300931130     C                   Z-ADD     WR2POI        WR2POW
538400930414     C*
538500930414     C* CICLO FINO A CHE:
538600930414     C*   - DATA INIZIO SUPERATO DATA FINE
538700930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
538800931223     C     WR2POW        ANDLT     WPOLIM
538900930414     C*
539000930414     C                   Z-ADD     WR2POW        WPOROU
539100930414     C                   MOVEL     'N'           WSLITT            1
539200930414     C*
539300930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
539400930414     C*
539500930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
539600930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
539700930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
539800930414     C                   END
539900930414     C*
540000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
540100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
540200930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
540300930618     C                   END
540400060420     c
540500060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
540600060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
540700060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
540800060420     C                   END
540900060420     c
541000931201     C*
541100931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
541200931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
541300931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
541400931201     C                   END
541500060418     c
541600060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
541700060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
541800060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
541900060418     C                   END
542000930414     C*
542100930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
542200930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
542300930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
542400930414     C                   END
542500930511     C*
542600930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
542700930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
542800930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
542900930511     C                   END
543000930414     C*
543100930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
543200930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
543300930414     C     WSLITT        IFEQ      'N'
543400930423     C                   ADD       12            WR2NOV            5 0
543500930414     C                   END
543600931130     C                   ADD       12            WR2POW            9 0
543700930414     C*
543800931223     C                   ENDDO                                                  <-- 1 ---
543900930414     C*
544000930414     C                   ENDSR
544100941228     C********************************************************************
544200941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
544300941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
544400941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
544500941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
544600941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
544700941228     C********************************************************************
544800941228     C     RO2COP        BEGSR
544900941228     C*
545000941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
545100941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
545200941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
545300941228     C                   Z-ADD     *ZEROS        WR2NOV
545400941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
545500941228     C                   Z-ADD     WR2POI        WR2POW
545600941228     C*
545700941228     C* CICLO FINO A CHE:
545800941228     C*   - DATA INIZIO SUPERATO DATA FINE
545900941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
546000941228     C*
546100950215     C                   Z-ADD     WR2POW        WPOROU
546200941228     C                   MOVEL     'N'           WSLITT
546300941228     C* TRASFORMA IL PROGR. ORE IN DATA
546400950215     C*******              Z-ADDWR2POW    WORE
546500950215     C*******              EXSR OREDAT
546600950215     C*******              Z-ADDWDAT      W12DAT  80
546700950215     C*******              Z-ADDWRES      W12RES  30
546800941228     C*
546900941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
547000941228     C*
547100941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
547200941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
547300941228     C     WSLITT        IFEQ      'N'
547400941228     C                   ADD       12            WR2NOV            5 0
547500941228     C                   END
547600941228     C                   ADD       12            WR2POW            9 0
547700941228     C*
547800941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
547900941228     C* ENTRI IN LOOP
548000941228     C     WR2NOV        IFGT      999
548100941228     C                   LEAVE                                                  *VA A ENDDO
548200941228     C                   END
548300941228     C*
548400941228     C                   ENDDO                                                  <-- 1 ---
548500941228     C*
548600941228     C                   ENDSR
548700930510     C********************************************************************
548800930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
548900930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
549000930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
549100930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
549200050905     c*                     il num ore deve essere un multiplo di 12
549300050905     c*                     oppure 1 ora
549400930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
549500930510     C********************************************************************
549600930510     C     RO4CDP        BEGSR
549700930510     C*
549800930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
549900930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
550000050905     c                   movel     '1'           wgiro             1
550100930510     C*
550200930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
550300930510     C                   Z-ADD     WR4POI        WR4POO            9 0
550400930510     C*
550500930510     C* CICLO FINO A CHE:
550600930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
550700930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
550800930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
550900930510     C     WR4POO        ANDLT     WPOLIM
551000930510     C*
551100141106     c                   if        nosomma<>'N'
551200931130     C                   ADD       12            WR4POO            9 0
551300141106     c                   endif
551400930510     C                   Z-ADD     WR4POO        WPOROU            9 0
551500930510     C                   MOVEL     'N'           WSLITT            1
551600930510     C*
551700930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
551800050905     c
551900050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
552000930510     C     WSLITT        IFEQ      'N'
552100050905     c
552200050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
552300050905     c*  se cado in giorno non festivo
552400050905     c                   if        wr4noi=1 and wgiro='1'
552500050905     c                   sub       12            wr4poo
552600050905     c                   add       wr4noi        wr4poo
552700050905     c                   endif
552800050905     c*
552900050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
553000050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
553100050905     c*  calcolate dal pgm
553200050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
553300050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
553400050905     c*  del giorno sucessivo non festivo
553500050905     c                   if        wr4noi=1 and wgiro<>'1'
553600050905     c                   add       wr4noi        wr4poo
553700050905     c                   endif
553800050905     c
553900930510     C                   SUB       12            WR4NOI            5 0
554000930510     C                   END
554100141106     c
554200141106     c                   if        nosomma= 'N'
554300141106     C                   ADD       12            WR4POO            9 0
554400141106     c                   endif
554500930510     C*
554600050905     c                   eval      wgiro='2'
554700931223     C                   ENDDO                                                  <-- 1 ---
554800930510     C*
554900930510     C                   ENDSR
555000931130     C********************************************************************
555100931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
555200931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
555300931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
555400931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
555500931130     C********************************************************************
555600931130     C     RO5COV        BEGSR
555700931130     C*
555800931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
555900060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
556000060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
556100060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
556200931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
556300931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
556400931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
556500931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
556600931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
556700931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
556800931130     C                   Z-ADD     *ZEROS        WR5NOV
556900940714     C*
557000940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
557100940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
557200931207     C*
557300931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
557400940908     C     WIC           IFEQ      'I'
557500090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
557600090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
557700931207     C                   ELSE
557800090827     C*****              Z-ADD     D98TCC        WPOTCT
557900090827     C                   Z-ADD     w99TCC        WPOTCT
558000931207     C                   END
558100931130     C*
558200931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
558300060420     C                   clear                   COV
558400931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
558500931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
558600931207     C                   Z-ADD     2             CO                3 0
558700931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
558800931207     C                   EXSR      RO5SUB
558900981102     C                   DO        99            IC                3 0
559000931207     C     LAF(IC)       IFNE      *ZEROS
559100931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
559200931207     C                   EXSR      RO5SUB
559300931207     C                   ENDIF
559400931207     C                   ENDDO
559500060420     c
559600060420     C                   DO        99            IC                3 0
559700060420     C     paif(IC)      IFNE      *ZEROS
559800060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
559900060420     C                   EXSR      RO5SUB
560000060420     C                   ENDIF
560100060420     C                   ENDDO
560200931207     C* ORDINA LA SCHIERA COV
560300931207     C                   SORTA     COV
560400931130     C*
560500931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
560600931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
560700981102     C                   DO        98            IC                             --- 1 -->
560800931207     C     IC            ADD       1             I1C               3 0
560900931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
561000931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
561100931207     C     COV(I1C)      OREQ      *ZEROS
561200931207     C                   ELSE
561300931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
561400931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
561500931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
561600931130     C                   EXSR      RO2COV
561700931207     C                   ADD       WR2NOV        WR5NOV            9 0
561800931209     C* SE NON SONO IN DATA CONSEGNA
561900931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
562000931207     C     WR5NOV        IFLT      WPOTCT
562100931209     C     COV(I1C)      ANDNE     WPODCM
5622009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5623009709111009 C     WPOTCT        SUB       12            WR5NOV
562400931130     C                   END
562500931207     C                   ENDIF                                                  <-- 2 ---
562600931207     C                   ENDDO                                                  <-- 1 ---
562700940714     C                   ENDIF                                                  <-- 0 ---
562800931130     C*
562900931130     C                   ENDSR
563000931207     C********************************************************************
563100931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
563200931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
563300931207     C********************************************************************
563400931207     C     RO5SUB        BEGSR
563500931207     C*
563600931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
563700931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
563800931207     C     WR5COV        ANDLT     WR5POF
563900931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
564000931207     C                   MOVEL     'N'           WR5PRE            1
564100981102     C*---                 DO   30        I
564200981102     C                   DO        99            I
564300931207     C     WR5COV        IFEQ      COV(I)
564400931207     C                   MOVEL     'S'           WR5PRE
564500931207     C                   LEAVE
564600931207     C                   ENDIF
564700931207     C                   ENDDO
564800931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
564900931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
565000931207     C                   ADD       1             CO                3 0
565100931207     C                   Z-ADD     WR5COV        COV(CO)
565200931207     C                   ENDIF                                                  <-- 2 ---
565300931207     C                   ENDIF                                                  <-- 1 ---
565400931207     C*
565500931207     C                   ENDSR
565600931201     C********************************************************************
565700060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
565800931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
565900931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
566000931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
566100931201     C********************************************************************
566200931201     C     RO6CDT        BEGSR
566300931201     C*
566400931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
566500931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
566600931201     C*
566700931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
566800931201     C                   Z-ADD     WR6POI        WR6POO            9 0
566900931201     C*
567000931201     C* CICLO FINO A CHE:
567100931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
567200931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
567300931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
567400931201     C     WR6POO        ANDLT     WPOLIM
567500931201     C*
567600931201     C                   Z-ADD     WR6POO        WPOROU            9 0
567700931201     C                   MOVEL     'N'           WSLITT            1
567800931201     C*
567900931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
568000931201     C*
568100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
568200150605     c     wdee          andeq     ' '
568300931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
568400931201     C                   END
568500931201     C*
568600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
568700150608     c     wdee          andeq     ' '
568800931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
568900931201     C                   END
569000931201     C*
569100931201     C     WSLITT        IFEQ      'N'
569200931201     C                   SUB       12            WR6NOI            5 0
569300931201     C                   END
569400931201     C                   ADD       12            WR6POO            9 0
569500931201     C*
569600931223     C                   ENDDO                                                  <-- 1 ---
569700931201     C*
569800931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
569900931202     C     WR6POO        SUB       12            WR7POI
570000931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
570100931207     C                   ADD       12            WR7POO
570200931202     C     WR7POO        IFGT      WR6POO
570300931201     C                   Z-ADD     WR7POO        WR6POO
570400931202     C                   END
570500931201     C*
570600931201     C                   ENDSR
570700931201     C********************************************************************
570800060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
570900060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
571000060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
571100060407     C*            INPUT:  -WR7POI (PROGR.ORE)
571200931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
571300931201     C********************************************************************
571400931201     C     RO7TCF        BEGSR
571500060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
571600060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
571700060407     C                   MOVEL     'N'           WSLITT
571800931201     C*
571900931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
572000931201     C                   Z-ADD     WR7POI        WR7POO            9 0
572100931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
572200931201     C* COINCIDA CON UN TURNO DI CHIUSURA
572300060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
572400931201     C     D98GC2        ANDEQ     *BLANKS
572500060407   x1C                   ELSE                                                   <-- 1 -->
572600931214     C*
572700931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
572800931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
572900931214     C*
573000931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
573100931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
573200931223     C     WPOROU        ANDLT     WPOLIM
573300931214     C* MEMORIZZO INIZIO E FINE RANGE
573400931202     C     WPOTI         IFEQ      *ZEROS
573500931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
573600931202     C                   END
573700931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
573800931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
573900931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
574000931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
574100931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
574200931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
574300931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
574400931201     C                   MOVEL     'N'           WSLITT
574500931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
574600060407     c* Se non trovat, controllo se c'e' evento part che giustifica
574700060407     c                   if        wslitt='N'
574800060407     c                   exsr      datcepg
574900060407     c                   endif
575000060407     c
575100931201     C                   ENDDO                                                  <-- 2 ---
575200931214     C*
575300931201     C                   Z-ADD     WPOROU        WR7POO
575400931214     C*
575500931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
575600931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
575700931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
575800931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
575900931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
576000931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
576100931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
576200931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
576300931209     C                   ELSE                                                   <-- 3 -->
576400931209     C     WIC           IFEQ      'I'
576500931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
576600931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
576700931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
576800931201     C                   ELSE
576900931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
577000931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
577100931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
577200931201     C                   ENDIF
577300931209     C                   ENDIF                                                  <-- 3 ---
577400931209     C                   ENDIF                                                  <-- 2 ---
577500931214     C*
577600060407    1C                   ENDIF                                                  <-- 1 ---
577700060407     c
577800060407     c* Se non trovato turno di chiusura, verifico se c'e'
577900060407     c*  evento particolare che GIUSTIFICA
578000060407     c                   if        wpoti=0
578100060418     C                   EXSR      RO7CEPG
578200060407     c                   endif
578300931201     C*
578400931201     C                   ENDSR
578500060406     C********************************************************************
578600060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
578700060406     c*                        che giustifica
578800060407     C*            INPUT:-WR7POI (PROGR.ORE)
578900060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
579000060406     C********************************************************************
579100060407     C     RO7CEPG       BEGSR
579200060406     C*
579300060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
579400060407     C                   Z-ADD     WR7POI        WR7POO            9 0
579500060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
579600060406     C* coincide
579700060406    1C                   if        wevpag='S'                                   <-- 1 -->
579800060406     C*
579900060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
580000060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
580100060406     C                   MOVEL     'N'           WSLITT
580200060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
580300060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
580400060406     C*
580500060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
580600060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
580700060406     C     WPOROU        ANDLT     WPOLIM
580800060406     C* MEMORIZZO INIZIO E FINE RANGE
580900060406    3C     WPOTI         IFEQ      *ZEROS
581000060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
581100060406    3C                   END
581200060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
581300060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
581400060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
581500060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
581600060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
581700060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
581800060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
581900060406     C                   MOVEL     'N'           WSLITT
582000060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
582100060406     C* Se non trovato,
582200060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
582300060406    3c                   if        wslitt='N'
582400060406     c                   exsr      DATTUC
582500060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
582600060406    2C                   ENDDO                                                  <-- 2 ---
582700060406     C*
582800060407     C                   Z-ADD     WPOROU        WR7POO
582900060406     C*
583000060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
583100060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
583200060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
583300060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
583400060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
583500060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
583600060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
583700060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
583800060406   x3C                   ELSE                                                   <-- 3 -->
583900060406    4C     WIC           IFEQ      'I'
584000060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
584100060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
584200060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
584300060406   x4C                   ELSE
584400060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
584500060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
584600060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
584700060406    4C                   ENDIF
584800060406    3C                   ENDIF                                                  <-- 3 ---
584900060406    2C                   ENDIF                                                  <-- 2 ---
585000060406     C*
585100060406    1C                   ENDIF                                                  <-- 1 ---
585200060406     C*
585300060406     C                   ENDSR
585400931207     C********************************************************************
585500931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
585600931207     C*            INPUT:  -WR8POI (PROGR.ORE)
585700931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
585800931207     C********************************************************************
585900931207     C     RO8FES        BEGSR
586000130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
586100130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
586200931207     C*
586300931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
586400130411     C     WR8POI        SUB       12            WR8POO            9 0
586500931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
586600931207     C                   MOVEL     'N'           WSLITT
586700130411     c
586800130411     c* Non salto i festivi ???
586900130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
587000130411     c
587100931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
587200130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
587300130411     C     WPOROU        ANDGT     WPODSP
587400130411     C                   SUB       12            WPOROU
587500130411     C                   MOVEL     'N'           WSLITT
587600130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
587700130411     C                   ENDDO                                                  <-- 2 ---
587800130411     c
587900130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
588000130411     C     D98GC2        ANDEQ     *BLANKS
588100130411   x1C                   ELSE                                                   <-- 1 -->
588200130411     c* eseguo controllo per i turni di chiusura
588300130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
588400130411     C*
588500130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
588600130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
588700130411     C     WPOROU        ANDgT     WPOdsp
588800130411     C* MEMORIZZO INIZIO E FINE RANGE
588900130411     C     WPOTf         IFEQ      *ZEROS
589000130411     C                   z-add     wporou        WPOTf
589100130411     C                   END
589200140826     C     WPOROU        SUB       wnoadd        WPOROU
589300130411     c
589400130411     C                   MOVEL     'N'           WSLITT
589500130411     c                   exsr      dattuc
589600130411     c                   if        wslitt='N'
589700130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
589800140826     c* Se slitta per festivo --> tolgo sempre 12 ore
589900140826     c                   z-add     12            WNOADD
590000130411     c                   endif
590100130411     C* SLITTA
590200140826     C     wporou        add       12            WPOTi
590300130411    2C                   ENDDO                                                  <-- 2 ---
590400130411     c
590500130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
590600130411     c
590700130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
590800130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
590900130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
591000130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
591100130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
591200130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
591300130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
591400130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
591500130411   x3C                   ELSE                                                   <-- 3 -->
591600130411    4C     WIC           IFEQ      'I'
591700130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
591800130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
591900130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
592000130411   x4C                   ELSE
592100130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
592200130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
592300130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
592400130411    4C                   ENDIF
592500130411    3C                   ENDIF                                                  <-- 3 ---
592600130411    2C                   ENDIF                                                  <-- 3 ---
592700130411     C*
592800130411    1C                   ENDIF                                                  <-- 1 ---
592900130411
593000130411     C**   WPOROU        ADD       12            WR8POO
593100130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
593200130411     c                   z-add     wr8poi        wr8poo
593300931207     C*
593400931207     C                   ENDSR
593500931210     C********************************************************************
593600931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
593700940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
593800931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
593900931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
594000931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
594100931210     C********************************************************************
594200931210     C     RO9COV        BEGSR
594300931210     C*
594400931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
594500931210     C                   Z-ADD     *ZEROS        WR9NOV
594600931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
594700931210     C                   Z-ADD     WR9POI        WR9POW
594800931210     C* CICLO FINO A CHE:
594900931210     C*   - DATA INIZIO SUPERATO DATA FINE
595000931223     C*   - RAGGIUNTO LIMITE MASSIMO
595100931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
595200931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
595300931223     C     WR9POW        ANDLT     WPOLIM
595400931210     C*
595500931210     C                   Z-ADD     WR9POW        WPOROU
595600931210     C                   MOVEL     'N'           WSLITT            1
595700940324     C* FESTIVI
595800931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
595900940324     C* GIACENZA
596000931214     C     WSLITT        IFEQ      'N'
596100941229£$   C     WPOAGC        IFNE      *ZEROS
596200940324£$   C     WPOCGC        ORNE      *ZEROS
596300941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
596400940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
596500940324£$   C                   MOVEL     'S'           WSLITT
596600940324£$   C                   END
596700940324£$   C                   END
596800940324£$   C                   END
596900960129     C* EVENTI GIACENZA
597000960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
597100960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
597200960129     C                   Z-ADD     *ZEROS        G                 3 0
597300960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
597400960129     C     G             ANDLT     IG
597500960129     C                   ADD       1             G
597600960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
597700960209     C     WPOROU        ANDLE     GCF(G)
597800960129     C                   MOVEL     'S'           WSLITT
597900960129     C                   ENDIF                                                  <-- 3 ---
598000960129     C                   ENDDO                                                  <-- 2 ---
598100960129     C                   ENDIF                                                  <-- 1 ---
598200960129     C                   ENDIF                                                  <-- 0 ---
598300940324     C* LASCIATO AVVISO
598400931214     C     WSLITT        IFEQ      'N'
598500941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
598600940324£$   C                   Z-ADD     *ZEROS        L                 3 0
598700940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
598800940324£$   C     L             ANDLT     IL
598900940324£$   C                   ADD       1             L
599000941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
599100941229£$   C     WPOROU        ANDLE     LCF(L)
599200050520     c     LCon(L)       andeq     'S'
599300940324£$   C                   MOVEL     'S'           WSLITT
599400940324£$   C                   END                                                    <-- 3 ---
599500940324£$   C                   ENDDO                                                  <-- 2 ---
599600940324£$   C                   END                                                    <-- 1 ---
599700931210     C                   END
599800940324     C* TURNO CHIUSURA
599900140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
600000140826     c*  di sistemare le ore da sommare 12 o 24...
600100931214     C     WSLITT        IFEQ      'N'
600200931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
600300931210     C                   END
600400940324     C* DATA CONSEGNA RICHIESTA
600500050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
600600050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
600700050907     c
600800931214     C     WSLITT        IFEQ      'N'
600900050907     c                   add       1             wporou
601000940324£$   C     WPOCRI        IFNE      *ZEROS
601100940324£$   C     WPOROU        IFLT      WPOCRC
601200940324£$   C                   MOVEL     'S'           WSLITT
601300940324£$   C                   END
601400940324£4   C                   END
601500050907     c                   sub       1             wporou
601600931210     C                   END
601700931210     C*
601800931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
601900931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
602000931210     C     WSLITT        IFEQ      'N'
602100931210     C                   ADD       12            WR9NOV            5 0
602200931210     C                   END
602300931210     C                   ADD       12            WR9POW            9 0
602400931210     C*
602500931223     C                   ENDDO                                                  <-- 1 ---
602600931210     C*
602700931210     C                   ENDSR
602800940628%%%  C********************************************************************
602900940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
603000940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
603100940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
603200940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
603300940628%%%  C********************************************************************
603400940628%%%  C     RO0GES        BEGSR
603500940628%%%  C*
603600940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
603700940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
603800940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
603900940628%%%  C*
604000940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
604100940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
604200940628%%%  C*
604300940628%%%  C* CICLO FINO A CHE:
604400940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
604500940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
604600940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
604700050922%%%  C     WR0POO        ANDGe     WPODSP
604800940628%%%  C*
604900940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
605000940628%%%  C                   MOVEL     'N'           WSLITT            1
605100940628%%%  C*
605200940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
605300940628%%%  C*
605400940628%%%  C     WSLITT        IFEQ      'N'
605500940628%%%  C                   SUB       12            WR0NOI            5 0
605600940628%%%  C                   END
605700940628%%%  C                   SUB       12            WR0POO            9 0
605800940628%%%  C*
605900940628%%%  C                   ENDDO                                                  <-- 1 ---
606000940628%%%  C*
606100940628%%%  C                   ADD       12            WR0POO            9 0
606200940628%%%  C*
606300940628%%%  C                   ENDSR
606400960911     C********************************************************************
606500960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
606600960911     C*            INPUT:  -W10POI (PROGR.ORE)
606700960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
606800960911     C********************************************************************
606900960911     C     R10FES        BEGSR
607000960911     C*
607100960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
607200960911     C                   Z-ADD     W10POI        W10POO            9 0
607300960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
607400960911     C                   MOVEL     'N'           WSLITT
607500960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
607600960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
607700960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
607800960911     C                   ADD       12            WPOROU
607900960911     C                   MOVEL     'N'           WSLITT
608000960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
608100960911     C                   ENDDO                                                  <-- 2 ---
608200960911     C                   Z-ADD     WPOROU        W10POO
608300960911     C*
608400960911     C                   ENDSR
608500930325     C********************************************************************
608600930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
608700930325     C********************************************************************
608800930325     C     CHKGIA        BEGSR
608900930406     C*
609000960129     C* APERTURA/CHIUSURA GIACENZA
609100930326     C     WPODAG        IFNE      *ZEROS
609200930406     C     WPODCG        ORNE      *ZEROS
609300930407     C     WPOROU        IFGE      WPODAG
609400930407     C     WPOROU        ANDLE     WPODCG
609500930325     C                   MOVEL     'S'           WSLITT
609600930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
609700930325     C                   END
609800930326     C                   END
609900960129     C*
610000960129     C* EVENTI GIACENZA
610100960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
610200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
610300960129     C                   Z-ADD     *ZEROS        G                 3 0
610400960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
610500960129     C     G             ANDLT     IG
610600960129     C                   ADD       1             G
610700960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
610800960129     C     WPOROU        ANDLE     GIF(G)
610900091009     c
611000091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
611100091009     c*  l'orario dell'evento
611200091009     c                   select
611300091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
611400091009     c* Se orario entro le 10.30  va bene
611500091014     c*  solo la prima volta per evento cioè quando controllo
611600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
611700091014     c*  e non devo dare errore
611800091014     C                   MOVEL     'OSC  '       WR3TAB
611900091014     C                   eval      WR3HHI=evo(gip(G))
612000091014     C                   EXSR      RO3ORE
612100091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
612200091014     c*  in ritardo come per "E")
612300091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
612400091014     c                              wr3matpom='P'
612500091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
612600091009     C                   MOVEL     'S'           WSLITT
612700091009     c                   endif
612800091014     c                   eval      SalvaEveG=gip(G)
612900091009     c
613000091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
613100091009     c* Se orario entro le 10.30  va bene
613200091014     c*  solo la prima volta per evento cioè quando controllo
613300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
613400091014     c*  e non devo dare errore
613500091014     C                   MOVEL     'OSC  '       WR3TAB
613600091014     C                   eval      WR3HHI=evo(gip(G))
613700091014     C                   EXSR      RO3ORE
613800091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
613900091014     c*  in ritardo come per "E")
614000091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
614100091014     c                              wr3matpom='P'
614200091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
614300091009     C                   MOVEL     'S'           WSLITT
614400091009     c                   endif
614500091014     c                   eval      SalvaEveG=gip(G)
614600091009     c
614700091009     c                   other
614800960129     C                   MOVEL     'S'           WSLITT
614900960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
615000091009     c                   endsl
615100091009
615200960129     C                   ENDIF                                                  <-- 3 ---
615300960129     C                   ENDDO                                                  <-- 2 ---
615400960129     C                   ENDIF                                                  <-- 1 ---
615500960129     C                   ENDIF                                                  <-- 0 ---
615600930406     C*
615700930325     C                   ENDSR
615800060420     C********************************************************************
615900930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
616000930325     C********************************************************************
616100930325     C     CHKLAV        BEGSR
616200930325     C*
616300941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
616400930421     C                   Z-ADD     *ZEROS        L                 3 0
616500930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
616600930421     C     L             ANDLT     IL
616700930421     C                   ADD       1             L
616800930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
616900930421     C     WPOROU        ANDLE     LAF(L)
617000090928     c
617100090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
617200091009     c*  l'orario dell'evento
617300091009     c                   select
617400091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
617500091009     c* Se orario entro le 10.30  va bene
617600091014     c*  solo la prima volta per evento cioè quando controllo
617700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
617800091014     c*  e non devo dare errore
617900091014     C                   MOVEL     'OSC  '       WR3TAB
618000091014     C                   eval      WR3HHI=evo(lap(L))
618100091014     C                   EXSR      RO3ORE
618200091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
618300091014     c*  in ritardo come per "E")
618400091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
618500091014     c                              wr3matpom='P'
618600091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
618700091009     C                   MOVEL     'S'           WSLITT
618800091009     c                   endif
618900091014     c                   eval      SalvaEveL=lap(L)
619000091009     c
619100091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
619200091009     c* Se orario entro le 10.30  va bene
619300091014     c*  solo la prima volta per evento cioè quando controllo
619400091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
619500091014     c*  e non devo dare errore
619600091014     C                   MOVEL     'OSC  '       WR3TAB
619700091014     C                   eval      WR3HHI=evo(lap(L))
619800091014     C                   EXSR      RO3ORE
619900091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
620000091014     c*  in ritardo come per "E")
620100091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
620200091014     c                              wr3matpom='P'
620300091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
620400091009     C                   MOVEL     'S'           WSLITT
620500091009     c                   endif
620600091014     c                   eval      SalvaEveL=lap(L)
620700090928     c
620800091009     c                   other
620900930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
621000091009     C                   MOVEL     'S'           WSLITT
621100091009     c                   endsl
621200090928     c
621300930421     C                   END                                                    <-- 3 ---
621400931223     C                   ENDDO                                                  <-- 2 ---
621500930421     C                   END                                                    <-- 1 ---
621600930325     C*
621700930325     C                   ENDSR
621800060420     C********************************************************************
621900060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
622000060420     c*                   che Allungano
622100060420     C********************************************************************
622200060420     C     CHKCEPA       BEGSR
622300060420     C*
622400060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
622500060420     C                   Z-ADD     *ZEROS        p                 3 0
622600060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
622700060420     C     p             ANDLT     IpA
622800060420     C                   ADD       1             P
622900060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
623000060420     C     WPOROU        ANDLE     paif(P)
623100060420     C                   MOVEL     'S'           WSLITT
623200060420     C                   ADD       12            W98EA
623300060420     C                   END                                                    <-- 3 ---
623400060420     C                   ENDDO                                                  <-- 2 ---
623500060420     C                   END                                                    <-- 1 ---
623600060420     C*
623700060420     C                   ENDSR
623800930414     C********************************************************************
623900930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
624000930414     C********************************************************************
624100930414     C     CHKCNT        BEGSR
624200930511     C*
624300930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
624400930611     C     WIC           IFEQ      'I'                                          *INTERNO
624500930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
624600930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
624700930611     C                   ELSE
624800930611     C     WPOROU        IFGE      WPOAII
624900930611     C     WPOROU        ANDLE     WPOAFI
625000930611     C                   MOVEL     'S'           WSLITT
625100930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
625200930611     C                   END
625300930611     C                   END
625400930611     C                   ELSE                                                   *CLIENTE
625500930611     C     WPOAIC        IFEQ      *ZEROS
625600930611     C     WPOAFC        ANDEQ     *ZEROS
625700930611     C                   ELSE
625800930611     C     WPOROU        IFGE      WPOAIC
625900930611     C     WPOROU        ANDLE     WPOAFC
626000930611     C                   MOVEL     'S'           WSLITT
626100930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
626200930611     C                   END
626300930611     C                   END
626400930611     C                   END
626500930414     C*
626600930414     C                   ENDSR
626700930511     C********************************************************************
626800930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
626900930511     C********************************************************************
627000930511     C     CHKFED        BEGSR
627100930511     C*
627200930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
627300930611     C     WIC           IFEQ      'I'                                          *INTERNO
627400930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
627500930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
627600930611     C                   ELSE
627700930611     C     WPOROU        IFGE      WPOFII
627800930611     C     WPOROU        ANDLE     WPOFFI
627900930611     C                   MOVEL     'S'           WSLITT
628000930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
628100930611     C                   END
628200930611     C                   END
628300930611     C                   ELSE                                                   *CLIENTE
628400930611     C     WPOFIC        IFEQ      *ZEROS
628500930611     C     WPOFFC        ANDEQ     *ZEROS
628600930611     C                   ELSE
628700930611     C     WPOROU        IFGE      WPOFIC
628800930611     C     WPOROU        ANDLE     WPOFFC
628900930611     C                   MOVEL     'S'           WSLITT
629000930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
629100930611     C                   END
629200930611     C                   END
629300930611     C                   END
629400930511     C*
629500930511     C                   ENDSR
629600930325     C********************************************************************
629700930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
629800930325     C********************************************************************
629900930325     C     CHKFES        BEGSR
630000060526     c* se già passata la data, non controllo wporou
630100060526     c                   if        wpodata>0
630200060526     c                   z-add     wpodata       w12dat
630300060526     c                   clear                   w12res
630400060526     c                   else
630500931130     C*
630600931130     C* TRASFORMA IL  PROGR.ORE IN DATA
630700931130     C                   Z-ADD     WPOROU        WORE
630800931130     C                   EXSR      OREDAT
630900941227     C                   Z-ADD     WDAT          W12DAT            8 0
631000931130     C                   Z-ADD     WRES          W12RES            3 0
631100060526     c                   endif
631200930325     C*
631300941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
631400930325     C* E' FESTIVO
631500170117     c* l'affidabilità partenza ora fa un doppio tentativo
631600170117    1C     WTC           IFEQ      'P'                                          *PARTENZA
631700170117     C                   Z-ADD     D98LNP        CLNTFP
631800170117     C                   Z-ADD     *ZEROS        CLNTFA
631900170117     c                   exsr      azcln_fes
632000170117     c* se non è festivo guardo anche la trazione
632100170117     c                   if        wslitt<>'S'
632200170117     C                   Z-ADD     D98TFP        CLNTFP
632300170117     C                   Z-ADD     D98TFA        CLNTFA
632400170117     c                   exsr      azcln_fes
632500170117     C                   ENDIF
632600170117     c
632700170117   x1C                   Else
632800170117     c
632900090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
633000090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
633100090108     c**   è possibile rendere festivo in terminal per un altro terminal
633200090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
633300170117     c
633400170117     c** ES 17/01/17 - Faccio doppio tentativo: prima con la filiale e poi con
633500170117     c**               la trazione , se filiale non festiva
633600930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
633700170117     C***  WTC           oreq      'P'                                          *TRAZIONE
633800941227     C                   Z-ADD     D98TFP        CLNTFP
633900941227     C                   Z-ADD     D98TFA        CLNTFA
634000941228     C                   ENDIF
634100941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
634200941227     C                   Z-ADD     *ZEROS        CLNTFP
634300941227     C                   Z-ADD     D98LNA        CLNTFA
634400941228     C                   ENDIF
634500170117     c
634600170117     c                   exsr      azcln_fes
634700170117    1c                   endif
634800930325     C*
634900060526     c                   clear                   wpodata
635000930325     C                   ENDSR
635100170117
635200170117     C********************************************************************
635300170117     c* Verifica se giorno festivo su calendario
635400170117     C********************************************************************
635500170117     c     Azcln_fes     BEGSR
635600170117     C                   Z-ADD     W12DAT        DSDAT8
635700170117     C                   Z-ADD     DSAAS         CLNANN
635800170117     C                   Z-ADD     DSMES         CLNMES
635900170117     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
636000170117     C     D98ERR        IFEQ      '0'                                          --- 1 -->
636100170117     C                   Z-ADD     DSGIO         G
636200170117     C     W12RES        IFEQ      *ZEROS
636300170117     C                   MOVEL     CLNMAT        GFEDS
636400170117     C                   ELSE
636500170117     C                   MOVEL     CLNPOM        GFEDS
636600170117     C                   END
636700170117     c
636800170117     c* se sto verificando i tempi partenza o trazione e
636900170117     c*  la data che sto controllando corrisponde alla data spedizione,
637000170117     c*  la considero sempre lavorativa
637100170117     c                   if        (wtc='P' or wtc='T') and w12dat=d98dfv
637200170117     c                   else
637300170117     c
637400170117     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
637500170117     C                   MOVEL     *BLANKS       WC1Y
637600170117     C                   MOVEL     GFE(G)        WC1Y              8
637700170117     C                   Z-ADD     1             I
637800170117     C     WC1Y          LOOKUP    C1Y(I)                                 99
637900170117     C     *IN99         IFEQ      '1'                                          --- 3 -->
638000170117     C     WIC           IFEQ      'I'
638100170117     C     I1Y(I)        ANDEQ     'S'
638200170117     C                   MOVEL     'S'           WSLITT
638300170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
638400170117     C                   END
638500170117     C     WIC           IFEQ      'C'
638600170117     C     E1Y(I)        ANDEQ     'S'
638700170117     C                   MOVEL     'S'           WSLITT
638800170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
638900170117     C                   END
639000170117     C     WIC           IFEQ      'X'
639100170117     C     E1Y(I)        ANDEQ     'S'
639200170117     C                   MOVEL     'S'           WSLITT
639300170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
639400170117     C                   END
639500170117     C                   ELSE                                                   <-- 3 -->
639600170117     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
639700170117     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
639800170117     C                   END                                                    <-- 3 ---
639900170117     C                   END                                                    <-- 2 ---
640000170117     C                   END                                                    <-- 1 ---
640100170117     c
640200170117     c                   endif
640300170117     c                   ENDSR
640400931201     C********************************************************************
640500931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
640600931201     C********************************************************************
640700931201     C     CHKTUC        BEGSR
640800931201     C*
640900931201     C     WIC           IFEQ      'I'                                          --- 1 -->
641000931201     C                   DO        TI            T                 3 0          --- 2 -->
641100931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
641200931201     C     WPOROU        ANDLE     TIF(T)
641300931201     C                   MOVEL     'S'           WSLITT
641400931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
641500931201     C                   LEAVE                                                  *VA A FINE DO
641600931201     C                   ENDIF                                                  <-- 3 ---
641700931201     C                   ENDDO                                                  <-- 2 ---
641800931201     C                   ELSE                                                   <-- 1 -->
641900931201     C                   DO        TC            T                 3 0          --- 2 -->
642000931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
642100931201     C     WPOROU        ANDLE     TCF(T)
642200931201     C                   MOVEL     'S'           WSLITT
642300931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
642400931201     C                   LEAVE                                                  *VA A FINE DO
642500931201     C                   ENDIF                                                  <-- 3 ---
642600931201     C                   ENDDO                                                  <-- 2 ---
642700931201     C                   ENDIF                                                  <-- 1 ---
642800931201     C*
642900931201     C                   ENDSR
643000060418     C********************************************************************
643100060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
643200060418     c*          - eventi particolari che giustificano
643300060418     C********************************************************************
643400060418     C     CHKCEPG       BEGSR
643500060418     C*
643600060418     C     WIC           IFEQ      'I'                                          --- 1 -->
643700060420     C                   DO        PGI           p                 3 0          --- 2 -->
643800060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
643900060420     C     WPOROU        ANDLE     PGIF(p)
644000060418     C                   MOVEL     'S'           WSLITT
644100060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
644200060418     C                   LEAVE                                                  *VA A FINE DO
644300060418     C                   ENDIF                                                  <-- 3 ---
644400060418     C                   ENDDO                                                  <-- 2 ---
644500060418     C                   ELSE                                                   <-- 1 -->
644600060420     C                   DO        PGC           p                 3 0          --- 2 -->
644700060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
644800060420     C     WPOROU        ANDLE     PGCF(p)
644900060418     C                   MOVEL     'S'           WSLITT
645000060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
645100060418     C                   LEAVE                                                  *VA A FINE DO
645200060418     C                   ENDIF                                                  <-- 3 ---
645300060418     C                   ENDDO                                                  <-- 2 ---
645400060418     C                   ENDIF                                                  <-- 1 ---
645500060418     C*
645600060418     C                   ENDSR
645700930512     C********************************************************************
645800931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
645900930512     C********************************************************************
646000931130     C     DATTUC        BEGSR
646100931214     C*
646200931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
646300931214     C                   Z-ADD     12            WNOADD            9 0
646400930325     C*
646500931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
646600931125     C     D98GC2        ORNE      *BLANKS
646700931130     C*
646800931213     C                   Z-ADD     WPOROU        WORE
646900931130     C                   EXSR      OREDAT                                       *ORE->DATA
647000931130     C                   Z-ADD     WDAT          W12DAT
647100931130     C                   Z-ADD     WRES          W12RES
647200930405     C*
647300941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
647400930405     C* DELLA SETTIMANA
647500941227     C                   Z-ADD     W12DAT        DSDAT8
647600941227     C                   Z-ADD     DSAAS         CLNANN
647700941227     C                   Z-ADD     DSMES         CLNMES
647800941227     C                   Z-ADD     *ZEROS        CLNTFP
647900941227     C                   Z-ADD     *ZEROS        CLNTFA
648000930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
648100930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
648200941227     C                   MOVEL     CLNMAT        GSEDS
648300960129     C                   Z-ADD     DSGIO         GI                6 0
648400960129     C                   MOVEL     GSE(GI)       WGSE              1
648500140911     c
648600140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
648700140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
648800140911     c                   if        wtuctut='S'
648900140911     c                   eval      wtuctut='P'
649000140911     c                   endif
649100930326     C*
649200931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
649300931213     C                   DO        4             IT                3 0          --- 3 -->
649400931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
649500931213     C                   MOVEL     TUC(IT)       W93GSE            1
649600931213     C                   MOVE      TUC(IT)       W93GMP            1
649700930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
649800930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
649900931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
650000930512     C     W93GSE        OREQ      ' '
650100140911     c     WTUCTUT       andeq     ' '
650200140911     c
650300930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
650400931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
650500930401     C     W93GMP        OREQ      ' '
650600930325     C                   MOVEL     'S'           WSLITT
650700931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
650800931213     C                   ELSE                                                   <-- 6 -->
650900001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
651000001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
651100001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
651200050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
651300050908     C     Usacons       IFNE      'S'                                          --- 7 -->
651400141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
651500141107     c*    orari giorno per giorno
651600141107     c                   if        OOR2FCSGL<>' '
651700141108     c                             and w12dat<>sav_12dat
651800141107     c                   eval      or_datarif=w12dat
651900141107     c                   eval      or_DDC    =w12dat
652000141107     c                   exsr      Reper_tempi
652100141108     c                   eval      sav_12dat=w12dat
652200141107     c                   endif
652300141107     c
652400930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
652500930325     C     W93GMP        IFEQ      'M'
652600930326     C     W12RES        ANDEQ     *ZEROS
652700141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
652800141111     c                             or OOR2STfS =0
652900141105     c                             or (tuc(IT)<>gc1part and
653000170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
653100170207     c
653200140825     C                   MOVEL     'S'           WSLITT
653300140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
653400140911     c* slitto solo una volta  se chiuso tutti " M"
653500140911     c                   if        W93GSE=' '
653600140911     c                   eval      wTUCTUT='S'
653700140911     c                   endif
653800140911     c
653900140825     c                   endif
654000930325     C                   END
654100930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
654200930325     C     W93GMP        IFEQ      'P'
654300930326     C     W12RES        ANDNE     *ZEROS
654400140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
654500140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
654600141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
654700141111     c                             or OOR2STiS =0
654800141105     c                             or (tuc(IT)<>gc1part and
654900170207     c                              tuc(IT)<>gc2part and d98dcr<=wdaodi)
655000930325     C                   MOVEL     'S'           WSLITT
655100140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
655200140911     c* slitto solo una volta  se chiuso tutti " P"
655300140911     c                   if        W93GSE=' '
655400140911     c                   eval      wTUCTUT='S'
655500140911     c                   endif
655600140825     c                   endif
655700140825     c
655800931213     C                   ENDIF
655900001128     C                   ENDIF                                                  <-- 7 ---
656000931213     C                   ENDIF                                                  <-- 6 ---
656100931213     C                   ENDIF                                                  <-- 5 ---
656200931213     C                   ENDIF                                                  <-- 4 ---
656300931213     C                   ENDDO                                                  <-- 3 ---
656400930405     C*
656500931213     C                   ENDIF                                                  <-- 2 ---
656600931213     C                   ENDIF                                                  <-- 1 ---
656700140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
656800140911     c                   if        wtuctut='P'  and wslitt='N'
656900140911     c                   clear                   wtuctut
657000140911     c                   endif
657100140911     c
657200930325     C                   ENDSR
657300060406     C********************************************************************
657400060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
657500060406     C********************************************************************
657600060406     C     DATCEPG       BEGSR
657700060406     C*
657800060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
657900060406     C                   Z-ADD     12            WNOADD            9 0
658000060406     C*
658100060406    1C                   if        wevpag='S'
658200060406     C*
658300060406     C                   Z-ADD     WPOROU        WORE
658400060406     C                   EXSR      OREDAT                                       *ORE->DATA
658500060406     C                   Z-ADD     WDAT          W12DAT
658600060406     C                   Z-ADD     WRES          W12RES
658700060418     c                   z-add     1             P
658800060406     c* Verifico se nella data esiste un evento Part che giustifica
658900060418     c     w12dat        lookup    pvd(P)                                 30
659000060418    2c                   dow       *in30 and pvf(P)<>'G'
659100060418     c                   add       1             P
659200060420     c     w12dat        lookup    pvd(P)                                 30
659300060406    2c                   enddo
659400060406     c
659500060406     c* 30 on --> TROVATO !!
659600060406    2c                   if        *in30
659700060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
659800060406     C                   MOVEL     'S'           WSLITT
659900060406     c                   z-add     24            wnoadd
660000060406    3C                   ENDIF
660100060406     C*
660200060406    2C                   ENDIF                                                  <-- 2 ---
660300060406    1C                   ENDIF                                                  <-- 1 ---
660400060406     C*
660500060406     C                   ENDSR
660600931210     C********************************************************************
660700931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
660800931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
660900931210     C********************************************************************
661000050805     C***  CHKDCR        BEGSR
661100931210     C*
661200050805     C***  WPOCRI        IFNE      *ZEROS
661300050805     C***  WPOROU        IFGE      WPOCRI
661400050805     C***  WPOROU        ANDLT     WPOCRF
661500050805     C***                ELSE
661600050805     C***                MOVEL     'S'           WSLITT
661700050805     C***                END
661800050805     C***                END
661900931210     C*
662000050805     C***                ENDSR
662100140822     C********************************************************************
662200140822     C*  Reperimento tempi di consegna per decidere se slittare
662300140822     C********************************************************************
662400140822     C     Reper_tempi   BEGSR
662500140822
662600140822     c                   clear                   trulorSds
662700140825     c                   clear                   trulor2ds
662800140822     c                   eval       IOREtfp = d98tfp
662900140822     c                   eval       IOREtfa = d98tfa
663000140825     c                   eval       IOREdti = d98dti
663100140825     c                   eval       IOREhti = d98hti
663200140825     c                   eval       IOREdcr = s98dcr
663300140825     c                   eval       IOREhcr = s98hcr
663400140825     c                   eval       IOREtcr = s98tcr
663500140825     c                   eval       IOREDEI = d98dei
663600140825     c                   eval       IOREOEI = d98oei
663700140825     c                   eval       IOREfil = d98lna
663800140825     c                   eval       IOREcap = d98cad
663900140825     c                   eval       IOREloc = d98lod
664000140825     c                   eval       IOREnar = d98nzd
664100140825     c
664200140825     c* Data di Riferimento:
664300140825     c* Se impostata la consegnabilità --> prendo questa
664400140822     c                   select
664500140825     c                   when      d98dfc>0
664600141107     c                   if        or_datarif<d98dfc and or_datarif>0
664700141107     c                   eval      ioredta=or_datarif
664800141107     c                   else
664900141107     c                   eval      ioredta=d98dfc
665000141107     c                   endif
665100140825     c* altrimenti data di consegna
665200140825     c                   when      d98dcm>0
665300141107     c                   if        or_datarif<d98dcm and or_datarif>0
665400141107     c                   eval      ioredta=or_datarif
665500141107     c                   else
665600140825     c                   eval      ioredta=d98dcm
665700141107     c                   endif
665800140825     c
665900140822     c                   other
666000140825     c* altrimenti data teorica consegna se > = oggi
666100140825     C                   time                    wrkoggiiso
666200140825     C     *iso          move      wrkoggiiso    ioredta
666300140822     c* se < di oggi, imposto oggi
666400140825     c                   if        d98dei>ioredta
666500140825     c                   eval      ioredta=d98dei
666600140822     c                   endif
666700141107     c
666800141107     c                   if        or_datarif<ioredta and or_datarif>0
666900141107     c                   eval      ioredta=or_datarif
667000141107     c                   endif
667100141107     c
667200140822     c                   endsl
667300140822     c
667400140825     c                   eval       IOREtser = 'C'
667500141107     c                   if         or_ddc>0 and or_ddc<d98ddc
667600141107     c                   eval       IOREDDC=or_ddc
667700141107     c                   eval       IOREnDC=8888888
667800141107     c                   eval       IOREcons = 'S'
667900141107     c                   else
668000140825     c                   eval       IOREDDC=d98ddc
668100140825     c                   eval       IOREnDC=wd98ndc
668200140825     c                   eval       IOREcons = d98cons
668300141107     c                   endif
668400141017     c                   eval       IOREtsp = d98tsp
668500140822     c
668600140822     c                   call      'TRULORSR'
668700140822     c                   parm                    kpjba
668800140822     c                   parm                    trulorSds
668900140825     c                   parm                    trulor2ds
669000140825     c
669100140825     c                   eval      wtrulor= 'S'
669200141107     c                   clear                   or_datarif        8 0
669300141107     c                   clear                   or_ddc            8 0
669400140822     C                   ENDSR
669500950406     C********************************************************************
669600950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
669700950406     C********************************************************************
669800950406     C     DATORE        BEGSR
669900950406     C*
670000950406     C                   Z-ADD     *ZEROS        WORE
670100950406     C     WDAT          IFNE      0
670200950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
670300950406     C     WAA           DIV       4             WQUA              4 0
670400950406     C                   MVR                     WRES
670500950406     C     WAA           MULT      365           WGIO              9 0
670600950406     C                   ADD       WQUA          WGIO
670700950406     C                   ADD       WGG           WGIO
670800950406     C     WMM           SUB       1             MC                3 0
670900950406     C                   DO        MC            WX                3 0
671000950406     C                   ADD       MES(WX)       WGIO
671100950406     C                   END
671200950406     C     WRES          IFEQ      0
671300950406     C     WMM           ANDLE     2
671400950406     C                   SUB       1             WGIO
671500950406     C                   END
671600950406     C     WGIO          MULT      24            WORE              9 0
671700950406     C                   ENDIF
671800950406     C*
671900950406     C                   ENDSR
672000930323     C********************************************************************
672100930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
672200930323     C********************************************************************
672300930323     C     OREDAT        BEGSR
672400930323     C*
672500950407     C                   SETOFF                                       717273
672600950407     C                   SETOFF                                       7475
672700930323     C     WORE          DIV       24            WGIO
672800930323     C                   MVR                     WRES              3 0
672900950406     C*
673000950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
673100950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
673200950406     C*  TROVO ANNO
673300950406     C     WGIO          DIV       365           WAA
673400950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
673500950406     C                   MVR                     WREST                71
673600950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
673700950406     C     WGIO          DIV       365           WAA
673800950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
673900950406     C*                                                    0 = 31/12/PR
674000950406     C   75
674100950406     CAN 71              DO                                                     BIS. E NON ERA
674200950406     C     WAA           SUB       1             WAA
674300950406     C                   Z-ADD     365           WREST
674400950406     C                   END
674500950406     C*
674600950406     C     WAA           DIV       4             WNBIS                                      T.
674700950406     C                   MVR                     WRBIS             3 073
674800950406     C   73
674900950406     CANN71                                                                     NON PIU' BISEST
675000950406     CORN73              ADD       1             WREST                          E'BISESTILE
675100950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
675200950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
675300950406     C  N73              SUB       1             $PM(1)
675400950406     C                   Z-ADD     1             WI                2 0
675500950406     C* TROVO IL MESE E GIORNI
675600950406     C     WREST         LOOKUP    $PM(WI)                            74  73
675700950406     C                   MOVE      WI            WMM
675800950406     C     WI            COMP      1                                      72
675900950406     C  N72
676000950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
676100950406     C   74              SUB       1             WI
676200950406     C  N72
676300950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
676400950406     C   72              MOVE      WREST         WGG
676500950406     C                   ADD       1900          WAA
676600950406     C*
676700930323     C*
676800930323     C                   ENDSR
6769009407151507 C********************************************************************
6770009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6771009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6772009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6773009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6774009407151507 C********************************************************************
6775009407151507 C     FINGIO        BEGSR
6776009407151507 C*
6777009407151507 C     WPORE         DIV       24            WGIO
6778009407151507 C                   MVR                     WRES
6779009407151507 C     WRES          IFEQ      *ZEROS
6780009407151507 C                   Z-ADD     12            WNORE             3 0
6781009407151507 C                   ELSE
6782009407151507 C                   Z-ADD     *ZEROS        WNORE
6783009407151507 C                   ENDIF
6784009407151507 C*
6785009407151507 C                   ENDSR
678600930517     C********************************************************************
678700971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
678800930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
678900930517     C********************************************************************
679000930517     C     CERCAL        BEGSR
679100930517     C*
679200930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
679300941227     C                   MOVE      CLNTFP        DCATFP
679400941227     C                   MOVE      CLNTFA        DCATFA
679500941227     C                   MOVE      CLNANN        DCAANN
679600941227     C                   MOVE      CLNMES        DCAMES
679700930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
679800930517     C                   Z-ADD     1             I
679900930517     C     DCALEN        LOOKUP    CAK(I)                                 99
680000930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
680100930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
680200941227     C     KEYCLN        CHAIN     AZCLN01L                           99
680300930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
680400930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
680500930517     C                   MOVEL     '1'           D98ERR
680600930517     C                   MOVEL     MSG(3)        D98MSG
680700930517     C                   ELSE                                                   <-- 2 -->
680800970401     C* CONTROLLA SE ANNULLATO
680900970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
681000970401     C                   MOVEL     '1'           D98ERR
681100970401     C                   MOVEL     MSG(3)        D98MSG
681200970401     C                   ELSE                                                   <-- 3 -->
681300930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
681400930517     C                   ADD       1             WINDCA            6 0
681500970401     C     WINDCA        IFLE      200                                          --- 4 -->
681600970401     C     WINDCA        ANDGT     *ZEROS
681700930517     C                   Z-ADD     WINDCA        I
681800930517     C                   MOVEL     DCALEN        CAK(I)
681900941227     C                   MOVEL     CLNMAT        CAM(I)
682000941227     C                   MOVEL     CLNPOM        CAP(I)
682100970401     C                   END                                                    <-- 4 ---
682200970401     C                   ENDIF                                                  <-- 3 ---
682300930517     C                   END                                                    <-- 2 ---
682400930517     C                   ELSE                                                   <-- 1 -->
682500930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
682600941227     C                   MOVEL     CAM(I)        CLNMAT
682700941227     C                   MOVEL     CAP(I)        CLNPOM
682800930517     C                   END                                                    <-- 1 ---
682900930517     C*
683000930517     C                   ENDSR
683100010821     C********************************************************************
683200010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
683300010821     C********************************************************************
683400010821     C     CHKVCA        BEGSR
683500010821     C*
683600010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
683700010821     C     D98CAS        IFGT      *ZEROS
683800010821     C     D98VCA        ANDEQ     *BLANKS
683900010821     C                   MOVEL     'ITL'         D98VCA
684000010821     C                   ENDIF
684100010822     C*
684200010822     C     D98VCA        IFNE      'ITL'
684300010822     C     D98VCA        ANDNE     'EUR'
684400010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
684500010822     C                   Z-ADD     D98CAS        INPIMP
684600010822     C                   MOVEL     D98VCA        INPDIV
684700010822     C                   EXSR      EXECNV
684800010822     C                   Z-ADD     OUTIMP        D98CAS
684900010822     C                   MOVEL     §GEDCN        D98VCA
685000010822     C                   ENDIF
685100010821     C*
685200010821     C                   ENDSR
685300010821     C********************************************************************
685400010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
685500010821     C********************************************************************
685600010821     C     EXECNV        BEGSR
685700010821     C*
685800010821     C                   CLEAR                   YEURDS
685900010821     C*
686000010821     C                   MOVEL     INPDIV        YECDVI
686100010821     C                   Z-ADD     INPIMP        YECIMI
686200010821     C                   MOVEL     §GEDCN        YECDVO
686300010821     C                   MOVEL     '3'           YECDCO
686400010821     C                   MOVEL     'H'           YECTAR
686500010821     C                   CALL      'YEURCO'
686600010821     C                   PARM                    YEURDS
686700010821     C*
686800010821     C                   Z-ADD     YECIMO        OUTIMP
686900010821     C*
687000010821     C                   ENDSR
687100011211     C*------------------------------------------------------------------------*
687200011211     C* Controllo importo massimo contrassegno.
687300011211     C*------------------------------------------------------------------------*
687400011211     C     REPMAXCSB     BEGSR
687500011211     C*
687600011211     C                   CLEAR                   Trul21DS
687700011211     C*
687800160420     c                   eval      i21cbo = d98cbo
687900011211     C                   EVAL      I21TSp = d98TSP
688000011211     C                   EVAL      I21NzD = d98NZD
688100011211     C                   EVAL      I21LnP = d98LNP
688200011211     C                   EVAL      I21LnA = d98LNA
688300011211     C                   EVAL      I21Ksc = d98KSC
688400011211     C                   EVAL      I21TiC = d98TIC
688500011211     C                   EVAL      I21Imp = d98CAS
688600011211     C                   EVAL      I21Div = d98VCA
688700160420     c                   eval      i21cbo = d98cbo
688800011211     C*
688900011211     C                   CALL      'TRUL21R'
689000011211     C                   PARM                    Trul21DS
689100160420
689200160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
689300160420     c                   if        wtrasm='N'
689400160420     c                   if        o21fca <>*blanks
689500160420     c                   eval      §b4ltca = 'S'
689600160420     c                   endif
689700160420     c                   endif
689800011211     C*
689900011211     C                   ENDSR
690000011211     C*------------------------------------------------------------------------*
690100011211
690200930414     C********************************************************************
690300930414     C*   RUTINZ - ROUTINE INIZIALE
690400930414     C********************************************************************
690500930414     C     RUTINZ        BEGSR
690600930414     C*
690700930414     C* PARAMETRI IN INPUT ALLA ROUTINE
690800930414     C     *ENTRY        PLIST
690900970731     C                   PARM                    DS99
691000941227     C*
691100941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
691200941227     C     KEYBLP        KLIST
691300941227     C                   KFLD                    BLPAAS
691400941227     C                   KFLD                    BLPLNP
691500941227     C                   KFLD                    BLPNRS
691600941227     C                   KFLD                    BLPNSP
691700050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
691800941227     C     KEYGCP        KLIST
691900050405     C                   KFLD                    KWGCPAAS
692000050405     C                   KFLD                    KWGCPLNP
692100050405     C                   KFLD                    KWGCPNRS
692200050405     C                   KFLD                    KWGCPNSP
692300050405     C                   KFLD                    KWGCPFRG
692400050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
692500031125     C     KEYGC_P       KLIST
692600050405     C                   KFLD                    KWGCPAAS
692700050405     C                   KFLD                    KWGCPLNP
692800050405     C                   KFLD                    KWGCPNRS
692900050405     C                   KFLD                    KWGCPNSP
693000970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
693100150120     C**   KEYBLT        KLIST
693200150120     C**                 KFLD                    BLTAAS
693300150120     C**                 KFLD                    BLTLNP
693400150120     C**                 KFLD                    BLTNRS
693500150120     C**                 KFLD                    BLTNSP
693600941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
693700941227     C     KEYARB        KLIST
693800941227     C                   KFLD                    ARBAAS
693900941227     C                   KFLD                    ARBLNP
694000941227     C                   KFLD                    ARBNRS
694100941227     C                   KFLD                    ARBNSP
694200051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
694300941228     C     KEYAR9        KLIST
694400941228     C                   KFLD                    AR9AAS
694500941228     C                   KFLD                    AR9LNP
694600941228     C                   KFLD                    AR9NRS
694700941228     C                   KFLD                    AR9NSP
694800941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
694900050405     C     KEYGCP21      KLIST
695000050405     C                   KFLD                    KWGCPAAS
695100050405     C                   KFLD                    KWGCPLNP
695200050405     C                   KFLD                    KWGCPNRS
695300050405     C                   KFLD                    KWGCPNSP
695400050405     C                   KFLD                    KWGCPFRG
695500970912     C* CHIAVE LETTURA FNART01L - PARZIALE
695600150120     C***  KEYART        KLIST
695700150120     C***                KFLD                    ARTAAS
695800150120     C***                KFLD                    ARTLNP
695900150120     C***                KFLD                    ARTNRS
696000150120     C***                KFLD                    ARTNSP
696100950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
696200950102     C     KEYLBL        KLIST
696300950102     C                   KFLD                    LBLAAN
696400950102     C                   KFLD                    LBLLPN
696500950102     C                   KFLD                    LBLNRN
696600950102     C                   KFLD                    LBLNSN
696700930414     C*
696800070827     c     kAR5          Klist
696900070827     c                   Kfld                    d98Aas
697000070827     c                   Kfld                    d98Lnp
697100070827     c                   Kfld                    d98nrs
697200070827     c                   Kfld                    d98nsp
697300070827     c                   Kfld                    kAr5Trd
697400150325     c     kAR4          Klist
697500150325     c                   Kfld                    d98Aas
697600150325     c                   Kfld                    d98Lnp
697700150325     c                   Kfld                    d98nrs
697800150325     c                   Kfld                    d98nsp
697900150325     c                   Kfld                    kAr4Trc
698000150218     c*
698100150218     c     kart          Klist
698200150218     c                   Kfld                    d98Aas
698300150218     c                   Kfld                    d98Lnp
698400150218     c                   Kfld                    d98nrs
698500150218     c                   Kfld                    d98nsp
698600150218     c     kbrv          Klist
698700150218     c                   Kfld                    artfls
698800150218     c                   Kfld                    artlna
698900150218     c                   Kfld                    artnrs
699000150218     c                   Kfld                    artnsc
699100150325     C     Kfapd         klist
699200150325     C                   kfld                    kpdtip
699300150325     C                   kfld                    kpdpdr
699400930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
699500930414     C     KEYTAB        KLIST
699600930414     C                   KFLD                    KTBUTE
699700930414     C                   KFLD                    KTBCOD
699800930414     C                   KFLD                    KTBKEY
699900060406     C     Ktab          KLIST
700000060406     C                   KFLD                    KTBUTE
700100060406     C                   KFLD                    KTBCOD
700200930414     C*
700300941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
700400941227     C     KEYCLN        KLIST
700500941227     C                   KFLD                    CLNTFP
700600941227     C                   KFLD                    CLNTFA
700700941227     C                   KFLD                    CLNANN
700800941227     C                   KFLD                    CLNMES
700900941227     C*
701000941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
701100941227     C     KEYCAS        KLIST
701200941227     C                   KFLD                    CASPTA
701300941227     C                   KFLD                    CASLNP
701400941227     C                   KFLD                    CASLNA
701500941227     C                   KFLD                    CASTSP
701600941227     C                   KFLD                    CASDDE
701700930420     C*
701800990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
701900930420     C     KEYEVB        KLIST
702000930420     C                   KFLD                    KEBAAS
702100930420     C                   KFLD                    KEBLNP
702200930420     C                   KFLD                    KEBNRS
702300930420     C                   KFLD                    KEBNSP
702400010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
702500010821     C     KTBE          KLIST
702600010821     C                   KFLD                    TBECOD
702700010821     C                   KFLD                    TBEKE1
702800010821     C                   KFLD                    TBEKE2
702900930414     C*
703000010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
703100930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
703200930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
703300930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
703400930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
703500930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
703600930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
703700930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
703800930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
703900930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
704000010821     C     *LIKE         DEFINE    D98CAS        INPIMP
704100010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
704200010821     C     *LIKE         DEFINE    D98VCA        INPDIV
704300930414     C*
704400930414     C* RECUPERO IL CODICE DELL'AZIENDA
704500930414     C                   Z-ADD     1             CODUT             1 0
704600970701     C                   Z-ADD     *ZEROS        I                 6 0
704700930414     C*
704800930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
704900930414     C                   EXSR      AZZERA
705000930414     C*
705100930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
705200941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
705300941227     C                   MOVEL     'S'           CTABSN            1
705400930414     C                   EXSR      CARTAB
705500941227     C                   ENDIF
705600041116     c* Se passato umero di spedizioni apro i files , altrimenti no
705700041116     c                   if        d98nsp>0 and wapri=' '
705800041116     c                   open      fnblp01l
705900050405     c                   open      tigcp01l
706000150120     c***                open      fnblt01l
706100041116     c                   open      fnarb01l
706200051115     c                   open      fiar901l
706300150325     c                   open      fiar401l
706400050405     c                   open      tigcp21l
706500150218     c                   open      fnart01l
706600150218     c                   open      fnbrv07l
706700041116     c                   open      fnlbl01l
706800041116     c                   open      fnevb01l
706900070827     c                   open      fiar501l
707000041116     c                   eval      wapri='S'
707100041116     c                   endif
707200930414     C*
707300930414     C                   ENDSR
707400941227     C*------------------------------------------------------------------------*
707500941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
707600941227     C*------------------------------------------------------------------------*
707700941227     C     SUBDAT        BEGSR
707800941227     C*
707900941227     C                   TIME                    DTTIME
708000941227     C                   Z-ADD     DTANN         DSANN
708100941227     C                   Z-ADD     DTMES         DSMES
708200941227     C                   Z-ADD     DTGIO         DSGIO
708300941227     C                   Z-ADD     DTSEC         DSSEC
708400941227     C*
708500941227     C                   ENDSR
708600930414     C********************************************************************
708700930414     C*   CARTAB - CARICA LE TABELLE
708800930414     C********************************************************************
708900930414     C     CARTAB        BEGSR
709000930414     C*
709100930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
709200930414     C     D98ERR        IFEQ      '0'
709300930421     C                   Z-ADD     0             I
709400930414     C                   Z-ADD     CODUT         KTBUTE
709500930414     C                   MOVEL     '1Y'          KTBCOD
709600930414     C                   MOVEL     *BLANKS       KTBKEY
709700930414     C     KEYTAB        SETLL     TABEL00F                           99
709800930414     C  N99              READ      TABEL00F                               99
709900930414     C     *IN99         IFEQ      '1'
710000930414     C                   MOVEL     '1'           D98ERR
710100930414     C                   MOVEL     MSG(5)        D98MSG
710200930414     C                   END
710300930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
710400930414     C     TBLCOD        ANDEQ     '1Y'
710500930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
710600930421     C                   ADD       1             I
710700930421     C                   MOVEL     TBLKEY        C1Y(I)
710800930414     C                   MOVEL     TBLUNI        DS1Y
710900930421     C                   MOVEL     §1YFEI        I1Y(I)
711000930421     C                   MOVEL     §1YFEC        E1Y(I)
711100930414     C                   END                                                    <-- 3 ---
711200930414     C                   READ      TABEL00F                               99
711300930414     C                   END                                                    <-- 2 ---
711400930414     C                   END
711500930414     C*
711600930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
711700930414     C     D98ERR        IFEQ      '0'
711800930421     C                   Z-ADD     0             I
711900930414     C                   Z-ADD     CODUT         KTBUTE
712000930414     C                   MOVEL     '1X'          KTBCOD
712100930414     C                   MOVEL     *BLANKS       KTBKEY
712200930414     C     KEYTAB        SETLL     TABEL00F                           99
712300930414     C  N99              READ      TABEL00F                               99
712400930414     C     *IN99         IFEQ      '1'
712500930414     C                   MOVEL     '1'           D98ERR
712600930414     C                   MOVEL     MSG(1)        D98MSG
712700930414     C                   END
712800930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
712900930414     C     TBLCOD        ANDEQ     '1X'
713000930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
713100930421     C                   ADD       1             I
713200930421     C                   MOVEL     TBLKEY        C1X(I)
713300930414     C                   MOVEL     TBLUNI        DS1X
713400930421     C                   Z-ADD     §1XHHG        G1X(I)
713500930421     C                   Z-ADD     §1XHHO        O1X(I)
713600930421     C                   Z-ADD     §1XHHM        M1X(I)
713700930421     C                   Z-ADD     §1XHHP        P1X(I)
713800930421     C                   Z-ADD     §1XHHI        I1X(I)
713900930421     C                   Z-ADD     §1XHHS        S1X(I)
714000930414     C                   END                                                    <-- 3 ---
714100930414     C                   READ      TABEL00F                               99
714200931223     C                   ENDDO                                                  <-- 2 ---
714300050523     c
714400050523     c* controllo riempimento skiere
714500050523     c                   clear                   trul0sds
714600050523     c                   eval      i0sski='C1X/G1X/O1X/...'
714700050523     c                   eval      i0sele=%elem(c1x)
714800050523     c                   eval      i0spie=i
714900050523     c                   eval      i0sfile='TABEL00F  '
715000050523     c                   eval      i0scod=' 1X  '
715100050523     c                   eval      i0spgm='TNSD99R'
715200050523     c                   movel     trul0sds      kpjbu
715300050523     c                   call      'TRUL0SR'
715400050523     c                   parm                    kpjba
715500050523     C                   END
715600930421     C*
715700930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
715800930421     C     D98ERR        IFEQ      '0'
715900930421     C                   Z-ADD     0             I
716000930421     C                   Z-ADD     CODUT         KTBUTE
716100930421     C                   MOVEL     '2A'          KTBCOD
716200060406     C     Ktab          SETLL     TABEL00F                           99
716300060406     C  N99ktab          READe     TABEL00F                               99
716400930421     C     *IN99         IFEQ      '1'
716500930421     C                   MOVEL     '1'           D98ERR
716600930421     C                   MOVEL     MSG(8)        D98MSG
716700930421     C                   END
716800930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
716900930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
717000930421     C                   ADD       1             I
717100930421     C                   MOVEL     TBLKEY        C2A(I)
717200060406     C                   MOVEL     TBLUNI        ds2a
717300060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
717400060406     C                   MOVEL     w003a         D2A(I)
717500930421     C                   END                                                    <-- 3 ---
717600060406     C     ktab          READe     TABEL00F                               99
717700931223     C                   ENDDO                                                  <-- 2 ---
717800050523     c* controllo riempimento skiere
717900050523     c                   clear                   trul0sds
718000050523     c                   eval      i0sski='C2A/D2A'
718100050523     c                   eval      i0sele=%elem(c2a)
718200050523     c                   eval      i0spie=i
718300050523     c                   eval      i0sfile='TABEL00F  '
718400060406     c                   eval      i0scod='2A/2Z'
718500050523     c                   eval      i0spgm='TNSD99R'
718600050523     c                   movel     trul0sds      kpjbu
718700050523     c                   call      'TRUL0SR'
718800050523     c                   parm                    kpjba
718900930421     C                   END
719000060406     c* Tabella  2Z
719100060406     C     D98ERR        IFEQ      '0'
719200060406     C                   Z-ADD     CODUT         KTBUTE
719300060406     C                   MOVEL     '2Z'          KTBCOD
719400060406     C     Ktab          SETLL     TABEL00F                           99
719500060406     C  N99ktab          READe     TABEL00F                               99
719600060406     C     *IN99         IFEQ      '1'
719700060406     C                   MOVEL     '1'           D98ERR
719800060406     C                   MOVEL     MSG(33)       D98MSG
719900060406     C                   END
720000060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
720100060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
720200060406     C                   Z-ADD     1             I
720300060406     C                   MOVEL     TBLKEY        w003a
720400060406     c     w003a         lookup    c2a(i)                                 99
720500060406     C     *IN99         IFEQ      *off
720600060406     C                   MOVEL     '1'           D98ERR
720700060406     C                   MOVEL     MSG(34)       D98MSG
720800060406     C                   Else
720900060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
721000060406     C                   END                                                    <-- 3 ---
721100060406     C                   END                                                    <-- 3 ---
721200060406     C     ktab          READe     TABEL00F                               99
721300060406     C                   ENDDO                                                  <-- 2 ---
721400060406     C                   END
721500941227     C*
721600941227     C* CARICA TABELLA CODICI BOLLA -3A-
721700941227     C     D98ERR        IFEQ      '0'
721800941228     C                   Z-ADD     0             I
721900941227     C                   Z-ADD     CODUT         KTBUTE
722000941227     C                   MOVEL     '3A'          KTBCOD
722100941227     C                   MOVEL     *BLANKS       KTBKEY
722200941227     C     KEYTAB        SETLL     TABEL00F                           99
722300941227     C  N99              READ      TABEL00F                               99
722400941227     C     *IN99         IFEQ      '1'
722500941227     C                   MOVEL     '1'           D98ERR
722600941227     C                   MOVEL     MSG(22)       D98MSG
722700941227     C                   END
722800941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
722900941227     C     TBLCOD        ANDEQ     '3A'
723000941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
723100941227     C                   ADD       1             I
723200941227     C                   MOVEL     TBLKEY        C3A(I)
723300941227     C                   MOVEL     TBLUNI        DS3A
723400941227     C                   MOVEL     §3ATB1        T3A(I)
723500941228     C                   Z-ADD     §3AFCA        F3A(I)
723600990308     C                   MOVEL     §3ARBL        R3A(I)
723700941227     C                   END                                                    <-- 3 ---
723800941227     C                   READ      TABEL00F                               99
723900941227     C                   END                                                    <-- 2 ---
724000050523     c* controllo riempimento skiere
724100050523     c                   clear                   trul0sds
724200050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
724300050523     c                   eval      i0sele=%elem(c3a)
724400050523     c                   eval      i0spie=i
724500050523     c                   eval      i0sfile='TABEL00F  '
724600050523     c                   eval      i0scod=' 3A  '
724700050523     c                   eval      i0spgm='TNSD99R'
724800050523     c                   movel     trul0sds      kpjbu
724900050523     c                   call      'TRUL0SR'
725000050523     c                   parm                    kpjba
725100941227     C                   END
725200971210     C*
725300020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
725400020226     C                   Z-ADD     *ZEROS        J                 3 0
725500160331     C                   Z-ADD     *ZEROS        D                 3 0
725600971210IF  0C     D98ERR        IFEQ      '0'
725700971210     C     *LOVAL        SETLL     AZORG01L
725800971210     C                   READ      AZORG01L                               98
725900971210DO  1C     *IN98         DOWEQ     *OFF
726000971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
726100971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
726200971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
726300020627     C                   MOVEL     ORGDE3        OG143
726400020702     C*
726500020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
726600020702     C                   CLEAR                   DNTW
726700020702     C*
726800020702     C                   MOVEL(P)  'NTW'         TBECOD
726900020702     C                   MOVEL(P)  §OGNTW        TBEKE1
727000020702     C     KTBE          CHAIN     TNTBE01L
727100020702     C                   IF        %found(TNTBE01L)
727200020702     C                   MOVEL     TBEUNI        DNTW
727300020702     C                   ENDIF
727400020702     C*
727500020702     C                   MOVEL     §NTWFIE       FL1(F)
727600020627     C*
727700020226     C* VERIFICO SE FILIALE FEDEX
727800020226     C                   IF        §OGNTW = 'FED'
727900020226     C                   ADD       1             J
728000020226     C                   Z-ADD     ORGFIL        FILFED(J)
728100020226     C                   ENDIF
728200160331     C* VERIFICO SE FILIALE DPD
728300160331     C                   IF        §OGNTW = 'DPD'
728400160331     C                   ADD       1             D
728500160331     C                   Z-ADD     ORGFIL        FILDPD(D)
728600160331     C                   ENDIF
728700020226     C*
728800971210E   2C                   ENDIF
728900971210     C                   READ      AZORG01L                               98
729000971210E   1C                   ENDDO
729100971210E   0C                   ENDIF
729200010821     C*
729300010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
729400010821     C                   MOVEL     'GED'         TBECOD
729500070514     C                   MOVEL     *BLANKS       TBEKE1
729600070514     C                   MOVEL     '1'           TBEKE1
729700010821     C                   MOVEL     *BLANKS       TBEKE2
729800010821     C     KTBE          CHAIN     TNTBE01L                           21
729900070514     C  N21              MOVEL     TBEUNI        DGED
730000150326     c
730100150326     C* REPERISCO LA tabella flag operativi per tabella OLP
730200150326     c                   clear                   dvpo
730300150326     C                   MOVEL     'VPO'         TBECOD
730400150326     C                   MOVEL     *BLANKS       TBEKE2
730500150326     C                   MOVEL(P)  'VPO'         TBEKE1
730600150326     C     KTBE          CHAIN     TNTBE01L                           21
730700150326     C  N21              MOVEL     TBEUNI        DVPO
730800150326     C*
730900031201     C*
731000031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
731100031201     C     D98ERR        IFEQ      '0'
731200060403     C                   Z-ADD     0             I7QW              3 0
731300031201     C                   Z-ADD     CODUT         KTBUTE
731400031201     C                   MOVEL     '7Q'          KTBCOD
731500031201     C                   MOVEL     *BLANKS       KTBKEY
731600031201     C     KEYTAB        SETLL     TABEL00F                           99
731700031201     C  N99              READ      TABEL00F                               99
731800031201     C     *IN99         IFEQ      '1'
731900031201     C                   MOVEL     '1'           D98ERR
732000031201     C                   MOVEL     MSG(8)        D98MSG
732100031201     C                   ENDIF
732200031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
732300031201     C     TBLCOD        ANDEQ     '7Q'
732400031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
732500031201     C                   MOVEL     TBLUNI        DS7Q
732600031201     C                   IF        §7Q4D1 = 'R' OR
732700031201     C                             §7Q4D2 = 'R' OR
732800031201     C                             §7Q4D3 = 'R' OR
732900031201     C                             §7Q4D4 = 'R' OR
733000031201     C                             §7Q4D5 = 'R'
733100031201     C                   ADD       1             I7QW
733200031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
733300031201     C                   ENDIF                                                  <-- 3 ---
733400031201     C                   ENDIF                                                  <-- 3 ---
733500031201     C                   READ      TABEL00F                               99
733600031201     C                   ENDDO                                                  <-- 2 ---
733700031201     C                   ENDIF
733800031201     C*
733900031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
734000031201     C     D98ERR        IFEQ      '0'
734100031201     C                   Z-ADD     CODUT         KTBUTE
734200031201     C                   MOVEL     '7W'          KTBCOD
734300031201     C                   MOVEL     *BLANKS       KTBKEY
734400031201     C     KEYTAB        SETLL     TABEL00F                           99
734500031201     C  N99              READ      TABEL00F                               99
734600031201     C     *IN99         IFEQ      '1'
734700031201     C                   MOVEL     '1'           D98ERR
734800031201     C                   MOVEL     MSG(8)        D98MSG
734900031201     C                   ENDIF
735000031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
735100031201     C     TBLCOD        ANDEQ     '7W'
735200031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
735300031201     C                   MOVEL     TBLUNI        DS7W
735400031201     C                   IF        §7W6D1 = 'C' OR
735500031201     C                             §7W6D2 = 'C' OR
735600031201     C                             §7W6D3 = 'C' OR
735700031201     C                             §7W6D4 = 'C' OR
735800031201     C                             §7W6D5 = 'C'
735900031201     C                   ADD       1             I7QW
736000031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
736100031201     C                   ENDIF                                                  <-- 3 ---
736200031201     C                   ENDIF                                                  <-- 3 ---
736300031201     C                   READ      TABEL00F                               99
736400031201     C                   ENDDO                                                  <-- 2 ---
736500050523     c* controllo riempimento skiere
736600050523     c                   clear                   trul0sds
736700050523     c                   eval      i0sski='SK7QW'
736800050523     c                   eval      i0sele=%elem(sk7qw)
736900050523     c                   eval      i0spie=i7qw
737000050523     c                   eval      i0sfile='TABEL00F  '
737100050523     c                   eval      i0scod='7Q-7W'
737200050523     c                   eval      i0spgm='TNSD99R'
737300050523     c                   movel     trul0sds      kpjbu
737400050523     c                   call      'TRUL0SR'
737500050523     c                   parm                    kpjba
737600031201     C                   ENDIF
737700930414     C*
737800930414     C                   ENDSR
737900930414     C********************************************************************
738000930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
738100930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
738200930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
738300930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
738400930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
738500930414     C********************************************************************
738600930414     C     RO3ORE        BEGSR
738700930414     C*
738800930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
738900930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
739000930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
739100050902     c                   movel     ' '           WR3MATPOM         1
739200931209     C                   Z-ADD     *ZEROS        WR3HHO
739300931209     C                   Z-ADD     *ZEROS        WR3HHA
739400930414     C                   MOVE      WR3TAB        WR3COD
739500930604     C                   MOVEL     D98LNA        WR3COD            8
739600930421     C                   Z-ADD     1             I
739700930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
739800930414     C     *IN99         IFEQ      '0'
739900930414     C                   MOVE      WR3TAB        WR3COD
740000930414     C                   MOVEL     '   '         WR3COD            8
740100930421     C                   Z-ADD     1             I
740200930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
740300930414     C                   END
740400930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
740500930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
740600930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
740700930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
740800930414     C                   ELSE                                                   <-- 2 -->
740900930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
741000930421     C                   Z-ADD     M1X(I)        WR3HHO
741100930510     C                   Z-ADD     I1X(I)        WR3HHA
741200050902     c                   movel     'M'           WR3MATPOM         1
741300930414     C                   END
741400930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
741500930421     C                   Z-ADD     P1X(I)        WR3HHO
741600930510     C                   Z-ADD     S1X(I)        WR3HHA
741700050902     c                   movel     'P'           WR3MATPOM
741800930414     C                   END
741900930414     C                   END                                                    <-- 2 ---
742000930414     C                   END                                                    <-- 1 ---
742100930414     C*
742200930414     C                   ENDSR
742300930423     C********************************************************************
742400930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
742500930423     C********************************************************************
742600930423     C     W98D98        BEGSR
742700930423     C*
742800941228     C*
742900941228     C* ORE REALI PARTENZA INTERNE
743000941228     C                   Z-ADD     W98RPI        W98               3 0
743100941228     C                   EXSR      W98999
743200941228     C                   Z-ADD     D98           D98RPI
743300941228     C* ORE REALI PARTENZA CLIENTE
743400941228     C                   Z-ADD     W98RPC        W98
743500941228     C                   EXSR      W98999
743600941228     C                   Z-ADD     D98           D98RPC
743700930423     C* ORE REALI TRAZIONE INTERNE
743800941228     C                   Z-ADD     W98RTI        W98
743900941228     C                   EXSR      W98999
744000941228     C                   Z-ADD     D98           D98RTI
744100930423     C* ORE REALI TRAZIONE CLIENTE
744200941228     C                   Z-ADD     W98RTC        W98
744300941228     C                   EXSR      W98999
744400941228     C                   Z-ADD     D98           D98RTC
744500930423     C* ORE REALI CONSEGNA INTERNE
744600941228     C                   Z-ADD     W98RCI        W98
744700941228     C                   EXSR      W98999
744800941228     C                   Z-ADD     D98           D98RCI
744900930423     C* ORE REALI CONSEGNA CLIENTE
745000941228     C                   Z-ADD     W98RCC        W98
745100941228     C                   EXSR      W98999
745200941228     C                   Z-ADD     D98           D98RCC
745300930423     C* ORE REALI RESA INTERNE
745400941228     C                   Z-ADD     W98RRI        W98
745500941228     C                   EXSR      W98999
745600941228     C                   Z-ADD     D98           D98RRI
745700930423     C* ORE REALI RESA CLIENTE
745800941228     C                   Z-ADD     W98RRC        W98
745900941228     C                   EXSR      W98999
746000941228     C                   Z-ADD     D98           D98RRC
746100941228     C* ORE DIFF. PARTENZA INTERNE
746200941228     C                   Z-ADD     W98FPI        W98
746300941228     C                   EXSR      W98999
746400941228     C                   Z-ADD     D98           D98FPI
746500941228     C* ORE DIFF. PARTENZA CLIENTE
746600941228     C                   Z-ADD     W98FPC        W98
746700941228     C                   EXSR      W98999
746800941228     C                   Z-ADD     D98           D98FPC
746900930423     C* ORE DIFF. TRAZIONE INTERNE
747000941228     C                   Z-ADD     W98FTI        W98
747100941228     C                   EXSR      W98999
747200941228     C                   Z-ADD     D98           D98FTI
747300930423     C* ORE DIFF. TRAZIONE CLIENTE
747400941228     C                   Z-ADD     W98FTC        W98
747500941228     C                   EXSR      W98999
747600941228     C                   Z-ADD     D98           D98FTC
747700930423     C* ORE DIFF. CONSEGNA INTERNE
747800941228     C                   Z-ADD     W98FCI        W98
747900941228     C                   EXSR      W98999
748000941228     C                   Z-ADD     D98           D98FCI
748100930423     C* ORE DIFF. CONSEGNA CLIENTE
748200941228     C                   Z-ADD     W98FCC        W98
748300941228     C                   EXSR      W98999
748400941228     C                   Z-ADD     D98           D98FCC
748500930423     C* ORE DIFF. RESA INTERNE
748600941228     C                   Z-ADD     W98FRI        W98
748700941228     C                   EXSR      W98999
748800941228     C                   Z-ADD     D98           D98FRI
748900930423     C* ORE DIFF. RESA CLIENTE
749000941228     C                   Z-ADD     W98FRC        W98
749100941228     C                   EXSR      W98999
749200941228     C                   Z-ADD     D98           D98FRC
749300941228     C* ORE NON CONF. PARTENZA INTERNE
749400941228     C     W98NPI        IFGT      999
749500941228     C                   Z-ADD     999           D98NPI
749600941228     C                   ELSE
749700941228     C                   Z-ADD     W98NPI        D98NPI
749800941228     C                   END
749900941228     C* ORE NON CONF. PARTENZA CLIENTE
750000941228     C     W98NPC        IFGT      999
750100941228     C                   Z-ADD     999           D98NPC
750200941228     C                   ELSE
750300941228     C                   Z-ADD     W98NPC        D98NPC
750400941228     C                   END
750500930423     C* ORE NON CONF. TRAZIONE INTERNE
750600930423     C     W98NTI        IFGT      999
750700930423     C                   Z-ADD     999           D98NTI
750800930423     C                   ELSE
750900930423     C                   Z-ADD     W98NTI        D98NTI
751000930423     C                   END
751100930423     C* ORE NON CONF. TRAZIONE CLIENTE
751200930423     C     W98NTC        IFGT      999
751300930423     C                   Z-ADD     999           D98NTC
751400930423     C                   ELSE
751500930423     C                   Z-ADD     W98NTC        D98NTC
751600930423     C                   END
751700930423     C* ORE NON CONF. CONSEGNA INTERNE
751800930423     C     W98NCI        IFGT      999
751900930423     C                   Z-ADD     999           D98NCI
752000930423     C                   ELSE
752100930423     C                   Z-ADD     W98NCI        D98NCI
752200930423     C                   END
752300930423     C* ORE NON CONF. CONSEGNA CLIENTE
752400930423     C     W98NCC        IFGT      999
752500930423     C                   Z-ADD     999           D98NCC
752600930423     C                   ELSE
752700930423     C                   Z-ADD     W98NCC        D98NCC
752800930423     C                   END
752900930423     C* ORE NON CONF. RESA INTERNE
753000930423     C     W98NRI        IFGT      999
753100930423     C                   Z-ADD     999           D98NRI
753200930423     C                   ELSE
753300930423     C                   Z-ADD     W98NRI        D98NRI
753400930423     C                   END
753500930423     C* ORE NON CONF. RESA CLIENTE
753600930423     C     W98NRC        IFGT      999
753700930423     C                   Z-ADD     999           D98NRC
753800930423     C                   ELSE
753900930423     C                   Z-ADD     W98NRC        D98NRC
754000930423     C                   END
754100930423     C* ORE X TURNO CHIUSURA INTERNE
754200941228     C                   Z-ADD     W98HHI        W98
754300941228     C                   EXSR      W98999
754400941228     C                   Z-ADD     D98           D98HHI
754500930423     C* ORE X TURNO CHIUSURA CLIENTE
754600941228     C                   Z-ADD     W98HHC        W98
754700941228     C                   EXSR      W98999
754800941228     C                   Z-ADD     D98           D98HHC
754900060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
755000060418     C                   Z-ADD     W98EGI        W98
755100060418     C                   EXSR      W98999
755200060418     C                   Z-ADD     D98           D98hEGI
755300060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
755400060418     C                   Z-ADD     W98EGC        W98
755500060418     C                   EXSR      W98999
755600060418     C                   Z-ADD     D98           D98hEGC
755700060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
755800060420     C                   Z-ADD     W98EAI        W98
755900060420     C                   EXSR      W98999
756000060420     C                   Z-ADD     D98           D98hEAI
756100060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
756200060420     C                   Z-ADD     W98EAC        W98
756300060420     C                   EXSR      W98999
756400060420     C                   Z-ADD     D98           D98hEAC
756500930423     C* ORE X LASC. AVVISO INTERNE
756600941228     C                   Z-ADD     W98HLI        W98
756700941228     C                   EXSR      W98999
756800941228     C                   Z-ADD     D98           D98HLI
756900930423     C* ORE X LASC. AVVISO CLIENTE
757000941228     C                   Z-ADD     W98HLC        W98
757100941228     C                   EXSR      W98999
757200941228     C                   Z-ADD     D98           D98HLC
757300930423     C* ORE X GIACENZA INTERNE
757400941228     C                   Z-ADD     W98HGI        W98
757500941228     C                   EXSR      W98999
757600941228     C                   Z-ADD     D98           D98HGI
757700930423     C* ORE X GIACENZA CLIENTE
757800941228     C                   Z-ADD     W98HGC        W98
757900941228     C                   EXSR      W98999
758000941228     C                   Z-ADD     D98           D98HGC
758100941228     C* ORE X FEST. PART. INTERNE
758200941228     C                   Z-ADD     W98HPI        W98
758300941228     C                   EXSR      W98999
758400941228     C                   Z-ADD     D98           D98HPI
758500941228     C* ORE X FEST. PART. CLIENTE
758600941228     C                   Z-ADD     W98HPC        W98
758700941228     C                   EXSR      W98999
758800941228     C                   Z-ADD     D98           D98HPC
758900930423     C* ORE X FEST. TRAZ. INTERNE
759000941228     C                   Z-ADD     W98HFI        W98
759100941228     C                   EXSR      W98999
759200941228     C                   Z-ADD     D98           D98HFI
759300930423     C* ORE X FEST. TRAZ. CLIENTE
759400941228     C                   Z-ADD     W98HFC        W98
759500941228     C                   EXSR      W98999
759600941228     C                   Z-ADD     D98           D98HFC
759700930423     C* ORE X FEST. CONS. INTERNE
759800941228     C                   Z-ADD     W98HEI        W98
759900941228     C                   EXSR      W98999
760000941228     C                   Z-ADD     D98           D98HEI
760100930423     C* ORE X FEST. CONS. CLIENTE
760200941228     C                   Z-ADD     W98HEC        W98
760300941228     C                   EXSR      W98999
760400941228     C                   Z-ADD     D98           D98HEC
760500930511     C* ORE X F.DEPOSITO INTERNE
760600941228     C                   Z-ADD     W98HDI        W98
760700941228     C                   EXSR      W98999
760800941228     C                   Z-ADD     D98           D98HDI
760900930511     C* ORE X F.DEPOSITO CLIENTE
761000941228     C                   Z-ADD     W98HDC        W98
761100941228     C                   EXSR      W98999
761200941228     C                   Z-ADD     D98           D98HDC
761300930511     C* ORE X CONTRASSEGNO INTERNE
761400941228     C                   Z-ADD     W98HOI        W98
761500941228     C                   EXSR      W98999
761600941228     C                   Z-ADD     D98           D98HOI
761700930511     C* ORE X CONTRASSEGNO CLIENTE
761800941228     C                   Z-ADD     W98HOC        W98
761900941228     C                   EXSR      W98999
762000941228     C                   Z-ADD     D98           D98HOC
762100930914     C* ORE DI CONSEGNABILITA' INTERNE
762200941228     C                   Z-ADD     W98OCI        W98
762300941228     C                   EXSR      W98000
762400941228     C                   Z-ADD     D98           D98OCI
762500930914     C* ORE DI CONSEGNABILITA' CLIENTE
762600941228     C                   Z-ADD     W98OCC        W98
762700941228     C                   EXSR      W98000
762800941228     C                   Z-ADD     D98           D98OCC
762900071122     C
763000071122     c* Per i codici bolla M prepagati annullati, azzero tutto
763100071122     c                   if        d98cbo='M '
763200071122     c                   clear                   d98rpi
763300071122     c                   clear                   d98rpc
763400071122     c                   clear                   d98rti
763500071122     c                   clear                   d98rtc
763600071122     c                   clear                   d98rci
763700071122     c                   clear                   d98rcc
763800071122     c                   clear                   d98rri
763900071122     c                   clear                   d98rrc
764000071122     c                   clear                   d98fpi
764100071122     c                   clear                   d98fpc
764200071122     c                   clear                   d98fti
764300071122     c                   clear                   d98ftc
764400071122     c                   clear                   d98fci
764500071122     c                   clear                   d98fcc
764600071122     c                   clear                   d98fri
764700071122     c                   clear                   d98frc
764800071122     c                   clear                   d98npi
764900071122     c                   clear                   d98npc
765000071122     c                   clear                   d98nti
765100071122     c                   clear                   d98ntc
765200071122     c                   clear                   d98nci
765300071122     c                   clear                   d98ncc
765400071122     c                   clear                   d98nri
765500071122     c                   clear                   d98nrc
765600071122     c                   clear                   d98OCi
765700071122     c                   clear                   d98OCC
765800071122     c                   endif
765900930423     C*
766000100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
766100100308     c*  calcolo
766200100308     c                   eval      d98dcr=s98dcr
766300100308     c                   eval      d98hcr=s98hcr
766400100308     c                   eval      d98tcr=s98tcr
766500160405     c
766600160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
766700160405     c*  perchè il pgm usa il campo verificado se =10 )
766800160405     c                   if        wVP='S'
766900160405     c                   add       24            d98tci
767000160405     c                   add       24            d98tcc
767100160405     c                   add       24            d98tri
767200160405     c                   add       24            d98trc
767300160405     c                   endif
767400100308     c
767500930423     C                   ENDSR
767600941228     C***********************************************************************
767700941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
767800941228     C***********************************************************************
767900941228     C     W98999        BEGSR
768000941228     C     W98           IFLT      -999
768100941228     C                   Z-ADD     -999          D98               3 0
768200941228     C                   ELSE
768300941228     C     W98           IFGT      999
768400941228     C                   Z-ADD     999           D98
768500941228     C                   ELSE
768600941228     C                   Z-ADD     W98           D98
768700941228     C                   END
768800941228     C                   END
768900941228     C                   ENDSR
769000941228     C***********************************************************************
769100941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
769200941228     C***********************************************************************
769300941228     C     W98000        BEGSR
769400941228     C     W98           IFLE      *ZEROS
769500941228     C                   Z-ADD     *ZEROS        D98
769600941228     C                   ELSE
769700941228     C     W98           IFGT      999
769800941228     C                   Z-ADD     999           D98
769900941228     C                   ELSE
770000941228     C                   Z-ADD     W98           D98
770100941228     C                   END
770200941228     C                   END
770300941228     C                   ENDSR
770400930305     C***********************************************************************
770500930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
770600930305     C***********************************************************************
770700930305     C     AZZERA        BEGSR
770800150219     c* data e ora del momento
770900150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
771000150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
771100150219     C                   Z-ADD     DTORA4        WHHODI            4 0
771200930305     C*
771300141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
771400150603     C                   MOVEL     'N'           opendan
771500141107     c                   clear                   or_ddc
771600141108     c                   clear                   sav_12dat         8 0
771700141107     c                   clear                   or_datarif
771800140402     c                   clear                   wpick2            1
771900140402     c                   clear                   Primotipo
772000090928     c                   clear                   Primodev
772100090928     c                   clear                   Primohev
772200070827     c                   clear                   CallAR5
772300071122     c                   clear                   d98cbo
772400100308     c                   clear                   s98dcr
772500100308     c                   clear                   s98hcr
772600100308     c                   clear                   s98tcr
772700141106     c                   clear                   nosomma           1
772800141229     c                   clear                   wtrasm
772900150324     c                   clear                   avvcmc
773000150325     c                   clear                   DSbl4L
773100160401     c                   clear                   dsbl4I
773200150401     c                   clear                   fnlv82ds
773300150331     c                   clear                   DS7R
773400150605     c                   clear                   wDEE              1
773500150629     c                   clear                   wtsogliaE
773600930323     C                   MOVEL     '0'           D98ERR
773700930323     C                   MOVEL     *BLANKS       D98MSG
773800931124     C                   Z-ADD     *ZEROS        D98TPI
773900931124     C                   Z-ADD     *ZEROS        D98TPC
774000930325     C                   Z-ADD     *ZEROS        D98TTI
774100930325     C                   Z-ADD     *ZEROS        D98TTC
774200930325     C                   Z-ADD     *ZEROS        D98TCI
774300930325     C                   Z-ADD     *ZEROS        D98TCC
774400090827     C                   Z-ADD     *ZEROS        w99TCI
774500090827     C                   Z-ADD     *ZEROS        w99TCC
774600930325     C                   Z-ADD     *ZEROS        D98TRI
774700930325     C                   Z-ADD     *ZEROS        D98TRC
774800090921     C                   Z-ADD     *ZEROS        w99TRI
774900090921     C                   Z-ADD     *ZEROS        w99TRC
775000931124     C                   Z-ADD     *ZEROS        D98RPI
775100931124     C                   Z-ADD     *ZEROS        D98RPC
775200930405     C                   Z-ADD     *ZEROS        D98RTI
775300930405     C                   Z-ADD     *ZEROS        D98RTC
775400930405     C                   Z-ADD     *ZEROS        D98RCI
775500930405     C                   Z-ADD     *ZEROS        D98RCC
775600930405     C                   Z-ADD     *ZEROS        D98RRI
775700930405     C                   Z-ADD     *ZEROS        D98RRC
775800931124     C                   Z-ADD     *ZEROS        D98FPI
775900931124     C                   Z-ADD     *ZEROS        D98FPC
776000930325     C                   Z-ADD     *ZEROS        D98FTI
776100930325     C                   Z-ADD     *ZEROS        D98FTC
776200930325     C                   Z-ADD     *ZEROS        D98FCI
776300930325     C                   Z-ADD     *ZEROS        D98FCC
776400930325     C                   Z-ADD     *ZEROS        D98FRI
776500930325     C                   Z-ADD     *ZEROS        D98FRC
776600931124     C                   MOVEL     *BLANKS       D98CPI
776700931124     C                   MOVEL     *BLANKS       D98CPC
776800930325     C                   MOVEL     *BLANKS       D98CTI
776900930325     C                   MOVEL     *BLANKS       D98CTC
777000930325     C                   MOVEL     *BLANKS       D98CCI
777100930325     C                   MOVEL     *BLANKS       D98CCC
777200930325     C                   MOVEL     *BLANKS       D98CRI
777300930325     C                   MOVEL     *BLANKS       D98CRC
777400931124     C                   Z-ADD     *ZEROS        D98NPI
777500931124     C                   Z-ADD     *ZEROS        D98NPC
777600930325     C                   Z-ADD     *ZEROS        D98NTI
777700930325     C                   Z-ADD     *ZEROS        D98NTC
777800930325     C                   Z-ADD     *ZEROS        D98NCI
777900930325     C                   Z-ADD     *ZEROS        D98NCC
778000930325     C                   Z-ADD     *ZEROS        D98NRI
778100930325     C                   Z-ADD     *ZEROS        D98NRC
778200930414     C                   Z-ADD     *ZEROS        D98HHI
778300930414     C                   Z-ADD     *ZEROS        D98HHC
778400930414     C                   Z-ADD     *ZEROS        D98HOI
778500930414     C                   Z-ADD     *ZEROS        D98HOC
778600930414     C                   Z-ADD     *ZEROS        D98HLI
778700930414     C                   Z-ADD     *ZEROS        D98HLC
778800930414     C                   Z-ADD     *ZEROS        D98HGI
778900930414     C                   Z-ADD     *ZEROS        D98HGC
779000931124     C                   Z-ADD     *ZEROS        D98HPI
779100931124     C                   Z-ADD     *ZEROS        D98HPC
779200930414     C                   Z-ADD     *ZEROS        D98HFI
779300930414     C                   Z-ADD     *ZEROS        D98HFC
779400930416     C                   Z-ADD     *ZEROS        D98HEI
779500930416     C                   Z-ADD     *ZEROS        D98HEC
779600930414     C                   Z-ADD     *ZEROS        D98HRI
779700930414     C                   Z-ADD     *ZEROS        D98HRC
779800930511     C                   Z-ADD     *ZEROS        D98HDI
779900930511     C                   Z-ADD     *ZEROS        D98HDC
780000930325     C                   Z-ADD     *ZEROS        D98DCI
780100930325     C                   Z-ADD     *ZEROS        D98HCI
780200930325     C                   Z-ADD     *ZEROS        D98DCE
780300930325     C                   Z-ADD     *ZEROS        D98HCE
780400931125     C                   Z-ADD     *ZEROS        D98OCI
780500931125     C                   Z-ADD     *ZEROS        D98OCC
780600931202     C                   Z-ADD     *ZEROS        D98DEI
780700931202     C                   Z-ADD     *ZEROS        D98OEI
780800931202     C                   Z-ADD     *ZEROS        D98DEE
780900931202     C                   Z-ADD     *ZEROS        D98OEE
781000151019     c                   CLEAR                   D98FLP
781100161107     c                   CLEAR                   D98Gfas
781200150325     C                   clear                   D98dtctd
781300150326     C                   clear                   D98picklim
781400150325     C                   clear                   D98spcdee
781500150331     C                   clear                   fddata
781600170208     C                   clear                   D98dam_rtc
781700140825     c
781800141017     c                   clear                   d98ddc
781900141017     c                   clear                   d98cons
782000160506     c                   clear                   d98disf
782100141017     c                   clear                   wd98ndc
782200140910     c                   clear                   wd98fbc
782300150119     c                   clear                   wd98acc
782400140825     c                   clear                   wtrulor
782500160401     c                   clear                   wVP               1
782600170208     c                   clear                   wsolodam
782700150219     c
782800150219     c                   clear                   IMPdata
782900150219     c                   clear                   IMPfgs
783000150219     c                   clear                   DISdata
783100150219     c                   clear                   DISfgs
783200150219     c                   clear                   DISora
783300150219     c                   clear                   DISnpg
783400150219     c                   clear                   IDnpg
783500150219     c                   clear                   IDdata
783600150219     c                   clear                   IDora
783700150219     c                   clear                   IDfgs
783800931125     C* CAMPI VARI
783900931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
784000140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
784100140825      *   DALLA LETTURA DI QUESTI FILE                               *
784200931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
784300931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
784400931125     C     D98NSP        IFNE      *ZEROS
784500930525     C                   MOVEL     *BLANKS       D98TSP
784600941228     C                   MOVEL     *BLANKS       D98CAD
784700970728     C                   MOVEL     *BLANKS       D98LOD
784800930525     C                   Z-ADD     *ZEROS        D98TFP
784900930525     C                   Z-ADD     *ZEROS        D98TFA
785000930525     C                   Z-ADD     *ZEROS        D98LNA
785100930525     C                   Z-ADD     *ZEROS        D98DSP
785200941228     C                   Z-ADD     *ZEROS        D98MGS
785300930525     C                   Z-ADD     *ZEROS        D98DTI
785400930525     C                   Z-ADD     *ZEROS        D98HTI
785500930525     C                   Z-ADD     *ZEROS        D98DCM
785600930525     C                   Z-ADD     *ZEROS        D98HMC
785700930525     C                   MOVEL     *BLANKS       D98GC1
785800930525     C                   MOVEL     *BLANKS       D98GC2
785900010820     C                   Z-ADD     *ZEROS        D98CAS
786000010821     C                   MOVEL     *BLANKS       D98VCA
786100930525     C                   MOVEL     *BLANKS       D98TIC
786200930525     C                   Z-ADD     *ZEROS        D98DCR
786300930525     C                   Z-ADD     *ZEROS        D98HCR
786400930525     C                   MOVEL     *BLANKS       D98TCR
786500050914     C                   MOVEL     *BLANKS       Consideraora      1
786600930525     C                   MOVEL     *BLANKS       D98CMC
786700170818     C                   clear                   wD98ntc_con       1 0
786800930525     C                   Z-ADD     *ZEROS        D98DAG
786900161215     C                   Z-ADD     *ZEROS        D98DCc
787000161215     C                   Z-ADD     *ZEROS        D98DAc
787100161215     C                   Z-ADD     *ZEROS        D98DCG
787200930525     C                   MOVEL     *BLANKS       D98FFD
787300930525     C                   Z-ADD     *ZEROS        D98NCD
787400930525     C                   MOVEL     *BLANKS       D98FNS
787500931124     C                   Z-ADD     *ZEROS        D98DAM
787600170105     C                   Z-ADD     *ZEROS        D98Dfv
787700940628     C                   MOVEL     *BLANKS       D98FIN
787800940628     C                   Z-ADD     *ZEROS        D98KSC
787900940628     C                   MOVEL     *BLANKS       D98TBL
788000931125     C                   END
788100960229     C     D98TLA        IFEQ      'T'
788200960229     C                   Z-ADD     *ZEROS        D98DTI
788300960229     C                   Z-ADD     *ZEROS        D98HTI
788400960229     C                   ENDIF
788500931125     C* CAMPI WORK
788600930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
788700930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
788800150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
788900930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
789000941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
789100960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
789200930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
789300930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
789400940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
789500930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
789600150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
789700931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
789800931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
789900150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
790000930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
790100930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
790200930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
790300930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
790400930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
790500940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
790600941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
790700930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
790800050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
790900050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
791000140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
791100930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
791200930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
791300930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
791400930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
791500930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
791600930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
791700930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
791800930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
791900930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
792000930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
792100930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
792200930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
792300930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
792400930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
792500140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
792600931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
792700060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
792800960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
792900931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
793000140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
793100140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
793200150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
793300140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
793400941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
793500970304     C******************** Z-ADD*ZEROS    W98DBR  80
793600970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
793700150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
793800050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
793900930414     C*
794000941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
794100941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
794200930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
794300930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
794400930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
794500930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
794600930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
794700930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
794800941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
794900941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
795000930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
795100930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
795200930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
795300930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
795400930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
795500930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
795600941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
795700941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
795800930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
795900930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
796000930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
796100930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
796200930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
796300930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
796400930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
796500930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
796600060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
796700060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
796800060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
796900060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
797000930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
797100930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
797200930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
797300930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
797400930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
797500930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
797600930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
797700930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
797800930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
797900930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
798000930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
798100930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
798200930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
798300930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
798400941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
798500941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
798600930423     C*
798700930511     C                   Z-ADD     *ZEROS        W98HF             5 0
798800930423     C                   Z-ADD     *ZEROS        W98HH             5 0
798900060418     C                   Z-ADD     *ZEROS        W98EG             5 0
799000060420     C                   Z-ADD     *ZEROS        W98EA             5 0
799100930423     C                   Z-ADD     *ZEROS        W98HG             5 0
799200930423     C                   Z-ADD     *ZEROS        W98HL             5 0
799300930423     C                   Z-ADD     *ZEROS        W98HO             5 0
799400930511     C                   Z-ADD     *ZEROS        W98HD             5 0
799500930611     C*
799600930611     C                   Z-ADD     *ZEROS        WORELA            3 0
799700960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
799800930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
799900930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
800000931207     C                   Z-ADD     *ZEROS        WORECR            3 0
800100090902     c
800200090902     c                   clear                   wmancaTRAZ
800300150113     c                   clear                   wcaldpcee         1
800400150220     c                   clear                   evemic            2
800500930305     C*
800600060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
800700060406     C                   clear                   LAF                            *LASC.AVVISO FINE
800800091014     C                   clear                   LAp                            *posizione l.avviso
800900060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
801000060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
801100060406c    c                   clear                   LCon
801200060406     C                   clear                   EVD                            *DATA EVENTO
801300060406     C                   clear                   EVO                            *ORA EVENTO
801400060406     C                   clear                   EVC                            *CAUSALE EVENTO
801500131218     C                   clear                   EVT
801600060406     C                   clear                   pVD                            *DATA EVENTO
801700060406     C                   clear                   pVO                            *ORA EVENTO
801800060406     C                   clear                   pVC                            *CAUSALE EVENTO
801900060406     C                   clear                   pVF                            *CAUSALE EVENTO
802000060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
802100060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
802200060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
802300060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
802400060406     C                   clear                   COV                            *DATE FINE L.A. E GI
802500060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
802600060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
802700091014     C                   clear                   GIp                            *posizione vento gia
802800060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
802900060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
803000060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
803100931213     C*
803200060406     C                   clear                   TUC                            *TURNI CHIUSURA
803300931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
803400931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
803500060406     c
803600060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
803700060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
803800060406     c                   clear                   pgii
803900060406     c                   clear                   pgif
804000060406     c                   clear                   pgci
804100060406     c                   clear                   pgcf
804200060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
804300060420     c                   clear                   paii
804400060420     c                   clear                   paif
804500930420     C*
804600930305     C                   ENDSR
804700930512     C***********************************************************************
804800930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
804900930512     C***********************************************************************
805000930512     C     FORCON        BEGSR
805100930512     C*
805200931124     C                   Z-ADD     *ZEROS        D98RPI
805300931124     C                   Z-ADD     *ZEROS        D98RPC
805400930512     C                   Z-ADD     *ZEROS        D98RTI
805500930512     C                   Z-ADD     *ZEROS        D98RTC
805600930512     C                   Z-ADD     *ZEROS        D98RCI
805700930512     C                   Z-ADD     *ZEROS        D98RCC
805800930512     C                   Z-ADD     *ZEROS        D98RRI
805900930512     C                   Z-ADD     *ZEROS        D98RRC
806000931124     C                   Z-ADD     *ZEROS        D98FPI
806100931124     C                   Z-ADD     *ZEROS        D98FPC
806200930512     C                   Z-ADD     *ZEROS        D98FTI
806300930512     C                   Z-ADD     *ZEROS        D98FTC
806400930512     C                   Z-ADD     *ZEROS        D98FCI
806500930512     C                   Z-ADD     *ZEROS        D98FCC
806600930512     C                   Z-ADD     *ZEROS        D98FRI
806700930512     C                   Z-ADD     *ZEROS        D98FRC
806800931124     C                   MOVEL     'S'           D98CPI
806900931124     C                   MOVEL     'S'           D98CPC
807000930512     C                   MOVEL     'S'           D98CTI
807100930512     C                   MOVEL     'S'           D98CTC
807200930512     C                   MOVEL     'S'           D98CCI
807300930512     C                   MOVEL     'S'           D98CCC
807400930512     C                   MOVEL     'S'           D98CRI
807500930512     C                   MOVEL     'S'           D98CRC
807600931124     C                   Z-ADD     *ZEROS        D98NPI
807700931124     C                   Z-ADD     *ZEROS        D98NPC
807800930512     C                   Z-ADD     *ZEROS        D98NTI
807900930512     C                   Z-ADD     *ZEROS        D98NTC
808000930512     C                   Z-ADD     *ZEROS        D98NCI
808100930512     C                   Z-ADD     *ZEROS        D98NCC
808200930512     C                   Z-ADD     *ZEROS        D98NRI
808300930512     C                   Z-ADD     *ZEROS        D98NRC
808400930512     C                   Z-ADD     *ZEROS        D98HHI
808500930512     C                   Z-ADD     *ZEROS        D98HHC
808600930512     C                   Z-ADD     *ZEROS        D98HOI
808700930512     C                   Z-ADD     *ZEROS        D98HOC
808800930512     C                   Z-ADD     *ZEROS        D98HLI
808900930512     C                   Z-ADD     *ZEROS        D98HLC
809000930512     C                   Z-ADD     *ZEROS        D98HGI
809100930512     C                   Z-ADD     *ZEROS        D98HGC
809200931124     C                   Z-ADD     *ZEROS        D98HPI
809300931124     C                   Z-ADD     *ZEROS        D98HPC
809400930512     C                   Z-ADD     *ZEROS        D98HFI
809500930512     C                   Z-ADD     *ZEROS        D98HFC
809600930512     C                   Z-ADD     *ZEROS        D98HEI
809700930512     C                   Z-ADD     *ZEROS        D98HEC
809800930512     C                   Z-ADD     *ZEROS        D98HRI
809900930512     C                   Z-ADD     *ZEROS        D98HRC
810000930512     C                   Z-ADD     *ZEROS        D98HDI
810100930512     C                   Z-ADD     *ZEROS        D98HDC
810200930520     C*
810300930512     C                   ENDSR
810400930305     C********************************************************************
810500971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
810600950406031059090120151181212243273304334365
810700950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
810800950406312831303130313130313031
810900930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
811000971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
811100971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
811200971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
811300971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
811400971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
811500971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
811600971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
811700971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
811800971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
811900971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
812000971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
812100971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
812200971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
812300971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
812400971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
812500971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
812600971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
812700971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
812800971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
812900971127TNSD99R- Bolla inesistente.                                               BOIN20
813000971127TNSD99R-     Cap inesistente.                                             CAPI21
813100971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
813200971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
813300971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
813400971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
813500971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
813600971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
813700971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
813800971127TNSD99R- Contrassegno non trovato.                                        CONT29
813900971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
814000990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
814100990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
814200060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
814300060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
814400971127TNSD99R- 35                                                                   35
814500971127TNSD99R- 36                                                                   36
814600971127TNSD99R- 37                                                                   37
814700971127TNSD99R- 38                                                                   38
814800971127TNSD99R- 39                                                                   39
814900971127TNSD99R- 40                                                                   40
