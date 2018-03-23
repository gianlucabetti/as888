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
037500140825     d
037600140825     D wrkoggiiso      s               d
037700930304     C*****************************************************************
037800930323     C*/ INIZIO MAIN DEL PGM
037900930304     C* ESEGUE ROUTINE INIZIALE
038000930304     C                   EXSR      RUTINZ
038100930528     C*
038200930528     C     D98ERR        IFEQ      '0'                                          --- 1 -->
038300930602     C*
038400930602     C* SE LANCIATO CON TIPO LANCIO='C' (CHIUDI FILE) VADO A FINE PGM
038500930602     C     D98TLA        IFNE      'C'                                          --- 2 -->
038600960729     C*
038700960729     C* CONTROLLO SE MI HANNO LANCIATO PER DETERMINARE LA CONSEGNABILITA'
038800960729     C                   MOVEL     'N'           FLCONS            1            *FLAG ESISTE CONSEGN
038900960729     C     D98DIC        IFGT      *ZEROS
039000960729     C     D98DFC        ORGT      *ZEROS
039100960729     C                   MOVEL     'S'           FLCONS                         *SI ESISTE CONSEGNAB
039200960729     C                   ENDIF
039300930323     C*
039400930511     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
039500931125     C* SE NON E' STATA IMPOSTATA DS DAL PGM CHIAMANTE (NSP=ZERO)
039600930420     C     D98ERR        IFEQ      '0'
039700010821     C     D98NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
039800930415     C     D98TBO        IFEQ      'A'
039900941227     C                   EXSR      LETARB                                       *LEGGE BOLLA ARRIVO
040000930323     C                   ELSE
040100941227     C                   EXSR      LETBLP                                       *LEGGE BOLLA PART.
040200930323     C                   END
040300140402     c
040400150401     c* Leggo fiar5 recod GEN  e FIAR4
040500140402     c                   exsr      leggiar5
040600140402     c
040700931206     C                   ELSE
040800931206     C* DS IMPOSTATA DAL PGM CHIAMANTE IMPOSTO CAMPI LAVORO
040900931206     C                   Z-ADD     D98DCG        W98DCG
041000990308     C                   MOVEL     *BLANKS       WRBL              1            *RAGGRUPP.BOLLE
041100990308     C                   MOVEL     *BLANKS       WFBR              1            *BOLLA DI RESO
041200031201     C                   MOVEL     *BLANKS       WGGA              2            *PARTICOLARIT' GIAC.
041300140822     C                   Z-ADD     d98DCR        s98DCR
041400140822     C                   Z-ADD     d98HCR        s98HCR
041500140822     C                   MOVEL     d98TCR        s98TCR
041600141104     c                   eval      gc1part=d98gc1
041700141104     c                   eval      gc2part=d98gc2
041800150303     c* imposto flag trasmissione bolla = 'N'
041900150303     c                   eval      wtrasm='N'
042000931125     C                   END
042100930420     C                   END
042200930528     C*
042300010821     C     D98ERR        IFEQ      '0'                                          --- 3 -->
042400050908     c* Se esiste la data consegnabilità, faccio finta che la bolla
042500050908     c*  non si consegnata, azzerando proprio la dta di consegna
042600050908     c*  perchè può trarre in inganno
042700050908     c                   if        d98dfc>0 and d98dcm>0
042800050908     c                   clear                   d98dcm
042900050908     c                   clear                   d98hmc
043000050908     c                   clear                   d98cca
043100050908     c                   endif
043200090902     c*
043300090902     c* Se non c'e' nè data consegna, ne data arrivo trazione
043400090902     c*  faccio finta che data arrivo trazione= udate utime ai fini dei calcoli
043500090902     c* in fondo al pgm dovrò azzerare i tempi di consegna interni e cliente
043600090902     c* perchè non calcolabili
043700090902     c                   clear                   wmancaTRAZ        1
043800090902     c
043900010821     C*
044000010821     C* EFFETTUO SUBITO CONSIDERAZIONI SU DIVISE (X QUESTIONE EURO)
044100010821     C                   EXSR      CHKVCA
044200141204     c
044300141204     c* Controllo se ha pratica di DANNO aperta
044400141204     c                   EXSR      CHKDAN
044500010108     C*
044600010108     C* SE BOLLA POSTA, UTILIZZA LA DATA BORDERO' ANZICHE' LA DATA SPEDIZIONE
044700091009     C**   D98TSP        IFEQ      'P'                                          *BOLLA POSTA
044800091009     C**   D98DBR        IFGT      *ZEROS                                       *DATA BORDERO' PRESE
044900091009     C**                 Z-ADD     D98DBR        D98DSP                         *DATA BORDERO' --> D
045000091009     C**                 ENDIF
045100091009     C**                 ENDIF
045200930304     C*
045300950110     C* RECUPERO I TEMPI TEORICI DI PARTENZA, TRAZIONE E DI CONSEGNA
045400930420     C     D98ERR        IFEQ      '0'
045500930323     C                   EXSR      RTTEOR
045600930420     C                   END
045700930528     C*
045800930602     C     D98ERR        IFEQ      '0'                                          --- 4 -->
045900970304     C*
046000970304     C* SE RICHIESTO LANCIO CON DATA RIFERIMENTO CONTROLLO LE DATE DA AZZERARE
046100970304     C     D98ERR        IFEQ      '0'
046200970304     C                   EXSR      DATRIF
046300970304     C                   END
046400940901     C*
046500940901     C* TRASF. DATA ARRIVO TRAZIONE E RICONSIDERO TEMPO CONSEGNA SE RITARDO
046600940901     C     D98ERR        IFEQ      '0'
046700940901     C                   EXSR      TDTIPO
046800940901     C                   END
046900930420     C*
047000930511     C* CARICO IL FILE DEGLI EVENTI BOLLA
047100930420     C     D98ERR        IFEQ      '0'
047200041116     c     d98nsp        andgt     0
047300930420     C                   EXSR      CAREVB
047400930420     C                   END
047500931213     C*
047600931213     C* CARICO LA SCHIERA DEI TURNI DI CHIUSURA
047700931213     C     D98ERR        IFEQ      '0'
047800931213     C                   EXSR      CARTUC
047900931213     C                   END
048000930528     C*
048100930602     C     D98ERR        IFEQ      '0'                                          --- 5 -->
048200930405     C*
048300940901     C* TRASFORMO LE DATE (SPED,CONS,ECC.) IN PROGRESSIVI ORE
048400930420     C     D98ERR        IFEQ      '0'
048500930405     C                   EXSR      TDATPO
048600930420     C                   END
048700930528     C*
048800930602     C     D98ERR        IFEQ      '0'                                          --- 6 -->
048900950110     C*
049000950110     C* CALCOLA LE DATE DI PREVISTA PARTENZA DELL'ULTIMO COLLO
049100950110     C     D98ERR        IFEQ      '0'
049200950110     C                   EXSR      CALDPP
049300950110     C                   END
049400930405     C*
049500930511     C* CALCOLA LE DATE DI PREVISTO ARRIVO DELLA TRAZIONE
049600930420     C     D98ERR        IFEQ      '0'
049700930511     C                   EXSR      CALDPT
049800090902     c*
049900090902     c* Ricalcolo le date facendo finta che la trazione stia arrivando
050000090903     c*  in questo momento se non presente
050100090902     c                   if        ricalcoloTRAZ='S'
050200090902     c                   eval      d98dti=WDAODI
050300090902     c                   eval      d98hti=WHHODI
050400090902     c                   eval      wmancaTRAZ='S'
050500090902     c
050600090902     C                   EXSR      TDTIPO
050700090902     C                   EXSR      CALDPP
050800090902     C                   EXSR      CALDPT
050900090902     c                   endif
051000090902     c
051100930420     C                   END
051200960229     C*
051300091009     d* D98TLA = 'T' --> in output solo i tempi di trazione
051400960229     C     D98TLA        IFNE      'T'                                          --- 6A ->
051500060407     C*
051600931206     C* CALCOLA IL RANGE PER IL CONTRASSEGNO
051700931206     C     D98ERR        IFEQ      '0'
051800931206     C                   EXSR      CALASS
051900931206     C                   END
052000931206     C*
052100931206     C* CALCOLA IL RANGE PER IL FERMO DEPOSITO
052200931206     C     D98ERR        IFEQ      '0'
052300931206     C                   EXSR      CALFED
052400931206     C                   END
052500931206     C*
052600931206     C* CALCOLA LE DATE TEORICHE DI CONSEGNA
052700931206     C     D98ERR        IFEQ      '0'
052800931206     C                   EXSR      CALDTE
052900931206     C                   END
053000930511     C*
053100930511     C* CALCOLA IL RANGE PER DATA CONSEGNA RICHIESTA
053200930511     C     D98ERR        IFEQ      '0'
053300930511     C                   EXSR      CALDCR
053400930511     C                   END
053500930511     C*
053600930511     C* CALCOLA I RANGE PER LASCIATO AVVISO
053700930511     C     D98ERR        IFEQ      '0'
053800930511     C                   EXSR      CALLAV
053900930511     C                   END
054000930511     C*
054100930511     C* CALCOLA IL RANGE PER LA GIACENZA
054200930511     C     D98ERR        IFEQ      '0'
054300930511     C                   EXSR      CALGIA
054400930511     C                   END
054500060420     C* CALCOLA IL RANGE PER Eventi Particolari che Allungano
054600060420     C     D98ERR        IFEQ      '0'
054700060420     C                   EXSR      CALcepa
054800060420     C                   END
054900930511     C*
055000930511     C* CALCOLA LE DATE DI PREVISTA CONSEGNA
055100930511     C     D98ERR        IFEQ      '0'
055200930511     C                   EXSR      CALDPC
055300930511     C                   END
055400960229     C*
055500960229     C                   ENDIF                                                  <-- 6A --
055600960229     C*
055700930602     C* TRASFORMO I PROGRESSIVI ORE IN DATE
055800930602     C                   EXSR      TPODAT
055900930602     C*
056000930914     C* CONTROLLO SE BOLLA RIENTRA IN UNO DEI CASI IN CUI NON RIESCO A CALCOLA
056100930602     C* RE I RITARDI PER CUI FORZO CONFORMITA' A SI
056200930602     C     D98ERR        IFEQ      '0'
056300960229     C     D98TLA        ANDNE     'T'
056400930602     C                   EXSR      CHKFOR
056500930602     C                   END
056600930602     C*
056700930602     C     D98ERR        IFEQ      '0'                                          --- 7 -->
056800960229     C     D98TLA        ANDNE     'T'
056900941228     C*
057000941228     C* CALCOLA LE ORE REALI DI PARTENZA
057100941228     C     D98ERR        IFEQ      '0'
057200941228     C                   EXSR      CALORP
057300941228     C                   END
057400930405     C*
057500930511     C* CALCOLA LE ORE REALI DI TRAZIONE
057600930420     C     D98ERR        IFEQ      '0'
057700930407     C                   EXSR      CALORT
057800930420     C                   END
057900930405     C*
058000930511     C* CALCOLA LE ORE REALI DI CONSEGNA
058100930420     C     D98ERR        IFEQ      '0'
058200930416     C                   EXSR      CALORC
058300930420     C                   END
058400930416     C*
058500930511     C* CALCOLA LE ORE REALI DI RESA
058600930420     C     D98ERR        IFEQ      '0'
058700930416     C                   EXSR      CALORR
058800930420     C                   END
058900930420     C*
059000930511     C* CALCOLA LE ORE DI DIFFERENZA REALI/TEORICHE
059100930420     C     D98ERR        IFEQ      '0'
059200930420     C                   EXSR      CALDIF
059300930420     C                   END
059400930420     C*
059500930511     C* CALCOLA LE CONFORMITA'
059600930420     C     D98ERR        IFEQ      '0'
059700930420     C                   EXSR      CALCNF
059800930420     C                   END
059900930913     C*
060000930913     C* CALCOLA LE ORE DI CONSEGNABILITA'
060100930913     C     D98ERR        IFEQ      '0'
060200950407     C     FLCONS        ANDEQ     'S'
060300930913     C                   EXSR      CALCNS
060400930913     C                   END
060500930423     C*
060600930511     C* PORTO I VALORI DAI CAMPI DI LAVORO AI CAMPI IN OUTPUT DELLA DS
060700930423     C     D98ERR        IFEQ      '0'
060800930423     C                   EXSR      W98D98
060900930423     C                   END
061000930528     C*
061100930602     C                   END                                                    <-- 7 ---
061200930602     C                   END                                                    <-- 6 ---
061300930528     C                   END                                                    <-- 5 ---
061400930528     C                   END                                                    <-- 4 ---
061500930528     C                   END                                                    <-- 3 ---
061600930528     C                   END                                                    <-- 2 ---
061700930528     C                   END                                                    <-- 1 ---
061800930323     C*
061900930324     C* SE PGM LANCIATO CON 'C' (LANCI MULTIPLI) CHIUDE CON LR (CHIUDE FILE)
062000930527     C* E LANCIA TUTTI I PGM CHIAMATI PER CHIUDERE I FILE DA LORO APERTI
062100930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
062200930324     C     D98TLA        IFEQ      'C'
062300970714     C                   CLEAR                   SI95
062400970701     C                   MOVEL     'C'           I95TLA
062500020226     C                   CALL      'TISI95R'
062600970701     C                   PARM                    SI95
062700040809     c*
062800040809     c                   clear                   fidn12ds
062900040809     c                   eval      i12tla = 'C'
063000040809     c                   call      'FIDN12R'
063100040809     c                   parm                    fidn12ds
063200050523     c**
063300050523     c* controllo riempimento skiera CAK solo alla fine
063400050523     c**                 clear                   trul0sds
063500050523     c**                 eval      i0sski='CAK/CAM/CAP'
063600050523     c**                 eval      i0sele=%elem(CAK)
063700050523     c**                 if        windca>i0sele
063800050523     c**                 eval      i0spie=i0sele
063900050523     c**                 else
064000050523     c**                 eval      i0spie=windca
064100050523     c**                 endif
064200050523     c**                 eval      i0sfile='AZCLN00F  '
064300050523     c**                 eval      i0scod='     '
064400050523     c**                 eval      i0spgm='TNSD99R'
064500050523     c**                 movel     trul0sds      kpjbu
064600050523     c**                 call      'TRUL0SR'
064700050523     c**                 parm                    kpjba
064800050523     c
064900930323     C                   SETON                                        LR
065000930324     C                   ELSE
065100930324     C                   SETON                                        RT
065200930323     C                   END
065300941227     C********************************************************************
065400941227     C*   LETARB - LEGGE IL FILE FNARB (BOLLE ARRIVI)
065500941227     C********************************************************************
065600941227     C     LETARB        BEGSR
065700941227     C*
065800140820     C* LEGGO IL FILE FNARB01L
065900941227     C                   Z-ADD     D98AAS        ARBAAS
066000941227     C                   Z-ADD     D98LNP        ARBLNP
066100941227     C                   Z-ADD     D98NRS        ARBNRS
066200941227     C                   Z-ADD     D98NSP        ARBNSP
066300941228     C     KEYARB        CHAIN     FNARB01L                           99
066400941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
066500941227     C                   MOVEL     '1'           D98ERR
066600941227     C                   MOVEL     MSG(20)       D98MSG                         *BOLLA INESISTENTE
066700941227     C                   END                                                    <-- 1 ---
066800941228     C     D98ERR        IFEQ      '0'                                          --- 1 --> *TROVATA
066900941227     C                   MOVEL     ARBTSP        D98TSP
067000140820     c* Se forzato da Ds lo imposto al posto di quello della bolla
067100130529     c                   if        i98tsp_for<>' '
067200130529     C                   MOVEL     i98tsp_for    D98TSP
067300130529     c                   endif
067400941228     C                   MOVEL     ARBNZD        D98NZD
067500941227     C                   MOVEL     ARBCAD        D98CAD
067600970701     C                   MOVEL     ARBLOD        D98LOD
067700941227     C                   Z-ADD     ARBTFP        D98TFP
067800941227     C                   Z-ADD     ARBTFA        D98TFA
067900941227     C                   Z-ADD     ARBLNA        D98LNA
068000941227     C                   Z-ADD     ARBAAS        DSAAS
068100941227     C                   Z-ADD     ARBMGS        DSMGS
068200941227     C                   Z-ADD     DSDAT8        D98DSP
068300941227     C                   Z-ADD     ARBMGS        D98MGS
068400941227     C                   Z-ADD     ARBDTI        D98DTI
068500941227     C                   Z-ADD     ARBHTI        D98HTI
068600941227     C                   Z-ADD     ARBDCM        D98DCM
068700941227     C                   Z-ADD     ARBHMC        D98HMC
068800941227     C                   MOVEL     ARBGC1        D98GC1
068900941227     C                   MOVEL     ARBGC2        D98GC2
069000941227     C                   Z-ADD     ARBDCR        D98DCR
069100941227     C                   Z-ADD     ARBHCR        D98HCR
069200100308     C                   MOVEL     ARBTCR        D98TCR
069300100308     c* salvo i campi perchè in alcuni casi li clearo
069400100308     C                   Z-ADD     ARBDCR        s98DCR
069500100308     C                   Z-ADD     ARBHCR        s98HCR
069600100308     C                   MOVEL     ARBTCR        s98TCR
069700100308     c
069800941227     C                   MOVEL     ARBCMC        D98CMC
069900941227     C                   MOVEL     ARBFFD        D98FFD
070000941227     C                   Z-ADD     ARBNCD        D98NCD
070100941227     C                   MOVEL     ARBFNS        D98FNS
070200941227     C                   Z-ADD     ARBDUC        D98DUC
070300150508     C                   Z-ADD     ARBDPC        W98DPC
070400970310     C                   Z-ADD     *ZEROS        W98HUC
070500071122     c
070600071122     c                   movel     arbcbo        d98cbo
070700140820     c                   z-add     arbddc        d98ddc
070800140825     c                   z-add     arbndc        wd98ndc
070900140911     c                   movel     arbfbc        wd98fbc
071000150119     c                   movel     arbacc        wd98acc
071100150325     c                   exsr      IN_dist
071200150331     c* Se in fermo deposito vedo se presente particlarità consegna
071300150331     c                   if        arbffd='S' and arbgma<>*blanks
071400150331     C                   Z-ADD     CODUT         KTBUTE
071500150331     C                   MOVEL     '7R'          KTBCOD
071600150331     C                   MOVEL(P)  arbgma        KTBKEY
071700150331     C     KEYTAB        chain     TABEL00F
071800150331     c                   if        %found(tabel00f)
071900150331     c                   movel     tbluni        ds7r
072000150331     c                   endif
072100150331     c                   endif
072200150325     c
072300070511     c
072400141215     C                   Z-ADD     ARBDBR        D98DBR
072500141215     c                   eval      d98fdrt=' '
072600070511     c
072700141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
072800070511     c* La data ritiro è presente solo su fnblp
072900070511     C     KEYARB        CHAIN     FNblp01L                           99
073000141215     c                   if        %found(fnblp01l)
073100070511     C                   Z-ADD     blpDrt        D98DBR
073200070511     c                   eval      d98fdrt='S'
073300070511     c                   endif
073400141215     c******             endif
073500070511     c
073600110923     C                   movel     ARBxco        D98xco
073700110923     C                   Z-ADD     ARBDAM        D98DAM
073800941227     C                   MOVEL     ARBFIN        D98FIN
073900990308     C                   MOVEL     ARBFBR        WFBR
074000031201     C                   MOVEL     ARBGGA        WGGA
074100050901     C                   MOVEL     ARBcca        d98cca
074200051124     C                   MOVEL     ARBTC1        d98TC1
074300051124     C                   MOVEL     ARBTC2        d98TC2
074400941227     C                   Z-ADD     1             I
074500941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
074600941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
074700941227     C                   MOVEL     '1'           D98ERR
074800941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
074900941227     C                   ELSE                                                   <-- 2 -->
075000941227     C                   MOVEL     T3A(I)        D98TBL
075100941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
075200990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
075300941227     C                   END                                                    <-- 2 ---
075400130826
075500130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
075600130826     c                   if        %subst(d98tbl:1:1) ='F'
075700130826     C                   Z-ADD     ARBKSC        D98KSC
075800130826     c                   else
075900130826     c
076000130826     c                   if        arbccm>0
076100130826     C                   Z-ADD     ARBccm        D98KSC
076200130826     c                   endif
076300130826     c                   if        d98ksc=0
076400130826     c                   z-add     8888          d98ksc
076500130826     c                   movel     d98lnp        d98ksc
076600130826     c                   endif
076700130826     c                   endif
076800130826     c
076900941227     C                   END                                                    <-- 1 ---
077000941228     C*
077100051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
077200950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
077300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
077400941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
077500941228     C                   Z-ADD     D98AAS        AR9AAS
077600941228     C                   Z-ADD     D98LNP        AR9LNP
077700941228     C                   Z-ADD     D98NRS        AR9NRS
077800941228     C                   Z-ADD     D98NSP        AR9NSP
077900051115     C     KEYAR9        CHAIN     FiAR901L                           99
078000950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
078100950102     C                   MOVEL     'S'           WFLASS
078200950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
078300950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
078400950102     C                   Z-ADD     D98AAS        LBLAAN
078500950102     C                   Z-ADD     D98LNP        LBLLPN
078600950102     C                   Z-ADD     D98NRS        LBLNRN
078700950102     C                   Z-ADD     D98NSP        LBLNSN
078800950102     C     KEYLBL        CHAIN     FNLBL01L                           99
078900950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
079000950102     C                   Z-ADD     LBLAAP        AR9AAS
079100950102     C                   Z-ADD     LBLLPP        AR9LNP
079200950102     C                   Z-ADD     LBLNRP        AR9NRS
079300950102     C                   Z-ADD     LBLNSP        AR9NSP
079400051115     C     KEYAR9        CHAIN     FiAR901L                           99
079500950102     C     *IN99         IFEQ      '0'
079600950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
079700950102     C                   ENDIF
079800950102     C                   ENDIF                                                  <-- 3 ---
079900950102     C                   ENDIF                                                  <-- 2 ---
080000950102     C*
080100950102     C* CONTRASSEGNO NON TROVATO
080200950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
080300941228     C                   ELSE                                                   <-- 2 -->
080400950102     C* CONTRASSEGNO TROVATO
080500941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
080600010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
080700010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
080800941228     C                   ENDIF                                                  <-- 2 ---
080900941228     C                   ENDIF                                                  <-- 1 ---
081000150325     c
081100941227     C*
081200050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
081300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
081400050405     C                   Z-ADD     D98AAS        KWGCPAAS
081500050405     C                   Z-ADD     D98LNP        KWGCPLNP
081600050405     C                   Z-ADD     D98NRS        KWGCPNRS
081700050405     C                   Z-ADD     D98NSP        KWGCPNSP
081800050405     C                   Z-ADD     *ZEROS        KWGCPFRG
081900050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
082000941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
082100941227     C     GCPATB        ANDNE     'A'
082200960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
082300960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
082400960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
082500050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
082600960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
082700960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
082800050405     C* O UGUALE A 35 INVECE DI 40)
082900960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
083000050405     C     GCPFAS        ANDGE     40
083100941227     C                   Z-ADD     GCPDCG        D98DCG
083200950112     C                   Z-ADD     GCPDCG        W98DCG
083300960123     C                   ENDIF
083400960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
083500960729     C     GCPFAS        ANDGE     35
083600070202     C     GCPFAS        ANDne     36
083700960729     C                   Z-ADD     GCPDCG        D98DCG
083800960729     C                   Z-ADD     GCPDCG        W98DCG
083900960729     C                   ENDIF
084000941228     C                   ENDIF
084100941228     C                   ENDIF                                                  <-- 1 ---
084200970911     C*
084300970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
084400970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
084500970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
084600050922     C*****              Z-ADD     D98DTI        W98DTI
084700971210     C                   Z-ADD     D98LNP        F
084800971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
084900970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
085000970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
085100150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
085200150120     c                   if        arbdcp=0 and d98dfc=0
085300150120     C                   Z-ADD     d98dam        D98DTI
085400150120     c                   endif
085500150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
085600150120     C                   Z-ADD     d98dam        D98DTI
085700150120     c                   endif
085800150120     c                   ENDIF
085900150120     c***
086000150120     C***                Z-ADD     D98AAS        ARTAAS
086100150120     C***                Z-ADD     D98LNP        ARTLNP
086200150120     C***                Z-ADD     D98NRS        ARTNRS
086300150120     C***                Z-ADD     D98NSP        ARTNSP
086400150120     C***  KEYART        SETLL     FNART01L                           99
086500150120     C**N99KEYART        READE     FNART01L                               99
086600150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
086700150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
086800970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
086900150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
087000970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
087100970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
087200150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
087300150120     C***  ARTDCM        ANDEQ     *ZEROS
087400970912     C* SE PRIMO REC. TROVATO, MEM. DATA
087500150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
087600150120     C***                Z-ADD     ARTDAM        D98DTI
087700150120     C***                ELSE                                                   <-- 6 -->
087800970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
087900150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
088000150120     C***                Z-ADD     ARTDAM        D98DTI
088100150120     C***                ENDIF                                                  <-- 7 ---
088200150120     C***                ENDIF                                                  <-- 6 ---
088300150120     C***                ENDIF                                                  <-- 5 ---
088400970911     C* C'E' DATA CONSEGNAB.
088500150120     C***                ELSE                                                   <-- 4 -->
088600970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
088700970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
088800150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
088900150120     C***  ARTDAM        ANDLE     D98DFC
089000970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
089100970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
089200150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
089300150120     C***  ARTDCM        ORGT      D98DFC
089400970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
089500150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
089600150120     C***                Z-ADD     ARTDAM        D98DTI
089700150120     C***                ELSE                                                   <-- 7 -->
089800970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
089900150120     C***  ARTDAM        IFLT      D98DTI
090000150120     C***                Z-ADD     ARTDAM        D98DTI
090100150120     C***                ENDIF
090200150120     C***                ENDIF                                                  <-- 7 ---
090300150120     C***                ENDIF                                                  <-- 6 ---
090400150120     C***                ENDIF                                                  <-- 5 ---
090500150120     C***                ENDIF                                                  <-- 4 ---
090600150120     C***                ENDIF                                                  <-- 3 ---
090700150120     C***  KEYART        READE     FNART01L                               99
090800150120     C***                ENDDO                                                  <-- 2 ---
090900150120     C***                ENDIF                                                  <-- 1 ---
091000941227     C*
091100941227     C                   ENDSR
091200150325     C********************************************************************
091300150325     C*   Verifico se bolla in distinta
091400150325     C********************************************************************
091500150325     c     IN_dist       BEGSR
091600150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
091700150325     c                   if         ARBfbc = *blanks and
091800150325     c                               ARBddc > 0 and
091900150325     c                               ARBndc > 0 and ARBdcm=0
092000150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
092100150325     c                   if        not %open(fiapd01l)
092200150325     c                   open      fiapd01l
092300150325     c                   endif
092400150325     c
092500150325     C                   move      'A'           kpdtip
092600150325     c                   eval      kpdpdr=arbpdc
092700150325     C     kfapd         chain     fiapd01l
092800150325     c
092900150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
093000150325     c                   eval       d98cons = 'S'
093100150325     c                   endif
093200150325     c                   endif
093300150325     c                   ENDSR
093400941227     C********************************************************************
093500941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
093600941227     C********************************************************************
093700941227     C     LETBLP        BEGSR
093800941227     C*
093900941227     C                   Z-ADD     D98AAS        BLPAAS
094000941227     C                   Z-ADD     D98LNP        BLPLNP
094100941227     C                   Z-ADD     D98NRS        BLPNRS
094200941227     C                   Z-ADD     D98NSP        BLPNSP
094300941227     C     KEYBLP        CHAIN     FNBLP01L                           99
094400941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
094500941227     C                   MOVEL     '1'           D98ERR
094600941227     C                   MOVEL     MSG(20)       D98MSG
094700941227     C                   END                                                    <-- 1 ---
094800941228     C     *IN99         IFEQ      '0'                                                    *TROV.
094900041028     C     KEYBLP        CHAIN     FNarb01L
095000041028     c                   if        %found(fnarb01l)
095100041028     c                   eval      wesarb='S'
095200041028     c                   else
095300041028     c                   eval      wesarb='N'
095400150219     c                   eval      wtrasm='D'
095500041028     c                   endif
095600141216     c
095700141216     c* se non trasmessa la bolla in arrivo memorizzo
095800150219     c                   if        blpft1= ' '
095900141229     c                   eval      wtrasm='N'
096000141216     c                   endif
096100141216     c
096200110923     C                   MOVEL     BLPxco        D98xco
096300110923     C                   MOVEL     BLPTSP        D98TSP
096400130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
096500130529     c                   if        i98tsp_for<>' '
096600130529     C                   MOVEL     i98tsp_for    D98TSP
096700130529     c                   endif
096800960209     C                   MOVEL     BLPNZD        D98NZD
096900941228     C                   MOVEL     BLPCAD        D98CAD
097000970701     C                   MOVEL     BLPLOD        D98LOD
097100941227     C                   Z-ADD     BLPTFP        D98TFP
097200941227     C                   Z-ADD     BLPTFA        D98TFA
097300941227     C                   Z-ADD     BLPLNA        D98LNA
097400941227     C                   Z-ADD     BLPAAS        DSAAS
097500941227     C                   Z-ADD     BLPMGS        DSMGS
097600941227     C                   Z-ADD     DSDAT8        D98DSP
097700941227     C                   Z-ADD     BLPMGS        D98MGS
097800051124     C                   movel     BLPTC1        D98TC1
097900051124     C                   movel     BLPTC2        D98TC2
098000041028     c                   if        wesarb='S'
098100041028     C                   Z-ADD     arbDTI        D98DTI
098200041028     C                   Z-ADD     arbHTI        D98HTI
098300041028     C                   Z-ADD     arbDCM        D98DCM
098400041028     C                   Z-ADD     arbHMC        D98HMC
098500041028     C                   Z-ADD     arbDAM        D98DAM
098600050901     C                   MOVEL     arbcca        d98cca
098700140820     C                   MOVEL     arbddc        d98ddc
098800140911     C                   z-add     arbndc        wd98ndc
098900140910     C                   MOVEL     arbfbc        wd98fbc
099000150119     c                   movel     arbacc        wd98acc
099100150325     c                   exsr      IN_DIST
099200140820     c*
099300041028     c                   else
099400941227     C                   Z-ADD     BLPDTI        D98DTI
099500941227     C                   Z-ADD     BLPHTI        D98HTI
099600941227     C                   Z-ADD     BLPDCM        D98DCM
099700941227     C                   Z-ADD     BLPHMC        D98HMC
099800041028     C                   Z-ADD     BLPDAM        D98DAM
099900050901     C                   MOVEL     BLPcca        d98cca
100000140820     C                   MOVEL     BLPddc        d98ddc
100100140825     C                   MOVEL     BLPndc        wd98ndc
100200140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
100300140820     c                   clear                   d98cons
100400041028     c                   endif
100500941227     C                   MOVEL     BLPGC1        D98GC1
100600941227     C                   MOVEL     BLPGC2        D98GC2
100700941227     C                   Z-ADD     BLPDCR        D98DCR
100800941227     C                   Z-ADD     BLPHCR        D98HCR
100900941227     C                   MOVEL     BLPTCR        D98TCR
101000100308     c* Salvo i campi perchè in alcuni casi li clearo
101100100308     C                   Z-ADD     blpDCR        s98DCR
101200100308     C                   Z-ADD     blpHCR        s98HCR
101300100308     C                   MOVEL     blpTCR        s98TCR
101400100308     c
101500941227     C                   MOVEL     *BLANKS       D98CMC
101600941227     C                   MOVEL     BLPFFD        D98FFD
101700941227     C                   Z-ADD     BLPNCD        D98NCD
101800941227     C                   MOVEL     BLPFNS        D98FNS
101900941228     C                   Z-ADD     BLPDUC        D98DUC
102000150508     C                   Z-ADD     BLPDPC        w98DPC
102100970310     C                   Z-ADD     *ZEROS        W98HUC
102200071122     c                   movel     blpcbo        d98cbo
102300150331     c*
102400150331     c* Se in fermo deposito vedo se presente particlarità consegna
102500150331     c                   if        blpffd='S' and blpgma<>*blanks
102600150331     C                   Z-ADD     CODUT         KTBUTE
102700150331     C                   MOVEL     '7R'          KTBCOD
102800150331     C                   MOVEL(P)  blpgma        KTBKEY
102900150331     C     KEYTAB        chain     TABEL00F
103000150331     c                   if        %found(tabel00f)
103100150331     c                   movel     tbluni        ds7r
103200150331     c                   endif
103300150331     c                   endif
103400150331     c
103500970304     C                   Z-ADD     BLPDBR        D98DBR
103600070514     c                   eval      d98fdrt=' '
103700070514     c
103800141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
103900070514     C                   Z-ADD     blpDrt        D98DBR
104000070514     c                   eval      d98fdrt='S'
104100141215     c********           endif
104200070514     c*
104300941227     C                   MOVEL     BLPFIN        D98FIN
104400130826     C****               Z-ADD     BLPKSC        D98KSC
104500990308     C                   MOVEL     BLPFBR        WFBR
104600031201     C                   MOVEL     BLPGGA        WGGA
104700941227     C                   Z-ADD     1             I
104800941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
104900941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
105000941227     C                   MOVEL     '1'           D98ERR
105100941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
105200941227     C                   ELSE                                                   <-- 2 -->
105300941227     C                   MOVEL     T3A(I)        D98TBL
105400941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
105500990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
105600941227     C                   END                                                    <-- 2 ---
105700130826     c                   if        %subst(d98tbl:1:1) ='F'
105800130826     C                   Z-ADD     blpKSC        D98KSC
105900130826     c                   else
106000130826     c
106100130826     c                   if        blpccm>0
106200130826     C                   Z-ADD     blpccm        D98KSC
106300130826     c                   endif
106400130826     c                   if        d98ksc=0
106500130826     c                   z-add     8888          d98ksc
106600130826     c                   movel     d98lnp        d98ksc
106700130826     c                   endif
106800130826     c                   endif
106900130826     c
107000941227     C                   END
107100941228     C*
107200051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
107300950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
107400941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
107500941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
107600051115     C                   Z-ADD     D98AAS        ar9AAS
107700051115     C                   Z-ADD     D98LNP        ar9LNP
107800051115     C                   Z-ADD     D98NRS        ar9NRS
107900051115     C                   Z-ADD     D98NSP        ar9NSP
108000051115     C     KEYar9        CHAIN     Fiar901L                           99
108100950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
108200950102     C                   MOVEL     'S'           WFLASS
108300950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
108400950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
108500950102     C                   Z-ADD     D98AAS        LBLAAN
108600950102     C                   Z-ADD     D98LNP        LBLLPN
108700950102     C                   Z-ADD     D98NRS        LBLNRN
108800950102     C                   Z-ADD     D98NSP        LBLNSN
108900950102     C     KEYLBL        CHAIN     FNLBL01L                           99
109000950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
109100051115     C                   Z-ADD     LBLAAP        ar9AAS
109200051115     C                   Z-ADD     LBLLPP        ar9LNP
109300051115     C                   Z-ADD     LBLNRP        ar9NRS
109400051115     C                   Z-ADD     LBLNSP        ar9NSP
109500051115     C     KEYar9        CHAIN     Fiar901L                           99
109600950102     C     *IN99         IFEQ      '0'
109700950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
109800950102     C                   ENDIF
109900950102     C                   ENDIF                                                  <-- 3 ---
110000950102     C                   ENDIF                                                  <-- 2 ---
110100950102     C*
110200950102     C* CONTRASSEGNO TROVATO
110300150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
110400051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
110500051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
110600051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
110700941228     C                   ENDIF                                                  <-- 2 ---
110800941228     C                   ENDIF                                                  <-- 1 ---
110900941227     C*
111000050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
111100941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
111200050405     C                   Z-ADD     D98AAS        KWGCPAAS
111300050405     C                   Z-ADD     D98LNP        KWGCPLNP
111400050405     C                   Z-ADD     D98NRS        KWGCPNRS
111500050405     C                   Z-ADD     D98NSP        KWGCPNSP
111600050405     C                   Z-ADD     *ZEROS        KWGCPFRG
111700050405     C     KEYGCP        CHAIN     TIGCP01L                           99
111800941228     C     *IN99         IFEQ      '0'                                          *TROV.
111900941227     C     GCPATB        ANDNE     'A'
112000960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
112100960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
112200960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
112300960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
112400050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
112500960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
112600960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
112700050405     C* O UGUALE A 35 INVECE DI 40)
112800960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
112900050405     C     GCPFAS        ANDGE     40
113000941227     C                   Z-ADD     GCPDCG        D98DCG
113100950112     C                   Z-ADD     GCPDCG        W98DCG
113200960123     C                   ENDIF
113300960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
113400960729     C     GCPFAS        ANDGE     35
113500070202     C     GCPFAS        ANDne     36
113600960729     C                   Z-ADD     GCPDCG        D98DCG
113700960729     C                   Z-ADD     GCPDCG        W98DCG
113800960729     C                   ENDIF
113900941228     C                   ENDIF
114000941228     C                   ENDIF                                                  <-- 1 ---
114100970912     C*
114200970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
114300970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
114400970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
114500050922     C******             Z-ADD     D98DTI        W98DTI
114600971210     C                   Z-ADD     D98LNP        F
114700971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
114800970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
114900970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
115000150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
115100150120     c                   if        blpdcp=0 and d98dfc=0
115200150120     C                   Z-ADD     d98dam        D98DTI
115300150120     c                   endif
115400150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
115500150120     C                   Z-ADD     d98dam        D98DTI
115600150120     c                   endif
115700150120     c                   ENDIF
115800150120     c*
115900150120     C***                Z-ADD     D98AAS        BLTAAS
116000150120     C****               Z-ADD     D98LNP        BLTLNP
116100150120     C***                Z-ADD     D98NRS        BLTNRS
116200150120     C***                Z-ADD     D98NSP        BLTNSP
116300150120     C***  KEYBLT        SETLL     FNBLT01L                           99
116400150120     C**N99KEYBLT        READE     FNBLT01L                               99
116500150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
116600150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
116700970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
116800150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
116900970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
117000970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
117100150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
117200150120     C**   BLTDCM        ANDEQ     *ZEROS
117300970912     C* SE PRIMO REC. TROVATO, MEM. DATA
117400150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
117500150120     C**                 Z-ADD     BLTDAM        D98DTI
117600150120     C**                 ELSE                                                   <-- 6 -->
117700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
117800150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
117900150120     C**                 Z-ADD     BLTDAM        D98DTI
118000150120     C**                 ENDIF                                                  <-- 7 ---
118100150120     C**                 ENDIF                                                  <-- 6 ---
118200150120     C**                 ENDIF                                                  <-- 5 ---
118300970912     C* C'E' DATA CONSEGNAB.
118400150120     **                  ELSE                                                   <-- 4 -->
118500970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
118600970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
118700150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
118800150120     C**   BLTDAM        ANDLT     D98DFC
118900970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
119000970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
119100150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
119200150120     C**   BLTDCM        ORGT      D98DFC
119300970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
119400150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
119500150120     C**                 Z-ADD     BLTDAM        D98DTI
119600150120     C**                 ELSE                                                   <-- 7 -->
119700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
119800150120     C**   BLTDAM        IFLT      D98DTI
119900150120     C**                 Z-ADD     BLTDAM        D98DTI
120000150120     C**                 ENDIF
120100150120     C**                 ENDIF                                                  <-- 7 ---
120200150120     C**                 ENDIF                                                  <-- 6 ---
120300150120     C**                 ENDIF                                                  <-- 5 ---
120400150120     C**                 ENDIF                                                  <-- 4 ---
120500150120     C**                 ENDIF                                                  <-- 3 ---
120600150120     C**   KEYBLT        READE     FNBLT01L                               99
120700150120     C**                 ENDDO                                                  <-- 2 ---
120800150120     C**                 ENDIF                                                  <-- 1 ---
120900941227     C*
121000941227     C                   ENDSR
121100930323     C********************************************************************
121200941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
121300930323     C********************************************************************
121400930323     C     RTTEOR        BEGSR
121500941227     C*
121600941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
121700941228     C     D98ERR        IFEQ      '0'
121800941227     C                   EXSR      RTPART
121900941227     C                   ENDIF
122000941227     C*
122100941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
122200941228     C     D98ERR        IFEQ      '0'
122300150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
122400150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
122500150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
122600150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
122700150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
122800941227     C                   EXSR      RTTRAZ
122900150219     c                   if        d98err<>'1'
123000150219     C                   Z-ADD     CASTIN        D98TTI
123100150219     C                   Z-ADD     CASTCN        D98TTC
123200150219     C                   endif
123300150219     C                   ENDIF
123400941227     C*
123500941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
123600941228     C     D98ERR        IFEQ      '0'
123700941227     C                   EXSR      RTCONS
123800941227     C                   ENDIF
123900941227     C*
124000941228     C     D98ERR        IFEQ      '0'
124100941227     C*
124200941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
124300941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
124400941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
124500090921     c                   z-add     d98tri        w99tri
124600090921     c* per la DS
124700090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
124800090921     c* per i calcoli interi
124900090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
125000090921     c
125100941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
125200941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
125300941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
125400090921     c                   z-add     d98trc        w99trc
125500090921     c* per la DS
125600090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
125700090921     c* per i calcoli interi
125800090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
125900941227     C*
126000941227     C                   ENDIF
126100930323     C*
126200930323     C                   ENDSR
126300941227     C********************************************************************
126400941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
126500941227     C********************************************************************
126600941227     C     RTPART        BEGSR
126700941228     C*
126800941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
126900941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
127000941228     C                   MOVEL     D98TSP        WTSP              1
127100130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
127200130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
127300050301     C*******
127400050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
127500050301     C*
127600050301     C*    W001          IFEQ      'A'
127700050301     C*    W0040         OREQ      8888
127800050301     C*                  MOVEL     'E'           WTSP
127900050301     C*                  ENDIF
128000050301     C*******
128100941227     C*
128200941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
128300941227     C                   MOVEL     'N'           WTRAOK            1
128400941227     C                   MOVEL     'P'           CASPTA
128500941227     C                   Z-ADD     D98LNP        CASLNP
128600941227     C                   Z-ADD     *ZEROS        CASLNA
128700941228     C                   MOVEL     WTSP          CASTSP
128800941228     C                   Z-ADD     99999999      CASDDE
128900941227     C     KEYCAS        SETGT     AZCAS01L                             99
129000941227     C  N99              READP     AZCAS01L                               99
129100941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
129200941227     C     WTRAOK        ANDEQ     'N'
129300941227     C     CASLNP        ANDEQ     D98LNP
129400950102     C     CASTSP        ANDEQ     WTSP
129500941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
129600941227     C     CASATB        ANDEQ     *BLANKS
129700941227     C                   MOVEL     'S'           WTRAOK
129800941227     C                   ELSE                                                   <-- 2 -->
129900941227     C                   READP     AZCAS01L                               99
130000941227     C                   ENDIF                                                  <-- 2 ---
130100941227     C                   ENDDO                                                  <-- 1 ---
130200941227     C*
130300941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
130400941227     C     WTRAOK        IFEQ      'N'
130500941227     C                   MOVEL     '1'           D98ERR
130600941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
130700941227     C                   ELSE
130800941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
130900941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
131000941227     C                   END
131100970214     C*
131200140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
131300140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
131400140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
131500140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
131600140403     C**                 Z-ADD     D98LNA        F
131700140403     C**   D98ERR        IFEQ      '0'
131800140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
131900140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
132000140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
132100140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
132200140403     C**                 ENDIF
132300140403     C**                 ENDIF
132400140402     c
132500140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
132600140402     c*  se tempi  = 0
132700140402     c                   if        wpick2='S' and d98tpi<=0
132800140402     c                   add       24            D98TPI
132900140402     c                   endif
133000941227     C*
133100941227     C                   ENDSR
133200941227     C********************************************************************
133300941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
133400941227     C********************************************************************
133500941227     C     RTTRAZ        BEGSR
133600941227     C*
133700941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
133800150219     c
133900150219     C                   Z-ADD     D98LNA        LTA(1)
134000150219     C                   Z-ADD     D98LNA        LTA(2)
134100150219     C                   Z-ADD     D98TFA        LTA(3)
134200941227     C                   Z-ADD     D98TFA        LTA(4)
134300941227     C*
134400941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
134500941227     C                   MOVEL     'N'           WTRAOK            1
134600941227     C                   Z-ADD     1             K                 3 0
134700941227     C*
134800941227     C* CICLO FINO A:
134900941227     C*    - FINE SCHIERA
135000941227     C*    - TROVATO TEMPI VALIDI
135100941227     C     K             DOUGT     4                                            --- 1 -->
135200941227     C     WTRAOK        OREQ      'S'
135300941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
135400941227     C     LTA(K)        ORNE      *ZEROS
135500941227     C                   MOVEL     'T'           CASPTA
135600941227     C                   Z-ADD     LTP(K)        CASLNP
135700941227     C                   Z-ADD     LTA(K)        CASLNA
135800941227     C                   MOVEL     D98TSP        CASTSP
135900941228     C                   Z-ADD     99999999      CASDDE
136000941227     C     KEYCAS        SETGT     AZCAS01L                             99
136100941227     C  N99              READP     AZCAS01L                               99
136200941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
136300941227     C     WTRAOK        ANDEQ     'N'
136400941227     C     LTP(K)        ANDEQ     CASLNP
136500941227     C     LTA(K)        ANDEQ     CASLNA
136600941227     C     D98TSP        ANDEQ     CASTSP
136700960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
136800941227     C     CASATB        ANDEQ     *BLANKS
136900941227     C                   MOVEL     'S'           WTRAOK
137000941227     C                   ELSE                                                   <-- 4 -->
137100941227     C                   READP     AZCAS01L                               99
137200941227     C                   ENDIF                                                  <-- 4 ---
137300941227     C                   ENDDO                                                  <-- 3 ---
137400941227     C                   ENDIF                                                  <-- 2 ---
137500941227     C                   ADD       1             K
137600941227     C                   ENDDO                                                  <-- 1 ---
137700941227     C*
137800941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
137900941227     C     WTRAOK        IFEQ      'N'
138000941227     C                   MOVEL     '1'           D98ERR
138100941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
138200150219     C***                ELSE
138300150219     C***                Z-ADD     CASTIN        D98TTI
138400150219     C***                Z-ADD     CASTCN        D98TTC
138500941227     C                   END
138600941227     C*
138700941227     C                   ENDSR
138800941227     C********************************************************************
138900941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
139000941227     C********************************************************************
139100941227     C     RTCONS        BEGSR
139200150629     c                   clear                   WTsogliaE         1
139300970701     C*
139400970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
139500970701     C                   CLEAR                   SI95
139600970701     C                   MOVEL     '3'           I95TCN
139700970701     C                   MOVEL     D98NZD        I95NAR
139800970701     C                   MOVEL     D98CAD        I95CAP
139900970701     C                   MOVEL     D98LOD        I95LOC
140000970701     C                   MOVEL     D98DSP        I95DAT
140100970701     C                   MOVEL     D98TSP        I95TSP
140200980602     C                   MOVEL     'S'           I95FRT
140300150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
140400150511     c                   if        d98ffd='S'
140500150511     c                   eval      i95tsp='C'
140600150511     c                   endif
140700020226     C*
140800020226     C* VERIFICO SE LNA E' FILIALE FEDEX
140900020226     C                   IF        D98LNA > *ZEROS
141000020226     C     D98LNA        LOOKUP    FILFED                                 55
141100020226     C                   IF        *IN55 = *ON
141200020226     C                   EVAL      I95FI1 = 'S'
141300020226     C                   ENDIF
141400020226     C                   ENDIF
141500020226     C*
141600020226     C                   CALL      'TISI95R'
141700970701     C                   PARM                    SI95
141800941227     C*
141900941228     C* SE NON TROVATO, ERRORE
142000970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
142100941227     C                   MOVEL     '1'           D98ERR
142200970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
142300941227     C                   ELSE                                                   <-- 1 -->
142400941227     C*
142500941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
142600941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
142700941227     C     CASTIN        DIV       24            WDIV              3 0
142800941227     C                   MVR                     WRES              3 0
142900941227     C     WRES          IFEQ      *ZEROS
143000970701     C                   Z-ADD     O950IN        D98TCI
143100941227     C                   ELSE
143200970701     C                   Z-ADD     O951IN        D98TCI
143300941227     C                   END
143400941227     C*
143500941227     C     CASTCN        DIV       24            WDIV                     99
143600941227     C                   MVR                     WRES
143700941227     C     WRES          IFEQ      *ZEROS
143800970701     C                   Z-ADD     O950CN        D98TCC
143900150629     c                   eval      wTSogliaE='0'
144000941227     C                   ELSE
144100970701     C                   Z-ADD     O951CN        D98TCC
144200150629     c                   eval      wTSogliaE='1'
144300941227     C                   END
144400941227     C*
144500150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
144600941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
144700150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
144800150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
144900150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
145000150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
145100150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
145200941227     C*
145300941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
145400941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
145500970701     C     O95ISO        IFEQ      'I'
145600941227     C     D98FIN        OREQ      'I'
145700970701     C                   ADD       O95TIS        D98TCI
145800970701     C                   ADD       O95TIS        D98TCC
145900941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
146000970701     C                   ADD       O95TIS        W990IN
146100970701     C                   ADD       O95TIS        W991IN
146200150629     C                   ADD       O95TIS        W990CN
146300150629     C                   ADD       O95TIS        W991CN
146400941227     C                   END
146500090827     c
146600090827     c* Sistemo i tempi di consegna se non sono multipli di 12
146700090827     c                   exsr      SisCONS
146800160404     c
146900160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
147000160404     c                   if        wVP='S'
147100160404     c                   add       24            w991IN
147200160404     c                   add       24            w991CN
147300160404     c                   add       24            w990IN
147400160404     c                   add       24            w990CN
147500160404     c                   endif
147600941228     C*
147700941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
147800941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
147900941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
148000941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
148100941228     C                   Z-ADD     D98TCC        W99CCE            3 0
148200941228     C                   ELSE                                                   <-- 3 -->
148300970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
148400970701     C                   CLEAR                   SI95
148500970701     C                   MOVEL     '3'           I95TCN
148600970701     C                   MOVEL     D98NZD        I95NAR
148700970701     C                   MOVEL     D98CAD        I95CAP
148800970701     C                   MOVEL     D98LOD        I95LOC
148900970701     C                   MOVEL     D98DSP        I95DAT
149000970701     C                   MOVEL     'E'           I95TSP
149100980602     C                   MOVEL     'S'           I95FRT
149200020226     C*
149300020226     C* VERIFICO SE LNA E' FILIALE FEDEX
149400020226     C                   IF        D98LNA > *ZEROS
149500020226     C     D98LNA        LOOKUP    FILFED                                 55
149600020226     C                   IF        *IN55 = *ON
149700020226     C                   EVAL      I95FI1 = 'S'
149800020226     C                   ENDIF
149900020226     C                   ENDIF
150000020226     C*
150100020226     C                   CALL      'TISI95R'
150200970701     C                   PARM                    SI95
150300970701     C*
150400941228     C* SE NON TROVATO, ERRORE
150500970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
150600941228     C                   MOVEL     '1'           D98ERR
150700970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
150800941228     C                   ELSE                                                   <-- 4 -->
150900941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
151000941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
151100941228     C     CASTCN        DIV       24            WDIV                     99
151200941228     C                   MVR                     WRES
151300941228     C     WRES          IFEQ      *ZEROS
151400970701     C                   Z-ADD     O950CN        W99CCE
151500941228     C                   ELSE
151600970701     C                   Z-ADD     O951CN        W99CCE
151700941228     C                   END
151800941228     C                   ENDIF                                                  <-- 4 ---
151900941228     C                   ENDIF                                                  <-- 3 ---
152000941228     C                   ENDIF                                                  <-- 2 ---
152100941227     C*
152200941227     C                   END                                                    <-- 1 ---
152300941227     C*
152400941227     C                   ENDSR
152500090827     C********************************************************************
152600090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
152700090827     C********************************************************************
152800090827     C     SisCONS       BEGSR
152900090827     c                   eval      w99tci=d98tci
153000090827     c                   if        d98tci=10
153100090827     c                   z-add     12            w99tci
153200090827     c                   endif
153300090827     c
153400090827     c                   eval      w99tcc=d98tcc
153500090827     c                   if        d98tcc=10
153600090827     c                   z-add     12            w99tcc
153700090827     c                   endif
153800160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
153900160404     c                   if        wVP='S'
154000160404     c                   add       24            w99tcc
154100160404     c                   add       24            w99tci
154200160404     c                   endif
154300090827     c
154400090827     C                   ENDSR
154500930420     C********************************************************************
154600930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
154700930420     C********************************************************************
154800930420     C     CAREVB        BEGSR
154900930420     C*
155000930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
155100930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
155200060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
155300930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
155400930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
155500060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
155600060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
155700930420     C                   Z-ADD     D98AAS        KEBAAS
155800930420     C                   Z-ADD     D98LNP        KEBLNP
155900930420     C                   Z-ADD     D98NRS        KEBNRS
156000930420     C                   Z-ADD     D98NSP        KEBNSP
156100990810     C     KEYEVB        SETLL     FNEVB01L                           99
156200060406     C  N99keyevb        READe     FNEVB01L                               99
156300060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
156400930421     C     D98ERR        ANDEQ     '0'
156500060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
156600930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
156700930421     C                   Z-ADD     1             I
156800930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
156900060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
157000930421     C                   MOVEL     '1'           D98ERR
157100930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
157200060406   x3C                   ELSE                                                   <-- 3 -->
157300060406     C                   MOVEL     D2A(I)        usa2A
157400060406     C                   MOVEL     D2Z(I)        usa2Z
157500930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
157600060406    4C     §2AFTC        IFEQ      'A'
157700930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
157800060406    4C                   END
157900060406    4C     §2AFTC        IFEQ      'G'
158000930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
158100060406    4C                   END
158200150216     c* Se si tratta di MIC verifico se uguale alla sped.
158300150303    4c                   if        evbcev='MIC' or evbcev='NIC'
158400150303    5c                   if        %subst(evbnot:1:7) >*zeros
158500150303     c
158600150303    6c                   if        evbcev='MIC' and
158700150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
158800150216     c                   eval      evemic='SI'
158900150303    6c                   endif
159000150303     c
159100150303    6c                   if        evbcev='NIC' and
159200150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
159300150216     c                   eval      evemic='  '
159400150303    6c                   endif
159500150303     c
159600150303   x5c                   else
159700150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
159800150303     c                   eval      evemic='SI'
159900150303    6c                   endif
160000150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
160100150303     c                   eval      evemic='  '
160200150303    6c                   endif
160300150303
160400150303    5c                   endif
160500150303    4c                   endif
160600150216     c
160700060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
160800930421     C     §2AFTC        OREQ      'G'
160900930421     C                   ADD       1             IE
161000990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
161100930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
161200930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
161300131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
161400090928     c
161500090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
161600091014     c**                 if        ie=1
161700091014     c**                 eval      Primotipo=§2aftc
161800091014     c**                 eval      Primodev=evbdev
161900091014     c**                 eval      Primohev=evbhev
162000091014     c**                 endif
162100090928     c
162200060406    4C                   END                                                    <-- 4 ---
162300060406     c* Verifico se si tratta di evento particolare
162400060406    4c                   if        §2zCEP<>' '
162500060526     c* non carico l'evento se cade in giorno festivo
162600060526     c                   z-add     evbdev        wpodata
162700060526     C                   EVAL      WTC='C'
162800060526     C                   EVAL      WIC='C'
162900060526     c                   eval      wslitt='N'
163000060526     c                   exsr      chkfes
163100060526     c
163200060526     c* Wslitt='N' non festivo
163300060526   4ac                   if        wslitt='N'
163400060526     c
163500060406    5c                   if        §2zfep='A'
163600060406     c                   eval      wevpaA='S'
163700060406    5C                   END                                                    <-- 3 ---
163800060406    5c                   if        §2zfep='G'
163900060406     c                   eval      wevpaG='S'
164000060406    5C                   END                                                    <-- 3 ---
164100060406     c                   add       1             ip
164200060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
164300060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
164400060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
164500060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
164600060526   4aC                   ENDif                                                  <-- 3 ---
164700060526    4C                   END                                                    <-- 3 ---
164800060406    3C                   END                                                    <-- 3 ---
164900060406    2C                   END                                                    <-- 2 ---
165000060406     C     keyevb        READe     FNEVB01L                               99
165100060406    1C                   ENDDO                                                  <-- 1 ---
165200050523     c* controllo riempimento skiere
165300050523     c                   clear                   trul0sds
165400050523     c                   eval      i0sski='EVD/EVO/EVC'
165500050523     c                   eval      i0sele=%elem(EVD)
165600050523     c                   eval      i0spie=ie
165700050523     c                   eval      i0sfile='FNEVB01L  '
165800050523     c                   eval      i0scod='     '
165900050523     c                   eval      i0spgm='TNSD99R'
166000050523     c                   movel     trul0sds      kpjbu
166100050523     c                   call      'TRUL0SR'
166200050523     c                   parm                    kpjba
166300930420     C*
166400060406     c* controllo riempimento skiere
166500060406     c                   clear                   trul0sds
166600060406     c                   eval      i0sski='PVD/PVO/PVC'
166700060406     c                   eval      i0sele=%elem(PVD)
166800060406     c                   eval      i0spie=ie
166900060406     c                   eval      i0sfile='FNEVB01L  '
167000060406     c                   eval      i0scod='     '
167100060406     c                   eval      i0spgm='TNSD99R'
167200060406     c                   movel     trul0sds      kpjbu
167300060406     c                   call      'TRUL0SR'
167400060406     c                   parm                    kpjba
167500060406     C*
167600930420     C                   ENDSR
167700931213     C********************************************************************
167800931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
167900931213     C********************************************************************
168000931213     C     CARTUC        BEGSR
168100931213     C*
168200931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
168300931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
168400931214     C     D98GC2        ANDEQ     *BLANKS
168500931213     C                   ELSE                                                   <-- 1 -->
168600931213     C*
168700931213     C                   MOVE      D98GC1        TUC(1)
168800931214     C                   MOVE      D98GC2        TUC(2)
168900931213     C*
169000931213     C                   END                                                    <-- 1 ---
169100931213     C*
169200931213     C                   ENDSR
169300970304     C********************************************************************
169400970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
169500970304     C********************************************************************
169600970304     C     DATRIF        BEGSR
169700970304     C*
169800050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
169900050922     c*  partenza
170000970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
170100970304     C*
170200970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
170300070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
170400070605     C***                MOVEL     '1'           D98ERR
170500070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
170600070605     C***                ELSE                                                   <-- 2 -->
170700970304     C*
170800970304     C                   Z-ADD     D98DRF        WD1
170900970304     C                   Z-ADD     D98ORF        WO1
171000970310     C* DATA PARTENZA ULTIMO COLLO
171100970310     C     D98DRF        IFLE      D98DUC
171200050928     c     d98duc        oreq      0
171300970310     C                   Z-ADD     D98DRF        D98DUC
171400970310     C                   Z-ADD     D98ORF        W98HUC
171500070605     C                   ENDIF
171600970304     C* DATA ARRIVO TRAZIONE
171700050922     C***                Z-ADD     D98DTI        WD2
171800050922     C***                Z-ADD     D98HTI        WO2
171900050922     C***  WDO1          IFLE      WDO2
172000050922     C***                Z-ADD     D98DRF        D98DTI
172100050922     C***                Z-ADD     D98ORF        D98HTI
172200050922     C***                ENDIF
172300970304     C* DATA CONSEGNA
172400050922     C***                Z-ADD     D98DCM        WD2
172500050922     C***                Z-ADD     D98HMC        WO2
172600050922     C***  WDO1          IFLE      WDO2
172700050922     C***                Z-ADD     D98DRF        D98DCM
172800050922     C***                Z-ADD     D98ORF        D98HMC
172900050922     C***                ENDIF
173000970304     C*
173100070605     C***                ENDIF                                                  <-- 2 ---
173200970304     C                   ENDIF                                                  <-- 1 ---
173300970304     C*
173400970304     C                   ENDSR
173500940901     C********************************************************************
173600940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
173700940901     C********************************************************************
173800940901     C     TDTIPO        BEGSR
173900940901     C*
174000940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
174100940902     C                   Z-ADD     D98TTI        W98TTI            3 0
174200941011     C*
174300941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
174400941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
174500941011     C     W98TTI        DIV       24            WDIV              3 0
174600941011     C                   MVR                     WRES              3 0
174700941011     C     WRES          IFEQ      *ZEROS
174800941011     C                   Z-ADD     *ZEROS        WASTA             9 0
174900941011     C                   ELSE
175000941011     C                   Z-ADD     12            WASTA
175100941011     C                   END
175200940901     C*
175300940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
175400940901     C     D98DTI        IFNE      *ZEROS
175500940901     C                   Z-ADD     D98DTI        WDAT
175600940901     C                   EXSR      DATORE
175700960911     C                   Z-ADD     WORE          WPODTI
175800960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
175900960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
176000960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
176100960911     C                   Z-ADD     WPODTI        W10POI            9 0
176200960911     C                   EXSR      R10FES
176300960911     C                   Z-ADD     W10POO        WPODTI
176400960911     C* AGGIUNGO LE ORA D'ASTA
176500960911     C                   ADD       WASTA         WPODTI
176600960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
176700960911     C     W10POI        IFEQ      W10POO
176800940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
176900940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
177000940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
177100940901     C                   EXSR      RO3ORE
177200940901     C                   ADD       WR3HHO        WPODTI
177300940901     C                   ADD       WR3HHO        W98TTI
177400940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
177500940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
177600940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
177700940901     C                   EXSR      RO3ORE
177800940901     C                   ADD       WR3HHO        WPODTI
177900940901     C                   ADD       WR3HHO        W98TTI
178000960911     C                   ENDIF                                                  *SE NON FESTIVO
178100940901     C                   END
178200940901     C*
178300940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
178400940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
178500940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
178600940901     C     W98TTI        DIV       24            WDIV              3 0
178700940901     C                   MVR                     WRES              3 0
178800940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
178900941228     C                   Z-ADD     W990IN        D98TCI
179000160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
179100160405     c*  per cui le sottraggo, per riagguingerle alla fine
179200160405     c                   if        wVP='S'
179300160405     c                   sub       24            D98TCI
179400160405     c                   endif
179500160405     c
179600150630     c                   eval      wTSogliaE='0'
179700940901     C                   ELSE                                                   <-- 2 -->
179800941228     C                   Z-ADD     W991IN        D98TCI
179900160405     c                   if        wVP='S'
180000160405     c                   sub       24            D98TCI
180100160405     c                   endif
180200150630     c                   eval      wTSogliaE='1'
180300940908     C                   ENDIF                                                  <-- 2 ---
180400090827     c* Sistemo i tempi di consegna se non sono multipli di 12
180500090827     c                   exsr      SisCONS
180600090827     c*
180700940908     C* RICALCOLO TEMPO DI RESA INTERNO
180800950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
180900950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
181000090921     c                   z-add     d98tri        w99tri
181100090921     c* per la DS
181200090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
181300090921     c* per i calcoli interni
181400090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
181500940908     C                   ENDIF                                                  <-- 1 ---
181600940901     C*
181700940901     C                   ENDSR
181800930325     C********************************************************************
181900930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
182000930325     C********************************************************************
182100930325     C     TDATPO        BEGSR
182200930406     C*
182300930420     C* TRASFORMO LA DATA ODIERNA
182400930416     C                   Z-ADD     WDAODI        WDAT
182500930416     C                   EXSR      DATORE
182600050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
182700960911     C                   MOVEL     'OSC  '       WR3TAB
182800960911     C                   Z-ADD     WHHODI        WR3HHI
182900960911     C                   EXSR      RO3ORE
183000050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
183100050922     c                   if        wr3hho=12
183200050922     c     wpoatt        add       12            wpoattdata24
183300050922     c                   else
183400050922     c                   z-add     wpoatt        wpoattdata24
183500050922     c                   endif
183600930325     C*
183700930420     C* TRASFORMO LA DATA DI SPEDIZIONE
183800931125     C     D98DSP        IFEQ      *ZEROS
183900930326     C                   MOVEL     '1'           D98ERR
184000930326     C                   MOVEL     MSG(2)        D98MSG
184100930326     C                   ELSE
184200931125     C                   Z-ADD     D98DSP        WDAT
184300930325     C                   EXSR      DATORE
184400930406     C                   Z-ADD     WORE          WPODSP
184500930326     C                   END
184600930414     C*
184700930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
184800930414     C                   MOVEL     'LIM  '       WR3TAB
184900930414     C                   Z-ADD     *ZEROS        WR3HHI
185000930414     C                   EXSR      RO3ORE
185100931222     C     WR3HHO        MULT      10            WTM060            6 0
185200931222     C     WPODSP        ADD       WTM060        WPOLIM
185300140822     c
185400930325     C*
185500930420     C* TRASFORMO LA DATA DI CONSEGNA
185600931125     C     D98DCM        IFNE      *ZEROS
185700931125     C                   Z-ADD     D98DCM        WDAT
185800930325     C                   EXSR      DATORE
185900930326     C                   Z-ADD     WORE          WPODCM
186000930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
186100930326     C                   MOVEL     'OSC  '       WR3TAB
186200931125     C                   Z-ADD     D98HMC        WR3HHI
186300930326     C                   EXSR      RO3ORE
186400930326     C                   ADD       WR3HHO        WPODCM
186500930326     C                   END
186600941228     C*
186700941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
186800941228     C     D98DUC        IFNE      *ZEROS
186900941228     C                   Z-ADD     D98DUC        WDAT
187000941228     C                   EXSR      DATORE
187100941228     C                   Z-ADD     WORE          WPODUC
187200970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
187300970310     C                   MOVEL     'OSP  '       WR3TAB
187400970310     C                   Z-ADD     W98HUC        WR3HHI
187500970310     C                   EXSR      RO3ORE
187600970310     C     D98DRF        IFNE      *ZEROS
187700050928     c     w98huc        andgt     1200
187800970310     C                   SUB       12            WR3HHO
187900970310     C                   ENDIF
188000970310     C                   ADD       WR3HHO        WPODUC
188100941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
188200070605     C***  WPODSP        IFGT      WPODUC
188300070605     C***                Z-ADD     WPODSP        WPODUC
188400070605     C***                ENDIF
188500941228     C                   ENDIF
188600960402     C*
188700141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
188800970304     C     D98DBR        IFNE      *ZEROS
188900970304     C                   Z-ADD     D98DBR        WDAT
189000960402     C                   EXSR      DATORE
189100960402     C                   Z-ADD     WORE          WPODBR
189200070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
189300960402     C                   ELSE
189400960402     C                   Z-ADD     WPODSP        WPODBR
189500960402     C                   ENDIF
189600930913     C*
189700960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
189800930913     C* CONSEGNABILITA'
189900960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
190000950407     C*
190100930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
190200931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
190300931125     C                   Z-ADD     D98DIC        WDAT
190400930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
190500930913     C                   Z-ADD     WORE          WPODIC
190600930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
190700931125     C                   Z-ADD     D98OIC        WR3HHI
190800930913     C                   EXSR      RO3ORE
190900930913     C                   ADD       WR3HHO        WPODIC
191000931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
191100950407     C*
191200930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
191300931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
191400931125     C                   Z-ADD     D98DFC        WDAT
191500930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
191600930913     C                   Z-ADD     WORE          WPODFC
191700140822     C                   Z-ADD     WORE          WPODFC_00
191800930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
191900931125     C                   Z-ADD     D98OFC        WR3HHI
192000930913     C                   EXSR      RO3ORE
192100930913     C                   ADD       WR3HHO        WPODFC
192200931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
192300930913     C*
192400930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
192500930913     C     D98ERR        IFEQ      '0'
192600930913     C     WPODFC        IFEQ      *ZEROS
192700930913     C     WPODIC        ANDNE     *ZEROS
192800930913     C                   MOVEL     '1'           D98ERR
192900930913     C                   MOVEL     MSG(17)       D98MSG
193000930913     C                   ENDIF
193100930913     C                   ENDIF
193200930907     C*
193300930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
193400930913     C     D98ERR        IFEQ      '0'
193500930913     C     WPODIC        IFGT      WPODFC
193600930907     C                   MOVEL     '1'           D98ERR
193700930913     C                   MOVEL     MSG(18)       D98MSG
193800930913     C                   ENDIF
193900930913     C                   ENDIF
194000950407     C*
194100950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
194200930511     C*
194300930511     C                   ENDSR
194400950110     C********************************************************************
194500950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
194600950110     C********************************************************************
194700950110     C     CALDPP        BEGSR
194800950110     C*
194900950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
195000950110     C                   MOVEL     'P'           WTC                            *PARTENZA
195100950110     C                   MOVEL     'I'           WIC                            *INTERNA
195200960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
195300950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
195400950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
195500950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
195600950110     C*
195700950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
195800950110     C                   MOVEL     'P'           WTC                            *PARTENZA
195900950110     C                   MOVEL     'C'           WIC                            *CLIENTE
196000950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
196100950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
196200950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
196300950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
196400960229     C*
196500960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
196600050922     c
196700050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
196800050922     c
196900050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
197000090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
197100050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
197200090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
197300050922%%%  C*  ritardo alla partenza.
197400050922     c* Il tutto solo se la data di oggi > della
197500050922     c*  data teorica di partenza determinata
197600050922     c*
197700950110%%%  C     D98ERR        IFEQ      '0'
197800950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
197900950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
198000950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
198100050922     c     wpoattData24  andgt     wpoppi
198200960911%%%  C********             MOVEL'OSC  '   WR3TAB
198300960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
198400960911%%%  C********             EXSR RO3ORE
198500960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
198600050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
198700090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
198800090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
198900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
199000050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
199100050922     c* La nuova data calcolata non può essere < della minima prevista
199200050922     c                   if        wr0poo>wpoppi
199300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
199400050922     c                   endif
199500050922     c
199600950110%%%  C                   ENDIF
199700950110%%%  C                   ENDIF
199800950110%%%  C*
199900950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
200000050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
200100050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
200200050915%%%  C*  alla prtenza.
200300950110%%%  C     D98ERR        IFEQ      '0'
200400950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
200500950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
200600950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
200700950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
200800950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
200900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
201000050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
201100950110%%%  C                   ENDIF
201200950110%%%  C                   ENDIF
201300950110%%%  C*
201400950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
201500950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
201600950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
201700950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
201800950110%%%  C     D98ERR        IFEQ      '0'
201900950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
202000950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
202100950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
202200950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
202300090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
202400090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
202500950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
202600050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
202700950110%%%  C                   ENDIF
202800950110%%%  C                   ENDIF
202900960229     C*
203000960229     C                   ENDIF                                                  <-- 1 ---
203100950110     C*
203200950110     C                   ENDSR
203300930604     C********************************************************************
203400930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
203500930604     C********************************************************************
203600930604     C     CALDPT        BEGSR
203700090902     c                   clear                   RicalcoloTRAZ     1
203800930604     C*
203900930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
204000931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
204100931129     C                   MOVEL     'I'           WIC                            *INTERNA
204200050915     c* se non presente la data partenza ultimo collo, uso la teorica
204300950110     C     D98DUC        IFEQ      *ZEROS
204400950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
204500950110     C                   ELSE
204600950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
204700950110     C                   ENDIF
204800930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
204900930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
205000930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
205100930604     C*
205200930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
205300931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
205400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
205500950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
205600930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
205700930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
205800930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
205900150508
206000150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
206100150508     c* se presente la data partenza PRIMO  collo --> la uso
206200150508     c                   if        w98dpc=0
206300150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
206400150508     c                   else
206500150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
206600150508     C                   MOVEL     'C'           WIC                            *INTERNA
206700150508     C                   Z-ADD     W98DPC        WDAT
206800150508     C                   EXSR      DATORE
206900150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
207000150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
207100150508     C                   EXSR      RO4CDP
207200150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
207300150508     c                   endif
207400960229     C*
207500960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
207600940624%%%  C*
207700050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
207800050915
207900940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
208000050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
208100050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
208200050915     c*   attribuito alla trazione
208300940624%%%  C     D98ERR        IFEQ      '0'
208400940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
208500940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
208600090903     c     wpoatt        andge     wpopti
208700050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
208800090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
208900090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
209000940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
209100050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
209200050922     c                   if        wr0poo>wpopti
209300050922     c                   eval      wpopti=wr0poo
209400050922%%%  C                   endif
209500090902     c*
209600090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
209700090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
209800090902     c*  della trazione udate e utime
209900090902     c                   eval      RicalcoloTRAZ='S'
210000090902     c
210100940624%%%  C                   ENDIF
210200940624%%%  C                   ENDIF
210300940628%%%  C*
210400050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
210500050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
210600050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
210700940628%%%  C     D98ERR        IFEQ      '0'
210800940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
210900940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
211000940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
211100090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
211200090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
211300940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
211400050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
211500940628%%%  C                   ENDIF
211600940628%%%  C                   ENDIF
211700960229     C*
211800960229     C                   ENDIF                                                  <-- 1 ---
211900930604     C*
212000930604     C                   ENDSR
212100930511     C********************************************************************
212200930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
212300930511     C********************************************************************
212400930511     C     CALDCR        BEGSR
212500050914     c                   clear                   consideraora
212600100308     c
212700100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
212800100308     c*
212900100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
213000100308     c* 1) se presente solo ORA --> ignoro sempre
213100100308     c* 2) se presente data e ora IL --> ignoro l'ora
213200100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
213300100309     c***                if        d98tsp='E'
213400100309     c***                clear                   d98dcr
213500100309     c***                clear                   d98hcr
213600100309     c**+                clear                   d98tcr
213700100309     c***                endif
213800100308     c
213900100308     c                   if        d98hcr>0
214000100308     c
214100100308     c                   if        d98dcr=0
214200100308     c                   clear                   d98hcr
214300100308     c                   endif
214400100308
214500100308     c                   if        d98dcr>0 and d98tcr=' '
214600100308     c                   clear                   d98hcr
214700100308     c                   endif
214800100308     c                   endif
214900941228     C*
215000941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
215100941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
215200050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
215300050901     c*  (per uguale si intende entrambe nel mattino o entrambe
215400050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
215500050901     c*  (EDPES 1/9/05)
215600050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
215700050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
215800050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
215900050831     C***                Z-ADD     D98DCM        D98DCR
216000050831     C***                ENDIF
216100050901     c*
216200070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
216300050914     c
216400100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
216500100308     c***                clear                   d98hcr
216600100308     c***                clear                   d98tcr
216700100308     c***                endif
216800050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
216900050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
217000070521     c*  richiesta dal cliente
217100100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
217200100308     C***                MOVEL     'OSC  '       WR3TAB
217300100308     C***                Z-ADD     D98HMC        WR3HHI
217400100308     C***                EXSR      RO3ORE
217500100308     c***                z-add     wr3hho        salvaoreHMC
217600050901     c
217700100308     C***                Z-ADD     D98Hcr        WR3HHI
217800100308     C***                EXSR      RO3ORE
217900100308     c***                z-add     wr3hho        salvaoreHCR
218000100308     c***
218100050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
218200100308    2c***                if        salvaoreHMC=salvaoreHCR
218300100308     C***                eval      Consideraora='S'
218400100308     c***                else
218500100308     C***                eval      Consideraora=' '
218600100308     c***                clear                   d98hcr
218700100308    2c***                endif
218800100308    1c***                endif
218900930511     C*
219000930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
219100931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
219200931125     C                   Z-ADD     D98DCR        WDAT
219300930325     C                   EXSR      DATORE
219400930326     C                   Z-ADD     WORE          WPODCR
219500930420     C*
219600930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
219700930420     C*
219800931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
219900930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
220000930326     C                   MOVEL     'CRD  '       WR3TAB
220100931125     C                   Z-ADD     D98HCR        WR3HHI
220200930419     C                   EXSR      RO3ORE
220300930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
220400940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
220500050902     c
220600931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
220700930420     C                   MOVEL     'CRD1 '       WR3TAB
220800930420     C                   Z-ADD     *ZEROS        WR3HHI
220900930420     C                   EXSR      RO3ORE
221000931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
221100090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
221200090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
221300050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
221400060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
221500050907     c                   z-add     worecr        oreILoDOPO
221600931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
221700931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
221800931207     C                   MOVEL     'C'           WIC                            *CLIENTE
221900931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
222000150330     C                   Z-ADD     WORECR        WR4NOI
222100931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
222200931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
222300931209     C     WPOCRF        SUB       12            WR7POI            9 0
222400931209     C                   MOVEL     'X'           WIC
222500931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
222600931209     C                   ADD       12            WR7POO
222700931207     C     WR7POO        IFGT      WPOCRF
222800931207     C                   Z-ADD     WR7POO        WPOCRF
222900931207     C                   END
223000150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
223100150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
223200150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
223300150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
223400150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
223500150330     C                   MOVEL     'C'           WIC                            *CLIENTE
223600150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
223700150330     C                   Z-ADD     w99TCC        WR4NOI
223800150330     c                   sub       12            wr4noi
223900150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
224000150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
224100150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
224200150330     c
224300150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
224400150330     C                   MOVEL     'X'           WIC
224500150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
224600150330     C******             ADD       12            WR7POO
224700150330     C     WR7POO        IFGT      WPOCRF_dee
224800150330     C                   Z-ADD     WR7POO        WPOCRF_dee
224900150330     C                   END
225000050902     c
225100050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
225200050902     c*  sono le ore di fine della non consegnabilità
225300050905     c* Se aggiungendo 1 finisco in giorno festivo
225400050905     c*  cerco primo giorno non festivo
225500050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
225600050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
225700050907     C****               MOVEL     'C'           WIC                            *CLIENTE
225800050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
225900050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
226000050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
226100050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
226200050907     c
226300050907     c                   add       1             wpocri
226400050902     c
226500050902   FFC                   Z-ADD     WPOCRI        WPOCRC
226600050902     c
226700930325     C                   END
226800150330     C*
226900931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
227000930420     C* DATA SPED. = DATA INIZIO RANGE
227100930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
227200940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
227300940324£$   C                   Z-ADD     WPOCRI        WPOCRC
227400930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
227500930420     C                   MOVEL     'CRP  '       WR3TAB
227600931125     C                   Z-ADD     D98HCR        WR3HHI
227700930420     C                   EXSR      RO3ORE
227800931207     C     WPODCR        ADD       WR3HHO        WPOCRF
227900931207     C                   SUB       24            WPOCRF                         *FINE RANGE
228000931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
228100931207     C* ANDANDO INDIETRO NEL TEMPO
228200050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
228300050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
228400050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
228500050907     c* data utile venedì sera
228600130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
228700130411     C                   MOVEL     'X'           WIC                            *CLIENTE
228800931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
228900931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
229000931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
229100930326     C                   END
229200930526     C*
229300931125     C     D98TCR        IFEQ      'P'
229400931125     C     D98TCR        OREQ      'D'
229500930526     C                   ELSE                                                   *CONS.RICH.IL ..
229600940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
229700940324£$   C                   Z-ADD     WPODCR        WPOCRC
229800930526     C* DATA SPED. = DATA INIZIO RANGE
229900050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
230000050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
230100050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
230200930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
230300930526     C                   MOVEL     'CRI  '       WR3TAB
230400931125     C                   Z-ADD     D98HCR        WR3HHI
230500930526     C                   EXSR      RO3ORE
230600050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
230700050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
230800050907     c                   z-add     wr3hho        oreILoDOPO
230900930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
231000050902     c
231100050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
231200050902     c                   if        wr3MATPOM='P' and d98hcr>0
231300050902     c                   add       12            wpocri
231400100308     c* tolgo 12 ore dalle ore conseganbili
231500050907     c                   sub       12            oreILoDOPO
231600050902     c                   endif
231700050902     c
231800931209     C     WPOCRF        SUB       12            WR7POI            9 0
231900931209     C                   MOVEL     'X'           WIC
232000931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
232100931209     C                   ADD       12            WR7POO
232200931209     C     WR7POO        IFGT      WPOCRF
232300931209     C                   Z-ADD     WR7POO        WPOCRF
232400931209     C                   END
232500050905     c
232600050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
232700050905     c*  sono le ore di fine della non consegnabilità
232800050905     c* Se aggiungendo 1 finisco in giorno festivo
232900050905     c*  cerco primo giorno non festivo
233000050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
233100050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
233200050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
233300050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
233400050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
233500050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
233600050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
233700050907     c
233800050907     c                   add       1             wpocri
233900050905     c
234000050905   FFC                   Z-ADD     WPOCRI        WPOCRC
234100050905     c
234200930526     C                   END
234300931206     C*
234400931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
234500931206     C* LA IMPOSTO COME LA CONS.TEORICA
234600140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
234700140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
234800931206     C                   ELSE
234900931206     C                   Z-ADD     WPOCRF        WPOCR1
235000931206     C                   END
235100150608     c* ai fini del calcolo affidabilità
235200150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
235300150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
235400931206     C                   ELSE
235500931206     C                   Z-ADD     WPOCRF        WPOCR2
235600931206     C                   END
235700150608     c* ai fini della data prevista consegna cliente
235800150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
235900150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
236000150608     C                   ELSE
236100150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
236200150608     C                   END
236300150331     c
236400150331     c                   if        wpocrf_dee>0
236500150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
236600150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
236700150330     C                   END
236800150331     C                   END
236900150331     C*
237000150331     c                   if        wpocrf_dee=0
237100150608     C     wpocr2_dee    sub       24            WPOCRF_dee
237200150331     c                   endif
237300150331     c
237400930526     C                   END                                                    <-- 1 ---
237500930511     C*
237600930511     C                   ENDSR
237700930511     C********************************************************************
237800930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
237900930511     C********************************************************************
238000930511     C     CALLAV        BEGSR
238100930511     C*
238200941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
238300930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
238400930421     C*
238500930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
238600930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
238700930421     C     1             DO        IE            E                 3 0          --- 2 -->
238800930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
238900930421     C                   Z-ADD     1             I
239000930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
239100060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
239200930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
239300930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
239400930421     C                   ADD       1             IL
239500930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
239600930421     C                   EXSR      DATORE
239700930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
239800091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
239900091014     c* Mi salvo la posizione dell'evento
240000091014     C                   Z-ADD     E             LAp(IL)
240100930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
240200930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
240300930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
240400930421     C                   EXSR      RO3ORE
240500930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
240600930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
240700930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
240800930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
240900091015     c**                 if        §2agla>1
241000091015     c**                 sub       1             §2agla
241100930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
241200091015     c**                 else
241300091015     c**                 clear                   whh
241400091015     c**                 endif
241500930510     C                   ADD       WHH           WORELA
241600940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
241700930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
241800090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
241900150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
242000930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
242100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
242200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
242300930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
242400930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
242500930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
242600930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
242700931209     C                   Z-ADD     LAF(IL)       WR7POI
242800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
242900931202     C     WR7POO        IFGT      LAF(IL)
243000931201     C                   Z-ADD     WR7POO        LAF(IL)
243100931202     C                   END
243200930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
243300930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
243400091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
243500930510     C                   END                                                    <-- 3 ---
243600931223     C                   ENDDO                                                  <-- 2 ---
243700930421     C*
243800930421     C                   END                                                    <-- 1 ---
243900930618££$$ C*
244000930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
244100931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
244200941228££$$ C     1             DO        IL            I
244300930618££$$ C     LAF(I)        IFGT      WPOLAF
244400930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
244500930618££$$ C                   END
244600931223££$$ C                   ENDDO
244700930511     C*
244800930511     C                   ENDSR
244900940324£$   C********************************************************************
245000940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
245100940324£$   C********************************************************************
245200940324£$   C     CALLAC        BEGSR
245300941229     C*
245400950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
245500950407     C*
245600950210     C                   Z-ADD     WORELA        WTM060
245700941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
245800941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
245900941229     C     D98DAG        IFEQ      *ZEROS
246000050520     c     §2aftg        andeq     'S'
246100090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
246200090827     C     WORELA        ADD       w99TCC        WTM060            6 0
246300160404     c                   if        wVP='S'
246400160404     c                   sub       24            wTM060
246500160404     c                   endif
246600941229     C                   ENDIF
246700941229     C*
246800940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
246900940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
247000941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
247100941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
247200941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
247300940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
247400941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
247500050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
247600050520     c                   movel     §2aftg        LCON(IL)
247700950407     C*
247800950407     C                   ENDIF
247900960129     C*
248000940324£$   C                   ENDSR
248100930511     C********************************************************************
248200930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
248300930511     C********************************************************************
248400930511     C     CALGIA        BEGSR
248500960129     C*
248600960129     C*-------------------------------------------------------------------
248700960129     C* EVENTI GIACENZA
248800960129     C*-------------------------------------------------------------------
248900960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
249000960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
249100960129     C*
249200050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
249300041116     c     d98nsp        ifgt      0
249400031125     C                   EXSR      CARGIAC
249500041116     c                   endif
249600031125     C*
249700960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
249800960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
249900960129     C     1             DO        IE            E                 3 0          --- 2 -->
250000960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
250100960129     C                   Z-ADD     1             I
250200960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
250300060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
250400960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
250500960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
250600960129     C                   ADD       1             IG
250700960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
250800960129     C                   EXSR      DATORE
250900960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
251000140606     c* Mi salvo la posizione dell'evento
251100140606     C                   Z-ADD     E             gip(IG)
251200960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
251300960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
251400960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
251500960129     C                   EXSR      RO3ORE
251600960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
251700960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
251800960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
251900960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
252000960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
252100960129     C                   ADD       WHH           WOREGI
252200960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
252300960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
252400090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
252500090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
252600160404     c                   if        wVP='S'
252700160404     c                   sub       24            wOREGI
252800160404     c                   endif
252900960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
253000960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
253100960129     C                   MOVEL     'C'           WIC                            *CLIENTE
253200960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
253300960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
253400960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
253500960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
253600960129     C                   Z-ADD     GIF(IG)       WR7POI
253700960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
253800031125     C     WR7POO        IFGT      GIF(IG)
253900031125     C                   Z-ADD     WR7POO        GIF(IG)
254000031125     C                   ENDIF
254100031201     C*
254200031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
254300031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
254400031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
254500031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
254600031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
254700031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
254800031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
254900031201     C                   IF        WPOATT > GIF(IG)
255000031201     C*
255100031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
255200031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
255300031201     C                   IF        WGGA = *BLANKS
255400031201     C                   SETOFF                                       54
255500031201     C                   ELSE
255600031201     C     WGGA          LOOKUP    SK7QW                                  54
255700031201     C                   ENDIF
255800031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
255900031201     C                   MOVEL     '1'           FLGGIACOK
256000031201     C                   ELSE
256100140127     c
256200031125     C*
256300031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
256400031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
256500031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
256600031125     C                   MOVEL     '0'           FLGGIACOK         1
256700031125     C*
256800031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
256900031125     C     1             DO        IAG           EG                3 0
257000031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
257100031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
257200031125     C                   EXSR      DATORE
257300031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
257400031125     C*
257500031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
257600031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
257700031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
257800031125     C                   EXSR      DATORE
257900031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
258000031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
258100031229     C* => NN LO CONSIDERO
258200031229     C                   IF        EVC(E+1) = 'G' AND
258300031229     C                             WPROXGII = WGCPDORE
258400031229     C                   MOVEL     '1'           FLGGIACOK
258500031229     C                   ENDIF
258600031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
258700031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
258800031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
258900031125     C                   EXSR      RO3ORE
259000031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
259100031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
259200031125     C* DELL'EVENTO GIACENZA CORRENTE
259300031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
259400060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
259500031125     C                   MOVEL     '1'           FLGGIACOK
259600031125     C                   LEAVE
259700031125     C                   ENDIF
259800031125     C                   ELSE
259900031229     C                   IF        EVC(E)  = 'G' AND
260000031229     C                             GII(IG) = WGCPDORE
260100031229     C                   MOVEL     '1'           FLGGIACOK
260200031229     C                   LEAVE
260300031229     C                   ENDIF
260400031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
260500031125     C                   MOVEL     '1'           FLGGIACOK
260600031125     C                   LEAVE
260700031125     C                   ENDIF
260800031125     C                   ENDIF
260900031125     C                   ENDDO
261000031201     C                   ENDIF
261100140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
261200140127     C                   IF        FLGGIACOK = '0'  and
261300140127     c                             d98dcm=evd(E)
261400140127     C                   MOVEL     '1'           FLGGIACOK
261500140127     c                   endif
261600031125     C*
261700031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
261800031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
261900031125     C* DELL'EVENTO GIACENZA CORRENTE
262000031125     C                   IF        FLGGIACOK = '0'
262100031125     C                   Z-ADD     *ZEROS        GIF(IG)
262200031125     C                   ELSE
262300031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
262400031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
262500031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
262600031125     C                   ENDIF
262700031125     C                   ELSE
262800031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
262900031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
263000031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
263100031125     C                   ENDIF
263200031125     C*
263300960129     C                   ENDIF                                                  <-- 3 ---
263400960129     C                   ENDDO                                                  <-- 2 ---
263500960129     C*
263600960129     C                   ENDIF                                                  <-- 1 ---
263700960129££$$ C*
263800960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
263900960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
264000960129££$$ C     1             DO        IG            I
264100960129££$$ C     GIF(I)        IFGT      WPOGIF
264200960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
264300960129££$$ C                   ENDIF
264400960129££$$ C                   ENDDO
264500941229     C*
264600941229     C*-------------------------------------------------------------------
264700941229     C* APERTURA GIACENZA
264800941229     C*-------------------------------------------------------------------
264900960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
265000960129     C*
265100960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
265200931125     C                   Z-ADD     D98DAG        WDAT
265300930325     C                   EXSR      DATORE
265400950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
265500960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
265600950413£££  C                   Z-ADD     WORE          WPODAG
265700960129$$£  C*********************ENDIF
265800930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
265900930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
266000930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
266100930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
266200930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
266300941229     C*
266400941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
266500941229     C                   Z-ADD     WPODAG        WPOAGC
266600960129     C*
266700960129     C                   ENDIF                                                  <-- 1 ---
266800941229     C*
266900941229     C*-------------------------------------------------------------------
267000930511     C* CHIUSURA GIACENZA
267100941229     C*-------------------------------------------------------------------
267200930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
267300930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
267400931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
267500931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
267600931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
267700931201     C                   Z-ADD     D98DCM        W98DCG
267800930416     C                   ELSE
267900930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
268000930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
268100931201     C                   Z-ADD     WDAODI        W98DCG
268200061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
268300061123     c*  chiusura giacenza, come già fa in altri posti
268400061123     c                   if        d98dfc>0
268500061123     C                   Z-ADD     d98dfc        W98DCG
268600061123     c                   endif
268700061123     c
268800930409     C                   END
268900930409     C                   END
269000930409     C                   END
269100930414     C* TRASF.DATA CHIUSURA
269200931201     C     W98DCG        IFNE      *ZEROS
269300931201     C                   Z-ADD     W98DCG        WDAT
269400930325     C                   EXSR      DATORE
269500930326     C                   Z-ADD     WORE          WPODCG
269600930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
269700930407     C* GIORNO DA ESCLUDERE
269800150216     C                   ADD       12            WPODCG
269900150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
270000150123     c*  il giorno di chiusura dovrebe già essere protetto
270100941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
270200941229     C     WPOCGC        IFEQ      *ZEROS
270300941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
270400941229     C                   ENDIF
270500930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
270600930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
270700931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
270800931129     C                   MOVEL     'C'           WIC                            *CLIENTE
270900150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
271000150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
271100160404     c                   if        wVP='S'
271200160404     c                   sub       24            wr4NOI
271300160404     c                   endif
271400930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
271500930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
271600930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
271700931209     C                   Z-ADD     WPODCG        WR7POI
271800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
271900931202     C     WR7POO        IFGT      WPODCG
272000931201     C                   Z-ADD     WR7POO        WPODCG
272100930326     C                   END
272200931202     C                   END
272300930511     C*
272400930511     C                   ENDSR
272500031125     C********************************************************************
272600031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
272700031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
272800031125     C*             DEGLI EVENTI DI TIPO GIACENZA
272900031125     C********************************************************************
273000031125     C     CARGIAC       BEGSR
273100031125     C*
273200031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
273300031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
273400031125     C*
273500050405     C                   Z-ADD     D98AAS        KWGCPAAS
273600050405     C                   Z-ADD     D98LNP        KWGCPLNP
273700050405     C                   Z-ADD     D98NRS        KWGCPNRS
273800050405     C                   Z-ADD     D98NSP        KWGCPNSP
273900031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
274000050405     C     KEYGC_P       SETLL     TIGCP01L
274100050405     C                   IF        %equal(TIGCP01L)
274200050405     C     KEYGC_P       READE     TIGCP01L
274300050405     C                   DOW       not %eof(TIGCP01L)
274400031125     C                   ADD       1             IAG
274500031125     C                   EVAL      GCPD(IAG) = GCPDUR
274600050405     C     KEYGC_P       READE     TIGCP01L
274700031125     C                   ENDDO
274800031125     C                   ENDIF
274900031125     C                   ELSE
275000050405     C     KEYGC_P       SETLL     TIGCP21L
275100050405     C                   IF        %equal(TIGCP21L)
275200050405     C     KEYGC_P       READE     TIGCP21L
275300050405     C                   DOW       not %eof(TIGCP21L)
275400031125     C                   ADD       1             IAG
275500031125     C                   EVAL      GCPD(IAG) = GCPDUR
275600050405     C     KEYGC_P       READE     TIGCP21L
275700031125     C                   ENDDO
275800031125     C                   ENDIF
275900031125     C                   ENDIF
276000031125     C*
276100031125     C                   ENDSR
276200941229     C********************************************************************
276300960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
276400941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
276500941229     C********************************************************************
276600960129     C     CALGIC        BEGSR
276700941229     C*
276800960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
276900960129     C*
277000960129     C                   Z-ADD     WOREGI        WTM060
277100960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
277200960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
277300960129     C     D98DAG        IFEQ      *ZEROS
277400960129$$£  C     WOREGI        ADD       12            WTM060            6 0
277500960129     C                   ENDIF
277600960129     C*
277700960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
277800960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
277900960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
278000960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
278100960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
278200960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
278300960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
278400960129     C*
278500960129     C                   ENDIF
278600941229     C*
278700941229     C                   ENDSR
278800060418     C********************************************************************
278900060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
279000060418     c*            l'affidabilità
279100060418     C********************************************************************
279200060418     C     CALCEPA       BEGSR
279300060418     C*
279400060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
279500060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
279600060420     c                   clear                   numggcepa         3 0
279700060418     C*
279800060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
279900060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
280000060420     c                   eval      wgiroepa='0'
280100060418     c                   eval      wfine=' '
280200060418     c
280300060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
280400060420    2C     W             DOwle     IP                                           --- 2 -->
280500060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
280600060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
280700060418     c
280800060420    4c                   if        wgiroepa='0'
280900060420     c                   exsr      ElaInizCEPA
281000060418   x4c                   else
281100060420    5c                   if        pvd(W)>wdat
281200060418     c                   eval      wfine='S'
281300060418    5c                   endif
281400060418    4c                   endif
281500060418     c
281600060420    4c                   if        wfine=' ' and pvd(W)=wdat
281700060420     C*
281800060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
281900060420     c*  finale
282000060420     c                   add       1             numggcepa
282100060420     c
282200060420     C                   z-add     24            WORELA
282300060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
282400060418     c
282500060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
282600060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
282700060418     C                   MOVEL     'C'           WIC                            *CLIENTE
282800060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
282900060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
283000060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
283100060418     c
283200060418     c* Trasformo le ore in data e verifico se c'e' altro evento
283300060418     c*  in sta data che allunga ancora
283400060420     c                   z-add     wr4poo        wore
283500060418     c                   EXSR      OREDAT
283600060418    4c                   endif
283700060418     c
283800060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
283900060418    4c                   if        wfine='S'
284000060418     c                   EXSR      ELAFINECEPA
284100060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
284200060420     c*  inizio range
284300060420     c                   else
284400060420     c                   add       1             w
284500060418    4C                   ENDif                                                  <-- 3 ---
284600060420     c
284700060420     c                   else
284800060420     c                   add       1             w
284900060418    3C                   ENDif                                                  <-- 3 ---
285000060418    2C                   ENDDO                                                  <-- 2 ---
285100060418     c
285200060420     c                   if        wgiroepa='1'
285300060418     c                   EXSR      ELAFINECEPA
285400060418     c                   endif
285500060418     C*
285600060418    1C                   END                                                    <-- 1 ---
285700060418££$$ C*
285800060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
285900060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
286000060420££$$ C     1             DO        Ipa           w
286100060420££$$ C     paif(w)       IFGT      WPOpaif
286200060420££$$ C                   Z-ADD     paif(w)       WPOpaif
286300060418££$$ C                   END
286400060418££$$ C                   ENDDO
286500060418     C*
286600060418     C                   ENDSR
286700060420     C********************************************************************
286800060420     c* Elabora Inizio range eventi particolari che allungano
286900060420     C********************************************************************
287000060420     c     ElaInizCEPA   BEGSR
287100060420     C                   ADD       1             IPA
287200060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
287300060420     C                   EXSR      DATORE
287400060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
287500061017     c
287600060420     c                   eval      wgiroepa='1'
287700060420     c                   eval      wdat=pvd(w)
287800060420     C                   ENDSR
287900060418     C********************************************************************
288000060418     c* Elabora fine range eventi particolari che allungano
288100060418     C********************************************************************
288200060418     c     ElafineCEPA   BEGSR
288300060420     c     numggcepa     mult      24            worela
288400060420     c                   sub       12            worela
288500090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
288600090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
288700160404     c                   if        wVP='S'
288800160404     c                   sub       24            WORELA
288900160404     c                   endif
289000060418     c
289100060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
289200060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
289300060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
289400060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
289500060418     C                   Z-ADD     paif(IPA)     WR7POI
289600060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
289700060418    5C     WR7POO        IFGT      paif(IPA)
289800060418     C                   Z-ADD     WR7POO        paif(IPA)
289900060418    5C                   END
290000061017     c*Sommo 24 ore di inizio giornata
290100061017     c*            : non ho capito perchè ma ci vogliono
290200061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
290300090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
290400090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
290500160404     c                   if        wVP='S'
290600160404     c                   sub       24            paii(IPA)
290700160404     c                   endif
290800061017     c* Se tempi di consegna inferiori alle 24 ore , devo
290900061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
291000061017     c*  partire prima
291100061017     c****               if        d98tcc<24
291200061017     c****               sub       12            paii(ipa)
291300061017     c****               endif
291400060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
291500060418     c* Imposto nuovo giro
291600060420     c                   eval      wgiroepa='0'
291700060418     c                   eval      wfine=' '
291800060420     c                   clear                   numggcepa         3 0
291900060418     c                   ENDSR
292000930511     C********************************************************************
292100930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
292200930511     C********************************************************************
292300010822     C     CALASS        BEGSR
292400010821     C*
292500011211     C                   EXSR      REPMAXCSB
292600930511     C*
292700930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
292800010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
292900011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
293000930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
293100930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
293200930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
293300930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
293400930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
293500930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
293600930511     C                   ELSE                                                   <-- 4 -->
2937009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
293800930512     C                   END                                                    <-- 4 ---
293900930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
294000930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
294100930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
294200930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
294300930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
294400930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
294500930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
294600930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
294700930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
294800930413     C                   END                                                    <-- 2 ---
294900011211     C                   END                                                    <-- 1 ---
295000930406     C*
295100930325     C                   ENDSR
295200930608     C********************************************************************
295300930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
295400930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2955009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
295600930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
295700930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
295800930608     C********************************************************************
295900930608     C     RDRASS        BEGSR
296000930608     C*
296100930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2962009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2963009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2964009407151507 C*********************EXSR RO3ORE
2965009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
296600930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2967009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2968009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2969009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2970009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2971009407191907 C                   Z-ADD     WPOASI        WPOROU
2972009407191907 C                   EXSR      CHKFES
2973009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2974009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2975009407191907 C                   ELSE                                                   *E' FERIALE
2976009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2977009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
2978009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
2979009407191907 C                   ENDIF
298000930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
298100930608     C                   MOVEL     'ASS1 '       WR3TAB
298200930608     C                   Z-ADD     *ZEROS        WR3HHI
298300930608     C                   EXSR      RO3ORE
298400930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
298500930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
298600931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
298700931129     C                   MOVEL     'C'           WIC                            *CLIENTE
298800930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
298900930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
299000930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
299100930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
299200930608     C*
299300930608     C                   ENDSR
299400930511     C********************************************************************
299500930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
299600930511     C********************************************************************
299700930511     C     CALFED        BEGSR
299800150331     c                   clear                   fddata
299900150331     c                   clear                   WPOFDdata
300000930511     C*
300100051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
300200931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
300300051108     C     D98FFD        oreq      'M'                                          --- 1 -->
300400930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
300500930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
300600930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
300700930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
300800150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
300900150331     c                   if        §ar5dfd>*zeros
301000150331     c                   movel     §ar5dfd       fddata            8 0
301100150331     c                   endif
301200150331     c                   if        fddata>0 and ((d98dti>0 and
301300150331     c                             fddata>d98dti) or d98dti=0)
301400150331     C                   Z-ADD     fddata        WDAT
301500150331     C                   EXSR      DATORE
301600150331     C                   Z-ADD     WORE          WPOfddata
301700150331     c                   endif
301800150331     c
301900930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
302000930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
302100930511     C                   ELSE                                                   <-- 2 -->
3022009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
302300930512     C                   END                                                    <-- 2 ---
302400150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3025001504081507 C                   Z-ADD     WPOfddata     WPOFDI
302600150331     c                   endif
302700150331
302800930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
302900930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
303000930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
303100930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
303200930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
303300930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
303400930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
303500930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
303600930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
303700930511     C                   END                                                    <-- 1 ---
303800930511     C*
303900930511     C                   ENDSR
304000930608     C********************************************************************
304100930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
304200930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3043009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
304400930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
304500930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
304600930608     C********************************************************************
304700930608     C     RDRFED        BEGSR
304800930608     C*
304900930608     C* CALCOLO ORE DEL FERMO DEPOSITO
305000930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3051009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3052001503311907 C                   Z-ADD     WPOFDI        WPOROU
3053001503311907 C                   EXSR      CHKFES
3054001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3055001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3056001503311907 C                   ELSE                                                   *E' FERIALE
3057001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3058001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3059001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3060001503311907 C                   ENDIF
306100150331     c* Se presente la particolarità consegna con più giorni di fermo depo
306200150331     c*  uso questi
306300150331     c                   if        §7rggfd>*zeros
306400150424     c                   movel     §7rggfd       wggfd             2 0
306500150331     c     wggfd         mult      24            Wnore
306600150331     c                   add       wnore         worefd
306700150331     c                   else
306800930608     C                   MOVEL     'FED1 '       WR3TAB
306900930608     C                   Z-ADD     *ZEROS        WR3HHI
307000930608     C                   EXSR      RO3ORE
307100940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
307200150331     c                   endif
307300930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
307400931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
307500931129     C                   MOVEL     'C'           WIC                            *CLIENTE
307600930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
307700930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
307800930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
307900930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
308000930608     C*
308100930608     C                   ENDSR
308200931201     C********************************************************************
308300931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
308400931201     C********************************************************************
308500931201     C     CALDTE        BEGSR
308600140825     c
308700140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
308800140825     c*  che tiene conto solo dei festivi per restituire in output
308900140825     c*  la data teorica di consegna
309000140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
309100140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
309200140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
309300140825     C                   ELSE
309400140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
309500140825     C                   END
309600140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
309700141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
309800141106     c*  quindi passo un flag
309900141106     c                   eval      nosomma='N'
310000140825     C                   EXSR      RO4CDP
310100141106     c                   eval      nosomma=' '
310200140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
310300140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
310400140825     c*  di consegna
310500140825     c                   EXSR      TRAS_DEI
310600140825     c
310700140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
310800140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
310900140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
311000140825     c                             ='M' or %subst(d98gc2:2:1)='P')
311100140825     c                   exsr      Reper_tempi
311200140825     c                   endif
311300931201     C*
311400140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
311500140825     c*  che tiene conto di:
311600140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
311700150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
311800931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
311900931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
312000931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
312100931201     C                   ELSE
312200931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
312300931201     C                   END
312400090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
312500090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
312600931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
312700140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
312800931201     C*
312900150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
313000931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
313100931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
313200090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
313300931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
313400150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
313500150608     c
313600150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
313700150608     c*  che non tiene conto dei tempi del C/Ass
313800150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
313900150609     c*
314000150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
314100150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
314200150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
314300150630     c                   Z-ADD     WPODTI        WR6POI
314400150609     c                   else
314500150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
314600150609     c                   endif
314700150609     c
314800150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
314900150608     c                   eval      wdee='S'
315000150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
315100150608     c                   eval      wdee=' '
315200150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
315300141017
315400141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
315500150219     C**                 EVAL      WTC='C'
315600150219     C**                 EVAL      WIC='C'
315700150219     c**                 z-add     wpoattdata00  WPOROU
315800150219     c**                 z-add     wdaodi        wpodata
315900150219     c**                 exsr      chkfes
316000150219     c**                 if        wslitt='S'
316100150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
316200150219     c**                 z-add     24            wr6noi
316300150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
316400150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
316500150219     c**                 else
316600150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
316700150219     c**                 endif
316800141204     c* Ricalcolo WPOrifDatLav
316900141229     C**                 MOVEL     'OSC  '       WR3TAB
317000141229     C**                 Z-ADD     WHHODI        WR3HHI
317100141229     C**                 EXSR      RO3ORE
317200141229     c**
317300141229     C**                 EVAL      WTC='C'
317400141229     C**                 EVAL      WIC='C'
317500141229     c**                 z-add     wpoattdata00  WPOROU
317600141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
317700141229     c**                 z-add     wr3hho        wr6noi
317800141229     c**                 add       12            wr6noi
317900141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
318000141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
318100141229     c
318200141017     c
318300141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
318400141017     c*  ma sempre devo slittare i festivi
318500141017     C* CONSEGNABILITA'
318600150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
318700150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
318800150219     C**                 EVAL      WTC='C'
318900150219     C**                 EVAL      WIC='C'
319000150219     c**                 z-add     d98dfc        WPOdata
319100150219     c**                 exsr      chkfes
319200150219     c**                 if        wslitt='S'
319300150219     c**                 z-add     wpodfc_00     Wr6poi
319400150219     c**                 z-add     24            wr6noi
319500150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
319600150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
319700150219     c**                 else
319800150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
319900150219     c**                 endif
320000150219     c**                 endif
320100931201     C*
320200931201     C                   ENDSR
320300930511     C********************************************************************
320400930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
320500930511     C********************************************************************
320600930511     C     CALDPC        BEGSR
320700930325     C*
320800930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
320900930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
321000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
321100930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
321200930325     C                   ELSE
321300930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
321400930407     C                   END
321500090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
321600090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
321700930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
321800090902     c
321900931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
322000090901
322100930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
322200930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
322300930423     C* A QUELLA DI RICHIESTA CONSEGNA,
322400930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
322500931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
322600931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
322700931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
322800931206     C                   END
322900930423     C                   END
323000930325     C*
323100930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
323200930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
323300930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
323400090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
323500090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
323600930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
323700931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
323800930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
323900930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
324000930423     C* A QUELLA DI RICHIESTA CONSEGNA,
324100930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
324200931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
324300931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
324400931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
324500931206     C                   END
324600930423     C                   END
324700140822     C*
324800930324     C                   ENDSR
324900150113     C********************************************************************
325000150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
325100150113     C********************************************************************
325200150113     c     Cal_DEE       BEGSR
325300150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
325400150508     c                   clear                   wtempi00C         1
325500151007     c                   clear                   wtempi12C         1
325600150722     c                   clear                   woreOLP
325700151009     c                   clear                   wdiff
325800150219     c
325900150219     c* 1) Se presente l'arrivo della trazione lo uso
326000150219    1c                   select
326100150219     c                   when      wmancatraz=' '  and d98dti>0
326200150630     c                   Z-ADD     WPODTI        WR1POI
326300150219     c
326400150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
326500150219    1c                   when      IDdata>0
326600150325     c                   exsr      CalcoloIMP
326700151008     c* Passo in output il terminal di disguido
326800151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
326900151008     c                   eval      d98flp=%editc(LTP(2):'X')
327000151008     c                   endif
327100151008
327200150528     c* stato IMP/disguido solo se >  alla data di oggi
327300151006     c                   if        wr1poi > wpoattdata00 and
327400151006     c                             (wr1poi-wpoattdata00)>12
327500150427     c                   eval      d98spcdee='IMP'
327600151008     c                   endif
327700151006     c                   if        wr1poi > wpoattdata00 and
327800151006     c                             (wr1poi-wpoattdata00)=12
327900151006     c                             and w991In>12
328000151006     c                   eval      d98spcdee='IMP'
328100151008     c                   endif
328200150219     c
328300150219     c                   other
328400150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
328500150225     c* devo far finta che sia arrivato.
328600150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
328700150508     c*  che tiene conto della data partenza PRIMO  collo
328800150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
328900150508     c*
329000150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
329100150508     c                   if        wr1poi >=wpoattdata00
329200150508     c                              and WPOPTC_DEE > WPOPTC
329300150508     c                   eval      d98spcdee='IMP'
329400150508     c                   endif
329500150219     c                   endsl
329600150113     c
329700150219     c* se il calcolo è < di oggi --> imposto oggi
329800150113     c                   if        wr1poi < wpoattdata00
329900150113     c                   eval      wr1poi= wpoattdata00
330000150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
330100150508     c                   eval      wTempi00C='S'
330200151009     c                   clear                   wdiff
330300150113     c                   endif
330400150217     c
330500150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
330600150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
330700150630     c                   if        wr1poi>wpoattdata00
330800150630     c                   eval      wdiff=wr1poi-wpoattdata00
330900150630     c                   endif
331000151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
331100151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
331200151006     c                   eval      wTempi12C='S'
331300151006     c                   endif
331400160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
331500151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
331600151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
331700160404     c                             and idfgs>0
331800151009     C     wdiff         DIV       24            WDIV
331900151009     C                   MVR                     WRES
332000151009     C                   IF        wres>0
332100151009     c                   eval      wTempi12C='S'
332200151009     c                   else
332300151009     c                   eval      wTempi00C='S'
332400151009     c                   endif
332500151009     c                   endif
332600151009     c
332700151006     c
332800151006     c                   select
332900151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
333000151006     c                             w991in>12 and
333100151006     c                             (deeindist<>'S' or d98cons<>'S'
333200151006     c                             or d98ddc>wdaodi)
333300151006     c
333400151006     c* Se stato IMP lo mantengo per avere messaggio diverso
333500151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
333600151006     c                   if        d98spcdee<>'IMP'
333700151006     c                   eval      d98spcdee='PRV'
333800151006     c                   endif
333900151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
334000150330     c                             (deeindist<>'S' or d98cons<>'S'
334100150330     c                             or d98ddc>wdaodi)
334200150427     c
334300150427     c* Se stato IMP lo mantengo per avere messaggio diverso
334400150427     c                   if        d98spcdee<>'IMP'
334500150304     c                   eval      d98spcdee='PRV'
334600150427     c                   endif
334700150427     c
334800151006   x1c                   other
334900150330     c                   exsr      CAL_OLP
335000150217     c
335100150528     c* Se passato l'orario oppure data distinta < data di oggi
335200150508     c* 1) spedizione in distinta          --> in consegna
335300150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
335400150528     c                             d98ddc<wdaodi)
335500150508    3c                   IF        deeindist='S' and d98cons='S'
335600150508     c                   eval      d98spcdee= 'DDC'
335700150508
335800150508   x3c                   else
335900150507     c
336000150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
336100150722     c*  entro sera
336200150630     c                   if        wr1poi>wpoattdata00
336300150722     c                   z-add     12            woreOLP           3 0
336400150722     c***                add       12            wr1poi
336500150630    4c                   else
336600150507    4c                   if        wr1poi<=wpoattdata00
336700150722     c                   add       24            woreOLP
336800150722     c***                add       24            wr1poi
336900150507    4c                   endif
337000150630    4c                   endif
337100150507     c
337200150722     c****               eval      d98spcdee='OLP'
337300150507     c* memorizzo orario limite picking
337400150507     c                   eval      d98picklim=%editc(picklim:'X')
337500150507    3c                   endif
337600150507    2c                   endif
337700150507     c
337800151006    1c                   endsl
337900150217     c
338000150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
338100150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
338200150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
338300150113     C                   Z-ADD     Wr1poi        W10POI            9 0
338400150113     C                   EXSR      R10FES
338500150113     C                   Z-ADD     W10POO        wr1poi
338600150113     C
338700151006     c                   select
338800151006    1c                   when      wtempi12C='S'
338900151006     c                   z-add     W991IN        wr1noi
339000151006     c                   if        woreOLP>0
339100151006     c                   add       woreOLP       wr1noi
339200151006     c                   eval      d98spcdee='OLP'
339300160404     c                   else
339400160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
339500160404     c**                 if        wVP='S'
339600160404     c**                 add       24            wr1noi
339700160404     c**                 endif
339800151006     c                   endif
339900160404
340000151006    1c                   when      wtempi00C='S'
340100150630     c***                z-add     W990CN        wr1noi
340200150630     c                   z-add     W990IN        wr1noi
340300150722     c
340400150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
340500150722     c*  quindi sottraggo 24
340600151009     c                   if        wr1noi>24  and wdiff=0
340700150722     c                   sub       24            wr1noi
340800150722     c                   endif
340900150722     c
341000150722     c* se passato orario limite picking, slitto in avanti
341100150722     c                   if        woreOLP>0
341200150722     c                   add       woreOLP       wr1noi
341300150722     c                   eval      d98spcdee='OLP'
341400160404     c                   else
341500160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
341600160404     c**                 if        wVP='S'
341700160404     c**                 add       24            wr1noi
341800160404     c**                 endif
341900150722     c                   endif
342000150722     c
342100151006   x1c                   other
342200150722     c
342300150714     c                   if        wmancatraz=' '  and d98dti>0
342400150630     c                   z-add     w99tci        wr1noi
342500150714     c                   else
342600150714     c                   z-add     w99tcc        wr1noi
342700150714     c                   endif
342800160404
342900160401     c
343000150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
343100150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
343200150722     c                   if        woreOLP=24 and wr1noi<=24
343300150722     c                   add       woreOLP       wr1noi
343400150722     c                   eval      d98spcdee='OLP'
343500150722     c                   endif
343600150722     c                   if        woreOLP=12 and wr1noi<24
343700150722     c                   add       woreOLP       wr1noi
343800150722     c                   eval      d98spcdee='OLP'
343900150722     c                   endif
344000150722     c
344100151006    1c                   endsl
344200150629     c
344300150722    1c**                 if        WR1NOI=10
344400150722     c**                 z-add     12            wr1noi
344500150722     c**                 endif
344600150722     c
344700150722     c     wr1noi        div       12            wdiv
344800150722     c                   mvr                     wres
344900150722     c                   if        wres >0
345000150722     c                   add       2             wr1noi
345100150722     c                   endif
345200150722     c
345300150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
345400150629    1c******             endif
345500150508     c
345600150605     c                   eval      wdee='S'
345700150113     C                   EXSR      RO1CDP
345800150605     c                   eval      wdee=' '
345900150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
346000150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
346100150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
346200150113     C* A QUELLA DI RICHIESTA CONSEGNA,
346300150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
346400150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
346500150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
346600150331     c*  2) IL        -> è il giorno indicato
346700150331     C     D98DCR        IFNE      *ZEROS
346800150331     C     D98tcr        andne     'P'
346900150331     C***  WPOdpcee      IFLE      WPOCR2
347000150331     C***  WPOCR2        SUB       12            WPOdpcee
347100150330     c
347200150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
347300150330     c*   dare la prima data utile di consegna
347400150330     C     WPOdpcee      IFLE      WPOCRF_dee
347500150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
347600150330     C                   z-add     WPOCRF_dee    WPOdpcee
347700150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
347800150409     c                   if        wpodpcee>wpoattdata00
347900150409     c                   eval      d98spcdee='PRV'
348000150409     c                   endif
348100150113     C                   END
348200150331     c
348300150220     c
348400150113     C                   END
348500150220     c
348600150113     c                   ENDSR
348700150330     C********************************************************************
348800150330     C*   Calcolo orario da tabella OLP
348900150330     C********************************************************************
349000150330     c     Cal_OLP       BEGSR
349100150330     c                   eval      picklim=0900
349200150330     c* Dopo l'ora limite picking, passo al giorno dopo
349300150330     C                   MOVEL(P)  'OLP'         TBECOD
349400150330     C                   MOVEL(P)  d98lna        TBEKE1
349500150330     C                   MOVEL(P)  wdaodi        TBEKE2
349600150330     C     KTBE          CHAIN     TNTBE01L
349700150330     C                   IF        not %found(TNTBE01L)
349800150330     c                   clear                   tbeke2
349900150330     C     KTBE          CHAIN     TNTBE01L
350000150330     c                   endif
350100150330     C                   IF        not %found(TNTBE01L)
350200150330     C                   MOVEL(P)  '000'         TBEKE1
350300150330     C     KTBE          CHAIN     TNTBE01L
350400150330     c                   endif
350500150330     c
350600150330     C                   IF        %found(TNTBE01L) and
350700150330     c                             %subst(tbeuni:1:4)>*zeros
350800150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
350900150330     c                   endif
351000150330     c                   ENDSR
351100150325     C********************************************************************
351200150325     C*   Riclcolo data se colli in IMP/disguido
351300150325     C********************************************************************
351400150325     c     CalcoloIMP    BEGSR
351500150325     C                   Z-ADD     IDdata        WDAT
351600150325     C                   EXSR      DATORE
351700150325     C                   Z-ADD     WORE          WR4POI
351800150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
351900151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
352000150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
352100150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
352200150325     c* calcolo il terminal di partenza
352300150325     c                   clear                   fnlv55ds
352400150325     c                   eval      d55tla='L'
352500150325     c                   eval      d55tpt='P'
352600150325     c                   eval      d55lin=IDfgs
352700150325     c                   eval      d55drf=IDdata
352800150325     c                   call      'FNLV55R'
352900150325     c                   parm                    fnlv55ds
353000150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
353100150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
353200150325     c                   EXSR      RTTRAZ
353300150325    3c                   if        d98err<>'1'
353400150325     C                   Z-ADD     CASTcn        wr4noi
353500150325     c                   else
353600150325     C                   Z-ADD     d98ttc        wr4noi
353700150325    3C                   endif
353800150325
353900150325   x2c                   else
354000150325     C                   Z-ADD     d98ttc        wr4noi
354100150325    2C                   endif
354200150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
354300150325     c*  perchè partirà un giorno dopo
354400150325    3c                   if        IDnpg='P'
354500150325     c                   add       24            WR4noi
354600150325    3c                   endif
354700150325     c
354800150325     c                   exsr      ro4cdp
354900150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
355000150325     c                   ENDSR
355100930604     C********************************************************************
355200930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
355300930604     C********************************************************************
355400930604     C     TPODAT        BEGSR
355500960229     C*
355600960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
355700960229     C*
355800960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
355900960229     C     WPOPTI        IFEQ      *ZEROS
356000960229     C                   Z-ADD     *ZEROS        D98DTI
356100960229     C                   Z-ADD     *ZEROS        D98HTI
356200960229     C                   ELSE
356300960229     C                   Z-ADD     WPOPTI        WORE
356400960229     C                   EXSR      OREDAT
356500960229     C                   Z-ADD     WDAT          D98DTI
356600960229     C     WRES          MULT      100           D98HTI
356700960229     C                   END
356800960229     C*
356900960229     C                   ELSE                                                   <-- 1 -->
357000930604     C*
357100931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
357200140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
357300140825     c*******            exsr      Tras_DEI
357400140825     c
357500930604     C*
357600150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
357700140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
357800140820     c* è data da :
357900140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
358000140820     c*  Tenere  conto, ovvero slittare in avanti, per :
358100140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
358200140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
358300140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
358400140911     c*
358500140820     c*  Da azzerare nei seguenti casi:
358600150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
358700150324     c* 4) bolla export    7) Cod manca consegna
358800140911     c* 8) bolla con DCR DOPOil o PRIMAdel
358900150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
359000150119     c*10) se completamente non arrivata e collo in IMP
359100150119     c
359200140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
359300150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
359400140925     C*  - FD   - export -
359500150217     c                   movel     'S'           DEEinDIST         1
359600150113     c                   eval      wcaldpcee='S'
359700150304     c                   eval      D98SPCDEE='PRV'
359800150324     c                   clear                   avvcmc            1
359900150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
360000150323     c                   if        d98cmc<>*blanks
360100150324     C                   Z-ADD     1             II
360200150326     C     d98cmc        LOOKUP    C2A(II)                                99
360300150324     C                   MOVEL     D2A(II)       usa2A
360400150324    4C     §2AFTC        IFEQ      'A'
360500150326     c                   eval      avvcmc='A'
360600150323     c                   endif
360700150324    4C     §2AFTC        IFEQ      'G'
360800150326     c                   eval      avvcmc='G'
360900150324     c                   endif
361000150324     c                   endif
361100140820     c
361200140911     c                   z-add     d98lna        EX
361300150113     c*  la azzero nei casi previsti                 0
361400150601    1c                   select
361500150119     c* 1) Se la bolla e consegnata
361600140925     c                   when      d98dcm>0
361700150113     c                   clear                   wcaldpcee
361800150216     c                   eval      d98spcdee='CON'
361900150217     c                   movel     ' '           DEEinDIST         1
362000150323     c* 2) Se pratica di danno
362100150603     c                   when      flgdan='S'
362200150323     c                   clear                   wcaldpcee
362300150323     c                   eval      d98spcdee='CA '
362400150323     c                   eval      deeindist='S'
362500150324     c* 3) Se bloccata/rimpallo
362600150114     c                   when      wd98fbc='B'
362700150113     c                   clear                   wcaldpcee
362800150216     c                   eval      d98spcdee='BLO'
362900150217     c                   movel     ' '           DEEinDIST         1
363000150324     c* 4) Se bolla export
363100150324     c                   when      fl1(EX)='E'
363200150324     c                   clear                   wcaldpcee
363300150324     c                   movel     ' '           DEEinDIST         1
363400150324     c                   eval      d98spcdee='EXP'
363500150324     c* 5) Se giacente  o in attesa di apertura giacenza
363600150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
363700150324     c                   clear                   wcaldpcee
363800150324     c                   eval      d98spcdee='GIA'
363900150324     c                   eval      deeindist='S'
364000150119     c* 7) Se codice manca consegna
364100150326     c                   when      d98cmc<>*blanks and avvcmc='A'
364200150324     c                   clear                   wpodpcee
364300150324     c                   clear                   wcaldpcee
364400150324     c                   movel     ' '           DEEinDIST         1
364500150324     c                   eval      d98spcdee='ATT'
364600150410     c* 7) Bolla in sospensione consegna
364700150410     c                   when      §B4LBGE='S'
364800150410     c                   clear                   wpodpcee
364900150410     c                   clear                   wcaldpcee
365000150410     c                   movel     ' '           DEEinDIST         1
365100150410     c                   eval      d98spcdee='SOS'
365200150616     c
365300150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
365400150616     c                   when      wrbl='R'
365500150616     c                   clear                   wcaldpcee
365600150616     c                   eval      d98spcdee='   '
365700150616     c                   eval      deeindist='S'
365800150601   x1c                   other
365900150325     c*
366000150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
366100150331     c                   if        d98ffd='S'
366200150601    2c                   select
366300150403     c* FD escluso dai contatti
366400150601    2c                   when      §B4LTFD= 'N'
366500150403     c                   eval      d98spcdee='FD3'
366600150403     c                   clear                   wcaldpcee
366700150403     c                   movel     ' '           DEEinDIST         1
366800150403
366900150403     c* FD non contattato ( o da ricontattare)
367000150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
367100150401     c                             olv82sts<>'1'
367200150323     c                   clear                   wcaldpcee
367300150323     c                   movel     ' '           DEEinDIST         1
367400150323     c                   eval      d98spcdee='FFD'
367500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
367600150424     c                   eval      wpodpcee=wpoffi
367700150403
367800150403     c* FD contattato e     IN distinta
367900150601    2c                   when      d98cons='S'
368000150401     c                   eval      d98spcdee='FD2'
368100150401     c                   clear                   wcaldpcee
368200150401     c                   movel     ' '           DEEinDIST         1
368300150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
368400150424     c                   eval      wpodpcee=wpoffi
368500150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
368600150427     c*  ritirare la merce
368700150427     C                   MOVEL     'C'           WTC                            *PARTENZA
368800150427     C                   MOVEL     'C'           WIC
368900150427     C                   Z-ADD     D98ddc        WDAT
369000150427     C                   EXSR      DATORE
369100150427     c                   z-add     wore          wr4poi
369200150427     c                   z-add     24            wr4noi
369300150427     C                   EXSR      RO4CDP
369400150427     c                   z-add     wr4poo        wore
369500150427     c                   exsr      OREDAT
369600150427     c                   movel     wdat          D98DTCTD
369700150601   x2c                   other
369800150403     c* FD contattato e NON in distinta
369900150331     c                   eval      d98spcdee='FD1'
370000150408     c                   eval      wpodpcee=wpoffi
370100150331     c                   clear                   wcaldpcee
370200150331     c                   movel     ' '           DEEinDIST         1
370300150601    2c                   endsl
370400150331     c
370500150331     c                   endif
370600150403     c
370700150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
370800150601    2c                   if        (d98tc1='A' or d98tc2='A' or
370900150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
371000150601     c                             and d98ffd= ' '
371100150325     c                   clear                   wcaldpcee
371200150325     c                   movel     ' '           DEEinDIST         1
371300150601    3c                   if        §B4LBAS= 'N'
371400150427     c                   eval      d98spcdee='AP3'
371500150403     c                   else
371600150403     c                   eval      d98spcdee='APP'
371700150601    3c                   endif
371800150601    2c                   endif
371900150325     c*   se la bolla ha il C/Assegno verifico se da contattare
372000150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
372100150325     c                   clear                   wcaldpcee
372200150325     c                   movel     ' '           DEEinDIST         1
372300150601    3c                   if        §B4LBAS= 'N'
372400150427     c                   eval      d98spcdee='CA3'
372500150403     c                   else
372600150403     c                   eval      d98spcdee='CAS'
372700150601    3c                   endif
372800150601    2c                   endif
372900150325     c* Per questi 3 casi devo impostare la data contatto destinatario
373000150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
373100150325     c                             d98spcdee='CAS'
373200150601    2c                   select
373300150601    2c                   when      d98dti>0 and wmancatraz= ' '
373400150325     c                   movel     d98dti        d98dtctd
373500150403     c* se piene le ore già portate ad un giorno lavorativo
373600150403     c                   if        wpodti>0
373700150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
373800150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
373900150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
374000150403     C                   Z-ADD     WPODTI        W10POI            9 0
374100150403     C                   EXSR      R10FES
374200150403     c                   z-add     w10poo        wore
374300150403     C                   EXSR      OREDAT
374400150403     C                   movel     WDAT          D98dtctd
374500150403     c                   endif
374600150403     c
374700150601    2c                   when      d98dam>0
374800150421     C                   Z-ADD     D98dam        WDAT
374900150421     C                   EXSR      DATORE
375000150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
375100150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
375200150421     C                   Z-ADD     Wore          W10POI            9 0
375300150421     C                   EXSR      R10FES
375400150421     c                   z-add     w10poo        wore
375500150421     C                   EXSR      OREDAT
375600150421     C                   movel     WDAT          D98dtctd
375700150601   x2c                   other
375800150325     c* data teorica di arrivo tenendo conto dell'IMP
375900150325     c                   exsr      letbrv
376000150325     c                   if        idDATA>0
376100150325     c                   exsr      CalcoloIMP
376200150325     c                   z-add     wr1poi        WORE
376300150325     c                   else
376400150325     c                   z-add     wpoptc        WORE
376500150325     c                   endif
376600150325     C                   EXSR      OREDAT
376700150325     c                   movel     wdat          d98dtctd
376800150601    2c                   endsl
376900150325     c
377000150325     c                   endif
377100150325     c
377200150601    1c                   endsl
377300140820     c
377400150113     c* Se la bolla non è in distinta calcolo la data se previsto
377500150220     c                   if        wcaldpcee='S' and
377600150220     c                             (d98cons<>'S'  or (d98cons='S' and
377700150220     c                             evemic<>'SI'))
377800150219     c
377900150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
378000150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
378100150219     c                   if        d98nsp>0 and wtrasm<>'D' and
378200150219     c                             d98dam=0
378300150219     c                   exsr      letBRV
378400150219     c                   endif
378500150219     c
378600150217     c                   eval      d98spcdee='PRG'
378700150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
378800150320     c                             d98tc1='S' or d98tc2='S')
378900150320     c                   eval      d98spcdee='APP'
379000150320     c                   endif
379100150320     c
379200150113     c                   exsr      cal_DEE
379300150113     c                   endif
379400150113     c
379500140820     C     WPOdpcee      IFEQ      *ZEROS
379600931202     C                   Z-ADD     *ZEROS        D98DEE
379700931202     C                   Z-ADD     *ZEROS        D98OEE
379800930604     C                   ELSE
379900140820     C                   Z-ADD     WPOdpcee      WORE
380000141021     C******             SUB       12            WORE
380100930604     C                   EXSR      OREDAT
380200931202     C                   Z-ADD     WDAT          D98DEE
380300090903     c                   if        d98tcc<>10
380400090903     C                   ADD       12            WRES
380500090903     c                   else
380600090903     C                   ADD       10            WRES
380700090903     c                   endif
380800090903
380900931202     C     WRES          MULT      100           D98OEE
381000090903     c
381100090903     c                   if        d98oee=1000
381200090903     c                   eval      d98oee=1030
381300090903     c                   endif
381400140925     C                   END
381500150311     c
381600150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
381700150311     c                   select
381800150311     c                   when      d98spcdee='OLP'
381900140820     c
382000150220     c* Se bolla in distinta --> data prevista = data distinta
382100150311     c                   when      deeindist='S'  and
382200150113     c                             d98cons='S'
382300150220
382400150225     c*  non la propongo senza MIC e se presente disguido o IMP
382500150220     c                   if        d98dee>d98ddc and evemic<>'SI'
382600150220     c                             and IDdata>0
382700150220     c                   else
382800150220
382900140820     c                   eval      d98dee=d98ddc
383000140925     c                   clear                   d98oee
383100150320     c* se la data è > di oggi stato PRV se non è in IMP
383200150320     c                   if        d98dee>wdaodi
383300150320     c                   if        d98spcdee<>'IMP'
383400150311     c                   eval      d98spcdee='PRV'
383500150320     c                   endif
383600150320     c
383700150225     c                   else
383800150217     c* se c'è il MIC
383900150217     c                   if        evemic='SI'
384000150217     c                   eval      d98spcdee='DDC'
384100150225     c                   endif
384200150217     c                   endif
384300150220     c                   endif
384400150217     c
384500150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
384600150722     c                   eval      d98spcdee='PRV'
384700150311     c                   endsl
384800931202     C*
384900931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
385000931202     C     WPODCI        IFEQ      *ZEROS
385100931202     C                   Z-ADD     *ZEROS        D98DCI
385200931202     C                   Z-ADD     *ZEROS        D98HCI
385300931202     C                   ELSE
385400931202     C                   Z-ADD     WPODCI        WORE
385500931202     C                   EXSR      OREDAT
385600931202     C                   Z-ADD     WDAT          D98DCI
385700090902     c                   if        d98tci<>10
385800931202     C                   ADD       12            WRES
385900090902     c                   else
386000090902     C                   ADD       10            WRES
386100090902     c                   endif
386200090902
386300931202     C     WRES          MULT      100           D98HCI
386400090901     c
386500090903     c                   if        d98hci=1000
386600090901     c                   eval      d98hci=1030
386700090901     c                   endif
386800090901     c
386900931202     C                   END
387000931202     C*
387100931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
387200931202     C     WPODCE        IFEQ      *ZEROS
387300931202     C                   Z-ADD     *ZEROS        D98DCE
387400931202     C                   Z-ADD     *ZEROS        D98HCE
387500931202     C                   ELSE
387600931202     C                   Z-ADD     WPODCE        WORE
387700931202//// C**************       SUB  12        WORE
387800931202     C                   EXSR      OREDAT
387900931202     C                   Z-ADD     WDAT          D98DCE
388000090902     c                   if        d98tcc >10
388100931202     C                   ADD       12            WRES
388200090902     c                   else
388300090902     C                   ADD       10            WRES
388400090902     c                   endif
388500090902     c
388600931202     C     WRES          MULT      100           D98HCE
388700090902     c
388800090903     c                   if        d98hce=1000
388900090902     c                   eval      d98hce=1030
389000090902     c                   endif
389100090902     c
389200931202     C                   END
389300960229     C*
389400960229     C                   ENDIF                                                  <-- 1 ---
389500930604     C*
389600930604     C                   ENDSR
389700150218     C********************************************************************
389800150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
389900150218     C********************************************************************
390000150218     c     LetBRV        BEGSR
390100150219     c                   clear                   bollaOK           1
390200150219     c                   clear                   IDnpg
390300150219     c                   clear                   IDdata
390400150219     c                   clear                   IDora
390500150219     c                   clear                   IDfgs
390600150219     c
390700150218     C     Kart          SETLL     FNART01L
390800150218     C     Kart          READE     FNART01L
390900151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
391000150219     c                   clear                   DISfgs
391100150219     c                   clear                   DISdata
391200150219     c                   clear                   DISnpg
391300150219     c                   clear                   DISora
391400150219     c                   clear                   IMPdata
391500150219     c                   clear                   IMPfgs
391600150508     c* per ogni collo verifico se in IMP o disguidato
391700150218     c* al primo che NON trovo, esco come --> OK
391800150218     c     kbrv          setll     fnbrv07l
391900150218     c     kbrv          reade     fnbrv07l
392000150219    2c                   dow       not %eof(fnbrv07l)
392100150219    3c                   if        brvatr='D'  or brvnpg=3
392200150218     c* prendo la data della spunta
392300150218     c                   clear                   fnlv53ds
392400150219     c                   eval      d53tla='L'
392500150218     C                   MOVEL     BRvNPG        D53NPG
392600150218     C                   Z-ADD     BRvNFV        D53NFV
392700150218     C                   MOVEL     BRvFGS        D53FGS
392800150218     C                   CALL      'FNLV53R'
392900150218     C                   PARM                    fnlv53ds
393000150218    4c                   if        d53err<>*blanks
393100150219    5c                   if        brvdcs>0
393200150218     c                   eval      d53dfv=brvdcs
393300150218     c                   else
393400150219    6c                   if        brvdfs>0
393500150218     c                   eval      d53dfv=brvdfs
393600150219    6c                   endif
393700150219    5c                   endif
393800150219    4c                   endif
393900150218     c
394000150219    4c                   select
394100150219     c* se si tratta di DISGUIDO
394200150218     c                   when      brvatr='D'
394300150219     c
394400150219    5c                   if        d53dfv=DISdata
394500150219     c* Verifico l'ora
394600150219    6c                   if        brvhcs>0 and brvhcs>DISora
394700150219     c                   eval      DISdata=d53dfv
394800150219     c                   eval      DISfgs =brvfgs
394900150219     c                   eval      DISnpg =brvnpg
395000150219     c                   eval      DISora =brvhcs
395100150219   x6c                   else
395200150219    7c                   if        brvhms>0 and brvhms>DISora
395300150219     c                   eval      DISdata=d53dfv
395400150219     c                   eval      DISfgs =brvfgs
395500150219     c                   eval      DISnpg =brvnpg
395600150219     c                   eval      DISora =brvhms
395700150219    7c                   endif
395800150219    6c                   endif
395900150219    5c                   endif
396000150219     c
396100150219    5c                   if        d53dfv>DISdata
396200150219     c                   eval      DISdata=d53dfv
396300150219     c                   eval      DISfgs =brvfgs
396400150219     c                   eval      DISnpg =brvnpg
396500150219     c                   eval      DISora =brvhcs
396600150219     c                   if        disora=0
396700150219     c                   eval      DISora =brvhms
396800150219     c                   endif
396900150219    5c                   endif
397000150219     c
397100151009     c* se si tratta di IMP
397200151009    4c                   when      d53spg='P' and d53dfv>impdata
397300151009     c                   eval      IMPdata=d53dfv
397400151009     c                   eval      IMPfgs =brvfgs
397500150219    4c                   endsl
397600150219    3c                   endif
397700151009
397800150219     c     kbrv          reade     fnbrv07l
397900150219    2c                   enddo
398000150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
398100150219     c*  esco dal ciclo: la bolla è OK
398200150219    2c                   if        impdata=0 and disdata=0
398300150219     c                   eval      bollaOK='S'
398400150219     c                   clear                   IDdata
398500150219     c                   clear                   IDfgs
398600150219     c                   clear                   IDora
398700150219     c                   clear                   IDnpg
398800150219   x2c                   else
398900150219     c* memorizzo la data più alta
399000150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
399100150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
399200150219     c                   eval      IDdata=IMPdata
399300150219     c                   eval      IDfgs=IMPfgs
399400150219     c                   eval      IDora=0
399500150219     c                   eval      IDnpg='P'
399600150219    3c                   endif
399700150219    3c                   if        (DISdata>0 and DISdata=IDdata
399800150219     c                             and IDnpg<>'P' and DISora>IDora) or
399900150219     c                             (DISdata>0 and DISdata>IDdata)
400000150219     c                   eval      IDdata=DISdata
400100150219     c                   eval      IDfgs=DISfgs
400200150219     c                   eval      IDora=DISora
400300150219     c                   eval      IDnpg=%char(DISnpg)
400400150219    3c                   endif
400500150219    2c                   endif
400600150218     c
400700150218     C     Kart          READE     FNART01L
400800150219    1c                   enddo
400900150219     c
401000150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
401100150508     c**                 if        d98duc> 0 and d98duc>IDdata
401200150508     c**                 eval      bollaOK='S'
401300150508     c**                 clear                   IDdata
401400150508     c**                 clear                   IDfgs
401500150508     c**                 clear                   IDora
401600150508     c**                 clear                   IDnpg
401700150508     c**                 endif
401800150219     c
401900150218     c
402000150218     C                   ENDSR
402100140825     C********************************************************************
402200140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
402300140825     C********************************************************************
402400140825     c     TRAS_DEI      BEGSR
402500140825     C     WPODEI        IFEQ      *ZEROS
402600140825     C                   Z-ADD     *ZEROS        D98DEI
402700140825     C                   Z-ADD     *ZEROS        D98OEI
402800140825     C                   ELSE
402900140825     C                   Z-ADD     WPODEI        WORE
403000140825     C                   SUB       12            WORE
403100140825     C                   EXSR      OREDAT
403200140825     c
403300140825     C                   Z-ADD     WDAT          D98DEI
403400140825     c                   if        d98tci<>10
403500140825     C                   ADD       12            WRES
403600140825     c                   else
403700140825     C                   ADD       10            WRES
403800140825     c                   endif
403900140825     c
404000140825     C     WRES          MULT      100           D98OEI
404100140825     c
404200140825     c                   if        d98oei=1000
404300140825     c                   eval      d98oei=1030
404400140825     c                   endif
404500140825     c
404600140825     C                   END
404700140825     c                   ENDSR
404800930604     C********************************************************************
404900930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
405000930604     C********************************************************************
405100930604     C     CHKFOR        BEGSR
405200930604     C*
405300930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
405400930604     C* E QUINDI FORZO LA CONFORMITA' A SI
405500941227     C     D98ERR        IFEQ      '0'
405600930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
405700930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
405800930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
405900930604     C                   MOVEL     '1'           D98ERR
406000930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
406100930604     C                   END
406200930604     C                   END
406300990212     C*
406400990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
406500990212     C* E QUINDI FORZO LA CONFORMITA' A SI
406600990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
406700990212     C     D98ERR        IFEQ      '0'
406800990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
406900141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
407000990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
407100990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
407200990212     C                   MOVEL     '1'           D98ERR
407300990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
407400990212     C                   END
407500990212     C                   END
407600990308     C*
407700990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
407800990308     C* E QUINDI FORZO LA CONFORMITA' A SI
407900100614     C**   D98ERR        IFEQ      '0'
408000100614     C**   WFBR          IFEQ      'S'                                          *RESO
408100100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
408200100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
408300100614     C**                 MOVEL     '1'           D98ERR
408400100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
408500100614     C**                 END
408600100614     C**                 END
408700930604     C*
408800930604     C                   ENDSR
408900990212     C********************************************************************
409000990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
409100990212     C********************************************************************
409200990212     C     CHKDAN        BEGSR
409300990212     C*
409400990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
409500150603     c                   movel     'N'           opendan           1
409600040809      *
409700040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
409800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
409900040809      * legate alla spedizione che passo
410000040809     c                   clear                   fidn12ds
410100040809     c                   eval      i12aas = D98AAS
410200040809     c                   eval      i12lnp = D98LNP
410300040809     c                   eval      i12nrs = D98NRS
410400040809     c                   eval      i12nsp = D98NSP
410500040809     c                   eval      i12fel = 'E'
410600040809     c                   eval      i12fan = 'E'
410700040809     c*
410800040809     c                   call      'FIDN12R'
410900040809     c                   parm                    fidn12ds
411000040809     c*
411100040809     c* se non ci sono errori
411200040809     c                   if        o12err = ' '
411300040809     c* se numero di CA trovate maggiore di zero
411400040809     c                   if        o12nca > 0
411500040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
411600150603     c
411700150603     c***                z-add     1             aa                2 0
411800150603     c***                dow       aa <= o12nca
411900150603     C* APERTA
412000150603     C***  skdch(aa)     IFEQ      0
412100150603     c***                movel     'S'           opendan
412200150603     c***                endif
412300150603     c*
412400150603     c***                add       1             aa
412500150603     c***                enddo
412600150603     c
412700040809     c                   endif
412800040809     c                   endif
412900990212     C*
413000990212     C                   ENDSR
413100941228     C********************************************************************
413200941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
413300941228     C********************************************************************
413400941228     C     CALORP        BEGSR
413500941228     C*
413600941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
413700941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
413800941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
413900960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
414000950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
414100950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
414200950110     C                   ELSE
414300950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
414400950110     C                   ENDIF
414500941228     C                   EXSR      RO2COP
414600941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
414700941228     C* SPED. IL GIORNO DOPO
414800941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
414900941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
415000941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
415100941228     C*
415200941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
415300941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
415400941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
415500941228     C                   Z-ADD     WPODSP        WR2POI
415600950110     C                   Z-ADD     WPOPPC        WR2POF
415700941228     C                   EXSR      RO2COP
415800941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
415900941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
416000941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
416100941228     C*
416200941228     C                   ENDSR
416300930414     C********************************************************************
416400930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
416500930414     C********************************************************************
416600930414     C     CALORT        BEGSR
416700930414     C*
416800930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
416900931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
417000930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
417100931126     C                   Z-ADD     *ZEROS        W98HF
417200950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
417300950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
417400950110     C                   ELSE
417500950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
417600950110     C                   ENDIF
417700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
417800931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
417900930608     C                   ELSE
418000931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
418100930608     C                   END
418200931126     C                   EXSR      RO2COV
418300931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
418400930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
418500930414     C*
418600930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
418700930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
418800930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
418900931126     C                   Z-ADD     *ZEROS        W98HF
419000950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
419100931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
419200931126     C                   EXSR      RO2COV
419300931126     C                   Z-ADD     WR2NOV        W98RTC
419400930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
419500930414     C*
419600930414     C                   ENDSR
419700930414     C********************************************************************
419800930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
419900930414     C********************************************************************
420000930414     C     CALORC        BEGSR
420100930511     C*
420200930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
420300930511     C* I TEMPI DI RITARDO AD OGGI
420400930511     C     WPODCM        IFEQ      *ZEROS
420500960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
420600930511     C                   END
420700940117     C*
420800940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
420900940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
421000940117     C* ALLA DATA DI FINE CONSEGNABILITA'
421100940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
421200940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
421300940118££££ C     WPODFC        IFNE      *ZEROS
421400940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
421500940118££££ C                   END
421600930414     C*
421700930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
421800930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
421900930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
422000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
422100931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
422200930608     C                   ELSE
422300931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
422400930608     C                   END
422500931126     C                   Z-ADD     WPODCM        WR5POF            9 0
422600931126     C                   EXSR      RO5COV
422700931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
422800930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
422900930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
423000060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
423100060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
423200930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
423300930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
423400930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
423500930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
423600930414     C*
423700930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
423800930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
423900930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
424000931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
424100931126     C                   Z-ADD     WPODCM        WR5POF            9 0
424200931126     C                   EXSR      RO5COV
424300931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
424400930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
424500930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
424600060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
424700060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
424800930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
424900930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
425000930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
425100930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
425200930414     C*
425300930414     C                   ENDSR
425400930414     C********************************************************************
425500930414     C*   CALORR - CALCOLA ORE REALI DI RESA
425600930414     C********************************************************************
425700930414     C     CALORR        BEGSR
425800930414     C*
425900950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
426000950110     C                   ADD       W98RTI        W98RRI
426100941228     C                   ADD       W98RCI        W98RRI
426200950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
426300950110     C                   ADD       W98RTC        W98RRC
426400941228     C                   ADD       W98RCC        W98RRC
426500930414     C*
426600930414     C                   ENDSR
426700930420     C********************************************************************
426800930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
426900930420     C********************************************************************
427000930420     C     CALDIF        BEGSR
427100930420     C*
427200941228     C* PARTENZA
427300941228     C     W98RPI        SUB       D98TPI        W98FPI
427400941228     C     W98RPC        SUB       D98TPC        W98FPC
427500941228     C*
427600930420     C* TRAZIONE
427700930423     C     W98RTI        SUB       D98TTI        W98FTI
427800930423     C     W98RTC        SUB       D98TTC        W98FTC
427900930420     C*
428000930420     C* CONSEGNA
428100090827     C***  W98RCI        SUB       D98TCI        W98FCI
428200090827     C     W98RCI        SUB       w99TCI        W98FCI
428300090827     C***  W98RCC        SUB       D98TCC        W98FCC
428400090827     C     W98RCC        SUB       w99TCC        W98FCC
428500930420     C*
428600930420     C* RESA
428700090921     C***  W98RRI        SUB       D98TRI        W98FRI
428800090921     C     W98RRI        SUB       w99TRI        W98FRI
428900090921     c
429000090921     C***  W98RRC        SUB       D98TRC        W98FRC
429100090921     C     W98RRC        SUB       w99TRC        W98FRC
429200930420     C*
429300930420     C                   ENDSR
429400930420     C********************************************************************
429500930420     C*   CALCNF - CALCOLA LE CONFORMITA'
429600930420     C********************************************************************
429700930420     C     CALCNF        BEGSR
429800941228     C*
429900941228     C*-------------------------------------------------------------------
430000941228     C* CONFORMITA' PARTENZA  INTERNA
430100941228     C*-------------------------------------------------------------------
430200941228     C                   Z-ADD     W98FPI        W98NPI
430300941228     C     W98FPI        IFLE      *ZEROS
430400941228     C                   MOVEL     'S'           D98CPI
430500941228     C                   ELSE
430600941228     C                   MOVEL     'N'           D98CPI
430700941228     C                   END
430800941228     C*
430900941228     C*-------------------------------------------------------------------
431000941228     C* CONFORMITA' PARTENZA  CLIENTE
431100941228     C*-------------------------------------------------------------------
431200941228     C                   Z-ADD     W98FPC        W98NPC
431300941228     C     W98FPC        IFLE      *ZEROS
431400941228     C                   MOVEL     'S'           D98CPC
431500941228     C                   ELSE
431600941228     C                   MOVEL     'N'           D98CPC
431700941228     C                   END
431800930420     C*
431900930420     C*-------------------------------------------------------------------
432000930420     C* CONFORMITA' TRAZIONE  INTERNA
432100930420     C*-------------------------------------------------------------------
432200930510     C                   Z-ADD     W98FTI        W98NTI
432300930423     C     W98FTI        IFLE      *ZEROS
432400930420     C                   MOVEL     'S'           D98CTI
432500930420     C                   ELSE
432600930420     C                   MOVEL     'N'           D98CTI
432700930420     C                   END
432800930420     C*
432900930420     C*-------------------------------------------------------------------
433000930420     C* CONFORMITA' TRAZIONE  CLIENTE
433100930420     C*-------------------------------------------------------------------
433200930510     C                   Z-ADD     W98FTC        W98NTC
433300930423     C     W98FTC        IFLE      *ZEROS
433400930420     C                   MOVEL     'S'           D98CTC
433500930420     C                   ELSE
433600930420     C                   MOVEL     'N'           D98CTC
433700930420     C                   END
433800930420     C*
433900930420     C*-------------------------------------------------------------------
434000930420     C* CONFORMITA' CONSEGNA  INTERNA
434100930420     C*-------------------------------------------------------------------
434200150226     c                   if        wmancatraz='S'
434300150226     c                   clear                   d98dti
434400150226     c                   clear                   d98hti
434500150226     c                   endif
434600150226     c
434700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
434800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
434900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
435000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
435100050914     c                   if        consideraora='S'
435200050914     c     w98fci        sub       24            w98nci
435300050914     c                   add       24            d98hrc
435400050914     c                   else
435500930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
435600050914     c                   endif
435700080116     c
435800110923     c                   select
435900080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
436000080116     c*  --> trasformo il ritardo in 0
436100090827     c                   when      d98cca='2'
436200080116     c                   z-add     0             w98nci
436300090827
436400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
436500080116     c*  e sto leggendo la bolla in arrivo
436600080116     c*  --> trasformo il ritardo = 0
436700090827     c                   when      d98cca='1' and d98tbo='A'
436800080116     c                   z-add     0             w98nci
436900090902     c
437000090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
437100090902     c                   when      wmancaTRAZ='S'
437200090902     c                   z-add     0             w98nci
437300090902     c                   clear                   d98dti
437400090902     c                   clear                   d98hti
437500110923     c
437600110923     c* Ignoro ritardo se pefect order
437700130221     c                   when      d98xco >*zeros
437800110923     c                   z-add     0             w98nci
437900090827     c
438000090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
438100091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
438200091014     c*  controllo ora soglia  consegna da tabella OSC)
438300091009     c                   when      d98tci=10   and
438400091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
438500091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
438600091014     c*
438700091014     c                             d98hci=1030
438800091014     C                   MOVEL     'OSC  '       WR3TAB
438900091014     C                   eval      WR3HHI=d98hmc
439000091014     C                   EXSR      RO3ORE
439100131218
439200091014     c                   if        wr3matpom='M' and d98hmc>1030
439300131218     c                   exsr      ctrLAVH1030
439400131218
439500131218     c                   if        wlav1030<>'S'
439600091014     c                   add       12            w98nci
439700091014     c                   endif
439800131218     c                   endif
439900090928     c
440000090827     c                   endsl
440100050914     c
440200930524     C                   ELSE                                                   <-- 1 -->
440300930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
440400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
440500930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
440600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
440700930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
440800930608     C                   ELSE
440900930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
441000930608     C                   END
441100930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
441200930525     C                   EXSR      RO2COV
441300930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
441400930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
441500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
441600930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
441700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
441800930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
441900930608     C                   ELSE
442000930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
442100930608     C                   END
442200931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
442300930525     C                   EXSR      RO2COV
442400930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
442500930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
442600930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
442700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
442800930525     C* E' IL RITARDO
442900930610     C     WNOCRI        SUB       W98RCI        W98NCI
443000050901     c
443100050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
443200050901     c*  --> trasformo il ritardo in anticipo
443300050901     c                   if        d98cca='2'
443400080116     c***                z-sub     w98nci        w98nci
443500080116     c                   z-add     0             w98nci
443600050901     c                   endif
443700111018     c* Ignoro ritardo se pefect order
443800130221     c                   if        d98xco >*zeros
443900111018     c                   z-add     0             w98nci
444000111018     c                   endif
444100061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
444200061121     c*  e sto leggendo la bolla in arrivo
444300061121     c*  --> trasformo il ritardo in anticipo
444400061121     c                   if        d98cca='1' and d98tbo='A'
444500080116     c***                z-sub     w98nci        w98nci
444600080116     c                   z-add     0             w98nci
444700061121     c                   endif
444800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
444900070827     c*  mittente, --> consegnato in anticipo
445000070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
445100140403     c                   if        Callar5=' '
445200070827     c                   exsr      LeggiAR5
445300140403     c                   endif
445400070827     c
445500070827     c                   if        dcrpart<>'S'
445600070824     c                   z-sub     w98nci        w98nci
445700070824     c                   endif
445800070827     c
445900070824     c                   endif
446000050907     c
446100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
446200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
446300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
446400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
446500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
446600050916     c***                if        d98dfc>0
446700070828     c                   if        d98dcm=0  and w98nci>0
446800050907     c                   z-sub     w98nci        w98nci
446900050907     c                   sub       oreILoDOPO    w98nci
447000050907     c                   endif
447100050901     c
447200930525     C                   ELSE                                                   <-- 2 -->
447300070521     c* non do ritardo se la consegna reale cade nel giorno
447400070521     c*  della consegna richiesta se presente IL
447500100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
447600070521     c                             wpodcm<=wpocr1
447700070521     c                   clear                   w98nci
447800100224     c                   else                                                   ---2a -->
447900930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
448000090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
448100090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
448200930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
448300930525     C* E' IL RITARDO O L'ANTICIPO
448400930525     C     W98RCI        SUB       WNOCRF        W98NCI
448500070115     c
448600070115     c* non do ritardo se comunque la consegna reale cade nel giorno
448700070115     c*  della consegna richiesta se presente IL
448800070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
448900070521     c***                          wpodcm<=wpocr1  and w98nci>0
449000070521     c***                clear                   w98nci
449100070521     c***                endif
449200070115     c
449300930525     C                   ELSE                                                   <-- 3 -->
449400930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
449500930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
449600930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
449700930525     C                   END                                                    <-- 3 ---
449800100224     C                   END                                                    <-- 2a---
449900100224
450000100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
450100100224     c                   select
450200100224     c                   when      d98cca='2'
450300100224     c                   z-add     0             w98nci
450400100224
450500100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
450600100224     c*  e sto leggendo la bolla in arrivo
450700100224     c                   when      d98cca='1' and d98tbo='A'
450800100224     c                   z-add     0             w98nci
450900100224     c                   endsl
451000100224     c
451100100224     C                   END                                                    <-- 2 ---
451200070521     C                   END                                                    <-- 1 ---
451300070521     c
451400970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
451500970915     C*/         WAZNCI    IFEQ 'S'
451600970915     C*/         W98NCI    ANDLT*ZEROS
451700970915     C*/                   Z-ADD*ZEROS    W98NCI
451800970915     C*/                   ENDIF
451900970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
452000050922     c* lo faccio solo se non c'e' la data cons richiesta
452100970915     C     D98DTI        IFEQ      *ZEROS
452200050922     c     d98dcr        andeq     *zeros
452300970915     C                   Z-ADD     *ZEROS        W98NCI
452400970915     C                   ENDIF
452500930524     C*
452600930524     C* CONTROLLO CONFORMITA
452700930423     C     W98NCI        IFLE      *ZEROS
452800930524     C                   MOVEL     'S'           D98CCI
452900930420     C                   ELSE
453000930524     C                   MOVEL     'N'           D98CCI
453100930420     C                   END
453200930420     C*
453300930420     C*-------------------------------------------------------------------
453400930420     C* CONFORMITA' CONSEGNA  CLIENTE
453500930420     C*-------------------------------------------------------------------
453600930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
453700930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
453800930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
453900050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
454000050914     c                   if        consideraora='S'
454100050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
454200050914     c                   else
454300050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
454400050914     c                   endif
454500080116     c
454600080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
454700080116     c*  --> trasformo il ritardo =0
454800090827     c                   select
454900090827     c                   when      d98cca='2'
455000080116     c                   z-add     0             w98ncc
455100090827
455200080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
455300080116     c*  e sto leggendo la bolla in arrivo
455400080116     c*  --> trasformo il ritardo =0
455500090827     c                   when      d98cca='1' and d98tbo='A'
455600080116     c                   z-add     0             w98ncc
455700090902     c
455800090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
455900090902     c                   when      wmancaTRAZ='S'
456000090902     c                   z-add     0             w98ncc
456100110923     c* Ignoro ritardo se Perfect Order
456200130221     c                   when      d98xco >*zeros
456300110923     c                   z-add     0             w98ncc
456400090902     c
456500090827     c
456600090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
456700090827     c* sono  in ritardo di 12 ore
456800091009     c                   when      d98tcc=10  and
456900091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
457000091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
457100091014     c*
457200091014     c                             d98hce=1030
457300091014     C                   MOVEL     'OSC  '       WR3TAB
457400091014     C                   eval      WR3HHI=d98hmc
457500091014     C                   EXSR      RO3ORE
457600091014     c                   if        wr3matpom='M' and d98hmc>1030
457700131218     c                   exsr      ctrLAVH1030
457800131218
457900131218     c                   if        wlav1030<>'S'
458000131218     c                   add       12            w98ncc
458100131218     c                   endif
458200131218     c****               add       12            w98ncc
458300091014     c                   endif
458400131218     c
458500091014     c***                z-add     12            w98ncc
458600090827     c                   endsl
458700050914     c
458800930524     C                   ELSE                                                   <-- 1 -->
458900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
459000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
459100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
459200930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
459300930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
459400930525     C                   EXSR      RO2COV
459500930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
459600930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
459700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
459800930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
459900930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
460000931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
460100930525     C                   EXSR      RO2COV
460200930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
460300930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
460400930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
460500930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
460600930525     C* E' IL RITARDO
460700930525     C     WNOCRI        SUB       W98RCC        W98NCC
460800050901     c
460900050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
461000050901     c*  --> trasformo il ritardo in anticipo
461100050901     c                   if        d98cca='2'
461200080116     c***                z-sub     w98ncc        w98ncc
461300080116     c                   z-add     0             w98ncc
461400050901     c                   endif
461500111018     c* Ignoro ritardo se pefect order
461600130221     c                   if        d98xco >*zeros
461700111018     c                   z-add     0             w98ncc
461800111018     c                   endif
461900061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
462000061121     c*  e sto leggendo la bolla in arrivo
462100061121     c*  --> trasformo il ritardo in anticipo
462200061121     c                   if        d98cca='1' and d98tbo='A'
462300080116     c***                z-sub     w98ncc        w98ncc
462400080116     c                   z-add     0             w98ncc
462500061121     c                   endif
462600070824     c
462700070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
462800070827     c*  mittente, --> consegnato in anticipo
462900070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
463000070827     c* SBR per vedere se tassativa
463100070827     c                   if        Callar5=' '
463200070827     c                   EXSR      LeggiAR5
463300070827     c                   endif
463400070827     c                   if        dcrpart<>'S'
463500070824     c                   z-sub     w98ncc        w98ncc
463600070824     c                   endif
463700070824     c                   endif
463800050907     c
463900050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
464000050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
464100050907     c*  lo trasformo in anticipo perchè non sono in ritardo
464200050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
464300050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
464400050916     c***                if        d98dfc>0
464500070828     c                   if        d98dcm=0 and w98ncc>0
464600050907     c                   z-sub     w98ncc        w98ncc
464700050907     c                   sub       oreILoDOPO    w98ncc
464800050907     c                   endif
464900050901     c
465000930525     C                   ELSE                                                   <-- 2 -->
465100070521     c* Non do ritardo se la consegna reale cade nel giorno
465200070521     c*  della consegna richiesta se presente IL
465300100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
465400070521     c                             wpodcm<=wpocr2
465500070521     c                   clear                   w98ncc
465600100224     c                   else                                                   <--2a -->
465700070521     c
465800930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
465900090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
466000090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
466100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
466200930525     C* E' IL RITARDO O L'ANTICIPO
466300930525     C     W98RCC        SUB       WNOCRF        W98NCC
466400070115     c
466500070115     c* non do ritardo se comunque la consegna reale cade nel giorno
466600070115     c*  della consegna richiesta se presente IL
466700070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
466800070521     c***                          wpodcm<=wpocr2 and w98ncc>0
466900070521     c***                clear                   w98ncc
467000070521     c***                endif
467100070115     c
467200930525     C                   ELSE                                                   <-- 3 -->
467300930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
467400930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
467500930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
467600930525     C                   END                                                    <-- 3 ---
467700100224     C                   END                                                    <-- 2a---
467800100224
467900100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
468000100224     c                   select
468100100224     c                   when      d98cca='2'
468200100224     c                   z-add     0             w98nci
468300100224
468400100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
468500100224     c*  e sto leggendo la bolla in arrivo
468600100224     c                   when      d98cca='1' and d98tbo='A'
468700100224     c                   z-add     0             w98nci
468800100224     c                   endsl
468900100224     c
469000100224     C                   END                                                    <-- 2 ---
469100070521     C                   END                                                    <-- 1 ---
469200930524     C*
469300930524     C* CONTROLLO CONFORMITA
469400930423     C     W98NCC        IFLE      *ZEROS
469500930524     C                   MOVEL     'S'           D98CCC
469600930420     C                   ELSE
469700930524     C                   MOVEL     'N'           D98CCC
469800930420     C                   END
469900930420     C*
470000930420     C*-------------------------------------------------------------------
470100930420     C* CONFORMITA' RESA INTERNA
470200930420     C*-------------------------------------------------------------------
470300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
470400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
470500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
470600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
470700050914     c                   if        consideraora='S'
470800050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
470900050914     c                   else
471000050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
471100050914     c                   endif
471200080116     c
471300080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
471400090827     c                   select
471500090827     c                   when      d98cca='2' and w98nri>0
471600080116     c                   z-add     0             w98nri
471700090827     c
471800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
471900080116     c*  e sto leggendo la bolla in arrivo
472000090827     c                   when      d98cca='1' and d98tbo='A' and
472100080116     c                                            w98nri>0
472200080116     c                   z-add     0             w98nri
472300110923     c
472400110923     c* Ignoro ritardo se Perfect Order
472500130221     c                   when      d98xco >*zeros
472600110923     c                   z-add     0             w98nri
472700110923     c
472800090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
472900090827     c* sono  in ritardo di 12 ore
473000091009     c                   when      d98tci=10  and
473100091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
473200091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
473300091014     c*
473400091014     c                             d98hci=1030
473500091014     C                   MOVEL     'OSC  '       WR3TAB
473600091014     C                   eval      WR3HHI=d98hmc
473700091014     C                   EXSR      RO3ORE
473800091014     c                   if        wr3matpom='M' and d98hmc>1030
473900131218     c                   exsr      ctrLAVH1030
474000131218
474100131218     c                   if        wlav1030<>'S'
474200091014     c                   add       12            w98nri
474300091014     c                   endif
474400131218     c                   endif
474500091014     c
474600091014     c***                z-add     12            w98nri
474700090827     c                   endsl
474800090827
474900930524     C                   ELSE                                                   <-- 1 -->
475000930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
475100930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
475200930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
475300931206     C     WPODCM        ANDLE     WPOCR1
475400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
475500930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
475600930525     C                   Z-ADD     WPODCM        WR2POI            9 0
475700931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
475800930525     C                   EXSR      RO2COV
475900930525     C     WR2NOV        IFNE      *ZEROS
476000930525     C     WR2NOV        MULT      -1            W98NRI
476100930525     C                   END
476200930524     C                   ELSE                                                   <-- 2 -->
476300930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
476400930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
476500930524     C     WPODCM        IFLT      WPOCRI
476600930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
476700930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
476800930524     C                   ELSE
476900931206     C                   Z-ADD     WPOCR1        WPORFI
477000930524     C                   Z-ADD     WPODCM        WPORFF
477100930524     C                   END
477200930524     C* CALCOLO ORE DI DIFFERENZA
477300930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
477400930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
477500930524     C                   Z-ADD     WPORFI        WR2POI            9 0
477600930524     C                   Z-ADD     WPORFF        WR2POF            9 0
477700930524     C                   EXSR      RO2COV
477800930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
477900050902     c
478000050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
478100100224     c                   if        d98cca='2' and w98nri>0
478200100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
478300080116     c***                z-sub     w98nri        w98nri
478400080116     c                   z-add     0             w98nri
478500050902     c                   endif
478600111018     c* Ignoro ritardo se pefect order
478700130221     c                   if        d98xco >*zeros
478800111018     c                   z-add     0             w98nri
478900111018     c                   endif
479000061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
479100061121     c*  e sto leggendo la bolla in arrivo
479200100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
479300100224     c***                if        d98cca='1' and d98tbo='A' and
479400100224     c***                                         w98nri>0 and wpodcm<wpocri
479500080116     c***                z-sub     w98nri        w98nri
479600080116     c                   z-add     0             w98nri
479700061121     c                   endif
479800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
479900070827     c*  mittente, --> consegnato in anticipo
480000070828     c                   if        d98tcr=' ' and d98nsp>0  and
480100070828     c                                            w98nri>0 and wpodcm<wpocri
480200070827     c                   if        CallAR5=' '
480300070827     c                   EXSR      LeggiAR5
480400070824     c                   endif
480500070827     c                   if        dcrpart<>'S'
480600070824     c                   z-sub     w98nri        w98nri
480700070824     c                   endif
480800070824     c                   endif
480900050907     c
481000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
481100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
481200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
481300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
481400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
481500050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
481600050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
481700050907     c                   z-sub     w98nri        w98nri
481800050907     c                   sub       oreILoDOPO    w98nri
481900050907     c                   endif
482000050907     c
482100050907     C                   END                                                    <-- 2 ---
482200930524     C                   END                                                    <-- 1 ---
482300930420     C* CALCOLO LE ORE DI RESA
482400930420     C* CONTROLLO CONFORMITA
482500930423     C     W98NRI        IFLE      *ZEROS
482600930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
482700930420     C                   ELSE
482800930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
482900930420     C                   END
483000930420     C*
483100930420     C*-------------------------------------------------------------------
483200930420     C* CONFORMITA' RESA CLIENTE
483300930420     C*-------------------------------------------------------------------
483400930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
483500930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
483600930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
483700050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
483800050914     c                   if        consideraora='S'
483900050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
484000050914     c                   else
484100050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
484200050914     c                   endif
484300090827     c
484400080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
484500090827     c                   select
484600090827     c                   when      d98cca='2' and w98nrc>0
484700080116     c                   z-add     0             w98nrc
484800090827
484900080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
485000080116     c*  e sto leggendo la bolla in arrivo
485100090827     c                   when      d98cca='1' and d98tbo='A' and
485200080116     c                                            w98nrc>0
485300080116     c                   z-add     0             w98nrc
485400110923     c
485500110923     c* Ignoro ritardo se Perfect Order
485600130221     c                   when      d98xco >*zeros
485700110923     c                   z-add     0             w98nrc
485800090827     c
485900090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
486000090827     c* sono  in ritardo di 12 ore
486100091009     c                   when      d98tcc=10  and
486200091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
486300091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
486400091014     c                             d98hce=1030
486500091014     C                   MOVEL     'OSC  '       WR3TAB
486600091014     C                   eval      WR3HHI=d98hmc
486700091014     C                   EXSR      RO3ORE
486800091014     c                   if        wr3matpom='M' and d98hmc>1030
486900131218     c                   exsr      ctrLAVH1030
487000131218
487100131218     c                   if        wlav1030<>'S'
487200091014     c                   add       12            w98nrc
487300091014     c                   endif
487400131218     c                   endif
487500091014     c
487600091014     c**                 z-add     12            w98nrc
487700090827     c                   endsl
487800090827     c
487900930524     C                   ELSE                                                   <-- 1 -->
488000930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
488100930524     C* DI DIFFERENZA SONO UGUALI A ZERO
488200930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
488300931206     C     WPODCM        ANDLE     WPOCR2
488400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
488500930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
488600930525     C                   Z-ADD     WPODCM        WR2POI            9 0
488700931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
488800930525     C                   EXSR      RO2COV
488900930525     C     WR2NOV        IFNE      *ZEROS
489000930525     C     WR2NOV        MULT      -1            W98NRC
489100930525     C                   END
489200930524     C                   ELSE                                                   <-- 2 -->
489300930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
489400930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
489500930524     C     WPODCM        IFLT      WPOCRI
489600930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
489700930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
489800930524     C                   ELSE
489900931206     C                   Z-ADD     WPOCR2        WPORFI
490000930524     C                   Z-ADD     WPODCM        WPORFF
490100930524     C                   END
490200930524     C* CALCOLO ORE DI DIFFERENZA
490300930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
490400930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
490500930524     C                   Z-ADD     WPORFI        WR2POI            9 0
490600930524     C                   Z-ADD     WPORFF        WR2POF            9 0
490700930524     C                   EXSR      RO2COV
490800930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
490900050902     c
491000050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
491100100224     c                   if        d98cca='2' and w98nrc>0
491200100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
491300080116     c***                z-sub     w98nrc        w98nrc
491400080116     c                   z-add     0             w98nrc
491500050902     c                   endif
491600111018     c* Ignoro ritardo se pefect order
491700130221     c                   if        d98xco >*zeros
491800111018     c                   z-add     0             w98nrc
491900111018     c                   endif
492000070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
492100061121     c*  e sto leggendo la bolla in arrivo
492200100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
492300100224     c***                if        d98cca='1' and d98tbo='A' and
492400100224     c***                                         w98nrc>0 and wpodcm<wpocri
492500080116     c***                z-sub     w98nrc        w98nrc
492600080116     c                   z-add     0             w98nrc
492700061121     c                   endif
492800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
492900070827     c*  mittente, --> consegnato in anticipo
493000070828     c                   if        d98tcr=' ' and d98nsp>0 and
493100070828     c                                            w98nrc>0 and wpodcm<wpocri
493200070827     c                   if        CallAR5=' '
493300070827     c                   EXSR      LeggiAR5
493400070827     c                   endif
493500070827     c                   if        dcrpart<>'S'
493600070824     c                   z-sub     w98nrc        w98nrc
493700070824     c                   endif
493800070824     c                   endif
493900050907     c
494000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
494100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
494200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
494300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
494400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
494500050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
494600050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
494700050907     c                   z-sub     w98nrc        w98nrc
494800050907     c                   sub       oreILoDOPO    w98nrc
494900050907     c                   endif
495000050907     c
495100050907     C                   END                                                    <-- 2 ---
495200930524     C                   END                                                    <-- 1 ---
495300930420     C* CALCOLO LE ORE DI RESA
495400930420     C* CONTROLLO CONFORMITA
495500930423     C     W98NRC        IFLE      *ZEROS
495600930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
495700930420     C                   ELSE
495800930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
495900930420     C                   END
496000930420     C*
496100930420     C                   ENDSR
496200070827     c
496300131218     c********************************************************************
496400131218     c     CtrlavH1030   BEGSR
496500131218     c********************************************************************
496600131218     c                   clear                   wlav1030          1
496700140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
496800131218     c                   z-add     1             xx                4 0
496900131218     c                   dow       evc(xx)<>*blanks
497000140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
497100140127     c                   if        evd(xx)=d98dcm
497200131218     c                   if        evo(xx)<=1030
497300131218     c                   eval      wlav1030='S'
497400131218     c                   leave
497500131218     c                   endif
497600131218     c                   endif
497700131218     c                   add       1             xx
497800131218     c                   enddo
497900131218     c                   ENDSR
498000070827     c********************************************************************
498100070827     c* Controllo record GEN di FIAR5
498200070827     c********************************************************************
498300070827     c     LeggiAR5      BEGSR
498400070827     c                   clear                   DCRPART           1
498500141104     c                   clear                   dar5gen
498600141104     c                   clear                   gc1PART           2
498700141104     c                   clear                   gc2PART           2
498800070827     c     kar5          chain     fiar501l
498900070827    1c                   if        not %found
499000070827     c                   eval      dcrpart='S'
499100141104     c                   eval      gc1part=d98gc1
499200141104     c                   eval      gc2part=d98gc2
499300070827   x1c                   else
499400070827     c* i dati devono essere quelli immessi dalla partenza
499500070827     c                   movel     ar5uni        dar5gen
499600070827     c
499700070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
499800070827     c*  ma il record GEN è stato scritto per altri motivi
499900070827    2c                   if        §ar5mdp='S'
500000070827     c                   movel     d98dcr        alfadcr           8
500100141104     c                   eval      gc1part=§ar5gc1
500200141104     c                   eval      gc2part=§ar5gc2
500300070827     c
500400070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
500500070827     c                   eval      dcrpart='S'
500600070827    3c                   endif
500700070827     c
500800070827   x2c                   else
500900070827     c                   eval      dcrpart='S'
501000141106     c                   eval      gc1part=d98gc1
501100141106     c                   eval      gc2part=d98gc2
501200070827    2c                   endif
501300070827    1c                   endif
501400070827     c
501500140402     c* Memorizzo il flag pickup 2
501600140402     c                   eval      wpick2=§AR5PK2
501700140402     c
501800150401     c*
501900150414     c**                 if        d98cas>0 or d98ffd<>' ' or
502000150414     c**                           d98tc1='A'  or d98tc2='A' or
502100150414     c**                           d98tc1='S'  or d98tc2='S'
502200160331     c                   eval      kar4trc='L'
502300150401     c     kar4          chain     fiar401l
502400150401     c                   if        %found(fiar401l)
502500150401     c                   eval      dsbl4L=ar4not
502600150401     c                   endif
502700150401     c* Solo per fermo deposito verifico se inviato alert
502800150401     c                   if        d98ffd<>' '
502900150401     c                   clear                   fnlv82ds
503000150401     c                   eval      ILV82AAS=d98aas
503100150401     c                   eval      ILV82LNP=d98lnp
503200150401     c                   eval      ILV82NRS=d98nrs
503300150401     c                   eval      ILV82NSP=d98nsp
503400150401     c                   eval      ILV82TIPA='FD'
503500150401     c                   call      'FNLV82R'
503600150401     c                   parm                    fnlv82ds
503700150401     c                   endif
503800150414     c**                 endif
503900150401
504000150401     c                   eval      callar5='S'
504100160331     c
504200160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
504300160331     c                   if        %lookup(d98lnp:fildpd)>0
504400160331     c                   eval      kar4trc='I'
504500160331     c     kar4          chain     fiar401l
504600160331     c                   if        %found(fiar401l)
504700160331     c                   eval      dsbl4I=ar4not
504800160331     c                   endif
504900160401     c                   if        §b4trd='1' or §b4trd='2'
505000160401     c                   eval      wVP='S'
505100160401     c                   endif
505200160401     c
505300160331     c                   endif
505400150401
505500070827     c                   ENDSR
505600930913     C********************************************************************
505700930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
505800930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
505900930913     C*            DATA/ORA (FINE RANGE)
506000930913     C********************************************************************
506100930913     C     CALCNS        BEGSR
506200930914     C*
506300930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
506400930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
506500930914     C     WPODTI        IFEQ      *ZEROS
506600960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
506700930914     C                   ELSE
506800930914     C                   Z-ADD     WPODTI        WPOIRC
506900930914     C                   END
507000930914     C*
507100930914     C* SE NON ANCORA CONSEGNATA
507200930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
507300050922     c* in realtà nella data consegna c'e' la data fine calcolo
507400050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
507500930914     C     WPODCM        IFEQ      *ZEROS
507600050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
507700930914     C                   ELSE
507800930916     C                   Z-ADD     WPODCM        WPOFRC
507900930914     C                   END
508000930914     C*
508100930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
508200930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
508300930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
508400930914     C     WPODFC        ANDLT     WPOIRC
508500930914     C                   ELSE
508600930914     C*
508700930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
508800930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
508900930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
509000930914     C     WPODFC        ANDGT     WPOFRC
509100930914     C                   ELSE
509200930916     C*
509300930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
509400930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
509500930916     C     WPODFC        ANDGT     WPOFRC
509600930916     C                   ELSE
509700930916     C*
509800930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
509900930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
510000930916     C     WPODFC        ANDLT     WPOIRC
510100930916     C                   ELSE
510200930914     C*
510300930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
510400930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
510500930914     C     WPODIC        IFNE      *ZEROS
510600930914     C     WPODIC        ANDGT     WPOFRC
510700930914     C                   Z-ADD     WPOFRC        WPODIC
510800930914     C                   ENDIF
510900930914     C*
511000930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
511100930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
511200930914     C     WPODFC        IFGT      WPOFRC
511300930914     C                   Z-ADD     WPOFRC        WPODFC
511400930914     C                   ENDIF
511500930914     C*
511600930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
511700930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
511800930914     C     WPODIC        IFNE      *ZEROS
511900930914     C     WPODIC        ANDLT     WPOIRC
512000930914     C                   Z-ADD     WPOIRC        WPODIC
512100930914     C                   ENDIF
512200930914     C*
512300930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
512400930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
512500930914     C     WPODFC        IFLT      WPOIRC
512600930914     C                   Z-ADD     WPOIRC        WPODFC
512700930914     C                   ENDIF
512800930914     C*
512900930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
513000931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
513100930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
513200930916     C*
513300931210     C***********WPODIC    IFNE *ZEROS
513400931210     C***********WPODFC    ANDNE*ZEROS
513500931210     C*********************ADD  12        WPODFC
513600931210     C*********************ENDIF
513700930914     C     WPODIC        IFEQ      *ZEROS
513800930914     C     WPODFC        ANDNE     *ZEROS
513900931130     C                   Z-ADD     WPODFC        WPODIC
514000931210     C*********************ADD  12        WPODFC
514100930916     C                   ENDIF
514200930913     C*
514300930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
514400930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
514500930914     C* INTERNA
514600050908     c                   eval      Usacons='S'
514700930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
514800930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
514900931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
515000931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
515100940324     C                   EXSR      RO9COV
515200931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
515300930914     C* CLIENTE
515400930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
515500931210     C                   EXSR      RO9COV
515600931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
515700050908     c                   eval      Usacons=' '
515800930914     C*
515900930916     C                   ENDIF                                                  <-- 5 ---
516000930916     C                   ENDIF                                                  <-- 4 ---
516100930916     C                   ENDIF                                                  <-- 3 ---
516200930914     C                   ENDIF                                                  <-- 2 ---
516300930914     C                   ENDIF                                                  <-- 1 ---
516400930913     C*
516500930913     C                   ENDSR
516600930325     C********************************************************************
516700930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
516800930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
516900930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
517000930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
517100930325     C********************************************************************
517200930325     C     RO1CDP        BEGSR
517300930414     C*
517400931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
517500931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
517600931126     C*
517700931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
517800931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
517900150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
518000150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
518100931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
518200060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
518300930326     C*
518400930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
518500931130     C                   Z-ADD     WR1POI        WR1POO            9 0
518600930325     C*
518700930325     C* CICLO FINO A CHE:
518800930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
518900930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
519000930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
519100930406     C     WR1POO        ANDLT     WPOLIM
519200930414     C*
519300930407     C                   Z-ADD     WR1POO        WPOROU            9 0
519400930325     C                   MOVEL     'N'           WSLITT            1
519500930414     C*
519600930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
519700060420     C*
519800931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
519900150605     c     wdee          andeq     ' '
520000930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
520100930414     C                   END
520200930511     C*
520300931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
520400150608     c     wdee          andeq     ' '
520500930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
520600930511     C                   END
520700930414     C*
520800930325     C     WSLITT        IFEQ      'N'
520900930423     C                   SUB       12            WR1NOI            5 0
521000930325     C                   END
521100931130     C                   ADD       12            WR1POO            9 0
521200930414     C*
521300931223     C                   ENDDO                                                  <-- 1 ---
521400931126     C*
521500931126     C                   ELSE                                                   <-- 0 -->
521600931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
521700931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
521800931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
521900960129     C     WPOLAF        IFGT      WPODCG
522000960129     C                   Z-ADD     WPOLAF        WTM090            9 0
522100960129     C                   ELSE
522200960129     C                   Z-ADD     WPODCG        WTM090
522300960129     C                   ENDIF
522400960129     C     WPOGIF        IFGT      WTM090
522500960129     C                   Z-ADD     WPOGIF        WTM090
522600960129     C                   ENDIF
522700060420     C     WPOpaif       IFGT      WTM090
522800060420     C                   Z-ADD     WPOpaif       WTM090
522900060420     C                   ENDIF
523000960129     C*
523100960129     C     12            ADD       WTM090        WR1POO
523200960129     C                   ENDIF                                                  <-- 0 ---
523300930512     C*
523400931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
523500931202     C     WR1POO        SUB       12            WR7POI
523600931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
523700931202     C                   ADD       12            WR7POO
523800931202     C     WR7POO        IFGT      WR1POO
523900931202     C                   Z-ADD     WR7POO        WR1POO
524000931202     C                   END
524100930406     C*
524200930325     C                   ENDSR
524300930414     C********************************************************************
524400930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
524500930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
524600930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
524700930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
524800930414     C********************************************************************
524900930414     C     RO2COV        BEGSR
525000091014     c                   clear                   salvaEveL
525100091014     c                   clear                   salvaEveG
525200931130     C*
525300930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
525400930414     C                   Z-ADD     *ZEROS        WR2NOV
525500930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
525600931130     C                   Z-ADD     WR2POI        WR2POW
525700930414     C*
525800930414     C* CICLO FINO A CHE:
525900930414     C*   - DATA INIZIO SUPERATO DATA FINE
526000930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
526100931223     C     WR2POW        ANDLT     WPOLIM
526200930414     C*
526300930414     C                   Z-ADD     WR2POW        WPOROU
526400930414     C                   MOVEL     'N'           WSLITT            1
526500930414     C*
526600930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
526700930414     C*
526800930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
526900930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
527000930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
527100930414     C                   END
527200930414     C*
527300930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
527400930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
527500930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
527600930618     C                   END
527700060420     c
527800060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
527900060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
528000060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
528100060420     C                   END
528200060420     c
528300931201     C*
528400931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
528500931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
528600931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
528700931201     C                   END
528800060418     c
528900060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
529000060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
529100060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
529200060418     C                   END
529300930414     C*
529400930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
529500930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
529600930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
529700930414     C                   END
529800930511     C*
529900930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
530000930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
530100930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
530200930511     C                   END
530300930414     C*
530400930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
530500930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
530600930414     C     WSLITT        IFEQ      'N'
530700930423     C                   ADD       12            WR2NOV            5 0
530800930414     C                   END
530900931130     C                   ADD       12            WR2POW            9 0
531000930414     C*
531100931223     C                   ENDDO                                                  <-- 1 ---
531200930414     C*
531300930414     C                   ENDSR
531400941228     C********************************************************************
531500941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
531600941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
531700941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
531800941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
531900941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
532000941228     C********************************************************************
532100941228     C     RO2COP        BEGSR
532200941228     C*
532300941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
532400941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
532500941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
532600941228     C                   Z-ADD     *ZEROS        WR2NOV
532700941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
532800941228     C                   Z-ADD     WR2POI        WR2POW
532900941228     C*
533000941228     C* CICLO FINO A CHE:
533100941228     C*   - DATA INIZIO SUPERATO DATA FINE
533200941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
533300941228     C*
533400950215     C                   Z-ADD     WR2POW        WPOROU
533500941228     C                   MOVEL     'N'           WSLITT
533600941228     C* TRASFORMA IL PROGR. ORE IN DATA
533700950215     C*******              Z-ADDWR2POW    WORE
533800950215     C*******              EXSR OREDAT
533900950215     C*******              Z-ADDWDAT      W12DAT  80
534000950215     C*******              Z-ADDWRES      W12RES  30
534100941228     C*
534200941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
534300941228     C*
534400941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
534500941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
534600941228     C     WSLITT        IFEQ      'N'
534700941228     C                   ADD       12            WR2NOV            5 0
534800941228     C                   END
534900941228     C                   ADD       12            WR2POW            9 0
535000941228     C*
535100941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
535200941228     C* ENTRI IN LOOP
535300941228     C     WR2NOV        IFGT      999
535400941228     C                   LEAVE                                                  *VA A ENDDO
535500941228     C                   END
535600941228     C*
535700941228     C                   ENDDO                                                  <-- 1 ---
535800941228     C*
535900941228     C                   ENDSR
536000930510     C********************************************************************
536100930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
536200930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
536300930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
536400930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
536500050905     c*                     il num ore deve essere un multiplo di 12
536600050905     c*                     oppure 1 ora
536700930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
536800930510     C********************************************************************
536900930510     C     RO4CDP        BEGSR
537000930510     C*
537100930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
537200930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
537300050905     c                   movel     '1'           wgiro             1
537400930510     C*
537500930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
537600930510     C                   Z-ADD     WR4POI        WR4POO            9 0
537700930510     C*
537800930510     C* CICLO FINO A CHE:
537900930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
538000930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
538100930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
538200930510     C     WR4POO        ANDLT     WPOLIM
538300930510     C*
538400141106     c                   if        nosomma<>'N'
538500931130     C                   ADD       12            WR4POO            9 0
538600141106     c                   endif
538700930510     C                   Z-ADD     WR4POO        WPOROU            9 0
538800930510     C                   MOVEL     'N'           WSLITT            1
538900930510     C*
539000930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
539100050905     c
539200050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
539300930510     C     WSLITT        IFEQ      'N'
539400050905     c
539500050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
539600050905     c*  se cado in giorno non festivo
539700050905     c                   if        wr4noi=1 and wgiro='1'
539800050905     c                   sub       12            wr4poo
539900050905     c                   add       wr4noi        wr4poo
540000050905     c                   endif
540100050905     c*
540200050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
540300050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
540400050905     c*  calcolate dal pgm
540500050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
540600050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
540700050905     c*  del giorno sucessivo non festivo
540800050905     c                   if        wr4noi=1 and wgiro<>'1'
540900050905     c                   add       wr4noi        wr4poo
541000050905     c                   endif
541100050905     c
541200930510     C                   SUB       12            WR4NOI            5 0
541300930510     C                   END
541400141106     c
541500141106     c                   if        nosomma= 'N'
541600141106     C                   ADD       12            WR4POO            9 0
541700141106     c                   endif
541800930510     C*
541900050905     c                   eval      wgiro='2'
542000931223     C                   ENDDO                                                  <-- 1 ---
542100930510     C*
542200930510     C                   ENDSR
542300931130     C********************************************************************
542400931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
542500931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
542600931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
542700931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
542800931130     C********************************************************************
542900931130     C     RO5COV        BEGSR
543000931130     C*
543100931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
543200060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
543300060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
543400060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
543500931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
543600931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
543700931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
543800931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
543900931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
544000931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
544100931130     C                   Z-ADD     *ZEROS        WR5NOV
544200940714     C*
544300940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
544400940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
544500931207     C*
544600931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
544700940908     C     WIC           IFEQ      'I'
544800090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
544900090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
545000931207     C                   ELSE
545100090827     C*****              Z-ADD     D98TCC        WPOTCT
545200090827     C                   Z-ADD     w99TCC        WPOTCT
545300931207     C                   END
545400931130     C*
545500931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
545600060420     C                   clear                   COV
545700931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
545800931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
545900931207     C                   Z-ADD     2             CO                3 0
546000931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
546100931207     C                   EXSR      RO5SUB
546200981102     C                   DO        99            IC                3 0
546300931207     C     LAF(IC)       IFNE      *ZEROS
546400931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
546500931207     C                   EXSR      RO5SUB
546600931207     C                   ENDIF
546700931207     C                   ENDDO
546800060420     c
546900060420     C                   DO        99            IC                3 0
547000060420     C     paif(IC)      IFNE      *ZEROS
547100060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
547200060420     C                   EXSR      RO5SUB
547300060420     C                   ENDIF
547400060420     C                   ENDDO
547500931207     C* ORDINA LA SCHIERA COV
547600931207     C                   SORTA     COV
547700931130     C*
547800931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
547900931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
548000981102     C                   DO        98            IC                             --- 1 -->
548100931207     C     IC            ADD       1             I1C               3 0
548200931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
548300931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
548400931207     C     COV(I1C)      OREQ      *ZEROS
548500931207     C                   ELSE
548600931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
548700931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
548800931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
548900931130     C                   EXSR      RO2COV
549000931207     C                   ADD       WR2NOV        WR5NOV            9 0
549100931209     C* SE NON SONO IN DATA CONSEGNA
549200931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
549300931207     C     WR5NOV        IFLT      WPOTCT
549400931209     C     COV(I1C)      ANDNE     WPODCM
5495009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5496009709111009 C     WPOTCT        SUB       12            WR5NOV
549700931130     C                   END
549800931207     C                   ENDIF                                                  <-- 2 ---
549900931207     C                   ENDDO                                                  <-- 1 ---
550000940714     C                   ENDIF                                                  <-- 0 ---
550100931130     C*
550200931130     C                   ENDSR
550300931207     C********************************************************************
550400931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
550500931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
550600931207     C********************************************************************
550700931207     C     RO5SUB        BEGSR
550800931207     C*
550900931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
551000931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
551100931207     C     WR5COV        ANDLT     WR5POF
551200931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
551300931207     C                   MOVEL     'N'           WR5PRE            1
551400981102     C*---                 DO   30        I
551500981102     C                   DO        99            I
551600931207     C     WR5COV        IFEQ      COV(I)
551700931207     C                   MOVEL     'S'           WR5PRE
551800931207     C                   LEAVE
551900931207     C                   ENDIF
552000931207     C                   ENDDO
552100931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
552200931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
552300931207     C                   ADD       1             CO                3 0
552400931207     C                   Z-ADD     WR5COV        COV(CO)
552500931207     C                   ENDIF                                                  <-- 2 ---
552600931207     C                   ENDIF                                                  <-- 1 ---
552700931207     C*
552800931207     C                   ENDSR
552900931201     C********************************************************************
553000060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
553100931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
553200931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
553300931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
553400931201     C********************************************************************
553500931201     C     RO6CDT        BEGSR
553600931201     C*
553700931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
553800931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
553900931201     C*
554000931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
554100931201     C                   Z-ADD     WR6POI        WR6POO            9 0
554200931201     C*
554300931201     C* CICLO FINO A CHE:
554400931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
554500931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
554600931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
554700931201     C     WR6POO        ANDLT     WPOLIM
554800931201     C*
554900931201     C                   Z-ADD     WR6POO        WPOROU            9 0
555000931201     C                   MOVEL     'N'           WSLITT            1
555100931201     C*
555200931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
555300931201     C*
555400931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
555500150605     c     wdee          andeq     ' '
555600931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
555700931201     C                   END
555800931201     C*
555900931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
556000150608     c     wdee          andeq     ' '
556100931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
556200931201     C                   END
556300931201     C*
556400931201     C     WSLITT        IFEQ      'N'
556500931201     C                   SUB       12            WR6NOI            5 0
556600931201     C                   END
556700931201     C                   ADD       12            WR6POO            9 0
556800931201     C*
556900931223     C                   ENDDO                                                  <-- 1 ---
557000931201     C*
557100931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
557200931202     C     WR6POO        SUB       12            WR7POI
557300931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
557400931207     C                   ADD       12            WR7POO
557500931202     C     WR7POO        IFGT      WR6POO
557600931201     C                   Z-ADD     WR7POO        WR6POO
557700931202     C                   END
557800931201     C*
557900931201     C                   ENDSR
558000931201     C********************************************************************
558100060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
558200060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
558300060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
558400060407     C*            INPUT:  -WR7POI (PROGR.ORE)
558500931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
558600931201     C********************************************************************
558700931201     C     RO7TCF        BEGSR
558800060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
558900060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
559000060407     C                   MOVEL     'N'           WSLITT
559100931201     C*
559200931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
559300931201     C                   Z-ADD     WR7POI        WR7POO            9 0
559400931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
559500931201     C* COINCIDA CON UN TURNO DI CHIUSURA
559600060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
559700931201     C     D98GC2        ANDEQ     *BLANKS
559800060407   x1C                   ELSE                                                   <-- 1 -->
559900931214     C*
560000931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
560100931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
560200931214     C*
560300931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
560400931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
560500931223     C     WPOROU        ANDLT     WPOLIM
560600931214     C* MEMORIZZO INIZIO E FINE RANGE
560700931202     C     WPOTI         IFEQ      *ZEROS
560800931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
560900931202     C                   END
561000931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
561100931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
561200931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
561300931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
561400931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
561500931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
561600931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
561700931201     C                   MOVEL     'N'           WSLITT
561800931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
561900060407     c* Se non trovat, controllo se c'e' evento part che giustifica
562000060407     c                   if        wslitt='N'
562100060407     c                   exsr      datcepg
562200060407     c                   endif
562300060407     c
562400931201     C                   ENDDO                                                  <-- 2 ---
562500931214     C*
562600931201     C                   Z-ADD     WPOROU        WR7POO
562700931214     C*
562800931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
562900931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
563000931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
563100931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
563200931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
563300931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
563400931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
563500931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
563600931209     C                   ELSE                                                   <-- 3 -->
563700931209     C     WIC           IFEQ      'I'
563800931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
563900931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
564000931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
564100931201     C                   ELSE
564200931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
564300931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
564400931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
564500931201     C                   ENDIF
564600931209     C                   ENDIF                                                  <-- 3 ---
564700931209     C                   ENDIF                                                  <-- 2 ---
564800931214     C*
564900060407    1C                   ENDIF                                                  <-- 1 ---
565000060407     c
565100060407     c* Se non trovato turno di chiusura, verifico se c'e'
565200060407     c*  evento particolare che GIUSTIFICA
565300060407     c                   if        wpoti=0
565400060418     C                   EXSR      RO7CEPG
565500060407     c                   endif
565600931201     C*
565700931201     C                   ENDSR
565800060406     C********************************************************************
565900060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
566000060406     c*                        che giustifica
566100060407     C*            INPUT:-WR7POI (PROGR.ORE)
566200060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
566300060406     C********************************************************************
566400060407     C     RO7CEPG       BEGSR
566500060406     C*
566600060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
566700060407     C                   Z-ADD     WR7POI        WR7POO            9 0
566800060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
566900060406     C* coincide
567000060406    1C                   if        wevpag='S'                                   <-- 1 -->
567100060406     C*
567200060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
567300060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
567400060406     C                   MOVEL     'N'           WSLITT
567500060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
567600060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
567700060406     C*
567800060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
567900060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
568000060406     C     WPOROU        ANDLT     WPOLIM
568100060406     C* MEMORIZZO INIZIO E FINE RANGE
568200060406    3C     WPOTI         IFEQ      *ZEROS
568300060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
568400060406    3C                   END
568500060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
568600060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
568700060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
568800060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
568900060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
569000060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
569100060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
569200060406     C                   MOVEL     'N'           WSLITT
569300060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
569400060406     C* Se non trovato,
569500060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
569600060406    3c                   if        wslitt='N'
569700060406     c                   exsr      DATTUC
569800060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
569900060406    2C                   ENDDO                                                  <-- 2 ---
570000060406     C*
570100060407     C                   Z-ADD     WPOROU        WR7POO
570200060406     C*
570300060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
570400060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
570500060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
570600060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
570700060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
570800060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
570900060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
571000060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
571100060406   x3C                   ELSE                                                   <-- 3 -->
571200060406    4C     WIC           IFEQ      'I'
571300060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
571400060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
571500060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
571600060406   x4C                   ELSE
571700060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
571800060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
571900060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
572000060406    4C                   ENDIF
572100060406    3C                   ENDIF                                                  <-- 3 ---
572200060406    2C                   ENDIF                                                  <-- 2 ---
572300060406     C*
572400060406    1C                   ENDIF                                                  <-- 1 ---
572500060406     C*
572600060406     C                   ENDSR
572700931207     C********************************************************************
572800931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
572900931207     C*            INPUT:  -WR8POI (PROGR.ORE)
573000931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
573100931207     C********************************************************************
573200931207     C     RO8FES        BEGSR
573300130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
573400130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
573500931207     C*
573600931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
573700130411     C     WR8POI        SUB       12            WR8POO            9 0
573800931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
573900931207     C                   MOVEL     'N'           WSLITT
574000130411     c
574100130411     c* Non salto i festivi ???
574200130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
574300130411     c
574400931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
574500130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
574600130411     C     WPOROU        ANDGT     WPODSP
574700130411     C                   SUB       12            WPOROU
574800130411     C                   MOVEL     'N'           WSLITT
574900130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
575000130411     C                   ENDDO                                                  <-- 2 ---
575100130411     c
575200130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
575300130411     C     D98GC2        ANDEQ     *BLANKS
575400130411   x1C                   ELSE                                                   <-- 1 -->
575500130411     c* eseguo controllo per i turni di chiusura
575600130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
575700130411     C*
575800130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
575900130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
576000130411     C     WPOROU        ANDgT     WPOdsp
576100130411     C* MEMORIZZO INIZIO E FINE RANGE
576200130411     C     WPOTf         IFEQ      *ZEROS
576300130411     C                   z-add     wporou        WPOTf
576400130411     C                   END
576500140826     C     WPOROU        SUB       wnoadd        WPOROU
576600130411     c
576700130411     C                   MOVEL     'N'           WSLITT
576800130411     c                   exsr      dattuc
576900130411     c                   if        wslitt='N'
577000130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
577100140826     c* Se slitta per festivo --> tolgo sempre 12 ore
577200140826     c                   z-add     12            WNOADD
577300130411     c                   endif
577400130411     C* SLITTA
577500140826     C     wporou        add       12            WPOTi
577600130411    2C                   ENDDO                                                  <-- 2 ---
577700130411     c
577800130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
577900130411     c
578000130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
578100130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
578200130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
578300130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
578400130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
578500130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
578600130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
578700130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
578800130411   x3C                   ELSE                                                   <-- 3 -->
578900130411    4C     WIC           IFEQ      'I'
579000130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
579100130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
579200130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
579300130411   x4C                   ELSE
579400130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
579500130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
579600130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
579700130411    4C                   ENDIF
579800130411    3C                   ENDIF                                                  <-- 3 ---
579900130411    2C                   ENDIF                                                  <-- 3 ---
580000130411     C*
580100130411    1C                   ENDIF                                                  <-- 1 ---
580200130411
580300130411     C**   WPOROU        ADD       12            WR8POO
580400130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
580500130411     c                   z-add     wr8poi        wr8poo
580600931207     C*
580700931207     C                   ENDSR
580800931210     C********************************************************************
580900931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
581000940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
581100931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
581200931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
581300931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
581400931210     C********************************************************************
581500931210     C     RO9COV        BEGSR
581600931210     C*
581700931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
581800931210     C                   Z-ADD     *ZEROS        WR9NOV
581900931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
582000931210     C                   Z-ADD     WR9POI        WR9POW
582100931210     C* CICLO FINO A CHE:
582200931210     C*   - DATA INIZIO SUPERATO DATA FINE
582300931223     C*   - RAGGIUNTO LIMITE MASSIMO
582400931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
582500931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
582600931223     C     WR9POW        ANDLT     WPOLIM
582700931210     C*
582800931210     C                   Z-ADD     WR9POW        WPOROU
582900931210     C                   MOVEL     'N'           WSLITT            1
583000940324     C* FESTIVI
583100931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
583200940324     C* GIACENZA
583300931214     C     WSLITT        IFEQ      'N'
583400941229£$   C     WPOAGC        IFNE      *ZEROS
583500940324£$   C     WPOCGC        ORNE      *ZEROS
583600941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
583700940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
583800940324£$   C                   MOVEL     'S'           WSLITT
583900940324£$   C                   END
584000940324£$   C                   END
584100940324£$   C                   END
584200960129     C* EVENTI GIACENZA
584300960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
584400960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
584500960129     C                   Z-ADD     *ZEROS        G                 3 0
584600960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
584700960129     C     G             ANDLT     IG
584800960129     C                   ADD       1             G
584900960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
585000960209     C     WPOROU        ANDLE     GCF(G)
585100960129     C                   MOVEL     'S'           WSLITT
585200960129     C                   ENDIF                                                  <-- 3 ---
585300960129     C                   ENDDO                                                  <-- 2 ---
585400960129     C                   ENDIF                                                  <-- 1 ---
585500960129     C                   ENDIF                                                  <-- 0 ---
585600940324     C* LASCIATO AVVISO
585700931214     C     WSLITT        IFEQ      'N'
585800941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
585900940324£$   C                   Z-ADD     *ZEROS        L                 3 0
586000940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
586100940324£$   C     L             ANDLT     IL
586200940324£$   C                   ADD       1             L
586300941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
586400941229£$   C     WPOROU        ANDLE     LCF(L)
586500050520     c     LCon(L)       andeq     'S'
586600940324£$   C                   MOVEL     'S'           WSLITT
586700940324£$   C                   END                                                    <-- 3 ---
586800940324£$   C                   ENDDO                                                  <-- 2 ---
586900940324£$   C                   END                                                    <-- 1 ---
587000931210     C                   END
587100940324     C* TURNO CHIUSURA
587200140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
587300140826     c*  di sistemare le ore da sommare 12 o 24...
587400931214     C     WSLITT        IFEQ      'N'
587500931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
587600931210     C                   END
587700940324     C* DATA CONSEGNA RICHIESTA
587800050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
587900050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
588000050907     c
588100931214     C     WSLITT        IFEQ      'N'
588200050907     c                   add       1             wporou
588300940324£$   C     WPOCRI        IFNE      *ZEROS
588400940324£$   C     WPOROU        IFLT      WPOCRC
588500940324£$   C                   MOVEL     'S'           WSLITT
588600940324£$   C                   END
588700940324£4   C                   END
588800050907     c                   sub       1             wporou
588900931210     C                   END
589000931210     C*
589100931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
589200931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
589300931210     C     WSLITT        IFEQ      'N'
589400931210     C                   ADD       12            WR9NOV            5 0
589500931210     C                   END
589600931210     C                   ADD       12            WR9POW            9 0
589700931210     C*
589800931223     C                   ENDDO                                                  <-- 1 ---
589900931210     C*
590000931210     C                   ENDSR
590100940628%%%  C********************************************************************
590200940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
590300940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
590400940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
590500940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
590600940628%%%  C********************************************************************
590700940628%%%  C     RO0GES        BEGSR
590800940628%%%  C*
590900940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
591000940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
591100940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
591200940628%%%  C*
591300940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
591400940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
591500940628%%%  C*
591600940628%%%  C* CICLO FINO A CHE:
591700940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
591800940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
591900940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
592000050922%%%  C     WR0POO        ANDGe     WPODSP
592100940628%%%  C*
592200940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
592300940628%%%  C                   MOVEL     'N'           WSLITT            1
592400940628%%%  C*
592500940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
592600940628%%%  C*
592700940628%%%  C     WSLITT        IFEQ      'N'
592800940628%%%  C                   SUB       12            WR0NOI            5 0
592900940628%%%  C                   END
593000940628%%%  C                   SUB       12            WR0POO            9 0
593100940628%%%  C*
593200940628%%%  C                   ENDDO                                                  <-- 1 ---
593300940628%%%  C*
593400940628%%%  C                   ADD       12            WR0POO            9 0
593500940628%%%  C*
593600940628%%%  C                   ENDSR
593700960911     C********************************************************************
593800960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
593900960911     C*            INPUT:  -W10POI (PROGR.ORE)
594000960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
594100960911     C********************************************************************
594200960911     C     R10FES        BEGSR
594300960911     C*
594400960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
594500960911     C                   Z-ADD     W10POI        W10POO            9 0
594600960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
594700960911     C                   MOVEL     'N'           WSLITT
594800960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
594900960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
595000960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
595100960911     C                   ADD       12            WPOROU
595200960911     C                   MOVEL     'N'           WSLITT
595300960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
595400960911     C                   ENDDO                                                  <-- 2 ---
595500960911     C                   Z-ADD     WPOROU        W10POO
595600960911     C*
595700960911     C                   ENDSR
595800930325     C********************************************************************
595900930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
596000930325     C********************************************************************
596100930325     C     CHKGIA        BEGSR
596200930406     C*
596300960129     C* APERTURA/CHIUSURA GIACENZA
596400930326     C     WPODAG        IFNE      *ZEROS
596500930406     C     WPODCG        ORNE      *ZEROS
596600930407     C     WPOROU        IFGE      WPODAG
596700930407     C     WPOROU        ANDLE     WPODCG
596800930325     C                   MOVEL     'S'           WSLITT
596900930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
597000930325     C                   END
597100930326     C                   END
597200960129     C*
597300960129     C* EVENTI GIACENZA
597400960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
597500960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
597600960129     C                   Z-ADD     *ZEROS        G                 3 0
597700960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
597800960129     C     G             ANDLT     IG
597900960129     C                   ADD       1             G
598000960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
598100960129     C     WPOROU        ANDLE     GIF(G)
598200091009     c
598300091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
598400091009     c*  l'orario dell'evento
598500091009     c                   select
598600091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
598700091009     c* Se orario entro le 10.30  va bene
598800091014     c*  solo la prima volta per evento cioè quando controllo
598900091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
599000091014     c*  e non devo dare errore
599100091014     C                   MOVEL     'OSC  '       WR3TAB
599200091014     C                   eval      WR3HHI=evo(gip(G))
599300091014     C                   EXSR      RO3ORE
599400091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
599500091014     c*  in ritardo come per "E")
599600091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
599700091014     c                              wr3matpom='P'
599800091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
599900091009     C                   MOVEL     'S'           WSLITT
600000091009     c                   endif
600100091014     c                   eval      SalvaEveG=gip(G)
600200091009     c
600300091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
600400091009     c* Se orario entro le 10.30  va bene
600500091014     c*  solo la prima volta per evento cioè quando controllo
600600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
600700091014     c*  e non devo dare errore
600800091014     C                   MOVEL     'OSC  '       WR3TAB
600900091014     C                   eval      WR3HHI=evo(gip(G))
601000091014     C                   EXSR      RO3ORE
601100091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
601200091014     c*  in ritardo come per "E")
601300091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
601400091014     c                              wr3matpom='P'
601500091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
601600091009     C                   MOVEL     'S'           WSLITT
601700091009     c                   endif
601800091014     c                   eval      SalvaEveG=gip(G)
601900091009     c
602000091009     c                   other
602100960129     C                   MOVEL     'S'           WSLITT
602200960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
602300091009     c                   endsl
602400091009
602500960129     C                   ENDIF                                                  <-- 3 ---
602600960129     C                   ENDDO                                                  <-- 2 ---
602700960129     C                   ENDIF                                                  <-- 1 ---
602800960129     C                   ENDIF                                                  <-- 0 ---
602900930406     C*
603000930325     C                   ENDSR
603100060420     C********************************************************************
603200930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
603300930325     C********************************************************************
603400930325     C     CHKLAV        BEGSR
603500930325     C*
603600941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
603700930421     C                   Z-ADD     *ZEROS        L                 3 0
603800930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
603900930421     C     L             ANDLT     IL
604000930421     C                   ADD       1             L
604100930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
604200930421     C     WPOROU        ANDLE     LAF(L)
604300090928     c
604400090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
604500091009     c*  l'orario dell'evento
604600091009     c                   select
604700091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
604800091009     c* Se orario entro le 10.30  va bene
604900091014     c*  solo la prima volta per evento cioè quando controllo
605000091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
605100091014     c*  e non devo dare errore
605200091014     C                   MOVEL     'OSC  '       WR3TAB
605300091014     C                   eval      WR3HHI=evo(lap(L))
605400091014     C                   EXSR      RO3ORE
605500091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
605600091014     c*  in ritardo come per "E")
605700091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
605800091014     c                              wr3matpom='P'
605900091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
606000091009     C                   MOVEL     'S'           WSLITT
606100091009     c                   endif
606200091014     c                   eval      SalvaEveL=lap(L)
606300091009     c
606400091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
606500091009     c* Se orario entro le 10.30  va bene
606600091014     c*  solo la prima volta per evento cioè quando controllo
606700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
606800091014     c*  e non devo dare errore
606900091014     C                   MOVEL     'OSC  '       WR3TAB
607000091014     C                   eval      WR3HHI=evo(lap(L))
607100091014     C                   EXSR      RO3ORE
607200091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
607300091014     c*  in ritardo come per "E")
607400091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
607500091014     c                              wr3matpom='P'
607600091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
607700091009     C                   MOVEL     'S'           WSLITT
607800091009     c                   endif
607900091014     c                   eval      SalvaEveL=lap(L)
608000090928     c
608100091009     c                   other
608200930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
608300091009     C                   MOVEL     'S'           WSLITT
608400091009     c                   endsl
608500090928     c
608600930421     C                   END                                                    <-- 3 ---
608700931223     C                   ENDDO                                                  <-- 2 ---
608800930421     C                   END                                                    <-- 1 ---
608900930325     C*
609000930325     C                   ENDSR
609100060420     C********************************************************************
609200060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
609300060420     c*                   che Allungano
609400060420     C********************************************************************
609500060420     C     CHKCEPA       BEGSR
609600060420     C*
609700060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
609800060420     C                   Z-ADD     *ZEROS        p                 3 0
609900060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
610000060420     C     p             ANDLT     IpA
610100060420     C                   ADD       1             P
610200060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
610300060420     C     WPOROU        ANDLE     paif(P)
610400060420     C                   MOVEL     'S'           WSLITT
610500060420     C                   ADD       12            W98EA
610600060420     C                   END                                                    <-- 3 ---
610700060420     C                   ENDDO                                                  <-- 2 ---
610800060420     C                   END                                                    <-- 1 ---
610900060420     C*
611000060420     C                   ENDSR
611100930414     C********************************************************************
611200930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
611300930414     C********************************************************************
611400930414     C     CHKCNT        BEGSR
611500930511     C*
611600930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
611700930611     C     WIC           IFEQ      'I'                                          *INTERNO
611800930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
611900930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
612000930611     C                   ELSE
612100930611     C     WPOROU        IFGE      WPOAII
612200930611     C     WPOROU        ANDLE     WPOAFI
612300930611     C                   MOVEL     'S'           WSLITT
612400930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
612500930611     C                   END
612600930611     C                   END
612700930611     C                   ELSE                                                   *CLIENTE
612800930611     C     WPOAIC        IFEQ      *ZEROS
612900930611     C     WPOAFC        ANDEQ     *ZEROS
613000930611     C                   ELSE
613100930611     C     WPOROU        IFGE      WPOAIC
613200930611     C     WPOROU        ANDLE     WPOAFC
613300930611     C                   MOVEL     'S'           WSLITT
613400930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
613500930611     C                   END
613600930611     C                   END
613700930611     C                   END
613800930414     C*
613900930414     C                   ENDSR
614000930511     C********************************************************************
614100930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
614200930511     C********************************************************************
614300930511     C     CHKFED        BEGSR
614400930511     C*
614500930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
614600930611     C     WIC           IFEQ      'I'                                          *INTERNO
614700930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
614800930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
614900930611     C                   ELSE
615000930611     C     WPOROU        IFGE      WPOFII
615100930611     C     WPOROU        ANDLE     WPOFFI
615200930611     C                   MOVEL     'S'           WSLITT
615300930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
615400930611     C                   END
615500930611     C                   END
615600930611     C                   ELSE                                                   *CLIENTE
615700930611     C     WPOFIC        IFEQ      *ZEROS
615800930611     C     WPOFFC        ANDEQ     *ZEROS
615900930611     C                   ELSE
616000930611     C     WPOROU        IFGE      WPOFIC
616100930611     C     WPOROU        ANDLE     WPOFFC
616200930611     C                   MOVEL     'S'           WSLITT
616300930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
616400930611     C                   END
616500930611     C                   END
616600930611     C                   END
616700930511     C*
616800930511     C                   ENDSR
616900930325     C********************************************************************
617000930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
617100930325     C********************************************************************
617200930325     C     CHKFES        BEGSR
617300060526     c* se già passata la data, non controllo wporou
617400060526     c                   if        wpodata>0
617500060526     c                   z-add     wpodata       w12dat
617600060526     c                   clear                   w12res
617700060526     c                   else
617800931130     C*
617900931130     C* TRASFORMA IL  PROGR.ORE IN DATA
618000931130     C                   Z-ADD     WPOROU        WORE
618100931130     C                   EXSR      OREDAT
618200941227     C                   Z-ADD     WDAT          W12DAT            8 0
618300931130     C                   Z-ADD     WRES          W12RES            3 0
618400060526     c                   endif
618500930325     C*
618600941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
618700930325     C* E' FESTIVO
618800090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
618900090108     C**                 Z-ADD     D98LNP        CLNTFP
619000090108     C**                 Z-ADD     *ZEROS        CLNTFA
619100090108     C**                 ENDIF
619200090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
619300090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
619400090108     c**   è possibile rendere festivo in terminal per un altro terminal
619500090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
619600930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
619700090108     C     WTC           oreq      'P'                                          *TRAZIONE
619800941227     C                   Z-ADD     D98TFP        CLNTFP
619900941227     C                   Z-ADD     D98TFA        CLNTFA
620000941228     C                   ENDIF
620100941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
620200941227     C                   Z-ADD     *ZEROS        CLNTFP
620300941227     C                   Z-ADD     D98LNA        CLNTFA
620400941228     C                   ENDIF
620500941227     C                   Z-ADD     W12DAT        DSDAT8
620600941227     C                   Z-ADD     DSAAS         CLNANN
620700941227     C                   Z-ADD     DSMES         CLNMES
620800930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
620900930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
621000930421     C                   Z-ADD     DSGIO         G
621100930415     C     W12RES        IFEQ      *ZEROS
621200941227     C                   MOVEL     CLNMAT        GFEDS
621300930415     C                   ELSE
621400941227     C                   MOVEL     CLNPOM        GFEDS
621500930415     C                   END
621600930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
621700930405     C                   MOVEL     *BLANKS       WC1Y
621800930421     C                   MOVEL     GFE(G)        WC1Y              8
621900930421     C                   Z-ADD     1             I
622000930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
622100930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
622200930405     C     WIC           IFEQ      'I'
622300930421     C     I1Y(I)        ANDEQ     'S'
622400930405     C                   MOVEL     'S'           WSLITT
622500930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
622600930405     C                   END
622700930405     C     WIC           IFEQ      'C'
622800930421     C     E1Y(I)        ANDEQ     'S'
622900930405     C                   MOVEL     'S'           WSLITT
623000930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
623100930405     C                   END
623200970204     C     WIC           IFEQ      'X'
623300970204     C     E1Y(I)        ANDEQ     'S'
623400970204     C                   MOVEL     'S'           WSLITT
623500970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
623600970204     C                   END
623700930405     C                   ELSE                                                   <-- 3 -->
623800930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
623900930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
624000930405     C                   END                                                    <-- 3 ---
624100930405     C                   END                                                    <-- 2 ---
624200930405     C                   END                                                    <-- 1 ---
624300930325     C*
624400060526     c                   clear                   wpodata
624500930325     C                   ENDSR
624600931201     C********************************************************************
624700931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
624800931201     C********************************************************************
624900931201     C     CHKTUC        BEGSR
625000931201     C*
625100931201     C     WIC           IFEQ      'I'                                          --- 1 -->
625200931201     C                   DO        TI            T                 3 0          --- 2 -->
625300931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
625400931201     C     WPOROU        ANDLE     TIF(T)
625500931201     C                   MOVEL     'S'           WSLITT
625600931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
625700931201     C                   LEAVE                                                  *VA A FINE DO
625800931201     C                   ENDIF                                                  <-- 3 ---
625900931201     C                   ENDDO                                                  <-- 2 ---
626000931201     C                   ELSE                                                   <-- 1 -->
626100931201     C                   DO        TC            T                 3 0          --- 2 -->
626200931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
626300931201     C     WPOROU        ANDLE     TCF(T)
626400931201     C                   MOVEL     'S'           WSLITT
626500931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
626600931201     C                   LEAVE                                                  *VA A FINE DO
626700931201     C                   ENDIF                                                  <-- 3 ---
626800931201     C                   ENDDO                                                  <-- 2 ---
626900931201     C                   ENDIF                                                  <-- 1 ---
627000931201     C*
627100931201     C                   ENDSR
627200060418     C********************************************************************
627300060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
627400060418     c*          - eventi particolari che giustificano
627500060418     C********************************************************************
627600060418     C     CHKCEPG       BEGSR
627700060418     C*
627800060418     C     WIC           IFEQ      'I'                                          --- 1 -->
627900060420     C                   DO        PGI           p                 3 0          --- 2 -->
628000060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
628100060420     C     WPOROU        ANDLE     PGIF(p)
628200060418     C                   MOVEL     'S'           WSLITT
628300060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
628400060418     C                   LEAVE                                                  *VA A FINE DO
628500060418     C                   ENDIF                                                  <-- 3 ---
628600060418     C                   ENDDO                                                  <-- 2 ---
628700060418     C                   ELSE                                                   <-- 1 -->
628800060420     C                   DO        PGC           p                 3 0          --- 2 -->
628900060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
629000060420     C     WPOROU        ANDLE     PGCF(p)
629100060418     C                   MOVEL     'S'           WSLITT
629200060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
629300060418     C                   LEAVE                                                  *VA A FINE DO
629400060418     C                   ENDIF                                                  <-- 3 ---
629500060418     C                   ENDDO                                                  <-- 2 ---
629600060418     C                   ENDIF                                                  <-- 1 ---
629700060418     C*
629800060418     C                   ENDSR
629900930512     C********************************************************************
630000931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
630100930512     C********************************************************************
630200931130     C     DATTUC        BEGSR
630300931214     C*
630400931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
630500931214     C                   Z-ADD     12            WNOADD            9 0
630600930325     C*
630700931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
630800931125     C     D98GC2        ORNE      *BLANKS
630900931130     C*
631000931213     C                   Z-ADD     WPOROU        WORE
631100931130     C                   EXSR      OREDAT                                       *ORE->DATA
631200931130     C                   Z-ADD     WDAT          W12DAT
631300931130     C                   Z-ADD     WRES          W12RES
631400930405     C*
631500941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
631600930405     C* DELLA SETTIMANA
631700941227     C                   Z-ADD     W12DAT        DSDAT8
631800941227     C                   Z-ADD     DSAAS         CLNANN
631900941227     C                   Z-ADD     DSMES         CLNMES
632000941227     C                   Z-ADD     *ZEROS        CLNTFP
632100941227     C                   Z-ADD     *ZEROS        CLNTFA
632200930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
632300930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
632400941227     C                   MOVEL     CLNMAT        GSEDS
632500960129     C                   Z-ADD     DSGIO         GI                6 0
632600960129     C                   MOVEL     GSE(GI)       WGSE              1
632700140911     c
632800140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
632900140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
633000140911     c                   if        wtuctut='S'
633100140911     c                   eval      wtuctut='P'
633200140911     c                   endif
633300930326     C*
633400931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
633500931213     C                   DO        4             IT                3 0          --- 3 -->
633600931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
633700931213     C                   MOVEL     TUC(IT)       W93GSE            1
633800931213     C                   MOVE      TUC(IT)       W93GMP            1
633900930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
634000930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
634100931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
634200930512     C     W93GSE        OREQ      ' '
634300140911     c     WTUCTUT       andeq     ' '
634400140911     c
634500930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
634600931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
634700930401     C     W93GMP        OREQ      ' '
634800930325     C                   MOVEL     'S'           WSLITT
634900931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
635000931213     C                   ELSE                                                   <-- 6 -->
635100001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
635200001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
635300001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
635400050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
635500050908     C     Usacons       IFNE      'S'                                          --- 7 -->
635600141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
635700141107     c*    orari giorno per giorno
635800141107     c                   if        OOR2FCSGL<>' '
635900141108     c                             and w12dat<>sav_12dat
636000141107     c                   eval      or_datarif=w12dat
636100141107     c                   eval      or_DDC    =w12dat
636200141107     c                   exsr      Reper_tempi
636300141108     c                   eval      sav_12dat=w12dat
636400141107     c                   endif
636500141107     c
636600930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
636700930325     C     W93GMP        IFEQ      'M'
636800930326     C     W12RES        ANDEQ     *ZEROS
636900141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
637000141111     c                             or OOR2STfS =0
637100141105     c                             or (tuc(IT)<>gc1part and
637200141105     c                              tuc(IT)<>gc2part)
637300140825     C                   MOVEL     'S'           WSLITT
637400140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
637500140911     c* slitto solo una volta  se chiuso tutti " M"
637600140911     c                   if        W93GSE=' '
637700140911     c                   eval      wTUCTUT='S'
637800140911     c                   endif
637900140911     c
638000140825     c                   endif
638100930325     C                   END
638200930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
638300930325     C     W93GMP        IFEQ      'P'
638400930326     C     W12RES        ANDNE     *ZEROS
638500140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
638600140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
638700141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
638800141111     c                             or OOR2STiS =0
638900141105     c                             or (tuc(IT)<>gc1part and
639000141105     c                              tuc(IT)<>gc2part)
639100930325     C                   MOVEL     'S'           WSLITT
639200140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
639300140911     c* slitto solo una volta  se chiuso tutti " P"
639400140911     c                   if        W93GSE=' '
639500140911     c                   eval      wTUCTUT='S'
639600140911     c                   endif
639700140825     c                   endif
639800140825     c
639900931213     C                   ENDIF
640000001128     C                   ENDIF                                                  <-- 7 ---
640100931213     C                   ENDIF                                                  <-- 6 ---
640200931213     C                   ENDIF                                                  <-- 5 ---
640300931213     C                   ENDIF                                                  <-- 4 ---
640400931213     C                   ENDDO                                                  <-- 3 ---
640500930405     C*
640600931213     C                   ENDIF                                                  <-- 2 ---
640700931213     C                   ENDIF                                                  <-- 1 ---
640800140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
640900140911     c                   if        wtuctut='P'  and wslitt='N'
641000140911     c                   clear                   wtuctut
641100140911     c                   endif
641200140911     c
641300930325     C                   ENDSR
641400060406     C********************************************************************
641500060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
641600060406     C********************************************************************
641700060406     C     DATCEPG       BEGSR
641800060406     C*
641900060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
642000060406     C                   Z-ADD     12            WNOADD            9 0
642100060406     C*
642200060406    1C                   if        wevpag='S'
642300060406     C*
642400060406     C                   Z-ADD     WPOROU        WORE
642500060406     C                   EXSR      OREDAT                                       *ORE->DATA
642600060406     C                   Z-ADD     WDAT          W12DAT
642700060406     C                   Z-ADD     WRES          W12RES
642800060418     c                   z-add     1             P
642900060406     c* Verifico se nella data esiste un evento Part che giustifica
643000060418     c     w12dat        lookup    pvd(P)                                 30
643100060418    2c                   dow       *in30 and pvf(P)<>'G'
643200060418     c                   add       1             P
643300060420     c     w12dat        lookup    pvd(P)                                 30
643400060406    2c                   enddo
643500060406     c
643600060406     c* 30 on --> TROVATO !!
643700060406    2c                   if        *in30
643800060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
643900060406     C                   MOVEL     'S'           WSLITT
644000060406     c                   z-add     24            wnoadd
644100060406    3C                   ENDIF
644200060406     C*
644300060406    2C                   ENDIF                                                  <-- 2 ---
644400060406    1C                   ENDIF                                                  <-- 1 ---
644500060406     C*
644600060406     C                   ENDSR
644700931210     C********************************************************************
644800931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
644900931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
645000931210     C********************************************************************
645100050805     C***  CHKDCR        BEGSR
645200931210     C*
645300050805     C***  WPOCRI        IFNE      *ZEROS
645400050805     C***  WPOROU        IFGE      WPOCRI
645500050805     C***  WPOROU        ANDLT     WPOCRF
645600050805     C***                ELSE
645700050805     C***                MOVEL     'S'           WSLITT
645800050805     C***                END
645900050805     C***                END
646000931210     C*
646100050805     C***                ENDSR
646200140822     C********************************************************************
646300140822     C*  Reperimento tempi di consegna per decidere se slittare
646400140822     C********************************************************************
646500140822     C     Reper_tempi   BEGSR
646600140822
646700140822     c                   clear                   trulorSds
646800140825     c                   clear                   trulor2ds
646900140822     c                   eval       IOREtfp = d98tfp
647000140822     c                   eval       IOREtfa = d98tfa
647100140825     c                   eval       IOREdti = d98dti
647200140825     c                   eval       IOREhti = d98hti
647300140825     c                   eval       IOREdcr = s98dcr
647400140825     c                   eval       IOREhcr = s98hcr
647500140825     c                   eval       IOREtcr = s98tcr
647600140825     c                   eval       IOREDEI = d98dei
647700140825     c                   eval       IOREOEI = d98oei
647800140825     c                   eval       IOREfil = d98lna
647900140825     c                   eval       IOREcap = d98cad
648000140825     c                   eval       IOREloc = d98lod
648100140825     c                   eval       IOREnar = d98nzd
648200140825     c
648300140825     c* Data di Riferimento:
648400140825     c* Se impostata la consegnabilità --> prendo questa
648500140822     c                   select
648600140825     c                   when      d98dfc>0
648700141107     c                   if        or_datarif<d98dfc and or_datarif>0
648800141107     c                   eval      ioredta=or_datarif
648900141107     c                   else
649000141107     c                   eval      ioredta=d98dfc
649100141107     c                   endif
649200140825     c* altrimenti data di consegna
649300140825     c                   when      d98dcm>0
649400141107     c                   if        or_datarif<d98dcm and or_datarif>0
649500141107     c                   eval      ioredta=or_datarif
649600141107     c                   else
649700140825     c                   eval      ioredta=d98dcm
649800141107     c                   endif
649900140825     c
650000140822     c                   other
650100140825     c* altrimenti data teorica consegna se > = oggi
650200140825     C                   time                    wrkoggiiso
650300140825     C     *iso          move      wrkoggiiso    ioredta
650400140822     c* se < di oggi, imposto oggi
650500140825     c                   if        d98dei>ioredta
650600140825     c                   eval      ioredta=d98dei
650700140822     c                   endif
650800141107     c
650900141107     c                   if        or_datarif<ioredta and or_datarif>0
651000141107     c                   eval      ioredta=or_datarif
651100141107     c                   endif
651200141107     c
651300140822     c                   endsl
651400140822     c
651500140825     c                   eval       IOREtser = 'C'
651600141107     c                   if         or_ddc>0 and or_ddc<d98ddc
651700141107     c                   eval       IOREDDC=or_ddc
651800141107     c                   eval       IOREnDC=8888888
651900141107     c                   eval       IOREcons = 'S'
652000141107     c                   else
652100140825     c                   eval       IOREDDC=d98ddc
652200140825     c                   eval       IOREnDC=wd98ndc
652300140825     c                   eval       IOREcons = d98cons
652400141107     c                   endif
652500141017     c                   eval       IOREtsp = d98tsp
652600140822     c
652700140822     c                   call      'TRULORSR'
652800140822     c                   parm                    kpjba
652900140822     c                   parm                    trulorSds
653000140825     c                   parm                    trulor2ds
653100140825     c
653200140825     c                   eval      wtrulor= 'S'
653300141107     c                   clear                   or_datarif        8 0
653400141107     c                   clear                   or_ddc            8 0
653500140822     C                   ENDSR
653600950406     C********************************************************************
653700950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
653800950406     C********************************************************************
653900950406     C     DATORE        BEGSR
654000950406     C*
654100950406     C                   Z-ADD     *ZEROS        WORE
654200950406     C     WDAT          IFNE      0
654300950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
654400950406     C     WAA           DIV       4             WQUA              4 0
654500950406     C                   MVR                     WRES
654600950406     C     WAA           MULT      365           WGIO              9 0
654700950406     C                   ADD       WQUA          WGIO
654800950406     C                   ADD       WGG           WGIO
654900950406     C     WMM           SUB       1             MC                3 0
655000950406     C                   DO        MC            WX                3 0
655100950406     C                   ADD       MES(WX)       WGIO
655200950406     C                   END
655300950406     C     WRES          IFEQ      0
655400950406     C     WMM           ANDLE     2
655500950406     C                   SUB       1             WGIO
655600950406     C                   END
655700950406     C     WGIO          MULT      24            WORE              9 0
655800950406     C                   ENDIF
655900950406     C*
656000950406     C                   ENDSR
656100930323     C********************************************************************
656200930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
656300930323     C********************************************************************
656400930323     C     OREDAT        BEGSR
656500930323     C*
656600950407     C                   SETOFF                                       717273
656700950407     C                   SETOFF                                       7475
656800930323     C     WORE          DIV       24            WGIO
656900930323     C                   MVR                     WRES              3 0
657000950406     C*
657100950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
657200950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
657300950406     C*  TROVO ANNO
657400950406     C     WGIO          DIV       365           WAA
657500950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
657600950406     C                   MVR                     WREST                71
657700950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
657800950406     C     WGIO          DIV       365           WAA
657900950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
658000950406     C*                                                    0 = 31/12/PR
658100950406     C   75
658200950406     CAN 71              DO                                                     BIS. E NON ERA
658300950406     C     WAA           SUB       1             WAA
658400950406     C                   Z-ADD     365           WREST
658500950406     C                   END
658600950406     C*
658700950406     C     WAA           DIV       4             WNBIS                                      T.
658800950406     C                   MVR                     WRBIS             3 073
658900950406     C   73
659000950406     CANN71                                                                     NON PIU' BISEST
659100950406     CORN73              ADD       1             WREST                          E'BISESTILE
659200950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
659300950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
659400950406     C  N73              SUB       1             $PM(1)
659500950406     C                   Z-ADD     1             WI                2 0
659600950406     C* TROVO IL MESE E GIORNI
659700950406     C     WREST         LOOKUP    $PM(WI)                            74  73
659800950406     C                   MOVE      WI            WMM
659900950406     C     WI            COMP      1                                      72
660000950406     C  N72
660100950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
660200950406     C   74              SUB       1             WI
660300950406     C  N72
660400950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
660500950406     C   72              MOVE      WREST         WGG
660600950406     C                   ADD       1900          WAA
660700950406     C*
660800930323     C*
660900930323     C                   ENDSR
6610009407151507 C********************************************************************
6611009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6612009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6613009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6614009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6615009407151507 C********************************************************************
6616009407151507 C     FINGIO        BEGSR
6617009407151507 C*
6618009407151507 C     WPORE         DIV       24            WGIO
6619009407151507 C                   MVR                     WRES
6620009407151507 C     WRES          IFEQ      *ZEROS
6621009407151507 C                   Z-ADD     12            WNORE             3 0
6622009407151507 C                   ELSE
6623009407151507 C                   Z-ADD     *ZEROS        WNORE
6624009407151507 C                   ENDIF
6625009407151507 C*
6626009407151507 C                   ENDSR
662700930517     C********************************************************************
662800971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
662900930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
663000930517     C********************************************************************
663100930517     C     CERCAL        BEGSR
663200930517     C*
663300930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
663400941227     C                   MOVE      CLNTFP        DCATFP
663500941227     C                   MOVE      CLNTFA        DCATFA
663600941227     C                   MOVE      CLNANN        DCAANN
663700941227     C                   MOVE      CLNMES        DCAMES
663800930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
663900930517     C                   Z-ADD     1             I
664000930517     C     DCALEN        LOOKUP    CAK(I)                                 99
664100930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
664200930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
664300941227     C     KEYCLN        CHAIN     AZCLN01L                           99
664400930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
664500930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
664600930517     C                   MOVEL     '1'           D98ERR
664700930517     C                   MOVEL     MSG(3)        D98MSG
664800930517     C                   ELSE                                                   <-- 2 -->
664900970401     C* CONTROLLA SE ANNULLATO
665000970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
665100970401     C                   MOVEL     '1'           D98ERR
665200970401     C                   MOVEL     MSG(3)        D98MSG
665300970401     C                   ELSE                                                   <-- 3 -->
665400930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
665500930517     C                   ADD       1             WINDCA            6 0
665600970401     C     WINDCA        IFLE      200                                          --- 4 -->
665700970401     C     WINDCA        ANDGT     *ZEROS
665800930517     C                   Z-ADD     WINDCA        I
665900930517     C                   MOVEL     DCALEN        CAK(I)
666000941227     C                   MOVEL     CLNMAT        CAM(I)
666100941227     C                   MOVEL     CLNPOM        CAP(I)
666200970401     C                   END                                                    <-- 4 ---
666300970401     C                   ENDIF                                                  <-- 3 ---
666400930517     C                   END                                                    <-- 2 ---
666500930517     C                   ELSE                                                   <-- 1 -->
666600930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
666700941227     C                   MOVEL     CAM(I)        CLNMAT
666800941227     C                   MOVEL     CAP(I)        CLNPOM
666900930517     C                   END                                                    <-- 1 ---
667000930517     C*
667100930517     C                   ENDSR
667200010821     C********************************************************************
667300010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
667400010821     C********************************************************************
667500010821     C     CHKVCA        BEGSR
667600010821     C*
667700010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
667800010821     C     D98CAS        IFGT      *ZEROS
667900010821     C     D98VCA        ANDEQ     *BLANKS
668000010821     C                   MOVEL     'ITL'         D98VCA
668100010821     C                   ENDIF
668200010822     C*
668300010822     C     D98VCA        IFNE      'ITL'
668400010822     C     D98VCA        ANDNE     'EUR'
668500010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
668600010822     C                   Z-ADD     D98CAS        INPIMP
668700010822     C                   MOVEL     D98VCA        INPDIV
668800010822     C                   EXSR      EXECNV
668900010822     C                   Z-ADD     OUTIMP        D98CAS
669000010822     C                   MOVEL     §GEDCN        D98VCA
669100010822     C                   ENDIF
669200010821     C*
669300010821     C                   ENDSR
669400010821     C********************************************************************
669500010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
669600010821     C********************************************************************
669700010821     C     EXECNV        BEGSR
669800010821     C*
669900010821     C                   CLEAR                   YEURDS
670000010821     C*
670100010821     C                   MOVEL     INPDIV        YECDVI
670200010821     C                   Z-ADD     INPIMP        YECIMI
670300010821     C                   MOVEL     §GEDCN        YECDVO
670400010821     C                   MOVEL     '3'           YECDCO
670500010821     C                   MOVEL     'H'           YECTAR
670600010821     C                   CALL      'YEURCO'
670700010821     C                   PARM                    YEURDS
670800010821     C*
670900010821     C                   Z-ADD     YECIMO        OUTIMP
671000010821     C*
671100010821     C                   ENDSR
671200011211     C*------------------------------------------------------------------------*
671300011211     C* Controllo importo massimo contrassegno.
671400011211     C*------------------------------------------------------------------------*
671500011211     C     REPMAXCSB     BEGSR
671600011211     C*
671700011211     C                   CLEAR                   Trul21DS
671800011211     C*
671900011211     C                   EVAL      I21TSp = d98TSP
672000011211     C                   EVAL      I21NzD = d98NZD
672100011211     C                   EVAL      I21LnP = d98LNP
672200011211     C                   EVAL      I21LnA = d98LNA
672300011211     C                   EVAL      I21Ksc = d98KSC
672400011211     C                   EVAL      I21TiC = d98TIC
672500011211     C                   EVAL      I21Imp = d98CAS
672600011211     C                   EVAL      I21Div = d98VCA
672700011211     C*
672800011211     C                   CALL      'TRUL21R'
672900011211     C                   PARM                    Trul21DS
673000011211     C*
673100011211     C                   ENDSR
673200011211     C*------------------------------------------------------------------------*
673300011211
673400930414     C********************************************************************
673500930414     C*   RUTINZ - ROUTINE INIZIALE
673600930414     C********************************************************************
673700930414     C     RUTINZ        BEGSR
673800930414     C*
673900930414     C* PARAMETRI IN INPUT ALLA ROUTINE
674000930414     C     *ENTRY        PLIST
674100970731     C                   PARM                    DS99
674200941227     C*
674300941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
674400941227     C     KEYBLP        KLIST
674500941227     C                   KFLD                    BLPAAS
674600941227     C                   KFLD                    BLPLNP
674700941227     C                   KFLD                    BLPNRS
674800941227     C                   KFLD                    BLPNSP
674900050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
675000941227     C     KEYGCP        KLIST
675100050405     C                   KFLD                    KWGCPAAS
675200050405     C                   KFLD                    KWGCPLNP
675300050405     C                   KFLD                    KWGCPNRS
675400050405     C                   KFLD                    KWGCPNSP
675500050405     C                   KFLD                    KWGCPFRG
675600050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
675700031125     C     KEYGC_P       KLIST
675800050405     C                   KFLD                    KWGCPAAS
675900050405     C                   KFLD                    KWGCPLNP
676000050405     C                   KFLD                    KWGCPNRS
676100050405     C                   KFLD                    KWGCPNSP
676200970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
676300150120     C**   KEYBLT        KLIST
676400150120     C**                 KFLD                    BLTAAS
676500150120     C**                 KFLD                    BLTLNP
676600150120     C**                 KFLD                    BLTNRS
676700150120     C**                 KFLD                    BLTNSP
676800941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
676900941227     C     KEYARB        KLIST
677000941227     C                   KFLD                    ARBAAS
677100941227     C                   KFLD                    ARBLNP
677200941227     C                   KFLD                    ARBNRS
677300941227     C                   KFLD                    ARBNSP
677400051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
677500941228     C     KEYAR9        KLIST
677600941228     C                   KFLD                    AR9AAS
677700941228     C                   KFLD                    AR9LNP
677800941228     C                   KFLD                    AR9NRS
677900941228     C                   KFLD                    AR9NSP
678000941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
678100050405     C     KEYGCP21      KLIST
678200050405     C                   KFLD                    KWGCPAAS
678300050405     C                   KFLD                    KWGCPLNP
678400050405     C                   KFLD                    KWGCPNRS
678500050405     C                   KFLD                    KWGCPNSP
678600050405     C                   KFLD                    KWGCPFRG
678700970912     C* CHIAVE LETTURA FNART01L - PARZIALE
678800150120     C***  KEYART        KLIST
678900150120     C***                KFLD                    ARTAAS
679000150120     C***                KFLD                    ARTLNP
679100150120     C***                KFLD                    ARTNRS
679200150120     C***                KFLD                    ARTNSP
679300950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
679400950102     C     KEYLBL        KLIST
679500950102     C                   KFLD                    LBLAAN
679600950102     C                   KFLD                    LBLLPN
679700950102     C                   KFLD                    LBLNRN
679800950102     C                   KFLD                    LBLNSN
679900930414     C*
680000070827     c     kAR5          Klist
680100070827     c                   Kfld                    d98Aas
680200070827     c                   Kfld                    d98Lnp
680300070827     c                   Kfld                    d98nrs
680400070827     c                   Kfld                    d98nsp
680500070827     c                   Kfld                    kAr5Trd
680600150325     c     kAR4          Klist
680700150325     c                   Kfld                    d98Aas
680800150325     c                   Kfld                    d98Lnp
680900150325     c                   Kfld                    d98nrs
681000150325     c                   Kfld                    d98nsp
681100150325     c                   Kfld                    kAr4Trc
681200150218     c*
681300150218     c     kart          Klist
681400150218     c                   Kfld                    d98Aas
681500150218     c                   Kfld                    d98Lnp
681600150218     c                   Kfld                    d98nrs
681700150218     c                   Kfld                    d98nsp
681800150218     c     kbrv          Klist
681900150218     c                   Kfld                    artfls
682000150218     c                   Kfld                    artlna
682100150218     c                   Kfld                    artnrs
682200150218     c                   Kfld                    artnsc
682300150325     C     Kfapd         klist
682400150325     C                   kfld                    kpdtip
682500150325     C                   kfld                    kpdpdr
682600930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
682700930414     C     KEYTAB        KLIST
682800930414     C                   KFLD                    KTBUTE
682900930414     C                   KFLD                    KTBCOD
683000930414     C                   KFLD                    KTBKEY
683100060406     C     Ktab          KLIST
683200060406     C                   KFLD                    KTBUTE
683300060406     C                   KFLD                    KTBCOD
683400930414     C*
683500941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
683600941227     C     KEYCLN        KLIST
683700941227     C                   KFLD                    CLNTFP
683800941227     C                   KFLD                    CLNTFA
683900941227     C                   KFLD                    CLNANN
684000941227     C                   KFLD                    CLNMES
684100941227     C*
684200941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
684300941227     C     KEYCAS        KLIST
684400941227     C                   KFLD                    CASPTA
684500941227     C                   KFLD                    CASLNP
684600941227     C                   KFLD                    CASLNA
684700941227     C                   KFLD                    CASTSP
684800941227     C                   KFLD                    CASDDE
684900930420     C*
685000990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
685100930420     C     KEYEVB        KLIST
685200930420     C                   KFLD                    KEBAAS
685300930420     C                   KFLD                    KEBLNP
685400930420     C                   KFLD                    KEBNRS
685500930420     C                   KFLD                    KEBNSP
685600010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
685700010821     C     KTBE          KLIST
685800010821     C                   KFLD                    TBECOD
685900010821     C                   KFLD                    TBEKE1
686000010821     C                   KFLD                    TBEKE2
686100930414     C*
686200010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
686300930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
686400930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
686500930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
686600930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
686700930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
686800930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
686900930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
687000930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
687100930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
687200010821     C     *LIKE         DEFINE    D98CAS        INPIMP
687300010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
687400010821     C     *LIKE         DEFINE    D98VCA        INPDIV
687500930414     C*
687600930414     C* RECUPERO IL CODICE DELL'AZIENDA
687700930414     C                   Z-ADD     1             CODUT             1 0
687800970701     C                   Z-ADD     *ZEROS        I                 6 0
687900930414     C*
688000930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
688100930414     C                   EXSR      AZZERA
688200930414     C*
688300930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
688400941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
688500941227     C                   MOVEL     'S'           CTABSN            1
688600930414     C                   EXSR      CARTAB
688700941227     C                   ENDIF
688800041116     c* Se passato umero di spedizioni apro i files , altrimenti no
688900041116     c                   if        d98nsp>0 and wapri=' '
689000041116     c                   open      fnblp01l
689100050405     c                   open      tigcp01l
689200150120     c***                open      fnblt01l
689300041116     c                   open      fnarb01l
689400051115     c                   open      fiar901l
689500150325     c                   open      fiar401l
689600050405     c                   open      tigcp21l
689700150218     c                   open      fnart01l
689800150218     c                   open      fnbrv07l
689900041116     c                   open      fnlbl01l
690000041116     c                   open      fnevb01l
690100070827     c                   open      fiar501l
690200041116     c                   eval      wapri='S'
690300041116     c                   endif
690400930414     C*
690500930414     C                   ENDSR
690600941227     C*------------------------------------------------------------------------*
690700941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
690800941227     C*------------------------------------------------------------------------*
690900941227     C     SUBDAT        BEGSR
691000941227     C*
691100941227     C                   TIME                    DTTIME
691200941227     C                   Z-ADD     DTANN         DSANN
691300941227     C                   Z-ADD     DTMES         DSMES
691400941227     C                   Z-ADD     DTGIO         DSGIO
691500941227     C                   Z-ADD     DTSEC         DSSEC
691600941227     C*
691700941227     C                   ENDSR
691800930414     C********************************************************************
691900930414     C*   CARTAB - CARICA LE TABELLE
692000930414     C********************************************************************
692100930414     C     CARTAB        BEGSR
692200930414     C*
692300930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
692400930414     C     D98ERR        IFEQ      '0'
692500930421     C                   Z-ADD     0             I
692600930414     C                   Z-ADD     CODUT         KTBUTE
692700930414     C                   MOVEL     '1Y'          KTBCOD
692800930414     C                   MOVEL     *BLANKS       KTBKEY
692900930414     C     KEYTAB        SETLL     TABEL00F                           99
693000930414     C  N99              READ      TABEL00F                               99
693100930414     C     *IN99         IFEQ      '1'
693200930414     C                   MOVEL     '1'           D98ERR
693300930414     C                   MOVEL     MSG(5)        D98MSG
693400930414     C                   END
693500930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
693600930414     C     TBLCOD        ANDEQ     '1Y'
693700930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
693800930421     C                   ADD       1             I
693900930421     C                   MOVEL     TBLKEY        C1Y(I)
694000930414     C                   MOVEL     TBLUNI        DS1Y
694100930421     C                   MOVEL     §1YFEI        I1Y(I)
694200930421     C                   MOVEL     §1YFEC        E1Y(I)
694300930414     C                   END                                                    <-- 3 ---
694400930414     C                   READ      TABEL00F                               99
694500930414     C                   END                                                    <-- 2 ---
694600930414     C                   END
694700930414     C*
694800930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
694900930414     C     D98ERR        IFEQ      '0'
695000930421     C                   Z-ADD     0             I
695100930414     C                   Z-ADD     CODUT         KTBUTE
695200930414     C                   MOVEL     '1X'          KTBCOD
695300930414     C                   MOVEL     *BLANKS       KTBKEY
695400930414     C     KEYTAB        SETLL     TABEL00F                           99
695500930414     C  N99              READ      TABEL00F                               99
695600930414     C     *IN99         IFEQ      '1'
695700930414     C                   MOVEL     '1'           D98ERR
695800930414     C                   MOVEL     MSG(1)        D98MSG
695900930414     C                   END
696000930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
696100930414     C     TBLCOD        ANDEQ     '1X'
696200930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
696300930421     C                   ADD       1             I
696400930421     C                   MOVEL     TBLKEY        C1X(I)
696500930414     C                   MOVEL     TBLUNI        DS1X
696600930421     C                   Z-ADD     §1XHHG        G1X(I)
696700930421     C                   Z-ADD     §1XHHO        O1X(I)
696800930421     C                   Z-ADD     §1XHHM        M1X(I)
696900930421     C                   Z-ADD     §1XHHP        P1X(I)
697000930421     C                   Z-ADD     §1XHHI        I1X(I)
697100930421     C                   Z-ADD     §1XHHS        S1X(I)
697200930414     C                   END                                                    <-- 3 ---
697300930414     C                   READ      TABEL00F                               99
697400931223     C                   ENDDO                                                  <-- 2 ---
697500050523     c
697600050523     c* controllo riempimento skiere
697700050523     c                   clear                   trul0sds
697800050523     c                   eval      i0sski='C1X/G1X/O1X/...'
697900050523     c                   eval      i0sele=%elem(c1x)
698000050523     c                   eval      i0spie=i
698100050523     c                   eval      i0sfile='TABEL00F  '
698200050523     c                   eval      i0scod=' 1X  '
698300050523     c                   eval      i0spgm='TNSD99R'
698400050523     c                   movel     trul0sds      kpjbu
698500050523     c                   call      'TRUL0SR'
698600050523     c                   parm                    kpjba
698700050523     C                   END
698800930421     C*
698900930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
699000930421     C     D98ERR        IFEQ      '0'
699100930421     C                   Z-ADD     0             I
699200930421     C                   Z-ADD     CODUT         KTBUTE
699300930421     C                   MOVEL     '2A'          KTBCOD
699400060406     C     Ktab          SETLL     TABEL00F                           99
699500060406     C  N99ktab          READe     TABEL00F                               99
699600930421     C     *IN99         IFEQ      '1'
699700930421     C                   MOVEL     '1'           D98ERR
699800930421     C                   MOVEL     MSG(8)        D98MSG
699900930421     C                   END
700000930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
700100930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
700200930421     C                   ADD       1             I
700300930421     C                   MOVEL     TBLKEY        C2A(I)
700400060406     C                   MOVEL     TBLUNI        ds2a
700500060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
700600060406     C                   MOVEL     w003a         D2A(I)
700700930421     C                   END                                                    <-- 3 ---
700800060406     C     ktab          READe     TABEL00F                               99
700900931223     C                   ENDDO                                                  <-- 2 ---
701000050523     c* controllo riempimento skiere
701100050523     c                   clear                   trul0sds
701200050523     c                   eval      i0sski='C2A/D2A'
701300050523     c                   eval      i0sele=%elem(c2a)
701400050523     c                   eval      i0spie=i
701500050523     c                   eval      i0sfile='TABEL00F  '
701600060406     c                   eval      i0scod='2A/2Z'
701700050523     c                   eval      i0spgm='TNSD99R'
701800050523     c                   movel     trul0sds      kpjbu
701900050523     c                   call      'TRUL0SR'
702000050523     c                   parm                    kpjba
702100930421     C                   END
702200060406     c* Tabella  2Z
702300060406     C     D98ERR        IFEQ      '0'
702400060406     C                   Z-ADD     CODUT         KTBUTE
702500060406     C                   MOVEL     '2Z'          KTBCOD
702600060406     C     Ktab          SETLL     TABEL00F                           99
702700060406     C  N99ktab          READe     TABEL00F                               99
702800060406     C     *IN99         IFEQ      '1'
702900060406     C                   MOVEL     '1'           D98ERR
703000060406     C                   MOVEL     MSG(33)       D98MSG
703100060406     C                   END
703200060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
703300060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
703400060406     C                   Z-ADD     1             I
703500060406     C                   MOVEL     TBLKEY        w003a
703600060406     c     w003a         lookup    c2a(i)                                 99
703700060406     C     *IN99         IFEQ      *off
703800060406     C                   MOVEL     '1'           D98ERR
703900060406     C                   MOVEL     MSG(34)       D98MSG
704000060406     C                   Else
704100060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
704200060406     C                   END                                                    <-- 3 ---
704300060406     C                   END                                                    <-- 3 ---
704400060406     C     ktab          READe     TABEL00F                               99
704500060406     C                   ENDDO                                                  <-- 2 ---
704600060406     C                   END
704700941227     C*
704800941227     C* CARICA TABELLA CODICI BOLLA -3A-
704900941227     C     D98ERR        IFEQ      '0'
705000941228     C                   Z-ADD     0             I
705100941227     C                   Z-ADD     CODUT         KTBUTE
705200941227     C                   MOVEL     '3A'          KTBCOD
705300941227     C                   MOVEL     *BLANKS       KTBKEY
705400941227     C     KEYTAB        SETLL     TABEL00F                           99
705500941227     C  N99              READ      TABEL00F                               99
705600941227     C     *IN99         IFEQ      '1'
705700941227     C                   MOVEL     '1'           D98ERR
705800941227     C                   MOVEL     MSG(22)       D98MSG
705900941227     C                   END
706000941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
706100941227     C     TBLCOD        ANDEQ     '3A'
706200941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
706300941227     C                   ADD       1             I
706400941227     C                   MOVEL     TBLKEY        C3A(I)
706500941227     C                   MOVEL     TBLUNI        DS3A
706600941227     C                   MOVEL     §3ATB1        T3A(I)
706700941228     C                   Z-ADD     §3AFCA        F3A(I)
706800990308     C                   MOVEL     §3ARBL        R3A(I)
706900941227     C                   END                                                    <-- 3 ---
707000941227     C                   READ      TABEL00F                               99
707100941227     C                   END                                                    <-- 2 ---
707200050523     c* controllo riempimento skiere
707300050523     c                   clear                   trul0sds
707400050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
707500050523     c                   eval      i0sele=%elem(c3a)
707600050523     c                   eval      i0spie=i
707700050523     c                   eval      i0sfile='TABEL00F  '
707800050523     c                   eval      i0scod=' 3A  '
707900050523     c                   eval      i0spgm='TNSD99R'
708000050523     c                   movel     trul0sds      kpjbu
708100050523     c                   call      'TRUL0SR'
708200050523     c                   parm                    kpjba
708300941227     C                   END
708400971210     C*
708500020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
708600020226     C                   Z-ADD     *ZEROS        J                 3 0
708700160331     C                   Z-ADD     *ZEROS        D                 3 0
708800971210IF  0C     D98ERR        IFEQ      '0'
708900971210     C     *LOVAL        SETLL     AZORG01L
709000971210     C                   READ      AZORG01L                               98
709100971210DO  1C     *IN98         DOWEQ     *OFF
709200971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
709300971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
709400971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
709500020627     C                   MOVEL     ORGDE3        OG143
709600020702     C*
709700020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
709800020702     C                   CLEAR                   DNTW
709900020702     C*
710000020702     C                   MOVEL(P)  'NTW'         TBECOD
710100020702     C                   MOVEL(P)  §OGNTW        TBEKE1
710200020702     C     KTBE          CHAIN     TNTBE01L
710300020702     C                   IF        %found(TNTBE01L)
710400020702     C                   MOVEL     TBEUNI        DNTW
710500020702     C                   ENDIF
710600020702     C*
710700020702     C                   MOVEL     §NTWFIE       FL1(F)
710800020627     C*
710900020226     C* VERIFICO SE FILIALE FEDEX
711000020226     C                   IF        §OGNTW = 'FED'
711100020226     C                   ADD       1             J
711200020226     C                   Z-ADD     ORGFIL        FILFED(J)
711300020226     C                   ENDIF
711400160331     C* VERIFICO SE FILIALE DPD
711500160331     C                   IF        §OGNTW = 'DPD'
711600160331     C                   ADD       1             D
711700160331     C                   Z-ADD     ORGFIL        FILDPD(D)
711800160331     C                   ENDIF
711900020226     C*
712000971210E   2C                   ENDIF
712100971210     C                   READ      AZORG01L                               98
712200971210E   1C                   ENDDO
712300971210E   0C                   ENDIF
712400010821     C*
712500010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
712600010821     C                   MOVEL     'GED'         TBECOD
712700070514     C                   MOVEL     *BLANKS       TBEKE1
712800070514     C                   MOVEL     '1'           TBEKE1
712900010821     C                   MOVEL     *BLANKS       TBEKE2
713000010821     C     KTBE          CHAIN     TNTBE01L                           21
713100070514     C  N21              MOVEL     TBEUNI        DGED
713200150326     c
713300150326     C* REPERISCO LA tabella flag operativi per tabella OLP
713400150326     c                   clear                   dvpo
713500150326     C                   MOVEL     'VPO'         TBECOD
713600150326     C                   MOVEL     *BLANKS       TBEKE2
713700150326     C                   MOVEL(P)  'VPO'         TBEKE1
713800150326     C     KTBE          CHAIN     TNTBE01L                           21
713900150326     C  N21              MOVEL     TBEUNI        DVPO
714000150326     C*
714100031201     C*
714200031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
714300031201     C     D98ERR        IFEQ      '0'
714400060403     C                   Z-ADD     0             I7QW              3 0
714500031201     C                   Z-ADD     CODUT         KTBUTE
714600031201     C                   MOVEL     '7Q'          KTBCOD
714700031201     C                   MOVEL     *BLANKS       KTBKEY
714800031201     C     KEYTAB        SETLL     TABEL00F                           99
714900031201     C  N99              READ      TABEL00F                               99
715000031201     C     *IN99         IFEQ      '1'
715100031201     C                   MOVEL     '1'           D98ERR
715200031201     C                   MOVEL     MSG(8)        D98MSG
715300031201     C                   ENDIF
715400031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
715500031201     C     TBLCOD        ANDEQ     '7Q'
715600031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
715700031201     C                   MOVEL     TBLUNI        DS7Q
715800031201     C                   IF        §7Q4D1 = 'R' OR
715900031201     C                             §7Q4D2 = 'R' OR
716000031201     C                             §7Q4D3 = 'R' OR
716100031201     C                             §7Q4D4 = 'R' OR
716200031201     C                             §7Q4D5 = 'R'
716300031201     C                   ADD       1             I7QW
716400031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
716500031201     C                   ENDIF                                                  <-- 3 ---
716600031201     C                   ENDIF                                                  <-- 3 ---
716700031201     C                   READ      TABEL00F                               99
716800031201     C                   ENDDO                                                  <-- 2 ---
716900031201     C                   ENDIF
717000031201     C*
717100031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
717200031201     C     D98ERR        IFEQ      '0'
717300031201     C                   Z-ADD     CODUT         KTBUTE
717400031201     C                   MOVEL     '7W'          KTBCOD
717500031201     C                   MOVEL     *BLANKS       KTBKEY
717600031201     C     KEYTAB        SETLL     TABEL00F                           99
717700031201     C  N99              READ      TABEL00F                               99
717800031201     C     *IN99         IFEQ      '1'
717900031201     C                   MOVEL     '1'           D98ERR
718000031201     C                   MOVEL     MSG(8)        D98MSG
718100031201     C                   ENDIF
718200031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
718300031201     C     TBLCOD        ANDEQ     '7W'
718400031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
718500031201     C                   MOVEL     TBLUNI        DS7W
718600031201     C                   IF        §7W6D1 = 'C' OR
718700031201     C                             §7W6D2 = 'C' OR
718800031201     C                             §7W6D3 = 'C' OR
718900031201     C                             §7W6D4 = 'C' OR
719000031201     C                             §7W6D5 = 'C'
719100031201     C                   ADD       1             I7QW
719200031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
719300031201     C                   ENDIF                                                  <-- 3 ---
719400031201     C                   ENDIF                                                  <-- 3 ---
719500031201     C                   READ      TABEL00F                               99
719600031201     C                   ENDDO                                                  <-- 2 ---
719700050523     c* controllo riempimento skiere
719800050523     c                   clear                   trul0sds
719900050523     c                   eval      i0sski='SK7QW'
720000050523     c                   eval      i0sele=%elem(sk7qw)
720100050523     c                   eval      i0spie=i7qw
720200050523     c                   eval      i0sfile='TABEL00F  '
720300050523     c                   eval      i0scod='7Q-7W'
720400050523     c                   eval      i0spgm='TNSD99R'
720500050523     c                   movel     trul0sds      kpjbu
720600050523     c                   call      'TRUL0SR'
720700050523     c                   parm                    kpjba
720800031201     C                   ENDIF
720900930414     C*
721000930414     C                   ENDSR
721100930414     C********************************************************************
721200930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
721300930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
721400930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
721500930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
721600930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
721700930414     C********************************************************************
721800930414     C     RO3ORE        BEGSR
721900930414     C*
722000930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
722100930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
722200930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
722300050902     c                   movel     ' '           WR3MATPOM         1
722400931209     C                   Z-ADD     *ZEROS        WR3HHO
722500931209     C                   Z-ADD     *ZEROS        WR3HHA
722600930414     C                   MOVE      WR3TAB        WR3COD
722700930604     C                   MOVEL     D98LNA        WR3COD            8
722800930421     C                   Z-ADD     1             I
722900930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
723000930414     C     *IN99         IFEQ      '0'
723100930414     C                   MOVE      WR3TAB        WR3COD
723200930414     C                   MOVEL     '   '         WR3COD            8
723300930421     C                   Z-ADD     1             I
723400930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
723500930414     C                   END
723600930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
723700930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
723800930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
723900930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
724000930414     C                   ELSE                                                   <-- 2 -->
724100930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
724200930421     C                   Z-ADD     M1X(I)        WR3HHO
724300930510     C                   Z-ADD     I1X(I)        WR3HHA
724400050902     c                   movel     'M'           WR3MATPOM         1
724500930414     C                   END
724600930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
724700930421     C                   Z-ADD     P1X(I)        WR3HHO
724800930510     C                   Z-ADD     S1X(I)        WR3HHA
724900050902     c                   movel     'P'           WR3MATPOM
725000930414     C                   END
725100930414     C                   END                                                    <-- 2 ---
725200930414     C                   END                                                    <-- 1 ---
725300930414     C*
725400930414     C                   ENDSR
725500930423     C********************************************************************
725600930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
725700930423     C********************************************************************
725800930423     C     W98D98        BEGSR
725900930423     C*
726000941228     C*
726100941228     C* ORE REALI PARTENZA INTERNE
726200941228     C                   Z-ADD     W98RPI        W98               3 0
726300941228     C                   EXSR      W98999
726400941228     C                   Z-ADD     D98           D98RPI
726500941228     C* ORE REALI PARTENZA CLIENTE
726600941228     C                   Z-ADD     W98RPC        W98
726700941228     C                   EXSR      W98999
726800941228     C                   Z-ADD     D98           D98RPC
726900930423     C* ORE REALI TRAZIONE INTERNE
727000941228     C                   Z-ADD     W98RTI        W98
727100941228     C                   EXSR      W98999
727200941228     C                   Z-ADD     D98           D98RTI
727300930423     C* ORE REALI TRAZIONE CLIENTE
727400941228     C                   Z-ADD     W98RTC        W98
727500941228     C                   EXSR      W98999
727600941228     C                   Z-ADD     D98           D98RTC
727700930423     C* ORE REALI CONSEGNA INTERNE
727800941228     C                   Z-ADD     W98RCI        W98
727900941228     C                   EXSR      W98999
728000941228     C                   Z-ADD     D98           D98RCI
728100930423     C* ORE REALI CONSEGNA CLIENTE
728200941228     C                   Z-ADD     W98RCC        W98
728300941228     C                   EXSR      W98999
728400941228     C                   Z-ADD     D98           D98RCC
728500930423     C* ORE REALI RESA INTERNE
728600941228     C                   Z-ADD     W98RRI        W98
728700941228     C                   EXSR      W98999
728800941228     C                   Z-ADD     D98           D98RRI
728900930423     C* ORE REALI RESA CLIENTE
729000941228     C                   Z-ADD     W98RRC        W98
729100941228     C                   EXSR      W98999
729200941228     C                   Z-ADD     D98           D98RRC
729300941228     C* ORE DIFF. PARTENZA INTERNE
729400941228     C                   Z-ADD     W98FPI        W98
729500941228     C                   EXSR      W98999
729600941228     C                   Z-ADD     D98           D98FPI
729700941228     C* ORE DIFF. PARTENZA CLIENTE
729800941228     C                   Z-ADD     W98FPC        W98
729900941228     C                   EXSR      W98999
730000941228     C                   Z-ADD     D98           D98FPC
730100930423     C* ORE DIFF. TRAZIONE INTERNE
730200941228     C                   Z-ADD     W98FTI        W98
730300941228     C                   EXSR      W98999
730400941228     C                   Z-ADD     D98           D98FTI
730500930423     C* ORE DIFF. TRAZIONE CLIENTE
730600941228     C                   Z-ADD     W98FTC        W98
730700941228     C                   EXSR      W98999
730800941228     C                   Z-ADD     D98           D98FTC
730900930423     C* ORE DIFF. CONSEGNA INTERNE
731000941228     C                   Z-ADD     W98FCI        W98
731100941228     C                   EXSR      W98999
731200941228     C                   Z-ADD     D98           D98FCI
731300930423     C* ORE DIFF. CONSEGNA CLIENTE
731400941228     C                   Z-ADD     W98FCC        W98
731500941228     C                   EXSR      W98999
731600941228     C                   Z-ADD     D98           D98FCC
731700930423     C* ORE DIFF. RESA INTERNE
731800941228     C                   Z-ADD     W98FRI        W98
731900941228     C                   EXSR      W98999
732000941228     C                   Z-ADD     D98           D98FRI
732100930423     C* ORE DIFF. RESA CLIENTE
732200941228     C                   Z-ADD     W98FRC        W98
732300941228     C                   EXSR      W98999
732400941228     C                   Z-ADD     D98           D98FRC
732500941228     C* ORE NON CONF. PARTENZA INTERNE
732600941228     C     W98NPI        IFGT      999
732700941228     C                   Z-ADD     999           D98NPI
732800941228     C                   ELSE
732900941228     C                   Z-ADD     W98NPI        D98NPI
733000941228     C                   END
733100941228     C* ORE NON CONF. PARTENZA CLIENTE
733200941228     C     W98NPC        IFGT      999
733300941228     C                   Z-ADD     999           D98NPC
733400941228     C                   ELSE
733500941228     C                   Z-ADD     W98NPC        D98NPC
733600941228     C                   END
733700930423     C* ORE NON CONF. TRAZIONE INTERNE
733800930423     C     W98NTI        IFGT      999
733900930423     C                   Z-ADD     999           D98NTI
734000930423     C                   ELSE
734100930423     C                   Z-ADD     W98NTI        D98NTI
734200930423     C                   END
734300930423     C* ORE NON CONF. TRAZIONE CLIENTE
734400930423     C     W98NTC        IFGT      999
734500930423     C                   Z-ADD     999           D98NTC
734600930423     C                   ELSE
734700930423     C                   Z-ADD     W98NTC        D98NTC
734800930423     C                   END
734900930423     C* ORE NON CONF. CONSEGNA INTERNE
735000930423     C     W98NCI        IFGT      999
735100930423     C                   Z-ADD     999           D98NCI
735200930423     C                   ELSE
735300930423     C                   Z-ADD     W98NCI        D98NCI
735400930423     C                   END
735500930423     C* ORE NON CONF. CONSEGNA CLIENTE
735600930423     C     W98NCC        IFGT      999
735700930423     C                   Z-ADD     999           D98NCC
735800930423     C                   ELSE
735900930423     C                   Z-ADD     W98NCC        D98NCC
736000930423     C                   END
736100930423     C* ORE NON CONF. RESA INTERNE
736200930423     C     W98NRI        IFGT      999
736300930423     C                   Z-ADD     999           D98NRI
736400930423     C                   ELSE
736500930423     C                   Z-ADD     W98NRI        D98NRI
736600930423     C                   END
736700930423     C* ORE NON CONF. RESA CLIENTE
736800930423     C     W98NRC        IFGT      999
736900930423     C                   Z-ADD     999           D98NRC
737000930423     C                   ELSE
737100930423     C                   Z-ADD     W98NRC        D98NRC
737200930423     C                   END
737300930423     C* ORE X TURNO CHIUSURA INTERNE
737400941228     C                   Z-ADD     W98HHI        W98
737500941228     C                   EXSR      W98999
737600941228     C                   Z-ADD     D98           D98HHI
737700930423     C* ORE X TURNO CHIUSURA CLIENTE
737800941228     C                   Z-ADD     W98HHC        W98
737900941228     C                   EXSR      W98999
738000941228     C                   Z-ADD     D98           D98HHC
738100060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
738200060418     C                   Z-ADD     W98EGI        W98
738300060418     C                   EXSR      W98999
738400060418     C                   Z-ADD     D98           D98hEGI
738500060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
738600060418     C                   Z-ADD     W98EGC        W98
738700060418     C                   EXSR      W98999
738800060418     C                   Z-ADD     D98           D98hEGC
738900060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
739000060420     C                   Z-ADD     W98EAI        W98
739100060420     C                   EXSR      W98999
739200060420     C                   Z-ADD     D98           D98hEAI
739300060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
739400060420     C                   Z-ADD     W98EAC        W98
739500060420     C                   EXSR      W98999
739600060420     C                   Z-ADD     D98           D98hEAC
739700930423     C* ORE X LASC. AVVISO INTERNE
739800941228     C                   Z-ADD     W98HLI        W98
739900941228     C                   EXSR      W98999
740000941228     C                   Z-ADD     D98           D98HLI
740100930423     C* ORE X LASC. AVVISO CLIENTE
740200941228     C                   Z-ADD     W98HLC        W98
740300941228     C                   EXSR      W98999
740400941228     C                   Z-ADD     D98           D98HLC
740500930423     C* ORE X GIACENZA INTERNE
740600941228     C                   Z-ADD     W98HGI        W98
740700941228     C                   EXSR      W98999
740800941228     C                   Z-ADD     D98           D98HGI
740900930423     C* ORE X GIACENZA CLIENTE
741000941228     C                   Z-ADD     W98HGC        W98
741100941228     C                   EXSR      W98999
741200941228     C                   Z-ADD     D98           D98HGC
741300941228     C* ORE X FEST. PART. INTERNE
741400941228     C                   Z-ADD     W98HPI        W98
741500941228     C                   EXSR      W98999
741600941228     C                   Z-ADD     D98           D98HPI
741700941228     C* ORE X FEST. PART. CLIENTE
741800941228     C                   Z-ADD     W98HPC        W98
741900941228     C                   EXSR      W98999
742000941228     C                   Z-ADD     D98           D98HPC
742100930423     C* ORE X FEST. TRAZ. INTERNE
742200941228     C                   Z-ADD     W98HFI        W98
742300941228     C                   EXSR      W98999
742400941228     C                   Z-ADD     D98           D98HFI
742500930423     C* ORE X FEST. TRAZ. CLIENTE
742600941228     C                   Z-ADD     W98HFC        W98
742700941228     C                   EXSR      W98999
742800941228     C                   Z-ADD     D98           D98HFC
742900930423     C* ORE X FEST. CONS. INTERNE
743000941228     C                   Z-ADD     W98HEI        W98
743100941228     C                   EXSR      W98999
743200941228     C                   Z-ADD     D98           D98HEI
743300930423     C* ORE X FEST. CONS. CLIENTE
743400941228     C                   Z-ADD     W98HEC        W98
743500941228     C                   EXSR      W98999
743600941228     C                   Z-ADD     D98           D98HEC
743700930511     C* ORE X F.DEPOSITO INTERNE
743800941228     C                   Z-ADD     W98HDI        W98
743900941228     C                   EXSR      W98999
744000941228     C                   Z-ADD     D98           D98HDI
744100930511     C* ORE X F.DEPOSITO CLIENTE
744200941228     C                   Z-ADD     W98HDC        W98
744300941228     C                   EXSR      W98999
744400941228     C                   Z-ADD     D98           D98HDC
744500930511     C* ORE X CONTRASSEGNO INTERNE
744600941228     C                   Z-ADD     W98HOI        W98
744700941228     C                   EXSR      W98999
744800941228     C                   Z-ADD     D98           D98HOI
744900930511     C* ORE X CONTRASSEGNO CLIENTE
745000941228     C                   Z-ADD     W98HOC        W98
745100941228     C                   EXSR      W98999
745200941228     C                   Z-ADD     D98           D98HOC
745300930914     C* ORE DI CONSEGNABILITA' INTERNE
745400941228     C                   Z-ADD     W98OCI        W98
745500941228     C                   EXSR      W98000
745600941228     C                   Z-ADD     D98           D98OCI
745700930914     C* ORE DI CONSEGNABILITA' CLIENTE
745800941228     C                   Z-ADD     W98OCC        W98
745900941228     C                   EXSR      W98000
746000941228     C                   Z-ADD     D98           D98OCC
746100071122     C
746200071122     c* Per i codici bolla M prepagati annullati, azzero tutto
746300071122     c                   if        d98cbo='M '
746400071122     c                   clear                   d98rpi
746500071122     c                   clear                   d98rpc
746600071122     c                   clear                   d98rti
746700071122     c                   clear                   d98rtc
746800071122     c                   clear                   d98rci
746900071122     c                   clear                   d98rcc
747000071122     c                   clear                   d98rri
747100071122     c                   clear                   d98rrc
747200071122     c                   clear                   d98fpi
747300071122     c                   clear                   d98fpc
747400071122     c                   clear                   d98fti
747500071122     c                   clear                   d98ftc
747600071122     c                   clear                   d98fci
747700071122     c                   clear                   d98fcc
747800071122     c                   clear                   d98fri
747900071122     c                   clear                   d98frc
748000071122     c                   clear                   d98npi
748100071122     c                   clear                   d98npc
748200071122     c                   clear                   d98nti
748300071122     c                   clear                   d98ntc
748400071122     c                   clear                   d98nci
748500071122     c                   clear                   d98ncc
748600071122     c                   clear                   d98nri
748700071122     c                   clear                   d98nrc
748800071122     c                   clear                   d98OCi
748900071122     c                   clear                   d98OCC
749000071122     c                   endif
749100930423     C*
749200100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
749300100308     c*  calcolo
749400100308     c                   eval      d98dcr=s98dcr
749500100308     c                   eval      d98hcr=s98hcr
749600100308     c                   eval      d98tcr=s98tcr
749700160405     c
749800160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
749900160405     c*  perchè il pgm usa il campo verificado se =10 )
750000160405     c                   if        wVP='S'
750100160405     c                   add       24            d98tci
750200160405     c                   add       24            d98tcc
750300160405     c                   add       24            d98tri
750400160405     c                   add       24            d98trc
750500160405     c                   endif
750600100308     c
750700930423     C                   ENDSR
750800941228     C***********************************************************************
750900941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
751000941228     C***********************************************************************
751100941228     C     W98999        BEGSR
751200941228     C     W98           IFLT      -999
751300941228     C                   Z-ADD     -999          D98               3 0
751400941228     C                   ELSE
751500941228     C     W98           IFGT      999
751600941228     C                   Z-ADD     999           D98
751700941228     C                   ELSE
751800941228     C                   Z-ADD     W98           D98
751900941228     C                   END
752000941228     C                   END
752100941228     C                   ENDSR
752200941228     C***********************************************************************
752300941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
752400941228     C***********************************************************************
752500941228     C     W98000        BEGSR
752600941228     C     W98           IFLE      *ZEROS
752700941228     C                   Z-ADD     *ZEROS        D98
752800941228     C                   ELSE
752900941228     C     W98           IFGT      999
753000941228     C                   Z-ADD     999           D98
753100941228     C                   ELSE
753200941228     C                   Z-ADD     W98           D98
753300941228     C                   END
753400941228     C                   END
753500941228     C                   ENDSR
753600930305     C***********************************************************************
753700930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
753800930305     C***********************************************************************
753900930305     C     AZZERA        BEGSR
754000150219     c* data e ora del momento
754100150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
754200150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
754300150219     C                   Z-ADD     DTORA4        WHHODI            4 0
754400930305     C*
754500141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
754600150603     C                   MOVEL     'N'           opendan
754700141107     c                   clear                   or_ddc
754800141108     c                   clear                   sav_12dat         8 0
754900141107     c                   clear                   or_datarif
755000140402     c                   clear                   wpick2            1
755100140402     c                   clear                   Primotipo
755200090928     c                   clear                   Primodev
755300090928     c                   clear                   Primohev
755400070827     c                   clear                   CallAR5
755500071122     c                   clear                   d98cbo
755600100308     c                   clear                   s98dcr
755700100308     c                   clear                   s98hcr
755800100308     c                   clear                   s98tcr
755900141106     c                   clear                   nosomma           1
756000141229     c                   clear                   wtrasm
756100150324     c                   clear                   avvcmc
756200150325     c                   clear                   DSbl4L
756300160401     c                   clear                   dsbl4I
756400150401     c                   clear                   fnlv82ds
756500150331     c                   clear                   DS7R
756600150605     c                   clear                   wDEE              1
756700150629     c                   clear                   wtsogliaE
756800930323     C                   MOVEL     '0'           D98ERR
756900930323     C                   MOVEL     *BLANKS       D98MSG
757000931124     C                   Z-ADD     *ZEROS        D98TPI
757100931124     C                   Z-ADD     *ZEROS        D98TPC
757200930325     C                   Z-ADD     *ZEROS        D98TTI
757300930325     C                   Z-ADD     *ZEROS        D98TTC
757400930325     C                   Z-ADD     *ZEROS        D98TCI
757500930325     C                   Z-ADD     *ZEROS        D98TCC
757600090827     C                   Z-ADD     *ZEROS        w99TCI
757700090827     C                   Z-ADD     *ZEROS        w99TCC
757800930325     C                   Z-ADD     *ZEROS        D98TRI
757900930325     C                   Z-ADD     *ZEROS        D98TRC
758000090921     C                   Z-ADD     *ZEROS        w99TRI
758100090921     C                   Z-ADD     *ZEROS        w99TRC
758200931124     C                   Z-ADD     *ZEROS        D98RPI
758300931124     C                   Z-ADD     *ZEROS        D98RPC
758400930405     C                   Z-ADD     *ZEROS        D98RTI
758500930405     C                   Z-ADD     *ZEROS        D98RTC
758600930405     C                   Z-ADD     *ZEROS        D98RCI
758700930405     C                   Z-ADD     *ZEROS        D98RCC
758800930405     C                   Z-ADD     *ZEROS        D98RRI
758900930405     C                   Z-ADD     *ZEROS        D98RRC
759000931124     C                   Z-ADD     *ZEROS        D98FPI
759100931124     C                   Z-ADD     *ZEROS        D98FPC
759200930325     C                   Z-ADD     *ZEROS        D98FTI
759300930325     C                   Z-ADD     *ZEROS        D98FTC
759400930325     C                   Z-ADD     *ZEROS        D98FCI
759500930325     C                   Z-ADD     *ZEROS        D98FCC
759600930325     C                   Z-ADD     *ZEROS        D98FRI
759700930325     C                   Z-ADD     *ZEROS        D98FRC
759800931124     C                   MOVEL     *BLANKS       D98CPI
759900931124     C                   MOVEL     *BLANKS       D98CPC
760000930325     C                   MOVEL     *BLANKS       D98CTI
760100930325     C                   MOVEL     *BLANKS       D98CTC
760200930325     C                   MOVEL     *BLANKS       D98CCI
760300930325     C                   MOVEL     *BLANKS       D98CCC
760400930325     C                   MOVEL     *BLANKS       D98CRI
760500930325     C                   MOVEL     *BLANKS       D98CRC
760600931124     C                   Z-ADD     *ZEROS        D98NPI
760700931124     C                   Z-ADD     *ZEROS        D98NPC
760800930325     C                   Z-ADD     *ZEROS        D98NTI
760900930325     C                   Z-ADD     *ZEROS        D98NTC
761000930325     C                   Z-ADD     *ZEROS        D98NCI
761100930325     C                   Z-ADD     *ZEROS        D98NCC
761200930325     C                   Z-ADD     *ZEROS        D98NRI
761300930325     C                   Z-ADD     *ZEROS        D98NRC
761400930414     C                   Z-ADD     *ZEROS        D98HHI
761500930414     C                   Z-ADD     *ZEROS        D98HHC
761600930414     C                   Z-ADD     *ZEROS        D98HOI
761700930414     C                   Z-ADD     *ZEROS        D98HOC
761800930414     C                   Z-ADD     *ZEROS        D98HLI
761900930414     C                   Z-ADD     *ZEROS        D98HLC
762000930414     C                   Z-ADD     *ZEROS        D98HGI
762100930414     C                   Z-ADD     *ZEROS        D98HGC
762200931124     C                   Z-ADD     *ZEROS        D98HPI
762300931124     C                   Z-ADD     *ZEROS        D98HPC
762400930414     C                   Z-ADD     *ZEROS        D98HFI
762500930414     C                   Z-ADD     *ZEROS        D98HFC
762600930416     C                   Z-ADD     *ZEROS        D98HEI
762700930416     C                   Z-ADD     *ZEROS        D98HEC
762800930414     C                   Z-ADD     *ZEROS        D98HRI
762900930414     C                   Z-ADD     *ZEROS        D98HRC
763000930511     C                   Z-ADD     *ZEROS        D98HDI
763100930511     C                   Z-ADD     *ZEROS        D98HDC
763200930325     C                   Z-ADD     *ZEROS        D98DCI
763300930325     C                   Z-ADD     *ZEROS        D98HCI
763400930325     C                   Z-ADD     *ZEROS        D98DCE
763500930325     C                   Z-ADD     *ZEROS        D98HCE
763600931125     C                   Z-ADD     *ZEROS        D98OCI
763700931125     C                   Z-ADD     *ZEROS        D98OCC
763800931202     C                   Z-ADD     *ZEROS        D98DEI
763900931202     C                   Z-ADD     *ZEROS        D98OEI
764000931202     C                   Z-ADD     *ZEROS        D98DEE
764100931202     C                   Z-ADD     *ZEROS        D98OEE
764200151019     c                   CLEAR                   D98FLP
764300150325     C                   clear                   D98dtctd
764400150326     C                   clear                   D98picklim
764500150325     C                   clear                   D98spcdee
764600150331     C                   clear                   fddata
764700140825     c
764800141017     c                   clear                   d98ddc
764900141017     c                   clear                   d98cons
765000141017     c                   clear                   wd98ndc
765100140910     c                   clear                   wd98fbc
765200150119     c                   clear                   wd98acc
765300140825     c                   clear                   wtrulor
765400160401     c                   clear                   wVP               1
765500150219     c
765600150219     c                   clear                   IMPdata
765700150219     c                   clear                   IMPfgs
765800150219     c                   clear                   DISdata
765900150219     c                   clear                   DISfgs
766000150219     c                   clear                   DISora
766100150219     c                   clear                   DISnpg
766200150219     c                   clear                   IDnpg
766300150219     c                   clear                   IDdata
766400150219     c                   clear                   IDora
766500150219     c                   clear                   IDfgs
766600931125     C* CAMPI VARI
766700931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
766800140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
766900140825      *   DALLA LETTURA DI QUESTI FILE                               *
767000931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
767100931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
767200931125     C     D98NSP        IFNE      *ZEROS
767300930525     C                   MOVEL     *BLANKS       D98TSP
767400941228     C                   MOVEL     *BLANKS       D98CAD
767500970728     C                   MOVEL     *BLANKS       D98LOD
767600930525     C                   Z-ADD     *ZEROS        D98TFP
767700930525     C                   Z-ADD     *ZEROS        D98TFA
767800930525     C                   Z-ADD     *ZEROS        D98LNA
767900930525     C                   Z-ADD     *ZEROS        D98DSP
768000941228     C                   Z-ADD     *ZEROS        D98MGS
768100930525     C                   Z-ADD     *ZEROS        D98DTI
768200930525     C                   Z-ADD     *ZEROS        D98HTI
768300930525     C                   Z-ADD     *ZEROS        D98DCM
768400930525     C                   Z-ADD     *ZEROS        D98HMC
768500930525     C                   MOVEL     *BLANKS       D98GC1
768600930525     C                   MOVEL     *BLANKS       D98GC2
768700010820     C                   Z-ADD     *ZEROS        D98CAS
768800010821     C                   MOVEL     *BLANKS       D98VCA
768900930525     C                   MOVEL     *BLANKS       D98TIC
769000930525     C                   Z-ADD     *ZEROS        D98DCR
769100930525     C                   Z-ADD     *ZEROS        D98HCR
769200930525     C                   MOVEL     *BLANKS       D98TCR
769300050914     C                   MOVEL     *BLANKS       Consideraora      1
769400930525     C                   MOVEL     *BLANKS       D98CMC
769500930525     C                   Z-ADD     *ZEROS        D98DAG
769600930525     C                   Z-ADD     *ZEROS        D98DCG
769700930525     C                   MOVEL     *BLANKS       D98FFD
769800930525     C                   Z-ADD     *ZEROS        D98NCD
769900930525     C                   MOVEL     *BLANKS       D98FNS
770000931124     C                   Z-ADD     *ZEROS        D98DAM
770100940628     C                   MOVEL     *BLANKS       D98FIN
770200940628     C                   Z-ADD     *ZEROS        D98KSC
770300940628     C                   MOVEL     *BLANKS       D98TBL
770400931125     C                   END
770500960229     C     D98TLA        IFEQ      'T'
770600960229     C                   Z-ADD     *ZEROS        D98DTI
770700960229     C                   Z-ADD     *ZEROS        D98HTI
770800960229     C                   ENDIF
770900931125     C* CAMPI WORK
771000930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
771100930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
771200150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
771300930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
771400941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
771500960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
771600930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
771700930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
771800940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
771900930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
772000150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
772100931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
772200931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
772300150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
772400930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
772500930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
772600930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
772700930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
772800930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
772900940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
773000941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
773100930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
773200050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
773300050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
773400140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
773500930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
773600930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
773700930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
773800930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
773900930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
774000930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
774100930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
774200930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
774300930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
774400930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
774500930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
774600930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
774700930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
774800930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
774900140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
775000931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
775100060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
775200960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
775300931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
775400140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
775500140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
775600150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
775700140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
775800941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
775900970304     C******************** Z-ADD*ZEROS    W98DBR  80
776000970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
776100150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
776200050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
776300930414     C*
776400941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
776500941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
776600930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
776700930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
776800930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
776900930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
777000930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
777100930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
777200941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
777300941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
777400930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
777500930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
777600930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
777700930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
777800930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
777900930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
778000941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
778100941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
778200930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
778300930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
778400930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
778500930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
778600930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
778700930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
778800930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
778900930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
779000060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
779100060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
779200060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
779300060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
779400930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
779500930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
779600930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
779700930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
779800930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
779900930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
780000930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
780100930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
780200930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
780300930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
780400930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
780500930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
780600930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
780700930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
780800941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
780900941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
781000930423     C*
781100930511     C                   Z-ADD     *ZEROS        W98HF             5 0
781200930423     C                   Z-ADD     *ZEROS        W98HH             5 0
781300060418     C                   Z-ADD     *ZEROS        W98EG             5 0
781400060420     C                   Z-ADD     *ZEROS        W98EA             5 0
781500930423     C                   Z-ADD     *ZEROS        W98HG             5 0
781600930423     C                   Z-ADD     *ZEROS        W98HL             5 0
781700930423     C                   Z-ADD     *ZEROS        W98HO             5 0
781800930511     C                   Z-ADD     *ZEROS        W98HD             5 0
781900930611     C*
782000930611     C                   Z-ADD     *ZEROS        WORELA            3 0
782100960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
782200930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
782300930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
782400931207     C                   Z-ADD     *ZEROS        WORECR            3 0
782500090902     c
782600090902     c                   clear                   wmancaTRAZ
782700150113     c                   clear                   wcaldpcee         1
782800150220     c                   clear                   evemic            2
782900930305     C*
783000060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
783100060406     C                   clear                   LAF                            *LASC.AVVISO FINE
783200091014     C                   clear                   LAp                            *posizione l.avviso
783300060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
783400060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
783500060406c    c                   clear                   LCon
783600060406     C                   clear                   EVD                            *DATA EVENTO
783700060406     C                   clear                   EVO                            *ORA EVENTO
783800060406     C                   clear                   EVC                            *CAUSALE EVENTO
783900131218     C                   clear                   EVT
784000060406     C                   clear                   pVD                            *DATA EVENTO
784100060406     C                   clear                   pVO                            *ORA EVENTO
784200060406     C                   clear                   pVC                            *CAUSALE EVENTO
784300060406     C                   clear                   pVF                            *CAUSALE EVENTO
784400060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
784500060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
784600060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
784700060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
784800060406     C                   clear                   COV                            *DATE FINE L.A. E GI
784900060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
785000060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
785100091014     C                   clear                   GIp                            *posizione vento gia
785200060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
785300060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
785400060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
785500931213     C*
785600060406     C                   clear                   TUC                            *TURNI CHIUSURA
785700931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
785800931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
785900060406     c
786000060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
786100060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
786200060406     c                   clear                   pgii
786300060406     c                   clear                   pgif
786400060406     c                   clear                   pgci
786500060406     c                   clear                   pgcf
786600060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
786700060420     c                   clear                   paii
786800060420     c                   clear                   paif
786900930420     C*
787000930305     C                   ENDSR
787100930512     C***********************************************************************
787200930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
787300930512     C***********************************************************************
787400930512     C     FORCON        BEGSR
787500930512     C*
787600931124     C                   Z-ADD     *ZEROS        D98RPI
787700931124     C                   Z-ADD     *ZEROS        D98RPC
787800930512     C                   Z-ADD     *ZEROS        D98RTI
787900930512     C                   Z-ADD     *ZEROS        D98RTC
788000930512     C                   Z-ADD     *ZEROS        D98RCI
788100930512     C                   Z-ADD     *ZEROS        D98RCC
788200930512     C                   Z-ADD     *ZEROS        D98RRI
788300930512     C                   Z-ADD     *ZEROS        D98RRC
788400931124     C                   Z-ADD     *ZEROS        D98FPI
788500931124     C                   Z-ADD     *ZEROS        D98FPC
788600930512     C                   Z-ADD     *ZEROS        D98FTI
788700930512     C                   Z-ADD     *ZEROS        D98FTC
788800930512     C                   Z-ADD     *ZEROS        D98FCI
788900930512     C                   Z-ADD     *ZEROS        D98FCC
789000930512     C                   Z-ADD     *ZEROS        D98FRI
789100930512     C                   Z-ADD     *ZEROS        D98FRC
789200931124     C                   MOVEL     'S'           D98CPI
789300931124     C                   MOVEL     'S'           D98CPC
789400930512     C                   MOVEL     'S'           D98CTI
789500930512     C                   MOVEL     'S'           D98CTC
789600930512     C                   MOVEL     'S'           D98CCI
789700930512     C                   MOVEL     'S'           D98CCC
789800930512     C                   MOVEL     'S'           D98CRI
789900930512     C                   MOVEL     'S'           D98CRC
790000931124     C                   Z-ADD     *ZEROS        D98NPI
790100931124     C                   Z-ADD     *ZEROS        D98NPC
790200930512     C                   Z-ADD     *ZEROS        D98NTI
790300930512     C                   Z-ADD     *ZEROS        D98NTC
790400930512     C                   Z-ADD     *ZEROS        D98NCI
790500930512     C                   Z-ADD     *ZEROS        D98NCC
790600930512     C                   Z-ADD     *ZEROS        D98NRI
790700930512     C                   Z-ADD     *ZEROS        D98NRC
790800930512     C                   Z-ADD     *ZEROS        D98HHI
790900930512     C                   Z-ADD     *ZEROS        D98HHC
791000930512     C                   Z-ADD     *ZEROS        D98HOI
791100930512     C                   Z-ADD     *ZEROS        D98HOC
791200930512     C                   Z-ADD     *ZEROS        D98HLI
791300930512     C                   Z-ADD     *ZEROS        D98HLC
791400930512     C                   Z-ADD     *ZEROS        D98HGI
791500930512     C                   Z-ADD     *ZEROS        D98HGC
791600931124     C                   Z-ADD     *ZEROS        D98HPI
791700931124     C                   Z-ADD     *ZEROS        D98HPC
791800930512     C                   Z-ADD     *ZEROS        D98HFI
791900930512     C                   Z-ADD     *ZEROS        D98HFC
792000930512     C                   Z-ADD     *ZEROS        D98HEI
792100930512     C                   Z-ADD     *ZEROS        D98HEC
792200930512     C                   Z-ADD     *ZEROS        D98HRI
792300930512     C                   Z-ADD     *ZEROS        D98HRC
792400930512     C                   Z-ADD     *ZEROS        D98HDI
792500930512     C                   Z-ADD     *ZEROS        D98HDC
792600930520     C*
792700930512     C                   ENDSR
792800930305     C********************************************************************
792900971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
793000950406031059090120151181212243273304334365
793100950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
793200950406312831303130313130313031
793300930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
793400971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
793500971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
793600971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
793700971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
793800971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
793900971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
794000971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
794100971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
794200971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
794300971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
794400971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
794500971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
794600971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
794700971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
794800971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
794900971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
795000971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
795100971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
795200971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
795300971127TNSD99R- Bolla inesistente.                                               BOIN20
795400971127TNSD99R-     Cap inesistente.                                             CAPI21
795500971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
795600971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
795700971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
795800971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
795900971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
796000971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
796100971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
796200971127TNSD99R- Contrassegno non trovato.                                        CONT29
796300971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
796400990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
796500990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
796600060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
796700060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
796800971127TNSD99R- 35                                                                   35
796900971127TNSD99R- 36                                                                   36
797000971127TNSD99R- 37                                                                   37
797100971127TNSD99R- 38                                                                   38
797200971127TNSD99R- 39                                                                   39
797300971127TNSD99R- 40                                                                   40
