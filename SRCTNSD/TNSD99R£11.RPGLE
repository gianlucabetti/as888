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
042400160420
042500160420     c* se bolla in partenza, devo ricostruire il record "L"
042600160420    2c                   if        wtrasm='N'
042700160420    3c                   if        d98tc1 = 'A' or d98tc1 = 'S' or d98tc2 = 'A'
042800160420     c                             or d98tc2 = 'S'
042900160420     c                   eval      §b4lbas = 'S'
043000160420      * se consegna tassativa sblocco
043100160420    4c                   if        d98dcr <> *zeros and d98tcr = *blanks
043200160420     c                   clear                   §b4lbas
043300160420    4c                   endif
043400160420    3c                   endif
043500160420
043600160420    3c                   if        d98ffd<>' '
043700160420     c                   eval      §b4ltfd = 'S'
043800160420    3c                   endif
043900160420
044000160420    2c                   endif
044100160420
044200050908     c* Se esiste la data consegnabilità, faccio finta che la bolla
044300050908     c*  non si consegnata, azzerando proprio la dta di consegna
044400050908     c*  perchè può trarre in inganno
044500050908     c                   if        d98dfc>0 and d98dcm>0
044600050908     c                   clear                   d98dcm
044700050908     c                   clear                   d98hmc
044800050908     c                   clear                   d98cca
044900050908     c                   endif
045000090902     c*
045100090902     c* Se non c'e' nè data consegna, ne data arrivo trazione
045200090902     c*  faccio finta che data arrivo trazione= udate utime ai fini dei calcoli
045300090902     c* in fondo al pgm dovrò azzerare i tempi di consegna interni e cliente
045400090902     c* perchè non calcolabili
045500090902     c                   clear                   wmancaTRAZ        1
045600090902     c
045700010821     C*
045800010821     C* EFFETTUO SUBITO CONSIDERAZIONI SU DIVISE (X QUESTIONE EURO)
045900010821     C                   EXSR      CHKVCA
046000141204     c
046100141204     c* Controllo se ha pratica di DANNO aperta
046200141204     c                   EXSR      CHKDAN
046300010108     C*
046400010108     C* SE BOLLA POSTA, UTILIZZA LA DATA BORDERO' ANZICHE' LA DATA SPEDIZIONE
046500091009     C**   D98TSP        IFEQ      'P'                                          *BOLLA POSTA
046600091009     C**   D98DBR        IFGT      *ZEROS                                       *DATA BORDERO' PRESE
046700091009     C**                 Z-ADD     D98DBR        D98DSP                         *DATA BORDERO' --> D
046800091009     C**                 ENDIF
046900091009     C**                 ENDIF
047000930304     C*
047100950110     C* RECUPERO I TEMPI TEORICI DI PARTENZA, TRAZIONE E DI CONSEGNA
047200930420     C     D98ERR        IFEQ      '0'
047300930323     C                   EXSR      RTTEOR
047400930420     C                   END
047500930528     C*
047600930602     C     D98ERR        IFEQ      '0'                                          --- 4 -->
047700970304     C*
047800970304     C* SE RICHIESTO LANCIO CON DATA RIFERIMENTO CONTROLLO LE DATE DA AZZERARE
047900970304     C     D98ERR        IFEQ      '0'
048000970304     C                   EXSR      DATRIF
048100970304     C                   END
048200940901     C*
048300940901     C* TRASF. DATA ARRIVO TRAZIONE E RICONSIDERO TEMPO CONSEGNA SE RITARDO
048400940901     C     D98ERR        IFEQ      '0'
048500940901     C                   EXSR      TDTIPO
048600940901     C                   END
048700930420     C*
048800930511     C* CARICO IL FILE DEGLI EVENTI BOLLA
048900930420     C     D98ERR        IFEQ      '0'
049000041116     c     d98nsp        andgt     0
049100930420     C                   EXSR      CAREVB
049200930420     C                   END
049300931213     C*
049400931213     C* CARICO LA SCHIERA DEI TURNI DI CHIUSURA
049500931213     C     D98ERR        IFEQ      '0'
049600931213     C                   EXSR      CARTUC
049700931213     C                   END
049800930528     C*
049900930602     C     D98ERR        IFEQ      '0'                                          --- 5 -->
050000930405     C*
050100940901     C* TRASFORMO LE DATE (SPED,CONS,ECC.) IN PROGRESSIVI ORE
050200930420     C     D98ERR        IFEQ      '0'
050300930405     C                   EXSR      TDATPO
050400930420     C                   END
050500930528     C*
050600930602     C     D98ERR        IFEQ      '0'                                          --- 6 -->
050700950110     C*
050800950110     C* CALCOLA LE DATE DI PREVISTA PARTENZA DELL'ULTIMO COLLO
050900950110     C     D98ERR        IFEQ      '0'
051000950110     C                   EXSR      CALDPP
051100950110     C                   END
051200930405     C*
051300930511     C* CALCOLA LE DATE DI PREVISTO ARRIVO DELLA TRAZIONE
051400930420     C     D98ERR        IFEQ      '0'
051500930511     C                   EXSR      CALDPT
051600090902     c*
051700090902     c* Ricalcolo le date facendo finta che la trazione stia arrivando
051800090903     c*  in questo momento se non presente
051900090902     c                   if        ricalcoloTRAZ='S'
052000090902     c                   eval      d98dti=WDAODI
052100090902     c                   eval      d98hti=WHHODI
052200090902     c                   eval      wmancaTRAZ='S'
052300090902     c
052400090902     C                   EXSR      TDTIPO
052500090902     C                   EXSR      CALDPP
052600090902     C                   EXSR      CALDPT
052700090902     c                   endif
052800090902     c
052900930420     C                   END
053000960229     C*
053100091009     d* D98TLA = 'T' --> in output solo i tempi di trazione
053200960229     C     D98TLA        IFNE      'T'                                          --- 6A ->
053300060407     C*
053400931206     C* CALCOLA IL RANGE PER IL CONTRASSEGNO
053500931206     C     D98ERR        IFEQ      '0'
053600931206     C                   EXSR      CALASS
053700931206     C                   END
053800931206     C*
053900931206     C* CALCOLA IL RANGE PER IL FERMO DEPOSITO
054000931206     C     D98ERR        IFEQ      '0'
054100931206     C                   EXSR      CALFED
054200931206     C                   END
054300931206     C*
054400931206     C* CALCOLA LE DATE TEORICHE DI CONSEGNA
054500931206     C     D98ERR        IFEQ      '0'
054600931206     C                   EXSR      CALDTE
054700931206     C                   END
054800930511     C*
054900930511     C* CALCOLA IL RANGE PER DATA CONSEGNA RICHIESTA
055000930511     C     D98ERR        IFEQ      '0'
055100930511     C                   EXSR      CALDCR
055200930511     C                   END
055300930511     C*
055400930511     C* CALCOLA I RANGE PER LASCIATO AVVISO
055500930511     C     D98ERR        IFEQ      '0'
055600930511     C                   EXSR      CALLAV
055700930511     C                   END
055800930511     C*
055900930511     C* CALCOLA IL RANGE PER LA GIACENZA
056000930511     C     D98ERR        IFEQ      '0'
056100930511     C                   EXSR      CALGIA
056200930511     C                   END
056300060420     C* CALCOLA IL RANGE PER Eventi Particolari che Allungano
056400060420     C     D98ERR        IFEQ      '0'
056500060420     C                   EXSR      CALcepa
056600060420     C                   END
056700930511     C*
056800930511     C* CALCOLA LE DATE DI PREVISTA CONSEGNA
056900930511     C     D98ERR        IFEQ      '0'
057000930511     C                   EXSR      CALDPC
057100930511     C                   END
057200960229     C*
057300960229     C                   ENDIF                                                  <-- 6A --
057400960229     C*
057500930602     C* TRASFORMO I PROGRESSIVI ORE IN DATE
057600930602     C                   EXSR      TPODAT
057700930602     C*
057800930914     C* CONTROLLO SE BOLLA RIENTRA IN UNO DEI CASI IN CUI NON RIESCO A CALCOLA
057900930602     C* RE I RITARDI PER CUI FORZO CONFORMITA' A SI
058000930602     C     D98ERR        IFEQ      '0'
058100960229     C     D98TLA        ANDNE     'T'
058200930602     C                   EXSR      CHKFOR
058300930602     C                   END
058400930602     C*
058500930602     C     D98ERR        IFEQ      '0'                                          --- 7 -->
058600960229     C     D98TLA        ANDNE     'T'
058700941228     C*
058800941228     C* CALCOLA LE ORE REALI DI PARTENZA
058900941228     C     D98ERR        IFEQ      '0'
059000941228     C                   EXSR      CALORP
059100941228     C                   END
059200930405     C*
059300930511     C* CALCOLA LE ORE REALI DI TRAZIONE
059400930420     C     D98ERR        IFEQ      '0'
059500930407     C                   EXSR      CALORT
059600930420     C                   END
059700930405     C*
059800930511     C* CALCOLA LE ORE REALI DI CONSEGNA
059900930420     C     D98ERR        IFEQ      '0'
060000930416     C                   EXSR      CALORC
060100930420     C                   END
060200930416     C*
060300930511     C* CALCOLA LE ORE REALI DI RESA
060400930420     C     D98ERR        IFEQ      '0'
060500930416     C                   EXSR      CALORR
060600930420     C                   END
060700930420     C*
060800930511     C* CALCOLA LE ORE DI DIFFERENZA REALI/TEORICHE
060900930420     C     D98ERR        IFEQ      '0'
061000930420     C                   EXSR      CALDIF
061100930420     C                   END
061200930420     C*
061300930511     C* CALCOLA LE CONFORMITA'
061400930420     C     D98ERR        IFEQ      '0'
061500930420     C                   EXSR      CALCNF
061600930420     C                   END
061700930913     C*
061800930913     C* CALCOLA LE ORE DI CONSEGNABILITA'
061900930913     C     D98ERR        IFEQ      '0'
062000950407     C     FLCONS        ANDEQ     'S'
062100930913     C                   EXSR      CALCNS
062200930913     C                   END
062300930423     C*
062400930511     C* PORTO I VALORI DAI CAMPI DI LAVORO AI CAMPI IN OUTPUT DELLA DS
062500930423     C     D98ERR        IFEQ      '0'
062600930423     C                   EXSR      W98D98
062700930423     C                   END
062800930528     C*
062900930602     C                   END                                                    <-- 7 ---
063000930602     C                   END                                                    <-- 6 ---
063100930528     C                   END                                                    <-- 5 ---
063200930528     C                   END                                                    <-- 4 ---
063300930528     C                   END                                                    <-- 3 ---
063400930528     C                   END                                                    <-- 2 ---
063500930528     C                   END                                                    <-- 1 ---
063600930323     C*
063700930324     C* SE PGM LANCIATO CON 'C' (LANCI MULTIPLI) CHIUDE CON LR (CHIUDE FILE)
063800930527     C* E LANCIA TUTTI I PGM CHIAMATI PER CHIUDERE I FILE DA LORO APERTI
063900930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
064000930324     C     D98TLA        IFEQ      'C'
064100970714     C                   CLEAR                   SI95
064200970701     C                   MOVEL     'C'           I95TLA
064300020226     C                   CALL      'TISI95R'
064400970701     C                   PARM                    SI95
064500040809     c*
064600040809     c                   clear                   fidn12ds
064700040809     c                   eval      i12tla = 'C'
064800040809     c                   call      'FIDN12R'
064900040809     c                   parm                    fidn12ds
065000050523     c**
065100050523     c* controllo riempimento skiera CAK solo alla fine
065200050523     c**                 clear                   trul0sds
065300050523     c**                 eval      i0sski='CAK/CAM/CAP'
065400050523     c**                 eval      i0sele=%elem(CAK)
065500050523     c**                 if        windca>i0sele
065600050523     c**                 eval      i0spie=i0sele
065700050523     c**                 else
065800050523     c**                 eval      i0spie=windca
065900050523     c**                 endif
066000050523     c**                 eval      i0sfile='AZCLN00F  '
066100050523     c**                 eval      i0scod='     '
066200050523     c**                 eval      i0spgm='TNSD99R'
066300050523     c**                 movel     trul0sds      kpjbu
066400050523     c**                 call      'TRUL0SR'
066500050523     c**                 parm                    kpjba
066600050523     c
066700930323     C                   SETON                                        LR
066800930324     C                   ELSE
066900930324     C                   SETON                                        RT
067000930323     C                   END
067100941227     C********************************************************************
067200941227     C*   LETARB - LEGGE IL FILE FNARB (BOLLE ARRIVI)
067300941227     C********************************************************************
067400941227     C     LETARB        BEGSR
067500941227     C*
067600140820     C* LEGGO IL FILE FNARB01L
067700941227     C                   Z-ADD     D98AAS        ARBAAS
067800941227     C                   Z-ADD     D98LNP        ARBLNP
067900941227     C                   Z-ADD     D98NRS        ARBNRS
068000941227     C                   Z-ADD     D98NSP        ARBNSP
068100941228     C     KEYARB        CHAIN     FNARB01L                           99
068200941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
068300941227     C                   MOVEL     '1'           D98ERR
068400941227     C                   MOVEL     MSG(20)       D98MSG                         *BOLLA INESISTENTE
068500941227     C                   END                                                    <-- 1 ---
068600941228     C     D98ERR        IFEQ      '0'                                          --- 1 --> *TROVATA
068700941227     C                   MOVEL     ARBTSP        D98TSP
068800140820     c* Se forzato da Ds lo imposto al posto di quello della bolla
068900130529     c                   if        i98tsp_for<>' '
069000130529     C                   MOVEL     i98tsp_for    D98TSP
069100130529     c                   endif
069200941228     C                   MOVEL     ARBNZD        D98NZD
069300941227     C                   MOVEL     ARBCAD        D98CAD
069400970701     C                   MOVEL     ARBLOD        D98LOD
069500941227     C                   Z-ADD     ARBTFP        D98TFP
069600941227     C                   Z-ADD     ARBTFA        D98TFA
069700941227     C                   Z-ADD     ARBLNA        D98LNA
069800941227     C                   Z-ADD     ARBAAS        DSAAS
069900941227     C                   Z-ADD     ARBMGS        DSMGS
070000941227     C                   Z-ADD     DSDAT8        D98DSP
070100941227     C                   Z-ADD     ARBMGS        D98MGS
070200941227     C                   Z-ADD     ARBDTI        D98DTI
070300941227     C                   Z-ADD     ARBHTI        D98HTI
070400941227     C                   Z-ADD     ARBDCM        D98DCM
070500941227     C                   Z-ADD     ARBHMC        D98HMC
070600941227     C                   MOVEL     ARBGC1        D98GC1
070700941227     C                   MOVEL     ARBGC2        D98GC2
070800941227     C                   Z-ADD     ARBDCR        D98DCR
070900941227     C                   Z-ADD     ARBHCR        D98HCR
071000100308     C                   MOVEL     ARBTCR        D98TCR
071100100308     c* salvo i campi perchè in alcuni casi li clearo
071200100308     C                   Z-ADD     ARBDCR        s98DCR
071300100308     C                   Z-ADD     ARBHCR        s98HCR
071400100308     C                   MOVEL     ARBTCR        s98TCR
071500100308     c
071600941227     C                   MOVEL     ARBCMC        D98CMC
071700941227     C                   MOVEL     ARBFFD        D98FFD
071800941227     C                   Z-ADD     ARBNCD        D98NCD
071900941227     C                   MOVEL     ARBFNS        D98FNS
072000941227     C                   Z-ADD     ARBDUC        D98DUC
072100150508     C                   Z-ADD     ARBDPC        W98DPC
072200970310     C                   Z-ADD     *ZEROS        W98HUC
072300071122     c
072400071122     c                   movel     arbcbo        d98cbo
072500140820     c                   z-add     arbddc        d98ddc
072600140825     c                   z-add     arbndc        wd98ndc
072700140911     c                   movel     arbfbc        wd98fbc
072800150119     c                   movel     arbacc        wd98acc
072900150325     c                   exsr      IN_dist
073000150331     c* Se in fermo deposito vedo se presente particlarità consegna
073100150331     c                   if        arbffd='S' and arbgma<>*blanks
073200150331     C                   Z-ADD     CODUT         KTBUTE
073300150331     C                   MOVEL     '7R'          KTBCOD
073400150331     C                   MOVEL(P)  arbgma        KTBKEY
073500150331     C     KEYTAB        chain     TABEL00F
073600150331     c                   if        %found(tabel00f)
073700150331     c                   movel     tbluni        ds7r
073800150331     c                   endif
073900150331     c                   endif
074000150325     c
074100070511     c
074200141215     C                   Z-ADD     ARBDBR        D98DBR
074300141215     c                   eval      d98fdrt=' '
074400070511     c
074500141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
074600070511     c* La data ritiro è presente solo su fnblp
074700070511     C     KEYARB        CHAIN     FNblp01L                           99
074800141215     c                   if        %found(fnblp01l)
074900070511     C                   Z-ADD     blpDrt        D98DBR
075000070511     c                   eval      d98fdrt='S'
075100070511     c                   endif
075200141215     c******             endif
075300070511     c
075400110923     C                   movel     ARBxco        D98xco
075500110923     C                   Z-ADD     ARBDAM        D98DAM
075600941227     C                   MOVEL     ARBFIN        D98FIN
075700990308     C                   MOVEL     ARBFBR        WFBR
075800031201     C                   MOVEL     ARBGGA        WGGA
075900050901     C                   MOVEL     ARBcca        d98cca
076000051124     C                   MOVEL     ARBTC1        d98TC1
076100051124     C                   MOVEL     ARBTC2        d98TC2
076200941227     C                   Z-ADD     1             I
076300941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
076400941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
076500941227     C                   MOVEL     '1'           D98ERR
076600941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
076700941227     C                   ELSE                                                   <-- 2 -->
076800941227     C                   MOVEL     T3A(I)        D98TBL
076900941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
077000990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
077100941227     C                   END                                                    <-- 2 ---
077200130826
077300130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
077400130826     c                   if        %subst(d98tbl:1:1) ='F'
077500130826     C                   Z-ADD     ARBKSC        D98KSC
077600130826     c                   else
077700130826     c
077800130826     c                   if        arbccm>0
077900130826     C                   Z-ADD     ARBccm        D98KSC
078000130826     c                   endif
078100130826     c                   if        d98ksc=0
078200130826     c                   z-add     8888          d98ksc
078300130826     c                   movel     d98lnp        d98ksc
078400130826     c                   endif
078500130826     c                   endif
078600130826     c
078700941227     C                   END                                                    <-- 1 ---
078800941228     C*
078900051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
079000950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
079100941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
079200941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
079300941228     C                   Z-ADD     D98AAS        AR9AAS
079400941228     C                   Z-ADD     D98LNP        AR9LNP
079500941228     C                   Z-ADD     D98NRS        AR9NRS
079600941228     C                   Z-ADD     D98NSP        AR9NSP
079700051115     C     KEYAR9        CHAIN     FiAR901L                           99
079800950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
079900950102     C                   MOVEL     'S'           WFLASS
080000950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
080100950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
080200950102     C                   Z-ADD     D98AAS        LBLAAN
080300950102     C                   Z-ADD     D98LNP        LBLLPN
080400950102     C                   Z-ADD     D98NRS        LBLNRN
080500950102     C                   Z-ADD     D98NSP        LBLNSN
080600950102     C     KEYLBL        CHAIN     FNLBL01L                           99
080700950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
080800950102     C                   Z-ADD     LBLAAP        AR9AAS
080900950102     C                   Z-ADD     LBLLPP        AR9LNP
081000950102     C                   Z-ADD     LBLNRP        AR9NRS
081100950102     C                   Z-ADD     LBLNSP        AR9NSP
081200051115     C     KEYAR9        CHAIN     FiAR901L                           99
081300950102     C     *IN99         IFEQ      '0'
081400950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
081500950102     C                   ENDIF
081600950102     C                   ENDIF                                                  <-- 3 ---
081700950102     C                   ENDIF                                                  <-- 2 ---
081800950102     C*
081900950102     C* CONTRASSEGNO NON TROVATO
082000950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
082100941228     C                   ELSE                                                   <-- 2 -->
082200950102     C* CONTRASSEGNO TROVATO
082300941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
082400010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
082500010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
082600941228     C                   ENDIF                                                  <-- 2 ---
082700941228     C                   ENDIF                                                  <-- 1 ---
082800150325     c
082900941227     C*
083000050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
083100941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
083200050405     C                   Z-ADD     D98AAS        KWGCPAAS
083300050405     C                   Z-ADD     D98LNP        KWGCPLNP
083400050405     C                   Z-ADD     D98NRS        KWGCPNRS
083500050405     C                   Z-ADD     D98NSP        KWGCPNSP
083600050405     C                   Z-ADD     *ZEROS        KWGCPFRG
083700050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
083800941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
083900941227     C     GCPATB        ANDNE     'A'
084000960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
084100960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
084200960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
084300050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
084400960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
084500960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
084600050405     C* O UGUALE A 35 INVECE DI 40)
084700960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
084800050405     C     GCPFAS        ANDGE     40
084900941227     C                   Z-ADD     GCPDCG        D98DCG
085000950112     C                   Z-ADD     GCPDCG        W98DCG
085100960123     C                   ENDIF
085200960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
085300960729     C     GCPFAS        ANDGE     35
085400070202     C     GCPFAS        ANDne     36
085500960729     C                   Z-ADD     GCPDCG        D98DCG
085600960729     C                   Z-ADD     GCPDCG        W98DCG
085700960729     C                   ENDIF
085800941228     C                   ENDIF
085900941228     C                   ENDIF                                                  <-- 1 ---
086000970911     C*
086100970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
086200970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
086300970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
086400050922     C*****              Z-ADD     D98DTI        W98DTI
086500971210     C                   Z-ADD     D98LNP        F
086600971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
086700970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
086800970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
086900150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
087000150120     c                   if        arbdcp=0 and d98dfc=0
087100150120     C                   Z-ADD     d98dam        D98DTI
087200150120     c                   endif
087300150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
087400150120     C                   Z-ADD     d98dam        D98DTI
087500150120     c                   endif
087600150120     c                   ENDIF
087700150120     c***
087800150120     C***                Z-ADD     D98AAS        ARTAAS
087900150120     C***                Z-ADD     D98LNP        ARTLNP
088000150120     C***                Z-ADD     D98NRS        ARTNRS
088100150120     C***                Z-ADD     D98NSP        ARTNSP
088200150120     C***  KEYART        SETLL     FNART01L                           99
088300150120     C**N99KEYART        READE     FNART01L                               99
088400150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
088500150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
088600970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
088700150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
088800970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
088900970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
089000150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
089100150120     C***  ARTDCM        ANDEQ     *ZEROS
089200970912     C* SE PRIMO REC. TROVATO, MEM. DATA
089300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
089400150120     C***                Z-ADD     ARTDAM        D98DTI
089500150120     C***                ELSE                                                   <-- 6 -->
089600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
089700150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
089800150120     C***                Z-ADD     ARTDAM        D98DTI
089900150120     C***                ENDIF                                                  <-- 7 ---
090000150120     C***                ENDIF                                                  <-- 6 ---
090100150120     C***                ENDIF                                                  <-- 5 ---
090200970911     C* C'E' DATA CONSEGNAB.
090300150120     C***                ELSE                                                   <-- 4 -->
090400970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
090500970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
090600150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
090700150120     C***  ARTDAM        ANDLE     D98DFC
090800970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
090900970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
091000150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
091100150120     C***  ARTDCM        ORGT      D98DFC
091200970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
091300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
091400150120     C***                Z-ADD     ARTDAM        D98DTI
091500150120     C***                ELSE                                                   <-- 7 -->
091600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
091700150120     C***  ARTDAM        IFLT      D98DTI
091800150120     C***                Z-ADD     ARTDAM        D98DTI
091900150120     C***                ENDIF
092000150120     C***                ENDIF                                                  <-- 7 ---
092100150120     C***                ENDIF                                                  <-- 6 ---
092200150120     C***                ENDIF                                                  <-- 5 ---
092300150120     C***                ENDIF                                                  <-- 4 ---
092400150120     C***                ENDIF                                                  <-- 3 ---
092500150120     C***  KEYART        READE     FNART01L                               99
092600150120     C***                ENDDO                                                  <-- 2 ---
092700150120     C***                ENDIF                                                  <-- 1 ---
092800941227     C*
092900941227     C                   ENDSR
093000150325     C********************************************************************
093100150325     C*   Verifico se bolla in distinta
093200150325     C********************************************************************
093300150325     c     IN_dist       BEGSR
093400150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
093500150325     c                   if         ARBfbc = *blanks and
093600150325     c                               ARBddc > 0 and
093700150325     c                               ARBndc > 0 and ARBdcm=0
093800150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
093900150325     c                   if        not %open(fiapd01l)
094000150325     c                   open      fiapd01l
094100150325     c                   endif
094200150325     c
094300150325     C                   move      'A'           kpdtip
094400150325     c                   eval      kpdpdr=arbpdc
094500150325     C     kfapd         chain     fiapd01l
094600150325     c
094700150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
094800150325     c                   eval       d98cons = 'S'
094900150325     c                   endif
095000150325     c                   endif
095100150325     c                   ENDSR
095200941227     C********************************************************************
095300941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
095400941227     C********************************************************************
095500941227     C     LETBLP        BEGSR
095600941227     C*
095700941227     C                   Z-ADD     D98AAS        BLPAAS
095800941227     C                   Z-ADD     D98LNP        BLPLNP
095900941227     C                   Z-ADD     D98NRS        BLPNRS
096000941227     C                   Z-ADD     D98NSP        BLPNSP
096100941227     C     KEYBLP        CHAIN     FNBLP01L                           99
096200941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
096300941227     C                   MOVEL     '1'           D98ERR
096400941227     C                   MOVEL     MSG(20)       D98MSG
096500941227     C                   END                                                    <-- 1 ---
096600941228     C     *IN99         IFEQ      '0'                                                    *TROV.
096700041028     C     KEYBLP        CHAIN     FNarb01L
096800041028     c                   if        %found(fnarb01l)
096900041028     c                   eval      wesarb='S'
097000041028     c                   else
097100041028     c                   eval      wesarb='N'
097200150219     c                   eval      wtrasm='D'
097300041028     c                   endif
097400141216     c
097500141216     c* se non trasmessa la bolla in arrivo memorizzo
097600150219     c                   if        blpft1= ' '
097700141229     c                   eval      wtrasm='N'
097800141216     c                   endif
097900141216     c
098000110923     C                   MOVEL     BLPxco        D98xco
098100110923     C                   MOVEL     BLPTSP        D98TSP
098200130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
098300130529     c                   if        i98tsp_for<>' '
098400130529     C                   MOVEL     i98tsp_for    D98TSP
098500130529     c                   endif
098600960209     C                   MOVEL     BLPNZD        D98NZD
098700941228     C                   MOVEL     BLPCAD        D98CAD
098800970701     C                   MOVEL     BLPLOD        D98LOD
098900941227     C                   Z-ADD     BLPTFP        D98TFP
099000941227     C                   Z-ADD     BLPTFA        D98TFA
099100941227     C                   Z-ADD     BLPLNA        D98LNA
099200941227     C                   Z-ADD     BLPAAS        DSAAS
099300941227     C                   Z-ADD     BLPMGS        DSMGS
099400941227     C                   Z-ADD     DSDAT8        D98DSP
099500941227     C                   Z-ADD     BLPMGS        D98MGS
099600051124     C                   movel     BLPTC1        D98TC1
099700051124     C                   movel     BLPTC2        D98TC2
099800041028     c                   if        wesarb='S'
099900041028     C                   Z-ADD     arbDTI        D98DTI
100000041028     C                   Z-ADD     arbHTI        D98HTI
100100041028     C                   Z-ADD     arbDCM        D98DCM
100200041028     C                   Z-ADD     arbHMC        D98HMC
100300041028     C                   Z-ADD     arbDAM        D98DAM
100400050901     C                   MOVEL     arbcca        d98cca
100500140820     C                   MOVEL     arbddc        d98ddc
100600140911     C                   z-add     arbndc        wd98ndc
100700140910     C                   MOVEL     arbfbc        wd98fbc
100800150119     c                   movel     arbacc        wd98acc
100900150325     c                   exsr      IN_DIST
101000140820     c*
101100041028     c                   else
101200941227     C                   Z-ADD     BLPDTI        D98DTI
101300941227     C                   Z-ADD     BLPHTI        D98HTI
101400941227     C                   Z-ADD     BLPDCM        D98DCM
101500941227     C                   Z-ADD     BLPHMC        D98HMC
101600041028     C                   Z-ADD     BLPDAM        D98DAM
101700050901     C                   MOVEL     BLPcca        d98cca
101800140820     C                   MOVEL     BLPddc        d98ddc
101900140825     C                   MOVEL     BLPndc        wd98ndc
102000140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
102100140820     c                   clear                   d98cons
102200041028     c                   endif
102300941227     C                   MOVEL     BLPGC1        D98GC1
102400941227     C                   MOVEL     BLPGC2        D98GC2
102500941227     C                   Z-ADD     BLPDCR        D98DCR
102600941227     C                   Z-ADD     BLPHCR        D98HCR
102700941227     C                   MOVEL     BLPTCR        D98TCR
102800100308     c* Salvo i campi perchè in alcuni casi li clearo
102900100308     C                   Z-ADD     blpDCR        s98DCR
103000100308     C                   Z-ADD     blpHCR        s98HCR
103100100308     C                   MOVEL     blpTCR        s98TCR
103200100308     c
103300941227     C                   MOVEL     *BLANKS       D98CMC
103400941227     C                   MOVEL     BLPFFD        D98FFD
103500941227     C                   Z-ADD     BLPNCD        D98NCD
103600941227     C                   MOVEL     BLPFNS        D98FNS
103700941228     C                   Z-ADD     BLPDUC        D98DUC
103800150508     C                   Z-ADD     BLPDPC        w98DPC
103900970310     C                   Z-ADD     *ZEROS        W98HUC
104000071122     c                   movel     blpcbo        d98cbo
104100150331     c*
104200150331     c* Se in fermo deposito vedo se presente particlarità consegna
104300150331     c                   if        blpffd='S' and blpgma<>*blanks
104400150331     C                   Z-ADD     CODUT         KTBUTE
104500150331     C                   MOVEL     '7R'          KTBCOD
104600150331     C                   MOVEL(P)  blpgma        KTBKEY
104700150331     C     KEYTAB        chain     TABEL00F
104800150331     c                   if        %found(tabel00f)
104900150331     c                   movel     tbluni        ds7r
105000150331     c                   endif
105100150331     c                   endif
105200150331     c
105300970304     C                   Z-ADD     BLPDBR        D98DBR
105400070514     c                   eval      d98fdrt=' '
105500070514     c
105600141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
105700070514     C                   Z-ADD     blpDrt        D98DBR
105800070514     c                   eval      d98fdrt='S'
105900141215     c********           endif
106000070514     c*
106100941227     C                   MOVEL     BLPFIN        D98FIN
106200130826     C****               Z-ADD     BLPKSC        D98KSC
106300990308     C                   MOVEL     BLPFBR        WFBR
106400031201     C                   MOVEL     BLPGGA        WGGA
106500941227     C                   Z-ADD     1             I
106600941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
106700941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
106800941227     C                   MOVEL     '1'           D98ERR
106900941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
107000941227     C                   ELSE                                                   <-- 2 -->
107100941227     C                   MOVEL     T3A(I)        D98TBL
107200941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
107300990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
107400941227     C                   END                                                    <-- 2 ---
107500130826     c                   if        %subst(d98tbl:1:1) ='F'
107600130826     C                   Z-ADD     blpKSC        D98KSC
107700130826     c                   else
107800130826     c
107900130826     c                   if        blpccm>0
108000130826     C                   Z-ADD     blpccm        D98KSC
108100130826     c                   endif
108200130826     c                   if        d98ksc=0
108300130826     c                   z-add     8888          d98ksc
108400130826     c                   movel     d98lnp        d98ksc
108500130826     c                   endif
108600130826     c                   endif
108700130826     c
108800941227     C                   END
108900941228     C*
109000051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
109100950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
109200941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
109300941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
109400051115     C                   Z-ADD     D98AAS        ar9AAS
109500051115     C                   Z-ADD     D98LNP        ar9LNP
109600051115     C                   Z-ADD     D98NRS        ar9NRS
109700051115     C                   Z-ADD     D98NSP        ar9NSP
109800051115     C     KEYar9        CHAIN     Fiar901L                           99
109900950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
110000950102     C                   MOVEL     'S'           WFLASS
110100950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
110200950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
110300950102     C                   Z-ADD     D98AAS        LBLAAN
110400950102     C                   Z-ADD     D98LNP        LBLLPN
110500950102     C                   Z-ADD     D98NRS        LBLNRN
110600950102     C                   Z-ADD     D98NSP        LBLNSN
110700950102     C     KEYLBL        CHAIN     FNLBL01L                           99
110800950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
110900051115     C                   Z-ADD     LBLAAP        ar9AAS
111000051115     C                   Z-ADD     LBLLPP        ar9LNP
111100051115     C                   Z-ADD     LBLNRP        ar9NRS
111200051115     C                   Z-ADD     LBLNSP        ar9NSP
111300051115     C     KEYar9        CHAIN     Fiar901L                           99
111400950102     C     *IN99         IFEQ      '0'
111500950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
111600950102     C                   ENDIF
111700950102     C                   ENDIF                                                  <-- 3 ---
111800950102     C                   ENDIF                                                  <-- 2 ---
111900950102     C*
112000950102     C* CONTRASSEGNO TROVATO
112100150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
112200051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
112300051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
112400051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
112500941228     C                   ENDIF                                                  <-- 2 ---
112600941228     C                   ENDIF                                                  <-- 1 ---
112700941227     C*
112800050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
112900941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
113000050405     C                   Z-ADD     D98AAS        KWGCPAAS
113100050405     C                   Z-ADD     D98LNP        KWGCPLNP
113200050405     C                   Z-ADD     D98NRS        KWGCPNRS
113300050405     C                   Z-ADD     D98NSP        KWGCPNSP
113400050405     C                   Z-ADD     *ZEROS        KWGCPFRG
113500050405     C     KEYGCP        CHAIN     TIGCP01L                           99
113600941228     C     *IN99         IFEQ      '0'                                          *TROV.
113700941227     C     GCPATB        ANDNE     'A'
113800960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
113900960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
114000960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
114100960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
114200050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
114300960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
114400960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
114500050405     C* O UGUALE A 35 INVECE DI 40)
114600960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
114700050405     C     GCPFAS        ANDGE     40
114800941227     C                   Z-ADD     GCPDCG        D98DCG
114900950112     C                   Z-ADD     GCPDCG        W98DCG
115000960123     C                   ENDIF
115100960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
115200960729     C     GCPFAS        ANDGE     35
115300070202     C     GCPFAS        ANDne     36
115400960729     C                   Z-ADD     GCPDCG        D98DCG
115500960729     C                   Z-ADD     GCPDCG        W98DCG
115600960729     C                   ENDIF
115700941228     C                   ENDIF
115800941228     C                   ENDIF                                                  <-- 1 ---
115900970912     C*
116000970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
116100970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
116200970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
116300050922     C******             Z-ADD     D98DTI        W98DTI
116400971210     C                   Z-ADD     D98LNP        F
116500971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
116600970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
116700970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
116800150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
116900150120     c                   if        blpdcp=0 and d98dfc=0
117000150120     C                   Z-ADD     d98dam        D98DTI
117100150120     c                   endif
117200150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
117300150120     C                   Z-ADD     d98dam        D98DTI
117400150120     c                   endif
117500150120     c                   ENDIF
117600150120     c*
117700150120     C***                Z-ADD     D98AAS        BLTAAS
117800150120     C****               Z-ADD     D98LNP        BLTLNP
117900150120     C***                Z-ADD     D98NRS        BLTNRS
118000150120     C***                Z-ADD     D98NSP        BLTNSP
118100150120     C***  KEYBLT        SETLL     FNBLT01L                           99
118200150120     C**N99KEYBLT        READE     FNBLT01L                               99
118300150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
118400150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
118500970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
118600150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
118700970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
118800970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
118900150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
119000150120     C**   BLTDCM        ANDEQ     *ZEROS
119100970912     C* SE PRIMO REC. TROVATO, MEM. DATA
119200150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
119300150120     C**                 Z-ADD     BLTDAM        D98DTI
119400150120     C**                 ELSE                                                   <-- 6 -->
119500970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
119600150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
119700150120     C**                 Z-ADD     BLTDAM        D98DTI
119800150120     C**                 ENDIF                                                  <-- 7 ---
119900150120     C**                 ENDIF                                                  <-- 6 ---
120000150120     C**                 ENDIF                                                  <-- 5 ---
120100970912     C* C'E' DATA CONSEGNAB.
120200150120     **                  ELSE                                                   <-- 4 -->
120300970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
120400970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
120500150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
120600150120     C**   BLTDAM        ANDLT     D98DFC
120700970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
120800970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
120900150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
121000150120     C**   BLTDCM        ORGT      D98DFC
121100970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
121200150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
121300150120     C**                 Z-ADD     BLTDAM        D98DTI
121400150120     C**                 ELSE                                                   <-- 7 -->
121500970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
121600150120     C**   BLTDAM        IFLT      D98DTI
121700150120     C**                 Z-ADD     BLTDAM        D98DTI
121800150120     C**                 ENDIF
121900150120     C**                 ENDIF                                                  <-- 7 ---
122000150120     C**                 ENDIF                                                  <-- 6 ---
122100150120     C**                 ENDIF                                                  <-- 5 ---
122200150120     C**                 ENDIF                                                  <-- 4 ---
122300150120     C**                 ENDIF                                                  <-- 3 ---
122400150120     C**   KEYBLT        READE     FNBLT01L                               99
122500150120     C**                 ENDDO                                                  <-- 2 ---
122600150120     C**                 ENDIF                                                  <-- 1 ---
122700941227     C*
122800941227     C                   ENDSR
122900930323     C********************************************************************
123000941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
123100930323     C********************************************************************
123200930323     C     RTTEOR        BEGSR
123300941227     C*
123400941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
123500941228     C     D98ERR        IFEQ      '0'
123600941227     C                   EXSR      RTPART
123700941227     C                   ENDIF
123800941227     C*
123900941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
124000941228     C     D98ERR        IFEQ      '0'
124100150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
124200150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
124300150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
124400150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
124500150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
124600941227     C                   EXSR      RTTRAZ
124700150219     c                   if        d98err<>'1'
124800150219     C                   Z-ADD     CASTIN        D98TTI
124900150219     C                   Z-ADD     CASTCN        D98TTC
125000150219     C                   endif
125100150219     C                   ENDIF
125200941227     C*
125300941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
125400941228     C     D98ERR        IFEQ      '0'
125500941227     C                   EXSR      RTCONS
125600941227     C                   ENDIF
125700941227     C*
125800941228     C     D98ERR        IFEQ      '0'
125900941227     C*
126000941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
126100941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
126200941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
126300090921     c                   z-add     d98tri        w99tri
126400090921     c* per la DS
126500090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
126600090921     c* per i calcoli interi
126700090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
126800090921     c
126900941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
127000941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
127100941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
127200090921     c                   z-add     d98trc        w99trc
127300090921     c* per la DS
127400090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
127500090921     c* per i calcoli interi
127600090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
127700941227     C*
127800941227     C                   ENDIF
127900930323     C*
128000930323     C                   ENDSR
128100941227     C********************************************************************
128200941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
128300941227     C********************************************************************
128400941227     C     RTPART        BEGSR
128500941228     C*
128600941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
128700941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
128800941228     C                   MOVEL     D98TSP        WTSP              1
128900130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
129000130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
129100050301     C*******
129200050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
129300050301     C*
129400050301     C*    W001          IFEQ      'A'
129500050301     C*    W0040         OREQ      8888
129600050301     C*                  MOVEL     'E'           WTSP
129700050301     C*                  ENDIF
129800050301     C*******
129900941227     C*
130000941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
130100941227     C                   MOVEL     'N'           WTRAOK            1
130200941227     C                   MOVEL     'P'           CASPTA
130300941227     C                   Z-ADD     D98LNP        CASLNP
130400941227     C                   Z-ADD     *ZEROS        CASLNA
130500941228     C                   MOVEL     WTSP          CASTSP
130600941228     C                   Z-ADD     99999999      CASDDE
130700941227     C     KEYCAS        SETGT     AZCAS01L                             99
130800941227     C  N99              READP     AZCAS01L                               99
130900941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
131000941227     C     WTRAOK        ANDEQ     'N'
131100941227     C     CASLNP        ANDEQ     D98LNP
131200950102     C     CASTSP        ANDEQ     WTSP
131300941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
131400941227     C     CASATB        ANDEQ     *BLANKS
131500941227     C                   MOVEL     'S'           WTRAOK
131600941227     C                   ELSE                                                   <-- 2 -->
131700941227     C                   READP     AZCAS01L                               99
131800941227     C                   ENDIF                                                  <-- 2 ---
131900941227     C                   ENDDO                                                  <-- 1 ---
132000941227     C*
132100941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
132200941227     C     WTRAOK        IFEQ      'N'
132300941227     C                   MOVEL     '1'           D98ERR
132400941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
132500941227     C                   ELSE
132600941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
132700941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
132800941227     C                   END
132900970214     C*
133000140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
133100140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
133200140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
133300140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
133400140403     C**                 Z-ADD     D98LNA        F
133500140403     C**   D98ERR        IFEQ      '0'
133600140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
133700140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
133800140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
133900140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
134000140403     C**                 ENDIF
134100140403     C**                 ENDIF
134200140402     c
134300140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
134400140402     c*  se tempi  = 0
134500140402     c                   if        wpick2='S' and d98tpi<=0
134600140402     c                   add       24            D98TPI
134700140402     c                   endif
134800941227     C*
134900941227     C                   ENDSR
135000941227     C********************************************************************
135100941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
135200941227     C********************************************************************
135300941227     C     RTTRAZ        BEGSR
135400941227     C*
135500941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
135600150219     c
135700150219     C                   Z-ADD     D98LNA        LTA(1)
135800150219     C                   Z-ADD     D98LNA        LTA(2)
135900150219     C                   Z-ADD     D98TFA        LTA(3)
136000941227     C                   Z-ADD     D98TFA        LTA(4)
136100941227     C*
136200941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
136300941227     C                   MOVEL     'N'           WTRAOK            1
136400941227     C                   Z-ADD     1             K                 3 0
136500941227     C*
136600941227     C* CICLO FINO A:
136700941227     C*    - FINE SCHIERA
136800941227     C*    - TROVATO TEMPI VALIDI
136900941227     C     K             DOUGT     4                                            --- 1 -->
137000941227     C     WTRAOK        OREQ      'S'
137100941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
137200941227     C     LTA(K)        ORNE      *ZEROS
137300941227     C                   MOVEL     'T'           CASPTA
137400941227     C                   Z-ADD     LTP(K)        CASLNP
137500941227     C                   Z-ADD     LTA(K)        CASLNA
137600941227     C                   MOVEL     D98TSP        CASTSP
137700941228     C                   Z-ADD     99999999      CASDDE
137800941227     C     KEYCAS        SETGT     AZCAS01L                             99
137900941227     C  N99              READP     AZCAS01L                               99
138000941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
138100941227     C     WTRAOK        ANDEQ     'N'
138200941227     C     LTP(K)        ANDEQ     CASLNP
138300941227     C     LTA(K)        ANDEQ     CASLNA
138400941227     C     D98TSP        ANDEQ     CASTSP
138500960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
138600941227     C     CASATB        ANDEQ     *BLANKS
138700941227     C                   MOVEL     'S'           WTRAOK
138800941227     C                   ELSE                                                   <-- 4 -->
138900941227     C                   READP     AZCAS01L                               99
139000941227     C                   ENDIF                                                  <-- 4 ---
139100941227     C                   ENDDO                                                  <-- 3 ---
139200941227     C                   ENDIF                                                  <-- 2 ---
139300941227     C                   ADD       1             K
139400941227     C                   ENDDO                                                  <-- 1 ---
139500941227     C*
139600941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
139700941227     C     WTRAOK        IFEQ      'N'
139800941227     C                   MOVEL     '1'           D98ERR
139900941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
140000150219     C***                ELSE
140100150219     C***                Z-ADD     CASTIN        D98TTI
140200150219     C***                Z-ADD     CASTCN        D98TTC
140300941227     C                   END
140400941227     C*
140500941227     C                   ENDSR
140600941227     C********************************************************************
140700941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
140800941227     C********************************************************************
140900941227     C     RTCONS        BEGSR
141000150629     c                   clear                   WTsogliaE         1
141100970701     C*
141200970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
141300970701     C                   CLEAR                   SI95
141400970701     C                   MOVEL     '3'           I95TCN
141500970701     C                   MOVEL     D98NZD        I95NAR
141600970701     C                   MOVEL     D98CAD        I95CAP
141700970701     C                   MOVEL     D98LOD        I95LOC
141800970701     C                   MOVEL     D98DSP        I95DAT
141900970701     C                   MOVEL     D98TSP        I95TSP
142000980602     C                   MOVEL     'S'           I95FRT
142100150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
142200150511     c                   if        d98ffd='S'
142300150511     c                   eval      i95tsp='C'
142400150511     c                   endif
142500020226     C*
142600020226     C* VERIFICO SE LNA E' FILIALE FEDEX
142700020226     C                   IF        D98LNA > *ZEROS
142800020226     C     D98LNA        LOOKUP    FILFED                                 55
142900020226     C                   IF        *IN55 = *ON
143000020226     C                   EVAL      I95FI1 = 'S'
143100020226     C                   ENDIF
143200020226     C                   ENDIF
143300020226     C*
143400020226     C                   CALL      'TISI95R'
143500970701     C                   PARM                    SI95
143600941227     C*
143700941228     C* SE NON TROVATO, ERRORE
143800970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
143900941227     C                   MOVEL     '1'           D98ERR
144000970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
144100941227     C                   ELSE                                                   <-- 1 -->
144200941227     C*
144300941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
144400941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
144500941227     C     CASTIN        DIV       24            WDIV              3 0
144600941227     C                   MVR                     WRES              3 0
144700941227     C     WRES          IFEQ      *ZEROS
144800970701     C                   Z-ADD     O950IN        D98TCI
144900941227     C                   ELSE
145000970701     C                   Z-ADD     O951IN        D98TCI
145100941227     C                   END
145200941227     C*
145300941227     C     CASTCN        DIV       24            WDIV                     99
145400941227     C                   MVR                     WRES
145500941227     C     WRES          IFEQ      *ZEROS
145600970701     C                   Z-ADD     O950CN        D98TCC
145700150629     c                   eval      wTSogliaE='0'
145800941227     C                   ELSE
145900970701     C                   Z-ADD     O951CN        D98TCC
146000150629     c                   eval      wTSogliaE='1'
146100941227     C                   END
146200941227     C*
146300150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
146400941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
146500150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
146600150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
146700150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
146800150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
146900150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
147000941227     C*
147100941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
147200941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
147300970701     C     O95ISO        IFEQ      'I'
147400941227     C     D98FIN        OREQ      'I'
147500970701     C                   ADD       O95TIS        D98TCI
147600970701     C                   ADD       O95TIS        D98TCC
147700941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
147800970701     C                   ADD       O95TIS        W990IN
147900970701     C                   ADD       O95TIS        W991IN
148000150629     C                   ADD       O95TIS        W990CN
148100150629     C                   ADD       O95TIS        W991CN
148200941227     C                   END
148300090827     c
148400090827     c* Sistemo i tempi di consegna se non sono multipli di 12
148500090827     c                   exsr      SisCONS
148600160404     c
148700160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
148800160404     c                   if        wVP='S'
148900160404     c                   add       24            w991IN
149000160404     c                   add       24            w991CN
149100160404     c                   add       24            w990IN
149200160404     c                   add       24            w990CN
149300160404     c                   endif
149400941228     C*
149500941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
149600941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
149700941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
149800941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
149900941228     C                   Z-ADD     D98TCC        W99CCE            3 0
150000941228     C                   ELSE                                                   <-- 3 -->
150100970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
150200970701     C                   CLEAR                   SI95
150300970701     C                   MOVEL     '3'           I95TCN
150400970701     C                   MOVEL     D98NZD        I95NAR
150500970701     C                   MOVEL     D98CAD        I95CAP
150600970701     C                   MOVEL     D98LOD        I95LOC
150700970701     C                   MOVEL     D98DSP        I95DAT
150800970701     C                   MOVEL     'E'           I95TSP
150900980602     C                   MOVEL     'S'           I95FRT
151000020226     C*
151100020226     C* VERIFICO SE LNA E' FILIALE FEDEX
151200020226     C                   IF        D98LNA > *ZEROS
151300020226     C     D98LNA        LOOKUP    FILFED                                 55
151400020226     C                   IF        *IN55 = *ON
151500020226     C                   EVAL      I95FI1 = 'S'
151600020226     C                   ENDIF
151700020226     C                   ENDIF
151800020226     C*
151900020226     C                   CALL      'TISI95R'
152000970701     C                   PARM                    SI95
152100970701     C*
152200941228     C* SE NON TROVATO, ERRORE
152300970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
152400941228     C                   MOVEL     '1'           D98ERR
152500970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
152600941228     C                   ELSE                                                   <-- 4 -->
152700941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
152800941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
152900941228     C     CASTCN        DIV       24            WDIV                     99
153000941228     C                   MVR                     WRES
153100941228     C     WRES          IFEQ      *ZEROS
153200970701     C                   Z-ADD     O950CN        W99CCE
153300941228     C                   ELSE
153400970701     C                   Z-ADD     O951CN        W99CCE
153500941228     C                   END
153600941228     C                   ENDIF                                                  <-- 4 ---
153700941228     C                   ENDIF                                                  <-- 3 ---
153800941228     C                   ENDIF                                                  <-- 2 ---
153900941227     C*
154000941227     C                   END                                                    <-- 1 ---
154100941227     C*
154200941227     C                   ENDSR
154300090827     C********************************************************************
154400090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
154500090827     C********************************************************************
154600090827     C     SisCONS       BEGSR
154700090827     c                   eval      w99tci=d98tci
154800090827     c                   if        d98tci=10
154900090827     c                   z-add     12            w99tci
155000090827     c                   endif
155100090827     c
155200090827     c                   eval      w99tcc=d98tcc
155300090827     c                   if        d98tcc=10
155400090827     c                   z-add     12            w99tcc
155500090827     c                   endif
155600160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
155700160404     c                   if        wVP='S'
155800160404     c                   add       24            w99tcc
155900160404     c                   add       24            w99tci
156000160404     c                   endif
156100090827     c
156200090827     C                   ENDSR
156300930420     C********************************************************************
156400930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
156500930420     C********************************************************************
156600930420     C     CAREVB        BEGSR
156700930420     C*
156800930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
156900930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
157000060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
157100930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
157200930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
157300060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
157400060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
157500930420     C                   Z-ADD     D98AAS        KEBAAS
157600930420     C                   Z-ADD     D98LNP        KEBLNP
157700930420     C                   Z-ADD     D98NRS        KEBNRS
157800930420     C                   Z-ADD     D98NSP        KEBNSP
157900990810     C     KEYEVB        SETLL     FNEVB01L                           99
158000060406     C  N99keyevb        READe     FNEVB01L                               99
158100060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
158200930421     C     D98ERR        ANDEQ     '0'
158300060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
158400930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
158500930421     C                   Z-ADD     1             I
158600930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
158700060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
158800930421     C                   MOVEL     '1'           D98ERR
158900930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
159000060406   x3C                   ELSE                                                   <-- 3 -->
159100060406     C                   MOVEL     D2A(I)        usa2A
159200060406     C                   MOVEL     D2Z(I)        usa2Z
159300930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
159400060406    4C     §2AFTC        IFEQ      'A'
159500930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
159600060406    4C                   END
159700060406    4C     §2AFTC        IFEQ      'G'
159800930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
159900060406    4C                   END
160000150216     c* Se si tratta di MIC verifico se uguale alla sped.
160100150303    4c                   if        evbcev='MIC' or evbcev='NIC'
160200150303    5c                   if        %subst(evbnot:1:7) >*zeros
160300150303     c
160400150303    6c                   if        evbcev='MIC' and
160500150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
160600150216     c                   eval      evemic='SI'
160700150303    6c                   endif
160800150303     c
160900150303    6c                   if        evbcev='NIC' and
161000150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
161100150216     c                   eval      evemic='  '
161200150303    6c                   endif
161300150303     c
161400150303   x5c                   else
161500150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
161600150303     c                   eval      evemic='SI'
161700150303    6c                   endif
161800150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
161900150303     c                   eval      evemic='  '
162000150303    6c                   endif
162100150303
162200150303    5c                   endif
162300150303    4c                   endif
162400150216     c
162500060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
162600930421     C     §2AFTC        OREQ      'G'
162700930421     C                   ADD       1             IE
162800990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
162900930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
163000930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
163100131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
163200090928     c
163300090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
163400091014     c**                 if        ie=1
163500091014     c**                 eval      Primotipo=§2aftc
163600091014     c**                 eval      Primodev=evbdev
163700091014     c**                 eval      Primohev=evbhev
163800091014     c**                 endif
163900090928     c
164000060406    4C                   END                                                    <-- 4 ---
164100060406     c* Verifico se si tratta di evento particolare
164200060406    4c                   if        §2zCEP<>' '
164300060526     c* non carico l'evento se cade in giorno festivo
164400060526     c                   z-add     evbdev        wpodata
164500060526     C                   EVAL      WTC='C'
164600060526     C                   EVAL      WIC='C'
164700060526     c                   eval      wslitt='N'
164800060526     c                   exsr      chkfes
164900060526     c
165000060526     c* Wslitt='N' non festivo
165100060526   4ac                   if        wslitt='N'
165200060526     c
165300060406    5c                   if        §2zfep='A'
165400060406     c                   eval      wevpaA='S'
165500060406    5C                   END                                                    <-- 3 ---
165600060406    5c                   if        §2zfep='G'
165700060406     c                   eval      wevpaG='S'
165800060406    5C                   END                                                    <-- 3 ---
165900060406     c                   add       1             ip
166000060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
166100060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
166200060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
166300060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
166400060526   4aC                   ENDif                                                  <-- 3 ---
166500060526    4C                   END                                                    <-- 3 ---
166600060406    3C                   END                                                    <-- 3 ---
166700060406    2C                   END                                                    <-- 2 ---
166800060406     C     keyevb        READe     FNEVB01L                               99
166900060406    1C                   ENDDO                                                  <-- 1 ---
167000050523     c* controllo riempimento skiere
167100050523     c                   clear                   trul0sds
167200050523     c                   eval      i0sski='EVD/EVO/EVC'
167300050523     c                   eval      i0sele=%elem(EVD)
167400050523     c                   eval      i0spie=ie
167500050523     c                   eval      i0sfile='FNEVB01L  '
167600050523     c                   eval      i0scod='     '
167700050523     c                   eval      i0spgm='TNSD99R'
167800050523     c                   movel     trul0sds      kpjbu
167900050523     c                   call      'TRUL0SR'
168000050523     c                   parm                    kpjba
168100930420     C*
168200060406     c* controllo riempimento skiere
168300060406     c                   clear                   trul0sds
168400060406     c                   eval      i0sski='PVD/PVO/PVC'
168500060406     c                   eval      i0sele=%elem(PVD)
168600060406     c                   eval      i0spie=ie
168700060406     c                   eval      i0sfile='FNEVB01L  '
168800060406     c                   eval      i0scod='     '
168900060406     c                   eval      i0spgm='TNSD99R'
169000060406     c                   movel     trul0sds      kpjbu
169100060406     c                   call      'TRUL0SR'
169200060406     c                   parm                    kpjba
169300060406     C*
169400930420     C                   ENDSR
169500931213     C********************************************************************
169600931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
169700931213     C********************************************************************
169800931213     C     CARTUC        BEGSR
169900931213     C*
170000931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
170100931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
170200931214     C     D98GC2        ANDEQ     *BLANKS
170300931213     C                   ELSE                                                   <-- 1 -->
170400931213     C*
170500931213     C                   MOVE      D98GC1        TUC(1)
170600931214     C                   MOVE      D98GC2        TUC(2)
170700931213     C*
170800931213     C                   END                                                    <-- 1 ---
170900931213     C*
171000931213     C                   ENDSR
171100970304     C********************************************************************
171200970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
171300970304     C********************************************************************
171400970304     C     DATRIF        BEGSR
171500970304     C*
171600050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
171700050922     c*  partenza
171800970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
171900970304     C*
172000970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
172100070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
172200070605     C***                MOVEL     '1'           D98ERR
172300070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
172400070605     C***                ELSE                                                   <-- 2 -->
172500970304     C*
172600970304     C                   Z-ADD     D98DRF        WD1
172700970304     C                   Z-ADD     D98ORF        WO1
172800970310     C* DATA PARTENZA ULTIMO COLLO
172900970310     C     D98DRF        IFLE      D98DUC
173000050928     c     d98duc        oreq      0
173100970310     C                   Z-ADD     D98DRF        D98DUC
173200970310     C                   Z-ADD     D98ORF        W98HUC
173300070605     C                   ENDIF
173400970304     C* DATA ARRIVO TRAZIONE
173500050922     C***                Z-ADD     D98DTI        WD2
173600050922     C***                Z-ADD     D98HTI        WO2
173700050922     C***  WDO1          IFLE      WDO2
173800050922     C***                Z-ADD     D98DRF        D98DTI
173900050922     C***                Z-ADD     D98ORF        D98HTI
174000050922     C***                ENDIF
174100970304     C* DATA CONSEGNA
174200050922     C***                Z-ADD     D98DCM        WD2
174300050922     C***                Z-ADD     D98HMC        WO2
174400050922     C***  WDO1          IFLE      WDO2
174500050922     C***                Z-ADD     D98DRF        D98DCM
174600050922     C***                Z-ADD     D98ORF        D98HMC
174700050922     C***                ENDIF
174800970304     C*
174900070605     C***                ENDIF                                                  <-- 2 ---
175000970304     C                   ENDIF                                                  <-- 1 ---
175100970304     C*
175200970304     C                   ENDSR
175300940901     C********************************************************************
175400940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
175500940901     C********************************************************************
175600940901     C     TDTIPO        BEGSR
175700940901     C*
175800940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
175900940902     C                   Z-ADD     D98TTI        W98TTI            3 0
176000941011     C*
176100941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
176200941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
176300941011     C     W98TTI        DIV       24            WDIV              3 0
176400941011     C                   MVR                     WRES              3 0
176500941011     C     WRES          IFEQ      *ZEROS
176600941011     C                   Z-ADD     *ZEROS        WASTA             9 0
176700941011     C                   ELSE
176800941011     C                   Z-ADD     12            WASTA
176900941011     C                   END
177000940901     C*
177100940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
177200940901     C     D98DTI        IFNE      *ZEROS
177300940901     C                   Z-ADD     D98DTI        WDAT
177400940901     C                   EXSR      DATORE
177500960911     C                   Z-ADD     WORE          WPODTI
177600960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
177700960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
177800960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
177900960911     C                   Z-ADD     WPODTI        W10POI            9 0
178000960911     C                   EXSR      R10FES
178100960911     C                   Z-ADD     W10POO        WPODTI
178200960911     C* AGGIUNGO LE ORA D'ASTA
178300960911     C                   ADD       WASTA         WPODTI
178400960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
178500960911     C     W10POI        IFEQ      W10POO
178600940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
178700940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
178800940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
178900940901     C                   EXSR      RO3ORE
179000940901     C                   ADD       WR3HHO        WPODTI
179100940901     C                   ADD       WR3HHO        W98TTI
179200940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
179300940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
179400940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
179500940901     C                   EXSR      RO3ORE
179600940901     C                   ADD       WR3HHO        WPODTI
179700940901     C                   ADD       WR3HHO        W98TTI
179800960911     C                   ENDIF                                                  *SE NON FESTIVO
179900940901     C                   END
180000940901     C*
180100940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
180200940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
180300940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
180400940901     C     W98TTI        DIV       24            WDIV              3 0
180500940901     C                   MVR                     WRES              3 0
180600940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
180700941228     C                   Z-ADD     W990IN        D98TCI
180800160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
180900160405     c*  per cui le sottraggo, per riagguingerle alla fine
181000160405     c                   if        wVP='S'
181100160405     c                   sub       24            D98TCI
181200160405     c                   endif
181300160405     c
181400150630     c                   eval      wTSogliaE='0'
181500940901     C                   ELSE                                                   <-- 2 -->
181600941228     C                   Z-ADD     W991IN        D98TCI
181700160405     c                   if        wVP='S'
181800160405     c                   sub       24            D98TCI
181900160405     c                   endif
182000150630     c                   eval      wTSogliaE='1'
182100940908     C                   ENDIF                                                  <-- 2 ---
182200090827     c* Sistemo i tempi di consegna se non sono multipli di 12
182300090827     c                   exsr      SisCONS
182400090827     c*
182500940908     C* RICALCOLO TEMPO DI RESA INTERNO
182600950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
182700950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
182800090921     c                   z-add     d98tri        w99tri
182900090921     c* per la DS
183000090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
183100090921     c* per i calcoli interni
183200090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
183300940908     C                   ENDIF                                                  <-- 1 ---
183400940901     C*
183500940901     C                   ENDSR
183600930325     C********************************************************************
183700930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
183800930325     C********************************************************************
183900930325     C     TDATPO        BEGSR
184000930406     C*
184100930420     C* TRASFORMO LA DATA ODIERNA
184200930416     C                   Z-ADD     WDAODI        WDAT
184300930416     C                   EXSR      DATORE
184400050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
184500960911     C                   MOVEL     'OSC  '       WR3TAB
184600960911     C                   Z-ADD     WHHODI        WR3HHI
184700960911     C                   EXSR      RO3ORE
184800050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
184900050922     c                   if        wr3hho=12
185000050922     c     wpoatt        add       12            wpoattdata24
185100050922     c                   else
185200050922     c                   z-add     wpoatt        wpoattdata24
185300050922     c                   endif
185400930325     C*
185500930420     C* TRASFORMO LA DATA DI SPEDIZIONE
185600931125     C     D98DSP        IFEQ      *ZEROS
185700930326     C                   MOVEL     '1'           D98ERR
185800930326     C                   MOVEL     MSG(2)        D98MSG
185900930326     C                   ELSE
186000931125     C                   Z-ADD     D98DSP        WDAT
186100930325     C                   EXSR      DATORE
186200930406     C                   Z-ADD     WORE          WPODSP
186300930326     C                   END
186400930414     C*
186500930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
186600930414     C                   MOVEL     'LIM  '       WR3TAB
186700930414     C                   Z-ADD     *ZEROS        WR3HHI
186800930414     C                   EXSR      RO3ORE
186900931222     C     WR3HHO        MULT      10            WTM060            6 0
187000931222     C     WPODSP        ADD       WTM060        WPOLIM
187100140822     c
187200930325     C*
187300930420     C* TRASFORMO LA DATA DI CONSEGNA
187400931125     C     D98DCM        IFNE      *ZEROS
187500931125     C                   Z-ADD     D98DCM        WDAT
187600930325     C                   EXSR      DATORE
187700930326     C                   Z-ADD     WORE          WPODCM
187800930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
187900930326     C                   MOVEL     'OSC  '       WR3TAB
188000931125     C                   Z-ADD     D98HMC        WR3HHI
188100930326     C                   EXSR      RO3ORE
188200930326     C                   ADD       WR3HHO        WPODCM
188300930326     C                   END
188400941228     C*
188500941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
188600941228     C     D98DUC        IFNE      *ZEROS
188700941228     C                   Z-ADD     D98DUC        WDAT
188800941228     C                   EXSR      DATORE
188900941228     C                   Z-ADD     WORE          WPODUC
189000970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
189100970310     C                   MOVEL     'OSP  '       WR3TAB
189200970310     C                   Z-ADD     W98HUC        WR3HHI
189300970310     C                   EXSR      RO3ORE
189400970310     C     D98DRF        IFNE      *ZEROS
189500050928     c     w98huc        andgt     1200
189600970310     C                   SUB       12            WR3HHO
189700970310     C                   ENDIF
189800970310     C                   ADD       WR3HHO        WPODUC
189900941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
190000070605     C***  WPODSP        IFGT      WPODUC
190100070605     C***                Z-ADD     WPODSP        WPODUC
190200070605     C***                ENDIF
190300941228     C                   ENDIF
190400960402     C*
190500141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
190600970304     C     D98DBR        IFNE      *ZEROS
190700970304     C                   Z-ADD     D98DBR        WDAT
190800960402     C                   EXSR      DATORE
190900960402     C                   Z-ADD     WORE          WPODBR
191000070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
191100960402     C                   ELSE
191200960402     C                   Z-ADD     WPODSP        WPODBR
191300960402     C                   ENDIF
191400930913     C*
191500960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
191600930913     C* CONSEGNABILITA'
191700960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
191800950407     C*
191900930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
192000931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
192100931125     C                   Z-ADD     D98DIC        WDAT
192200930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
192300930913     C                   Z-ADD     WORE          WPODIC
192400930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
192500931125     C                   Z-ADD     D98OIC        WR3HHI
192600930913     C                   EXSR      RO3ORE
192700930913     C                   ADD       WR3HHO        WPODIC
192800931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
192900950407     C*
193000930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
193100931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
193200931125     C                   Z-ADD     D98DFC        WDAT
193300930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
193400930913     C                   Z-ADD     WORE          WPODFC
193500140822     C                   Z-ADD     WORE          WPODFC_00
193600930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
193700931125     C                   Z-ADD     D98OFC        WR3HHI
193800930913     C                   EXSR      RO3ORE
193900930913     C                   ADD       WR3HHO        WPODFC
194000931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
194100930913     C*
194200930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
194300930913     C     D98ERR        IFEQ      '0'
194400930913     C     WPODFC        IFEQ      *ZEROS
194500930913     C     WPODIC        ANDNE     *ZEROS
194600930913     C                   MOVEL     '1'           D98ERR
194700930913     C                   MOVEL     MSG(17)       D98MSG
194800930913     C                   ENDIF
194900930913     C                   ENDIF
195000930907     C*
195100930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
195200930913     C     D98ERR        IFEQ      '0'
195300930913     C     WPODIC        IFGT      WPODFC
195400930907     C                   MOVEL     '1'           D98ERR
195500930913     C                   MOVEL     MSG(18)       D98MSG
195600930913     C                   ENDIF
195700930913     C                   ENDIF
195800950407     C*
195900950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
196000930511     C*
196100930511     C                   ENDSR
196200950110     C********************************************************************
196300950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
196400950110     C********************************************************************
196500950110     C     CALDPP        BEGSR
196600950110     C*
196700950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
196800950110     C                   MOVEL     'P'           WTC                            *PARTENZA
196900950110     C                   MOVEL     'I'           WIC                            *INTERNA
197000960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
197100950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
197200950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
197300950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
197400950110     C*
197500950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
197600950110     C                   MOVEL     'P'           WTC                            *PARTENZA
197700950110     C                   MOVEL     'C'           WIC                            *CLIENTE
197800950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
197900950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
198000950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
198100950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
198200960229     C*
198300960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
198400050922     c
198500050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
198600050922     c
198700050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
198800090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
198900050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
199000090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
199100050922%%%  C*  ritardo alla partenza.
199200050922     c* Il tutto solo se la data di oggi > della
199300050922     c*  data teorica di partenza determinata
199400050922     c*
199500950110%%%  C     D98ERR        IFEQ      '0'
199600950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
199700950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
199800950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
199900050922     c     wpoattData24  andgt     wpoppi
200000960911%%%  C********             MOVEL'OSC  '   WR3TAB
200100960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
200200960911%%%  C********             EXSR RO3ORE
200300960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
200400050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
200500090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
200600090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
200700950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
200800050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
200900050922     c* La nuova data calcolata non può essere < della minima prevista
201000050922     c                   if        wr0poo>wpoppi
201100050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
201200050922     c                   endif
201300050922     c
201400950110%%%  C                   ENDIF
201500950110%%%  C                   ENDIF
201600950110%%%  C*
201700950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
201800050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
201900050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
202000050915%%%  C*  alla prtenza.
202100950110%%%  C     D98ERR        IFEQ      '0'
202200950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
202300950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
202400950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
202500950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
202600950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
202700950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
202800050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
202900950110%%%  C                   ENDIF
203000950110%%%  C                   ENDIF
203100950110%%%  C*
203200950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
203300950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
203400950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
203500950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
203600950110%%%  C     D98ERR        IFEQ      '0'
203700950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
203800950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
203900950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
204000950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
204100090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
204200090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
204300950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
204400050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
204500950110%%%  C                   ENDIF
204600950110%%%  C                   ENDIF
204700960229     C*
204800960229     C                   ENDIF                                                  <-- 1 ---
204900950110     C*
205000950110     C                   ENDSR
205100930604     C********************************************************************
205200930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
205300930604     C********************************************************************
205400930604     C     CALDPT        BEGSR
205500090902     c                   clear                   RicalcoloTRAZ     1
205600930604     C*
205700930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
205800931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
205900931129     C                   MOVEL     'I'           WIC                            *INTERNA
206000050915     c* se non presente la data partenza ultimo collo, uso la teorica
206100950110     C     D98DUC        IFEQ      *ZEROS
206200950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
206300950110     C                   ELSE
206400950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
206500950110     C                   ENDIF
206600930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
206700930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
206800930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
206900930604     C*
207000930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
207100931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
207200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
207300950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
207400930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
207500930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
207600930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
207700150508
207800150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
207900150508     c* se presente la data partenza PRIMO  collo --> la uso
208000150508     c                   if        w98dpc=0
208100150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
208200150508     c                   else
208300150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
208400150508     C                   MOVEL     'C'           WIC                            *INTERNA
208500150508     C                   Z-ADD     W98DPC        WDAT
208600150508     C                   EXSR      DATORE
208700150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
208800150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
208900150508     C                   EXSR      RO4CDP
209000150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
209100150508     c                   endif
209200960229     C*
209300960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
209400940624%%%  C*
209500050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
209600050915
209700940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
209800050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
209900050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
210000050915     c*   attribuito alla trazione
210100940624%%%  C     D98ERR        IFEQ      '0'
210200940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
210300940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
210400090903     c     wpoatt        andge     wpopti
210500050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
210600090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
210700090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
210800940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
210900050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
211000050922     c                   if        wr0poo>wpopti
211100050922     c                   eval      wpopti=wr0poo
211200050922%%%  C                   endif
211300090902     c*
211400090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
211500090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
211600090902     c*  della trazione udate e utime
211700090902     c                   eval      RicalcoloTRAZ='S'
211800090902     c
211900940624%%%  C                   ENDIF
212000940624%%%  C                   ENDIF
212100940628%%%  C*
212200050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
212300050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
212400050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
212500940628%%%  C     D98ERR        IFEQ      '0'
212600940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
212700940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
212800940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
212900090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
213000090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
213100940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
213200050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
213300940628%%%  C                   ENDIF
213400940628%%%  C                   ENDIF
213500960229     C*
213600960229     C                   ENDIF                                                  <-- 1 ---
213700930604     C*
213800930604     C                   ENDSR
213900930511     C********************************************************************
214000930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
214100930511     C********************************************************************
214200930511     C     CALDCR        BEGSR
214300050914     c                   clear                   consideraora
214400100308     c
214500100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
214600100308     c*
214700100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
214800100308     c* 1) se presente solo ORA --> ignoro sempre
214900100308     c* 2) se presente data e ora IL --> ignoro l'ora
215000100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
215100100309     c***                if        d98tsp='E'
215200100309     c***                clear                   d98dcr
215300100309     c***                clear                   d98hcr
215400100309     c**+                clear                   d98tcr
215500100309     c***                endif
215600100308     c
215700100308     c                   if        d98hcr>0
215800100308     c
215900100308     c                   if        d98dcr=0
216000100308     c                   clear                   d98hcr
216100100308     c                   endif
216200100308
216300100308     c                   if        d98dcr>0 and d98tcr=' '
216400100308     c                   clear                   d98hcr
216500100308     c                   endif
216600100308     c                   endif
216700941228     C*
216800941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
216900941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
217000050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
217100050901     c*  (per uguale si intende entrambe nel mattino o entrambe
217200050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
217300050901     c*  (EDPES 1/9/05)
217400050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
217500050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
217600050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
217700050831     C***                Z-ADD     D98DCM        D98DCR
217800050831     C***                ENDIF
217900050901     c*
218000070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
218100050914     c
218200100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
218300100308     c***                clear                   d98hcr
218400100308     c***                clear                   d98tcr
218500100308     c***                endif
218600050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
218700050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
218800070521     c*  richiesta dal cliente
218900100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
219000100308     C***                MOVEL     'OSC  '       WR3TAB
219100100308     C***                Z-ADD     D98HMC        WR3HHI
219200100308     C***                EXSR      RO3ORE
219300100308     c***                z-add     wr3hho        salvaoreHMC
219400050901     c
219500100308     C***                Z-ADD     D98Hcr        WR3HHI
219600100308     C***                EXSR      RO3ORE
219700100308     c***                z-add     wr3hho        salvaoreHCR
219800100308     c***
219900050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
220000100308    2c***                if        salvaoreHMC=salvaoreHCR
220100100308     C***                eval      Consideraora='S'
220200100308     c***                else
220300100308     C***                eval      Consideraora=' '
220400100308     c***                clear                   d98hcr
220500100308    2c***                endif
220600100308    1c***                endif
220700930511     C*
220800930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
220900931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
221000931125     C                   Z-ADD     D98DCR        WDAT
221100930325     C                   EXSR      DATORE
221200930326     C                   Z-ADD     WORE          WPODCR
221300930420     C*
221400930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
221500930420     C*
221600931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
221700930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
221800930326     C                   MOVEL     'CRD  '       WR3TAB
221900931125     C                   Z-ADD     D98HCR        WR3HHI
222000930419     C                   EXSR      RO3ORE
222100930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
222200940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
222300050902     c
222400931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
222500930420     C                   MOVEL     'CRD1 '       WR3TAB
222600930420     C                   Z-ADD     *ZEROS        WR3HHI
222700930420     C                   EXSR      RO3ORE
222800931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
222900090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
223000090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
223100050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
223200060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
223300050907     c                   z-add     worecr        oreILoDOPO
223400931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
223500931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
223600931207     C                   MOVEL     'C'           WIC                            *CLIENTE
223700931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
223800150330     C                   Z-ADD     WORECR        WR4NOI
223900931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
224000931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
224100931209     C     WPOCRF        SUB       12            WR7POI            9 0
224200931209     C                   MOVEL     'X'           WIC
224300931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
224400931209     C                   ADD       12            WR7POO
224500931207     C     WR7POO        IFGT      WPOCRF
224600931207     C                   Z-ADD     WR7POO        WPOCRF
224700931207     C                   END
224800150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
224900150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
225000150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
225100150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
225200150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
225300150330     C                   MOVEL     'C'           WIC                            *CLIENTE
225400150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
225500150330     C                   Z-ADD     w99TCC        WR4NOI
225600150330     c                   sub       12            wr4noi
225700150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
225800150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
225900150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
226000150330     c
226100150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
226200150330     C                   MOVEL     'X'           WIC
226300150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
226400150330     C******             ADD       12            WR7POO
226500150330     C     WR7POO        IFGT      WPOCRF_dee
226600150330     C                   Z-ADD     WR7POO        WPOCRF_dee
226700150330     C                   END
226800050902     c
226900050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
227000050902     c*  sono le ore di fine della non consegnabilità
227100050905     c* Se aggiungendo 1 finisco in giorno festivo
227200050905     c*  cerco primo giorno non festivo
227300050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
227400050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
227500050907     C****               MOVEL     'C'           WIC                            *CLIENTE
227600050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
227700050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
227800050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
227900050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
228000050907     c
228100050907     c                   add       1             wpocri
228200050902     c
228300050902   FFC                   Z-ADD     WPOCRI        WPOCRC
228400050902     c
228500930325     C                   END
228600150330     C*
228700931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
228800930420     C* DATA SPED. = DATA INIZIO RANGE
228900930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
229000940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
229100940324£$   C                   Z-ADD     WPOCRI        WPOCRC
229200930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
229300930420     C                   MOVEL     'CRP  '       WR3TAB
229400931125     C                   Z-ADD     D98HCR        WR3HHI
229500930420     C                   EXSR      RO3ORE
229600931207     C     WPODCR        ADD       WR3HHO        WPOCRF
229700931207     C                   SUB       24            WPOCRF                         *FINE RANGE
229800931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
229900931207     C* ANDANDO INDIETRO NEL TEMPO
230000050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
230100050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
230200050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
230300050907     c* data utile venedì sera
230400130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
230500130411     C                   MOVEL     'X'           WIC                            *CLIENTE
230600931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
230700931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
230800931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
230900930326     C                   END
231000930526     C*
231100931125     C     D98TCR        IFEQ      'P'
231200931125     C     D98TCR        OREQ      'D'
231300930526     C                   ELSE                                                   *CONS.RICH.IL ..
231400940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
231500940324£$   C                   Z-ADD     WPODCR        WPOCRC
231600930526     C* DATA SPED. = DATA INIZIO RANGE
231700050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
231800050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
231900050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
232000930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
232100930526     C                   MOVEL     'CRI  '       WR3TAB
232200931125     C                   Z-ADD     D98HCR        WR3HHI
232300930526     C                   EXSR      RO3ORE
232400050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
232500050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
232600050907     c                   z-add     wr3hho        oreILoDOPO
232700930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
232800050902     c
232900050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
233000050902     c                   if        wr3MATPOM='P' and d98hcr>0
233100050902     c                   add       12            wpocri
233200100308     c* tolgo 12 ore dalle ore conseganbili
233300050907     c                   sub       12            oreILoDOPO
233400050902     c                   endif
233500050902     c
233600931209     C     WPOCRF        SUB       12            WR7POI            9 0
233700931209     C                   MOVEL     'X'           WIC
233800931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
233900931209     C                   ADD       12            WR7POO
234000931209     C     WR7POO        IFGT      WPOCRF
234100931209     C                   Z-ADD     WR7POO        WPOCRF
234200931209     C                   END
234300050905     c
234400050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
234500050905     c*  sono le ore di fine della non consegnabilità
234600050905     c* Se aggiungendo 1 finisco in giorno festivo
234700050905     c*  cerco primo giorno non festivo
234800050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
234900050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
235000050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
235100050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
235200050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
235300050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
235400050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
235500050907     c
235600050907     c                   add       1             wpocri
235700050905     c
235800050905   FFC                   Z-ADD     WPOCRI        WPOCRC
235900050905     c
236000930526     C                   END
236100931206     C*
236200931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
236300931206     C* LA IMPOSTO COME LA CONS.TEORICA
236400140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
236500140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
236600931206     C                   ELSE
236700931206     C                   Z-ADD     WPOCRF        WPOCR1
236800931206     C                   END
236900150608     c* ai fini del calcolo affidabilità
237000150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
237100150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
237200931206     C                   ELSE
237300931206     C                   Z-ADD     WPOCRF        WPOCR2
237400931206     C                   END
237500150608     c* ai fini della data prevista consegna cliente
237600150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
237700150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
237800150608     C                   ELSE
237900150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
238000150608     C                   END
238100150331     c
238200150331     c                   if        wpocrf_dee>0
238300150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
238400150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
238500150330     C                   END
238600150331     C                   END
238700150331     C*
238800150331     c                   if        wpocrf_dee=0
238900150608     C     wpocr2_dee    sub       24            WPOCRF_dee
239000150331     c                   endif
239100150331     c
239200930526     C                   END                                                    <-- 1 ---
239300930511     C*
239400930511     C                   ENDSR
239500930511     C********************************************************************
239600930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
239700930511     C********************************************************************
239800930511     C     CALLAV        BEGSR
239900930511     C*
240000941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
240100930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
240200930421     C*
240300930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
240400930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
240500930421     C     1             DO        IE            E                 3 0          --- 2 -->
240600930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
240700930421     C                   Z-ADD     1             I
240800930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
240900060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
241000930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
241100930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
241200930421     C                   ADD       1             IL
241300930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
241400930421     C                   EXSR      DATORE
241500930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
241600091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
241700091014     c* Mi salvo la posizione dell'evento
241800091014     C                   Z-ADD     E             LAp(IL)
241900930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
242000930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
242100930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
242200930421     C                   EXSR      RO3ORE
242300930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
242400930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
242500930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
242600930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
242700091015     c**                 if        §2agla>1
242800091015     c**                 sub       1             §2agla
242900930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
243000091015     c**                 else
243100091015     c**                 clear                   whh
243200091015     c**                 endif
243300930510     C                   ADD       WHH           WORELA
243400940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
243500930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
243600090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
243700150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
243800930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
243900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
244000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
244100930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
244200930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
244300930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
244400930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
244500931209     C                   Z-ADD     LAF(IL)       WR7POI
244600931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
244700931202     C     WR7POO        IFGT      LAF(IL)
244800931201     C                   Z-ADD     WR7POO        LAF(IL)
244900931202     C                   END
245000930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
245100930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
245200091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
245300930510     C                   END                                                    <-- 3 ---
245400931223     C                   ENDDO                                                  <-- 2 ---
245500930421     C*
245600930421     C                   END                                                    <-- 1 ---
245700930618££$$ C*
245800930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
245900931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
246000941228££$$ C     1             DO        IL            I
246100930618££$$ C     LAF(I)        IFGT      WPOLAF
246200930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
246300930618££$$ C                   END
246400931223££$$ C                   ENDDO
246500930511     C*
246600930511     C                   ENDSR
246700940324£$   C********************************************************************
246800940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
246900940324£$   C********************************************************************
247000940324£$   C     CALLAC        BEGSR
247100941229     C*
247200950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
247300950407     C*
247400950210     C                   Z-ADD     WORELA        WTM060
247500941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
247600941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
247700941229     C     D98DAG        IFEQ      *ZEROS
247800050520     c     §2aftg        andeq     'S'
247900090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
248000090827     C     WORELA        ADD       w99TCC        WTM060            6 0
248100160404     c                   if        wVP='S'
248200160404     c                   sub       24            wTM060
248300160404     c                   endif
248400941229     C                   ENDIF
248500941229     C*
248600940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
248700940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
248800941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
248900941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
249000941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
249100940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
249200941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
249300050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
249400050520     c                   movel     §2aftg        LCON(IL)
249500950407     C*
249600950407     C                   ENDIF
249700960129     C*
249800940324£$   C                   ENDSR
249900930511     C********************************************************************
250000930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
250100930511     C********************************************************************
250200930511     C     CALGIA        BEGSR
250300960129     C*
250400960129     C*-------------------------------------------------------------------
250500960129     C* EVENTI GIACENZA
250600960129     C*-------------------------------------------------------------------
250700960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
250800960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
250900960129     C*
251000050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
251100041116     c     d98nsp        ifgt      0
251200031125     C                   EXSR      CARGIAC
251300041116     c                   endif
251400031125     C*
251500960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
251600960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
251700960129     C     1             DO        IE            E                 3 0          --- 2 -->
251800960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
251900960129     C                   Z-ADD     1             I
252000960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
252100060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
252200960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
252300960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
252400960129     C                   ADD       1             IG
252500960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
252600960129     C                   EXSR      DATORE
252700960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
252800140606     c* Mi salvo la posizione dell'evento
252900140606     C                   Z-ADD     E             gip(IG)
253000960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
253100960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
253200960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
253300960129     C                   EXSR      RO3ORE
253400960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
253500960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
253600960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
253700960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
253800960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
253900960129     C                   ADD       WHH           WOREGI
254000960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
254100960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
254200090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
254300090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
254400160404     c                   if        wVP='S'
254500160404     c                   sub       24            wOREGI
254600160404     c                   endif
254700960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
254800960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
254900960129     C                   MOVEL     'C'           WIC                            *CLIENTE
255000960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
255100960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
255200960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
255300960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
255400960129     C                   Z-ADD     GIF(IG)       WR7POI
255500960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
255600031125     C     WR7POO        IFGT      GIF(IG)
255700031125     C                   Z-ADD     WR7POO        GIF(IG)
255800031125     C                   ENDIF
255900031201     C*
256000031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
256100031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
256200031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
256300031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
256400031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
256500031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
256600031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
256700031201     C                   IF        WPOATT > GIF(IG)
256800031201     C*
256900031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
257000031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
257100031201     C                   IF        WGGA = *BLANKS
257200031201     C                   SETOFF                                       54
257300031201     C                   ELSE
257400031201     C     WGGA          LOOKUP    SK7QW                                  54
257500031201     C                   ENDIF
257600031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
257700031201     C                   MOVEL     '1'           FLGGIACOK
257800031201     C                   ELSE
257900140127     c
258000031125     C*
258100031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
258200031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
258300031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
258400031125     C                   MOVEL     '0'           FLGGIACOK         1
258500031125     C*
258600031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
258700031125     C     1             DO        IAG           EG                3 0
258800031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
258900031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
259000031125     C                   EXSR      DATORE
259100031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
259200031125     C*
259300031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
259400031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
259500031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
259600031125     C                   EXSR      DATORE
259700031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
259800031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
259900031229     C* => NN LO CONSIDERO
260000031229     C                   IF        EVC(E+1) = 'G' AND
260100031229     C                             WPROXGII = WGCPDORE
260200031229     C                   MOVEL     '1'           FLGGIACOK
260300031229     C                   ENDIF
260400031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
260500031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
260600031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
260700031125     C                   EXSR      RO3ORE
260800031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
260900031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
261000031125     C* DELL'EVENTO GIACENZA CORRENTE
261100031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
261200060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
261300031125     C                   MOVEL     '1'           FLGGIACOK
261400031125     C                   LEAVE
261500031125     C                   ENDIF
261600031125     C                   ELSE
261700031229     C                   IF        EVC(E)  = 'G' AND
261800031229     C                             GII(IG) = WGCPDORE
261900031229     C                   MOVEL     '1'           FLGGIACOK
262000031229     C                   LEAVE
262100031229     C                   ENDIF
262200031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
262300031125     C                   MOVEL     '1'           FLGGIACOK
262400031125     C                   LEAVE
262500031125     C                   ENDIF
262600031125     C                   ENDIF
262700031125     C                   ENDDO
262800031201     C                   ENDIF
262900140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
263000140127     C                   IF        FLGGIACOK = '0'  and
263100140127     c                             d98dcm=evd(E)
263200140127     C                   MOVEL     '1'           FLGGIACOK
263300140127     c                   endif
263400031125     C*
263500031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
263600031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
263700031125     C* DELL'EVENTO GIACENZA CORRENTE
263800031125     C                   IF        FLGGIACOK = '0'
263900031125     C                   Z-ADD     *ZEROS        GIF(IG)
264000031125     C                   ELSE
264100031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
264200031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
264300031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
264400031125     C                   ENDIF
264500031125     C                   ELSE
264600031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
264700031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
264800031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
264900031125     C                   ENDIF
265000031125     C*
265100960129     C                   ENDIF                                                  <-- 3 ---
265200960129     C                   ENDDO                                                  <-- 2 ---
265300960129     C*
265400960129     C                   ENDIF                                                  <-- 1 ---
265500960129££$$ C*
265600960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
265700960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
265800960129££$$ C     1             DO        IG            I
265900960129££$$ C     GIF(I)        IFGT      WPOGIF
266000960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
266100960129££$$ C                   ENDIF
266200960129££$$ C                   ENDDO
266300941229     C*
266400941229     C*-------------------------------------------------------------------
266500941229     C* APERTURA GIACENZA
266600941229     C*-------------------------------------------------------------------
266700960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
266800960129     C*
266900960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
267000931125     C                   Z-ADD     D98DAG        WDAT
267100930325     C                   EXSR      DATORE
267200950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
267300960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
267400950413£££  C                   Z-ADD     WORE          WPODAG
267500960129$$£  C*********************ENDIF
267600930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
267700930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
267800930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
267900930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
268000930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
268100941229     C*
268200941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
268300941229     C                   Z-ADD     WPODAG        WPOAGC
268400960129     C*
268500960129     C                   ENDIF                                                  <-- 1 ---
268600941229     C*
268700941229     C*-------------------------------------------------------------------
268800930511     C* CHIUSURA GIACENZA
268900941229     C*-------------------------------------------------------------------
269000930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
269100930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
269200931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
269300931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
269400931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
269500931201     C                   Z-ADD     D98DCM        W98DCG
269600930416     C                   ELSE
269700930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
269800930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
269900931201     C                   Z-ADD     WDAODI        W98DCG
270000061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
270100061123     c*  chiusura giacenza, come già fa in altri posti
270200061123     c                   if        d98dfc>0
270300061123     C                   Z-ADD     d98dfc        W98DCG
270400061123     c                   endif
270500061123     c
270600930409     C                   END
270700930409     C                   END
270800930409     C                   END
270900930414     C* TRASF.DATA CHIUSURA
271000931201     C     W98DCG        IFNE      *ZEROS
271100931201     C                   Z-ADD     W98DCG        WDAT
271200930325     C                   EXSR      DATORE
271300930326     C                   Z-ADD     WORE          WPODCG
271400930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
271500930407     C* GIORNO DA ESCLUDERE
271600150216     C                   ADD       12            WPODCG
271700150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
271800150123     c*  il giorno di chiusura dovrebe già essere protetto
271900941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
272000941229     C     WPOCGC        IFEQ      *ZEROS
272100941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
272200941229     C                   ENDIF
272300930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
272400930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
272500931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
272600931129     C                   MOVEL     'C'           WIC                            *CLIENTE
272700150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
272800150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
272900160404     c                   if        wVP='S'
273000160404     c                   sub       24            wr4NOI
273100160404     c                   endif
273200930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
273300930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
273400930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
273500931209     C                   Z-ADD     WPODCG        WR7POI
273600931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
273700931202     C     WR7POO        IFGT      WPODCG
273800931201     C                   Z-ADD     WR7POO        WPODCG
273900930326     C                   END
274000931202     C                   END
274100930511     C*
274200930511     C                   ENDSR
274300031125     C********************************************************************
274400031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
274500031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
274600031125     C*             DEGLI EVENTI DI TIPO GIACENZA
274700031125     C********************************************************************
274800031125     C     CARGIAC       BEGSR
274900031125     C*
275000031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
275100031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
275200031125     C*
275300050405     C                   Z-ADD     D98AAS        KWGCPAAS
275400050405     C                   Z-ADD     D98LNP        KWGCPLNP
275500050405     C                   Z-ADD     D98NRS        KWGCPNRS
275600050405     C                   Z-ADD     D98NSP        KWGCPNSP
275700031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
275800050405     C     KEYGC_P       SETLL     TIGCP01L
275900050405     C                   IF        %equal(TIGCP01L)
276000050405     C     KEYGC_P       READE     TIGCP01L
276100050405     C                   DOW       not %eof(TIGCP01L)
276200031125     C                   ADD       1             IAG
276300031125     C                   EVAL      GCPD(IAG) = GCPDUR
276400050405     C     KEYGC_P       READE     TIGCP01L
276500031125     C                   ENDDO
276600031125     C                   ENDIF
276700031125     C                   ELSE
276800050405     C     KEYGC_P       SETLL     TIGCP21L
276900050405     C                   IF        %equal(TIGCP21L)
277000050405     C     KEYGC_P       READE     TIGCP21L
277100050405     C                   DOW       not %eof(TIGCP21L)
277200031125     C                   ADD       1             IAG
277300031125     C                   EVAL      GCPD(IAG) = GCPDUR
277400050405     C     KEYGC_P       READE     TIGCP21L
277500031125     C                   ENDDO
277600031125     C                   ENDIF
277700031125     C                   ENDIF
277800031125     C*
277900031125     C                   ENDSR
278000941229     C********************************************************************
278100960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
278200941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
278300941229     C********************************************************************
278400960129     C     CALGIC        BEGSR
278500941229     C*
278600960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
278700960129     C*
278800960129     C                   Z-ADD     WOREGI        WTM060
278900960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
279000960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
279100960129     C     D98DAG        IFEQ      *ZEROS
279200960129$$£  C     WOREGI        ADD       12            WTM060            6 0
279300960129     C                   ENDIF
279400960129     C*
279500960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
279600960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
279700960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
279800960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
279900960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
280000960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
280100960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
280200960129     C*
280300960129     C                   ENDIF
280400941229     C*
280500941229     C                   ENDSR
280600060418     C********************************************************************
280700060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
280800060418     c*            l'affidabilità
280900060418     C********************************************************************
281000060418     C     CALCEPA       BEGSR
281100060418     C*
281200060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
281300060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
281400060420     c                   clear                   numggcepa         3 0
281500060418     C*
281600060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
281700060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
281800060420     c                   eval      wgiroepa='0'
281900060418     c                   eval      wfine=' '
282000060418     c
282100060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
282200060420    2C     W             DOwle     IP                                           --- 2 -->
282300060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
282400060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
282500060418     c
282600060420    4c                   if        wgiroepa='0'
282700060420     c                   exsr      ElaInizCEPA
282800060418   x4c                   else
282900060420    5c                   if        pvd(W)>wdat
283000060418     c                   eval      wfine='S'
283100060418    5c                   endif
283200060418    4c                   endif
283300060418     c
283400060420    4c                   if        wfine=' ' and pvd(W)=wdat
283500060420     C*
283600060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
283700060420     c*  finale
283800060420     c                   add       1             numggcepa
283900060420     c
284000060420     C                   z-add     24            WORELA
284100060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
284200060418     c
284300060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
284400060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
284500060418     C                   MOVEL     'C'           WIC                            *CLIENTE
284600060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
284700060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
284800060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
284900060418     c
285000060418     c* Trasformo le ore in data e verifico se c'e' altro evento
285100060418     c*  in sta data che allunga ancora
285200060420     c                   z-add     wr4poo        wore
285300060418     c                   EXSR      OREDAT
285400060418    4c                   endif
285500060418     c
285600060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
285700060418    4c                   if        wfine='S'
285800060418     c                   EXSR      ELAFINECEPA
285900060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
286000060420     c*  inizio range
286100060420     c                   else
286200060420     c                   add       1             w
286300060418    4C                   ENDif                                                  <-- 3 ---
286400060420     c
286500060420     c                   else
286600060420     c                   add       1             w
286700060418    3C                   ENDif                                                  <-- 3 ---
286800060418    2C                   ENDDO                                                  <-- 2 ---
286900060418     c
287000060420     c                   if        wgiroepa='1'
287100060418     c                   EXSR      ELAFINECEPA
287200060418     c                   endif
287300060418     C*
287400060418    1C                   END                                                    <-- 1 ---
287500060418££$$ C*
287600060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
287700060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
287800060420££$$ C     1             DO        Ipa           w
287900060420££$$ C     paif(w)       IFGT      WPOpaif
288000060420££$$ C                   Z-ADD     paif(w)       WPOpaif
288100060418££$$ C                   END
288200060418££$$ C                   ENDDO
288300060418     C*
288400060418     C                   ENDSR
288500060420     C********************************************************************
288600060420     c* Elabora Inizio range eventi particolari che allungano
288700060420     C********************************************************************
288800060420     c     ElaInizCEPA   BEGSR
288900060420     C                   ADD       1             IPA
289000060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
289100060420     C                   EXSR      DATORE
289200060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
289300061017     c
289400060420     c                   eval      wgiroepa='1'
289500060420     c                   eval      wdat=pvd(w)
289600060420     C                   ENDSR
289700060418     C********************************************************************
289800060418     c* Elabora fine range eventi particolari che allungano
289900060418     C********************************************************************
290000060418     c     ElafineCEPA   BEGSR
290100060420     c     numggcepa     mult      24            worela
290200060420     c                   sub       12            worela
290300090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
290400090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
290500160404     c                   if        wVP='S'
290600160404     c                   sub       24            WORELA
290700160404     c                   endif
290800060418     c
290900060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
291000060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
291100060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
291200060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
291300060418     C                   Z-ADD     paif(IPA)     WR7POI
291400060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
291500060418    5C     WR7POO        IFGT      paif(IPA)
291600060418     C                   Z-ADD     WR7POO        paif(IPA)
291700060418    5C                   END
291800061017     c*Sommo 24 ore di inizio giornata
291900061017     c*            : non ho capito perchè ma ci vogliono
292000061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
292100090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
292200090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
292300160404     c                   if        wVP='S'
292400160404     c                   sub       24            paii(IPA)
292500160404     c                   endif
292600061017     c* Se tempi di consegna inferiori alle 24 ore , devo
292700061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
292800061017     c*  partire prima
292900061017     c****               if        d98tcc<24
293000061017     c****               sub       12            paii(ipa)
293100061017     c****               endif
293200060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
293300060418     c* Imposto nuovo giro
293400060420     c                   eval      wgiroepa='0'
293500060418     c                   eval      wfine=' '
293600060420     c                   clear                   numggcepa         3 0
293700060418     c                   ENDSR
293800930511     C********************************************************************
293900930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
294000930511     C********************************************************************
294100010822     C     CALASS        BEGSR
294200010821     C*
294300011211     C                   EXSR      REPMAXCSB
294400930511     C*
294500930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
294600010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
294700011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
294800930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
294900930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
295000930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
295100930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
295200930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
295300930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
295400930511     C                   ELSE                                                   <-- 4 -->
2955009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
295600930512     C                   END                                                    <-- 4 ---
295700930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
295800930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
295900930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
296000930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
296100930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
296200930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
296300930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
296400930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
296500930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
296600930413     C                   END                                                    <-- 2 ---
296700011211     C                   END                                                    <-- 1 ---
296800930406     C*
296900930325     C                   ENDSR
297000930608     C********************************************************************
297100930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
297200930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2973009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
297400930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
297500930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
297600930608     C********************************************************************
297700930608     C     RDRASS        BEGSR
297800930608     C*
297900930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2980009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2981009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2982009407151507 C*********************EXSR RO3ORE
2983009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
298400930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2985009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2986009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2987009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2988009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2989009407191907 C                   Z-ADD     WPOASI        WPOROU
2990009407191907 C                   EXSR      CHKFES
2991009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2992009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2993009407191907 C                   ELSE                                                   *E' FERIALE
2994009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2995009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
2996009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
2997009407191907 C                   ENDIF
299800930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
299900930608     C                   MOVEL     'ASS1 '       WR3TAB
300000930608     C                   Z-ADD     *ZEROS        WR3HHI
300100930608     C                   EXSR      RO3ORE
300200930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
300300930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
300400931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
300500931129     C                   MOVEL     'C'           WIC                            *CLIENTE
300600930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
300700930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
300800930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
300900930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
301000930608     C*
301100930608     C                   ENDSR
301200930511     C********************************************************************
301300930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
301400930511     C********************************************************************
301500930511     C     CALFED        BEGSR
301600150331     c                   clear                   fddata
301700150331     c                   clear                   WPOFDdata
301800930511     C*
301900051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
302000931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
302100051108     C     D98FFD        oreq      'M'                                          --- 1 -->
302200930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
302300930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
302400930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
302500930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
302600150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
302700150331     c                   if        §ar5dfd>*zeros
302800150331     c                   movel     §ar5dfd       fddata            8 0
302900150331     c                   endif
303000150331     c                   if        fddata>0 and ((d98dti>0 and
303100150331     c                             fddata>d98dti) or d98dti=0)
303200150331     C                   Z-ADD     fddata        WDAT
303300150331     C                   EXSR      DATORE
303400150331     C                   Z-ADD     WORE          WPOfddata
303500150331     c                   endif
303600150331     c
303700930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
303800930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
303900930511     C                   ELSE                                                   <-- 2 -->
3040009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
304100930512     C                   END                                                    <-- 2 ---
304200150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3043001504081507 C                   Z-ADD     WPOfddata     WPOFDI
304400150331     c                   endif
304500150331
304600930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
304700930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
304800930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
304900930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
305000930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
305100930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
305200930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
305300930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
305400930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
305500930511     C                   END                                                    <-- 1 ---
305600930511     C*
305700930511     C                   ENDSR
305800930608     C********************************************************************
305900930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
306000930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3061009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
306200930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
306300930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
306400930608     C********************************************************************
306500930608     C     RDRFED        BEGSR
306600930608     C*
306700930608     C* CALCOLO ORE DEL FERMO DEPOSITO
306800930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3069009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3070001503311907 C                   Z-ADD     WPOFDI        WPOROU
3071001503311907 C                   EXSR      CHKFES
3072001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3073001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3074001503311907 C                   ELSE                                                   *E' FERIALE
3075001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3076001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3077001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3078001503311907 C                   ENDIF
307900150331     c* Se presente la particolarità consegna con più giorni di fermo depo
308000150331     c*  uso questi
308100150331     c                   if        §7rggfd>*zeros
308200150424     c                   movel     §7rggfd       wggfd             2 0
308300150331     c     wggfd         mult      24            Wnore
308400150331     c                   add       wnore         worefd
308500150331     c                   else
308600930608     C                   MOVEL     'FED1 '       WR3TAB
308700930608     C                   Z-ADD     *ZEROS        WR3HHI
308800930608     C                   EXSR      RO3ORE
308900940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
309000150331     c                   endif
309100930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
309200931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
309300931129     C                   MOVEL     'C'           WIC                            *CLIENTE
309400930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
309500930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
309600930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
309700930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
309800930608     C*
309900930608     C                   ENDSR
310000931201     C********************************************************************
310100931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
310200931201     C********************************************************************
310300931201     C     CALDTE        BEGSR
310400140825     c
310500140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
310600140825     c*  che tiene conto solo dei festivi per restituire in output
310700140825     c*  la data teorica di consegna
310800140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
310900140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
311000140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
311100140825     C                   ELSE
311200140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
311300140825     C                   END
311400140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
311500141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
311600141106     c*  quindi passo un flag
311700141106     c                   eval      nosomma='N'
311800140825     C                   EXSR      RO4CDP
311900141106     c                   eval      nosomma=' '
312000140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
312100140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
312200140825     c*  di consegna
312300140825     c                   EXSR      TRAS_DEI
312400140825     c
312500140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
312600140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
312700140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
312800140825     c                             ='M' or %subst(d98gc2:2:1)='P')
312900140825     c                   exsr      Reper_tempi
313000140825     c                   endif
313100931201     C*
313200140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
313300140825     c*  che tiene conto di:
313400140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
313500150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
313600931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
313700931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
313800931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
313900931201     C                   ELSE
314000931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
314100931201     C                   END
314200090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314300090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314400931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
314500140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
314600931201     C*
314700150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
314800931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
314900931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
315000090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
315100931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
315200150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
315300150608     c
315400150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
315500150608     c*  che non tiene conto dei tempi del C/Ass
315600150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
315700150609     c*
315800150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
315900150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
316000150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
316100150630     c                   Z-ADD     WPODTI        WR6POI
316200150609     c                   else
316300150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
316400150609     c                   endif
316500150609     c
316600150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
316700150608     c                   eval      wdee='S'
316800150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
316900150608     c                   eval      wdee=' '
317000150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
317100141017
317200141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
317300150219     C**                 EVAL      WTC='C'
317400150219     C**                 EVAL      WIC='C'
317500150219     c**                 z-add     wpoattdata00  WPOROU
317600150219     c**                 z-add     wdaodi        wpodata
317700150219     c**                 exsr      chkfes
317800150219     c**                 if        wslitt='S'
317900150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
318000150219     c**                 z-add     24            wr6noi
318100150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
318200150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
318300150219     c**                 else
318400150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
318500150219     c**                 endif
318600141204     c* Ricalcolo WPOrifDatLav
318700141229     C**                 MOVEL     'OSC  '       WR3TAB
318800141229     C**                 Z-ADD     WHHODI        WR3HHI
318900141229     C**                 EXSR      RO3ORE
319000141229     c**
319100141229     C**                 EVAL      WTC='C'
319200141229     C**                 EVAL      WIC='C'
319300141229     c**                 z-add     wpoattdata00  WPOROU
319400141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
319500141229     c**                 z-add     wr3hho        wr6noi
319600141229     c**                 add       12            wr6noi
319700141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
319800141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
319900141229     c
320000141017     c
320100141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
320200141017     c*  ma sempre devo slittare i festivi
320300141017     C* CONSEGNABILITA'
320400150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
320500150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
320600150219     C**                 EVAL      WTC='C'
320700150219     C**                 EVAL      WIC='C'
320800150219     c**                 z-add     d98dfc        WPOdata
320900150219     c**                 exsr      chkfes
321000150219     c**                 if        wslitt='S'
321100150219     c**                 z-add     wpodfc_00     Wr6poi
321200150219     c**                 z-add     24            wr6noi
321300150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
321400150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
321500150219     c**                 else
321600150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
321700150219     c**                 endif
321800150219     c**                 endif
321900931201     C*
322000931201     C                   ENDSR
322100930511     C********************************************************************
322200930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
322300930511     C********************************************************************
322400930511     C     CALDPC        BEGSR
322500930325     C*
322600930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
322700930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
322800931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
322900930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
323000930325     C                   ELSE
323100930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
323200930407     C                   END
323300090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
323400090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
323500930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
323600090902     c
323700931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
323800090901
323900930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
324000930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
324100930423     C* A QUELLA DI RICHIESTA CONSEGNA,
324200930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
324300931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
324400931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
324500931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
324600931206     C                   END
324700930423     C                   END
324800930325     C*
324900930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
325000930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
325100930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
325200090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325300090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325400930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
325500931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
325600930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
325700930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
325800930423     C* A QUELLA DI RICHIESTA CONSEGNA,
325900930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
326000931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
326100931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
326200931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
326300931206     C                   END
326400930423     C                   END
326500140822     C*
326600930324     C                   ENDSR
326700150113     C********************************************************************
326800150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
326900150113     C********************************************************************
327000150113     c     Cal_DEE       BEGSR
327100150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
327200150508     c                   clear                   wtempi00C         1
327300151007     c                   clear                   wtempi12C         1
327400150722     c                   clear                   woreOLP
327500151009     c                   clear                   wdiff
327600150219     c
327700150219     c* 1) Se presente l'arrivo della trazione lo uso
327800150219    1c                   select
327900150219     c                   when      wmancatraz=' '  and d98dti>0
328000150630     c                   Z-ADD     WPODTI        WR1POI
328100150219     c
328200150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
328300150219    1c                   when      IDdata>0
328400150325     c                   exsr      CalcoloIMP
328500151008     c* Passo in output il terminal di disguido
328600151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
328700151008     c                   eval      d98flp=%editc(LTP(2):'X')
328800151008     c                   endif
328900151008
329000150528     c* stato IMP/disguido solo se >  alla data di oggi
329100151006     c                   if        wr1poi > wpoattdata00 and
329200151006     c                             (wr1poi-wpoattdata00)>12
329300150427     c                   eval      d98spcdee='IMP'
329400151008     c                   endif
329500151006     c                   if        wr1poi > wpoattdata00 and
329600151006     c                             (wr1poi-wpoattdata00)=12
329700151006     c                             and w991In>12
329800151006     c                   eval      d98spcdee='IMP'
329900151008     c                   endif
330000150219     c
330100150219     c                   other
330200150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
330300150225     c* devo far finta che sia arrivato.
330400150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
330500150508     c*  che tiene conto della data partenza PRIMO  collo
330600150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
330700150508     c*
330800150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
330900150508     c                   if        wr1poi >=wpoattdata00
331000150508     c                              and WPOPTC_DEE > WPOPTC
331100150508     c                   eval      d98spcdee='IMP'
331200150508     c                   endif
331300150219     c                   endsl
331400150113     c
331500150219     c* se il calcolo è < di oggi --> imposto oggi
331600150113     c                   if        wr1poi < wpoattdata00
331700150113     c                   eval      wr1poi= wpoattdata00
331800150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
331900150508     c                   eval      wTempi00C='S'
332000151009     c                   clear                   wdiff
332100150113     c                   endif
332200150217     c
332300150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
332400150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
332500150630     c                   if        wr1poi>wpoattdata00
332600150630     c                   eval      wdiff=wr1poi-wpoattdata00
332700150630     c                   endif
332800151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
332900151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
333000151006     c                   eval      wTempi12C='S'
333100151006     c                   endif
333200160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
333300151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
333400151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
333500160404     c                             and idfgs>0
333600151009     C     wdiff         DIV       24            WDIV
333700151009     C                   MVR                     WRES
333800151009     C                   IF        wres>0
333900151009     c                   eval      wTempi12C='S'
334000151009     c                   else
334100151009     c                   eval      wTempi00C='S'
334200151009     c                   endif
334300151009     c                   endif
334400151009     c
334500151006     c
334600151006     c                   select
334700151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
334800151006     c                             w991in>12 and
334900151006     c                             (deeindist<>'S' or d98cons<>'S'
335000151006     c                             or d98ddc>wdaodi)
335100151006     c
335200151006     c* Se stato IMP lo mantengo per avere messaggio diverso
335300151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
335400151006     c                   if        d98spcdee<>'IMP'
335500151006     c                   eval      d98spcdee='PRV'
335600151006     c                   endif
335700151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
335800150330     c                             (deeindist<>'S' or d98cons<>'S'
335900150330     c                             or d98ddc>wdaodi)
336000150427     c
336100150427     c* Se stato IMP lo mantengo per avere messaggio diverso
336200150427     c                   if        d98spcdee<>'IMP'
336300150304     c                   eval      d98spcdee='PRV'
336400150427     c                   endif
336500150427     c
336600151006   x1c                   other
336700150330     c                   exsr      CAL_OLP
336800150217     c
336900150528     c* Se passato l'orario oppure data distinta < data di oggi
337000150508     c* 1) spedizione in distinta          --> in consegna
337100150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
337200150528     c                             d98ddc<wdaodi)
337300150508    3c                   IF        deeindist='S' and d98cons='S'
337400150508     c                   eval      d98spcdee= 'DDC'
337500150508
337600150508   x3c                   else
337700150507     c
337800150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
337900150722     c*  entro sera
338000150630     c                   if        wr1poi>wpoattdata00
338100150722     c                   z-add     12            woreOLP           3 0
338200150722     c***                add       12            wr1poi
338300150630    4c                   else
338400150507    4c                   if        wr1poi<=wpoattdata00
338500150722     c                   add       24            woreOLP
338600150722     c***                add       24            wr1poi
338700150507    4c                   endif
338800150630    4c                   endif
338900150507     c
339000150722     c****               eval      d98spcdee='OLP'
339100150507     c* memorizzo orario limite picking
339200150507     c                   eval      d98picklim=%editc(picklim:'X')
339300150507    3c                   endif
339400150507    2c                   endif
339500150507     c
339600151006    1c                   endsl
339700150217     c
339800150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
339900150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
340000150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
340100150113     C                   Z-ADD     Wr1poi        W10POI            9 0
340200150113     C                   EXSR      R10FES
340300150113     C                   Z-ADD     W10POO        wr1poi
340400150113     C
340500151006     c                   select
340600151006    1c                   when      wtempi12C='S'
340700151006     c                   z-add     W991IN        wr1noi
340800151006     c                   if        woreOLP>0
340900151006     c                   add       woreOLP       wr1noi
341000151006     c                   eval      d98spcdee='OLP'
341100160404     c                   else
341200160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
341300160404     c**                 if        wVP='S'
341400160404     c**                 add       24            wr1noi
341500160404     c**                 endif
341600151006     c                   endif
341700160404
341800151006    1c                   when      wtempi00C='S'
341900150630     c***                z-add     W990CN        wr1noi
342000150630     c                   z-add     W990IN        wr1noi
342100150722     c
342200150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
342300150722     c*  quindi sottraggo 24
342400151009     c                   if        wr1noi>24  and wdiff=0
342500150722     c                   sub       24            wr1noi
342600150722     c                   endif
342700150722     c
342800150722     c* se passato orario limite picking, slitto in avanti
342900150722     c                   if        woreOLP>0
343000150722     c                   add       woreOLP       wr1noi
343100150722     c                   eval      d98spcdee='OLP'
343200160404     c                   else
343300160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
343400160404     c**                 if        wVP='S'
343500160404     c**                 add       24            wr1noi
343600160404     c**                 endif
343700150722     c                   endif
343800150722     c
343900151006   x1c                   other
344000150722     c
344100150714     c                   if        wmancatraz=' '  and d98dti>0
344200150630     c                   z-add     w99tci        wr1noi
344300150714     c                   else
344400150714     c                   z-add     w99tcc        wr1noi
344500150714     c                   endif
344600160404
344700160401     c
344800150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
344900150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
345000150722     c                   if        woreOLP=24 and wr1noi<=24
345100150722     c                   add       woreOLP       wr1noi
345200150722     c                   eval      d98spcdee='OLP'
345300150722     c                   endif
345400150722     c                   if        woreOLP=12 and wr1noi<24
345500150722     c                   add       woreOLP       wr1noi
345600150722     c                   eval      d98spcdee='OLP'
345700150722     c                   endif
345800150722     c
345900151006    1c                   endsl
346000150629     c
346100150722    1c**                 if        WR1NOI=10
346200150722     c**                 z-add     12            wr1noi
346300150722     c**                 endif
346400150722     c
346500150722     c     wr1noi        div       12            wdiv
346600150722     c                   mvr                     wres
346700150722     c                   if        wres >0
346800150722     c                   add       2             wr1noi
346900150722     c                   endif
347000150722     c
347100150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
347200150629    1c******             endif
347300150508     c
347400150605     c                   eval      wdee='S'
347500150113     C                   EXSR      RO1CDP
347600150605     c                   eval      wdee=' '
347700150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
347800150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
347900150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
348000150113     C* A QUELLA DI RICHIESTA CONSEGNA,
348100150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
348200150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
348300150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
348400150331     c*  2) IL        -> è il giorno indicato
348500150331     C     D98DCR        IFNE      *ZEROS
348600150331     C     D98tcr        andne     'P'
348700150331     C***  WPOdpcee      IFLE      WPOCR2
348800150331     C***  WPOCR2        SUB       12            WPOdpcee
348900150330     c
349000150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
349100150330     c*   dare la prima data utile di consegna
349200150330     C     WPOdpcee      IFLE      WPOCRF_dee
349300150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
349400150330     C                   z-add     WPOCRF_dee    WPOdpcee
349500150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
349600150409     c                   if        wpodpcee>wpoattdata00
349700150409     c                   eval      d98spcdee='PRV'
349800150409     c                   endif
349900150113     C                   END
350000150331     c
350100150220     c
350200150113     C                   END
350300150220     c
350400150113     c                   ENDSR
350500150330     C********************************************************************
350600150330     C*   Calcolo orario da tabella OLP
350700150330     C********************************************************************
350800150330     c     Cal_OLP       BEGSR
350900150330     c                   eval      picklim=0900
351000150330     c* Dopo l'ora limite picking, passo al giorno dopo
351100150330     C                   MOVEL(P)  'OLP'         TBECOD
351200150330     C                   MOVEL(P)  d98lna        TBEKE1
351300150330     C                   MOVEL(P)  wdaodi        TBEKE2
351400150330     C     KTBE          CHAIN     TNTBE01L
351500150330     C                   IF        not %found(TNTBE01L)
351600150330     c                   clear                   tbeke2
351700150330     C     KTBE          CHAIN     TNTBE01L
351800150330     c                   endif
351900150330     C                   IF        not %found(TNTBE01L)
352000150330     C                   MOVEL(P)  '000'         TBEKE1
352100150330     C     KTBE          CHAIN     TNTBE01L
352200150330     c                   endif
352300150330     c
352400150330     C                   IF        %found(TNTBE01L) and
352500150330     c                             %subst(tbeuni:1:4)>*zeros
352600150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
352700150330     c                   endif
352800150330     c                   ENDSR
352900150325     C********************************************************************
353000150325     C*   Riclcolo data se colli in IMP/disguido
353100150325     C********************************************************************
353200150325     c     CalcoloIMP    BEGSR
353300150325     C                   Z-ADD     IDdata        WDAT
353400150325     C                   EXSR      DATORE
353500150325     C                   Z-ADD     WORE          WR4POI
353600150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
353700151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
353800150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
353900150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
354000150325     c* calcolo il terminal di partenza
354100150325     c                   clear                   fnlv55ds
354200150325     c                   eval      d55tla='L'
354300150325     c                   eval      d55tpt='P'
354400150325     c                   eval      d55lin=IDfgs
354500150325     c                   eval      d55drf=IDdata
354600150325     c                   call      'FNLV55R'
354700150325     c                   parm                    fnlv55ds
354800150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
354900150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
355000150325     c                   EXSR      RTTRAZ
355100150325    3c                   if        d98err<>'1'
355200150325     C                   Z-ADD     CASTcn        wr4noi
355300150325     c                   else
355400150325     C                   Z-ADD     d98ttc        wr4noi
355500150325    3C                   endif
355600150325
355700150325   x2c                   else
355800150325     C                   Z-ADD     d98ttc        wr4noi
355900150325    2C                   endif
356000150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
356100150325     c*  perchè partirà un giorno dopo
356200150325    3c                   if        IDnpg='P'
356300150325     c                   add       24            WR4noi
356400150325    3c                   endif
356500150325     c
356600150325     c                   exsr      ro4cdp
356700150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
356800150325     c                   ENDSR
356900930604     C********************************************************************
357000930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
357100930604     C********************************************************************
357200930604     C     TPODAT        BEGSR
357300960229     C*
357400960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
357500960229     C*
357600960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
357700960229     C     WPOPTI        IFEQ      *ZEROS
357800960229     C                   Z-ADD     *ZEROS        D98DTI
357900960229     C                   Z-ADD     *ZEROS        D98HTI
358000960229     C                   ELSE
358100960229     C                   Z-ADD     WPOPTI        WORE
358200960229     C                   EXSR      OREDAT
358300960229     C                   Z-ADD     WDAT          D98DTI
358400960229     C     WRES          MULT      100           D98HTI
358500960229     C                   END
358600960229     C*
358700960229     C                   ELSE                                                   <-- 1 -->
358800930604     C*
358900931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
359000140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
359100140825     c*******            exsr      Tras_DEI
359200140825     c
359300930604     C*
359400150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
359500140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
359600140820     c* è data da :
359700140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
359800140820     c*  Tenere  conto, ovvero slittare in avanti, per :
359900140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
360000140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
360100140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
360200140911     c*
360300140820     c*  Da azzerare nei seguenti casi:
360400150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
360500150324     c* 4) bolla export    7) Cod manca consegna
360600140911     c* 8) bolla con DCR DOPOil o PRIMAdel
360700150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
360800150119     c*10) se completamente non arrivata e collo in IMP
360900150119     c
361000140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
361100150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
361200140925     C*  - FD   - export -
361300150217     c                   movel     'S'           DEEinDIST         1
361400150113     c                   eval      wcaldpcee='S'
361500150304     c                   eval      D98SPCDEE='PRV'
361600150324     c                   clear                   avvcmc            1
361700150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
361800150323     c                   if        d98cmc<>*blanks
361900150324     C                   Z-ADD     1             II
362000150326     C     d98cmc        LOOKUP    C2A(II)                                99
362100150324     C                   MOVEL     D2A(II)       usa2A
362200150324    4C     §2AFTC        IFEQ      'A'
362300150326     c                   eval      avvcmc='A'
362400150323     c                   endif
362500150324    4C     §2AFTC        IFEQ      'G'
362600150326     c                   eval      avvcmc='G'
362700150324     c                   endif
362800150324     c                   endif
362900140820     c
363000140911     c                   z-add     d98lna        EX
363100150113     c*  la azzero nei casi previsti                 0
363200150601    1c                   select
363300150119     c* 1) Se la bolla e consegnata
363400140925     c                   when      d98dcm>0
363500150113     c                   clear                   wcaldpcee
363600150216     c                   eval      d98spcdee='CON'
363700150217     c                   movel     ' '           DEEinDIST         1
363800150323     c* 2) Se pratica di danno
363900150603     c                   when      flgdan='S'
364000150323     c                   clear                   wcaldpcee
364100150323     c                   eval      d98spcdee='CA '
364200150323     c                   eval      deeindist='S'
364300150324     c* 3) Se bloccata/rimpallo
364400150114     c                   when      wd98fbc='B'
364500150113     c                   clear                   wcaldpcee
364600150216     c                   eval      d98spcdee='BLO'
364700150217     c                   movel     ' '           DEEinDIST         1
364800150324     c* 4) Se bolla export
364900150324     c                   when      fl1(EX)='E'
365000150324     c                   clear                   wcaldpcee
365100150324     c                   movel     ' '           DEEinDIST         1
365200150324     c                   eval      d98spcdee='EXP'
365300150324     c* 5) Se giacente  o in attesa di apertura giacenza
365400150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
365500150324     c                   clear                   wcaldpcee
365600150324     c                   eval      d98spcdee='GIA'
365700150324     c                   eval      deeindist='S'
365800150119     c* 7) Se codice manca consegna
365900150326     c                   when      d98cmc<>*blanks and avvcmc='A'
366000150324     c                   clear                   wpodpcee
366100150324     c                   clear                   wcaldpcee
366200150324     c                   movel     ' '           DEEinDIST         1
366300150324     c                   eval      d98spcdee='ATT'
366400150410     c* 7) Bolla in sospensione consegna
366500150410     c                   when      §B4LBGE='S'
366600150410     c                   clear                   wpodpcee
366700150410     c                   clear                   wcaldpcee
366800150410     c                   movel     ' '           DEEinDIST         1
366900150410     c                   eval      d98spcdee='SOS'
367000150616     c
367100150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
367200150616     c                   when      wrbl='R'
367300150616     c                   clear                   wcaldpcee
367400150616     c                   eval      d98spcdee='   '
367500150616     c                   eval      deeindist='S'
367600150601   x1c                   other
367700150325     c*
367800150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
367900150331     c                   if        d98ffd='S'
368000150601    2c                   select
368100150403     c* FD escluso dai contatti
368200150601    2c                   when      §B4LTFD= 'N'
368300150403     c                   eval      d98spcdee='FD3'
368400150403     c                   clear                   wcaldpcee
368500150403     c                   movel     ' '           DEEinDIST         1
368600150403
368700150403     c* FD non contattato ( o da ricontattare)
368800150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
368900150401     c                             olv82sts<>'1'
369000150323     c                   clear                   wcaldpcee
369100150323     c                   movel     ' '           DEEinDIST         1
369200150323     c                   eval      d98spcdee='FFD'
369300150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
369400150424     c                   eval      wpodpcee=wpoffi
369500150403
369600150403     c* FD contattato e     IN distinta
369700150601    2c                   when      d98cons='S'
369800150401     c                   eval      d98spcdee='FD2'
369900150401     c                   clear                   wcaldpcee
370000150401     c                   movel     ' '           DEEinDIST         1
370100150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
370200150424     c                   eval      wpodpcee=wpoffi
370300150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
370400150427     c*  ritirare la merce
370500150427     C                   MOVEL     'C'           WTC                            *PARTENZA
370600150427     C                   MOVEL     'C'           WIC
370700150427     C                   Z-ADD     D98ddc        WDAT
370800150427     C                   EXSR      DATORE
370900150427     c                   z-add     wore          wr4poi
371000150427     c                   z-add     24            wr4noi
371100150427     C                   EXSR      RO4CDP
371200150427     c                   z-add     wr4poo        wore
371300150427     c                   exsr      OREDAT
371400150427     c                   movel     wdat          D98DTCTD
371500150601   x2c                   other
371600150403     c* FD contattato e NON in distinta
371700150331     c                   eval      d98spcdee='FD1'
371800150408     c                   eval      wpodpcee=wpoffi
371900150331     c                   clear                   wcaldpcee
372000150331     c                   movel     ' '           DEEinDIST         1
372100150601    2c                   endsl
372200150331     c
372300150331     c                   endif
372400150403     c
372500150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
372600150601    2c                   if        (d98tc1='A' or d98tc2='A' or
372700150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
372800150601     c                             and d98ffd= ' '
372900150325     c                   clear                   wcaldpcee
373000150325     c                   movel     ' '           DEEinDIST         1
373100150601    3c                   if        §B4LBAS= 'N'
373200150427     c                   eval      d98spcdee='AP3'
373300150403     c                   else
373400150403     c                   eval      d98spcdee='APP'
373500150601    3c                   endif
373600150601    2c                   endif
373700150325     c*   se la bolla ha il C/Assegno verifico se da contattare
373800150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
373900150325     c                   clear                   wcaldpcee
374000150325     c                   movel     ' '           DEEinDIST         1
374100150601    3c                   if        §B4LBAS= 'N'
374200150427     c                   eval      d98spcdee='CA3'
374300150403     c                   else
374400150403     c                   eval      d98spcdee='CAS'
374500150601    3c                   endif
374600150601    2c                   endif
374700150325     c* Per questi 3 casi devo impostare la data contatto destinatario
374800150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
374900150325     c                             d98spcdee='CAS'
375000150601    2c                   select
375100150601    2c                   when      d98dti>0 and wmancatraz= ' '
375200150325     c                   movel     d98dti        d98dtctd
375300150403     c* se piene le ore già portate ad un giorno lavorativo
375400150403     c                   if        wpodti>0
375500150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
375600150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
375700150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
375800150403     C                   Z-ADD     WPODTI        W10POI            9 0
375900150403     C                   EXSR      R10FES
376000150403     c                   z-add     w10poo        wore
376100150403     C                   EXSR      OREDAT
376200150403     C                   movel     WDAT          D98dtctd
376300150403     c                   endif
376400150403     c
376500150601    2c                   when      d98dam>0
376600150421     C                   Z-ADD     D98dam        WDAT
376700150421     C                   EXSR      DATORE
376800150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
376900150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
377000150421     C                   Z-ADD     Wore          W10POI            9 0
377100150421     C                   EXSR      R10FES
377200150421     c                   z-add     w10poo        wore
377300150421     C                   EXSR      OREDAT
377400150421     C                   movel     WDAT          D98dtctd
377500150601   x2c                   other
377600150325     c* data teorica di arrivo tenendo conto dell'IMP
377700160426     c                   if        d98nsp>0
377800150325     c                   exsr      letbrv
377900160426     c                   endif
378000150325     c                   if        idDATA>0
378100150325     c                   exsr      CalcoloIMP
378200150325     c                   z-add     wr1poi        WORE
378300150325     c                   else
378400150325     c                   z-add     wpoptc        WORE
378500150325     c                   endif
378600150325     C                   EXSR      OREDAT
378700150325     c                   movel     wdat          d98dtctd
378800150601    2c                   endsl
378900150325     c
379000150325     c                   endif
379100150325     c
379200150601    1c                   endsl
379300140820     c
379400150113     c* Se la bolla non è in distinta calcolo la data se previsto
379500150220     c                   if        wcaldpcee='S' and
379600150220     c                             (d98cons<>'S'  or (d98cons='S' and
379700150220     c                             evemic<>'SI'))
379800150219     c
379900150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
380000150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
380100150219     c                   if        d98nsp>0 and wtrasm<>'D' and
380200150219     c                             d98dam=0
380300150219     c                   exsr      letBRV
380400150219     c                   endif
380500150219     c
380600150217     c                   eval      d98spcdee='PRG'
380700150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
380800150320     c                             d98tc1='S' or d98tc2='S')
380900150320     c                   eval      d98spcdee='APP'
381000150320     c                   endif
381100150320     c
381200150113     c                   exsr      cal_DEE
381300150113     c                   endif
381400150113     c
381500140820     C     WPOdpcee      IFEQ      *ZEROS
381600931202     C                   Z-ADD     *ZEROS        D98DEE
381700931202     C                   Z-ADD     *ZEROS        D98OEE
381800930604     C                   ELSE
381900140820     C                   Z-ADD     WPOdpcee      WORE
382000141021     C******             SUB       12            WORE
382100930604     C                   EXSR      OREDAT
382200931202     C                   Z-ADD     WDAT          D98DEE
382300090903     c                   if        d98tcc<>10
382400090903     C                   ADD       12            WRES
382500090903     c                   else
382600090903     C                   ADD       10            WRES
382700090903     c                   endif
382800090903
382900931202     C     WRES          MULT      100           D98OEE
383000090903     c
383100090903     c                   if        d98oee=1000
383200090903     c                   eval      d98oee=1030
383300090903     c                   endif
383400140925     C                   END
383500150311     c
383600150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
383700150311     c                   select
383800150311     c                   when      d98spcdee='OLP'
383900140820     c
384000150220     c* Se bolla in distinta --> data prevista = data distinta
384100150311     c                   when      deeindist='S'  and
384200150113     c                             d98cons='S'
384300150220
384400150225     c*  non la propongo senza MIC e se presente disguido o IMP
384500150220     c                   if        d98dee>d98ddc and evemic<>'SI'
384600150220     c                             and IDdata>0
384700150220     c                   else
384800150220
384900140820     c                   eval      d98dee=d98ddc
385000140925     c                   clear                   d98oee
385100150320     c* se la data è > di oggi stato PRV se non è in IMP
385200150320     c                   if        d98dee>wdaodi
385300150320     c                   if        d98spcdee<>'IMP'
385400150311     c                   eval      d98spcdee='PRV'
385500150320     c                   endif
385600150320     c
385700150225     c                   else
385800150217     c* se c'è il MIC
385900150217     c                   if        evemic='SI'
386000150217     c                   eval      d98spcdee='DDC'
386100150225     c                   endif
386200150217     c                   endif
386300150220     c                   endif
386400150217     c
386500150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
386600150722     c                   eval      d98spcdee='PRV'
386700150311     c                   endsl
386800931202     C*
386900931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
387000931202     C     WPODCI        IFEQ      *ZEROS
387100931202     C                   Z-ADD     *ZEROS        D98DCI
387200931202     C                   Z-ADD     *ZEROS        D98HCI
387300931202     C                   ELSE
387400931202     C                   Z-ADD     WPODCI        WORE
387500931202     C                   EXSR      OREDAT
387600931202     C                   Z-ADD     WDAT          D98DCI
387700090902     c                   if        d98tci<>10
387800931202     C                   ADD       12            WRES
387900090902     c                   else
388000090902     C                   ADD       10            WRES
388100090902     c                   endif
388200090902
388300931202     C     WRES          MULT      100           D98HCI
388400090901     c
388500090903     c                   if        d98hci=1000
388600090901     c                   eval      d98hci=1030
388700090901     c                   endif
388800090901     c
388900931202     C                   END
389000931202     C*
389100931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
389200931202     C     WPODCE        IFEQ      *ZEROS
389300931202     C                   Z-ADD     *ZEROS        D98DCE
389400931202     C                   Z-ADD     *ZEROS        D98HCE
389500931202     C                   ELSE
389600931202     C                   Z-ADD     WPODCE        WORE
389700931202//// C**************       SUB  12        WORE
389800931202     C                   EXSR      OREDAT
389900931202     C                   Z-ADD     WDAT          D98DCE
390000090902     c                   if        d98tcc >10
390100931202     C                   ADD       12            WRES
390200090902     c                   else
390300090902     C                   ADD       10            WRES
390400090902     c                   endif
390500090902     c
390600931202     C     WRES          MULT      100           D98HCE
390700090902     c
390800090903     c                   if        d98hce=1000
390900090902     c                   eval      d98hce=1030
391000090902     c                   endif
391100090902     c
391200931202     C                   END
391300960229     C*
391400960229     C                   ENDIF                                                  <-- 1 ---
391500930604     C*
391600930604     C                   ENDSR
391700150218     C********************************************************************
391800150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
391900150218     C********************************************************************
392000150218     c     LetBRV        BEGSR
392100150219     c                   clear                   bollaOK           1
392200150219     c                   clear                   IDnpg
392300150219     c                   clear                   IDdata
392400150219     c                   clear                   IDora
392500150219     c                   clear                   IDfgs
392600150219     c
392700150218     C     Kart          SETLL     FNART01L
392800150218     C     Kart          READE     FNART01L
392900151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
393000150219     c                   clear                   DISfgs
393100150219     c                   clear                   DISdata
393200150219     c                   clear                   DISnpg
393300150219     c                   clear                   DISora
393400150219     c                   clear                   IMPdata
393500150219     c                   clear                   IMPfgs
393600150508     c* per ogni collo verifico se in IMP o disguidato
393700150218     c* al primo che NON trovo, esco come --> OK
393800150218     c     kbrv          setll     fnbrv07l
393900150218     c     kbrv          reade     fnbrv07l
394000150219    2c                   dow       not %eof(fnbrv07l)
394100150219    3c                   if        brvatr='D'  or brvnpg=3
394200150218     c* prendo la data della spunta
394300150218     c                   clear                   fnlv53ds
394400150219     c                   eval      d53tla='L'
394500150218     C                   MOVEL     BRvNPG        D53NPG
394600150218     C                   Z-ADD     BRvNFV        D53NFV
394700150218     C                   MOVEL     BRvFGS        D53FGS
394800150218     C                   CALL      'FNLV53R'
394900150218     C                   PARM                    fnlv53ds
395000150218    4c                   if        d53err<>*blanks
395100150219    5c                   if        brvdcs>0
395200150218     c                   eval      d53dfv=brvdcs
395300150218     c                   else
395400150219    6c                   if        brvdfs>0
395500150218     c                   eval      d53dfv=brvdfs
395600150219    6c                   endif
395700150219    5c                   endif
395800150219    4c                   endif
395900150218     c
396000150219    4c                   select
396100150219     c* se si tratta di DISGUIDO
396200150218     c                   when      brvatr='D'
396300150219     c
396400150219    5c                   if        d53dfv=DISdata
396500150219     c* Verifico l'ora
396600150219    6c                   if        brvhcs>0 and brvhcs>DISora
396700150219     c                   eval      DISdata=d53dfv
396800150219     c                   eval      DISfgs =brvfgs
396900150219     c                   eval      DISnpg =brvnpg
397000150219     c                   eval      DISora =brvhcs
397100150219   x6c                   else
397200150219    7c                   if        brvhms>0 and brvhms>DISora
397300150219     c                   eval      DISdata=d53dfv
397400150219     c                   eval      DISfgs =brvfgs
397500150219     c                   eval      DISnpg =brvnpg
397600150219     c                   eval      DISora =brvhms
397700150219    7c                   endif
397800150219    6c                   endif
397900150219    5c                   endif
398000150219     c
398100150219    5c                   if        d53dfv>DISdata
398200150219     c                   eval      DISdata=d53dfv
398300150219     c                   eval      DISfgs =brvfgs
398400150219     c                   eval      DISnpg =brvnpg
398500150219     c                   eval      DISora =brvhcs
398600150219     c                   if        disora=0
398700150219     c                   eval      DISora =brvhms
398800150219     c                   endif
398900150219    5c                   endif
399000150219     c
399100151009     c* se si tratta di IMP
399200151009    4c                   when      d53spg='P' and d53dfv>impdata
399300151009     c                   eval      IMPdata=d53dfv
399400151009     c                   eval      IMPfgs =brvfgs
399500150219    4c                   endsl
399600150219    3c                   endif
399700151009
399800150219     c     kbrv          reade     fnbrv07l
399900150219    2c                   enddo
400000150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
400100150219     c*  esco dal ciclo: la bolla è OK
400200150219    2c                   if        impdata=0 and disdata=0
400300150219     c                   eval      bollaOK='S'
400400150219     c                   clear                   IDdata
400500150219     c                   clear                   IDfgs
400600150219     c                   clear                   IDora
400700150219     c                   clear                   IDnpg
400800150219   x2c                   else
400900150219     c* memorizzo la data più alta
401000150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
401100150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
401200150219     c                   eval      IDdata=IMPdata
401300150219     c                   eval      IDfgs=IMPfgs
401400150219     c                   eval      IDora=0
401500150219     c                   eval      IDnpg='P'
401600150219    3c                   endif
401700150219    3c                   if        (DISdata>0 and DISdata=IDdata
401800150219     c                             and IDnpg<>'P' and DISora>IDora) or
401900150219     c                             (DISdata>0 and DISdata>IDdata)
402000150219     c                   eval      IDdata=DISdata
402100150219     c                   eval      IDfgs=DISfgs
402200150219     c                   eval      IDora=DISora
402300150219     c                   eval      IDnpg=%char(DISnpg)
402400150219    3c                   endif
402500150219    2c                   endif
402600150218     c
402700150218     C     Kart          READE     FNART01L
402800150219    1c                   enddo
402900150219     c
403000150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
403100150508     c**                 if        d98duc> 0 and d98duc>IDdata
403200150508     c**                 eval      bollaOK='S'
403300150508     c**                 clear                   IDdata
403400150508     c**                 clear                   IDfgs
403500150508     c**                 clear                   IDora
403600150508     c**                 clear                   IDnpg
403700150508     c**                 endif
403800150219     c
403900150218     c
404000150218     C                   ENDSR
404100140825     C********************************************************************
404200140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
404300140825     C********************************************************************
404400140825     c     TRAS_DEI      BEGSR
404500140825     C     WPODEI        IFEQ      *ZEROS
404600140825     C                   Z-ADD     *ZEROS        D98DEI
404700140825     C                   Z-ADD     *ZEROS        D98OEI
404800140825     C                   ELSE
404900140825     C                   Z-ADD     WPODEI        WORE
405000140825     C                   SUB       12            WORE
405100140825     C                   EXSR      OREDAT
405200140825     c
405300140825     C                   Z-ADD     WDAT          D98DEI
405400140825     c                   if        d98tci<>10
405500140825     C                   ADD       12            WRES
405600140825     c                   else
405700140825     C                   ADD       10            WRES
405800140825     c                   endif
405900140825     c
406000140825     C     WRES          MULT      100           D98OEI
406100140825     c
406200140825     c                   if        d98oei=1000
406300140825     c                   eval      d98oei=1030
406400140825     c                   endif
406500140825     c
406600140825     C                   END
406700140825     c                   ENDSR
406800930604     C********************************************************************
406900930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
407000930604     C********************************************************************
407100930604     C     CHKFOR        BEGSR
407200930604     C*
407300930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
407400930604     C* E QUINDI FORZO LA CONFORMITA' A SI
407500941227     C     D98ERR        IFEQ      '0'
407600930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
407700930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
407800930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
407900930604     C                   MOVEL     '1'           D98ERR
408000930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
408100930604     C                   END
408200930604     C                   END
408300990212     C*
408400990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
408500990212     C* E QUINDI FORZO LA CONFORMITA' A SI
408600990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
408700990212     C     D98ERR        IFEQ      '0'
408800990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
408900141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
409000990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
409100990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
409200990212     C                   MOVEL     '1'           D98ERR
409300990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
409400990212     C                   END
409500990212     C                   END
409600990308     C*
409700990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
409800990308     C* E QUINDI FORZO LA CONFORMITA' A SI
409900100614     C**   D98ERR        IFEQ      '0'
410000100614     C**   WFBR          IFEQ      'S'                                          *RESO
410100100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
410200100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
410300100614     C**                 MOVEL     '1'           D98ERR
410400100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
410500100614     C**                 END
410600100614     C**                 END
410700930604     C*
410800930604     C                   ENDSR
410900990212     C********************************************************************
411000990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
411100990212     C********************************************************************
411200990212     C     CHKDAN        BEGSR
411300990212     C*
411400990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
411500150603     c                   movel     'N'           opendan           1
411600040809      *
411700040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
411800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
411900040809      * legate alla spedizione che passo
412000040809     c                   clear                   fidn12ds
412100040809     c                   eval      i12aas = D98AAS
412200040809     c                   eval      i12lnp = D98LNP
412300040809     c                   eval      i12nrs = D98NRS
412400040809     c                   eval      i12nsp = D98NSP
412500040809     c                   eval      i12fel = 'E'
412600040809     c                   eval      i12fan = 'E'
412700040809     c*
412800040809     c                   call      'FIDN12R'
412900040809     c                   parm                    fidn12ds
413000040809     c*
413100040809     c* se non ci sono errori
413200040809     c                   if        o12err = ' '
413300040809     c* se numero di CA trovate maggiore di zero
413400040809     c                   if        o12nca > 0
413500040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
413600150603     c
413700150603     c***                z-add     1             aa                2 0
413800150603     c***                dow       aa <= o12nca
413900150603     C* APERTA
414000150603     C***  skdch(aa)     IFEQ      0
414100150603     c***                movel     'S'           opendan
414200150603     c***                endif
414300150603     c*
414400150603     c***                add       1             aa
414500150603     c***                enddo
414600150603     c
414700040809     c                   endif
414800040809     c                   endif
414900990212     C*
415000990212     C                   ENDSR
415100941228     C********************************************************************
415200941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
415300941228     C********************************************************************
415400941228     C     CALORP        BEGSR
415500941228     C*
415600941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
415700941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
415800941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
415900960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
416000950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
416100950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
416200950110     C                   ELSE
416300950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
416400950110     C                   ENDIF
416500941228     C                   EXSR      RO2COP
416600941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
416700941228     C* SPED. IL GIORNO DOPO
416800941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
416900941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
417000941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
417100941228     C*
417200941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
417300941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
417400941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
417500941228     C                   Z-ADD     WPODSP        WR2POI
417600950110     C                   Z-ADD     WPOPPC        WR2POF
417700941228     C                   EXSR      RO2COP
417800941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
417900941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
418000941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
418100941228     C*
418200941228     C                   ENDSR
418300930414     C********************************************************************
418400930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
418500930414     C********************************************************************
418600930414     C     CALORT        BEGSR
418700930414     C*
418800930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
418900931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
419000930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
419100931126     C                   Z-ADD     *ZEROS        W98HF
419200950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
419300950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
419400950110     C                   ELSE
419500950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
419600950110     C                   ENDIF
419700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
419800931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
419900930608     C                   ELSE
420000931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
420100930608     C                   END
420200931126     C                   EXSR      RO2COV
420300931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
420400930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
420500930414     C*
420600930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
420700930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
420800930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
420900931126     C                   Z-ADD     *ZEROS        W98HF
421000950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
421100931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
421200931126     C                   EXSR      RO2COV
421300931126     C                   Z-ADD     WR2NOV        W98RTC
421400930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
421500930414     C*
421600930414     C                   ENDSR
421700930414     C********************************************************************
421800930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
421900930414     C********************************************************************
422000930414     C     CALORC        BEGSR
422100930511     C*
422200930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
422300930511     C* I TEMPI DI RITARDO AD OGGI
422400930511     C     WPODCM        IFEQ      *ZEROS
422500960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
422600930511     C                   END
422700940117     C*
422800940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
422900940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
423000940117     C* ALLA DATA DI FINE CONSEGNABILITA'
423100940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
423200940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
423300940118££££ C     WPODFC        IFNE      *ZEROS
423400940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
423500940118££££ C                   END
423600930414     C*
423700930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
423800930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
423900930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
424000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
424100931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
424200930608     C                   ELSE
424300931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
424400930608     C                   END
424500931126     C                   Z-ADD     WPODCM        WR5POF            9 0
424600931126     C                   EXSR      RO5COV
424700931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
424800930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
424900930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
425000060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
425100060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
425200930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
425300930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
425400930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
425500930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
425600930414     C*
425700930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
425800930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
425900930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
426000931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
426100931126     C                   Z-ADD     WPODCM        WR5POF            9 0
426200931126     C                   EXSR      RO5COV
426300931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
426400930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
426500930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
426600060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
426700060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
426800930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
426900930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
427000930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
427100930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
427200930414     C*
427300930414     C                   ENDSR
427400930414     C********************************************************************
427500930414     C*   CALORR - CALCOLA ORE REALI DI RESA
427600930414     C********************************************************************
427700930414     C     CALORR        BEGSR
427800930414     C*
427900950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
428000950110     C                   ADD       W98RTI        W98RRI
428100941228     C                   ADD       W98RCI        W98RRI
428200950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
428300950110     C                   ADD       W98RTC        W98RRC
428400941228     C                   ADD       W98RCC        W98RRC
428500930414     C*
428600930414     C                   ENDSR
428700930420     C********************************************************************
428800930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
428900930420     C********************************************************************
429000930420     C     CALDIF        BEGSR
429100930420     C*
429200941228     C* PARTENZA
429300941228     C     W98RPI        SUB       D98TPI        W98FPI
429400941228     C     W98RPC        SUB       D98TPC        W98FPC
429500941228     C*
429600930420     C* TRAZIONE
429700930423     C     W98RTI        SUB       D98TTI        W98FTI
429800930423     C     W98RTC        SUB       D98TTC        W98FTC
429900930420     C*
430000930420     C* CONSEGNA
430100090827     C***  W98RCI        SUB       D98TCI        W98FCI
430200090827     C     W98RCI        SUB       w99TCI        W98FCI
430300090827     C***  W98RCC        SUB       D98TCC        W98FCC
430400090827     C     W98RCC        SUB       w99TCC        W98FCC
430500930420     C*
430600930420     C* RESA
430700090921     C***  W98RRI        SUB       D98TRI        W98FRI
430800090921     C     W98RRI        SUB       w99TRI        W98FRI
430900090921     c
431000090921     C***  W98RRC        SUB       D98TRC        W98FRC
431100090921     C     W98RRC        SUB       w99TRC        W98FRC
431200930420     C*
431300930420     C                   ENDSR
431400930420     C********************************************************************
431500930420     C*   CALCNF - CALCOLA LE CONFORMITA'
431600930420     C********************************************************************
431700930420     C     CALCNF        BEGSR
431800941228     C*
431900941228     C*-------------------------------------------------------------------
432000941228     C* CONFORMITA' PARTENZA  INTERNA
432100941228     C*-------------------------------------------------------------------
432200941228     C                   Z-ADD     W98FPI        W98NPI
432300941228     C     W98FPI        IFLE      *ZEROS
432400941228     C                   MOVEL     'S'           D98CPI
432500941228     C                   ELSE
432600941228     C                   MOVEL     'N'           D98CPI
432700941228     C                   END
432800941228     C*
432900941228     C*-------------------------------------------------------------------
433000941228     C* CONFORMITA' PARTENZA  CLIENTE
433100941228     C*-------------------------------------------------------------------
433200941228     C                   Z-ADD     W98FPC        W98NPC
433300941228     C     W98FPC        IFLE      *ZEROS
433400941228     C                   MOVEL     'S'           D98CPC
433500941228     C                   ELSE
433600941228     C                   MOVEL     'N'           D98CPC
433700941228     C                   END
433800930420     C*
433900930420     C*-------------------------------------------------------------------
434000930420     C* CONFORMITA' TRAZIONE  INTERNA
434100930420     C*-------------------------------------------------------------------
434200930510     C                   Z-ADD     W98FTI        W98NTI
434300930423     C     W98FTI        IFLE      *ZEROS
434400930420     C                   MOVEL     'S'           D98CTI
434500930420     C                   ELSE
434600930420     C                   MOVEL     'N'           D98CTI
434700930420     C                   END
434800930420     C*
434900930420     C*-------------------------------------------------------------------
435000930420     C* CONFORMITA' TRAZIONE  CLIENTE
435100930420     C*-------------------------------------------------------------------
435200930510     C                   Z-ADD     W98FTC        W98NTC
435300930423     C     W98FTC        IFLE      *ZEROS
435400930420     C                   MOVEL     'S'           D98CTC
435500930420     C                   ELSE
435600930420     C                   MOVEL     'N'           D98CTC
435700930420     C                   END
435800930420     C*
435900930420     C*-------------------------------------------------------------------
436000930420     C* CONFORMITA' CONSEGNA  INTERNA
436100930420     C*-------------------------------------------------------------------
436200150226     c                   if        wmancatraz='S'
436300150226     c                   clear                   d98dti
436400150226     c                   clear                   d98hti
436500150226     c                   endif
436600150226     c
436700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
436800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
436900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
437000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
437100050914     c                   if        consideraora='S'
437200050914     c     w98fci        sub       24            w98nci
437300050914     c                   add       24            d98hrc
437400050914     c                   else
437500930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
437600050914     c                   endif
437700080116     c
437800110923     c                   select
437900080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
438000080116     c*  --> trasformo il ritardo in 0
438100090827     c                   when      d98cca='2'
438200080116     c                   z-add     0             w98nci
438300090827
438400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
438500080116     c*  e sto leggendo la bolla in arrivo
438600080116     c*  --> trasformo il ritardo = 0
438700090827     c                   when      d98cca='1' and d98tbo='A'
438800080116     c                   z-add     0             w98nci
438900090902     c
439000090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
439100090902     c                   when      wmancaTRAZ='S'
439200090902     c                   z-add     0             w98nci
439300090902     c                   clear                   d98dti
439400090902     c                   clear                   d98hti
439500110923     c
439600110923     c* Ignoro ritardo se pefect order
439700130221     c                   when      d98xco >*zeros
439800110923     c                   z-add     0             w98nci
439900090827     c
440000090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
440100091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
440200091014     c*  controllo ora soglia  consegna da tabella OSC)
440300091009     c                   when      d98tci=10   and
440400091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
440500091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
440600091014     c*
440700091014     c                             d98hci=1030
440800091014     C                   MOVEL     'OSC  '       WR3TAB
440900091014     C                   eval      WR3HHI=d98hmc
441000091014     C                   EXSR      RO3ORE
441100131218
441200091014     c                   if        wr3matpom='M' and d98hmc>1030
441300131218     c                   exsr      ctrLAVH1030
441400131218
441500131218     c                   if        wlav1030<>'S'
441600091014     c                   add       12            w98nci
441700091014     c                   endif
441800131218     c                   endif
441900090928     c
442000090827     c                   endsl
442100050914     c
442200930524     C                   ELSE                                                   <-- 1 -->
442300930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
442400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
442500930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
442600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
442700930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
442800930608     C                   ELSE
442900930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
443000930608     C                   END
443100930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
443200930525     C                   EXSR      RO2COV
443300930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
443400930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
443500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
443600930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
443700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
443800930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
443900930608     C                   ELSE
444000930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
444100930608     C                   END
444200931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
444300930525     C                   EXSR      RO2COV
444400930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
444500930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
444600930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
444700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
444800930525     C* E' IL RITARDO
444900930610     C     WNOCRI        SUB       W98RCI        W98NCI
445000050901     c
445100050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
445200050901     c*  --> trasformo il ritardo in anticipo
445300050901     c                   if        d98cca='2'
445400080116     c***                z-sub     w98nci        w98nci
445500080116     c                   z-add     0             w98nci
445600050901     c                   endif
445700111018     c* Ignoro ritardo se pefect order
445800130221     c                   if        d98xco >*zeros
445900111018     c                   z-add     0             w98nci
446000111018     c                   endif
446100061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
446200061121     c*  e sto leggendo la bolla in arrivo
446300061121     c*  --> trasformo il ritardo in anticipo
446400061121     c                   if        d98cca='1' and d98tbo='A'
446500080116     c***                z-sub     w98nci        w98nci
446600080116     c                   z-add     0             w98nci
446700061121     c                   endif
446800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
446900070827     c*  mittente, --> consegnato in anticipo
447000070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
447100140403     c                   if        Callar5=' '
447200070827     c                   exsr      LeggiAR5
447300140403     c                   endif
447400070827     c
447500070827     c                   if        dcrpart<>'S'
447600070824     c                   z-sub     w98nci        w98nci
447700070824     c                   endif
447800070827     c
447900070824     c                   endif
448000050907     c
448100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
448200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
448300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
448400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
448500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
448600050916     c***                if        d98dfc>0
448700070828     c                   if        d98dcm=0  and w98nci>0
448800050907     c                   z-sub     w98nci        w98nci
448900050907     c                   sub       oreILoDOPO    w98nci
449000050907     c                   endif
449100050901     c
449200930525     C                   ELSE                                                   <-- 2 -->
449300070521     c* non do ritardo se la consegna reale cade nel giorno
449400070521     c*  della consegna richiesta se presente IL
449500100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
449600070521     c                             wpodcm<=wpocr1
449700070521     c                   clear                   w98nci
449800100224     c                   else                                                   ---2a -->
449900930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
450000090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
450100090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
450200930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
450300930525     C* E' IL RITARDO O L'ANTICIPO
450400930525     C     W98RCI        SUB       WNOCRF        W98NCI
450500070115     c
450600070115     c* non do ritardo se comunque la consegna reale cade nel giorno
450700070115     c*  della consegna richiesta se presente IL
450800070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
450900070521     c***                          wpodcm<=wpocr1  and w98nci>0
451000070521     c***                clear                   w98nci
451100070521     c***                endif
451200070115     c
451300930525     C                   ELSE                                                   <-- 3 -->
451400930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
451500930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
451600930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
451700930525     C                   END                                                    <-- 3 ---
451800100224     C                   END                                                    <-- 2a---
451900100224
452000100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
452100100224     c                   select
452200100224     c                   when      d98cca='2'
452300100224     c                   z-add     0             w98nci
452400100224
452500100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
452600100224     c*  e sto leggendo la bolla in arrivo
452700100224     c                   when      d98cca='1' and d98tbo='A'
452800100224     c                   z-add     0             w98nci
452900100224     c                   endsl
453000100224     c
453100100224     C                   END                                                    <-- 2 ---
453200070521     C                   END                                                    <-- 1 ---
453300070521     c
453400970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
453500970915     C*/         WAZNCI    IFEQ 'S'
453600970915     C*/         W98NCI    ANDLT*ZEROS
453700970915     C*/                   Z-ADD*ZEROS    W98NCI
453800970915     C*/                   ENDIF
453900970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
454000050922     c* lo faccio solo se non c'e' la data cons richiesta
454100970915     C     D98DTI        IFEQ      *ZEROS
454200050922     c     d98dcr        andeq     *zeros
454300970915     C                   Z-ADD     *ZEROS        W98NCI
454400970915     C                   ENDIF
454500930524     C*
454600930524     C* CONTROLLO CONFORMITA
454700930423     C     W98NCI        IFLE      *ZEROS
454800930524     C                   MOVEL     'S'           D98CCI
454900930420     C                   ELSE
455000930524     C                   MOVEL     'N'           D98CCI
455100930420     C                   END
455200930420     C*
455300930420     C*-------------------------------------------------------------------
455400930420     C* CONFORMITA' CONSEGNA  CLIENTE
455500930420     C*-------------------------------------------------------------------
455600930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
455700930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
455800930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
455900050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
456000050914     c                   if        consideraora='S'
456100050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
456200050914     c                   else
456300050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
456400050914     c                   endif
456500080116     c
456600080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
456700080116     c*  --> trasformo il ritardo =0
456800090827     c                   select
456900090827     c                   when      d98cca='2'
457000080116     c                   z-add     0             w98ncc
457100090827
457200080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
457300080116     c*  e sto leggendo la bolla in arrivo
457400080116     c*  --> trasformo il ritardo =0
457500090827     c                   when      d98cca='1' and d98tbo='A'
457600080116     c                   z-add     0             w98ncc
457700090902     c
457800090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
457900090902     c                   when      wmancaTRAZ='S'
458000090902     c                   z-add     0             w98ncc
458100110923     c* Ignoro ritardo se Perfect Order
458200130221     c                   when      d98xco >*zeros
458300110923     c                   z-add     0             w98ncc
458400090902     c
458500090827     c
458600090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
458700090827     c* sono  in ritardo di 12 ore
458800091009     c                   when      d98tcc=10  and
458900091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
459000091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
459100091014     c*
459200091014     c                             d98hce=1030
459300091014     C                   MOVEL     'OSC  '       WR3TAB
459400091014     C                   eval      WR3HHI=d98hmc
459500091014     C                   EXSR      RO3ORE
459600091014     c                   if        wr3matpom='M' and d98hmc>1030
459700131218     c                   exsr      ctrLAVH1030
459800131218
459900131218     c                   if        wlav1030<>'S'
460000131218     c                   add       12            w98ncc
460100131218     c                   endif
460200131218     c****               add       12            w98ncc
460300091014     c                   endif
460400131218     c
460500091014     c***                z-add     12            w98ncc
460600090827     c                   endsl
460700050914     c
460800930524     C                   ELSE                                                   <-- 1 -->
460900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
461000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
461100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
461200930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
461300930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
461400930525     C                   EXSR      RO2COV
461500930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
461600930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
461700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
461800930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
461900930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
462000931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
462100930525     C                   EXSR      RO2COV
462200930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
462300930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
462400930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
462500930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
462600930525     C* E' IL RITARDO
462700930525     C     WNOCRI        SUB       W98RCC        W98NCC
462800050901     c
462900050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
463000050901     c*  --> trasformo il ritardo in anticipo
463100050901     c                   if        d98cca='2'
463200080116     c***                z-sub     w98ncc        w98ncc
463300080116     c                   z-add     0             w98ncc
463400050901     c                   endif
463500111018     c* Ignoro ritardo se pefect order
463600130221     c                   if        d98xco >*zeros
463700111018     c                   z-add     0             w98ncc
463800111018     c                   endif
463900061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
464000061121     c*  e sto leggendo la bolla in arrivo
464100061121     c*  --> trasformo il ritardo in anticipo
464200061121     c                   if        d98cca='1' and d98tbo='A'
464300080116     c***                z-sub     w98ncc        w98ncc
464400080116     c                   z-add     0             w98ncc
464500061121     c                   endif
464600070824     c
464700070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
464800070827     c*  mittente, --> consegnato in anticipo
464900070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
465000070827     c* SBR per vedere se tassativa
465100070827     c                   if        Callar5=' '
465200070827     c                   EXSR      LeggiAR5
465300070827     c                   endif
465400070827     c                   if        dcrpart<>'S'
465500070824     c                   z-sub     w98ncc        w98ncc
465600070824     c                   endif
465700070824     c                   endif
465800050907     c
465900050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
466000050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
466100050907     c*  lo trasformo in anticipo perchè non sono in ritardo
466200050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
466300050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
466400050916     c***                if        d98dfc>0
466500070828     c                   if        d98dcm=0 and w98ncc>0
466600050907     c                   z-sub     w98ncc        w98ncc
466700050907     c                   sub       oreILoDOPO    w98ncc
466800050907     c                   endif
466900050901     c
467000930525     C                   ELSE                                                   <-- 2 -->
467100070521     c* Non do ritardo se la consegna reale cade nel giorno
467200070521     c*  della consegna richiesta se presente IL
467300100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
467400070521     c                             wpodcm<=wpocr2
467500070521     c                   clear                   w98ncc
467600100224     c                   else                                                   <--2a -->
467700070521     c
467800930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
467900090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
468000090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
468100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
468200930525     C* E' IL RITARDO O L'ANTICIPO
468300930525     C     W98RCC        SUB       WNOCRF        W98NCC
468400070115     c
468500070115     c* non do ritardo se comunque la consegna reale cade nel giorno
468600070115     c*  della consegna richiesta se presente IL
468700070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
468800070521     c***                          wpodcm<=wpocr2 and w98ncc>0
468900070521     c***                clear                   w98ncc
469000070521     c***                endif
469100070115     c
469200930525     C                   ELSE                                                   <-- 3 -->
469300930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
469400930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
469500930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
469600930525     C                   END                                                    <-- 3 ---
469700100224     C                   END                                                    <-- 2a---
469800100224
469900100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
470000100224     c                   select
470100100224     c                   when      d98cca='2'
470200100224     c                   z-add     0             w98nci
470300100224
470400100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
470500100224     c*  e sto leggendo la bolla in arrivo
470600100224     c                   when      d98cca='1' and d98tbo='A'
470700100224     c                   z-add     0             w98nci
470800100224     c                   endsl
470900100224     c
471000100224     C                   END                                                    <-- 2 ---
471100070521     C                   END                                                    <-- 1 ---
471200930524     C*
471300930524     C* CONTROLLO CONFORMITA
471400930423     C     W98NCC        IFLE      *ZEROS
471500930524     C                   MOVEL     'S'           D98CCC
471600930420     C                   ELSE
471700930524     C                   MOVEL     'N'           D98CCC
471800930420     C                   END
471900930420     C*
472000930420     C*-------------------------------------------------------------------
472100930420     C* CONFORMITA' RESA INTERNA
472200930420     C*-------------------------------------------------------------------
472300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
472400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
472500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
472600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
472700050914     c                   if        consideraora='S'
472800050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
472900050914     c                   else
473000050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
473100050914     c                   endif
473200080116     c
473300080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
473400090827     c                   select
473500090827     c                   when      d98cca='2' and w98nri>0
473600080116     c                   z-add     0             w98nri
473700090827     c
473800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
473900080116     c*  e sto leggendo la bolla in arrivo
474000090827     c                   when      d98cca='1' and d98tbo='A' and
474100080116     c                                            w98nri>0
474200080116     c                   z-add     0             w98nri
474300110923     c
474400110923     c* Ignoro ritardo se Perfect Order
474500130221     c                   when      d98xco >*zeros
474600110923     c                   z-add     0             w98nri
474700110923     c
474800090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
474900090827     c* sono  in ritardo di 12 ore
475000091009     c                   when      d98tci=10  and
475100091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
475200091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
475300091014     c*
475400091014     c                             d98hci=1030
475500091014     C                   MOVEL     'OSC  '       WR3TAB
475600091014     C                   eval      WR3HHI=d98hmc
475700091014     C                   EXSR      RO3ORE
475800091014     c                   if        wr3matpom='M' and d98hmc>1030
475900131218     c                   exsr      ctrLAVH1030
476000131218
476100131218     c                   if        wlav1030<>'S'
476200091014     c                   add       12            w98nri
476300091014     c                   endif
476400131218     c                   endif
476500091014     c
476600091014     c***                z-add     12            w98nri
476700090827     c                   endsl
476800090827
476900930524     C                   ELSE                                                   <-- 1 -->
477000930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
477100930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
477200930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
477300931206     C     WPODCM        ANDLE     WPOCR1
477400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
477500930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
477600930525     C                   Z-ADD     WPODCM        WR2POI            9 0
477700931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
477800930525     C                   EXSR      RO2COV
477900930525     C     WR2NOV        IFNE      *ZEROS
478000930525     C     WR2NOV        MULT      -1            W98NRI
478100930525     C                   END
478200930524     C                   ELSE                                                   <-- 2 -->
478300930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
478400930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
478500930524     C     WPODCM        IFLT      WPOCRI
478600930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
478700930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
478800930524     C                   ELSE
478900931206     C                   Z-ADD     WPOCR1        WPORFI
479000930524     C                   Z-ADD     WPODCM        WPORFF
479100930524     C                   END
479200930524     C* CALCOLO ORE DI DIFFERENZA
479300930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
479400930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
479500930524     C                   Z-ADD     WPORFI        WR2POI            9 0
479600930524     C                   Z-ADD     WPORFF        WR2POF            9 0
479700930524     C                   EXSR      RO2COV
479800930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
479900050902     c
480000050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
480100100224     c                   if        d98cca='2' and w98nri>0
480200100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
480300080116     c***                z-sub     w98nri        w98nri
480400080116     c                   z-add     0             w98nri
480500050902     c                   endif
480600111018     c* Ignoro ritardo se pefect order
480700130221     c                   if        d98xco >*zeros
480800111018     c                   z-add     0             w98nri
480900111018     c                   endif
481000061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
481100061121     c*  e sto leggendo la bolla in arrivo
481200100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
481300100224     c***                if        d98cca='1' and d98tbo='A' and
481400100224     c***                                         w98nri>0 and wpodcm<wpocri
481500080116     c***                z-sub     w98nri        w98nri
481600080116     c                   z-add     0             w98nri
481700061121     c                   endif
481800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
481900070827     c*  mittente, --> consegnato in anticipo
482000070828     c                   if        d98tcr=' ' and d98nsp>0  and
482100070828     c                                            w98nri>0 and wpodcm<wpocri
482200070827     c                   if        CallAR5=' '
482300070827     c                   EXSR      LeggiAR5
482400070824     c                   endif
482500070827     c                   if        dcrpart<>'S'
482600070824     c                   z-sub     w98nri        w98nri
482700070824     c                   endif
482800070824     c                   endif
482900050907     c
483000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
483100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
483200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
483300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
483400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
483500050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
483600050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
483700050907     c                   z-sub     w98nri        w98nri
483800050907     c                   sub       oreILoDOPO    w98nri
483900050907     c                   endif
484000050907     c
484100050907     C                   END                                                    <-- 2 ---
484200930524     C                   END                                                    <-- 1 ---
484300930420     C* CALCOLO LE ORE DI RESA
484400930420     C* CONTROLLO CONFORMITA
484500930423     C     W98NRI        IFLE      *ZEROS
484600930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
484700930420     C                   ELSE
484800930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
484900930420     C                   END
485000930420     C*
485100930420     C*-------------------------------------------------------------------
485200930420     C* CONFORMITA' RESA CLIENTE
485300930420     C*-------------------------------------------------------------------
485400930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
485500930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
485600930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
485700050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
485800050914     c                   if        consideraora='S'
485900050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
486000050914     c                   else
486100050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
486200050914     c                   endif
486300090827     c
486400080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
486500090827     c                   select
486600090827     c                   when      d98cca='2' and w98nrc>0
486700080116     c                   z-add     0             w98nrc
486800090827
486900080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
487000080116     c*  e sto leggendo la bolla in arrivo
487100090827     c                   when      d98cca='1' and d98tbo='A' and
487200080116     c                                            w98nrc>0
487300080116     c                   z-add     0             w98nrc
487400110923     c
487500110923     c* Ignoro ritardo se Perfect Order
487600130221     c                   when      d98xco >*zeros
487700110923     c                   z-add     0             w98nrc
487800090827     c
487900090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
488000090827     c* sono  in ritardo di 12 ore
488100091009     c                   when      d98tcc=10  and
488200091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
488300091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
488400091014     c                             d98hce=1030
488500091014     C                   MOVEL     'OSC  '       WR3TAB
488600091014     C                   eval      WR3HHI=d98hmc
488700091014     C                   EXSR      RO3ORE
488800091014     c                   if        wr3matpom='M' and d98hmc>1030
488900131218     c                   exsr      ctrLAVH1030
489000131218
489100131218     c                   if        wlav1030<>'S'
489200091014     c                   add       12            w98nrc
489300091014     c                   endif
489400131218     c                   endif
489500091014     c
489600091014     c**                 z-add     12            w98nrc
489700090827     c                   endsl
489800090827     c
489900930524     C                   ELSE                                                   <-- 1 -->
490000930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
490100930524     C* DI DIFFERENZA SONO UGUALI A ZERO
490200930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
490300931206     C     WPODCM        ANDLE     WPOCR2
490400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
490500930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
490600930525     C                   Z-ADD     WPODCM        WR2POI            9 0
490700931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
490800930525     C                   EXSR      RO2COV
490900930525     C     WR2NOV        IFNE      *ZEROS
491000930525     C     WR2NOV        MULT      -1            W98NRC
491100930525     C                   END
491200930524     C                   ELSE                                                   <-- 2 -->
491300930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
491400930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
491500930524     C     WPODCM        IFLT      WPOCRI
491600930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
491700930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
491800930524     C                   ELSE
491900931206     C                   Z-ADD     WPOCR2        WPORFI
492000930524     C                   Z-ADD     WPODCM        WPORFF
492100930524     C                   END
492200930524     C* CALCOLO ORE DI DIFFERENZA
492300930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
492400930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
492500930524     C                   Z-ADD     WPORFI        WR2POI            9 0
492600930524     C                   Z-ADD     WPORFF        WR2POF            9 0
492700930524     C                   EXSR      RO2COV
492800930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
492900050902     c
493000050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
493100100224     c                   if        d98cca='2' and w98nrc>0
493200100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
493300080116     c***                z-sub     w98nrc        w98nrc
493400080116     c                   z-add     0             w98nrc
493500050902     c                   endif
493600111018     c* Ignoro ritardo se pefect order
493700130221     c                   if        d98xco >*zeros
493800111018     c                   z-add     0             w98nrc
493900111018     c                   endif
494000070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
494100061121     c*  e sto leggendo la bolla in arrivo
494200100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
494300100224     c***                if        d98cca='1' and d98tbo='A' and
494400100224     c***                                         w98nrc>0 and wpodcm<wpocri
494500080116     c***                z-sub     w98nrc        w98nrc
494600080116     c                   z-add     0             w98nrc
494700061121     c                   endif
494800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
494900070827     c*  mittente, --> consegnato in anticipo
495000070828     c                   if        d98tcr=' ' and d98nsp>0 and
495100070828     c                                            w98nrc>0 and wpodcm<wpocri
495200070827     c                   if        CallAR5=' '
495300070827     c                   EXSR      LeggiAR5
495400070827     c                   endif
495500070827     c                   if        dcrpart<>'S'
495600070824     c                   z-sub     w98nrc        w98nrc
495700070824     c                   endif
495800070824     c                   endif
495900050907     c
496000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
496100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
496200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
496300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
496400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
496500050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
496600050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
496700050907     c                   z-sub     w98nrc        w98nrc
496800050907     c                   sub       oreILoDOPO    w98nrc
496900050907     c                   endif
497000050907     c
497100050907     C                   END                                                    <-- 2 ---
497200930524     C                   END                                                    <-- 1 ---
497300930420     C* CALCOLO LE ORE DI RESA
497400930420     C* CONTROLLO CONFORMITA
497500930423     C     W98NRC        IFLE      *ZEROS
497600930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
497700930420     C                   ELSE
497800930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
497900930420     C                   END
498000930420     C*
498100930420     C                   ENDSR
498200070827     c
498300131218     c********************************************************************
498400131218     c     CtrlavH1030   BEGSR
498500131218     c********************************************************************
498600131218     c                   clear                   wlav1030          1
498700140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
498800131218     c                   z-add     1             xx                4 0
498900131218     c                   dow       evc(xx)<>*blanks
499000140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
499100140127     c                   if        evd(xx)=d98dcm
499200131218     c                   if        evo(xx)<=1030
499300131218     c                   eval      wlav1030='S'
499400131218     c                   leave
499500131218     c                   endif
499600131218     c                   endif
499700131218     c                   add       1             xx
499800131218     c                   enddo
499900131218     c                   ENDSR
500000070827     c********************************************************************
500100070827     c* Controllo record GEN di FIAR5
500200070827     c********************************************************************
500300070827     c     LeggiAR5      BEGSR
500400070827     c                   clear                   DCRPART           1
500500141104     c                   clear                   dar5gen
500600141104     c                   clear                   gc1PART           2
500700141104     c                   clear                   gc2PART           2
500800070827     c     kar5          chain     fiar501l
500900070827    1c                   if        not %found
501000070827     c                   eval      dcrpart='S'
501100141104     c                   eval      gc1part=d98gc1
501200141104     c                   eval      gc2part=d98gc2
501300070827   x1c                   else
501400070827     c* i dati devono essere quelli immessi dalla partenza
501500070827     c                   movel     ar5uni        dar5gen
501600070827     c
501700070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
501800070827     c*  ma il record GEN è stato scritto per altri motivi
501900070827    2c                   if        §ar5mdp='S'
502000070827     c                   movel     d98dcr        alfadcr           8
502100141104     c                   eval      gc1part=§ar5gc1
502200141104     c                   eval      gc2part=§ar5gc2
502300070827     c
502400070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
502500070827     c                   eval      dcrpart='S'
502600070827    3c                   endif
502700070827     c
502800070827   x2c                   else
502900070827     c                   eval      dcrpart='S'
503000141106     c                   eval      gc1part=d98gc1
503100141106     c                   eval      gc2part=d98gc2
503200070827    2c                   endif
503300070827    1c                   endif
503400070827     c
503500140402     c* Memorizzo il flag pickup 2
503600140402     c                   eval      wpick2=§AR5PK2
503700140402     c
503800150401     c*
503900150414     c**                 if        d98cas>0 or d98ffd<>' ' or
504000150414     c**                           d98tc1='A'  or d98tc2='A' or
504100150414     c**                           d98tc1='S'  or d98tc2='S'
504200160331     c                   eval      kar4trc='L'
504300150401     c     kar4          chain     fiar401l
504400160420    1c                   if        %found(fiar401l)
504500150401     c                   eval      dsbl4L=ar4not
504600160420   x1c                   else
504700160420    1c                   endif
504800160420
504900150401     c* Solo per fermo deposito verifico se inviato alert
505000150401     c                   if        d98ffd<>' '
505100150401     c                   clear                   fnlv82ds
505200150401     c                   eval      ILV82AAS=d98aas
505300150401     c                   eval      ILV82LNP=d98lnp
505400150401     c                   eval      ILV82NRS=d98nrs
505500150401     c                   eval      ILV82NSP=d98nsp
505600150401     c                   eval      ILV82TIPA='FD'
505700150401     c                   call      'FNLV82R'
505800150401     c                   parm                    fnlv82ds
505900150401     c                   endif
506000150414     c**                 endif
506100150401
506200150401     c                   eval      callar5='S'
506300160331     c
506400160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
506500160331     c                   if        %lookup(d98lnp:fildpd)>0
506600160331     c                   eval      kar4trc='I'
506700160331     c     kar4          chain     fiar401l
506800160331     c                   if        %found(fiar401l)
506900160331     c                   eval      dsbl4I=ar4not
507000160331     c                   endif
507100160401     c                   if        §b4trd='1' or §b4trd='2'
507200160401     c                   eval      wVP='S'
507300160401     c                   endif
507400160401     c
507500160331     c                   endif
507600150401
507700070827     c                   ENDSR
507800930913     C********************************************************************
507900930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
508000930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
508100930913     C*            DATA/ORA (FINE RANGE)
508200930913     C********************************************************************
508300930913     C     CALCNS        BEGSR
508400930914     C*
508500930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
508600930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
508700930914     C     WPODTI        IFEQ      *ZEROS
508800960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
508900930914     C                   ELSE
509000930914     C                   Z-ADD     WPODTI        WPOIRC
509100930914     C                   END
509200930914     C*
509300930914     C* SE NON ANCORA CONSEGNATA
509400930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
509500050922     c* in realtà nella data consegna c'e' la data fine calcolo
509600050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
509700930914     C     WPODCM        IFEQ      *ZEROS
509800050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
509900930914     C                   ELSE
510000930916     C                   Z-ADD     WPODCM        WPOFRC
510100930914     C                   END
510200930914     C*
510300930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
510400930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
510500930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
510600930914     C     WPODFC        ANDLT     WPOIRC
510700930914     C                   ELSE
510800930914     C*
510900930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
511000930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
511100930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
511200930914     C     WPODFC        ANDGT     WPOFRC
511300930914     C                   ELSE
511400930916     C*
511500930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
511600930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
511700930916     C     WPODFC        ANDGT     WPOFRC
511800930916     C                   ELSE
511900930916     C*
512000930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
512100930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
512200930916     C     WPODFC        ANDLT     WPOIRC
512300930916     C                   ELSE
512400930914     C*
512500930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
512600930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
512700930914     C     WPODIC        IFNE      *ZEROS
512800930914     C     WPODIC        ANDGT     WPOFRC
512900930914     C                   Z-ADD     WPOFRC        WPODIC
513000930914     C                   ENDIF
513100930914     C*
513200930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
513300930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
513400930914     C     WPODFC        IFGT      WPOFRC
513500930914     C                   Z-ADD     WPOFRC        WPODFC
513600930914     C                   ENDIF
513700930914     C*
513800930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
513900930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
514000930914     C     WPODIC        IFNE      *ZEROS
514100930914     C     WPODIC        ANDLT     WPOIRC
514200930914     C                   Z-ADD     WPOIRC        WPODIC
514300930914     C                   ENDIF
514400930914     C*
514500930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
514600930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
514700930914     C     WPODFC        IFLT      WPOIRC
514800930914     C                   Z-ADD     WPOIRC        WPODFC
514900930914     C                   ENDIF
515000930914     C*
515100930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
515200931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
515300930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
515400930916     C*
515500931210     C***********WPODIC    IFNE *ZEROS
515600931210     C***********WPODFC    ANDNE*ZEROS
515700931210     C*********************ADD  12        WPODFC
515800931210     C*********************ENDIF
515900930914     C     WPODIC        IFEQ      *ZEROS
516000930914     C     WPODFC        ANDNE     *ZEROS
516100931130     C                   Z-ADD     WPODFC        WPODIC
516200931210     C*********************ADD  12        WPODFC
516300930916     C                   ENDIF
516400930913     C*
516500930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
516600930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
516700930914     C* INTERNA
516800050908     c                   eval      Usacons='S'
516900930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
517000930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
517100931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
517200931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
517300940324     C                   EXSR      RO9COV
517400931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
517500930914     C* CLIENTE
517600930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
517700931210     C                   EXSR      RO9COV
517800931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
517900050908     c                   eval      Usacons=' '
518000930914     C*
518100930916     C                   ENDIF                                                  <-- 5 ---
518200930916     C                   ENDIF                                                  <-- 4 ---
518300930916     C                   ENDIF                                                  <-- 3 ---
518400930914     C                   ENDIF                                                  <-- 2 ---
518500930914     C                   ENDIF                                                  <-- 1 ---
518600930913     C*
518700930913     C                   ENDSR
518800930325     C********************************************************************
518900930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
519000930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
519100930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
519200930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
519300930325     C********************************************************************
519400930325     C     RO1CDP        BEGSR
519500930414     C*
519600931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
519700931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
519800931126     C*
519900931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
520000931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
520100150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
520200150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
520300931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
520400060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
520500930326     C*
520600930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
520700931130     C                   Z-ADD     WR1POI        WR1POO            9 0
520800930325     C*
520900930325     C* CICLO FINO A CHE:
521000930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
521100930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
521200930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
521300930406     C     WR1POO        ANDLT     WPOLIM
521400930414     C*
521500930407     C                   Z-ADD     WR1POO        WPOROU            9 0
521600930325     C                   MOVEL     'N'           WSLITT            1
521700930414     C*
521800930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
521900060420     C*
522000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
522100150605     c     wdee          andeq     ' '
522200930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
522300930414     C                   END
522400930511     C*
522500931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
522600150608     c     wdee          andeq     ' '
522700930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
522800930511     C                   END
522900930414     C*
523000930325     C     WSLITT        IFEQ      'N'
523100930423     C                   SUB       12            WR1NOI            5 0
523200930325     C                   END
523300931130     C                   ADD       12            WR1POO            9 0
523400930414     C*
523500931223     C                   ENDDO                                                  <-- 1 ---
523600931126     C*
523700931126     C                   ELSE                                                   <-- 0 -->
523800931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
523900931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
524000931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
524100960129     C     WPOLAF        IFGT      WPODCG
524200960129     C                   Z-ADD     WPOLAF        WTM090            9 0
524300960129     C                   ELSE
524400960129     C                   Z-ADD     WPODCG        WTM090
524500960129     C                   ENDIF
524600960129     C     WPOGIF        IFGT      WTM090
524700960129     C                   Z-ADD     WPOGIF        WTM090
524800960129     C                   ENDIF
524900060420     C     WPOpaif       IFGT      WTM090
525000060420     C                   Z-ADD     WPOpaif       WTM090
525100060420     C                   ENDIF
525200960129     C*
525300960129     C     12            ADD       WTM090        WR1POO
525400960129     C                   ENDIF                                                  <-- 0 ---
525500930512     C*
525600931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
525700931202     C     WR1POO        SUB       12            WR7POI
525800931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
525900931202     C                   ADD       12            WR7POO
526000931202     C     WR7POO        IFGT      WR1POO
526100931202     C                   Z-ADD     WR7POO        WR1POO
526200931202     C                   END
526300930406     C*
526400930325     C                   ENDSR
526500930414     C********************************************************************
526600930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
526700930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
526800930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
526900930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
527000930414     C********************************************************************
527100930414     C     RO2COV        BEGSR
527200091014     c                   clear                   salvaEveL
527300091014     c                   clear                   salvaEveG
527400931130     C*
527500930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
527600930414     C                   Z-ADD     *ZEROS        WR2NOV
527700930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
527800931130     C                   Z-ADD     WR2POI        WR2POW
527900930414     C*
528000930414     C* CICLO FINO A CHE:
528100930414     C*   - DATA INIZIO SUPERATO DATA FINE
528200930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
528300931223     C     WR2POW        ANDLT     WPOLIM
528400930414     C*
528500930414     C                   Z-ADD     WR2POW        WPOROU
528600930414     C                   MOVEL     'N'           WSLITT            1
528700930414     C*
528800930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
528900930414     C*
529000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
529100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
529200930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
529300930414     C                   END
529400930414     C*
529500930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
529600930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
529700930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
529800930618     C                   END
529900060420     c
530000060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
530100060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
530200060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
530300060420     C                   END
530400060420     c
530500931201     C*
530600931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
530700931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
530800931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
530900931201     C                   END
531000060418     c
531100060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
531200060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
531300060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
531400060418     C                   END
531500930414     C*
531600930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
531700930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
531800930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
531900930414     C                   END
532000930511     C*
532100930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
532200930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
532300930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
532400930511     C                   END
532500930414     C*
532600930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
532700930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
532800930414     C     WSLITT        IFEQ      'N'
532900930423     C                   ADD       12            WR2NOV            5 0
533000930414     C                   END
533100931130     C                   ADD       12            WR2POW            9 0
533200930414     C*
533300931223     C                   ENDDO                                                  <-- 1 ---
533400930414     C*
533500930414     C                   ENDSR
533600941228     C********************************************************************
533700941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
533800941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
533900941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
534000941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
534100941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
534200941228     C********************************************************************
534300941228     C     RO2COP        BEGSR
534400941228     C*
534500941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
534600941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
534700941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
534800941228     C                   Z-ADD     *ZEROS        WR2NOV
534900941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
535000941228     C                   Z-ADD     WR2POI        WR2POW
535100941228     C*
535200941228     C* CICLO FINO A CHE:
535300941228     C*   - DATA INIZIO SUPERATO DATA FINE
535400941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
535500941228     C*
535600950215     C                   Z-ADD     WR2POW        WPOROU
535700941228     C                   MOVEL     'N'           WSLITT
535800941228     C* TRASFORMA IL PROGR. ORE IN DATA
535900950215     C*******              Z-ADDWR2POW    WORE
536000950215     C*******              EXSR OREDAT
536100950215     C*******              Z-ADDWDAT      W12DAT  80
536200950215     C*******              Z-ADDWRES      W12RES  30
536300941228     C*
536400941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
536500941228     C*
536600941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
536700941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
536800941228     C     WSLITT        IFEQ      'N'
536900941228     C                   ADD       12            WR2NOV            5 0
537000941228     C                   END
537100941228     C                   ADD       12            WR2POW            9 0
537200941228     C*
537300941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
537400941228     C* ENTRI IN LOOP
537500941228     C     WR2NOV        IFGT      999
537600941228     C                   LEAVE                                                  *VA A ENDDO
537700941228     C                   END
537800941228     C*
537900941228     C                   ENDDO                                                  <-- 1 ---
538000941228     C*
538100941228     C                   ENDSR
538200930510     C********************************************************************
538300930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
538400930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
538500930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
538600930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
538700050905     c*                     il num ore deve essere un multiplo di 12
538800050905     c*                     oppure 1 ora
538900930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
539000930510     C********************************************************************
539100930510     C     RO4CDP        BEGSR
539200930510     C*
539300930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
539400930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
539500050905     c                   movel     '1'           wgiro             1
539600930510     C*
539700930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
539800930510     C                   Z-ADD     WR4POI        WR4POO            9 0
539900930510     C*
540000930510     C* CICLO FINO A CHE:
540100930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
540200930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
540300930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
540400930510     C     WR4POO        ANDLT     WPOLIM
540500930510     C*
540600141106     c                   if        nosomma<>'N'
540700931130     C                   ADD       12            WR4POO            9 0
540800141106     c                   endif
540900930510     C                   Z-ADD     WR4POO        WPOROU            9 0
541000930510     C                   MOVEL     'N'           WSLITT            1
541100930510     C*
541200930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
541300050905     c
541400050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
541500930510     C     WSLITT        IFEQ      'N'
541600050905     c
541700050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
541800050905     c*  se cado in giorno non festivo
541900050905     c                   if        wr4noi=1 and wgiro='1'
542000050905     c                   sub       12            wr4poo
542100050905     c                   add       wr4noi        wr4poo
542200050905     c                   endif
542300050905     c*
542400050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
542500050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
542600050905     c*  calcolate dal pgm
542700050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
542800050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
542900050905     c*  del giorno sucessivo non festivo
543000050905     c                   if        wr4noi=1 and wgiro<>'1'
543100050905     c                   add       wr4noi        wr4poo
543200050905     c                   endif
543300050905     c
543400930510     C                   SUB       12            WR4NOI            5 0
543500930510     C                   END
543600141106     c
543700141106     c                   if        nosomma= 'N'
543800141106     C                   ADD       12            WR4POO            9 0
543900141106     c                   endif
544000930510     C*
544100050905     c                   eval      wgiro='2'
544200931223     C                   ENDDO                                                  <-- 1 ---
544300930510     C*
544400930510     C                   ENDSR
544500931130     C********************************************************************
544600931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
544700931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
544800931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
544900931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
545000931130     C********************************************************************
545100931130     C     RO5COV        BEGSR
545200931130     C*
545300931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
545400060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
545500060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
545600060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
545700931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
545800931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
545900931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
546000931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
546100931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
546200931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
546300931130     C                   Z-ADD     *ZEROS        WR5NOV
546400940714     C*
546500940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
546600940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
546700931207     C*
546800931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
546900940908     C     WIC           IFEQ      'I'
547000090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
547100090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
547200931207     C                   ELSE
547300090827     C*****              Z-ADD     D98TCC        WPOTCT
547400090827     C                   Z-ADD     w99TCC        WPOTCT
547500931207     C                   END
547600931130     C*
547700931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
547800060420     C                   clear                   COV
547900931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
548000931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
548100931207     C                   Z-ADD     2             CO                3 0
548200931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
548300931207     C                   EXSR      RO5SUB
548400981102     C                   DO        99            IC                3 0
548500931207     C     LAF(IC)       IFNE      *ZEROS
548600931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
548700931207     C                   EXSR      RO5SUB
548800931207     C                   ENDIF
548900931207     C                   ENDDO
549000060420     c
549100060420     C                   DO        99            IC                3 0
549200060420     C     paif(IC)      IFNE      *ZEROS
549300060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
549400060420     C                   EXSR      RO5SUB
549500060420     C                   ENDIF
549600060420     C                   ENDDO
549700931207     C* ORDINA LA SCHIERA COV
549800931207     C                   SORTA     COV
549900931130     C*
550000931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
550100931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
550200981102     C                   DO        98            IC                             --- 1 -->
550300931207     C     IC            ADD       1             I1C               3 0
550400931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
550500931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
550600931207     C     COV(I1C)      OREQ      *ZEROS
550700931207     C                   ELSE
550800931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
550900931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
551000931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
551100931130     C                   EXSR      RO2COV
551200931207     C                   ADD       WR2NOV        WR5NOV            9 0
551300931209     C* SE NON SONO IN DATA CONSEGNA
551400931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
551500931207     C     WR5NOV        IFLT      WPOTCT
551600931209     C     COV(I1C)      ANDNE     WPODCM
5517009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5518009709111009 C     WPOTCT        SUB       12            WR5NOV
551900931130     C                   END
552000931207     C                   ENDIF                                                  <-- 2 ---
552100931207     C                   ENDDO                                                  <-- 1 ---
552200940714     C                   ENDIF                                                  <-- 0 ---
552300931130     C*
552400931130     C                   ENDSR
552500931207     C********************************************************************
552600931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
552700931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
552800931207     C********************************************************************
552900931207     C     RO5SUB        BEGSR
553000931207     C*
553100931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
553200931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
553300931207     C     WR5COV        ANDLT     WR5POF
553400931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
553500931207     C                   MOVEL     'N'           WR5PRE            1
553600981102     C*---                 DO   30        I
553700981102     C                   DO        99            I
553800931207     C     WR5COV        IFEQ      COV(I)
553900931207     C                   MOVEL     'S'           WR5PRE
554000931207     C                   LEAVE
554100931207     C                   ENDIF
554200931207     C                   ENDDO
554300931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
554400931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
554500931207     C                   ADD       1             CO                3 0
554600931207     C                   Z-ADD     WR5COV        COV(CO)
554700931207     C                   ENDIF                                                  <-- 2 ---
554800931207     C                   ENDIF                                                  <-- 1 ---
554900931207     C*
555000931207     C                   ENDSR
555100931201     C********************************************************************
555200060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
555300931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
555400931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
555500931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
555600931201     C********************************************************************
555700931201     C     RO6CDT        BEGSR
555800931201     C*
555900931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
556000931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
556100931201     C*
556200931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
556300931201     C                   Z-ADD     WR6POI        WR6POO            9 0
556400931201     C*
556500931201     C* CICLO FINO A CHE:
556600931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
556700931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
556800931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
556900931201     C     WR6POO        ANDLT     WPOLIM
557000931201     C*
557100931201     C                   Z-ADD     WR6POO        WPOROU            9 0
557200931201     C                   MOVEL     'N'           WSLITT            1
557300931201     C*
557400931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
557500931201     C*
557600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
557700150605     c     wdee          andeq     ' '
557800931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
557900931201     C                   END
558000931201     C*
558100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
558200150608     c     wdee          andeq     ' '
558300931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
558400931201     C                   END
558500931201     C*
558600931201     C     WSLITT        IFEQ      'N'
558700931201     C                   SUB       12            WR6NOI            5 0
558800931201     C                   END
558900931201     C                   ADD       12            WR6POO            9 0
559000931201     C*
559100931223     C                   ENDDO                                                  <-- 1 ---
559200931201     C*
559300931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
559400931202     C     WR6POO        SUB       12            WR7POI
559500931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
559600931207     C                   ADD       12            WR7POO
559700931202     C     WR7POO        IFGT      WR6POO
559800931201     C                   Z-ADD     WR7POO        WR6POO
559900931202     C                   END
560000931201     C*
560100931201     C                   ENDSR
560200931201     C********************************************************************
560300060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
560400060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
560500060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
560600060407     C*            INPUT:  -WR7POI (PROGR.ORE)
560700931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
560800931201     C********************************************************************
560900931201     C     RO7TCF        BEGSR
561000060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
561100060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
561200060407     C                   MOVEL     'N'           WSLITT
561300931201     C*
561400931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
561500931201     C                   Z-ADD     WR7POI        WR7POO            9 0
561600931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
561700931201     C* COINCIDA CON UN TURNO DI CHIUSURA
561800060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
561900931201     C     D98GC2        ANDEQ     *BLANKS
562000060407   x1C                   ELSE                                                   <-- 1 -->
562100931214     C*
562200931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
562300931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
562400931214     C*
562500931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
562600931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
562700931223     C     WPOROU        ANDLT     WPOLIM
562800931214     C* MEMORIZZO INIZIO E FINE RANGE
562900931202     C     WPOTI         IFEQ      *ZEROS
563000931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
563100931202     C                   END
563200931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
563300931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
563400931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
563500931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
563600931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
563700931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
563800931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
563900931201     C                   MOVEL     'N'           WSLITT
564000931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
564100060407     c* Se non trovat, controllo se c'e' evento part che giustifica
564200060407     c                   if        wslitt='N'
564300060407     c                   exsr      datcepg
564400060407     c                   endif
564500060407     c
564600931201     C                   ENDDO                                                  <-- 2 ---
564700931214     C*
564800931201     C                   Z-ADD     WPOROU        WR7POO
564900931214     C*
565000931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
565100931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
565200931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
565300931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
565400931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
565500931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
565600931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
565700931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
565800931209     C                   ELSE                                                   <-- 3 -->
565900931209     C     WIC           IFEQ      'I'
566000931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
566100931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
566200931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
566300931201     C                   ELSE
566400931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
566500931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
566600931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
566700931201     C                   ENDIF
566800931209     C                   ENDIF                                                  <-- 3 ---
566900931209     C                   ENDIF                                                  <-- 2 ---
567000931214     C*
567100060407    1C                   ENDIF                                                  <-- 1 ---
567200060407     c
567300060407     c* Se non trovato turno di chiusura, verifico se c'e'
567400060407     c*  evento particolare che GIUSTIFICA
567500060407     c                   if        wpoti=0
567600060418     C                   EXSR      RO7CEPG
567700060407     c                   endif
567800931201     C*
567900931201     C                   ENDSR
568000060406     C********************************************************************
568100060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
568200060406     c*                        che giustifica
568300060407     C*            INPUT:-WR7POI (PROGR.ORE)
568400060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
568500060406     C********************************************************************
568600060407     C     RO7CEPG       BEGSR
568700060406     C*
568800060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
568900060407     C                   Z-ADD     WR7POI        WR7POO            9 0
569000060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
569100060406     C* coincide
569200060406    1C                   if        wevpag='S'                                   <-- 1 -->
569300060406     C*
569400060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
569500060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
569600060406     C                   MOVEL     'N'           WSLITT
569700060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
569800060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
569900060406     C*
570000060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
570100060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
570200060406     C     WPOROU        ANDLT     WPOLIM
570300060406     C* MEMORIZZO INIZIO E FINE RANGE
570400060406    3C     WPOTI         IFEQ      *ZEROS
570500060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
570600060406    3C                   END
570700060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
570800060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
570900060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
571000060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
571100060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
571200060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
571300060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
571400060406     C                   MOVEL     'N'           WSLITT
571500060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
571600060406     C* Se non trovato,
571700060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
571800060406    3c                   if        wslitt='N'
571900060406     c                   exsr      DATTUC
572000060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
572100060406    2C                   ENDDO                                                  <-- 2 ---
572200060406     C*
572300060407     C                   Z-ADD     WPOROU        WR7POO
572400060406     C*
572500060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
572600060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
572700060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
572800060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
572900060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
573000060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
573100060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
573200060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
573300060406   x3C                   ELSE                                                   <-- 3 -->
573400060406    4C     WIC           IFEQ      'I'
573500060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
573600060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
573700060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
573800060406   x4C                   ELSE
573900060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
574000060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
574100060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
574200060406    4C                   ENDIF
574300060406    3C                   ENDIF                                                  <-- 3 ---
574400060406    2C                   ENDIF                                                  <-- 2 ---
574500060406     C*
574600060406    1C                   ENDIF                                                  <-- 1 ---
574700060406     C*
574800060406     C                   ENDSR
574900931207     C********************************************************************
575000931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
575100931207     C*            INPUT:  -WR8POI (PROGR.ORE)
575200931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
575300931207     C********************************************************************
575400931207     C     RO8FES        BEGSR
575500130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
575600130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
575700931207     C*
575800931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
575900130411     C     WR8POI        SUB       12            WR8POO            9 0
576000931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
576100931207     C                   MOVEL     'N'           WSLITT
576200130411     c
576300130411     c* Non salto i festivi ???
576400130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
576500130411     c
576600931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
576700130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
576800130411     C     WPOROU        ANDGT     WPODSP
576900130411     C                   SUB       12            WPOROU
577000130411     C                   MOVEL     'N'           WSLITT
577100130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
577200130411     C                   ENDDO                                                  <-- 2 ---
577300130411     c
577400130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
577500130411     C     D98GC2        ANDEQ     *BLANKS
577600130411   x1C                   ELSE                                                   <-- 1 -->
577700130411     c* eseguo controllo per i turni di chiusura
577800130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
577900130411     C*
578000130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
578100130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
578200130411     C     WPOROU        ANDgT     WPOdsp
578300130411     C* MEMORIZZO INIZIO E FINE RANGE
578400130411     C     WPOTf         IFEQ      *ZEROS
578500130411     C                   z-add     wporou        WPOTf
578600130411     C                   END
578700140826     C     WPOROU        SUB       wnoadd        WPOROU
578800130411     c
578900130411     C                   MOVEL     'N'           WSLITT
579000130411     c                   exsr      dattuc
579100130411     c                   if        wslitt='N'
579200130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
579300140826     c* Se slitta per festivo --> tolgo sempre 12 ore
579400140826     c                   z-add     12            WNOADD
579500130411     c                   endif
579600130411     C* SLITTA
579700140826     C     wporou        add       12            WPOTi
579800130411    2C                   ENDDO                                                  <-- 2 ---
579900130411     c
580000130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
580100130411     c
580200130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
580300130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
580400130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
580500130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
580600130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
580700130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
580800130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
580900130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
581000130411   x3C                   ELSE                                                   <-- 3 -->
581100130411    4C     WIC           IFEQ      'I'
581200130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
581300130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
581400130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
581500130411   x4C                   ELSE
581600130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
581700130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
581800130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
581900130411    4C                   ENDIF
582000130411    3C                   ENDIF                                                  <-- 3 ---
582100130411    2C                   ENDIF                                                  <-- 3 ---
582200130411     C*
582300130411    1C                   ENDIF                                                  <-- 1 ---
582400130411
582500130411     C**   WPOROU        ADD       12            WR8POO
582600130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
582700130411     c                   z-add     wr8poi        wr8poo
582800931207     C*
582900931207     C                   ENDSR
583000931210     C********************************************************************
583100931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
583200940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
583300931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
583400931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
583500931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
583600931210     C********************************************************************
583700931210     C     RO9COV        BEGSR
583800931210     C*
583900931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
584000931210     C                   Z-ADD     *ZEROS        WR9NOV
584100931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
584200931210     C                   Z-ADD     WR9POI        WR9POW
584300931210     C* CICLO FINO A CHE:
584400931210     C*   - DATA INIZIO SUPERATO DATA FINE
584500931223     C*   - RAGGIUNTO LIMITE MASSIMO
584600931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
584700931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
584800931223     C     WR9POW        ANDLT     WPOLIM
584900931210     C*
585000931210     C                   Z-ADD     WR9POW        WPOROU
585100931210     C                   MOVEL     'N'           WSLITT            1
585200940324     C* FESTIVI
585300931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
585400940324     C* GIACENZA
585500931214     C     WSLITT        IFEQ      'N'
585600941229£$   C     WPOAGC        IFNE      *ZEROS
585700940324£$   C     WPOCGC        ORNE      *ZEROS
585800941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
585900940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
586000940324£$   C                   MOVEL     'S'           WSLITT
586100940324£$   C                   END
586200940324£$   C                   END
586300940324£$   C                   END
586400960129     C* EVENTI GIACENZA
586500960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
586600960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
586700960129     C                   Z-ADD     *ZEROS        G                 3 0
586800960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
586900960129     C     G             ANDLT     IG
587000960129     C                   ADD       1             G
587100960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
587200960209     C     WPOROU        ANDLE     GCF(G)
587300960129     C                   MOVEL     'S'           WSLITT
587400960129     C                   ENDIF                                                  <-- 3 ---
587500960129     C                   ENDDO                                                  <-- 2 ---
587600960129     C                   ENDIF                                                  <-- 1 ---
587700960129     C                   ENDIF                                                  <-- 0 ---
587800940324     C* LASCIATO AVVISO
587900931214     C     WSLITT        IFEQ      'N'
588000941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
588100940324£$   C                   Z-ADD     *ZEROS        L                 3 0
588200940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
588300940324£$   C     L             ANDLT     IL
588400940324£$   C                   ADD       1             L
588500941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
588600941229£$   C     WPOROU        ANDLE     LCF(L)
588700050520     c     LCon(L)       andeq     'S'
588800940324£$   C                   MOVEL     'S'           WSLITT
588900940324£$   C                   END                                                    <-- 3 ---
589000940324£$   C                   ENDDO                                                  <-- 2 ---
589100940324£$   C                   END                                                    <-- 1 ---
589200931210     C                   END
589300940324     C* TURNO CHIUSURA
589400140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
589500140826     c*  di sistemare le ore da sommare 12 o 24...
589600931214     C     WSLITT        IFEQ      'N'
589700931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
589800931210     C                   END
589900940324     C* DATA CONSEGNA RICHIESTA
590000050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
590100050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
590200050907     c
590300931214     C     WSLITT        IFEQ      'N'
590400050907     c                   add       1             wporou
590500940324£$   C     WPOCRI        IFNE      *ZEROS
590600940324£$   C     WPOROU        IFLT      WPOCRC
590700940324£$   C                   MOVEL     'S'           WSLITT
590800940324£$   C                   END
590900940324£4   C                   END
591000050907     c                   sub       1             wporou
591100931210     C                   END
591200931210     C*
591300931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
591400931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
591500931210     C     WSLITT        IFEQ      'N'
591600931210     C                   ADD       12            WR9NOV            5 0
591700931210     C                   END
591800931210     C                   ADD       12            WR9POW            9 0
591900931210     C*
592000931223     C                   ENDDO                                                  <-- 1 ---
592100931210     C*
592200931210     C                   ENDSR
592300940628%%%  C********************************************************************
592400940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
592500940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
592600940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
592700940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
592800940628%%%  C********************************************************************
592900940628%%%  C     RO0GES        BEGSR
593000940628%%%  C*
593100940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
593200940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
593300940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
593400940628%%%  C*
593500940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
593600940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
593700940628%%%  C*
593800940628%%%  C* CICLO FINO A CHE:
593900940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
594000940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
594100940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
594200050922%%%  C     WR0POO        ANDGe     WPODSP
594300940628%%%  C*
594400940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
594500940628%%%  C                   MOVEL     'N'           WSLITT            1
594600940628%%%  C*
594700940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
594800940628%%%  C*
594900940628%%%  C     WSLITT        IFEQ      'N'
595000940628%%%  C                   SUB       12            WR0NOI            5 0
595100940628%%%  C                   END
595200940628%%%  C                   SUB       12            WR0POO            9 0
595300940628%%%  C*
595400940628%%%  C                   ENDDO                                                  <-- 1 ---
595500940628%%%  C*
595600940628%%%  C                   ADD       12            WR0POO            9 0
595700940628%%%  C*
595800940628%%%  C                   ENDSR
595900960911     C********************************************************************
596000960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
596100960911     C*            INPUT:  -W10POI (PROGR.ORE)
596200960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
596300960911     C********************************************************************
596400960911     C     R10FES        BEGSR
596500960911     C*
596600960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
596700960911     C                   Z-ADD     W10POI        W10POO            9 0
596800960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
596900960911     C                   MOVEL     'N'           WSLITT
597000960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
597100960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
597200960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
597300960911     C                   ADD       12            WPOROU
597400960911     C                   MOVEL     'N'           WSLITT
597500960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
597600960911     C                   ENDDO                                                  <-- 2 ---
597700960911     C                   Z-ADD     WPOROU        W10POO
597800960911     C*
597900960911     C                   ENDSR
598000930325     C********************************************************************
598100930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
598200930325     C********************************************************************
598300930325     C     CHKGIA        BEGSR
598400930406     C*
598500960129     C* APERTURA/CHIUSURA GIACENZA
598600930326     C     WPODAG        IFNE      *ZEROS
598700930406     C     WPODCG        ORNE      *ZEROS
598800930407     C     WPOROU        IFGE      WPODAG
598900930407     C     WPOROU        ANDLE     WPODCG
599000930325     C                   MOVEL     'S'           WSLITT
599100930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
599200930325     C                   END
599300930326     C                   END
599400960129     C*
599500960129     C* EVENTI GIACENZA
599600960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
599700960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
599800960129     C                   Z-ADD     *ZEROS        G                 3 0
599900960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
600000960129     C     G             ANDLT     IG
600100960129     C                   ADD       1             G
600200960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
600300960129     C     WPOROU        ANDLE     GIF(G)
600400091009     c
600500091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
600600091009     c*  l'orario dell'evento
600700091009     c                   select
600800091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
600900091009     c* Se orario entro le 10.30  va bene
601000091014     c*  solo la prima volta per evento cioè quando controllo
601100091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
601200091014     c*  e non devo dare errore
601300091014     C                   MOVEL     'OSC  '       WR3TAB
601400091014     C                   eval      WR3HHI=evo(gip(G))
601500091014     C                   EXSR      RO3ORE
601600091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
601700091014     c*  in ritardo come per "E")
601800091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
601900091014     c                              wr3matpom='P'
602000091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
602100091009     C                   MOVEL     'S'           WSLITT
602200091009     c                   endif
602300091014     c                   eval      SalvaEveG=gip(G)
602400091009     c
602500091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
602600091009     c* Se orario entro le 10.30  va bene
602700091014     c*  solo la prima volta per evento cioè quando controllo
602800091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
602900091014     c*  e non devo dare errore
603000091014     C                   MOVEL     'OSC  '       WR3TAB
603100091014     C                   eval      WR3HHI=evo(gip(G))
603200091014     C                   EXSR      RO3ORE
603300091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
603400091014     c*  in ritardo come per "E")
603500091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
603600091014     c                              wr3matpom='P'
603700091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
603800091009     C                   MOVEL     'S'           WSLITT
603900091009     c                   endif
604000091014     c                   eval      SalvaEveG=gip(G)
604100091009     c
604200091009     c                   other
604300960129     C                   MOVEL     'S'           WSLITT
604400960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
604500091009     c                   endsl
604600091009
604700960129     C                   ENDIF                                                  <-- 3 ---
604800960129     C                   ENDDO                                                  <-- 2 ---
604900960129     C                   ENDIF                                                  <-- 1 ---
605000960129     C                   ENDIF                                                  <-- 0 ---
605100930406     C*
605200930325     C                   ENDSR
605300060420     C********************************************************************
605400930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
605500930325     C********************************************************************
605600930325     C     CHKLAV        BEGSR
605700930325     C*
605800941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
605900930421     C                   Z-ADD     *ZEROS        L                 3 0
606000930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
606100930421     C     L             ANDLT     IL
606200930421     C                   ADD       1             L
606300930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
606400930421     C     WPOROU        ANDLE     LAF(L)
606500090928     c
606600090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
606700091009     c*  l'orario dell'evento
606800091009     c                   select
606900091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
607000091009     c* Se orario entro le 10.30  va bene
607100091014     c*  solo la prima volta per evento cioè quando controllo
607200091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
607300091014     c*  e non devo dare errore
607400091014     C                   MOVEL     'OSC  '       WR3TAB
607500091014     C                   eval      WR3HHI=evo(lap(L))
607600091014     C                   EXSR      RO3ORE
607700091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
607800091014     c*  in ritardo come per "E")
607900091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
608000091014     c                              wr3matpom='P'
608100091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
608200091009     C                   MOVEL     'S'           WSLITT
608300091009     c                   endif
608400091014     c                   eval      SalvaEveL=lap(L)
608500091009     c
608600091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
608700091009     c* Se orario entro le 10.30  va bene
608800091014     c*  solo la prima volta per evento cioè quando controllo
608900091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
609000091014     c*  e non devo dare errore
609100091014     C                   MOVEL     'OSC  '       WR3TAB
609200091014     C                   eval      WR3HHI=evo(lap(L))
609300091014     C                   EXSR      RO3ORE
609400091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
609500091014     c*  in ritardo come per "E")
609600091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
609700091014     c                              wr3matpom='P'
609800091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
609900091009     C                   MOVEL     'S'           WSLITT
610000091009     c                   endif
610100091014     c                   eval      SalvaEveL=lap(L)
610200090928     c
610300091009     c                   other
610400930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
610500091009     C                   MOVEL     'S'           WSLITT
610600091009     c                   endsl
610700090928     c
610800930421     C                   END                                                    <-- 3 ---
610900931223     C                   ENDDO                                                  <-- 2 ---
611000930421     C                   END                                                    <-- 1 ---
611100930325     C*
611200930325     C                   ENDSR
611300060420     C********************************************************************
611400060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
611500060420     c*                   che Allungano
611600060420     C********************************************************************
611700060420     C     CHKCEPA       BEGSR
611800060420     C*
611900060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
612000060420     C                   Z-ADD     *ZEROS        p                 3 0
612100060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
612200060420     C     p             ANDLT     IpA
612300060420     C                   ADD       1             P
612400060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
612500060420     C     WPOROU        ANDLE     paif(P)
612600060420     C                   MOVEL     'S'           WSLITT
612700060420     C                   ADD       12            W98EA
612800060420     C                   END                                                    <-- 3 ---
612900060420     C                   ENDDO                                                  <-- 2 ---
613000060420     C                   END                                                    <-- 1 ---
613100060420     C*
613200060420     C                   ENDSR
613300930414     C********************************************************************
613400930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
613500930414     C********************************************************************
613600930414     C     CHKCNT        BEGSR
613700930511     C*
613800930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
613900930611     C     WIC           IFEQ      'I'                                          *INTERNO
614000930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
614100930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
614200930611     C                   ELSE
614300930611     C     WPOROU        IFGE      WPOAII
614400930611     C     WPOROU        ANDLE     WPOAFI
614500930611     C                   MOVEL     'S'           WSLITT
614600930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
614700930611     C                   END
614800930611     C                   END
614900930611     C                   ELSE                                                   *CLIENTE
615000930611     C     WPOAIC        IFEQ      *ZEROS
615100930611     C     WPOAFC        ANDEQ     *ZEROS
615200930611     C                   ELSE
615300930611     C     WPOROU        IFGE      WPOAIC
615400930611     C     WPOROU        ANDLE     WPOAFC
615500930611     C                   MOVEL     'S'           WSLITT
615600930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
615700930611     C                   END
615800930611     C                   END
615900930611     C                   END
616000930414     C*
616100930414     C                   ENDSR
616200930511     C********************************************************************
616300930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
616400930511     C********************************************************************
616500930511     C     CHKFED        BEGSR
616600930511     C*
616700930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
616800930611     C     WIC           IFEQ      'I'                                          *INTERNO
616900930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
617000930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
617100930611     C                   ELSE
617200930611     C     WPOROU        IFGE      WPOFII
617300930611     C     WPOROU        ANDLE     WPOFFI
617400930611     C                   MOVEL     'S'           WSLITT
617500930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
617600930611     C                   END
617700930611     C                   END
617800930611     C                   ELSE                                                   *CLIENTE
617900930611     C     WPOFIC        IFEQ      *ZEROS
618000930611     C     WPOFFC        ANDEQ     *ZEROS
618100930611     C                   ELSE
618200930611     C     WPOROU        IFGE      WPOFIC
618300930611     C     WPOROU        ANDLE     WPOFFC
618400930611     C                   MOVEL     'S'           WSLITT
618500930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
618600930611     C                   END
618700930611     C                   END
618800930611     C                   END
618900930511     C*
619000930511     C                   ENDSR
619100930325     C********************************************************************
619200930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
619300930325     C********************************************************************
619400930325     C     CHKFES        BEGSR
619500060526     c* se già passata la data, non controllo wporou
619600060526     c                   if        wpodata>0
619700060526     c                   z-add     wpodata       w12dat
619800060526     c                   clear                   w12res
619900060526     c                   else
620000931130     C*
620100931130     C* TRASFORMA IL  PROGR.ORE IN DATA
620200931130     C                   Z-ADD     WPOROU        WORE
620300931130     C                   EXSR      OREDAT
620400941227     C                   Z-ADD     WDAT          W12DAT            8 0
620500931130     C                   Z-ADD     WRES          W12RES            3 0
620600060526     c                   endif
620700930325     C*
620800941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
620900930325     C* E' FESTIVO
621000090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
621100090108     C**                 Z-ADD     D98LNP        CLNTFP
621200090108     C**                 Z-ADD     *ZEROS        CLNTFA
621300090108     C**                 ENDIF
621400090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
621500090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
621600090108     c**   è possibile rendere festivo in terminal per un altro terminal
621700090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
621800930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
621900090108     C     WTC           oreq      'P'                                          *TRAZIONE
622000941227     C                   Z-ADD     D98TFP        CLNTFP
622100941227     C                   Z-ADD     D98TFA        CLNTFA
622200941228     C                   ENDIF
622300941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
622400941227     C                   Z-ADD     *ZEROS        CLNTFP
622500941227     C                   Z-ADD     D98LNA        CLNTFA
622600941228     C                   ENDIF
622700941227     C                   Z-ADD     W12DAT        DSDAT8
622800941227     C                   Z-ADD     DSAAS         CLNANN
622900941227     C                   Z-ADD     DSMES         CLNMES
623000930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
623100930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
623200930421     C                   Z-ADD     DSGIO         G
623300930415     C     W12RES        IFEQ      *ZEROS
623400941227     C                   MOVEL     CLNMAT        GFEDS
623500930415     C                   ELSE
623600941227     C                   MOVEL     CLNPOM        GFEDS
623700930415     C                   END
623800930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
623900930405     C                   MOVEL     *BLANKS       WC1Y
624000930421     C                   MOVEL     GFE(G)        WC1Y              8
624100930421     C                   Z-ADD     1             I
624200930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
624300930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
624400930405     C     WIC           IFEQ      'I'
624500930421     C     I1Y(I)        ANDEQ     'S'
624600930405     C                   MOVEL     'S'           WSLITT
624700930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
624800930405     C                   END
624900930405     C     WIC           IFEQ      'C'
625000930421     C     E1Y(I)        ANDEQ     'S'
625100930405     C                   MOVEL     'S'           WSLITT
625200930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
625300930405     C                   END
625400970204     C     WIC           IFEQ      'X'
625500970204     C     E1Y(I)        ANDEQ     'S'
625600970204     C                   MOVEL     'S'           WSLITT
625700970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
625800970204     C                   END
625900930405     C                   ELSE                                                   <-- 3 -->
626000930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
626100930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
626200930405     C                   END                                                    <-- 3 ---
626300930405     C                   END                                                    <-- 2 ---
626400930405     C                   END                                                    <-- 1 ---
626500930325     C*
626600060526     c                   clear                   wpodata
626700930325     C                   ENDSR
626800931201     C********************************************************************
626900931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
627000931201     C********************************************************************
627100931201     C     CHKTUC        BEGSR
627200931201     C*
627300931201     C     WIC           IFEQ      'I'                                          --- 1 -->
627400931201     C                   DO        TI            T                 3 0          --- 2 -->
627500931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
627600931201     C     WPOROU        ANDLE     TIF(T)
627700931201     C                   MOVEL     'S'           WSLITT
627800931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
627900931201     C                   LEAVE                                                  *VA A FINE DO
628000931201     C                   ENDIF                                                  <-- 3 ---
628100931201     C                   ENDDO                                                  <-- 2 ---
628200931201     C                   ELSE                                                   <-- 1 -->
628300931201     C                   DO        TC            T                 3 0          --- 2 -->
628400931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
628500931201     C     WPOROU        ANDLE     TCF(T)
628600931201     C                   MOVEL     'S'           WSLITT
628700931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
628800931201     C                   LEAVE                                                  *VA A FINE DO
628900931201     C                   ENDIF                                                  <-- 3 ---
629000931201     C                   ENDDO                                                  <-- 2 ---
629100931201     C                   ENDIF                                                  <-- 1 ---
629200931201     C*
629300931201     C                   ENDSR
629400060418     C********************************************************************
629500060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
629600060418     c*          - eventi particolari che giustificano
629700060418     C********************************************************************
629800060418     C     CHKCEPG       BEGSR
629900060418     C*
630000060418     C     WIC           IFEQ      'I'                                          --- 1 -->
630100060420     C                   DO        PGI           p                 3 0          --- 2 -->
630200060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
630300060420     C     WPOROU        ANDLE     PGIF(p)
630400060418     C                   MOVEL     'S'           WSLITT
630500060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
630600060418     C                   LEAVE                                                  *VA A FINE DO
630700060418     C                   ENDIF                                                  <-- 3 ---
630800060418     C                   ENDDO                                                  <-- 2 ---
630900060418     C                   ELSE                                                   <-- 1 -->
631000060420     C                   DO        PGC           p                 3 0          --- 2 -->
631100060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
631200060420     C     WPOROU        ANDLE     PGCF(p)
631300060418     C                   MOVEL     'S'           WSLITT
631400060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
631500060418     C                   LEAVE                                                  *VA A FINE DO
631600060418     C                   ENDIF                                                  <-- 3 ---
631700060418     C                   ENDDO                                                  <-- 2 ---
631800060418     C                   ENDIF                                                  <-- 1 ---
631900060418     C*
632000060418     C                   ENDSR
632100930512     C********************************************************************
632200931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
632300930512     C********************************************************************
632400931130     C     DATTUC        BEGSR
632500931214     C*
632600931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
632700931214     C                   Z-ADD     12            WNOADD            9 0
632800930325     C*
632900931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
633000931125     C     D98GC2        ORNE      *BLANKS
633100931130     C*
633200931213     C                   Z-ADD     WPOROU        WORE
633300931130     C                   EXSR      OREDAT                                       *ORE->DATA
633400931130     C                   Z-ADD     WDAT          W12DAT
633500931130     C                   Z-ADD     WRES          W12RES
633600930405     C*
633700941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
633800930405     C* DELLA SETTIMANA
633900941227     C                   Z-ADD     W12DAT        DSDAT8
634000941227     C                   Z-ADD     DSAAS         CLNANN
634100941227     C                   Z-ADD     DSMES         CLNMES
634200941227     C                   Z-ADD     *ZEROS        CLNTFP
634300941227     C                   Z-ADD     *ZEROS        CLNTFA
634400930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
634500930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
634600941227     C                   MOVEL     CLNMAT        GSEDS
634700960129     C                   Z-ADD     DSGIO         GI                6 0
634800960129     C                   MOVEL     GSE(GI)       WGSE              1
634900140911     c
635000140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
635100140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
635200140911     c                   if        wtuctut='S'
635300140911     c                   eval      wtuctut='P'
635400140911     c                   endif
635500930326     C*
635600931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
635700931213     C                   DO        4             IT                3 0          --- 3 -->
635800931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
635900931213     C                   MOVEL     TUC(IT)       W93GSE            1
636000931213     C                   MOVE      TUC(IT)       W93GMP            1
636100930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
636200930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
636300931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
636400930512     C     W93GSE        OREQ      ' '
636500140911     c     WTUCTUT       andeq     ' '
636600140911     c
636700930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
636800931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
636900930401     C     W93GMP        OREQ      ' '
637000930325     C                   MOVEL     'S'           WSLITT
637100931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
637200931213     C                   ELSE                                                   <-- 6 -->
637300001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
637400001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
637500001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
637600050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
637700050908     C     Usacons       IFNE      'S'                                          --- 7 -->
637800141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
637900141107     c*    orari giorno per giorno
638000141107     c                   if        OOR2FCSGL<>' '
638100141108     c                             and w12dat<>sav_12dat
638200141107     c                   eval      or_datarif=w12dat
638300141107     c                   eval      or_DDC    =w12dat
638400141107     c                   exsr      Reper_tempi
638500141108     c                   eval      sav_12dat=w12dat
638600141107     c                   endif
638700141107     c
638800930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
638900930325     C     W93GMP        IFEQ      'M'
639000930326     C     W12RES        ANDEQ     *ZEROS
639100141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
639200141111     c                             or OOR2STfS =0
639300141105     c                             or (tuc(IT)<>gc1part and
639400141105     c                              tuc(IT)<>gc2part)
639500140825     C                   MOVEL     'S'           WSLITT
639600140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
639700140911     c* slitto solo una volta  se chiuso tutti " M"
639800140911     c                   if        W93GSE=' '
639900140911     c                   eval      wTUCTUT='S'
640000140911     c                   endif
640100140911     c
640200140825     c                   endif
640300930325     C                   END
640400930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
640500930325     C     W93GMP        IFEQ      'P'
640600930326     C     W12RES        ANDNE     *ZEROS
640700140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
640800140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
640900141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
641000141111     c                             or OOR2STiS =0
641100141105     c                             or (tuc(IT)<>gc1part and
641200141105     c                              tuc(IT)<>gc2part)
641300930325     C                   MOVEL     'S'           WSLITT
641400140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
641500140911     c* slitto solo una volta  se chiuso tutti " P"
641600140911     c                   if        W93GSE=' '
641700140911     c                   eval      wTUCTUT='S'
641800140911     c                   endif
641900140825     c                   endif
642000140825     c
642100931213     C                   ENDIF
642200001128     C                   ENDIF                                                  <-- 7 ---
642300931213     C                   ENDIF                                                  <-- 6 ---
642400931213     C                   ENDIF                                                  <-- 5 ---
642500931213     C                   ENDIF                                                  <-- 4 ---
642600931213     C                   ENDDO                                                  <-- 3 ---
642700930405     C*
642800931213     C                   ENDIF                                                  <-- 2 ---
642900931213     C                   ENDIF                                                  <-- 1 ---
643000140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
643100140911     c                   if        wtuctut='P'  and wslitt='N'
643200140911     c                   clear                   wtuctut
643300140911     c                   endif
643400140911     c
643500930325     C                   ENDSR
643600060406     C********************************************************************
643700060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
643800060406     C********************************************************************
643900060406     C     DATCEPG       BEGSR
644000060406     C*
644100060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
644200060406     C                   Z-ADD     12            WNOADD            9 0
644300060406     C*
644400060406    1C                   if        wevpag='S'
644500060406     C*
644600060406     C                   Z-ADD     WPOROU        WORE
644700060406     C                   EXSR      OREDAT                                       *ORE->DATA
644800060406     C                   Z-ADD     WDAT          W12DAT
644900060406     C                   Z-ADD     WRES          W12RES
645000060418     c                   z-add     1             P
645100060406     c* Verifico se nella data esiste un evento Part che giustifica
645200060418     c     w12dat        lookup    pvd(P)                                 30
645300060418    2c                   dow       *in30 and pvf(P)<>'G'
645400060418     c                   add       1             P
645500060420     c     w12dat        lookup    pvd(P)                                 30
645600060406    2c                   enddo
645700060406     c
645800060406     c* 30 on --> TROVATO !!
645900060406    2c                   if        *in30
646000060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
646100060406     C                   MOVEL     'S'           WSLITT
646200060406     c                   z-add     24            wnoadd
646300060406    3C                   ENDIF
646400060406     C*
646500060406    2C                   ENDIF                                                  <-- 2 ---
646600060406    1C                   ENDIF                                                  <-- 1 ---
646700060406     C*
646800060406     C                   ENDSR
646900931210     C********************************************************************
647000931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
647100931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
647200931210     C********************************************************************
647300050805     C***  CHKDCR        BEGSR
647400931210     C*
647500050805     C***  WPOCRI        IFNE      *ZEROS
647600050805     C***  WPOROU        IFGE      WPOCRI
647700050805     C***  WPOROU        ANDLT     WPOCRF
647800050805     C***                ELSE
647900050805     C***                MOVEL     'S'           WSLITT
648000050805     C***                END
648100050805     C***                END
648200931210     C*
648300050805     C***                ENDSR
648400140822     C********************************************************************
648500140822     C*  Reperimento tempi di consegna per decidere se slittare
648600140822     C********************************************************************
648700140822     C     Reper_tempi   BEGSR
648800140822
648900140822     c                   clear                   trulorSds
649000140825     c                   clear                   trulor2ds
649100140822     c                   eval       IOREtfp = d98tfp
649200140822     c                   eval       IOREtfa = d98tfa
649300140825     c                   eval       IOREdti = d98dti
649400140825     c                   eval       IOREhti = d98hti
649500140825     c                   eval       IOREdcr = s98dcr
649600140825     c                   eval       IOREhcr = s98hcr
649700140825     c                   eval       IOREtcr = s98tcr
649800140825     c                   eval       IOREDEI = d98dei
649900140825     c                   eval       IOREOEI = d98oei
650000140825     c                   eval       IOREfil = d98lna
650100140825     c                   eval       IOREcap = d98cad
650200140825     c                   eval       IOREloc = d98lod
650300140825     c                   eval       IOREnar = d98nzd
650400140825     c
650500140825     c* Data di Riferimento:
650600140825     c* Se impostata la consegnabilità --> prendo questa
650700140822     c                   select
650800140825     c                   when      d98dfc>0
650900141107     c                   if        or_datarif<d98dfc and or_datarif>0
651000141107     c                   eval      ioredta=or_datarif
651100141107     c                   else
651200141107     c                   eval      ioredta=d98dfc
651300141107     c                   endif
651400140825     c* altrimenti data di consegna
651500140825     c                   when      d98dcm>0
651600141107     c                   if        or_datarif<d98dcm and or_datarif>0
651700141107     c                   eval      ioredta=or_datarif
651800141107     c                   else
651900140825     c                   eval      ioredta=d98dcm
652000141107     c                   endif
652100140825     c
652200140822     c                   other
652300140825     c* altrimenti data teorica consegna se > = oggi
652400140825     C                   time                    wrkoggiiso
652500140825     C     *iso          move      wrkoggiiso    ioredta
652600140822     c* se < di oggi, imposto oggi
652700140825     c                   if        d98dei>ioredta
652800140825     c                   eval      ioredta=d98dei
652900140822     c                   endif
653000141107     c
653100141107     c                   if        or_datarif<ioredta and or_datarif>0
653200141107     c                   eval      ioredta=or_datarif
653300141107     c                   endif
653400141107     c
653500140822     c                   endsl
653600140822     c
653700140825     c                   eval       IOREtser = 'C'
653800141107     c                   if         or_ddc>0 and or_ddc<d98ddc
653900141107     c                   eval       IOREDDC=or_ddc
654000141107     c                   eval       IOREnDC=8888888
654100141107     c                   eval       IOREcons = 'S'
654200141107     c                   else
654300140825     c                   eval       IOREDDC=d98ddc
654400140825     c                   eval       IOREnDC=wd98ndc
654500140825     c                   eval       IOREcons = d98cons
654600141107     c                   endif
654700141017     c                   eval       IOREtsp = d98tsp
654800140822     c
654900140822     c                   call      'TRULORSR'
655000140822     c                   parm                    kpjba
655100140822     c                   parm                    trulorSds
655200140825     c                   parm                    trulor2ds
655300140825     c
655400140825     c                   eval      wtrulor= 'S'
655500141107     c                   clear                   or_datarif        8 0
655600141107     c                   clear                   or_ddc            8 0
655700140822     C                   ENDSR
655800950406     C********************************************************************
655900950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
656000950406     C********************************************************************
656100950406     C     DATORE        BEGSR
656200950406     C*
656300950406     C                   Z-ADD     *ZEROS        WORE
656400950406     C     WDAT          IFNE      0
656500950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
656600950406     C     WAA           DIV       4             WQUA              4 0
656700950406     C                   MVR                     WRES
656800950406     C     WAA           MULT      365           WGIO              9 0
656900950406     C                   ADD       WQUA          WGIO
657000950406     C                   ADD       WGG           WGIO
657100950406     C     WMM           SUB       1             MC                3 0
657200950406     C                   DO        MC            WX                3 0
657300950406     C                   ADD       MES(WX)       WGIO
657400950406     C                   END
657500950406     C     WRES          IFEQ      0
657600950406     C     WMM           ANDLE     2
657700950406     C                   SUB       1             WGIO
657800950406     C                   END
657900950406     C     WGIO          MULT      24            WORE              9 0
658000950406     C                   ENDIF
658100950406     C*
658200950406     C                   ENDSR
658300930323     C********************************************************************
658400930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
658500930323     C********************************************************************
658600930323     C     OREDAT        BEGSR
658700930323     C*
658800950407     C                   SETOFF                                       717273
658900950407     C                   SETOFF                                       7475
659000930323     C     WORE          DIV       24            WGIO
659100930323     C                   MVR                     WRES              3 0
659200950406     C*
659300950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
659400950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
659500950406     C*  TROVO ANNO
659600950406     C     WGIO          DIV       365           WAA
659700950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
659800950406     C                   MVR                     WREST                71
659900950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
660000950406     C     WGIO          DIV       365           WAA
660100950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
660200950406     C*                                                    0 = 31/12/PR
660300950406     C   75
660400950406     CAN 71              DO                                                     BIS. E NON ERA
660500950406     C     WAA           SUB       1             WAA
660600950406     C                   Z-ADD     365           WREST
660700950406     C                   END
660800950406     C*
660900950406     C     WAA           DIV       4             WNBIS                                      T.
661000950406     C                   MVR                     WRBIS             3 073
661100950406     C   73
661200950406     CANN71                                                                     NON PIU' BISEST
661300950406     CORN73              ADD       1             WREST                          E'BISESTILE
661400950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
661500950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
661600950406     C  N73              SUB       1             $PM(1)
661700950406     C                   Z-ADD     1             WI                2 0
661800950406     C* TROVO IL MESE E GIORNI
661900950406     C     WREST         LOOKUP    $PM(WI)                            74  73
662000950406     C                   MOVE      WI            WMM
662100950406     C     WI            COMP      1                                      72
662200950406     C  N72
662300950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
662400950406     C   74              SUB       1             WI
662500950406     C  N72
662600950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
662700950406     C   72              MOVE      WREST         WGG
662800950406     C                   ADD       1900          WAA
662900950406     C*
663000930323     C*
663100930323     C                   ENDSR
6632009407151507 C********************************************************************
6633009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6634009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6635009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6636009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6637009407151507 C********************************************************************
6638009407151507 C     FINGIO        BEGSR
6639009407151507 C*
6640009407151507 C     WPORE         DIV       24            WGIO
6641009407151507 C                   MVR                     WRES
6642009407151507 C     WRES          IFEQ      *ZEROS
6643009407151507 C                   Z-ADD     12            WNORE             3 0
6644009407151507 C                   ELSE
6645009407151507 C                   Z-ADD     *ZEROS        WNORE
6646009407151507 C                   ENDIF
6647009407151507 C*
6648009407151507 C                   ENDSR
664900930517     C********************************************************************
665000971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
665100930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
665200930517     C********************************************************************
665300930517     C     CERCAL        BEGSR
665400930517     C*
665500930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
665600941227     C                   MOVE      CLNTFP        DCATFP
665700941227     C                   MOVE      CLNTFA        DCATFA
665800941227     C                   MOVE      CLNANN        DCAANN
665900941227     C                   MOVE      CLNMES        DCAMES
666000930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
666100930517     C                   Z-ADD     1             I
666200930517     C     DCALEN        LOOKUP    CAK(I)                                 99
666300930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
666400930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
666500941227     C     KEYCLN        CHAIN     AZCLN01L                           99
666600930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
666700930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
666800930517     C                   MOVEL     '1'           D98ERR
666900930517     C                   MOVEL     MSG(3)        D98MSG
667000930517     C                   ELSE                                                   <-- 2 -->
667100970401     C* CONTROLLA SE ANNULLATO
667200970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
667300970401     C                   MOVEL     '1'           D98ERR
667400970401     C                   MOVEL     MSG(3)        D98MSG
667500970401     C                   ELSE                                                   <-- 3 -->
667600930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
667700930517     C                   ADD       1             WINDCA            6 0
667800970401     C     WINDCA        IFLE      200                                          --- 4 -->
667900970401     C     WINDCA        ANDGT     *ZEROS
668000930517     C                   Z-ADD     WINDCA        I
668100930517     C                   MOVEL     DCALEN        CAK(I)
668200941227     C                   MOVEL     CLNMAT        CAM(I)
668300941227     C                   MOVEL     CLNPOM        CAP(I)
668400970401     C                   END                                                    <-- 4 ---
668500970401     C                   ENDIF                                                  <-- 3 ---
668600930517     C                   END                                                    <-- 2 ---
668700930517     C                   ELSE                                                   <-- 1 -->
668800930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
668900941227     C                   MOVEL     CAM(I)        CLNMAT
669000941227     C                   MOVEL     CAP(I)        CLNPOM
669100930517     C                   END                                                    <-- 1 ---
669200930517     C*
669300930517     C                   ENDSR
669400010821     C********************************************************************
669500010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
669600010821     C********************************************************************
669700010821     C     CHKVCA        BEGSR
669800010821     C*
669900010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
670000010821     C     D98CAS        IFGT      *ZEROS
670100010821     C     D98VCA        ANDEQ     *BLANKS
670200010821     C                   MOVEL     'ITL'         D98VCA
670300010821     C                   ENDIF
670400010822     C*
670500010822     C     D98VCA        IFNE      'ITL'
670600010822     C     D98VCA        ANDNE     'EUR'
670700010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
670800010822     C                   Z-ADD     D98CAS        INPIMP
670900010822     C                   MOVEL     D98VCA        INPDIV
671000010822     C                   EXSR      EXECNV
671100010822     C                   Z-ADD     OUTIMP        D98CAS
671200010822     C                   MOVEL     §GEDCN        D98VCA
671300010822     C                   ENDIF
671400010821     C*
671500010821     C                   ENDSR
671600010821     C********************************************************************
671700010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
671800010821     C********************************************************************
671900010821     C     EXECNV        BEGSR
672000010821     C*
672100010821     C                   CLEAR                   YEURDS
672200010821     C*
672300010821     C                   MOVEL     INPDIV        YECDVI
672400010821     C                   Z-ADD     INPIMP        YECIMI
672500010821     C                   MOVEL     §GEDCN        YECDVO
672600010821     C                   MOVEL     '3'           YECDCO
672700010821     C                   MOVEL     'H'           YECTAR
672800010821     C                   CALL      'YEURCO'
672900010821     C                   PARM                    YEURDS
673000010821     C*
673100010821     C                   Z-ADD     YECIMO        OUTIMP
673200010821     C*
673300010821     C                   ENDSR
673400011211     C*------------------------------------------------------------------------*
673500011211     C* Controllo importo massimo contrassegno.
673600011211     C*------------------------------------------------------------------------*
673700011211     C     REPMAXCSB     BEGSR
673800011211     C*
673900011211     C                   CLEAR                   Trul21DS
674000011211     C*
674100160420     c                   eval      i21cbo = d98cbo
674200011211     C                   EVAL      I21TSp = d98TSP
674300011211     C                   EVAL      I21NzD = d98NZD
674400011211     C                   EVAL      I21LnP = d98LNP
674500011211     C                   EVAL      I21LnA = d98LNA
674600011211     C                   EVAL      I21Ksc = d98KSC
674700011211     C                   EVAL      I21TiC = d98TIC
674800011211     C                   EVAL      I21Imp = d98CAS
674900011211     C                   EVAL      I21Div = d98VCA
675000160420     c                   eval      i21cbo = d98cbo
675100011211     C*
675200011211     C                   CALL      'TRUL21R'
675300011211     C                   PARM                    Trul21DS
675400160420
675500160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
675600160420     c                   if        wtrasm='N'
675700160420     c                   if        o21fca <>*blanks
675800160420     c                   eval      §b4ltca = 'S'
675900160420     c                   endif
676000160420     c                   endif
676100011211     C*
676200011211     C                   ENDSR
676300011211     C*------------------------------------------------------------------------*
676400011211
676500930414     C********************************************************************
676600930414     C*   RUTINZ - ROUTINE INIZIALE
676700930414     C********************************************************************
676800930414     C     RUTINZ        BEGSR
676900930414     C*
677000930414     C* PARAMETRI IN INPUT ALLA ROUTINE
677100930414     C     *ENTRY        PLIST
677200970731     C                   PARM                    DS99
677300941227     C*
677400941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
677500941227     C     KEYBLP        KLIST
677600941227     C                   KFLD                    BLPAAS
677700941227     C                   KFLD                    BLPLNP
677800941227     C                   KFLD                    BLPNRS
677900941227     C                   KFLD                    BLPNSP
678000050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
678100941227     C     KEYGCP        KLIST
678200050405     C                   KFLD                    KWGCPAAS
678300050405     C                   KFLD                    KWGCPLNP
678400050405     C                   KFLD                    KWGCPNRS
678500050405     C                   KFLD                    KWGCPNSP
678600050405     C                   KFLD                    KWGCPFRG
678700050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
678800031125     C     KEYGC_P       KLIST
678900050405     C                   KFLD                    KWGCPAAS
679000050405     C                   KFLD                    KWGCPLNP
679100050405     C                   KFLD                    KWGCPNRS
679200050405     C                   KFLD                    KWGCPNSP
679300970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
679400150120     C**   KEYBLT        KLIST
679500150120     C**                 KFLD                    BLTAAS
679600150120     C**                 KFLD                    BLTLNP
679700150120     C**                 KFLD                    BLTNRS
679800150120     C**                 KFLD                    BLTNSP
679900941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
680000941227     C     KEYARB        KLIST
680100941227     C                   KFLD                    ARBAAS
680200941227     C                   KFLD                    ARBLNP
680300941227     C                   KFLD                    ARBNRS
680400941227     C                   KFLD                    ARBNSP
680500051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
680600941228     C     KEYAR9        KLIST
680700941228     C                   KFLD                    AR9AAS
680800941228     C                   KFLD                    AR9LNP
680900941228     C                   KFLD                    AR9NRS
681000941228     C                   KFLD                    AR9NSP
681100941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
681200050405     C     KEYGCP21      KLIST
681300050405     C                   KFLD                    KWGCPAAS
681400050405     C                   KFLD                    KWGCPLNP
681500050405     C                   KFLD                    KWGCPNRS
681600050405     C                   KFLD                    KWGCPNSP
681700050405     C                   KFLD                    KWGCPFRG
681800970912     C* CHIAVE LETTURA FNART01L - PARZIALE
681900150120     C***  KEYART        KLIST
682000150120     C***                KFLD                    ARTAAS
682100150120     C***                KFLD                    ARTLNP
682200150120     C***                KFLD                    ARTNRS
682300150120     C***                KFLD                    ARTNSP
682400950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
682500950102     C     KEYLBL        KLIST
682600950102     C                   KFLD                    LBLAAN
682700950102     C                   KFLD                    LBLLPN
682800950102     C                   KFLD                    LBLNRN
682900950102     C                   KFLD                    LBLNSN
683000930414     C*
683100070827     c     kAR5          Klist
683200070827     c                   Kfld                    d98Aas
683300070827     c                   Kfld                    d98Lnp
683400070827     c                   Kfld                    d98nrs
683500070827     c                   Kfld                    d98nsp
683600070827     c                   Kfld                    kAr5Trd
683700150325     c     kAR4          Klist
683800150325     c                   Kfld                    d98Aas
683900150325     c                   Kfld                    d98Lnp
684000150325     c                   Kfld                    d98nrs
684100150325     c                   Kfld                    d98nsp
684200150325     c                   Kfld                    kAr4Trc
684300150218     c*
684400150218     c     kart          Klist
684500150218     c                   Kfld                    d98Aas
684600150218     c                   Kfld                    d98Lnp
684700150218     c                   Kfld                    d98nrs
684800150218     c                   Kfld                    d98nsp
684900150218     c     kbrv          Klist
685000150218     c                   Kfld                    artfls
685100150218     c                   Kfld                    artlna
685200150218     c                   Kfld                    artnrs
685300150218     c                   Kfld                    artnsc
685400150325     C     Kfapd         klist
685500150325     C                   kfld                    kpdtip
685600150325     C                   kfld                    kpdpdr
685700930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
685800930414     C     KEYTAB        KLIST
685900930414     C                   KFLD                    KTBUTE
686000930414     C                   KFLD                    KTBCOD
686100930414     C                   KFLD                    KTBKEY
686200060406     C     Ktab          KLIST
686300060406     C                   KFLD                    KTBUTE
686400060406     C                   KFLD                    KTBCOD
686500930414     C*
686600941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
686700941227     C     KEYCLN        KLIST
686800941227     C                   KFLD                    CLNTFP
686900941227     C                   KFLD                    CLNTFA
687000941227     C                   KFLD                    CLNANN
687100941227     C                   KFLD                    CLNMES
687200941227     C*
687300941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
687400941227     C     KEYCAS        KLIST
687500941227     C                   KFLD                    CASPTA
687600941227     C                   KFLD                    CASLNP
687700941227     C                   KFLD                    CASLNA
687800941227     C                   KFLD                    CASTSP
687900941227     C                   KFLD                    CASDDE
688000930420     C*
688100990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
688200930420     C     KEYEVB        KLIST
688300930420     C                   KFLD                    KEBAAS
688400930420     C                   KFLD                    KEBLNP
688500930420     C                   KFLD                    KEBNRS
688600930420     C                   KFLD                    KEBNSP
688700010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
688800010821     C     KTBE          KLIST
688900010821     C                   KFLD                    TBECOD
689000010821     C                   KFLD                    TBEKE1
689100010821     C                   KFLD                    TBEKE2
689200930414     C*
689300010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
689400930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
689500930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
689600930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
689700930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
689800930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
689900930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
690000930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
690100930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
690200930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
690300010821     C     *LIKE         DEFINE    D98CAS        INPIMP
690400010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
690500010821     C     *LIKE         DEFINE    D98VCA        INPDIV
690600930414     C*
690700930414     C* RECUPERO IL CODICE DELL'AZIENDA
690800930414     C                   Z-ADD     1             CODUT             1 0
690900970701     C                   Z-ADD     *ZEROS        I                 6 0
691000930414     C*
691100930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
691200930414     C                   EXSR      AZZERA
691300930414     C*
691400930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
691500941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
691600941227     C                   MOVEL     'S'           CTABSN            1
691700930414     C                   EXSR      CARTAB
691800941227     C                   ENDIF
691900041116     c* Se passato umero di spedizioni apro i files , altrimenti no
692000041116     c                   if        d98nsp>0 and wapri=' '
692100041116     c                   open      fnblp01l
692200050405     c                   open      tigcp01l
692300150120     c***                open      fnblt01l
692400041116     c                   open      fnarb01l
692500051115     c                   open      fiar901l
692600150325     c                   open      fiar401l
692700050405     c                   open      tigcp21l
692800150218     c                   open      fnart01l
692900150218     c                   open      fnbrv07l
693000041116     c                   open      fnlbl01l
693100041116     c                   open      fnevb01l
693200070827     c                   open      fiar501l
693300041116     c                   eval      wapri='S'
693400041116     c                   endif
693500930414     C*
693600930414     C                   ENDSR
693700941227     C*------------------------------------------------------------------------*
693800941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
693900941227     C*------------------------------------------------------------------------*
694000941227     C     SUBDAT        BEGSR
694100941227     C*
694200941227     C                   TIME                    DTTIME
694300941227     C                   Z-ADD     DTANN         DSANN
694400941227     C                   Z-ADD     DTMES         DSMES
694500941227     C                   Z-ADD     DTGIO         DSGIO
694600941227     C                   Z-ADD     DTSEC         DSSEC
694700941227     C*
694800941227     C                   ENDSR
694900930414     C********************************************************************
695000930414     C*   CARTAB - CARICA LE TABELLE
695100930414     C********************************************************************
695200930414     C     CARTAB        BEGSR
695300930414     C*
695400930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
695500930414     C     D98ERR        IFEQ      '0'
695600930421     C                   Z-ADD     0             I
695700930414     C                   Z-ADD     CODUT         KTBUTE
695800930414     C                   MOVEL     '1Y'          KTBCOD
695900930414     C                   MOVEL     *BLANKS       KTBKEY
696000930414     C     KEYTAB        SETLL     TABEL00F                           99
696100930414     C  N99              READ      TABEL00F                               99
696200930414     C     *IN99         IFEQ      '1'
696300930414     C                   MOVEL     '1'           D98ERR
696400930414     C                   MOVEL     MSG(5)        D98MSG
696500930414     C                   END
696600930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
696700930414     C     TBLCOD        ANDEQ     '1Y'
696800930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
696900930421     C                   ADD       1             I
697000930421     C                   MOVEL     TBLKEY        C1Y(I)
697100930414     C                   MOVEL     TBLUNI        DS1Y
697200930421     C                   MOVEL     §1YFEI        I1Y(I)
697300930421     C                   MOVEL     §1YFEC        E1Y(I)
697400930414     C                   END                                                    <-- 3 ---
697500930414     C                   READ      TABEL00F                               99
697600930414     C                   END                                                    <-- 2 ---
697700930414     C                   END
697800930414     C*
697900930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
698000930414     C     D98ERR        IFEQ      '0'
698100930421     C                   Z-ADD     0             I
698200930414     C                   Z-ADD     CODUT         KTBUTE
698300930414     C                   MOVEL     '1X'          KTBCOD
698400930414     C                   MOVEL     *BLANKS       KTBKEY
698500930414     C     KEYTAB        SETLL     TABEL00F                           99
698600930414     C  N99              READ      TABEL00F                               99
698700930414     C     *IN99         IFEQ      '1'
698800930414     C                   MOVEL     '1'           D98ERR
698900930414     C                   MOVEL     MSG(1)        D98MSG
699000930414     C                   END
699100930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
699200930414     C     TBLCOD        ANDEQ     '1X'
699300930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
699400930421     C                   ADD       1             I
699500930421     C                   MOVEL     TBLKEY        C1X(I)
699600930414     C                   MOVEL     TBLUNI        DS1X
699700930421     C                   Z-ADD     §1XHHG        G1X(I)
699800930421     C                   Z-ADD     §1XHHO        O1X(I)
699900930421     C                   Z-ADD     §1XHHM        M1X(I)
700000930421     C                   Z-ADD     §1XHHP        P1X(I)
700100930421     C                   Z-ADD     §1XHHI        I1X(I)
700200930421     C                   Z-ADD     §1XHHS        S1X(I)
700300930414     C                   END                                                    <-- 3 ---
700400930414     C                   READ      TABEL00F                               99
700500931223     C                   ENDDO                                                  <-- 2 ---
700600050523     c
700700050523     c* controllo riempimento skiere
700800050523     c                   clear                   trul0sds
700900050523     c                   eval      i0sski='C1X/G1X/O1X/...'
701000050523     c                   eval      i0sele=%elem(c1x)
701100050523     c                   eval      i0spie=i
701200050523     c                   eval      i0sfile='TABEL00F  '
701300050523     c                   eval      i0scod=' 1X  '
701400050523     c                   eval      i0spgm='TNSD99R'
701500050523     c                   movel     trul0sds      kpjbu
701600050523     c                   call      'TRUL0SR'
701700050523     c                   parm                    kpjba
701800050523     C                   END
701900930421     C*
702000930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
702100930421     C     D98ERR        IFEQ      '0'
702200930421     C                   Z-ADD     0             I
702300930421     C                   Z-ADD     CODUT         KTBUTE
702400930421     C                   MOVEL     '2A'          KTBCOD
702500060406     C     Ktab          SETLL     TABEL00F                           99
702600060406     C  N99ktab          READe     TABEL00F                               99
702700930421     C     *IN99         IFEQ      '1'
702800930421     C                   MOVEL     '1'           D98ERR
702900930421     C                   MOVEL     MSG(8)        D98MSG
703000930421     C                   END
703100930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
703200930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
703300930421     C                   ADD       1             I
703400930421     C                   MOVEL     TBLKEY        C2A(I)
703500060406     C                   MOVEL     TBLUNI        ds2a
703600060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
703700060406     C                   MOVEL     w003a         D2A(I)
703800930421     C                   END                                                    <-- 3 ---
703900060406     C     ktab          READe     TABEL00F                               99
704000931223     C                   ENDDO                                                  <-- 2 ---
704100050523     c* controllo riempimento skiere
704200050523     c                   clear                   trul0sds
704300050523     c                   eval      i0sski='C2A/D2A'
704400050523     c                   eval      i0sele=%elem(c2a)
704500050523     c                   eval      i0spie=i
704600050523     c                   eval      i0sfile='TABEL00F  '
704700060406     c                   eval      i0scod='2A/2Z'
704800050523     c                   eval      i0spgm='TNSD99R'
704900050523     c                   movel     trul0sds      kpjbu
705000050523     c                   call      'TRUL0SR'
705100050523     c                   parm                    kpjba
705200930421     C                   END
705300060406     c* Tabella  2Z
705400060406     C     D98ERR        IFEQ      '0'
705500060406     C                   Z-ADD     CODUT         KTBUTE
705600060406     C                   MOVEL     '2Z'          KTBCOD
705700060406     C     Ktab          SETLL     TABEL00F                           99
705800060406     C  N99ktab          READe     TABEL00F                               99
705900060406     C     *IN99         IFEQ      '1'
706000060406     C                   MOVEL     '1'           D98ERR
706100060406     C                   MOVEL     MSG(33)       D98MSG
706200060406     C                   END
706300060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
706400060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
706500060406     C                   Z-ADD     1             I
706600060406     C                   MOVEL     TBLKEY        w003a
706700060406     c     w003a         lookup    c2a(i)                                 99
706800060406     C     *IN99         IFEQ      *off
706900060406     C                   MOVEL     '1'           D98ERR
707000060406     C                   MOVEL     MSG(34)       D98MSG
707100060406     C                   Else
707200060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
707300060406     C                   END                                                    <-- 3 ---
707400060406     C                   END                                                    <-- 3 ---
707500060406     C     ktab          READe     TABEL00F                               99
707600060406     C                   ENDDO                                                  <-- 2 ---
707700060406     C                   END
707800941227     C*
707900941227     C* CARICA TABELLA CODICI BOLLA -3A-
708000941227     C     D98ERR        IFEQ      '0'
708100941228     C                   Z-ADD     0             I
708200941227     C                   Z-ADD     CODUT         KTBUTE
708300941227     C                   MOVEL     '3A'          KTBCOD
708400941227     C                   MOVEL     *BLANKS       KTBKEY
708500941227     C     KEYTAB        SETLL     TABEL00F                           99
708600941227     C  N99              READ      TABEL00F                               99
708700941227     C     *IN99         IFEQ      '1'
708800941227     C                   MOVEL     '1'           D98ERR
708900941227     C                   MOVEL     MSG(22)       D98MSG
709000941227     C                   END
709100941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
709200941227     C     TBLCOD        ANDEQ     '3A'
709300941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
709400941227     C                   ADD       1             I
709500941227     C                   MOVEL     TBLKEY        C3A(I)
709600941227     C                   MOVEL     TBLUNI        DS3A
709700941227     C                   MOVEL     §3ATB1        T3A(I)
709800941228     C                   Z-ADD     §3AFCA        F3A(I)
709900990308     C                   MOVEL     §3ARBL        R3A(I)
710000941227     C                   END                                                    <-- 3 ---
710100941227     C                   READ      TABEL00F                               99
710200941227     C                   END                                                    <-- 2 ---
710300050523     c* controllo riempimento skiere
710400050523     c                   clear                   trul0sds
710500050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
710600050523     c                   eval      i0sele=%elem(c3a)
710700050523     c                   eval      i0spie=i
710800050523     c                   eval      i0sfile='TABEL00F  '
710900050523     c                   eval      i0scod=' 3A  '
711000050523     c                   eval      i0spgm='TNSD99R'
711100050523     c                   movel     trul0sds      kpjbu
711200050523     c                   call      'TRUL0SR'
711300050523     c                   parm                    kpjba
711400941227     C                   END
711500971210     C*
711600020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
711700020226     C                   Z-ADD     *ZEROS        J                 3 0
711800160331     C                   Z-ADD     *ZEROS        D                 3 0
711900971210IF  0C     D98ERR        IFEQ      '0'
712000971210     C     *LOVAL        SETLL     AZORG01L
712100971210     C                   READ      AZORG01L                               98
712200971210DO  1C     *IN98         DOWEQ     *OFF
712300971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
712400971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
712500971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
712600020627     C                   MOVEL     ORGDE3        OG143
712700020702     C*
712800020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
712900020702     C                   CLEAR                   DNTW
713000020702     C*
713100020702     C                   MOVEL(P)  'NTW'         TBECOD
713200020702     C                   MOVEL(P)  §OGNTW        TBEKE1
713300020702     C     KTBE          CHAIN     TNTBE01L
713400020702     C                   IF        %found(TNTBE01L)
713500020702     C                   MOVEL     TBEUNI        DNTW
713600020702     C                   ENDIF
713700020702     C*
713800020702     C                   MOVEL     §NTWFIE       FL1(F)
713900020627     C*
714000020226     C* VERIFICO SE FILIALE FEDEX
714100020226     C                   IF        §OGNTW = 'FED'
714200020226     C                   ADD       1             J
714300020226     C                   Z-ADD     ORGFIL        FILFED(J)
714400020226     C                   ENDIF
714500160331     C* VERIFICO SE FILIALE DPD
714600160331     C                   IF        §OGNTW = 'DPD'
714700160331     C                   ADD       1             D
714800160331     C                   Z-ADD     ORGFIL        FILDPD(D)
714900160331     C                   ENDIF
715000020226     C*
715100971210E   2C                   ENDIF
715200971210     C                   READ      AZORG01L                               98
715300971210E   1C                   ENDDO
715400971210E   0C                   ENDIF
715500010821     C*
715600010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
715700010821     C                   MOVEL     'GED'         TBECOD
715800070514     C                   MOVEL     *BLANKS       TBEKE1
715900070514     C                   MOVEL     '1'           TBEKE1
716000010821     C                   MOVEL     *BLANKS       TBEKE2
716100010821     C     KTBE          CHAIN     TNTBE01L                           21
716200070514     C  N21              MOVEL     TBEUNI        DGED
716300150326     c
716400150326     C* REPERISCO LA tabella flag operativi per tabella OLP
716500150326     c                   clear                   dvpo
716600150326     C                   MOVEL     'VPO'         TBECOD
716700150326     C                   MOVEL     *BLANKS       TBEKE2
716800150326     C                   MOVEL(P)  'VPO'         TBEKE1
716900150326     C     KTBE          CHAIN     TNTBE01L                           21
717000150326     C  N21              MOVEL     TBEUNI        DVPO
717100150326     C*
717200031201     C*
717300031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
717400031201     C     D98ERR        IFEQ      '0'
717500060403     C                   Z-ADD     0             I7QW              3 0
717600031201     C                   Z-ADD     CODUT         KTBUTE
717700031201     C                   MOVEL     '7Q'          KTBCOD
717800031201     C                   MOVEL     *BLANKS       KTBKEY
717900031201     C     KEYTAB        SETLL     TABEL00F                           99
718000031201     C  N99              READ      TABEL00F                               99
718100031201     C     *IN99         IFEQ      '1'
718200031201     C                   MOVEL     '1'           D98ERR
718300031201     C                   MOVEL     MSG(8)        D98MSG
718400031201     C                   ENDIF
718500031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
718600031201     C     TBLCOD        ANDEQ     '7Q'
718700031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
718800031201     C                   MOVEL     TBLUNI        DS7Q
718900031201     C                   IF        §7Q4D1 = 'R' OR
719000031201     C                             §7Q4D2 = 'R' OR
719100031201     C                             §7Q4D3 = 'R' OR
719200031201     C                             §7Q4D4 = 'R' OR
719300031201     C                             §7Q4D5 = 'R'
719400031201     C                   ADD       1             I7QW
719500031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
719600031201     C                   ENDIF                                                  <-- 3 ---
719700031201     C                   ENDIF                                                  <-- 3 ---
719800031201     C                   READ      TABEL00F                               99
719900031201     C                   ENDDO                                                  <-- 2 ---
720000031201     C                   ENDIF
720100031201     C*
720200031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
720300031201     C     D98ERR        IFEQ      '0'
720400031201     C                   Z-ADD     CODUT         KTBUTE
720500031201     C                   MOVEL     '7W'          KTBCOD
720600031201     C                   MOVEL     *BLANKS       KTBKEY
720700031201     C     KEYTAB        SETLL     TABEL00F                           99
720800031201     C  N99              READ      TABEL00F                               99
720900031201     C     *IN99         IFEQ      '1'
721000031201     C                   MOVEL     '1'           D98ERR
721100031201     C                   MOVEL     MSG(8)        D98MSG
721200031201     C                   ENDIF
721300031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
721400031201     C     TBLCOD        ANDEQ     '7W'
721500031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
721600031201     C                   MOVEL     TBLUNI        DS7W
721700031201     C                   IF        §7W6D1 = 'C' OR
721800031201     C                             §7W6D2 = 'C' OR
721900031201     C                             §7W6D3 = 'C' OR
722000031201     C                             §7W6D4 = 'C' OR
722100031201     C                             §7W6D5 = 'C'
722200031201     C                   ADD       1             I7QW
722300031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
722400031201     C                   ENDIF                                                  <-- 3 ---
722500031201     C                   ENDIF                                                  <-- 3 ---
722600031201     C                   READ      TABEL00F                               99
722700031201     C                   ENDDO                                                  <-- 2 ---
722800050523     c* controllo riempimento skiere
722900050523     c                   clear                   trul0sds
723000050523     c                   eval      i0sski='SK7QW'
723100050523     c                   eval      i0sele=%elem(sk7qw)
723200050523     c                   eval      i0spie=i7qw
723300050523     c                   eval      i0sfile='TABEL00F  '
723400050523     c                   eval      i0scod='7Q-7W'
723500050523     c                   eval      i0spgm='TNSD99R'
723600050523     c                   movel     trul0sds      kpjbu
723700050523     c                   call      'TRUL0SR'
723800050523     c                   parm                    kpjba
723900031201     C                   ENDIF
724000930414     C*
724100930414     C                   ENDSR
724200930414     C********************************************************************
724300930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
724400930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
724500930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
724600930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
724700930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
724800930414     C********************************************************************
724900930414     C     RO3ORE        BEGSR
725000930414     C*
725100930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
725200930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
725300930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
725400050902     c                   movel     ' '           WR3MATPOM         1
725500931209     C                   Z-ADD     *ZEROS        WR3HHO
725600931209     C                   Z-ADD     *ZEROS        WR3HHA
725700930414     C                   MOVE      WR3TAB        WR3COD
725800930604     C                   MOVEL     D98LNA        WR3COD            8
725900930421     C                   Z-ADD     1             I
726000930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
726100930414     C     *IN99         IFEQ      '0'
726200930414     C                   MOVE      WR3TAB        WR3COD
726300930414     C                   MOVEL     '   '         WR3COD            8
726400930421     C                   Z-ADD     1             I
726500930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
726600930414     C                   END
726700930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
726800930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
726900930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
727000930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
727100930414     C                   ELSE                                                   <-- 2 -->
727200930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
727300930421     C                   Z-ADD     M1X(I)        WR3HHO
727400930510     C                   Z-ADD     I1X(I)        WR3HHA
727500050902     c                   movel     'M'           WR3MATPOM         1
727600930414     C                   END
727700930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
727800930421     C                   Z-ADD     P1X(I)        WR3HHO
727900930510     C                   Z-ADD     S1X(I)        WR3HHA
728000050902     c                   movel     'P'           WR3MATPOM
728100930414     C                   END
728200930414     C                   END                                                    <-- 2 ---
728300930414     C                   END                                                    <-- 1 ---
728400930414     C*
728500930414     C                   ENDSR
728600930423     C********************************************************************
728700930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
728800930423     C********************************************************************
728900930423     C     W98D98        BEGSR
729000930423     C*
729100941228     C*
729200941228     C* ORE REALI PARTENZA INTERNE
729300941228     C                   Z-ADD     W98RPI        W98               3 0
729400941228     C                   EXSR      W98999
729500941228     C                   Z-ADD     D98           D98RPI
729600941228     C* ORE REALI PARTENZA CLIENTE
729700941228     C                   Z-ADD     W98RPC        W98
729800941228     C                   EXSR      W98999
729900941228     C                   Z-ADD     D98           D98RPC
730000930423     C* ORE REALI TRAZIONE INTERNE
730100941228     C                   Z-ADD     W98RTI        W98
730200941228     C                   EXSR      W98999
730300941228     C                   Z-ADD     D98           D98RTI
730400930423     C* ORE REALI TRAZIONE CLIENTE
730500941228     C                   Z-ADD     W98RTC        W98
730600941228     C                   EXSR      W98999
730700941228     C                   Z-ADD     D98           D98RTC
730800930423     C* ORE REALI CONSEGNA INTERNE
730900941228     C                   Z-ADD     W98RCI        W98
731000941228     C                   EXSR      W98999
731100941228     C                   Z-ADD     D98           D98RCI
731200930423     C* ORE REALI CONSEGNA CLIENTE
731300941228     C                   Z-ADD     W98RCC        W98
731400941228     C                   EXSR      W98999
731500941228     C                   Z-ADD     D98           D98RCC
731600930423     C* ORE REALI RESA INTERNE
731700941228     C                   Z-ADD     W98RRI        W98
731800941228     C                   EXSR      W98999
731900941228     C                   Z-ADD     D98           D98RRI
732000930423     C* ORE REALI RESA CLIENTE
732100941228     C                   Z-ADD     W98RRC        W98
732200941228     C                   EXSR      W98999
732300941228     C                   Z-ADD     D98           D98RRC
732400941228     C* ORE DIFF. PARTENZA INTERNE
732500941228     C                   Z-ADD     W98FPI        W98
732600941228     C                   EXSR      W98999
732700941228     C                   Z-ADD     D98           D98FPI
732800941228     C* ORE DIFF. PARTENZA CLIENTE
732900941228     C                   Z-ADD     W98FPC        W98
733000941228     C                   EXSR      W98999
733100941228     C                   Z-ADD     D98           D98FPC
733200930423     C* ORE DIFF. TRAZIONE INTERNE
733300941228     C                   Z-ADD     W98FTI        W98
733400941228     C                   EXSR      W98999
733500941228     C                   Z-ADD     D98           D98FTI
733600930423     C* ORE DIFF. TRAZIONE CLIENTE
733700941228     C                   Z-ADD     W98FTC        W98
733800941228     C                   EXSR      W98999
733900941228     C                   Z-ADD     D98           D98FTC
734000930423     C* ORE DIFF. CONSEGNA INTERNE
734100941228     C                   Z-ADD     W98FCI        W98
734200941228     C                   EXSR      W98999
734300941228     C                   Z-ADD     D98           D98FCI
734400930423     C* ORE DIFF. CONSEGNA CLIENTE
734500941228     C                   Z-ADD     W98FCC        W98
734600941228     C                   EXSR      W98999
734700941228     C                   Z-ADD     D98           D98FCC
734800930423     C* ORE DIFF. RESA INTERNE
734900941228     C                   Z-ADD     W98FRI        W98
735000941228     C                   EXSR      W98999
735100941228     C                   Z-ADD     D98           D98FRI
735200930423     C* ORE DIFF. RESA CLIENTE
735300941228     C                   Z-ADD     W98FRC        W98
735400941228     C                   EXSR      W98999
735500941228     C                   Z-ADD     D98           D98FRC
735600941228     C* ORE NON CONF. PARTENZA INTERNE
735700941228     C     W98NPI        IFGT      999
735800941228     C                   Z-ADD     999           D98NPI
735900941228     C                   ELSE
736000941228     C                   Z-ADD     W98NPI        D98NPI
736100941228     C                   END
736200941228     C* ORE NON CONF. PARTENZA CLIENTE
736300941228     C     W98NPC        IFGT      999
736400941228     C                   Z-ADD     999           D98NPC
736500941228     C                   ELSE
736600941228     C                   Z-ADD     W98NPC        D98NPC
736700941228     C                   END
736800930423     C* ORE NON CONF. TRAZIONE INTERNE
736900930423     C     W98NTI        IFGT      999
737000930423     C                   Z-ADD     999           D98NTI
737100930423     C                   ELSE
737200930423     C                   Z-ADD     W98NTI        D98NTI
737300930423     C                   END
737400930423     C* ORE NON CONF. TRAZIONE CLIENTE
737500930423     C     W98NTC        IFGT      999
737600930423     C                   Z-ADD     999           D98NTC
737700930423     C                   ELSE
737800930423     C                   Z-ADD     W98NTC        D98NTC
737900930423     C                   END
738000930423     C* ORE NON CONF. CONSEGNA INTERNE
738100930423     C     W98NCI        IFGT      999
738200930423     C                   Z-ADD     999           D98NCI
738300930423     C                   ELSE
738400930423     C                   Z-ADD     W98NCI        D98NCI
738500930423     C                   END
738600930423     C* ORE NON CONF. CONSEGNA CLIENTE
738700930423     C     W98NCC        IFGT      999
738800930423     C                   Z-ADD     999           D98NCC
738900930423     C                   ELSE
739000930423     C                   Z-ADD     W98NCC        D98NCC
739100930423     C                   END
739200930423     C* ORE NON CONF. RESA INTERNE
739300930423     C     W98NRI        IFGT      999
739400930423     C                   Z-ADD     999           D98NRI
739500930423     C                   ELSE
739600930423     C                   Z-ADD     W98NRI        D98NRI
739700930423     C                   END
739800930423     C* ORE NON CONF. RESA CLIENTE
739900930423     C     W98NRC        IFGT      999
740000930423     C                   Z-ADD     999           D98NRC
740100930423     C                   ELSE
740200930423     C                   Z-ADD     W98NRC        D98NRC
740300930423     C                   END
740400930423     C* ORE X TURNO CHIUSURA INTERNE
740500941228     C                   Z-ADD     W98HHI        W98
740600941228     C                   EXSR      W98999
740700941228     C                   Z-ADD     D98           D98HHI
740800930423     C* ORE X TURNO CHIUSURA CLIENTE
740900941228     C                   Z-ADD     W98HHC        W98
741000941228     C                   EXSR      W98999
741100941228     C                   Z-ADD     D98           D98HHC
741200060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
741300060418     C                   Z-ADD     W98EGI        W98
741400060418     C                   EXSR      W98999
741500060418     C                   Z-ADD     D98           D98hEGI
741600060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
741700060418     C                   Z-ADD     W98EGC        W98
741800060418     C                   EXSR      W98999
741900060418     C                   Z-ADD     D98           D98hEGC
742000060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
742100060420     C                   Z-ADD     W98EAI        W98
742200060420     C                   EXSR      W98999
742300060420     C                   Z-ADD     D98           D98hEAI
742400060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
742500060420     C                   Z-ADD     W98EAC        W98
742600060420     C                   EXSR      W98999
742700060420     C                   Z-ADD     D98           D98hEAC
742800930423     C* ORE X LASC. AVVISO INTERNE
742900941228     C                   Z-ADD     W98HLI        W98
743000941228     C                   EXSR      W98999
743100941228     C                   Z-ADD     D98           D98HLI
743200930423     C* ORE X LASC. AVVISO CLIENTE
743300941228     C                   Z-ADD     W98HLC        W98
743400941228     C                   EXSR      W98999
743500941228     C                   Z-ADD     D98           D98HLC
743600930423     C* ORE X GIACENZA INTERNE
743700941228     C                   Z-ADD     W98HGI        W98
743800941228     C                   EXSR      W98999
743900941228     C                   Z-ADD     D98           D98HGI
744000930423     C* ORE X GIACENZA CLIENTE
744100941228     C                   Z-ADD     W98HGC        W98
744200941228     C                   EXSR      W98999
744300941228     C                   Z-ADD     D98           D98HGC
744400941228     C* ORE X FEST. PART. INTERNE
744500941228     C                   Z-ADD     W98HPI        W98
744600941228     C                   EXSR      W98999
744700941228     C                   Z-ADD     D98           D98HPI
744800941228     C* ORE X FEST. PART. CLIENTE
744900941228     C                   Z-ADD     W98HPC        W98
745000941228     C                   EXSR      W98999
745100941228     C                   Z-ADD     D98           D98HPC
745200930423     C* ORE X FEST. TRAZ. INTERNE
745300941228     C                   Z-ADD     W98HFI        W98
745400941228     C                   EXSR      W98999
745500941228     C                   Z-ADD     D98           D98HFI
745600930423     C* ORE X FEST. TRAZ. CLIENTE
745700941228     C                   Z-ADD     W98HFC        W98
745800941228     C                   EXSR      W98999
745900941228     C                   Z-ADD     D98           D98HFC
746000930423     C* ORE X FEST. CONS. INTERNE
746100941228     C                   Z-ADD     W98HEI        W98
746200941228     C                   EXSR      W98999
746300941228     C                   Z-ADD     D98           D98HEI
746400930423     C* ORE X FEST. CONS. CLIENTE
746500941228     C                   Z-ADD     W98HEC        W98
746600941228     C                   EXSR      W98999
746700941228     C                   Z-ADD     D98           D98HEC
746800930511     C* ORE X F.DEPOSITO INTERNE
746900941228     C                   Z-ADD     W98HDI        W98
747000941228     C                   EXSR      W98999
747100941228     C                   Z-ADD     D98           D98HDI
747200930511     C* ORE X F.DEPOSITO CLIENTE
747300941228     C                   Z-ADD     W98HDC        W98
747400941228     C                   EXSR      W98999
747500941228     C                   Z-ADD     D98           D98HDC
747600930511     C* ORE X CONTRASSEGNO INTERNE
747700941228     C                   Z-ADD     W98HOI        W98
747800941228     C                   EXSR      W98999
747900941228     C                   Z-ADD     D98           D98HOI
748000930511     C* ORE X CONTRASSEGNO CLIENTE
748100941228     C                   Z-ADD     W98HOC        W98
748200941228     C                   EXSR      W98999
748300941228     C                   Z-ADD     D98           D98HOC
748400930914     C* ORE DI CONSEGNABILITA' INTERNE
748500941228     C                   Z-ADD     W98OCI        W98
748600941228     C                   EXSR      W98000
748700941228     C                   Z-ADD     D98           D98OCI
748800930914     C* ORE DI CONSEGNABILITA' CLIENTE
748900941228     C                   Z-ADD     W98OCC        W98
749000941228     C                   EXSR      W98000
749100941228     C                   Z-ADD     D98           D98OCC
749200071122     C
749300071122     c* Per i codici bolla M prepagati annullati, azzero tutto
749400071122     c                   if        d98cbo='M '
749500071122     c                   clear                   d98rpi
749600071122     c                   clear                   d98rpc
749700071122     c                   clear                   d98rti
749800071122     c                   clear                   d98rtc
749900071122     c                   clear                   d98rci
750000071122     c                   clear                   d98rcc
750100071122     c                   clear                   d98rri
750200071122     c                   clear                   d98rrc
750300071122     c                   clear                   d98fpi
750400071122     c                   clear                   d98fpc
750500071122     c                   clear                   d98fti
750600071122     c                   clear                   d98ftc
750700071122     c                   clear                   d98fci
750800071122     c                   clear                   d98fcc
750900071122     c                   clear                   d98fri
751000071122     c                   clear                   d98frc
751100071122     c                   clear                   d98npi
751200071122     c                   clear                   d98npc
751300071122     c                   clear                   d98nti
751400071122     c                   clear                   d98ntc
751500071122     c                   clear                   d98nci
751600071122     c                   clear                   d98ncc
751700071122     c                   clear                   d98nri
751800071122     c                   clear                   d98nrc
751900071122     c                   clear                   d98OCi
752000071122     c                   clear                   d98OCC
752100071122     c                   endif
752200930423     C*
752300100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
752400100308     c*  calcolo
752500100308     c                   eval      d98dcr=s98dcr
752600100308     c                   eval      d98hcr=s98hcr
752700100308     c                   eval      d98tcr=s98tcr
752800160405     c
752900160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
753000160405     c*  perchè il pgm usa il campo verificado se =10 )
753100160405     c                   if        wVP='S'
753200160405     c                   add       24            d98tci
753300160405     c                   add       24            d98tcc
753400160405     c                   add       24            d98tri
753500160405     c                   add       24            d98trc
753600160405     c                   endif
753700100308     c
753800930423     C                   ENDSR
753900941228     C***********************************************************************
754000941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
754100941228     C***********************************************************************
754200941228     C     W98999        BEGSR
754300941228     C     W98           IFLT      -999
754400941228     C                   Z-ADD     -999          D98               3 0
754500941228     C                   ELSE
754600941228     C     W98           IFGT      999
754700941228     C                   Z-ADD     999           D98
754800941228     C                   ELSE
754900941228     C                   Z-ADD     W98           D98
755000941228     C                   END
755100941228     C                   END
755200941228     C                   ENDSR
755300941228     C***********************************************************************
755400941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
755500941228     C***********************************************************************
755600941228     C     W98000        BEGSR
755700941228     C     W98           IFLE      *ZEROS
755800941228     C                   Z-ADD     *ZEROS        D98
755900941228     C                   ELSE
756000941228     C     W98           IFGT      999
756100941228     C                   Z-ADD     999           D98
756200941228     C                   ELSE
756300941228     C                   Z-ADD     W98           D98
756400941228     C                   END
756500941228     C                   END
756600941228     C                   ENDSR
756700930305     C***********************************************************************
756800930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
756900930305     C***********************************************************************
757000930305     C     AZZERA        BEGSR
757100150219     c* data e ora del momento
757200150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
757300150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
757400150219     C                   Z-ADD     DTORA4        WHHODI            4 0
757500930305     C*
757600141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
757700150603     C                   MOVEL     'N'           opendan
757800141107     c                   clear                   or_ddc
757900141108     c                   clear                   sav_12dat         8 0
758000141107     c                   clear                   or_datarif
758100140402     c                   clear                   wpick2            1
758200140402     c                   clear                   Primotipo
758300090928     c                   clear                   Primodev
758400090928     c                   clear                   Primohev
758500070827     c                   clear                   CallAR5
758600071122     c                   clear                   d98cbo
758700100308     c                   clear                   s98dcr
758800100308     c                   clear                   s98hcr
758900100308     c                   clear                   s98tcr
759000141106     c                   clear                   nosomma           1
759100141229     c                   clear                   wtrasm
759200150324     c                   clear                   avvcmc
759300150325     c                   clear                   DSbl4L
759400160401     c                   clear                   dsbl4I
759500150401     c                   clear                   fnlv82ds
759600150331     c                   clear                   DS7R
759700150605     c                   clear                   wDEE              1
759800150629     c                   clear                   wtsogliaE
759900930323     C                   MOVEL     '0'           D98ERR
760000930323     C                   MOVEL     *BLANKS       D98MSG
760100931124     C                   Z-ADD     *ZEROS        D98TPI
760200931124     C                   Z-ADD     *ZEROS        D98TPC
760300930325     C                   Z-ADD     *ZEROS        D98TTI
760400930325     C                   Z-ADD     *ZEROS        D98TTC
760500930325     C                   Z-ADD     *ZEROS        D98TCI
760600930325     C                   Z-ADD     *ZEROS        D98TCC
760700090827     C                   Z-ADD     *ZEROS        w99TCI
760800090827     C                   Z-ADD     *ZEROS        w99TCC
760900930325     C                   Z-ADD     *ZEROS        D98TRI
761000930325     C                   Z-ADD     *ZEROS        D98TRC
761100090921     C                   Z-ADD     *ZEROS        w99TRI
761200090921     C                   Z-ADD     *ZEROS        w99TRC
761300931124     C                   Z-ADD     *ZEROS        D98RPI
761400931124     C                   Z-ADD     *ZEROS        D98RPC
761500930405     C                   Z-ADD     *ZEROS        D98RTI
761600930405     C                   Z-ADD     *ZEROS        D98RTC
761700930405     C                   Z-ADD     *ZEROS        D98RCI
761800930405     C                   Z-ADD     *ZEROS        D98RCC
761900930405     C                   Z-ADD     *ZEROS        D98RRI
762000930405     C                   Z-ADD     *ZEROS        D98RRC
762100931124     C                   Z-ADD     *ZEROS        D98FPI
762200931124     C                   Z-ADD     *ZEROS        D98FPC
762300930325     C                   Z-ADD     *ZEROS        D98FTI
762400930325     C                   Z-ADD     *ZEROS        D98FTC
762500930325     C                   Z-ADD     *ZEROS        D98FCI
762600930325     C                   Z-ADD     *ZEROS        D98FCC
762700930325     C                   Z-ADD     *ZEROS        D98FRI
762800930325     C                   Z-ADD     *ZEROS        D98FRC
762900931124     C                   MOVEL     *BLANKS       D98CPI
763000931124     C                   MOVEL     *BLANKS       D98CPC
763100930325     C                   MOVEL     *BLANKS       D98CTI
763200930325     C                   MOVEL     *BLANKS       D98CTC
763300930325     C                   MOVEL     *BLANKS       D98CCI
763400930325     C                   MOVEL     *BLANKS       D98CCC
763500930325     C                   MOVEL     *BLANKS       D98CRI
763600930325     C                   MOVEL     *BLANKS       D98CRC
763700931124     C                   Z-ADD     *ZEROS        D98NPI
763800931124     C                   Z-ADD     *ZEROS        D98NPC
763900930325     C                   Z-ADD     *ZEROS        D98NTI
764000930325     C                   Z-ADD     *ZEROS        D98NTC
764100930325     C                   Z-ADD     *ZEROS        D98NCI
764200930325     C                   Z-ADD     *ZEROS        D98NCC
764300930325     C                   Z-ADD     *ZEROS        D98NRI
764400930325     C                   Z-ADD     *ZEROS        D98NRC
764500930414     C                   Z-ADD     *ZEROS        D98HHI
764600930414     C                   Z-ADD     *ZEROS        D98HHC
764700930414     C                   Z-ADD     *ZEROS        D98HOI
764800930414     C                   Z-ADD     *ZEROS        D98HOC
764900930414     C                   Z-ADD     *ZEROS        D98HLI
765000930414     C                   Z-ADD     *ZEROS        D98HLC
765100930414     C                   Z-ADD     *ZEROS        D98HGI
765200930414     C                   Z-ADD     *ZEROS        D98HGC
765300931124     C                   Z-ADD     *ZEROS        D98HPI
765400931124     C                   Z-ADD     *ZEROS        D98HPC
765500930414     C                   Z-ADD     *ZEROS        D98HFI
765600930414     C                   Z-ADD     *ZEROS        D98HFC
765700930416     C                   Z-ADD     *ZEROS        D98HEI
765800930416     C                   Z-ADD     *ZEROS        D98HEC
765900930414     C                   Z-ADD     *ZEROS        D98HRI
766000930414     C                   Z-ADD     *ZEROS        D98HRC
766100930511     C                   Z-ADD     *ZEROS        D98HDI
766200930511     C                   Z-ADD     *ZEROS        D98HDC
766300930325     C                   Z-ADD     *ZEROS        D98DCI
766400930325     C                   Z-ADD     *ZEROS        D98HCI
766500930325     C                   Z-ADD     *ZEROS        D98DCE
766600930325     C                   Z-ADD     *ZEROS        D98HCE
766700931125     C                   Z-ADD     *ZEROS        D98OCI
766800931125     C                   Z-ADD     *ZEROS        D98OCC
766900931202     C                   Z-ADD     *ZEROS        D98DEI
767000931202     C                   Z-ADD     *ZEROS        D98OEI
767100931202     C                   Z-ADD     *ZEROS        D98DEE
767200931202     C                   Z-ADD     *ZEROS        D98OEE
767300151019     c                   CLEAR                   D98FLP
767400150325     C                   clear                   D98dtctd
767500150326     C                   clear                   D98picklim
767600150325     C                   clear                   D98spcdee
767700150331     C                   clear                   fddata
767800140825     c
767900141017     c                   clear                   d98ddc
768000141017     c                   clear                   d98cons
768100141017     c                   clear                   wd98ndc
768200140910     c                   clear                   wd98fbc
768300150119     c                   clear                   wd98acc
768400140825     c                   clear                   wtrulor
768500160401     c                   clear                   wVP               1
768600150219     c
768700150219     c                   clear                   IMPdata
768800150219     c                   clear                   IMPfgs
768900150219     c                   clear                   DISdata
769000150219     c                   clear                   DISfgs
769100150219     c                   clear                   DISora
769200150219     c                   clear                   DISnpg
769300150219     c                   clear                   IDnpg
769400150219     c                   clear                   IDdata
769500150219     c                   clear                   IDora
769600150219     c                   clear                   IDfgs
769700931125     C* CAMPI VARI
769800931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
769900140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
770000140825      *   DALLA LETTURA DI QUESTI FILE                               *
770100931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
770200931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
770300931125     C     D98NSP        IFNE      *ZEROS
770400930525     C                   MOVEL     *BLANKS       D98TSP
770500941228     C                   MOVEL     *BLANKS       D98CAD
770600970728     C                   MOVEL     *BLANKS       D98LOD
770700930525     C                   Z-ADD     *ZEROS        D98TFP
770800930525     C                   Z-ADD     *ZEROS        D98TFA
770900930525     C                   Z-ADD     *ZEROS        D98LNA
771000930525     C                   Z-ADD     *ZEROS        D98DSP
771100941228     C                   Z-ADD     *ZEROS        D98MGS
771200930525     C                   Z-ADD     *ZEROS        D98DTI
771300930525     C                   Z-ADD     *ZEROS        D98HTI
771400930525     C                   Z-ADD     *ZEROS        D98DCM
771500930525     C                   Z-ADD     *ZEROS        D98HMC
771600930525     C                   MOVEL     *BLANKS       D98GC1
771700930525     C                   MOVEL     *BLANKS       D98GC2
771800010820     C                   Z-ADD     *ZEROS        D98CAS
771900010821     C                   MOVEL     *BLANKS       D98VCA
772000930525     C                   MOVEL     *BLANKS       D98TIC
772100930525     C                   Z-ADD     *ZEROS        D98DCR
772200930525     C                   Z-ADD     *ZEROS        D98HCR
772300930525     C                   MOVEL     *BLANKS       D98TCR
772400050914     C                   MOVEL     *BLANKS       Consideraora      1
772500930525     C                   MOVEL     *BLANKS       D98CMC
772600930525     C                   Z-ADD     *ZEROS        D98DAG
772700930525     C                   Z-ADD     *ZEROS        D98DCG
772800930525     C                   MOVEL     *BLANKS       D98FFD
772900930525     C                   Z-ADD     *ZEROS        D98NCD
773000930525     C                   MOVEL     *BLANKS       D98FNS
773100931124     C                   Z-ADD     *ZEROS        D98DAM
773200940628     C                   MOVEL     *BLANKS       D98FIN
773300940628     C                   Z-ADD     *ZEROS        D98KSC
773400940628     C                   MOVEL     *BLANKS       D98TBL
773500931125     C                   END
773600960229     C     D98TLA        IFEQ      'T'
773700960229     C                   Z-ADD     *ZEROS        D98DTI
773800960229     C                   Z-ADD     *ZEROS        D98HTI
773900960229     C                   ENDIF
774000931125     C* CAMPI WORK
774100930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
774200930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
774300150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
774400930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
774500941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
774600960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
774700930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
774800930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
774900940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
775000930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
775100150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
775200931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
775300931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
775400150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
775500930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
775600930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
775700930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
775800930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
775900930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
776000940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
776100941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
776200930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
776300050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
776400050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
776500140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
776600930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
776700930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
776800930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
776900930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
777000930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
777100930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
777200930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
777300930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
777400930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
777500930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
777600930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
777700930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
777800930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
777900930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
778000140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
778100931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
778200060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
778300960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
778400931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
778500140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
778600140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
778700150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
778800140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
778900941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
779000970304     C******************** Z-ADD*ZEROS    W98DBR  80
779100970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
779200150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
779300050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
779400930414     C*
779500941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
779600941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
779700930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
779800930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
779900930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
780000930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
780100930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
780200930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
780300941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
780400941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
780500930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
780600930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
780700930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
780800930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
780900930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
781000930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
781100941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
781200941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
781300930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
781400930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
781500930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
781600930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
781700930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
781800930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
781900930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
782000930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
782100060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
782200060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
782300060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
782400060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
782500930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
782600930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
782700930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
782800930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
782900930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
783000930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
783100930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
783200930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
783300930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
783400930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
783500930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
783600930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
783700930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
783800930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
783900941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
784000941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
784100930423     C*
784200930511     C                   Z-ADD     *ZEROS        W98HF             5 0
784300930423     C                   Z-ADD     *ZEROS        W98HH             5 0
784400060418     C                   Z-ADD     *ZEROS        W98EG             5 0
784500060420     C                   Z-ADD     *ZEROS        W98EA             5 0
784600930423     C                   Z-ADD     *ZEROS        W98HG             5 0
784700930423     C                   Z-ADD     *ZEROS        W98HL             5 0
784800930423     C                   Z-ADD     *ZEROS        W98HO             5 0
784900930511     C                   Z-ADD     *ZEROS        W98HD             5 0
785000930611     C*
785100930611     C                   Z-ADD     *ZEROS        WORELA            3 0
785200960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
785300930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
785400930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
785500931207     C                   Z-ADD     *ZEROS        WORECR            3 0
785600090902     c
785700090902     c                   clear                   wmancaTRAZ
785800150113     c                   clear                   wcaldpcee         1
785900150220     c                   clear                   evemic            2
786000930305     C*
786100060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
786200060406     C                   clear                   LAF                            *LASC.AVVISO FINE
786300091014     C                   clear                   LAp                            *posizione l.avviso
786400060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
786500060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
786600060406c    c                   clear                   LCon
786700060406     C                   clear                   EVD                            *DATA EVENTO
786800060406     C                   clear                   EVO                            *ORA EVENTO
786900060406     C                   clear                   EVC                            *CAUSALE EVENTO
787000131218     C                   clear                   EVT
787100060406     C                   clear                   pVD                            *DATA EVENTO
787200060406     C                   clear                   pVO                            *ORA EVENTO
787300060406     C                   clear                   pVC                            *CAUSALE EVENTO
787400060406     C                   clear                   pVF                            *CAUSALE EVENTO
787500060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
787600060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
787700060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
787800060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
787900060406     C                   clear                   COV                            *DATE FINE L.A. E GI
788000060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
788100060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
788200091014     C                   clear                   GIp                            *posizione vento gia
788300060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
788400060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
788500060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
788600931213     C*
788700060406     C                   clear                   TUC                            *TURNI CHIUSURA
788800931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
788900931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
789000060406     c
789100060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
789200060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
789300060406     c                   clear                   pgii
789400060406     c                   clear                   pgif
789500060406     c                   clear                   pgci
789600060406     c                   clear                   pgcf
789700060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
789800060420     c                   clear                   paii
789900060420     c                   clear                   paif
790000930420     C*
790100930305     C                   ENDSR
790200930512     C***********************************************************************
790300930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
790400930512     C***********************************************************************
790500930512     C     FORCON        BEGSR
790600930512     C*
790700931124     C                   Z-ADD     *ZEROS        D98RPI
790800931124     C                   Z-ADD     *ZEROS        D98RPC
790900930512     C                   Z-ADD     *ZEROS        D98RTI
791000930512     C                   Z-ADD     *ZEROS        D98RTC
791100930512     C                   Z-ADD     *ZEROS        D98RCI
791200930512     C                   Z-ADD     *ZEROS        D98RCC
791300930512     C                   Z-ADD     *ZEROS        D98RRI
791400930512     C                   Z-ADD     *ZEROS        D98RRC
791500931124     C                   Z-ADD     *ZEROS        D98FPI
791600931124     C                   Z-ADD     *ZEROS        D98FPC
791700930512     C                   Z-ADD     *ZEROS        D98FTI
791800930512     C                   Z-ADD     *ZEROS        D98FTC
791900930512     C                   Z-ADD     *ZEROS        D98FCI
792000930512     C                   Z-ADD     *ZEROS        D98FCC
792100930512     C                   Z-ADD     *ZEROS        D98FRI
792200930512     C                   Z-ADD     *ZEROS        D98FRC
792300931124     C                   MOVEL     'S'           D98CPI
792400931124     C                   MOVEL     'S'           D98CPC
792500930512     C                   MOVEL     'S'           D98CTI
792600930512     C                   MOVEL     'S'           D98CTC
792700930512     C                   MOVEL     'S'           D98CCI
792800930512     C                   MOVEL     'S'           D98CCC
792900930512     C                   MOVEL     'S'           D98CRI
793000930512     C                   MOVEL     'S'           D98CRC
793100931124     C                   Z-ADD     *ZEROS        D98NPI
793200931124     C                   Z-ADD     *ZEROS        D98NPC
793300930512     C                   Z-ADD     *ZEROS        D98NTI
793400930512     C                   Z-ADD     *ZEROS        D98NTC
793500930512     C                   Z-ADD     *ZEROS        D98NCI
793600930512     C                   Z-ADD     *ZEROS        D98NCC
793700930512     C                   Z-ADD     *ZEROS        D98NRI
793800930512     C                   Z-ADD     *ZEROS        D98NRC
793900930512     C                   Z-ADD     *ZEROS        D98HHI
794000930512     C                   Z-ADD     *ZEROS        D98HHC
794100930512     C                   Z-ADD     *ZEROS        D98HOI
794200930512     C                   Z-ADD     *ZEROS        D98HOC
794300930512     C                   Z-ADD     *ZEROS        D98HLI
794400930512     C                   Z-ADD     *ZEROS        D98HLC
794500930512     C                   Z-ADD     *ZEROS        D98HGI
794600930512     C                   Z-ADD     *ZEROS        D98HGC
794700931124     C                   Z-ADD     *ZEROS        D98HPI
794800931124     C                   Z-ADD     *ZEROS        D98HPC
794900930512     C                   Z-ADD     *ZEROS        D98HFI
795000930512     C                   Z-ADD     *ZEROS        D98HFC
795100930512     C                   Z-ADD     *ZEROS        D98HEI
795200930512     C                   Z-ADD     *ZEROS        D98HEC
795300930512     C                   Z-ADD     *ZEROS        D98HRI
795400930512     C                   Z-ADD     *ZEROS        D98HRC
795500930512     C                   Z-ADD     *ZEROS        D98HDI
795600930512     C                   Z-ADD     *ZEROS        D98HDC
795700930520     C*
795800930512     C                   ENDSR
795900930305     C********************************************************************
796000971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
796100950406031059090120151181212243273304334365
796200950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
796300950406312831303130313130313031
796400930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
796500971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
796600971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
796700971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
796800971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
796900971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
797000971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
797100971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
797200971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
797300971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
797400971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
797500971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
797600971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
797700971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
797800971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
797900971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
798000971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
798100971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
798200971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
798300971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
798400971127TNSD99R- Bolla inesistente.                                               BOIN20
798500971127TNSD99R-     Cap inesistente.                                             CAPI21
798600971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
798700971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
798800971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
798900971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
799000971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
799100971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
799200971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
799300971127TNSD99R- Contrassegno non trovato.                                        CONT29
799400971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
799500990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
799600990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
799700060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
799800060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
799900971127TNSD99R- 35                                                                   35
800000971127TNSD99R- 36                                                                   36
800100971127TNSD99R- 37                                                                   37
800200971127TNSD99R- 38                                                                   38
800300971127TNSD99R- 39                                                                   39
800400971127TNSD99R- 40                                                                   40
