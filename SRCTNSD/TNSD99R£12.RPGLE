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
094900160506     c                   else
095000160506     c                   eval       d98disf = 'F'
095100150325     c                   endif
095200150325     c                   endif
095300150325     c                   ENDSR
095400941227     C********************************************************************
095500941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
095600941227     C********************************************************************
095700941227     C     LETBLP        BEGSR
095800941227     C*
095900941227     C                   Z-ADD     D98AAS        BLPAAS
096000941227     C                   Z-ADD     D98LNP        BLPLNP
096100941227     C                   Z-ADD     D98NRS        BLPNRS
096200941227     C                   Z-ADD     D98NSP        BLPNSP
096300941227     C     KEYBLP        CHAIN     FNBLP01L                           99
096400941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
096500941227     C                   MOVEL     '1'           D98ERR
096600941227     C                   MOVEL     MSG(20)       D98MSG
096700941227     C                   END                                                    <-- 1 ---
096800941228     C     *IN99         IFEQ      '0'                                                    *TROV.
096900041028     C     KEYBLP        CHAIN     FNarb01L
097000041028     c                   if        %found(fnarb01l)
097100041028     c                   eval      wesarb='S'
097200041028     c                   else
097300041028     c                   eval      wesarb='N'
097400150219     c                   eval      wtrasm='D'
097500041028     c                   endif
097600141216     c
097700141216     c* se non trasmessa la bolla in arrivo memorizzo
097800150219     c                   if        blpft1= ' '
097900141229     c                   eval      wtrasm='N'
098000141216     c                   endif
098100141216     c
098200110923     C                   MOVEL     BLPxco        D98xco
098300110923     C                   MOVEL     BLPTSP        D98TSP
098400130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
098500130529     c                   if        i98tsp_for<>' '
098600130529     C                   MOVEL     i98tsp_for    D98TSP
098700130529     c                   endif
098800960209     C                   MOVEL     BLPNZD        D98NZD
098900941228     C                   MOVEL     BLPCAD        D98CAD
099000970701     C                   MOVEL     BLPLOD        D98LOD
099100941227     C                   Z-ADD     BLPTFP        D98TFP
099200941227     C                   Z-ADD     BLPTFA        D98TFA
099300941227     C                   Z-ADD     BLPLNA        D98LNA
099400941227     C                   Z-ADD     BLPAAS        DSAAS
099500941227     C                   Z-ADD     BLPMGS        DSMGS
099600941227     C                   Z-ADD     DSDAT8        D98DSP
099700941227     C                   Z-ADD     BLPMGS        D98MGS
099800051124     C                   movel     BLPTC1        D98TC1
099900051124     C                   movel     BLPTC2        D98TC2
100000041028     c                   if        wesarb='S'
100100041028     C                   Z-ADD     arbDTI        D98DTI
100200041028     C                   Z-ADD     arbHTI        D98HTI
100300041028     C                   Z-ADD     arbDCM        D98DCM
100400041028     C                   Z-ADD     arbHMC        D98HMC
100500041028     C                   Z-ADD     arbDAM        D98DAM
100600050901     C                   MOVEL     arbcca        d98cca
100700140820     C                   MOVEL     arbddc        d98ddc
100800140911     C                   z-add     arbndc        wd98ndc
100900140910     C                   MOVEL     arbfbc        wd98fbc
101000150119     c                   movel     arbacc        wd98acc
101100150325     c                   exsr      IN_DIST
101200140820     c*
101300041028     c                   else
101400941227     C                   Z-ADD     BLPDTI        D98DTI
101500941227     C                   Z-ADD     BLPHTI        D98HTI
101600941227     C                   Z-ADD     BLPDCM        D98DCM
101700941227     C                   Z-ADD     BLPHMC        D98HMC
101800041028     C                   Z-ADD     BLPDAM        D98DAM
101900050901     C                   MOVEL     BLPcca        d98cca
102000140820     C                   MOVEL     BLPddc        d98ddc
102100140825     C                   MOVEL     BLPndc        wd98ndc
102200140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
102300140820     c                   clear                   d98cons
102400041028     c                   endif
102500941227     C                   MOVEL     BLPGC1        D98GC1
102600941227     C                   MOVEL     BLPGC2        D98GC2
102700941227     C                   Z-ADD     BLPDCR        D98DCR
102800941227     C                   Z-ADD     BLPHCR        D98HCR
102900941227     C                   MOVEL     BLPTCR        D98TCR
103000100308     c* Salvo i campi perchè in alcuni casi li clearo
103100100308     C                   Z-ADD     blpDCR        s98DCR
103200100308     C                   Z-ADD     blpHCR        s98HCR
103300100308     C                   MOVEL     blpTCR        s98TCR
103400100308     c
103500941227     C                   MOVEL     *BLANKS       D98CMC
103600941227     C                   MOVEL     BLPFFD        D98FFD
103700941227     C                   Z-ADD     BLPNCD        D98NCD
103800941227     C                   MOVEL     BLPFNS        D98FNS
103900941228     C                   Z-ADD     BLPDUC        D98DUC
104000150508     C                   Z-ADD     BLPDPC        w98DPC
104100970310     C                   Z-ADD     *ZEROS        W98HUC
104200071122     c                   movel     blpcbo        d98cbo
104300150331     c*
104400150331     c* Se in fermo deposito vedo se presente particlarità consegna
104500150331     c                   if        blpffd='S' and blpgma<>*blanks
104600150331     C                   Z-ADD     CODUT         KTBUTE
104700150331     C                   MOVEL     '7R'          KTBCOD
104800150331     C                   MOVEL(P)  blpgma        KTBKEY
104900150331     C     KEYTAB        chain     TABEL00F
105000150331     c                   if        %found(tabel00f)
105100150331     c                   movel     tbluni        ds7r
105200150331     c                   endif
105300150331     c                   endif
105400150331     c
105500970304     C                   Z-ADD     BLPDBR        D98DBR
105600070514     c                   eval      d98fdrt=' '
105700070514     c
105800141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
105900070514     C                   Z-ADD     blpDrt        D98DBR
106000070514     c                   eval      d98fdrt='S'
106100141215     c********           endif
106200070514     c*
106300941227     C                   MOVEL     BLPFIN        D98FIN
106400130826     C****               Z-ADD     BLPKSC        D98KSC
106500990308     C                   MOVEL     BLPFBR        WFBR
106600031201     C                   MOVEL     BLPGGA        WGGA
106700941227     C                   Z-ADD     1             I
106800941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
106900941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
107000941227     C                   MOVEL     '1'           D98ERR
107100941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
107200941227     C                   ELSE                                                   <-- 2 -->
107300941227     C                   MOVEL     T3A(I)        D98TBL
107400941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
107500990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
107600941227     C                   END                                                    <-- 2 ---
107700130826     c                   if        %subst(d98tbl:1:1) ='F'
107800130826     C                   Z-ADD     blpKSC        D98KSC
107900130826     c                   else
108000130826     c
108100130826     c                   if        blpccm>0
108200130826     C                   Z-ADD     blpccm        D98KSC
108300130826     c                   endif
108400130826     c                   if        d98ksc=0
108500130826     c                   z-add     8888          d98ksc
108600130826     c                   movel     d98lnp        d98ksc
108700130826     c                   endif
108800130826     c                   endif
108900130826     c
109000941227     C                   END
109100941228     C*
109200051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
109300950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
109400941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
109500941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
109600051115     C                   Z-ADD     D98AAS        ar9AAS
109700051115     C                   Z-ADD     D98LNP        ar9LNP
109800051115     C                   Z-ADD     D98NRS        ar9NRS
109900051115     C                   Z-ADD     D98NSP        ar9NSP
110000051115     C     KEYar9        CHAIN     Fiar901L                           99
110100950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
110200950102     C                   MOVEL     'S'           WFLASS
110300950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
110400950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
110500950102     C                   Z-ADD     D98AAS        LBLAAN
110600950102     C                   Z-ADD     D98LNP        LBLLPN
110700950102     C                   Z-ADD     D98NRS        LBLNRN
110800950102     C                   Z-ADD     D98NSP        LBLNSN
110900950102     C     KEYLBL        CHAIN     FNLBL01L                           99
111000950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
111100051115     C                   Z-ADD     LBLAAP        ar9AAS
111200051115     C                   Z-ADD     LBLLPP        ar9LNP
111300051115     C                   Z-ADD     LBLNRP        ar9NRS
111400051115     C                   Z-ADD     LBLNSP        ar9NSP
111500051115     C     KEYar9        CHAIN     Fiar901L                           99
111600950102     C     *IN99         IFEQ      '0'
111700950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
111800950102     C                   ENDIF
111900950102     C                   ENDIF                                                  <-- 3 ---
112000950102     C                   ENDIF                                                  <-- 2 ---
112100950102     C*
112200950102     C* CONTRASSEGNO TROVATO
112300150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
112400051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
112500051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
112600051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
112700941228     C                   ENDIF                                                  <-- 2 ---
112800941228     C                   ENDIF                                                  <-- 1 ---
112900941227     C*
113000050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
113100941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
113200050405     C                   Z-ADD     D98AAS        KWGCPAAS
113300050405     C                   Z-ADD     D98LNP        KWGCPLNP
113400050405     C                   Z-ADD     D98NRS        KWGCPNRS
113500050405     C                   Z-ADD     D98NSP        KWGCPNSP
113600050405     C                   Z-ADD     *ZEROS        KWGCPFRG
113700050405     C     KEYGCP        CHAIN     TIGCP01L                           99
113800941228     C     *IN99         IFEQ      '0'                                          *TROV.
113900941227     C     GCPATB        ANDNE     'A'
114000960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
114100960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
114200960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
114300960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
114400050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
114500960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
114600960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
114700050405     C* O UGUALE A 35 INVECE DI 40)
114800960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
114900050405     C     GCPFAS        ANDGE     40
115000941227     C                   Z-ADD     GCPDCG        D98DCG
115100950112     C                   Z-ADD     GCPDCG        W98DCG
115200960123     C                   ENDIF
115300960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
115400960729     C     GCPFAS        ANDGE     35
115500070202     C     GCPFAS        ANDne     36
115600960729     C                   Z-ADD     GCPDCG        D98DCG
115700960729     C                   Z-ADD     GCPDCG        W98DCG
115800960729     C                   ENDIF
115900941228     C                   ENDIF
116000941228     C                   ENDIF                                                  <-- 1 ---
116100970912     C*
116200970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
116300970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
116400970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
116500050922     C******             Z-ADD     D98DTI        W98DTI
116600971210     C                   Z-ADD     D98LNP        F
116700971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
116800970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
116900970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
117000150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
117100150120     c                   if        blpdcp=0 and d98dfc=0
117200150120     C                   Z-ADD     d98dam        D98DTI
117300150120     c                   endif
117400150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
117500150120     C                   Z-ADD     d98dam        D98DTI
117600150120     c                   endif
117700150120     c                   ENDIF
117800150120     c*
117900150120     C***                Z-ADD     D98AAS        BLTAAS
118000150120     C****               Z-ADD     D98LNP        BLTLNP
118100150120     C***                Z-ADD     D98NRS        BLTNRS
118200150120     C***                Z-ADD     D98NSP        BLTNSP
118300150120     C***  KEYBLT        SETLL     FNBLT01L                           99
118400150120     C**N99KEYBLT        READE     FNBLT01L                               99
118500150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
118600150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
118700970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
118800150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
118900970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
119000970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
119100150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
119200150120     C**   BLTDCM        ANDEQ     *ZEROS
119300970912     C* SE PRIMO REC. TROVATO, MEM. DATA
119400150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
119500150120     C**                 Z-ADD     BLTDAM        D98DTI
119600150120     C**                 ELSE                                                   <-- 6 -->
119700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
119800150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
119900150120     C**                 Z-ADD     BLTDAM        D98DTI
120000150120     C**                 ENDIF                                                  <-- 7 ---
120100150120     C**                 ENDIF                                                  <-- 6 ---
120200150120     C**                 ENDIF                                                  <-- 5 ---
120300970912     C* C'E' DATA CONSEGNAB.
120400150120     **                  ELSE                                                   <-- 4 -->
120500970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
120600970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
120700150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
120800150120     C**   BLTDAM        ANDLT     D98DFC
120900970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
121000970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
121100150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
121200150120     C**   BLTDCM        ORGT      D98DFC
121300970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
121400150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
121500150120     C**                 Z-ADD     BLTDAM        D98DTI
121600150120     C**                 ELSE                                                   <-- 7 -->
121700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
121800150120     C**   BLTDAM        IFLT      D98DTI
121900150120     C**                 Z-ADD     BLTDAM        D98DTI
122000150120     C**                 ENDIF
122100150120     C**                 ENDIF                                                  <-- 7 ---
122200150120     C**                 ENDIF                                                  <-- 6 ---
122300150120     C**                 ENDIF                                                  <-- 5 ---
122400150120     C**                 ENDIF                                                  <-- 4 ---
122500150120     C**                 ENDIF                                                  <-- 3 ---
122600150120     C**   KEYBLT        READE     FNBLT01L                               99
122700150120     C**                 ENDDO                                                  <-- 2 ---
122800150120     C**                 ENDIF                                                  <-- 1 ---
122900941227     C*
123000941227     C                   ENDSR
123100930323     C********************************************************************
123200941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
123300930323     C********************************************************************
123400930323     C     RTTEOR        BEGSR
123500941227     C*
123600941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
123700941228     C     D98ERR        IFEQ      '0'
123800941227     C                   EXSR      RTPART
123900941227     C                   ENDIF
124000941227     C*
124100941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
124200941228     C     D98ERR        IFEQ      '0'
124300150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
124400150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
124500150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
124600150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
124700150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
124800941227     C                   EXSR      RTTRAZ
124900150219     c                   if        d98err<>'1'
125000150219     C                   Z-ADD     CASTIN        D98TTI
125100150219     C                   Z-ADD     CASTCN        D98TTC
125200150219     C                   endif
125300150219     C                   ENDIF
125400941227     C*
125500941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
125600941228     C     D98ERR        IFEQ      '0'
125700941227     C                   EXSR      RTCONS
125800941227     C                   ENDIF
125900941227     C*
126000941228     C     D98ERR        IFEQ      '0'
126100941227     C*
126200941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
126300941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
126400941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
126500090921     c                   z-add     d98tri        w99tri
126600090921     c* per la DS
126700090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
126800090921     c* per i calcoli interi
126900090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
127000090921     c
127100941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
127200941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
127300941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
127400090921     c                   z-add     d98trc        w99trc
127500090921     c* per la DS
127600090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
127700090921     c* per i calcoli interi
127800090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
127900941227     C*
128000941227     C                   ENDIF
128100930323     C*
128200930323     C                   ENDSR
128300941227     C********************************************************************
128400941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
128500941227     C********************************************************************
128600941227     C     RTPART        BEGSR
128700941228     C*
128800941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
128900941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
129000941228     C                   MOVEL     D98TSP        WTSP              1
129100130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
129200130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
129300050301     C*******
129400050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
129500050301     C*
129600050301     C*    W001          IFEQ      'A'
129700050301     C*    W0040         OREQ      8888
129800050301     C*                  MOVEL     'E'           WTSP
129900050301     C*                  ENDIF
130000050301     C*******
130100941227     C*
130200941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
130300941227     C                   MOVEL     'N'           WTRAOK            1
130400941227     C                   MOVEL     'P'           CASPTA
130500941227     C                   Z-ADD     D98LNP        CASLNP
130600941227     C                   Z-ADD     *ZEROS        CASLNA
130700941228     C                   MOVEL     WTSP          CASTSP
130800941228     C                   Z-ADD     99999999      CASDDE
130900941227     C     KEYCAS        SETGT     AZCAS01L                             99
131000941227     C  N99              READP     AZCAS01L                               99
131100941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
131200941227     C     WTRAOK        ANDEQ     'N'
131300941227     C     CASLNP        ANDEQ     D98LNP
131400950102     C     CASTSP        ANDEQ     WTSP
131500941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
131600941227     C     CASATB        ANDEQ     *BLANKS
131700941227     C                   MOVEL     'S'           WTRAOK
131800941227     C                   ELSE                                                   <-- 2 -->
131900941227     C                   READP     AZCAS01L                               99
132000941227     C                   ENDIF                                                  <-- 2 ---
132100941227     C                   ENDDO                                                  <-- 1 ---
132200941227     C*
132300941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
132400941227     C     WTRAOK        IFEQ      'N'
132500941227     C                   MOVEL     '1'           D98ERR
132600941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
132700941227     C                   ELSE
132800941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
132900941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
133000941227     C                   END
133100970214     C*
133200140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
133300140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
133400140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
133500140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
133600140403     C**                 Z-ADD     D98LNA        F
133700140403     C**   D98ERR        IFEQ      '0'
133800140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
133900140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
134000140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
134100140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
134200140403     C**                 ENDIF
134300140403     C**                 ENDIF
134400140402     c
134500140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
134600140402     c*  se tempi  = 0
134700140402     c                   if        wpick2='S' and d98tpi<=0
134800140402     c                   add       24            D98TPI
134900140402     c                   endif
135000941227     C*
135100941227     C                   ENDSR
135200941227     C********************************************************************
135300941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
135400941227     C********************************************************************
135500941227     C     RTTRAZ        BEGSR
135600941227     C*
135700941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
135800150219     c
135900150219     C                   Z-ADD     D98LNA        LTA(1)
136000150219     C                   Z-ADD     D98LNA        LTA(2)
136100150219     C                   Z-ADD     D98TFA        LTA(3)
136200941227     C                   Z-ADD     D98TFA        LTA(4)
136300941227     C*
136400941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
136500941227     C                   MOVEL     'N'           WTRAOK            1
136600941227     C                   Z-ADD     1             K                 3 0
136700941227     C*
136800941227     C* CICLO FINO A:
136900941227     C*    - FINE SCHIERA
137000941227     C*    - TROVATO TEMPI VALIDI
137100941227     C     K             DOUGT     4                                            --- 1 -->
137200941227     C     WTRAOK        OREQ      'S'
137300941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
137400941227     C     LTA(K)        ORNE      *ZEROS
137500941227     C                   MOVEL     'T'           CASPTA
137600941227     C                   Z-ADD     LTP(K)        CASLNP
137700941227     C                   Z-ADD     LTA(K)        CASLNA
137800941227     C                   MOVEL     D98TSP        CASTSP
137900941228     C                   Z-ADD     99999999      CASDDE
138000941227     C     KEYCAS        SETGT     AZCAS01L                             99
138100941227     C  N99              READP     AZCAS01L                               99
138200941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
138300941227     C     WTRAOK        ANDEQ     'N'
138400941227     C     LTP(K)        ANDEQ     CASLNP
138500941227     C     LTA(K)        ANDEQ     CASLNA
138600941227     C     D98TSP        ANDEQ     CASTSP
138700960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
138800941227     C     CASATB        ANDEQ     *BLANKS
138900941227     C                   MOVEL     'S'           WTRAOK
139000941227     C                   ELSE                                                   <-- 4 -->
139100941227     C                   READP     AZCAS01L                               99
139200941227     C                   ENDIF                                                  <-- 4 ---
139300941227     C                   ENDDO                                                  <-- 3 ---
139400941227     C                   ENDIF                                                  <-- 2 ---
139500941227     C                   ADD       1             K
139600941227     C                   ENDDO                                                  <-- 1 ---
139700941227     C*
139800941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
139900941227     C     WTRAOK        IFEQ      'N'
140000941227     C                   MOVEL     '1'           D98ERR
140100941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
140200150219     C***                ELSE
140300150219     C***                Z-ADD     CASTIN        D98TTI
140400150219     C***                Z-ADD     CASTCN        D98TTC
140500941227     C                   END
140600941227     C*
140700941227     C                   ENDSR
140800941227     C********************************************************************
140900941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
141000941227     C********************************************************************
141100941227     C     RTCONS        BEGSR
141200150629     c                   clear                   WTsogliaE         1
141300970701     C*
141400970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
141500970701     C                   CLEAR                   SI95
141600970701     C                   MOVEL     '3'           I95TCN
141700970701     C                   MOVEL     D98NZD        I95NAR
141800970701     C                   MOVEL     D98CAD        I95CAP
141900970701     C                   MOVEL     D98LOD        I95LOC
142000970701     C                   MOVEL     D98DSP        I95DAT
142100970701     C                   MOVEL     D98TSP        I95TSP
142200980602     C                   MOVEL     'S'           I95FRT
142300150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
142400150511     c                   if        d98ffd='S'
142500150511     c                   eval      i95tsp='C'
142600150511     c                   endif
142700020226     C*
142800020226     C* VERIFICO SE LNA E' FILIALE FEDEX
142900020226     C                   IF        D98LNA > *ZEROS
143000020226     C     D98LNA        LOOKUP    FILFED                                 55
143100020226     C                   IF        *IN55 = *ON
143200020226     C                   EVAL      I95FI1 = 'S'
143300020226     C                   ENDIF
143400020226     C                   ENDIF
143500020226     C*
143600020226     C                   CALL      'TISI95R'
143700970701     C                   PARM                    SI95
143800941227     C*
143900941228     C* SE NON TROVATO, ERRORE
144000970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
144100941227     C                   MOVEL     '1'           D98ERR
144200970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
144300941227     C                   ELSE                                                   <-- 1 -->
144400941227     C*
144500941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
144600941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
144700941227     C     CASTIN        DIV       24            WDIV              3 0
144800941227     C                   MVR                     WRES              3 0
144900941227     C     WRES          IFEQ      *ZEROS
145000970701     C                   Z-ADD     O950IN        D98TCI
145100941227     C                   ELSE
145200970701     C                   Z-ADD     O951IN        D98TCI
145300941227     C                   END
145400941227     C*
145500941227     C     CASTCN        DIV       24            WDIV                     99
145600941227     C                   MVR                     WRES
145700941227     C     WRES          IFEQ      *ZEROS
145800970701     C                   Z-ADD     O950CN        D98TCC
145900150629     c                   eval      wTSogliaE='0'
146000941227     C                   ELSE
146100970701     C                   Z-ADD     O951CN        D98TCC
146200150629     c                   eval      wTSogliaE='1'
146300941227     C                   END
146400941227     C*
146500150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
146600941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
146700150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
146800150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
146900150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
147000150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
147100150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
147200941227     C*
147300941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
147400941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
147500970701     C     O95ISO        IFEQ      'I'
147600941227     C     D98FIN        OREQ      'I'
147700970701     C                   ADD       O95TIS        D98TCI
147800970701     C                   ADD       O95TIS        D98TCC
147900941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
148000970701     C                   ADD       O95TIS        W990IN
148100970701     C                   ADD       O95TIS        W991IN
148200150629     C                   ADD       O95TIS        W990CN
148300150629     C                   ADD       O95TIS        W991CN
148400941227     C                   END
148500090827     c
148600090827     c* Sistemo i tempi di consegna se non sono multipli di 12
148700090827     c                   exsr      SisCONS
148800160404     c
148900160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
149000160404     c                   if        wVP='S'
149100160404     c                   add       24            w991IN
149200160404     c                   add       24            w991CN
149300160404     c                   add       24            w990IN
149400160404     c                   add       24            w990CN
149500160404     c                   endif
149600941228     C*
149700941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
149800941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
149900941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
150000941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
150100941228     C                   Z-ADD     D98TCC        W99CCE            3 0
150200941228     C                   ELSE                                                   <-- 3 -->
150300970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
150400970701     C                   CLEAR                   SI95
150500970701     C                   MOVEL     '3'           I95TCN
150600970701     C                   MOVEL     D98NZD        I95NAR
150700970701     C                   MOVEL     D98CAD        I95CAP
150800970701     C                   MOVEL     D98LOD        I95LOC
150900970701     C                   MOVEL     D98DSP        I95DAT
151000970701     C                   MOVEL     'E'           I95TSP
151100980602     C                   MOVEL     'S'           I95FRT
151200020226     C*
151300020226     C* VERIFICO SE LNA E' FILIALE FEDEX
151400020226     C                   IF        D98LNA > *ZEROS
151500020226     C     D98LNA        LOOKUP    FILFED                                 55
151600020226     C                   IF        *IN55 = *ON
151700020226     C                   EVAL      I95FI1 = 'S'
151800020226     C                   ENDIF
151900020226     C                   ENDIF
152000020226     C*
152100020226     C                   CALL      'TISI95R'
152200970701     C                   PARM                    SI95
152300970701     C*
152400941228     C* SE NON TROVATO, ERRORE
152500970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
152600941228     C                   MOVEL     '1'           D98ERR
152700970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
152800941228     C                   ELSE                                                   <-- 4 -->
152900941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
153000941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
153100941228     C     CASTCN        DIV       24            WDIV                     99
153200941228     C                   MVR                     WRES
153300941228     C     WRES          IFEQ      *ZEROS
153400970701     C                   Z-ADD     O950CN        W99CCE
153500941228     C                   ELSE
153600970701     C                   Z-ADD     O951CN        W99CCE
153700941228     C                   END
153800941228     C                   ENDIF                                                  <-- 4 ---
153900941228     C                   ENDIF                                                  <-- 3 ---
154000941228     C                   ENDIF                                                  <-- 2 ---
154100941227     C*
154200941227     C                   END                                                    <-- 1 ---
154300941227     C*
154400941227     C                   ENDSR
154500090827     C********************************************************************
154600090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
154700090827     C********************************************************************
154800090827     C     SisCONS       BEGSR
154900090827     c                   eval      w99tci=d98tci
155000090827     c                   if        d98tci=10
155100090827     c                   z-add     12            w99tci
155200090827     c                   endif
155300090827     c
155400090827     c                   eval      w99tcc=d98tcc
155500090827     c                   if        d98tcc=10
155600090827     c                   z-add     12            w99tcc
155700090827     c                   endif
155800160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
155900160404     c                   if        wVP='S'
156000160404     c                   add       24            w99tcc
156100160404     c                   add       24            w99tci
156200160404     c                   endif
156300090827     c
156400090827     C                   ENDSR
156500930420     C********************************************************************
156600930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
156700930420     C********************************************************************
156800930420     C     CAREVB        BEGSR
156900930420     C*
157000930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
157100930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
157200060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
157300930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
157400930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
157500060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
157600060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
157700930420     C                   Z-ADD     D98AAS        KEBAAS
157800930420     C                   Z-ADD     D98LNP        KEBLNP
157900930420     C                   Z-ADD     D98NRS        KEBNRS
158000930420     C                   Z-ADD     D98NSP        KEBNSP
158100990810     C     KEYEVB        SETLL     FNEVB01L                           99
158200060406     C  N99keyevb        READe     FNEVB01L                               99
158300060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
158400930421     C     D98ERR        ANDEQ     '0'
158500060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
158600930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
158700930421     C                   Z-ADD     1             I
158800930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
158900060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
159000930421     C                   MOVEL     '1'           D98ERR
159100930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
159200060406   x3C                   ELSE                                                   <-- 3 -->
159300060406     C                   MOVEL     D2A(I)        usa2A
159400060406     C                   MOVEL     D2Z(I)        usa2Z
159500930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
159600060406    4C     §2AFTC        IFEQ      'A'
159700930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
159800060406    4C                   END
159900060406    4C     §2AFTC        IFEQ      'G'
160000930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
160100060406    4C                   END
160200150216     c* Se si tratta di MIC verifico se uguale alla sped.
160300150303    4c                   if        evbcev='MIC' or evbcev='NIC'
160400150303    5c                   if        %subst(evbnot:1:7) >*zeros
160500150303     c
160600150303    6c                   if        evbcev='MIC' and
160700150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
160800150216     c                   eval      evemic='SI'
160900150303    6c                   endif
161000150303     c
161100150303    6c                   if        evbcev='NIC' and
161200150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
161300150216     c                   eval      evemic='  '
161400150303    6c                   endif
161500150303     c
161600150303   x5c                   else
161700150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
161800150303     c                   eval      evemic='SI'
161900150303    6c                   endif
162000150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
162100150303     c                   eval      evemic='  '
162200150303    6c                   endif
162300150303
162400150303    5c                   endif
162500150303    4c                   endif
162600150216     c
162700060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
162800930421     C     §2AFTC        OREQ      'G'
162900930421     C                   ADD       1             IE
163000990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
163100930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
163200930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
163300131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
163400090928     c
163500090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
163600091014     c**                 if        ie=1
163700091014     c**                 eval      Primotipo=§2aftc
163800091014     c**                 eval      Primodev=evbdev
163900091014     c**                 eval      Primohev=evbhev
164000091014     c**                 endif
164100090928     c
164200060406    4C                   END                                                    <-- 4 ---
164300060406     c* Verifico se si tratta di evento particolare
164400060406    4c                   if        §2zCEP<>' '
164500060526     c* non carico l'evento se cade in giorno festivo
164600060526     c                   z-add     evbdev        wpodata
164700060526     C                   EVAL      WTC='C'
164800060526     C                   EVAL      WIC='C'
164900060526     c                   eval      wslitt='N'
165000060526     c                   exsr      chkfes
165100060526     c
165200060526     c* Wslitt='N' non festivo
165300060526   4ac                   if        wslitt='N'
165400060526     c
165500060406    5c                   if        §2zfep='A'
165600060406     c                   eval      wevpaA='S'
165700060406    5C                   END                                                    <-- 3 ---
165800060406    5c                   if        §2zfep='G'
165900060406     c                   eval      wevpaG='S'
166000060406    5C                   END                                                    <-- 3 ---
166100060406     c                   add       1             ip
166200060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
166300060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
166400060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
166500060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
166600060526   4aC                   ENDif                                                  <-- 3 ---
166700060526    4C                   END                                                    <-- 3 ---
166800060406    3C                   END                                                    <-- 3 ---
166900060406    2C                   END                                                    <-- 2 ---
167000060406     C     keyevb        READe     FNEVB01L                               99
167100060406    1C                   ENDDO                                                  <-- 1 ---
167200050523     c* controllo riempimento skiere
167300050523     c                   clear                   trul0sds
167400050523     c                   eval      i0sski='EVD/EVO/EVC'
167500050523     c                   eval      i0sele=%elem(EVD)
167600050523     c                   eval      i0spie=ie
167700050523     c                   eval      i0sfile='FNEVB01L  '
167800050523     c                   eval      i0scod='     '
167900050523     c                   eval      i0spgm='TNSD99R'
168000050523     c                   movel     trul0sds      kpjbu
168100050523     c                   call      'TRUL0SR'
168200050523     c                   parm                    kpjba
168300930420     C*
168400060406     c* controllo riempimento skiere
168500060406     c                   clear                   trul0sds
168600060406     c                   eval      i0sski='PVD/PVO/PVC'
168700060406     c                   eval      i0sele=%elem(PVD)
168800060406     c                   eval      i0spie=ie
168900060406     c                   eval      i0sfile='FNEVB01L  '
169000060406     c                   eval      i0scod='     '
169100060406     c                   eval      i0spgm='TNSD99R'
169200060406     c                   movel     trul0sds      kpjbu
169300060406     c                   call      'TRUL0SR'
169400060406     c                   parm                    kpjba
169500060406     C*
169600930420     C                   ENDSR
169700931213     C********************************************************************
169800931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
169900931213     C********************************************************************
170000931213     C     CARTUC        BEGSR
170100931213     C*
170200931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
170300931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
170400931214     C     D98GC2        ANDEQ     *BLANKS
170500931213     C                   ELSE                                                   <-- 1 -->
170600931213     C*
170700931213     C                   MOVE      D98GC1        TUC(1)
170800931214     C                   MOVE      D98GC2        TUC(2)
170900931213     C*
171000931213     C                   END                                                    <-- 1 ---
171100931213     C*
171200931213     C                   ENDSR
171300970304     C********************************************************************
171400970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
171500970304     C********************************************************************
171600970304     C     DATRIF        BEGSR
171700970304     C*
171800050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
171900050922     c*  partenza
172000970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
172100970304     C*
172200970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
172300070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
172400070605     C***                MOVEL     '1'           D98ERR
172500070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
172600070605     C***                ELSE                                                   <-- 2 -->
172700970304     C*
172800970304     C                   Z-ADD     D98DRF        WD1
172900970304     C                   Z-ADD     D98ORF        WO1
173000970310     C* DATA PARTENZA ULTIMO COLLO
173100970310     C     D98DRF        IFLE      D98DUC
173200050928     c     d98duc        oreq      0
173300970310     C                   Z-ADD     D98DRF        D98DUC
173400970310     C                   Z-ADD     D98ORF        W98HUC
173500070605     C                   ENDIF
173600970304     C* DATA ARRIVO TRAZIONE
173700050922     C***                Z-ADD     D98DTI        WD2
173800050922     C***                Z-ADD     D98HTI        WO2
173900050922     C***  WDO1          IFLE      WDO2
174000050922     C***                Z-ADD     D98DRF        D98DTI
174100050922     C***                Z-ADD     D98ORF        D98HTI
174200050922     C***                ENDIF
174300970304     C* DATA CONSEGNA
174400050922     C***                Z-ADD     D98DCM        WD2
174500050922     C***                Z-ADD     D98HMC        WO2
174600050922     C***  WDO1          IFLE      WDO2
174700050922     C***                Z-ADD     D98DRF        D98DCM
174800050922     C***                Z-ADD     D98ORF        D98HMC
174900050922     C***                ENDIF
175000970304     C*
175100070605     C***                ENDIF                                                  <-- 2 ---
175200970304     C                   ENDIF                                                  <-- 1 ---
175300970304     C*
175400970304     C                   ENDSR
175500940901     C********************************************************************
175600940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
175700940901     C********************************************************************
175800940901     C     TDTIPO        BEGSR
175900940901     C*
176000940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
176100940902     C                   Z-ADD     D98TTI        W98TTI            3 0
176200941011     C*
176300941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
176400941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
176500941011     C     W98TTI        DIV       24            WDIV              3 0
176600941011     C                   MVR                     WRES              3 0
176700941011     C     WRES          IFEQ      *ZEROS
176800941011     C                   Z-ADD     *ZEROS        WASTA             9 0
176900941011     C                   ELSE
177000941011     C                   Z-ADD     12            WASTA
177100941011     C                   END
177200940901     C*
177300940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
177400940901     C     D98DTI        IFNE      *ZEROS
177500940901     C                   Z-ADD     D98DTI        WDAT
177600940901     C                   EXSR      DATORE
177700960911     C                   Z-ADD     WORE          WPODTI
177800960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
177900960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
178000960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
178100960911     C                   Z-ADD     WPODTI        W10POI            9 0
178200960911     C                   EXSR      R10FES
178300960911     C                   Z-ADD     W10POO        WPODTI
178400960911     C* AGGIUNGO LE ORA D'ASTA
178500960911     C                   ADD       WASTA         WPODTI
178600960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
178700960911     C     W10POI        IFEQ      W10POO
178800940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
178900940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
179000940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
179100940901     C                   EXSR      RO3ORE
179200940901     C                   ADD       WR3HHO        WPODTI
179300940901     C                   ADD       WR3HHO        W98TTI
179400940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
179500940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
179600940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
179700940901     C                   EXSR      RO3ORE
179800940901     C                   ADD       WR3HHO        WPODTI
179900940901     C                   ADD       WR3HHO        W98TTI
180000960911     C                   ENDIF                                                  *SE NON FESTIVO
180100940901     C                   END
180200940901     C*
180300940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
180400940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
180500940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
180600940901     C     W98TTI        DIV       24            WDIV              3 0
180700940901     C                   MVR                     WRES              3 0
180800940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
180900941228     C                   Z-ADD     W990IN        D98TCI
181000160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
181100160405     c*  per cui le sottraggo, per riagguingerle alla fine
181200160405     c                   if        wVP='S'
181300160405     c                   sub       24            D98TCI
181400160405     c                   endif
181500160405     c
181600150630     c                   eval      wTSogliaE='0'
181700940901     C                   ELSE                                                   <-- 2 -->
181800941228     C                   Z-ADD     W991IN        D98TCI
181900160405     c                   if        wVP='S'
182000160405     c                   sub       24            D98TCI
182100160405     c                   endif
182200150630     c                   eval      wTSogliaE='1'
182300940908     C                   ENDIF                                                  <-- 2 ---
182400090827     c* Sistemo i tempi di consegna se non sono multipli di 12
182500090827     c                   exsr      SisCONS
182600090827     c*
182700940908     C* RICALCOLO TEMPO DI RESA INTERNO
182800950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
182900950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
183000090921     c                   z-add     d98tri        w99tri
183100090921     c* per la DS
183200090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
183300090921     c* per i calcoli interni
183400090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
183500940908     C                   ENDIF                                                  <-- 1 ---
183600940901     C*
183700940901     C                   ENDSR
183800930325     C********************************************************************
183900930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
184000930325     C********************************************************************
184100930325     C     TDATPO        BEGSR
184200930406     C*
184300930420     C* TRASFORMO LA DATA ODIERNA
184400930416     C                   Z-ADD     WDAODI        WDAT
184500930416     C                   EXSR      DATORE
184600050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
184700960911     C                   MOVEL     'OSC  '       WR3TAB
184800960911     C                   Z-ADD     WHHODI        WR3HHI
184900960911     C                   EXSR      RO3ORE
185000050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
185100050922     c                   if        wr3hho=12
185200050922     c     wpoatt        add       12            wpoattdata24
185300050922     c                   else
185400050922     c                   z-add     wpoatt        wpoattdata24
185500050922     c                   endif
185600930325     C*
185700930420     C* TRASFORMO LA DATA DI SPEDIZIONE
185800931125     C     D98DSP        IFEQ      *ZEROS
185900930326     C                   MOVEL     '1'           D98ERR
186000930326     C                   MOVEL     MSG(2)        D98MSG
186100930326     C                   ELSE
186200931125     C                   Z-ADD     D98DSP        WDAT
186300930325     C                   EXSR      DATORE
186400930406     C                   Z-ADD     WORE          WPODSP
186500930326     C                   END
186600930414     C*
186700930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
186800930414     C                   MOVEL     'LIM  '       WR3TAB
186900930414     C                   Z-ADD     *ZEROS        WR3HHI
187000930414     C                   EXSR      RO3ORE
187100931222     C     WR3HHO        MULT      10            WTM060            6 0
187200931222     C     WPODSP        ADD       WTM060        WPOLIM
187300140822     c
187400930325     C*
187500930420     C* TRASFORMO LA DATA DI CONSEGNA
187600931125     C     D98DCM        IFNE      *ZEROS
187700931125     C                   Z-ADD     D98DCM        WDAT
187800930325     C                   EXSR      DATORE
187900930326     C                   Z-ADD     WORE          WPODCM
188000930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
188100930326     C                   MOVEL     'OSC  '       WR3TAB
188200931125     C                   Z-ADD     D98HMC        WR3HHI
188300930326     C                   EXSR      RO3ORE
188400930326     C                   ADD       WR3HHO        WPODCM
188500930326     C                   END
188600941228     C*
188700941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
188800941228     C     D98DUC        IFNE      *ZEROS
188900941228     C                   Z-ADD     D98DUC        WDAT
189000941228     C                   EXSR      DATORE
189100941228     C                   Z-ADD     WORE          WPODUC
189200970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
189300970310     C                   MOVEL     'OSP  '       WR3TAB
189400970310     C                   Z-ADD     W98HUC        WR3HHI
189500970310     C                   EXSR      RO3ORE
189600970310     C     D98DRF        IFNE      *ZEROS
189700050928     c     w98huc        andgt     1200
189800970310     C                   SUB       12            WR3HHO
189900970310     C                   ENDIF
190000970310     C                   ADD       WR3HHO        WPODUC
190100941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
190200070605     C***  WPODSP        IFGT      WPODUC
190300070605     C***                Z-ADD     WPODSP        WPODUC
190400070605     C***                ENDIF
190500941228     C                   ENDIF
190600960402     C*
190700141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
190800970304     C     D98DBR        IFNE      *ZEROS
190900970304     C                   Z-ADD     D98DBR        WDAT
191000960402     C                   EXSR      DATORE
191100960402     C                   Z-ADD     WORE          WPODBR
191200070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
191300960402     C                   ELSE
191400960402     C                   Z-ADD     WPODSP        WPODBR
191500960402     C                   ENDIF
191600930913     C*
191700960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
191800930913     C* CONSEGNABILITA'
191900960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
192000950407     C*
192100930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
192200931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
192300931125     C                   Z-ADD     D98DIC        WDAT
192400930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
192500930913     C                   Z-ADD     WORE          WPODIC
192600930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
192700931125     C                   Z-ADD     D98OIC        WR3HHI
192800930913     C                   EXSR      RO3ORE
192900930913     C                   ADD       WR3HHO        WPODIC
193000931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
193100950407     C*
193200930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
193300931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
193400931125     C                   Z-ADD     D98DFC        WDAT
193500930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
193600930913     C                   Z-ADD     WORE          WPODFC
193700140822     C                   Z-ADD     WORE          WPODFC_00
193800930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
193900931125     C                   Z-ADD     D98OFC        WR3HHI
194000930913     C                   EXSR      RO3ORE
194100930913     C                   ADD       WR3HHO        WPODFC
194200931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
194300930913     C*
194400930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
194500930913     C     D98ERR        IFEQ      '0'
194600930913     C     WPODFC        IFEQ      *ZEROS
194700930913     C     WPODIC        ANDNE     *ZEROS
194800930913     C                   MOVEL     '1'           D98ERR
194900930913     C                   MOVEL     MSG(17)       D98MSG
195000930913     C                   ENDIF
195100930913     C                   ENDIF
195200930907     C*
195300930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
195400930913     C     D98ERR        IFEQ      '0'
195500930913     C     WPODIC        IFGT      WPODFC
195600930907     C                   MOVEL     '1'           D98ERR
195700930913     C                   MOVEL     MSG(18)       D98MSG
195800930913     C                   ENDIF
195900930913     C                   ENDIF
196000950407     C*
196100950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
196200930511     C*
196300930511     C                   ENDSR
196400950110     C********************************************************************
196500950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
196600950110     C********************************************************************
196700950110     C     CALDPP        BEGSR
196800950110     C*
196900950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
197000950110     C                   MOVEL     'P'           WTC                            *PARTENZA
197100950110     C                   MOVEL     'I'           WIC                            *INTERNA
197200960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
197300950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
197400950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
197500950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
197600950110     C*
197700950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
197800950110     C                   MOVEL     'P'           WTC                            *PARTENZA
197900950110     C                   MOVEL     'C'           WIC                            *CLIENTE
198000950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
198100950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
198200950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
198300950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
198400960229     C*
198500960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
198600050922     c
198700050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
198800050922     c
198900050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
199000090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
199100050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
199200090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
199300050922%%%  C*  ritardo alla partenza.
199400050922     c* Il tutto solo se la data di oggi > della
199500050922     c*  data teorica di partenza determinata
199600050922     c*
199700950110%%%  C     D98ERR        IFEQ      '0'
199800950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
199900950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
200000950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
200100050922     c     wpoattData24  andgt     wpoppi
200200960911%%%  C********             MOVEL'OSC  '   WR3TAB
200300960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
200400960911%%%  C********             EXSR RO3ORE
200500960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
200600050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
200700090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
200800090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
200900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
201000050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
201100050922     c* La nuova data calcolata non può essere < della minima prevista
201200050922     c                   if        wr0poo>wpoppi
201300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
201400050922     c                   endif
201500050922     c
201600950110%%%  C                   ENDIF
201700950110%%%  C                   ENDIF
201800950110%%%  C*
201900950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
202000050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
202100050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
202200050915%%%  C*  alla prtenza.
202300950110%%%  C     D98ERR        IFEQ      '0'
202400950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
202500950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
202600950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
202700950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
202800950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
202900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
203000050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
203100950110%%%  C                   ENDIF
203200950110%%%  C                   ENDIF
203300950110%%%  C*
203400950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
203500950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
203600950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
203700950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
203800950110%%%  C     D98ERR        IFEQ      '0'
203900950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
204000950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
204100950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
204200950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
204300090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
204400090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
204500950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
204600050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
204700950110%%%  C                   ENDIF
204800950110%%%  C                   ENDIF
204900960229     C*
205000960229     C                   ENDIF                                                  <-- 1 ---
205100950110     C*
205200950110     C                   ENDSR
205300930604     C********************************************************************
205400930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
205500930604     C********************************************************************
205600930604     C     CALDPT        BEGSR
205700090902     c                   clear                   RicalcoloTRAZ     1
205800930604     C*
205900930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
206000931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
206100931129     C                   MOVEL     'I'           WIC                            *INTERNA
206200050915     c* se non presente la data partenza ultimo collo, uso la teorica
206300950110     C     D98DUC        IFEQ      *ZEROS
206400950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
206500950110     C                   ELSE
206600950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
206700950110     C                   ENDIF
206800930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
206900930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
207000930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
207100930604     C*
207200930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
207300931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
207400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
207500950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
207600930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
207700930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
207800930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
207900150508
208000150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
208100150508     c* se presente la data partenza PRIMO  collo --> la uso
208200150508     c                   if        w98dpc=0
208300150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
208400150508     c                   else
208500150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
208600150508     C                   MOVEL     'C'           WIC                            *INTERNA
208700150508     C                   Z-ADD     W98DPC        WDAT
208800150508     C                   EXSR      DATORE
208900150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
209000150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
209100150508     C                   EXSR      RO4CDP
209200150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
209300150508     c                   endif
209400960229     C*
209500960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
209600940624%%%  C*
209700050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
209800050915
209900940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
210000050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
210100050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
210200050915     c*   attribuito alla trazione
210300940624%%%  C     D98ERR        IFEQ      '0'
210400940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
210500940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
210600090903     c     wpoatt        andge     wpopti
210700050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
210800090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
210900090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
211000940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
211100050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
211200050922     c                   if        wr0poo>wpopti
211300050922     c                   eval      wpopti=wr0poo
211400050922%%%  C                   endif
211500090902     c*
211600090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
211700090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
211800090902     c*  della trazione udate e utime
211900090902     c                   eval      RicalcoloTRAZ='S'
212000090902     c
212100940624%%%  C                   ENDIF
212200940624%%%  C                   ENDIF
212300940628%%%  C*
212400050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
212500050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
212600050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
212700940628%%%  C     D98ERR        IFEQ      '0'
212800940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
212900940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
213000940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
213100090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
213200090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
213300940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
213400050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
213500940628%%%  C                   ENDIF
213600940628%%%  C                   ENDIF
213700960229     C*
213800960229     C                   ENDIF                                                  <-- 1 ---
213900930604     C*
214000930604     C                   ENDSR
214100930511     C********************************************************************
214200930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
214300930511     C********************************************************************
214400930511     C     CALDCR        BEGSR
214500050914     c                   clear                   consideraora
214600100308     c
214700100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
214800100308     c*
214900100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
215000100308     c* 1) se presente solo ORA --> ignoro sempre
215100100308     c* 2) se presente data e ora IL --> ignoro l'ora
215200100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
215300100309     c***                if        d98tsp='E'
215400100309     c***                clear                   d98dcr
215500100309     c***                clear                   d98hcr
215600100309     c**+                clear                   d98tcr
215700100309     c***                endif
215800100308     c
215900100308     c                   if        d98hcr>0
216000100308     c
216100100308     c                   if        d98dcr=0
216200100308     c                   clear                   d98hcr
216300100308     c                   endif
216400100308
216500100308     c                   if        d98dcr>0 and d98tcr=' '
216600100308     c                   clear                   d98hcr
216700100308     c                   endif
216800100308     c                   endif
216900941228     C*
217000941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
217100941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
217200050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
217300050901     c*  (per uguale si intende entrambe nel mattino o entrambe
217400050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
217500050901     c*  (EDPES 1/9/05)
217600050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
217700050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
217800050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
217900050831     C***                Z-ADD     D98DCM        D98DCR
218000050831     C***                ENDIF
218100050901     c*
218200070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
218300050914     c
218400100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
218500100308     c***                clear                   d98hcr
218600100308     c***                clear                   d98tcr
218700100308     c***                endif
218800050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
218900050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
219000070521     c*  richiesta dal cliente
219100100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
219200100308     C***                MOVEL     'OSC  '       WR3TAB
219300100308     C***                Z-ADD     D98HMC        WR3HHI
219400100308     C***                EXSR      RO3ORE
219500100308     c***                z-add     wr3hho        salvaoreHMC
219600050901     c
219700100308     C***                Z-ADD     D98Hcr        WR3HHI
219800100308     C***                EXSR      RO3ORE
219900100308     c***                z-add     wr3hho        salvaoreHCR
220000100308     c***
220100050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
220200100308    2c***                if        salvaoreHMC=salvaoreHCR
220300100308     C***                eval      Consideraora='S'
220400100308     c***                else
220500100308     C***                eval      Consideraora=' '
220600100308     c***                clear                   d98hcr
220700100308    2c***                endif
220800100308    1c***                endif
220900930511     C*
221000930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
221100931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
221200931125     C                   Z-ADD     D98DCR        WDAT
221300930325     C                   EXSR      DATORE
221400930326     C                   Z-ADD     WORE          WPODCR
221500930420     C*
221600930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
221700930420     C*
221800931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
221900930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
222000930326     C                   MOVEL     'CRD  '       WR3TAB
222100931125     C                   Z-ADD     D98HCR        WR3HHI
222200930419     C                   EXSR      RO3ORE
222300930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
222400940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
222500050902     c
222600931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
222700930420     C                   MOVEL     'CRD1 '       WR3TAB
222800930420     C                   Z-ADD     *ZEROS        WR3HHI
222900930420     C                   EXSR      RO3ORE
223000931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
223100090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
223200090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
223300050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
223400060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
223500050907     c                   z-add     worecr        oreILoDOPO
223600931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
223700931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
223800931207     C                   MOVEL     'C'           WIC                            *CLIENTE
223900931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
224000150330     C                   Z-ADD     WORECR        WR4NOI
224100931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
224200931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
224300931209     C     WPOCRF        SUB       12            WR7POI            9 0
224400931209     C                   MOVEL     'X'           WIC
224500931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
224600931209     C                   ADD       12            WR7POO
224700931207     C     WR7POO        IFGT      WPOCRF
224800931207     C                   Z-ADD     WR7POO        WPOCRF
224900931207     C                   END
225000150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
225100150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
225200150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
225300150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
225400150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
225500150330     C                   MOVEL     'C'           WIC                            *CLIENTE
225600150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
225700150330     C                   Z-ADD     w99TCC        WR4NOI
225800150330     c                   sub       12            wr4noi
225900150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
226000150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
226100150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
226200150330     c
226300150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
226400150330     C                   MOVEL     'X'           WIC
226500150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
226600150330     C******             ADD       12            WR7POO
226700150330     C     WR7POO        IFGT      WPOCRF_dee
226800150330     C                   Z-ADD     WR7POO        WPOCRF_dee
226900150330     C                   END
227000050902     c
227100050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
227200050902     c*  sono le ore di fine della non consegnabilità
227300050905     c* Se aggiungendo 1 finisco in giorno festivo
227400050905     c*  cerco primo giorno non festivo
227500050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
227600050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
227700050907     C****               MOVEL     'C'           WIC                            *CLIENTE
227800050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
227900050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
228000050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
228100050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
228200050907     c
228300050907     c                   add       1             wpocri
228400050902     c
228500050902   FFC                   Z-ADD     WPOCRI        WPOCRC
228600050902     c
228700930325     C                   END
228800150330     C*
228900931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
229000930420     C* DATA SPED. = DATA INIZIO RANGE
229100930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
229200940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
229300940324£$   C                   Z-ADD     WPOCRI        WPOCRC
229400930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
229500930420     C                   MOVEL     'CRP  '       WR3TAB
229600931125     C                   Z-ADD     D98HCR        WR3HHI
229700930420     C                   EXSR      RO3ORE
229800931207     C     WPODCR        ADD       WR3HHO        WPOCRF
229900931207     C                   SUB       24            WPOCRF                         *FINE RANGE
230000931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
230100931207     C* ANDANDO INDIETRO NEL TEMPO
230200050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
230300050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
230400050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
230500050907     c* data utile venedì sera
230600130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
230700130411     C                   MOVEL     'X'           WIC                            *CLIENTE
230800931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
230900931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
231000931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
231100930326     C                   END
231200930526     C*
231300931125     C     D98TCR        IFEQ      'P'
231400931125     C     D98TCR        OREQ      'D'
231500930526     C                   ELSE                                                   *CONS.RICH.IL ..
231600940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
231700940324£$   C                   Z-ADD     WPODCR        WPOCRC
231800930526     C* DATA SPED. = DATA INIZIO RANGE
231900050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
232000050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
232100050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
232200930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
232300930526     C                   MOVEL     'CRI  '       WR3TAB
232400931125     C                   Z-ADD     D98HCR        WR3HHI
232500930526     C                   EXSR      RO3ORE
232600050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
232700050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
232800050907     c                   z-add     wr3hho        oreILoDOPO
232900930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
233000050902     c
233100050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
233200050902     c                   if        wr3MATPOM='P' and d98hcr>0
233300050902     c                   add       12            wpocri
233400100308     c* tolgo 12 ore dalle ore conseganbili
233500050907     c                   sub       12            oreILoDOPO
233600050902     c                   endif
233700050902     c
233800931209     C     WPOCRF        SUB       12            WR7POI            9 0
233900931209     C                   MOVEL     'X'           WIC
234000931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
234100931209     C                   ADD       12            WR7POO
234200931209     C     WR7POO        IFGT      WPOCRF
234300931209     C                   Z-ADD     WR7POO        WPOCRF
234400931209     C                   END
234500050905     c
234600050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
234700050905     c*  sono le ore di fine della non consegnabilità
234800050905     c* Se aggiungendo 1 finisco in giorno festivo
234900050905     c*  cerco primo giorno non festivo
235000050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
235100050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
235200050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
235300050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
235400050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
235500050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
235600050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
235700050907     c
235800050907     c                   add       1             wpocri
235900050905     c
236000050905   FFC                   Z-ADD     WPOCRI        WPOCRC
236100050905     c
236200930526     C                   END
236300931206     C*
236400931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
236500931206     C* LA IMPOSTO COME LA CONS.TEORICA
236600140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
236700140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
236800931206     C                   ELSE
236900931206     C                   Z-ADD     WPOCRF        WPOCR1
237000931206     C                   END
237100150608     c* ai fini del calcolo affidabilità
237200150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
237300150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
237400931206     C                   ELSE
237500931206     C                   Z-ADD     WPOCRF        WPOCR2
237600931206     C                   END
237700150608     c* ai fini della data prevista consegna cliente
237800150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
237900150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
238000150608     C                   ELSE
238100150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
238200150608     C                   END
238300150331     c
238400150331     c                   if        wpocrf_dee>0
238500150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
238600150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
238700150330     C                   END
238800150331     C                   END
238900150331     C*
239000150331     c                   if        wpocrf_dee=0
239100150608     C     wpocr2_dee    sub       24            WPOCRF_dee
239200150331     c                   endif
239300150331     c
239400930526     C                   END                                                    <-- 1 ---
239500930511     C*
239600930511     C                   ENDSR
239700930511     C********************************************************************
239800930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
239900930511     C********************************************************************
240000930511     C     CALLAV        BEGSR
240100930511     C*
240200941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
240300930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
240400930421     C*
240500930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
240600930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
240700930421     C     1             DO        IE            E                 3 0          --- 2 -->
240800930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
240900930421     C                   Z-ADD     1             I
241000930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
241100060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
241200930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
241300930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
241400930421     C                   ADD       1             IL
241500930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
241600930421     C                   EXSR      DATORE
241700930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
241800091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
241900091014     c* Mi salvo la posizione dell'evento
242000091014     C                   Z-ADD     E             LAp(IL)
242100930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
242200930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
242300930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
242400930421     C                   EXSR      RO3ORE
242500930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
242600930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
242700930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
242800930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
242900091015     c**                 if        §2agla>1
243000091015     c**                 sub       1             §2agla
243100930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
243200091015     c**                 else
243300091015     c**                 clear                   whh
243400091015     c**                 endif
243500930510     C                   ADD       WHH           WORELA
243600940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
243700930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
243800090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
243900150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
244000930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
244100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
244200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
244300930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
244400930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
244500930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
244600930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
244700931209     C                   Z-ADD     LAF(IL)       WR7POI
244800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
244900931202     C     WR7POO        IFGT      LAF(IL)
245000931201     C                   Z-ADD     WR7POO        LAF(IL)
245100931202     C                   END
245200930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
245300930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
245400091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
245500930510     C                   END                                                    <-- 3 ---
245600931223     C                   ENDDO                                                  <-- 2 ---
245700930421     C*
245800930421     C                   END                                                    <-- 1 ---
245900930618££$$ C*
246000930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
246100931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
246200941228££$$ C     1             DO        IL            I
246300930618££$$ C     LAF(I)        IFGT      WPOLAF
246400930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
246500930618££$$ C                   END
246600931223££$$ C                   ENDDO
246700930511     C*
246800930511     C                   ENDSR
246900940324£$   C********************************************************************
247000940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
247100940324£$   C********************************************************************
247200940324£$   C     CALLAC        BEGSR
247300941229     C*
247400950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
247500950407     C*
247600950210     C                   Z-ADD     WORELA        WTM060
247700941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
247800941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
247900941229     C     D98DAG        IFEQ      *ZEROS
248000050520     c     §2aftg        andeq     'S'
248100090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
248200090827     C     WORELA        ADD       w99TCC        WTM060            6 0
248300160404     c                   if        wVP='S'
248400160404     c                   sub       24            wTM060
248500160404     c                   endif
248600941229     C                   ENDIF
248700941229     C*
248800940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
248900940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
249000941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
249100941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
249200941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
249300940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
249400941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
249500050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
249600050520     c                   movel     §2aftg        LCON(IL)
249700950407     C*
249800950407     C                   ENDIF
249900960129     C*
250000940324£$   C                   ENDSR
250100930511     C********************************************************************
250200930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
250300930511     C********************************************************************
250400930511     C     CALGIA        BEGSR
250500960129     C*
250600960129     C*-------------------------------------------------------------------
250700960129     C* EVENTI GIACENZA
250800960129     C*-------------------------------------------------------------------
250900960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
251000960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
251100960129     C*
251200050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
251300041116     c     d98nsp        ifgt      0
251400031125     C                   EXSR      CARGIAC
251500041116     c                   endif
251600031125     C*
251700960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
251800960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
251900960129     C     1             DO        IE            E                 3 0          --- 2 -->
252000960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
252100960129     C                   Z-ADD     1             I
252200960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
252300060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
252400960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
252500960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
252600960129     C                   ADD       1             IG
252700960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
252800960129     C                   EXSR      DATORE
252900960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
253000140606     c* Mi salvo la posizione dell'evento
253100140606     C                   Z-ADD     E             gip(IG)
253200960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
253300960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
253400960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
253500960129     C                   EXSR      RO3ORE
253600960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
253700960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
253800960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
253900960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
254000960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
254100960129     C                   ADD       WHH           WOREGI
254200960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
254300960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
254400090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
254500090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
254600160404     c                   if        wVP='S'
254700160404     c                   sub       24            wOREGI
254800160404     c                   endif
254900960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
255000960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
255100960129     C                   MOVEL     'C'           WIC                            *CLIENTE
255200960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
255300960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
255400960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
255500960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
255600960129     C                   Z-ADD     GIF(IG)       WR7POI
255700960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
255800031125     C     WR7POO        IFGT      GIF(IG)
255900031125     C                   Z-ADD     WR7POO        GIF(IG)
256000031125     C                   ENDIF
256100031201     C*
256200031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
256300031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
256400031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
256500031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
256600031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
256700031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
256800031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
256900031201     C                   IF        WPOATT > GIF(IG)
257000031201     C*
257100031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
257200031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
257300031201     C                   IF        WGGA = *BLANKS
257400031201     C                   SETOFF                                       54
257500031201     C                   ELSE
257600031201     C     WGGA          LOOKUP    SK7QW                                  54
257700031201     C                   ENDIF
257800031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
257900031201     C                   MOVEL     '1'           FLGGIACOK
258000031201     C                   ELSE
258100140127     c
258200031125     C*
258300031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
258400031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
258500031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
258600031125     C                   MOVEL     '0'           FLGGIACOK         1
258700031125     C*
258800031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
258900031125     C     1             DO        IAG           EG                3 0
259000031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
259100031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
259200031125     C                   EXSR      DATORE
259300031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
259400031125     C*
259500031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
259600031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
259700031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
259800031125     C                   EXSR      DATORE
259900031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
260000031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
260100031229     C* => NN LO CONSIDERO
260200031229     C                   IF        EVC(E+1) = 'G' AND
260300031229     C                             WPROXGII = WGCPDORE
260400031229     C                   MOVEL     '1'           FLGGIACOK
260500031229     C                   ENDIF
260600031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
260700031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
260800031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
260900031125     C                   EXSR      RO3ORE
261000031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
261100031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
261200031125     C* DELL'EVENTO GIACENZA CORRENTE
261300031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
261400060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
261500031125     C                   MOVEL     '1'           FLGGIACOK
261600031125     C                   LEAVE
261700031125     C                   ENDIF
261800031125     C                   ELSE
261900031229     C                   IF        EVC(E)  = 'G' AND
262000031229     C                             GII(IG) = WGCPDORE
262100031229     C                   MOVEL     '1'           FLGGIACOK
262200031229     C                   LEAVE
262300031229     C                   ENDIF
262400031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
262500031125     C                   MOVEL     '1'           FLGGIACOK
262600031125     C                   LEAVE
262700031125     C                   ENDIF
262800031125     C                   ENDIF
262900031125     C                   ENDDO
263000031201     C                   ENDIF
263100140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
263200140127     C                   IF        FLGGIACOK = '0'  and
263300140127     c                             d98dcm=evd(E)
263400140127     C                   MOVEL     '1'           FLGGIACOK
263500140127     c                   endif
263600031125     C*
263700031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
263800031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
263900031125     C* DELL'EVENTO GIACENZA CORRENTE
264000031125     C                   IF        FLGGIACOK = '0'
264100031125     C                   Z-ADD     *ZEROS        GIF(IG)
264200031125     C                   ELSE
264300031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
264400031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
264500031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
264600031125     C                   ENDIF
264700031125     C                   ELSE
264800031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
264900031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
265000031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
265100031125     C                   ENDIF
265200031125     C*
265300960129     C                   ENDIF                                                  <-- 3 ---
265400960129     C                   ENDDO                                                  <-- 2 ---
265500960129     C*
265600960129     C                   ENDIF                                                  <-- 1 ---
265700960129££$$ C*
265800960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
265900960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
266000960129££$$ C     1             DO        IG            I
266100960129££$$ C     GIF(I)        IFGT      WPOGIF
266200960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
266300960129££$$ C                   ENDIF
266400960129££$$ C                   ENDDO
266500941229     C*
266600941229     C*-------------------------------------------------------------------
266700941229     C* APERTURA GIACENZA
266800941229     C*-------------------------------------------------------------------
266900960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
267000960129     C*
267100960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
267200931125     C                   Z-ADD     D98DAG        WDAT
267300930325     C                   EXSR      DATORE
267400950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
267500960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
267600950413£££  C                   Z-ADD     WORE          WPODAG
267700960129$$£  C*********************ENDIF
267800930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
267900930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
268000930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
268100930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
268200930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
268300941229     C*
268400941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
268500941229     C                   Z-ADD     WPODAG        WPOAGC
268600960129     C*
268700960129     C                   ENDIF                                                  <-- 1 ---
268800941229     C*
268900941229     C*-------------------------------------------------------------------
269000930511     C* CHIUSURA GIACENZA
269100941229     C*-------------------------------------------------------------------
269200930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
269300930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
269400931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
269500931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
269600931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
269700931201     C                   Z-ADD     D98DCM        W98DCG
269800930416     C                   ELSE
269900930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
270000930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
270100931201     C                   Z-ADD     WDAODI        W98DCG
270200061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
270300061123     c*  chiusura giacenza, come già fa in altri posti
270400061123     c                   if        d98dfc>0
270500061123     C                   Z-ADD     d98dfc        W98DCG
270600061123     c                   endif
270700061123     c
270800930409     C                   END
270900930409     C                   END
271000930409     C                   END
271100930414     C* TRASF.DATA CHIUSURA
271200931201     C     W98DCG        IFNE      *ZEROS
271300931201     C                   Z-ADD     W98DCG        WDAT
271400930325     C                   EXSR      DATORE
271500930326     C                   Z-ADD     WORE          WPODCG
271600930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
271700930407     C* GIORNO DA ESCLUDERE
271800150216     C                   ADD       12            WPODCG
271900150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
272000150123     c*  il giorno di chiusura dovrebe già essere protetto
272100941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
272200941229     C     WPOCGC        IFEQ      *ZEROS
272300941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
272400941229     C                   ENDIF
272500930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
272600930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
272700931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
272800931129     C                   MOVEL     'C'           WIC                            *CLIENTE
272900150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
273000150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
273100160404     c                   if        wVP='S'
273200160404     c                   sub       24            wr4NOI
273300160404     c                   endif
273400930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
273500930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
273600930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
273700931209     C                   Z-ADD     WPODCG        WR7POI
273800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
273900931202     C     WR7POO        IFGT      WPODCG
274000931201     C                   Z-ADD     WR7POO        WPODCG
274100930326     C                   END
274200931202     C                   END
274300930511     C*
274400930511     C                   ENDSR
274500031125     C********************************************************************
274600031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
274700031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
274800031125     C*             DEGLI EVENTI DI TIPO GIACENZA
274900031125     C********************************************************************
275000031125     C     CARGIAC       BEGSR
275100031125     C*
275200031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
275300031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
275400031125     C*
275500050405     C                   Z-ADD     D98AAS        KWGCPAAS
275600050405     C                   Z-ADD     D98LNP        KWGCPLNP
275700050405     C                   Z-ADD     D98NRS        KWGCPNRS
275800050405     C                   Z-ADD     D98NSP        KWGCPNSP
275900031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
276000050405     C     KEYGC_P       SETLL     TIGCP01L
276100050405     C                   IF        %equal(TIGCP01L)
276200050405     C     KEYGC_P       READE     TIGCP01L
276300050405     C                   DOW       not %eof(TIGCP01L)
276400031125     C                   ADD       1             IAG
276500031125     C                   EVAL      GCPD(IAG) = GCPDUR
276600050405     C     KEYGC_P       READE     TIGCP01L
276700031125     C                   ENDDO
276800031125     C                   ENDIF
276900031125     C                   ELSE
277000050405     C     KEYGC_P       SETLL     TIGCP21L
277100050405     C                   IF        %equal(TIGCP21L)
277200050405     C     KEYGC_P       READE     TIGCP21L
277300050405     C                   DOW       not %eof(TIGCP21L)
277400031125     C                   ADD       1             IAG
277500031125     C                   EVAL      GCPD(IAG) = GCPDUR
277600050405     C     KEYGC_P       READE     TIGCP21L
277700031125     C                   ENDDO
277800031125     C                   ENDIF
277900031125     C                   ENDIF
278000031125     C*
278100031125     C                   ENDSR
278200941229     C********************************************************************
278300960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
278400941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
278500941229     C********************************************************************
278600960129     C     CALGIC        BEGSR
278700941229     C*
278800960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
278900960129     C*
279000960129     C                   Z-ADD     WOREGI        WTM060
279100960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
279200960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
279300960129     C     D98DAG        IFEQ      *ZEROS
279400960129$$£  C     WOREGI        ADD       12            WTM060            6 0
279500960129     C                   ENDIF
279600960129     C*
279700960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
279800960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
279900960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
280000960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
280100960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
280200960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
280300960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
280400960129     C*
280500960129     C                   ENDIF
280600941229     C*
280700941229     C                   ENDSR
280800060418     C********************************************************************
280900060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
281000060418     c*            l'affidabilità
281100060418     C********************************************************************
281200060418     C     CALCEPA       BEGSR
281300060418     C*
281400060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
281500060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
281600060420     c                   clear                   numggcepa         3 0
281700060418     C*
281800060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
281900060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
282000060420     c                   eval      wgiroepa='0'
282100060418     c                   eval      wfine=' '
282200060418     c
282300060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
282400060420    2C     W             DOwle     IP                                           --- 2 -->
282500060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
282600060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
282700060418     c
282800060420    4c                   if        wgiroepa='0'
282900060420     c                   exsr      ElaInizCEPA
283000060418   x4c                   else
283100060420    5c                   if        pvd(W)>wdat
283200060418     c                   eval      wfine='S'
283300060418    5c                   endif
283400060418    4c                   endif
283500060418     c
283600060420    4c                   if        wfine=' ' and pvd(W)=wdat
283700060420     C*
283800060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
283900060420     c*  finale
284000060420     c                   add       1             numggcepa
284100060420     c
284200060420     C                   z-add     24            WORELA
284300060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
284400060418     c
284500060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
284600060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
284700060418     C                   MOVEL     'C'           WIC                            *CLIENTE
284800060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
284900060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
285000060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
285100060418     c
285200060418     c* Trasformo le ore in data e verifico se c'e' altro evento
285300060418     c*  in sta data che allunga ancora
285400060420     c                   z-add     wr4poo        wore
285500060418     c                   EXSR      OREDAT
285600060418    4c                   endif
285700060418     c
285800060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
285900060418    4c                   if        wfine='S'
286000060418     c                   EXSR      ELAFINECEPA
286100060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
286200060420     c*  inizio range
286300060420     c                   else
286400060420     c                   add       1             w
286500060418    4C                   ENDif                                                  <-- 3 ---
286600060420     c
286700060420     c                   else
286800060420     c                   add       1             w
286900060418    3C                   ENDif                                                  <-- 3 ---
287000060418    2C                   ENDDO                                                  <-- 2 ---
287100060418     c
287200060420     c                   if        wgiroepa='1'
287300060418     c                   EXSR      ELAFINECEPA
287400060418     c                   endif
287500060418     C*
287600060418    1C                   END                                                    <-- 1 ---
287700060418££$$ C*
287800060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
287900060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
288000060420££$$ C     1             DO        Ipa           w
288100060420££$$ C     paif(w)       IFGT      WPOpaif
288200060420££$$ C                   Z-ADD     paif(w)       WPOpaif
288300060418££$$ C                   END
288400060418££$$ C                   ENDDO
288500060418     C*
288600060418     C                   ENDSR
288700060420     C********************************************************************
288800060420     c* Elabora Inizio range eventi particolari che allungano
288900060420     C********************************************************************
289000060420     c     ElaInizCEPA   BEGSR
289100060420     C                   ADD       1             IPA
289200060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
289300060420     C                   EXSR      DATORE
289400060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
289500061017     c
289600060420     c                   eval      wgiroepa='1'
289700060420     c                   eval      wdat=pvd(w)
289800060420     C                   ENDSR
289900060418     C********************************************************************
290000060418     c* Elabora fine range eventi particolari che allungano
290100060418     C********************************************************************
290200060418     c     ElafineCEPA   BEGSR
290300060420     c     numggcepa     mult      24            worela
290400060420     c                   sub       12            worela
290500090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
290600090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
290700160404     c                   if        wVP='S'
290800160404     c                   sub       24            WORELA
290900160404     c                   endif
291000060418     c
291100060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
291200060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
291300060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
291400060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
291500060418     C                   Z-ADD     paif(IPA)     WR7POI
291600060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
291700060418    5C     WR7POO        IFGT      paif(IPA)
291800060418     C                   Z-ADD     WR7POO        paif(IPA)
291900060418    5C                   END
292000061017     c*Sommo 24 ore di inizio giornata
292100061017     c*            : non ho capito perchè ma ci vogliono
292200061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
292300090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
292400090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
292500160404     c                   if        wVP='S'
292600160404     c                   sub       24            paii(IPA)
292700160404     c                   endif
292800061017     c* Se tempi di consegna inferiori alle 24 ore , devo
292900061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
293000061017     c*  partire prima
293100061017     c****               if        d98tcc<24
293200061017     c****               sub       12            paii(ipa)
293300061017     c****               endif
293400060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
293500060418     c* Imposto nuovo giro
293600060420     c                   eval      wgiroepa='0'
293700060418     c                   eval      wfine=' '
293800060420     c                   clear                   numggcepa         3 0
293900060418     c                   ENDSR
294000930511     C********************************************************************
294100930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
294200930511     C********************************************************************
294300010822     C     CALASS        BEGSR
294400010821     C*
294500011211     C                   EXSR      REPMAXCSB
294600930511     C*
294700930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
294800010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
294900011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
295000930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
295100930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
295200930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
295300930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
295400930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
295500930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
295600930511     C                   ELSE                                                   <-- 4 -->
2957009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
295800930512     C                   END                                                    <-- 4 ---
295900930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
296000930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
296100930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
296200930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
296300930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
296400930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
296500930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
296600930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
296700930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
296800930413     C                   END                                                    <-- 2 ---
296900011211     C                   END                                                    <-- 1 ---
297000930406     C*
297100930325     C                   ENDSR
297200930608     C********************************************************************
297300930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
297400930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2975009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
297600930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
297700930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
297800930608     C********************************************************************
297900930608     C     RDRASS        BEGSR
298000930608     C*
298100930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2982009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2983009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2984009407151507 C*********************EXSR RO3ORE
2985009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
298600930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2987009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2988009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2989009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2990009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2991009407191907 C                   Z-ADD     WPOASI        WPOROU
2992009407191907 C                   EXSR      CHKFES
2993009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2994009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2995009407191907 C                   ELSE                                                   *E' FERIALE
2996009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2997009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
2998009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
2999009407191907 C                   ENDIF
300000930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
300100930608     C                   MOVEL     'ASS1 '       WR3TAB
300200930608     C                   Z-ADD     *ZEROS        WR3HHI
300300930608     C                   EXSR      RO3ORE
300400930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
300500930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
300600931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
300700931129     C                   MOVEL     'C'           WIC                            *CLIENTE
300800930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
300900930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
301000930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
301100930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
301200930608     C*
301300930608     C                   ENDSR
301400930511     C********************************************************************
301500930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
301600930511     C********************************************************************
301700930511     C     CALFED        BEGSR
301800150331     c                   clear                   fddata
301900150331     c                   clear                   WPOFDdata
302000930511     C*
302100051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
302200931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
302300051108     C     D98FFD        oreq      'M'                                          --- 1 -->
302400930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
302500930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
302600930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
302700930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
302800150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
302900150331     c                   if        §ar5dfd>*zeros
303000150331     c                   movel     §ar5dfd       fddata            8 0
303100150331     c                   endif
303200150331     c                   if        fddata>0 and ((d98dti>0 and
303300150331     c                             fddata>d98dti) or d98dti=0)
303400150331     C                   Z-ADD     fddata        WDAT
303500150331     C                   EXSR      DATORE
303600150331     C                   Z-ADD     WORE          WPOfddata
303700150331     c                   endif
303800150331     c
303900930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
304000930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
304100930511     C                   ELSE                                                   <-- 2 -->
3042009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
304300930512     C                   END                                                    <-- 2 ---
304400150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3045001504081507 C                   Z-ADD     WPOfddata     WPOFDI
304600150331     c                   endif
304700150331
304800930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
304900930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
305000930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
305100930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
305200930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
305300930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
305400930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
305500930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
305600930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
305700930511     C                   END                                                    <-- 1 ---
305800930511     C*
305900930511     C                   ENDSR
306000930608     C********************************************************************
306100930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
306200930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3063009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
306400930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
306500930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
306600930608     C********************************************************************
306700930608     C     RDRFED        BEGSR
306800930608     C*
306900930608     C* CALCOLO ORE DEL FERMO DEPOSITO
307000930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3071009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3072001503311907 C                   Z-ADD     WPOFDI        WPOROU
3073001503311907 C                   EXSR      CHKFES
3074001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3075001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3076001503311907 C                   ELSE                                                   *E' FERIALE
3077001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3078001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3079001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3080001503311907 C                   ENDIF
308100150331     c* Se presente la particolarità consegna con più giorni di fermo depo
308200150331     c*  uso questi
308300150331     c                   if        §7rggfd>*zeros
308400150424     c                   movel     §7rggfd       wggfd             2 0
308500150331     c     wggfd         mult      24            Wnore
308600150331     c                   add       wnore         worefd
308700150331     c                   else
308800930608     C                   MOVEL     'FED1 '       WR3TAB
308900930608     C                   Z-ADD     *ZEROS        WR3HHI
309000930608     C                   EXSR      RO3ORE
309100940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
309200150331     c                   endif
309300930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
309400931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
309500931129     C                   MOVEL     'C'           WIC                            *CLIENTE
309600930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
309700930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
309800930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
309900930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
310000930608     C*
310100930608     C                   ENDSR
310200931201     C********************************************************************
310300931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
310400931201     C********************************************************************
310500931201     C     CALDTE        BEGSR
310600140825     c
310700140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
310800140825     c*  che tiene conto solo dei festivi per restituire in output
310900140825     c*  la data teorica di consegna
311000140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
311100140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
311200140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
311300140825     C                   ELSE
311400140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
311500140825     C                   END
311600140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
311700141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
311800141106     c*  quindi passo un flag
311900141106     c                   eval      nosomma='N'
312000140825     C                   EXSR      RO4CDP
312100141106     c                   eval      nosomma=' '
312200140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
312300140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
312400140825     c*  di consegna
312500140825     c                   EXSR      TRAS_DEI
312600140825     c
312700140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
312800140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
312900140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
313000140825     c                             ='M' or %subst(d98gc2:2:1)='P')
313100140825     c                   exsr      Reper_tempi
313200140825     c                   endif
313300931201     C*
313400140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
313500140825     c*  che tiene conto di:
313600140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
313700150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
313800931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
313900931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
314000931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
314100931201     C                   ELSE
314200931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
314300931201     C                   END
314400090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314500090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314600931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
314700140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
314800931201     C*
314900150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
315000931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
315100931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
315200090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
315300931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
315400150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
315500150608     c
315600150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
315700150608     c*  che non tiene conto dei tempi del C/Ass
315800150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
315900150609     c*
316000150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
316100150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
316200150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
316300150630     c                   Z-ADD     WPODTI        WR6POI
316400150609     c                   else
316500150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
316600150609     c                   endif
316700150609     c
316800150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
316900150608     c                   eval      wdee='S'
317000150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
317100150608     c                   eval      wdee=' '
317200150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
317300141017
317400141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
317500150219     C**                 EVAL      WTC='C'
317600150219     C**                 EVAL      WIC='C'
317700150219     c**                 z-add     wpoattdata00  WPOROU
317800150219     c**                 z-add     wdaodi        wpodata
317900150219     c**                 exsr      chkfes
318000150219     c**                 if        wslitt='S'
318100150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
318200150219     c**                 z-add     24            wr6noi
318300150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
318400150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
318500150219     c**                 else
318600150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
318700150219     c**                 endif
318800141204     c* Ricalcolo WPOrifDatLav
318900141229     C**                 MOVEL     'OSC  '       WR3TAB
319000141229     C**                 Z-ADD     WHHODI        WR3HHI
319100141229     C**                 EXSR      RO3ORE
319200141229     c**
319300141229     C**                 EVAL      WTC='C'
319400141229     C**                 EVAL      WIC='C'
319500141229     c**                 z-add     wpoattdata00  WPOROU
319600141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
319700141229     c**                 z-add     wr3hho        wr6noi
319800141229     c**                 add       12            wr6noi
319900141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
320000141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
320100141229     c
320200141017     c
320300141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
320400141017     c*  ma sempre devo slittare i festivi
320500141017     C* CONSEGNABILITA'
320600150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
320700150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
320800150219     C**                 EVAL      WTC='C'
320900150219     C**                 EVAL      WIC='C'
321000150219     c**                 z-add     d98dfc        WPOdata
321100150219     c**                 exsr      chkfes
321200150219     c**                 if        wslitt='S'
321300150219     c**                 z-add     wpodfc_00     Wr6poi
321400150219     c**                 z-add     24            wr6noi
321500150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
321600150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
321700150219     c**                 else
321800150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
321900150219     c**                 endif
322000150219     c**                 endif
322100931201     C*
322200931201     C                   ENDSR
322300930511     C********************************************************************
322400930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
322500930511     C********************************************************************
322600930511     C     CALDPC        BEGSR
322700930325     C*
322800930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
322900930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
323000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
323100930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
323200930325     C                   ELSE
323300930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
323400930407     C                   END
323500090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
323600090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
323700930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
323800090902     c
323900931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
324000090901
324100930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
324200930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
324300930423     C* A QUELLA DI RICHIESTA CONSEGNA,
324400930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
324500931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
324600931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
324700931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
324800931206     C                   END
324900930423     C                   END
325000930325     C*
325100930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
325200930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
325300930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
325400090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325500090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325600930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
325700931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
325800930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
325900930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
326000930423     C* A QUELLA DI RICHIESTA CONSEGNA,
326100930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
326200931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
326300931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
326400931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
326500931206     C                   END
326600930423     C                   END
326700140822     C*
326800930324     C                   ENDSR
326900150113     C********************************************************************
327000150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
327100150113     C********************************************************************
327200150113     c     Cal_DEE       BEGSR
327300150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
327400150508     c                   clear                   wtempi00C         1
327500151007     c                   clear                   wtempi12C         1
327600150722     c                   clear                   woreOLP
327700151009     c                   clear                   wdiff
327800150219     c
327900150219     c* 1) Se presente l'arrivo della trazione lo uso
328000150219    1c                   select
328100150219     c                   when      wmancatraz=' '  and d98dti>0
328200150630     c                   Z-ADD     WPODTI        WR1POI
328300150219     c
328400150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
328500150219    1c                   when      IDdata>0
328600150325     c                   exsr      CalcoloIMP
328700151008     c* Passo in output il terminal di disguido
328800151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
328900151008     c                   eval      d98flp=%editc(LTP(2):'X')
329000151008     c                   endif
329100151008
329200150528     c* stato IMP/disguido solo se >  alla data di oggi
329300151006     c                   if        wr1poi > wpoattdata00 and
329400151006     c                             (wr1poi-wpoattdata00)>12
329500150427     c                   eval      d98spcdee='IMP'
329600151008     c                   endif
329700151006     c                   if        wr1poi > wpoattdata00 and
329800151006     c                             (wr1poi-wpoattdata00)=12
329900151006     c                             and w991In>12
330000151006     c                   eval      d98spcdee='IMP'
330100151008     c                   endif
330200150219     c
330300150219     c                   other
330400150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
330500150225     c* devo far finta che sia arrivato.
330600150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
330700150508     c*  che tiene conto della data partenza PRIMO  collo
330800150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
330900150508     c*
331000150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
331100150508     c                   if        wr1poi >=wpoattdata00
331200150508     c                              and WPOPTC_DEE > WPOPTC
331300150508     c                   eval      d98spcdee='IMP'
331400150508     c                   endif
331500150219     c                   endsl
331600150113     c
331700150219     c* se il calcolo è < di oggi --> imposto oggi
331800150113     c                   if        wr1poi < wpoattdata00
331900150113     c                   eval      wr1poi= wpoattdata00
332000150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
332100150508     c                   eval      wTempi00C='S'
332200151009     c                   clear                   wdiff
332300150113     c                   endif
332400150217     c
332500150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
332600150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
332700150630     c                   if        wr1poi>wpoattdata00
332800150630     c                   eval      wdiff=wr1poi-wpoattdata00
332900150630     c                   endif
333000151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
333100151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
333200151006     c                   eval      wTempi12C='S'
333300151006     c                   endif
333400160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
333500151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
333600151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
333700160404     c                             and idfgs>0
333800151009     C     wdiff         DIV       24            WDIV
333900151009     C                   MVR                     WRES
334000151009     C                   IF        wres>0
334100151009     c                   eval      wTempi12C='S'
334200151009     c                   else
334300151009     c                   eval      wTempi00C='S'
334400151009     c                   endif
334500151009     c                   endif
334600151009     c
334700151006     c
334800151006     c                   select
334900151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
335000151006     c                             w991in>12 and
335100151006     c                             (deeindist<>'S' or d98cons<>'S'
335200151006     c                             or d98ddc>wdaodi)
335300151006     c
335400151006     c* Se stato IMP lo mantengo per avere messaggio diverso
335500151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
335600151006     c                   if        d98spcdee<>'IMP'
335700151006     c                   eval      d98spcdee='PRV'
335800151006     c                   endif
335900151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
336000150330     c                             (deeindist<>'S' or d98cons<>'S'
336100150330     c                             or d98ddc>wdaodi)
336200150427     c
336300150427     c* Se stato IMP lo mantengo per avere messaggio diverso
336400150427     c                   if        d98spcdee<>'IMP'
336500150304     c                   eval      d98spcdee='PRV'
336600150427     c                   endif
336700150427     c
336800151006   x1c                   other
336900150330     c                   exsr      CAL_OLP
337000150217     c
337100150528     c* Se passato l'orario oppure data distinta < data di oggi
337200150508     c* 1) spedizione in distinta          --> in consegna
337300150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
337400150528     c                             d98ddc<wdaodi)
337500150508    3c                   IF        deeindist='S' and d98cons='S'
337600150508     c                   eval      d98spcdee= 'DDC'
337700150508
337800150508   x3c                   else
337900150507     c
338000150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
338100150722     c*  entro sera
338200150630     c                   if        wr1poi>wpoattdata00
338300150722     c                   z-add     12            woreOLP           3 0
338400150722     c***                add       12            wr1poi
338500150630    4c                   else
338600150507    4c                   if        wr1poi<=wpoattdata00
338700150722     c                   add       24            woreOLP
338800150722     c***                add       24            wr1poi
338900150507    4c                   endif
339000150630    4c                   endif
339100150507     c
339200150722     c****               eval      d98spcdee='OLP'
339300150507     c* memorizzo orario limite picking
339400150507     c                   eval      d98picklim=%editc(picklim:'X')
339500150507    3c                   endif
339600150507    2c                   endif
339700150507     c
339800151006    1c                   endsl
339900150217     c
340000150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
340100150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
340200150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
340300150113     C                   Z-ADD     Wr1poi        W10POI            9 0
340400150113     C                   EXSR      R10FES
340500150113     C                   Z-ADD     W10POO        wr1poi
340600150113     C
340700151006     c                   select
340800151006    1c                   when      wtempi12C='S'
340900151006     c                   z-add     W991IN        wr1noi
341000151006     c                   if        woreOLP>0
341100151006     c                   add       woreOLP       wr1noi
341200151006     c                   eval      d98spcdee='OLP'
341300160404     c                   else
341400160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
341500160404     c**                 if        wVP='S'
341600160404     c**                 add       24            wr1noi
341700160404     c**                 endif
341800151006     c                   endif
341900160404
342000151006    1c                   when      wtempi00C='S'
342100150630     c***                z-add     W990CN        wr1noi
342200150630     c                   z-add     W990IN        wr1noi
342300150722     c
342400150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
342500150722     c*  quindi sottraggo 24
342600151009     c                   if        wr1noi>24  and wdiff=0
342700150722     c                   sub       24            wr1noi
342800150722     c                   endif
342900150722     c
343000150722     c* se passato orario limite picking, slitto in avanti
343100150722     c                   if        woreOLP>0
343200150722     c                   add       woreOLP       wr1noi
343300150722     c                   eval      d98spcdee='OLP'
343400160404     c                   else
343500160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
343600160404     c**                 if        wVP='S'
343700160404     c**                 add       24            wr1noi
343800160404     c**                 endif
343900150722     c                   endif
344000150722     c
344100151006   x1c                   other
344200150722     c
344300150714     c                   if        wmancatraz=' '  and d98dti>0
344400150630     c                   z-add     w99tci        wr1noi
344500150714     c                   else
344600150714     c                   z-add     w99tcc        wr1noi
344700150714     c                   endif
344800160404
344900160401     c
345000150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
345100150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
345200150722     c                   if        woreOLP=24 and wr1noi<=24
345300150722     c                   add       woreOLP       wr1noi
345400150722     c                   eval      d98spcdee='OLP'
345500150722     c                   endif
345600150722     c                   if        woreOLP=12 and wr1noi<24
345700150722     c                   add       woreOLP       wr1noi
345800150722     c                   eval      d98spcdee='OLP'
345900150722     c                   endif
346000150722     c
346100151006    1c                   endsl
346200150629     c
346300150722    1c**                 if        WR1NOI=10
346400150722     c**                 z-add     12            wr1noi
346500150722     c**                 endif
346600150722     c
346700150722     c     wr1noi        div       12            wdiv
346800150722     c                   mvr                     wres
346900150722     c                   if        wres >0
347000150722     c                   add       2             wr1noi
347100150722     c                   endif
347200150722     c
347300150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
347400150629    1c******             endif
347500150508     c
347600150605     c                   eval      wdee='S'
347700150113     C                   EXSR      RO1CDP
347800150605     c                   eval      wdee=' '
347900150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
348000150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
348100150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
348200150113     C* A QUELLA DI RICHIESTA CONSEGNA,
348300150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
348400150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
348500150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
348600150331     c*  2) IL        -> è il giorno indicato
348700150331     C     D98DCR        IFNE      *ZEROS
348800150331     C     D98tcr        andne     'P'
348900150331     C***  WPOdpcee      IFLE      WPOCR2
349000150331     C***  WPOCR2        SUB       12            WPOdpcee
349100150330     c
349200150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
349300150330     c*   dare la prima data utile di consegna
349400150330     C     WPOdpcee      IFLE      WPOCRF_dee
349500150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
349600150330     C                   z-add     WPOCRF_dee    WPOdpcee
349700150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
349800150409     c                   if        wpodpcee>wpoattdata00
349900150409     c                   eval      d98spcdee='PRV'
350000150409     c                   endif
350100150113     C                   END
350200150331     c
350300150220     c
350400150113     C                   END
350500150220     c
350600150113     c                   ENDSR
350700150330     C********************************************************************
350800150330     C*   Calcolo orario da tabella OLP
350900150330     C********************************************************************
351000150330     c     Cal_OLP       BEGSR
351100150330     c                   eval      picklim=0900
351200150330     c* Dopo l'ora limite picking, passo al giorno dopo
351300150330     C                   MOVEL(P)  'OLP'         TBECOD
351400150330     C                   MOVEL(P)  d98lna        TBEKE1
351500150330     C                   MOVEL(P)  wdaodi        TBEKE2
351600150330     C     KTBE          CHAIN     TNTBE01L
351700150330     C                   IF        not %found(TNTBE01L)
351800150330     c                   clear                   tbeke2
351900150330     C     KTBE          CHAIN     TNTBE01L
352000150330     c                   endif
352100150330     C                   IF        not %found(TNTBE01L)
352200150330     C                   MOVEL(P)  '000'         TBEKE1
352300150330     C     KTBE          CHAIN     TNTBE01L
352400150330     c                   endif
352500150330     c
352600150330     C                   IF        %found(TNTBE01L) and
352700150330     c                             %subst(tbeuni:1:4)>*zeros
352800150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
352900150330     c                   endif
353000150330     c                   ENDSR
353100150325     C********************************************************************
353200150325     C*   Riclcolo data se colli in IMP/disguido
353300150325     C********************************************************************
353400150325     c     CalcoloIMP    BEGSR
353500150325     C                   Z-ADD     IDdata        WDAT
353600150325     C                   EXSR      DATORE
353700150325     C                   Z-ADD     WORE          WR4POI
353800150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
353900151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
354000150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
354100150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
354200150325     c* calcolo il terminal di partenza
354300150325     c                   clear                   fnlv55ds
354400150325     c                   eval      d55tla='L'
354500150325     c                   eval      d55tpt='P'
354600150325     c                   eval      d55lin=IDfgs
354700150325     c                   eval      d55drf=IDdata
354800150325     c                   call      'FNLV55R'
354900150325     c                   parm                    fnlv55ds
355000150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
355100150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
355200150325     c                   EXSR      RTTRAZ
355300150325    3c                   if        d98err<>'1'
355400150325     C                   Z-ADD     CASTcn        wr4noi
355500150325     c                   else
355600150325     C                   Z-ADD     d98ttc        wr4noi
355700150325    3C                   endif
355800150325
355900150325   x2c                   else
356000150325     C                   Z-ADD     d98ttc        wr4noi
356100150325    2C                   endif
356200150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
356300150325     c*  perchè partirà un giorno dopo
356400150325    3c                   if        IDnpg='P'
356500150325     c                   add       24            WR4noi
356600150325    3c                   endif
356700150325     c
356800150325     c                   exsr      ro4cdp
356900150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
357000150325     c                   ENDSR
357100930604     C********************************************************************
357200930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
357300930604     C********************************************************************
357400930604     C     TPODAT        BEGSR
357500960229     C*
357600960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
357700960229     C*
357800960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
357900960229     C     WPOPTI        IFEQ      *ZEROS
358000960229     C                   Z-ADD     *ZEROS        D98DTI
358100960229     C                   Z-ADD     *ZEROS        D98HTI
358200960229     C                   ELSE
358300960229     C                   Z-ADD     WPOPTI        WORE
358400960229     C                   EXSR      OREDAT
358500960229     C                   Z-ADD     WDAT          D98DTI
358600960229     C     WRES          MULT      100           D98HTI
358700960229     C                   END
358800960229     C*
358900960229     C                   ELSE                                                   <-- 1 -->
359000930604     C*
359100931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
359200140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
359300140825     c*******            exsr      Tras_DEI
359400140825     c
359500930604     C*
359600150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
359700140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
359800140820     c* è data da :
359900140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
360000140820     c*  Tenere  conto, ovvero slittare in avanti, per :
360100140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
360200140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
360300140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
360400140911     c*
360500140820     c*  Da azzerare nei seguenti casi:
360600150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
360700150324     c* 4) bolla export    7) Cod manca consegna
360800140911     c* 8) bolla con DCR DOPOil o PRIMAdel
360900150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
361000150119     c*10) se completamente non arrivata e collo in IMP
361100150119     c
361200140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
361300150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
361400140925     C*  - FD   - export -
361500150217     c                   movel     'S'           DEEinDIST         1
361600150113     c                   eval      wcaldpcee='S'
361700150304     c                   eval      D98SPCDEE='PRV'
361800150324     c                   clear                   avvcmc            1
361900150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
362000150323     c                   if        d98cmc<>*blanks
362100150324     C                   Z-ADD     1             II
362200150326     C     d98cmc        LOOKUP    C2A(II)                                99
362300150324     C                   MOVEL     D2A(II)       usa2A
362400150324    4C     §2AFTC        IFEQ      'A'
362500150326     c                   eval      avvcmc='A'
362600150323     c                   endif
362700150324    4C     §2AFTC        IFEQ      'G'
362800150326     c                   eval      avvcmc='G'
362900150324     c                   endif
363000150324     c                   endif
363100140820     c
363200140911     c                   z-add     d98lna        EX
363300150113     c*  la azzero nei casi previsti                 0
363400150601    1c                   select
363500150119     c* 1) Se la bolla e consegnata
363600140925     c                   when      d98dcm>0
363700150113     c                   clear                   wcaldpcee
363800150216     c                   eval      d98spcdee='CON'
363900150217     c                   movel     ' '           DEEinDIST         1
364000150323     c* 2) Se pratica di danno
364100150603     c                   when      flgdan='S'
364200150323     c                   clear                   wcaldpcee
364300150323     c                   eval      d98spcdee='CA '
364400150323     c                   eval      deeindist='S'
364500150324     c* 3) Se bloccata/rimpallo
364600150114     c                   when      wd98fbc='B'
364700150113     c                   clear                   wcaldpcee
364800150216     c                   eval      d98spcdee='BLO'
364900150217     c                   movel     ' '           DEEinDIST         1
365000150324     c* 4) Se bolla export
365100150324     c                   when      fl1(EX)='E'
365200150324     c                   clear                   wcaldpcee
365300150324     c                   movel     ' '           DEEinDIST         1
365400150324     c                   eval      d98spcdee='EXP'
365500150324     c* 5) Se giacente  o in attesa di apertura giacenza
365600150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
365700150324     c                   clear                   wcaldpcee
365800150324     c                   eval      d98spcdee='GIA'
365900150324     c                   eval      deeindist='S'
366000150119     c* 7) Se codice manca consegna
366100150326     c                   when      d98cmc<>*blanks and avvcmc='A'
366200150324     c                   clear                   wpodpcee
366300150324     c                   clear                   wcaldpcee
366400150324     c                   movel     ' '           DEEinDIST         1
366500150324     c                   eval      d98spcdee='ATT'
366600150410     c* 7) Bolla in sospensione consegna
366700150410     c                   when      §B4LBGE='S'
366800150410     c                   clear                   wpodpcee
366900150410     c                   clear                   wcaldpcee
367000150410     c                   movel     ' '           DEEinDIST         1
367100150410     c                   eval      d98spcdee='SOS'
367200150616     c
367300150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
367400150616     c                   when      wrbl='R'
367500150616     c                   clear                   wcaldpcee
367600150616     c                   eval      d98spcdee='   '
367700150616     c                   eval      deeindist='S'
367800150601   x1c                   other
367900150325     c*
368000150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
368100150331     c                   if        d98ffd='S'
368200150601    2c                   select
368300150403     c* FD escluso dai contatti
368400150601    2c                   when      §B4LTFD= 'N'
368500150403     c                   eval      d98spcdee='FD3'
368600150403     c                   clear                   wcaldpcee
368700150403     c                   movel     ' '           DEEinDIST         1
368800150403
368900150403     c* FD non contattato ( o da ricontattare)
369000150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
369100150401     c                             olv82sts<>'1'
369200150323     c                   clear                   wcaldpcee
369300150323     c                   movel     ' '           DEEinDIST         1
369400150323     c                   eval      d98spcdee='FFD'
369500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
369600150424     c                   eval      wpodpcee=wpoffi
369700150403
369800150403     c* FD contattato e     IN distinta
369900150601    2c                   when      d98cons='S'
370000150401     c                   eval      d98spcdee='FD2'
370100150401     c                   clear                   wcaldpcee
370200150401     c                   movel     ' '           DEEinDIST         1
370300150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
370400150424     c                   eval      wpodpcee=wpoffi
370500150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
370600150427     c*  ritirare la merce
370700150427     C                   MOVEL     'C'           WTC                            *PARTENZA
370800150427     C                   MOVEL     'C'           WIC
370900150427     C                   Z-ADD     D98ddc        WDAT
371000150427     C                   EXSR      DATORE
371100150427     c                   z-add     wore          wr4poi
371200150427     c                   z-add     24            wr4noi
371300150427     C                   EXSR      RO4CDP
371400150427     c                   z-add     wr4poo        wore
371500150427     c                   exsr      OREDAT
371600150427     c                   movel     wdat          D98DTCTD
371700150601   x2c                   other
371800150403     c* FD contattato e NON in distinta
371900150331     c                   eval      d98spcdee='FD1'
372000150408     c                   eval      wpodpcee=wpoffi
372100150331     c                   clear                   wcaldpcee
372200150331     c                   movel     ' '           DEEinDIST         1
372300150601    2c                   endsl
372400150331     c
372500150331     c                   endif
372600150403     c
372700150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
372800150601    2c                   if        (d98tc1='A' or d98tc2='A' or
372900150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
373000150601     c                             and d98ffd= ' '
373100150325     c                   clear                   wcaldpcee
373200150325     c                   movel     ' '           DEEinDIST         1
373300150601    3c                   if        §B4LBAS= 'N'
373400150427     c                   eval      d98spcdee='AP3'
373500150403     c                   else
373600150403     c                   eval      d98spcdee='APP'
373700150601    3c                   endif
373800150601    2c                   endif
373900150325     c*   se la bolla ha il C/Assegno verifico se da contattare
374000150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
374100150325     c                   clear                   wcaldpcee
374200150325     c                   movel     ' '           DEEinDIST         1
374300150601    3c                   if        §B4LBAS= 'N'
374400150427     c                   eval      d98spcdee='CA3'
374500150403     c                   else
374600150403     c                   eval      d98spcdee='CAS'
374700150601    3c                   endif
374800150601    2c                   endif
374900150325     c* Per questi 3 casi devo impostare la data contatto destinatario
375000150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
375100150325     c                             d98spcdee='CAS'
375200150601    2c                   select
375300150601    2c                   when      d98dti>0 and wmancatraz= ' '
375400150325     c                   movel     d98dti        d98dtctd
375500150403     c* se piene le ore già portate ad un giorno lavorativo
375600150403     c                   if        wpodti>0
375700150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
375800150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
375900150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
376000150403     C                   Z-ADD     WPODTI        W10POI            9 0
376100150403     C                   EXSR      R10FES
376200150403     c                   z-add     w10poo        wore
376300150403     C                   EXSR      OREDAT
376400150403     C                   movel     WDAT          D98dtctd
376500150403     c                   endif
376600150403     c
376700150601    2c                   when      d98dam>0
376800150421     C                   Z-ADD     D98dam        WDAT
376900150421     C                   EXSR      DATORE
377000150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
377100150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
377200150421     C                   Z-ADD     Wore          W10POI            9 0
377300150421     C                   EXSR      R10FES
377400150421     c                   z-add     w10poo        wore
377500150421     C                   EXSR      OREDAT
377600150421     C                   movel     WDAT          D98dtctd
377700150601   x2c                   other
377800150325     c* data teorica di arrivo tenendo conto dell'IMP
377900160426     c                   if        d98nsp>0
378000150325     c                   exsr      letbrv
378100160426     c                   endif
378200150325     c                   if        idDATA>0
378300150325     c                   exsr      CalcoloIMP
378400150325     c                   z-add     wr1poi        WORE
378500150325     c                   else
378600150325     c                   z-add     wpoptc        WORE
378700150325     c                   endif
378800150325     C                   EXSR      OREDAT
378900150325     c                   movel     wdat          d98dtctd
379000150601    2c                   endsl
379100150325     c
379200150325     c                   endif
379300150325     c
379400150601    1c                   endsl
379500140820     c
379600150113     c* Se la bolla non è in distinta calcolo la data se previsto
379700150220     c                   if        wcaldpcee='S' and
379800150220     c                             (d98cons<>'S'  or (d98cons='S' and
379900150220     c                             evemic<>'SI'))
380000150219     c
380100150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
380200150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
380300150219     c                   if        d98nsp>0 and wtrasm<>'D' and
380400150219     c                             d98dam=0
380500150219     c                   exsr      letBRV
380600150219     c                   endif
380700150219     c
380800150217     c                   eval      d98spcdee='PRG'
380900150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
381000150320     c                             d98tc1='S' or d98tc2='S')
381100150320     c                   eval      d98spcdee='APP'
381200150320     c                   endif
381300150320     c
381400150113     c                   exsr      cal_DEE
381500150113     c                   endif
381600150113     c
381700140820     C     WPOdpcee      IFEQ      *ZEROS
381800931202     C                   Z-ADD     *ZEROS        D98DEE
381900931202     C                   Z-ADD     *ZEROS        D98OEE
382000930604     C                   ELSE
382100140820     C                   Z-ADD     WPOdpcee      WORE
382200141021     C******             SUB       12            WORE
382300930604     C                   EXSR      OREDAT
382400931202     C                   Z-ADD     WDAT          D98DEE
382500090903     c                   if        d98tcc<>10
382600090903     C                   ADD       12            WRES
382700090903     c                   else
382800090903     C                   ADD       10            WRES
382900090903     c                   endif
383000090903
383100931202     C     WRES          MULT      100           D98OEE
383200090903     c
383300090903     c                   if        d98oee=1000
383400090903     c                   eval      d98oee=1030
383500090903     c                   endif
383600140925     C                   END
383700150311     c
383800150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
383900150311     c                   select
384000150311     c                   when      d98spcdee='OLP'
384100140820     c
384200150220     c* Se bolla in distinta --> data prevista = data distinta
384300150311     c                   when      deeindist='S'  and
384400150113     c                             d98cons='S'
384500150220
384600150225     c*  non la propongo senza MIC e se presente disguido o IMP
384700150220     c                   if        d98dee>d98ddc and evemic<>'SI'
384800150220     c                             and IDdata>0
384900150220     c                   else
385000150220
385100140820     c                   eval      d98dee=d98ddc
385200140925     c                   clear                   d98oee
385300150320     c* se la data è > di oggi stato PRV se non è in IMP
385400150320     c                   if        d98dee>wdaodi
385500150320     c                   if        d98spcdee<>'IMP'
385600150311     c                   eval      d98spcdee='PRV'
385700150320     c                   endif
385800150320     c
385900150225     c                   else
386000150217     c* se c'è il MIC
386100150217     c                   if        evemic='SI'
386200150217     c                   eval      d98spcdee='DDC'
386300150225     c                   endif
386400150217     c                   endif
386500150220     c                   endif
386600150217     c
386700150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
386800150722     c                   eval      d98spcdee='PRV'
386900150311     c                   endsl
387000931202     C*
387100931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
387200931202     C     WPODCI        IFEQ      *ZEROS
387300931202     C                   Z-ADD     *ZEROS        D98DCI
387400931202     C                   Z-ADD     *ZEROS        D98HCI
387500931202     C                   ELSE
387600931202     C                   Z-ADD     WPODCI        WORE
387700931202     C                   EXSR      OREDAT
387800931202     C                   Z-ADD     WDAT          D98DCI
387900090902     c                   if        d98tci<>10
388000931202     C                   ADD       12            WRES
388100090902     c                   else
388200090902     C                   ADD       10            WRES
388300090902     c                   endif
388400090902
388500931202     C     WRES          MULT      100           D98HCI
388600090901     c
388700090903     c                   if        d98hci=1000
388800090901     c                   eval      d98hci=1030
388900090901     c                   endif
389000090901     c
389100931202     C                   END
389200931202     C*
389300931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
389400931202     C     WPODCE        IFEQ      *ZEROS
389500931202     C                   Z-ADD     *ZEROS        D98DCE
389600931202     C                   Z-ADD     *ZEROS        D98HCE
389700931202     C                   ELSE
389800931202     C                   Z-ADD     WPODCE        WORE
389900931202//// C**************       SUB  12        WORE
390000931202     C                   EXSR      OREDAT
390100931202     C                   Z-ADD     WDAT          D98DCE
390200090902     c                   if        d98tcc >10
390300931202     C                   ADD       12            WRES
390400090902     c                   else
390500090902     C                   ADD       10            WRES
390600090902     c                   endif
390700090902     c
390800931202     C     WRES          MULT      100           D98HCE
390900090902     c
391000090903     c                   if        d98hce=1000
391100090902     c                   eval      d98hce=1030
391200090902     c                   endif
391300090902     c
391400931202     C                   END
391500960229     C*
391600960229     C                   ENDIF                                                  <-- 1 ---
391700930604     C*
391800930604     C                   ENDSR
391900150218     C********************************************************************
392000150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
392100150218     C********************************************************************
392200150218     c     LetBRV        BEGSR
392300150219     c                   clear                   bollaOK           1
392400150219     c                   clear                   IDnpg
392500150219     c                   clear                   IDdata
392600150219     c                   clear                   IDora
392700150219     c                   clear                   IDfgs
392800150219     c
392900150218     C     Kart          SETLL     FNART01L
393000150218     C     Kart          READE     FNART01L
393100151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
393200150219     c                   clear                   DISfgs
393300150219     c                   clear                   DISdata
393400150219     c                   clear                   DISnpg
393500150219     c                   clear                   DISora
393600150219     c                   clear                   IMPdata
393700150219     c                   clear                   IMPfgs
393800150508     c* per ogni collo verifico se in IMP o disguidato
393900150218     c* al primo che NON trovo, esco come --> OK
394000150218     c     kbrv          setll     fnbrv07l
394100150218     c     kbrv          reade     fnbrv07l
394200150219    2c                   dow       not %eof(fnbrv07l)
394300150219    3c                   if        brvatr='D'  or brvnpg=3
394400150218     c* prendo la data della spunta
394500150218     c                   clear                   fnlv53ds
394600150219     c                   eval      d53tla='L'
394700150218     C                   MOVEL     BRvNPG        D53NPG
394800150218     C                   Z-ADD     BRvNFV        D53NFV
394900150218     C                   MOVEL     BRvFGS        D53FGS
395000150218     C                   CALL      'FNLV53R'
395100150218     C                   PARM                    fnlv53ds
395200150218    4c                   if        d53err<>*blanks
395300150219    5c                   if        brvdcs>0
395400150218     c                   eval      d53dfv=brvdcs
395500150218     c                   else
395600150219    6c                   if        brvdfs>0
395700150218     c                   eval      d53dfv=brvdfs
395800150219    6c                   endif
395900150219    5c                   endif
396000150219    4c                   endif
396100150218     c
396200150219    4c                   select
396300150219     c* se si tratta di DISGUIDO
396400150218     c                   when      brvatr='D'
396500150219     c
396600150219    5c                   if        d53dfv=DISdata
396700150219     c* Verifico l'ora
396800150219    6c                   if        brvhcs>0 and brvhcs>DISora
396900150219     c                   eval      DISdata=d53dfv
397000150219     c                   eval      DISfgs =brvfgs
397100150219     c                   eval      DISnpg =brvnpg
397200150219     c                   eval      DISora =brvhcs
397300150219   x6c                   else
397400150219    7c                   if        brvhms>0 and brvhms>DISora
397500150219     c                   eval      DISdata=d53dfv
397600150219     c                   eval      DISfgs =brvfgs
397700150219     c                   eval      DISnpg =brvnpg
397800150219     c                   eval      DISora =brvhms
397900150219    7c                   endif
398000150219    6c                   endif
398100150219    5c                   endif
398200150219     c
398300150219    5c                   if        d53dfv>DISdata
398400150219     c                   eval      DISdata=d53dfv
398500150219     c                   eval      DISfgs =brvfgs
398600150219     c                   eval      DISnpg =brvnpg
398700150219     c                   eval      DISora =brvhcs
398800150219     c                   if        disora=0
398900150219     c                   eval      DISora =brvhms
399000150219     c                   endif
399100150219    5c                   endif
399200150219     c
399300151009     c* se si tratta di IMP
399400151009    4c                   when      d53spg='P' and d53dfv>impdata
399500151009     c                   eval      IMPdata=d53dfv
399600151009     c                   eval      IMPfgs =brvfgs
399700150219    4c                   endsl
399800150219    3c                   endif
399900151009
400000150219     c     kbrv          reade     fnbrv07l
400100150219    2c                   enddo
400200150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
400300150219     c*  esco dal ciclo: la bolla è OK
400400150219    2c                   if        impdata=0 and disdata=0
400500150219     c                   eval      bollaOK='S'
400600150219     c                   clear                   IDdata
400700150219     c                   clear                   IDfgs
400800150219     c                   clear                   IDora
400900150219     c                   clear                   IDnpg
401000150219   x2c                   else
401100150219     c* memorizzo la data più alta
401200150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
401300150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
401400150219     c                   eval      IDdata=IMPdata
401500150219     c                   eval      IDfgs=IMPfgs
401600150219     c                   eval      IDora=0
401700150219     c                   eval      IDnpg='P'
401800150219    3c                   endif
401900150219    3c                   if        (DISdata>0 and DISdata=IDdata
402000150219     c                             and IDnpg<>'P' and DISora>IDora) or
402100150219     c                             (DISdata>0 and DISdata>IDdata)
402200150219     c                   eval      IDdata=DISdata
402300150219     c                   eval      IDfgs=DISfgs
402400150219     c                   eval      IDora=DISora
402500150219     c                   eval      IDnpg=%char(DISnpg)
402600150219    3c                   endif
402700150219    2c                   endif
402800150218     c
402900150218     C     Kart          READE     FNART01L
403000150219    1c                   enddo
403100150219     c
403200150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
403300150508     c**                 if        d98duc> 0 and d98duc>IDdata
403400150508     c**                 eval      bollaOK='S'
403500150508     c**                 clear                   IDdata
403600150508     c**                 clear                   IDfgs
403700150508     c**                 clear                   IDora
403800150508     c**                 clear                   IDnpg
403900150508     c**                 endif
404000150219     c
404100150218     c
404200150218     C                   ENDSR
404300140825     C********************************************************************
404400140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
404500140825     C********************************************************************
404600140825     c     TRAS_DEI      BEGSR
404700140825     C     WPODEI        IFEQ      *ZEROS
404800140825     C                   Z-ADD     *ZEROS        D98DEI
404900140825     C                   Z-ADD     *ZEROS        D98OEI
405000140825     C                   ELSE
405100140825     C                   Z-ADD     WPODEI        WORE
405200140825     C                   SUB       12            WORE
405300140825     C                   EXSR      OREDAT
405400140825     c
405500140825     C                   Z-ADD     WDAT          D98DEI
405600140825     c                   if        d98tci<>10
405700140825     C                   ADD       12            WRES
405800140825     c                   else
405900140825     C                   ADD       10            WRES
406000140825     c                   endif
406100140825     c
406200140825     C     WRES          MULT      100           D98OEI
406300140825     c
406400140825     c                   if        d98oei=1000
406500140825     c                   eval      d98oei=1030
406600140825     c                   endif
406700140825     c
406800140825     C                   END
406900140825     c                   ENDSR
407000930604     C********************************************************************
407100930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
407200930604     C********************************************************************
407300930604     C     CHKFOR        BEGSR
407400930604     C*
407500930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
407600930604     C* E QUINDI FORZO LA CONFORMITA' A SI
407700941227     C     D98ERR        IFEQ      '0'
407800930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
407900930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
408000930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
408100930604     C                   MOVEL     '1'           D98ERR
408200930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
408300930604     C                   END
408400930604     C                   END
408500990212     C*
408600990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
408700990212     C* E QUINDI FORZO LA CONFORMITA' A SI
408800990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
408900990212     C     D98ERR        IFEQ      '0'
409000990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
409100141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
409200990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
409300990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
409400990212     C                   MOVEL     '1'           D98ERR
409500990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
409600990212     C                   END
409700990212     C                   END
409800990308     C*
409900990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
410000990308     C* E QUINDI FORZO LA CONFORMITA' A SI
410100100614     C**   D98ERR        IFEQ      '0'
410200100614     C**   WFBR          IFEQ      'S'                                          *RESO
410300100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
410400100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
410500100614     C**                 MOVEL     '1'           D98ERR
410600100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
410700100614     C**                 END
410800100614     C**                 END
410900930604     C*
411000930604     C                   ENDSR
411100990212     C********************************************************************
411200990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
411300990212     C********************************************************************
411400990212     C     CHKDAN        BEGSR
411500990212     C*
411600990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
411700150603     c                   movel     'N'           opendan           1
411800040809      *
411900040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
412000040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
412100040809      * legate alla spedizione che passo
412200040809     c                   clear                   fidn12ds
412300040809     c                   eval      i12aas = D98AAS
412400040809     c                   eval      i12lnp = D98LNP
412500040809     c                   eval      i12nrs = D98NRS
412600040809     c                   eval      i12nsp = D98NSP
412700040809     c                   eval      i12fel = 'E'
412800040809     c                   eval      i12fan = 'E'
412900040809     c*
413000040809     c                   call      'FIDN12R'
413100040809     c                   parm                    fidn12ds
413200040809     c*
413300040809     c* se non ci sono errori
413400040809     c                   if        o12err = ' '
413500040809     c* se numero di CA trovate maggiore di zero
413600040809     c                   if        o12nca > 0
413700040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
413800150603     c
413900150603     c***                z-add     1             aa                2 0
414000150603     c***                dow       aa <= o12nca
414100150603     C* APERTA
414200150603     C***  skdch(aa)     IFEQ      0
414300150603     c***                movel     'S'           opendan
414400150603     c***                endif
414500150603     c*
414600150603     c***                add       1             aa
414700150603     c***                enddo
414800150603     c
414900040809     c                   endif
415000040809     c                   endif
415100990212     C*
415200990212     C                   ENDSR
415300941228     C********************************************************************
415400941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
415500941228     C********************************************************************
415600941228     C     CALORP        BEGSR
415700941228     C*
415800941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
415900941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
416000941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
416100960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
416200950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
416300950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
416400950110     C                   ELSE
416500950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
416600950110     C                   ENDIF
416700941228     C                   EXSR      RO2COP
416800941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
416900941228     C* SPED. IL GIORNO DOPO
417000941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
417100941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
417200941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
417300941228     C*
417400941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
417500941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
417600941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
417700941228     C                   Z-ADD     WPODSP        WR2POI
417800950110     C                   Z-ADD     WPOPPC        WR2POF
417900941228     C                   EXSR      RO2COP
418000941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
418100941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
418200941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
418300941228     C*
418400941228     C                   ENDSR
418500930414     C********************************************************************
418600930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
418700930414     C********************************************************************
418800930414     C     CALORT        BEGSR
418900930414     C*
419000930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
419100931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
419200930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
419300931126     C                   Z-ADD     *ZEROS        W98HF
419400950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
419500950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
419600950110     C                   ELSE
419700950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
419800950110     C                   ENDIF
419900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
420000931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
420100930608     C                   ELSE
420200931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
420300930608     C                   END
420400931126     C                   EXSR      RO2COV
420500931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
420600930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
420700930414     C*
420800930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
420900930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
421000930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
421100931126     C                   Z-ADD     *ZEROS        W98HF
421200950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
421300931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
421400931126     C                   EXSR      RO2COV
421500931126     C                   Z-ADD     WR2NOV        W98RTC
421600930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
421700930414     C*
421800930414     C                   ENDSR
421900930414     C********************************************************************
422000930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
422100930414     C********************************************************************
422200930414     C     CALORC        BEGSR
422300930511     C*
422400930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
422500930511     C* I TEMPI DI RITARDO AD OGGI
422600930511     C     WPODCM        IFEQ      *ZEROS
422700960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
422800930511     C                   END
422900940117     C*
423000940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
423100940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
423200940117     C* ALLA DATA DI FINE CONSEGNABILITA'
423300940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
423400940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
423500940118££££ C     WPODFC        IFNE      *ZEROS
423600940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
423700940118££££ C                   END
423800930414     C*
423900930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
424000930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
424100930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
424200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
424300931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
424400930608     C                   ELSE
424500931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
424600930608     C                   END
424700931126     C                   Z-ADD     WPODCM        WR5POF            9 0
424800931126     C                   EXSR      RO5COV
424900931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
425000930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
425100930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
425200060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
425300060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
425400930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
425500930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
425600930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
425700930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
425800930414     C*
425900930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
426000930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
426100930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
426200931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
426300931126     C                   Z-ADD     WPODCM        WR5POF            9 0
426400931126     C                   EXSR      RO5COV
426500931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
426600930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
426700930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
426800060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
426900060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
427000930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
427100930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
427200930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
427300930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
427400930414     C*
427500930414     C                   ENDSR
427600930414     C********************************************************************
427700930414     C*   CALORR - CALCOLA ORE REALI DI RESA
427800930414     C********************************************************************
427900930414     C     CALORR        BEGSR
428000930414     C*
428100950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
428200950110     C                   ADD       W98RTI        W98RRI
428300941228     C                   ADD       W98RCI        W98RRI
428400950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
428500950110     C                   ADD       W98RTC        W98RRC
428600941228     C                   ADD       W98RCC        W98RRC
428700930414     C*
428800930414     C                   ENDSR
428900930420     C********************************************************************
429000930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
429100930420     C********************************************************************
429200930420     C     CALDIF        BEGSR
429300930420     C*
429400941228     C* PARTENZA
429500941228     C     W98RPI        SUB       D98TPI        W98FPI
429600941228     C     W98RPC        SUB       D98TPC        W98FPC
429700941228     C*
429800930420     C* TRAZIONE
429900930423     C     W98RTI        SUB       D98TTI        W98FTI
430000930423     C     W98RTC        SUB       D98TTC        W98FTC
430100930420     C*
430200930420     C* CONSEGNA
430300090827     C***  W98RCI        SUB       D98TCI        W98FCI
430400090827     C     W98RCI        SUB       w99TCI        W98FCI
430500090827     C***  W98RCC        SUB       D98TCC        W98FCC
430600090827     C     W98RCC        SUB       w99TCC        W98FCC
430700930420     C*
430800930420     C* RESA
430900090921     C***  W98RRI        SUB       D98TRI        W98FRI
431000090921     C     W98RRI        SUB       w99TRI        W98FRI
431100090921     c
431200090921     C***  W98RRC        SUB       D98TRC        W98FRC
431300090921     C     W98RRC        SUB       w99TRC        W98FRC
431400930420     C*
431500930420     C                   ENDSR
431600930420     C********************************************************************
431700930420     C*   CALCNF - CALCOLA LE CONFORMITA'
431800930420     C********************************************************************
431900930420     C     CALCNF        BEGSR
432000941228     C*
432100941228     C*-------------------------------------------------------------------
432200941228     C* CONFORMITA' PARTENZA  INTERNA
432300941228     C*-------------------------------------------------------------------
432400941228     C                   Z-ADD     W98FPI        W98NPI
432500941228     C     W98FPI        IFLE      *ZEROS
432600941228     C                   MOVEL     'S'           D98CPI
432700941228     C                   ELSE
432800941228     C                   MOVEL     'N'           D98CPI
432900941228     C                   END
433000941228     C*
433100941228     C*-------------------------------------------------------------------
433200941228     C* CONFORMITA' PARTENZA  CLIENTE
433300941228     C*-------------------------------------------------------------------
433400941228     C                   Z-ADD     W98FPC        W98NPC
433500941228     C     W98FPC        IFLE      *ZEROS
433600941228     C                   MOVEL     'S'           D98CPC
433700941228     C                   ELSE
433800941228     C                   MOVEL     'N'           D98CPC
433900941228     C                   END
434000930420     C*
434100930420     C*-------------------------------------------------------------------
434200930420     C* CONFORMITA' TRAZIONE  INTERNA
434300930420     C*-------------------------------------------------------------------
434400930510     C                   Z-ADD     W98FTI        W98NTI
434500930423     C     W98FTI        IFLE      *ZEROS
434600930420     C                   MOVEL     'S'           D98CTI
434700930420     C                   ELSE
434800930420     C                   MOVEL     'N'           D98CTI
434900930420     C                   END
435000930420     C*
435100930420     C*-------------------------------------------------------------------
435200930420     C* CONFORMITA' TRAZIONE  CLIENTE
435300930420     C*-------------------------------------------------------------------
435400930510     C                   Z-ADD     W98FTC        W98NTC
435500930423     C     W98FTC        IFLE      *ZEROS
435600930420     C                   MOVEL     'S'           D98CTC
435700930420     C                   ELSE
435800930420     C                   MOVEL     'N'           D98CTC
435900930420     C                   END
436000930420     C*
436100930420     C*-------------------------------------------------------------------
436200930420     C* CONFORMITA' CONSEGNA  INTERNA
436300930420     C*-------------------------------------------------------------------
436400150226     c                   if        wmancatraz='S'
436500150226     c                   clear                   d98dti
436600150226     c                   clear                   d98hti
436700150226     c                   endif
436800150226     c
436900930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
437000930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
437100930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
437200050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
437300050914     c                   if        consideraora='S'
437400050914     c     w98fci        sub       24            w98nci
437500050914     c                   add       24            d98hrc
437600050914     c                   else
437700930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
437800050914     c                   endif
437900080116     c
438000110923     c                   select
438100080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
438200080116     c*  --> trasformo il ritardo in 0
438300090827     c                   when      d98cca='2'
438400080116     c                   z-add     0             w98nci
438500090827
438600080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
438700080116     c*  e sto leggendo la bolla in arrivo
438800080116     c*  --> trasformo il ritardo = 0
438900090827     c                   when      d98cca='1' and d98tbo='A'
439000080116     c                   z-add     0             w98nci
439100090902     c
439200090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
439300090902     c                   when      wmancaTRAZ='S'
439400090902     c                   z-add     0             w98nci
439500090902     c                   clear                   d98dti
439600090902     c                   clear                   d98hti
439700110923     c
439800110923     c* Ignoro ritardo se pefect order
439900130221     c                   when      d98xco >*zeros
440000110923     c                   z-add     0             w98nci
440100090827     c
440200090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
440300091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
440400091014     c*  controllo ora soglia  consegna da tabella OSC)
440500091009     c                   when      d98tci=10   and
440600091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
440700091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
440800091014     c*
440900091014     c                             d98hci=1030
441000091014     C                   MOVEL     'OSC  '       WR3TAB
441100091014     C                   eval      WR3HHI=d98hmc
441200091014     C                   EXSR      RO3ORE
441300131218
441400091014     c                   if        wr3matpom='M' and d98hmc>1030
441500131218     c                   exsr      ctrLAVH1030
441600131218
441700131218     c                   if        wlav1030<>'S'
441800091014     c                   add       12            w98nci
441900091014     c                   endif
442000131218     c                   endif
442100090928     c
442200090827     c                   endsl
442300050914     c
442400930524     C                   ELSE                                                   <-- 1 -->
442500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
442600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
442700930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
442800931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
442900930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
443000930608     C                   ELSE
443100930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
443200930608     C                   END
443300930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
443400930525     C                   EXSR      RO2COV
443500930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
443600930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
443700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
443800930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
443900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
444000930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
444100930608     C                   ELSE
444200930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
444300930608     C                   END
444400931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
444500930525     C                   EXSR      RO2COV
444600930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
444700930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
444800930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
444900930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
445000930525     C* E' IL RITARDO
445100930610     C     WNOCRI        SUB       W98RCI        W98NCI
445200050901     c
445300050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
445400050901     c*  --> trasformo il ritardo in anticipo
445500050901     c                   if        d98cca='2'
445600080116     c***                z-sub     w98nci        w98nci
445700080116     c                   z-add     0             w98nci
445800050901     c                   endif
445900111018     c* Ignoro ritardo se pefect order
446000130221     c                   if        d98xco >*zeros
446100111018     c                   z-add     0             w98nci
446200111018     c                   endif
446300061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
446400061121     c*  e sto leggendo la bolla in arrivo
446500061121     c*  --> trasformo il ritardo in anticipo
446600061121     c                   if        d98cca='1' and d98tbo='A'
446700080116     c***                z-sub     w98nci        w98nci
446800080116     c                   z-add     0             w98nci
446900061121     c                   endif
447000070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
447100070827     c*  mittente, --> consegnato in anticipo
447200070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
447300140403     c                   if        Callar5=' '
447400070827     c                   exsr      LeggiAR5
447500140403     c                   endif
447600070827     c
447700070827     c                   if        dcrpart<>'S'
447800070824     c                   z-sub     w98nci        w98nci
447900070824     c                   endif
448000070827     c
448100070824     c                   endif
448200050907     c
448300050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
448400050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
448500050907     c*  lo trasformo in anticipo perchè non sono in ritardo
448600050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
448700050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
448800050916     c***                if        d98dfc>0
448900070828     c                   if        d98dcm=0  and w98nci>0
449000050907     c                   z-sub     w98nci        w98nci
449100050907     c                   sub       oreILoDOPO    w98nci
449200050907     c                   endif
449300050901     c
449400930525     C                   ELSE                                                   <-- 2 -->
449500070521     c* non do ritardo se la consegna reale cade nel giorno
449600070521     c*  della consegna richiesta se presente IL
449700100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
449800070521     c                             wpodcm<=wpocr1
449900070521     c                   clear                   w98nci
450000100224     c                   else                                                   ---2a -->
450100930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
450200090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
450300090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
450400930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
450500930525     C* E' IL RITARDO O L'ANTICIPO
450600930525     C     W98RCI        SUB       WNOCRF        W98NCI
450700070115     c
450800070115     c* non do ritardo se comunque la consegna reale cade nel giorno
450900070115     c*  della consegna richiesta se presente IL
451000070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
451100070521     c***                          wpodcm<=wpocr1  and w98nci>0
451200070521     c***                clear                   w98nci
451300070521     c***                endif
451400070115     c
451500930525     C                   ELSE                                                   <-- 3 -->
451600930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
451700930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
451800930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
451900930525     C                   END                                                    <-- 3 ---
452000100224     C                   END                                                    <-- 2a---
452100100224
452200100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
452300100224     c                   select
452400100224     c                   when      d98cca='2'
452500100224     c                   z-add     0             w98nci
452600100224
452700100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
452800100224     c*  e sto leggendo la bolla in arrivo
452900100224     c                   when      d98cca='1' and d98tbo='A'
453000100224     c                   z-add     0             w98nci
453100100224     c                   endsl
453200100224     c
453300100224     C                   END                                                    <-- 2 ---
453400070521     C                   END                                                    <-- 1 ---
453500070521     c
453600970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
453700970915     C*/         WAZNCI    IFEQ 'S'
453800970915     C*/         W98NCI    ANDLT*ZEROS
453900970915     C*/                   Z-ADD*ZEROS    W98NCI
454000970915     C*/                   ENDIF
454100970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
454200050922     c* lo faccio solo se non c'e' la data cons richiesta
454300970915     C     D98DTI        IFEQ      *ZEROS
454400050922     c     d98dcr        andeq     *zeros
454500970915     C                   Z-ADD     *ZEROS        W98NCI
454600970915     C                   ENDIF
454700930524     C*
454800930524     C* CONTROLLO CONFORMITA
454900930423     C     W98NCI        IFLE      *ZEROS
455000930524     C                   MOVEL     'S'           D98CCI
455100930420     C                   ELSE
455200930524     C                   MOVEL     'N'           D98CCI
455300930420     C                   END
455400930420     C*
455500930420     C*-------------------------------------------------------------------
455600930420     C* CONFORMITA' CONSEGNA  CLIENTE
455700930420     C*-------------------------------------------------------------------
455800930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
455900930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
456000930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
456100050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
456200050914     c                   if        consideraora='S'
456300050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
456400050914     c                   else
456500050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
456600050914     c                   endif
456700080116     c
456800080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
456900080116     c*  --> trasformo il ritardo =0
457000090827     c                   select
457100090827     c                   when      d98cca='2'
457200080116     c                   z-add     0             w98ncc
457300090827
457400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
457500080116     c*  e sto leggendo la bolla in arrivo
457600080116     c*  --> trasformo il ritardo =0
457700090827     c                   when      d98cca='1' and d98tbo='A'
457800080116     c                   z-add     0             w98ncc
457900090902     c
458000090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
458100090902     c                   when      wmancaTRAZ='S'
458200090902     c                   z-add     0             w98ncc
458300110923     c* Ignoro ritardo se Perfect Order
458400130221     c                   when      d98xco >*zeros
458500110923     c                   z-add     0             w98ncc
458600090902     c
458700090827     c
458800090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
458900090827     c* sono  in ritardo di 12 ore
459000091009     c                   when      d98tcc=10  and
459100091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
459200091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
459300091014     c*
459400091014     c                             d98hce=1030
459500091014     C                   MOVEL     'OSC  '       WR3TAB
459600091014     C                   eval      WR3HHI=d98hmc
459700091014     C                   EXSR      RO3ORE
459800091014     c                   if        wr3matpom='M' and d98hmc>1030
459900131218     c                   exsr      ctrLAVH1030
460000131218
460100131218     c                   if        wlav1030<>'S'
460200131218     c                   add       12            w98ncc
460300131218     c                   endif
460400131218     c****               add       12            w98ncc
460500091014     c                   endif
460600131218     c
460700091014     c***                z-add     12            w98ncc
460800090827     c                   endsl
460900050914     c
461000930524     C                   ELSE                                                   <-- 1 -->
461100930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
461200930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
461300930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
461400930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
461500930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
461600930525     C                   EXSR      RO2COV
461700930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
461800930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
461900930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
462000930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
462100930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
462200931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
462300930525     C                   EXSR      RO2COV
462400930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
462500930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
462600930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
462700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
462800930525     C* E' IL RITARDO
462900930525     C     WNOCRI        SUB       W98RCC        W98NCC
463000050901     c
463100050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
463200050901     c*  --> trasformo il ritardo in anticipo
463300050901     c                   if        d98cca='2'
463400080116     c***                z-sub     w98ncc        w98ncc
463500080116     c                   z-add     0             w98ncc
463600050901     c                   endif
463700111018     c* Ignoro ritardo se pefect order
463800130221     c                   if        d98xco >*zeros
463900111018     c                   z-add     0             w98ncc
464000111018     c                   endif
464100061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
464200061121     c*  e sto leggendo la bolla in arrivo
464300061121     c*  --> trasformo il ritardo in anticipo
464400061121     c                   if        d98cca='1' and d98tbo='A'
464500080116     c***                z-sub     w98ncc        w98ncc
464600080116     c                   z-add     0             w98ncc
464700061121     c                   endif
464800070824     c
464900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
465000070827     c*  mittente, --> consegnato in anticipo
465100070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
465200070827     c* SBR per vedere se tassativa
465300070827     c                   if        Callar5=' '
465400070827     c                   EXSR      LeggiAR5
465500070827     c                   endif
465600070827     c                   if        dcrpart<>'S'
465700070824     c                   z-sub     w98ncc        w98ncc
465800070824     c                   endif
465900070824     c                   endif
466000050907     c
466100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
466200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
466300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
466400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
466500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
466600050916     c***                if        d98dfc>0
466700070828     c                   if        d98dcm=0 and w98ncc>0
466800050907     c                   z-sub     w98ncc        w98ncc
466900050907     c                   sub       oreILoDOPO    w98ncc
467000050907     c                   endif
467100050901     c
467200930525     C                   ELSE                                                   <-- 2 -->
467300070521     c* Non do ritardo se la consegna reale cade nel giorno
467400070521     c*  della consegna richiesta se presente IL
467500100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
467600070521     c                             wpodcm<=wpocr2
467700070521     c                   clear                   w98ncc
467800100224     c                   else                                                   <--2a -->
467900070521     c
468000930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
468100090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
468200090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
468300930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
468400930525     C* E' IL RITARDO O L'ANTICIPO
468500930525     C     W98RCC        SUB       WNOCRF        W98NCC
468600070115     c
468700070115     c* non do ritardo se comunque la consegna reale cade nel giorno
468800070115     c*  della consegna richiesta se presente IL
468900070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
469000070521     c***                          wpodcm<=wpocr2 and w98ncc>0
469100070521     c***                clear                   w98ncc
469200070521     c***                endif
469300070115     c
469400930525     C                   ELSE                                                   <-- 3 -->
469500930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
469600930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
469700930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
469800930525     C                   END                                                    <-- 3 ---
469900100224     C                   END                                                    <-- 2a---
470000100224
470100100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
470200100224     c                   select
470300100224     c                   when      d98cca='2'
470400100224     c                   z-add     0             w98nci
470500100224
470600100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
470700100224     c*  e sto leggendo la bolla in arrivo
470800100224     c                   when      d98cca='1' and d98tbo='A'
470900100224     c                   z-add     0             w98nci
471000100224     c                   endsl
471100100224     c
471200100224     C                   END                                                    <-- 2 ---
471300070521     C                   END                                                    <-- 1 ---
471400930524     C*
471500930524     C* CONTROLLO CONFORMITA
471600930423     C     W98NCC        IFLE      *ZEROS
471700930524     C                   MOVEL     'S'           D98CCC
471800930420     C                   ELSE
471900930524     C                   MOVEL     'N'           D98CCC
472000930420     C                   END
472100930420     C*
472200930420     C*-------------------------------------------------------------------
472300930420     C* CONFORMITA' RESA INTERNA
472400930420     C*-------------------------------------------------------------------
472500930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
472600930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
472700930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
472800050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
472900050914     c                   if        consideraora='S'
473000050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
473100050914     c                   else
473200050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
473300050914     c                   endif
473400080116     c
473500080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
473600090827     c                   select
473700090827     c                   when      d98cca='2' and w98nri>0
473800080116     c                   z-add     0             w98nri
473900090827     c
474000080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
474100080116     c*  e sto leggendo la bolla in arrivo
474200090827     c                   when      d98cca='1' and d98tbo='A' and
474300080116     c                                            w98nri>0
474400080116     c                   z-add     0             w98nri
474500110923     c
474600110923     c* Ignoro ritardo se Perfect Order
474700130221     c                   when      d98xco >*zeros
474800110923     c                   z-add     0             w98nri
474900110923     c
475000090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
475100090827     c* sono  in ritardo di 12 ore
475200091009     c                   when      d98tci=10  and
475300091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
475400091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
475500091014     c*
475600091014     c                             d98hci=1030
475700091014     C                   MOVEL     'OSC  '       WR3TAB
475800091014     C                   eval      WR3HHI=d98hmc
475900091014     C                   EXSR      RO3ORE
476000091014     c                   if        wr3matpom='M' and d98hmc>1030
476100131218     c                   exsr      ctrLAVH1030
476200131218
476300131218     c                   if        wlav1030<>'S'
476400091014     c                   add       12            w98nri
476500091014     c                   endif
476600131218     c                   endif
476700091014     c
476800091014     c***                z-add     12            w98nri
476900090827     c                   endsl
477000090827
477100930524     C                   ELSE                                                   <-- 1 -->
477200930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
477300930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
477400930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
477500931206     C     WPODCM        ANDLE     WPOCR1
477600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
477700930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
477800930525     C                   Z-ADD     WPODCM        WR2POI            9 0
477900931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
478000930525     C                   EXSR      RO2COV
478100930525     C     WR2NOV        IFNE      *ZEROS
478200930525     C     WR2NOV        MULT      -1            W98NRI
478300930525     C                   END
478400930524     C                   ELSE                                                   <-- 2 -->
478500930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
478600930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
478700930524     C     WPODCM        IFLT      WPOCRI
478800930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
478900930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
479000930524     C                   ELSE
479100931206     C                   Z-ADD     WPOCR1        WPORFI
479200930524     C                   Z-ADD     WPODCM        WPORFF
479300930524     C                   END
479400930524     C* CALCOLO ORE DI DIFFERENZA
479500930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
479600930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
479700930524     C                   Z-ADD     WPORFI        WR2POI            9 0
479800930524     C                   Z-ADD     WPORFF        WR2POF            9 0
479900930524     C                   EXSR      RO2COV
480000930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
480100050902     c
480200050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
480300100224     c                   if        d98cca='2' and w98nri>0
480400100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
480500080116     c***                z-sub     w98nri        w98nri
480600080116     c                   z-add     0             w98nri
480700050902     c                   endif
480800111018     c* Ignoro ritardo se pefect order
480900130221     c                   if        d98xco >*zeros
481000111018     c                   z-add     0             w98nri
481100111018     c                   endif
481200061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
481300061121     c*  e sto leggendo la bolla in arrivo
481400100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
481500100224     c***                if        d98cca='1' and d98tbo='A' and
481600100224     c***                                         w98nri>0 and wpodcm<wpocri
481700080116     c***                z-sub     w98nri        w98nri
481800080116     c                   z-add     0             w98nri
481900061121     c                   endif
482000070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
482100070827     c*  mittente, --> consegnato in anticipo
482200070828     c                   if        d98tcr=' ' and d98nsp>0  and
482300070828     c                                            w98nri>0 and wpodcm<wpocri
482400070827     c                   if        CallAR5=' '
482500070827     c                   EXSR      LeggiAR5
482600070824     c                   endif
482700070827     c                   if        dcrpart<>'S'
482800070824     c                   z-sub     w98nri        w98nri
482900070824     c                   endif
483000070824     c                   endif
483100050907     c
483200050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
483300050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
483400050907     c*  lo trasformo in anticipo perchè non sono in ritardo
483500050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
483600050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
483700050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
483800050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
483900050907     c                   z-sub     w98nri        w98nri
484000050907     c                   sub       oreILoDOPO    w98nri
484100050907     c                   endif
484200050907     c
484300050907     C                   END                                                    <-- 2 ---
484400930524     C                   END                                                    <-- 1 ---
484500930420     C* CALCOLO LE ORE DI RESA
484600930420     C* CONTROLLO CONFORMITA
484700930423     C     W98NRI        IFLE      *ZEROS
484800930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
484900930420     C                   ELSE
485000930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
485100930420     C                   END
485200930420     C*
485300930420     C*-------------------------------------------------------------------
485400930420     C* CONFORMITA' RESA CLIENTE
485500930420     C*-------------------------------------------------------------------
485600930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
485700930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
485800930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
485900050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
486000050914     c                   if        consideraora='S'
486100050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
486200050914     c                   else
486300050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
486400050914     c                   endif
486500090827     c
486600080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
486700090827     c                   select
486800090827     c                   when      d98cca='2' and w98nrc>0
486900080116     c                   z-add     0             w98nrc
487000090827
487100080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
487200080116     c*  e sto leggendo la bolla in arrivo
487300090827     c                   when      d98cca='1' and d98tbo='A' and
487400080116     c                                            w98nrc>0
487500080116     c                   z-add     0             w98nrc
487600110923     c
487700110923     c* Ignoro ritardo se Perfect Order
487800130221     c                   when      d98xco >*zeros
487900110923     c                   z-add     0             w98nrc
488000090827     c
488100090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
488200090827     c* sono  in ritardo di 12 ore
488300091009     c                   when      d98tcc=10  and
488400091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
488500091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
488600091014     c                             d98hce=1030
488700091014     C                   MOVEL     'OSC  '       WR3TAB
488800091014     C                   eval      WR3HHI=d98hmc
488900091014     C                   EXSR      RO3ORE
489000091014     c                   if        wr3matpom='M' and d98hmc>1030
489100131218     c                   exsr      ctrLAVH1030
489200131218
489300131218     c                   if        wlav1030<>'S'
489400091014     c                   add       12            w98nrc
489500091014     c                   endif
489600131218     c                   endif
489700091014     c
489800091014     c**                 z-add     12            w98nrc
489900090827     c                   endsl
490000090827     c
490100930524     C                   ELSE                                                   <-- 1 -->
490200930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
490300930524     C* DI DIFFERENZA SONO UGUALI A ZERO
490400930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
490500931206     C     WPODCM        ANDLE     WPOCR2
490600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
490700930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
490800930525     C                   Z-ADD     WPODCM        WR2POI            9 0
490900931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
491000930525     C                   EXSR      RO2COV
491100930525     C     WR2NOV        IFNE      *ZEROS
491200930525     C     WR2NOV        MULT      -1            W98NRC
491300930525     C                   END
491400930524     C                   ELSE                                                   <-- 2 -->
491500930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
491600930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
491700930524     C     WPODCM        IFLT      WPOCRI
491800930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
491900930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
492000930524     C                   ELSE
492100931206     C                   Z-ADD     WPOCR2        WPORFI
492200930524     C                   Z-ADD     WPODCM        WPORFF
492300930524     C                   END
492400930524     C* CALCOLO ORE DI DIFFERENZA
492500930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
492600930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
492700930524     C                   Z-ADD     WPORFI        WR2POI            9 0
492800930524     C                   Z-ADD     WPORFF        WR2POF            9 0
492900930524     C                   EXSR      RO2COV
493000930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
493100050902     c
493200050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
493300100224     c                   if        d98cca='2' and w98nrc>0
493400100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
493500080116     c***                z-sub     w98nrc        w98nrc
493600080116     c                   z-add     0             w98nrc
493700050902     c                   endif
493800111018     c* Ignoro ritardo se pefect order
493900130221     c                   if        d98xco >*zeros
494000111018     c                   z-add     0             w98nrc
494100111018     c                   endif
494200070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
494300061121     c*  e sto leggendo la bolla in arrivo
494400100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
494500100224     c***                if        d98cca='1' and d98tbo='A' and
494600100224     c***                                         w98nrc>0 and wpodcm<wpocri
494700080116     c***                z-sub     w98nrc        w98nrc
494800080116     c                   z-add     0             w98nrc
494900061121     c                   endif
495000070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
495100070827     c*  mittente, --> consegnato in anticipo
495200070828     c                   if        d98tcr=' ' and d98nsp>0 and
495300070828     c                                            w98nrc>0 and wpodcm<wpocri
495400070827     c                   if        CallAR5=' '
495500070827     c                   EXSR      LeggiAR5
495600070827     c                   endif
495700070827     c                   if        dcrpart<>'S'
495800070824     c                   z-sub     w98nrc        w98nrc
495900070824     c                   endif
496000070824     c                   endif
496100050907     c
496200050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
496300050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
496400050907     c*  lo trasformo in anticipo perchè non sono in ritardo
496500050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
496600050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
496700050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
496800050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
496900050907     c                   z-sub     w98nrc        w98nrc
497000050907     c                   sub       oreILoDOPO    w98nrc
497100050907     c                   endif
497200050907     c
497300050907     C                   END                                                    <-- 2 ---
497400930524     C                   END                                                    <-- 1 ---
497500930420     C* CALCOLO LE ORE DI RESA
497600930420     C* CONTROLLO CONFORMITA
497700930423     C     W98NRC        IFLE      *ZEROS
497800930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
497900930420     C                   ELSE
498000930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
498100930420     C                   END
498200930420     C*
498300930420     C                   ENDSR
498400070827     c
498500131218     c********************************************************************
498600131218     c     CtrlavH1030   BEGSR
498700131218     c********************************************************************
498800131218     c                   clear                   wlav1030          1
498900140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
499000131218     c                   z-add     1             xx                4 0
499100131218     c                   dow       evc(xx)<>*blanks
499200140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
499300140127     c                   if        evd(xx)=d98dcm
499400131218     c                   if        evo(xx)<=1030
499500131218     c                   eval      wlav1030='S'
499600131218     c                   leave
499700131218     c                   endif
499800131218     c                   endif
499900131218     c                   add       1             xx
500000131218     c                   enddo
500100131218     c                   ENDSR
500200070827     c********************************************************************
500300070827     c* Controllo record GEN di FIAR5
500400070827     c********************************************************************
500500070827     c     LeggiAR5      BEGSR
500600070827     c                   clear                   DCRPART           1
500700141104     c                   clear                   dar5gen
500800141104     c                   clear                   gc1PART           2
500900141104     c                   clear                   gc2PART           2
501000070827     c     kar5          chain     fiar501l
501100070827    1c                   if        not %found
501200070827     c                   eval      dcrpart='S'
501300141104     c                   eval      gc1part=d98gc1
501400141104     c                   eval      gc2part=d98gc2
501500070827   x1c                   else
501600070827     c* i dati devono essere quelli immessi dalla partenza
501700070827     c                   movel     ar5uni        dar5gen
501800070827     c
501900070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
502000070827     c*  ma il record GEN è stato scritto per altri motivi
502100070827    2c                   if        §ar5mdp='S'
502200070827     c                   movel     d98dcr        alfadcr           8
502300141104     c                   eval      gc1part=§ar5gc1
502400141104     c                   eval      gc2part=§ar5gc2
502500070827     c
502600070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
502700070827     c                   eval      dcrpart='S'
502800070827    3c                   endif
502900070827     c
503000070827   x2c                   else
503100070827     c                   eval      dcrpart='S'
503200141106     c                   eval      gc1part=d98gc1
503300141106     c                   eval      gc2part=d98gc2
503400070827    2c                   endif
503500070827    1c                   endif
503600070827     c
503700140402     c* Memorizzo il flag pickup 2
503800140402     c                   eval      wpick2=§AR5PK2
503900140402     c
504000150401     c*
504100150414     c**                 if        d98cas>0 or d98ffd<>' ' or
504200150414     c**                           d98tc1='A'  or d98tc2='A' or
504300150414     c**                           d98tc1='S'  or d98tc2='S'
504400160331     c                   eval      kar4trc='L'
504500150401     c     kar4          chain     fiar401l
504600160420    1c                   if        %found(fiar401l)
504700150401     c                   eval      dsbl4L=ar4not
504800160420   x1c                   else
504900160420    1c                   endif
505000160420
505100150401     c* Solo per fermo deposito verifico se inviato alert
505200150401     c                   if        d98ffd<>' '
505300150401     c                   clear                   fnlv82ds
505400150401     c                   eval      ILV82AAS=d98aas
505500150401     c                   eval      ILV82LNP=d98lnp
505600150401     c                   eval      ILV82NRS=d98nrs
505700150401     c                   eval      ILV82NSP=d98nsp
505800150401     c                   eval      ILV82TIPA='FD'
505900150401     c                   call      'FNLV82R'
506000150401     c                   parm                    fnlv82ds
506100150401     c                   endif
506200150414     c**                 endif
506300150401
506400150401     c                   eval      callar5='S'
506500160331     c
506600160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
506700160331     c                   if        %lookup(d98lnp:fildpd)>0
506800160331     c                   eval      kar4trc='I'
506900160331     c     kar4          chain     fiar401l
507000160331     c                   if        %found(fiar401l)
507100160331     c                   eval      dsbl4I=ar4not
507200160331     c                   endif
507300160401     c                   if        §b4trd='1' or §b4trd='2'
507400160401     c                   eval      wVP='S'
507500160401     c                   endif
507600160401     c
507700160331     c                   endif
507800150401
507900070827     c                   ENDSR
508000930913     C********************************************************************
508100930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
508200930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
508300930913     C*            DATA/ORA (FINE RANGE)
508400930913     C********************************************************************
508500930913     C     CALCNS        BEGSR
508600930914     C*
508700930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
508800930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
508900930914     C     WPODTI        IFEQ      *ZEROS
509000960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
509100930914     C                   ELSE
509200930914     C                   Z-ADD     WPODTI        WPOIRC
509300930914     C                   END
509400930914     C*
509500930914     C* SE NON ANCORA CONSEGNATA
509600930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
509700050922     c* in realtà nella data consegna c'e' la data fine calcolo
509800050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
509900930914     C     WPODCM        IFEQ      *ZEROS
510000050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
510100930914     C                   ELSE
510200930916     C                   Z-ADD     WPODCM        WPOFRC
510300930914     C                   END
510400930914     C*
510500930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
510600930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
510700930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
510800930914     C     WPODFC        ANDLT     WPOIRC
510900930914     C                   ELSE
511000930914     C*
511100930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
511200930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
511300930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
511400930914     C     WPODFC        ANDGT     WPOFRC
511500930914     C                   ELSE
511600930916     C*
511700930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
511800930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
511900930916     C     WPODFC        ANDGT     WPOFRC
512000930916     C                   ELSE
512100930916     C*
512200930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
512300930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
512400930916     C     WPODFC        ANDLT     WPOIRC
512500930916     C                   ELSE
512600930914     C*
512700930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
512800930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
512900930914     C     WPODIC        IFNE      *ZEROS
513000930914     C     WPODIC        ANDGT     WPOFRC
513100930914     C                   Z-ADD     WPOFRC        WPODIC
513200930914     C                   ENDIF
513300930914     C*
513400930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
513500930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
513600930914     C     WPODFC        IFGT      WPOFRC
513700930914     C                   Z-ADD     WPOFRC        WPODFC
513800930914     C                   ENDIF
513900930914     C*
514000930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
514100930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
514200930914     C     WPODIC        IFNE      *ZEROS
514300930914     C     WPODIC        ANDLT     WPOIRC
514400930914     C                   Z-ADD     WPOIRC        WPODIC
514500930914     C                   ENDIF
514600930914     C*
514700930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
514800930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
514900930914     C     WPODFC        IFLT      WPOIRC
515000930914     C                   Z-ADD     WPOIRC        WPODFC
515100930914     C                   ENDIF
515200930914     C*
515300930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
515400931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
515500930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
515600930916     C*
515700931210     C***********WPODIC    IFNE *ZEROS
515800931210     C***********WPODFC    ANDNE*ZEROS
515900931210     C*********************ADD  12        WPODFC
516000931210     C*********************ENDIF
516100930914     C     WPODIC        IFEQ      *ZEROS
516200930914     C     WPODFC        ANDNE     *ZEROS
516300931130     C                   Z-ADD     WPODFC        WPODIC
516400931210     C*********************ADD  12        WPODFC
516500930916     C                   ENDIF
516600930913     C*
516700930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
516800930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
516900930914     C* INTERNA
517000050908     c                   eval      Usacons='S'
517100930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
517200930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
517300931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
517400931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
517500940324     C                   EXSR      RO9COV
517600931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
517700930914     C* CLIENTE
517800930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
517900931210     C                   EXSR      RO9COV
518000931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
518100050908     c                   eval      Usacons=' '
518200930914     C*
518300930916     C                   ENDIF                                                  <-- 5 ---
518400930916     C                   ENDIF                                                  <-- 4 ---
518500930916     C                   ENDIF                                                  <-- 3 ---
518600930914     C                   ENDIF                                                  <-- 2 ---
518700930914     C                   ENDIF                                                  <-- 1 ---
518800930913     C*
518900930913     C                   ENDSR
519000930325     C********************************************************************
519100930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
519200930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
519300930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
519400930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
519500930325     C********************************************************************
519600930325     C     RO1CDP        BEGSR
519700930414     C*
519800931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
519900931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
520000931126     C*
520100931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
520200931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
520300150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
520400150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
520500931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
520600060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
520700930326     C*
520800930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
520900931130     C                   Z-ADD     WR1POI        WR1POO            9 0
521000930325     C*
521100930325     C* CICLO FINO A CHE:
521200930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
521300930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
521400930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
521500930406     C     WR1POO        ANDLT     WPOLIM
521600930414     C*
521700930407     C                   Z-ADD     WR1POO        WPOROU            9 0
521800930325     C                   MOVEL     'N'           WSLITT            1
521900930414     C*
522000930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
522100060420     C*
522200931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
522300150605     c     wdee          andeq     ' '
522400930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
522500930414     C                   END
522600930511     C*
522700931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
522800150608     c     wdee          andeq     ' '
522900930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
523000930511     C                   END
523100930414     C*
523200930325     C     WSLITT        IFEQ      'N'
523300930423     C                   SUB       12            WR1NOI            5 0
523400930325     C                   END
523500931130     C                   ADD       12            WR1POO            9 0
523600930414     C*
523700931223     C                   ENDDO                                                  <-- 1 ---
523800931126     C*
523900931126     C                   ELSE                                                   <-- 0 -->
524000931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
524100931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
524200931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
524300960129     C     WPOLAF        IFGT      WPODCG
524400960129     C                   Z-ADD     WPOLAF        WTM090            9 0
524500960129     C                   ELSE
524600960129     C                   Z-ADD     WPODCG        WTM090
524700960129     C                   ENDIF
524800960129     C     WPOGIF        IFGT      WTM090
524900960129     C                   Z-ADD     WPOGIF        WTM090
525000960129     C                   ENDIF
525100060420     C     WPOpaif       IFGT      WTM090
525200060420     C                   Z-ADD     WPOpaif       WTM090
525300060420     C                   ENDIF
525400960129     C*
525500960129     C     12            ADD       WTM090        WR1POO
525600960129     C                   ENDIF                                                  <-- 0 ---
525700930512     C*
525800931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
525900931202     C     WR1POO        SUB       12            WR7POI
526000931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
526100931202     C                   ADD       12            WR7POO
526200931202     C     WR7POO        IFGT      WR1POO
526300931202     C                   Z-ADD     WR7POO        WR1POO
526400931202     C                   END
526500930406     C*
526600930325     C                   ENDSR
526700930414     C********************************************************************
526800930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
526900930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
527000930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
527100930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
527200930414     C********************************************************************
527300930414     C     RO2COV        BEGSR
527400091014     c                   clear                   salvaEveL
527500091014     c                   clear                   salvaEveG
527600931130     C*
527700930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
527800930414     C                   Z-ADD     *ZEROS        WR2NOV
527900930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
528000931130     C                   Z-ADD     WR2POI        WR2POW
528100930414     C*
528200930414     C* CICLO FINO A CHE:
528300930414     C*   - DATA INIZIO SUPERATO DATA FINE
528400930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
528500931223     C     WR2POW        ANDLT     WPOLIM
528600930414     C*
528700930414     C                   Z-ADD     WR2POW        WPOROU
528800930414     C                   MOVEL     'N'           WSLITT            1
528900930414     C*
529000930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
529100930414     C*
529200930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
529300930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
529400930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
529500930414     C                   END
529600930414     C*
529700930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
529800930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
529900930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
530000930618     C                   END
530100060420     c
530200060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
530300060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
530400060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
530500060420     C                   END
530600060420     c
530700931201     C*
530800931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
530900931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
531000931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
531100931201     C                   END
531200060418     c
531300060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
531400060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
531500060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
531600060418     C                   END
531700930414     C*
531800930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
531900930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
532000930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
532100930414     C                   END
532200930511     C*
532300930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
532400930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
532500930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
532600930511     C                   END
532700930414     C*
532800930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
532900930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
533000930414     C     WSLITT        IFEQ      'N'
533100930423     C                   ADD       12            WR2NOV            5 0
533200930414     C                   END
533300931130     C                   ADD       12            WR2POW            9 0
533400930414     C*
533500931223     C                   ENDDO                                                  <-- 1 ---
533600930414     C*
533700930414     C                   ENDSR
533800941228     C********************************************************************
533900941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
534000941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
534100941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
534200941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
534300941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
534400941228     C********************************************************************
534500941228     C     RO2COP        BEGSR
534600941228     C*
534700941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
534800941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
534900941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
535000941228     C                   Z-ADD     *ZEROS        WR2NOV
535100941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
535200941228     C                   Z-ADD     WR2POI        WR2POW
535300941228     C*
535400941228     C* CICLO FINO A CHE:
535500941228     C*   - DATA INIZIO SUPERATO DATA FINE
535600941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
535700941228     C*
535800950215     C                   Z-ADD     WR2POW        WPOROU
535900941228     C                   MOVEL     'N'           WSLITT
536000941228     C* TRASFORMA IL PROGR. ORE IN DATA
536100950215     C*******              Z-ADDWR2POW    WORE
536200950215     C*******              EXSR OREDAT
536300950215     C*******              Z-ADDWDAT      W12DAT  80
536400950215     C*******              Z-ADDWRES      W12RES  30
536500941228     C*
536600941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
536700941228     C*
536800941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
536900941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
537000941228     C     WSLITT        IFEQ      'N'
537100941228     C                   ADD       12            WR2NOV            5 0
537200941228     C                   END
537300941228     C                   ADD       12            WR2POW            9 0
537400941228     C*
537500941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
537600941228     C* ENTRI IN LOOP
537700941228     C     WR2NOV        IFGT      999
537800941228     C                   LEAVE                                                  *VA A ENDDO
537900941228     C                   END
538000941228     C*
538100941228     C                   ENDDO                                                  <-- 1 ---
538200941228     C*
538300941228     C                   ENDSR
538400930510     C********************************************************************
538500930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
538600930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
538700930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
538800930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
538900050905     c*                     il num ore deve essere un multiplo di 12
539000050905     c*                     oppure 1 ora
539100930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
539200930510     C********************************************************************
539300930510     C     RO4CDP        BEGSR
539400930510     C*
539500930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
539600930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
539700050905     c                   movel     '1'           wgiro             1
539800930510     C*
539900930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
540000930510     C                   Z-ADD     WR4POI        WR4POO            9 0
540100930510     C*
540200930510     C* CICLO FINO A CHE:
540300930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
540400930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
540500930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
540600930510     C     WR4POO        ANDLT     WPOLIM
540700930510     C*
540800141106     c                   if        nosomma<>'N'
540900931130     C                   ADD       12            WR4POO            9 0
541000141106     c                   endif
541100930510     C                   Z-ADD     WR4POO        WPOROU            9 0
541200930510     C                   MOVEL     'N'           WSLITT            1
541300930510     C*
541400930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
541500050905     c
541600050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
541700930510     C     WSLITT        IFEQ      'N'
541800050905     c
541900050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
542000050905     c*  se cado in giorno non festivo
542100050905     c                   if        wr4noi=1 and wgiro='1'
542200050905     c                   sub       12            wr4poo
542300050905     c                   add       wr4noi        wr4poo
542400050905     c                   endif
542500050905     c*
542600050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
542700050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
542800050905     c*  calcolate dal pgm
542900050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
543000050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
543100050905     c*  del giorno sucessivo non festivo
543200050905     c                   if        wr4noi=1 and wgiro<>'1'
543300050905     c                   add       wr4noi        wr4poo
543400050905     c                   endif
543500050905     c
543600930510     C                   SUB       12            WR4NOI            5 0
543700930510     C                   END
543800141106     c
543900141106     c                   if        nosomma= 'N'
544000141106     C                   ADD       12            WR4POO            9 0
544100141106     c                   endif
544200930510     C*
544300050905     c                   eval      wgiro='2'
544400931223     C                   ENDDO                                                  <-- 1 ---
544500930510     C*
544600930510     C                   ENDSR
544700931130     C********************************************************************
544800931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
544900931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
545000931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
545100931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
545200931130     C********************************************************************
545300931130     C     RO5COV        BEGSR
545400931130     C*
545500931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
545600060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
545700060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
545800060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
545900931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
546000931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
546100931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
546200931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
546300931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
546400931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
546500931130     C                   Z-ADD     *ZEROS        WR5NOV
546600940714     C*
546700940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
546800940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
546900931207     C*
547000931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
547100940908     C     WIC           IFEQ      'I'
547200090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
547300090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
547400931207     C                   ELSE
547500090827     C*****              Z-ADD     D98TCC        WPOTCT
547600090827     C                   Z-ADD     w99TCC        WPOTCT
547700931207     C                   END
547800931130     C*
547900931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
548000060420     C                   clear                   COV
548100931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
548200931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
548300931207     C                   Z-ADD     2             CO                3 0
548400931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
548500931207     C                   EXSR      RO5SUB
548600981102     C                   DO        99            IC                3 0
548700931207     C     LAF(IC)       IFNE      *ZEROS
548800931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
548900931207     C                   EXSR      RO5SUB
549000931207     C                   ENDIF
549100931207     C                   ENDDO
549200060420     c
549300060420     C                   DO        99            IC                3 0
549400060420     C     paif(IC)      IFNE      *ZEROS
549500060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
549600060420     C                   EXSR      RO5SUB
549700060420     C                   ENDIF
549800060420     C                   ENDDO
549900931207     C* ORDINA LA SCHIERA COV
550000931207     C                   SORTA     COV
550100931130     C*
550200931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
550300931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
550400981102     C                   DO        98            IC                             --- 1 -->
550500931207     C     IC            ADD       1             I1C               3 0
550600931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
550700931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
550800931207     C     COV(I1C)      OREQ      *ZEROS
550900931207     C                   ELSE
551000931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
551100931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
551200931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
551300931130     C                   EXSR      RO2COV
551400931207     C                   ADD       WR2NOV        WR5NOV            9 0
551500931209     C* SE NON SONO IN DATA CONSEGNA
551600931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
551700931207     C     WR5NOV        IFLT      WPOTCT
551800931209     C     COV(I1C)      ANDNE     WPODCM
5519009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5520009709111009 C     WPOTCT        SUB       12            WR5NOV
552100931130     C                   END
552200931207     C                   ENDIF                                                  <-- 2 ---
552300931207     C                   ENDDO                                                  <-- 1 ---
552400940714     C                   ENDIF                                                  <-- 0 ---
552500931130     C*
552600931130     C                   ENDSR
552700931207     C********************************************************************
552800931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
552900931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
553000931207     C********************************************************************
553100931207     C     RO5SUB        BEGSR
553200931207     C*
553300931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
553400931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
553500931207     C     WR5COV        ANDLT     WR5POF
553600931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
553700931207     C                   MOVEL     'N'           WR5PRE            1
553800981102     C*---                 DO   30        I
553900981102     C                   DO        99            I
554000931207     C     WR5COV        IFEQ      COV(I)
554100931207     C                   MOVEL     'S'           WR5PRE
554200931207     C                   LEAVE
554300931207     C                   ENDIF
554400931207     C                   ENDDO
554500931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
554600931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
554700931207     C                   ADD       1             CO                3 0
554800931207     C                   Z-ADD     WR5COV        COV(CO)
554900931207     C                   ENDIF                                                  <-- 2 ---
555000931207     C                   ENDIF                                                  <-- 1 ---
555100931207     C*
555200931207     C                   ENDSR
555300931201     C********************************************************************
555400060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
555500931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
555600931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
555700931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
555800931201     C********************************************************************
555900931201     C     RO6CDT        BEGSR
556000931201     C*
556100931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
556200931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
556300931201     C*
556400931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
556500931201     C                   Z-ADD     WR6POI        WR6POO            9 0
556600931201     C*
556700931201     C* CICLO FINO A CHE:
556800931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
556900931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
557000931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
557100931201     C     WR6POO        ANDLT     WPOLIM
557200931201     C*
557300931201     C                   Z-ADD     WR6POO        WPOROU            9 0
557400931201     C                   MOVEL     'N'           WSLITT            1
557500931201     C*
557600931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
557700931201     C*
557800931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
557900150605     c     wdee          andeq     ' '
558000931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
558100931201     C                   END
558200931201     C*
558300931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
558400150608     c     wdee          andeq     ' '
558500931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
558600931201     C                   END
558700931201     C*
558800931201     C     WSLITT        IFEQ      'N'
558900931201     C                   SUB       12            WR6NOI            5 0
559000931201     C                   END
559100931201     C                   ADD       12            WR6POO            9 0
559200931201     C*
559300931223     C                   ENDDO                                                  <-- 1 ---
559400931201     C*
559500931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
559600931202     C     WR6POO        SUB       12            WR7POI
559700931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
559800931207     C                   ADD       12            WR7POO
559900931202     C     WR7POO        IFGT      WR6POO
560000931201     C                   Z-ADD     WR7POO        WR6POO
560100931202     C                   END
560200931201     C*
560300931201     C                   ENDSR
560400931201     C********************************************************************
560500060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
560600060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
560700060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
560800060407     C*            INPUT:  -WR7POI (PROGR.ORE)
560900931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
561000931201     C********************************************************************
561100931201     C     RO7TCF        BEGSR
561200060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
561300060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
561400060407     C                   MOVEL     'N'           WSLITT
561500931201     C*
561600931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
561700931201     C                   Z-ADD     WR7POI        WR7POO            9 0
561800931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
561900931201     C* COINCIDA CON UN TURNO DI CHIUSURA
562000060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
562100931201     C     D98GC2        ANDEQ     *BLANKS
562200060407   x1C                   ELSE                                                   <-- 1 -->
562300931214     C*
562400931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
562500931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
562600931214     C*
562700931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
562800931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
562900931223     C     WPOROU        ANDLT     WPOLIM
563000931214     C* MEMORIZZO INIZIO E FINE RANGE
563100931202     C     WPOTI         IFEQ      *ZEROS
563200931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
563300931202     C                   END
563400931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
563500931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
563600931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
563700931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
563800931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
563900931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
564000931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
564100931201     C                   MOVEL     'N'           WSLITT
564200931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
564300060407     c* Se non trovat, controllo se c'e' evento part che giustifica
564400060407     c                   if        wslitt='N'
564500060407     c                   exsr      datcepg
564600060407     c                   endif
564700060407     c
564800931201     C                   ENDDO                                                  <-- 2 ---
564900931214     C*
565000931201     C                   Z-ADD     WPOROU        WR7POO
565100931214     C*
565200931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
565300931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
565400931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
565500931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
565600931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
565700931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
565800931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
565900931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
566000931209     C                   ELSE                                                   <-- 3 -->
566100931209     C     WIC           IFEQ      'I'
566200931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
566300931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
566400931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
566500931201     C                   ELSE
566600931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
566700931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
566800931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
566900931201     C                   ENDIF
567000931209     C                   ENDIF                                                  <-- 3 ---
567100931209     C                   ENDIF                                                  <-- 2 ---
567200931214     C*
567300060407    1C                   ENDIF                                                  <-- 1 ---
567400060407     c
567500060407     c* Se non trovato turno di chiusura, verifico se c'e'
567600060407     c*  evento particolare che GIUSTIFICA
567700060407     c                   if        wpoti=0
567800060418     C                   EXSR      RO7CEPG
567900060407     c                   endif
568000931201     C*
568100931201     C                   ENDSR
568200060406     C********************************************************************
568300060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
568400060406     c*                        che giustifica
568500060407     C*            INPUT:-WR7POI (PROGR.ORE)
568600060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
568700060406     C********************************************************************
568800060407     C     RO7CEPG       BEGSR
568900060406     C*
569000060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
569100060407     C                   Z-ADD     WR7POI        WR7POO            9 0
569200060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
569300060406     C* coincide
569400060406    1C                   if        wevpag='S'                                   <-- 1 -->
569500060406     C*
569600060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
569700060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
569800060406     C                   MOVEL     'N'           WSLITT
569900060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
570000060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
570100060406     C*
570200060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
570300060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
570400060406     C     WPOROU        ANDLT     WPOLIM
570500060406     C* MEMORIZZO INIZIO E FINE RANGE
570600060406    3C     WPOTI         IFEQ      *ZEROS
570700060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
570800060406    3C                   END
570900060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
571000060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
571100060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
571200060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
571300060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
571400060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
571500060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
571600060406     C                   MOVEL     'N'           WSLITT
571700060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
571800060406     C* Se non trovato,
571900060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
572000060406    3c                   if        wslitt='N'
572100060406     c                   exsr      DATTUC
572200060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
572300060406    2C                   ENDDO                                                  <-- 2 ---
572400060406     C*
572500060407     C                   Z-ADD     WPOROU        WR7POO
572600060406     C*
572700060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
572800060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
572900060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
573000060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
573100060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
573200060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
573300060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
573400060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
573500060406   x3C                   ELSE                                                   <-- 3 -->
573600060406    4C     WIC           IFEQ      'I'
573700060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
573800060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
573900060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
574000060406   x4C                   ELSE
574100060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
574200060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
574300060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
574400060406    4C                   ENDIF
574500060406    3C                   ENDIF                                                  <-- 3 ---
574600060406    2C                   ENDIF                                                  <-- 2 ---
574700060406     C*
574800060406    1C                   ENDIF                                                  <-- 1 ---
574900060406     C*
575000060406     C                   ENDSR
575100931207     C********************************************************************
575200931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
575300931207     C*            INPUT:  -WR8POI (PROGR.ORE)
575400931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
575500931207     C********************************************************************
575600931207     C     RO8FES        BEGSR
575700130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
575800130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
575900931207     C*
576000931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
576100130411     C     WR8POI        SUB       12            WR8POO            9 0
576200931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
576300931207     C                   MOVEL     'N'           WSLITT
576400130411     c
576500130411     c* Non salto i festivi ???
576600130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
576700130411     c
576800931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
576900130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
577000130411     C     WPOROU        ANDGT     WPODSP
577100130411     C                   SUB       12            WPOROU
577200130411     C                   MOVEL     'N'           WSLITT
577300130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
577400130411     C                   ENDDO                                                  <-- 2 ---
577500130411     c
577600130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
577700130411     C     D98GC2        ANDEQ     *BLANKS
577800130411   x1C                   ELSE                                                   <-- 1 -->
577900130411     c* eseguo controllo per i turni di chiusura
578000130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
578100130411     C*
578200130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
578300130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
578400130411     C     WPOROU        ANDgT     WPOdsp
578500130411     C* MEMORIZZO INIZIO E FINE RANGE
578600130411     C     WPOTf         IFEQ      *ZEROS
578700130411     C                   z-add     wporou        WPOTf
578800130411     C                   END
578900140826     C     WPOROU        SUB       wnoadd        WPOROU
579000130411     c
579100130411     C                   MOVEL     'N'           WSLITT
579200130411     c                   exsr      dattuc
579300130411     c                   if        wslitt='N'
579400130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
579500140826     c* Se slitta per festivo --> tolgo sempre 12 ore
579600140826     c                   z-add     12            WNOADD
579700130411     c                   endif
579800130411     C* SLITTA
579900140826     C     wporou        add       12            WPOTi
580000130411    2C                   ENDDO                                                  <-- 2 ---
580100130411     c
580200130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
580300130411     c
580400130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
580500130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
580600130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
580700130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
580800130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
580900130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
581000130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
581100130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
581200130411   x3C                   ELSE                                                   <-- 3 -->
581300130411    4C     WIC           IFEQ      'I'
581400130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
581500130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
581600130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
581700130411   x4C                   ELSE
581800130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
581900130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
582000130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
582100130411    4C                   ENDIF
582200130411    3C                   ENDIF                                                  <-- 3 ---
582300130411    2C                   ENDIF                                                  <-- 3 ---
582400130411     C*
582500130411    1C                   ENDIF                                                  <-- 1 ---
582600130411
582700130411     C**   WPOROU        ADD       12            WR8POO
582800130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
582900130411     c                   z-add     wr8poi        wr8poo
583000931207     C*
583100931207     C                   ENDSR
583200931210     C********************************************************************
583300931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
583400940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
583500931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
583600931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
583700931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
583800931210     C********************************************************************
583900931210     C     RO9COV        BEGSR
584000931210     C*
584100931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
584200931210     C                   Z-ADD     *ZEROS        WR9NOV
584300931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
584400931210     C                   Z-ADD     WR9POI        WR9POW
584500931210     C* CICLO FINO A CHE:
584600931210     C*   - DATA INIZIO SUPERATO DATA FINE
584700931223     C*   - RAGGIUNTO LIMITE MASSIMO
584800931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
584900931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
585000931223     C     WR9POW        ANDLT     WPOLIM
585100931210     C*
585200931210     C                   Z-ADD     WR9POW        WPOROU
585300931210     C                   MOVEL     'N'           WSLITT            1
585400940324     C* FESTIVI
585500931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
585600940324     C* GIACENZA
585700931214     C     WSLITT        IFEQ      'N'
585800941229£$   C     WPOAGC        IFNE      *ZEROS
585900940324£$   C     WPOCGC        ORNE      *ZEROS
586000941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
586100940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
586200940324£$   C                   MOVEL     'S'           WSLITT
586300940324£$   C                   END
586400940324£$   C                   END
586500940324£$   C                   END
586600960129     C* EVENTI GIACENZA
586700960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
586800960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
586900960129     C                   Z-ADD     *ZEROS        G                 3 0
587000960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
587100960129     C     G             ANDLT     IG
587200960129     C                   ADD       1             G
587300960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
587400960209     C     WPOROU        ANDLE     GCF(G)
587500960129     C                   MOVEL     'S'           WSLITT
587600960129     C                   ENDIF                                                  <-- 3 ---
587700960129     C                   ENDDO                                                  <-- 2 ---
587800960129     C                   ENDIF                                                  <-- 1 ---
587900960129     C                   ENDIF                                                  <-- 0 ---
588000940324     C* LASCIATO AVVISO
588100931214     C     WSLITT        IFEQ      'N'
588200941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
588300940324£$   C                   Z-ADD     *ZEROS        L                 3 0
588400940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
588500940324£$   C     L             ANDLT     IL
588600940324£$   C                   ADD       1             L
588700941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
588800941229£$   C     WPOROU        ANDLE     LCF(L)
588900050520     c     LCon(L)       andeq     'S'
589000940324£$   C                   MOVEL     'S'           WSLITT
589100940324£$   C                   END                                                    <-- 3 ---
589200940324£$   C                   ENDDO                                                  <-- 2 ---
589300940324£$   C                   END                                                    <-- 1 ---
589400931210     C                   END
589500940324     C* TURNO CHIUSURA
589600140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
589700140826     c*  di sistemare le ore da sommare 12 o 24...
589800931214     C     WSLITT        IFEQ      'N'
589900931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
590000931210     C                   END
590100940324     C* DATA CONSEGNA RICHIESTA
590200050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
590300050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
590400050907     c
590500931214     C     WSLITT        IFEQ      'N'
590600050907     c                   add       1             wporou
590700940324£$   C     WPOCRI        IFNE      *ZEROS
590800940324£$   C     WPOROU        IFLT      WPOCRC
590900940324£$   C                   MOVEL     'S'           WSLITT
591000940324£$   C                   END
591100940324£4   C                   END
591200050907     c                   sub       1             wporou
591300931210     C                   END
591400931210     C*
591500931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
591600931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
591700931210     C     WSLITT        IFEQ      'N'
591800931210     C                   ADD       12            WR9NOV            5 0
591900931210     C                   END
592000931210     C                   ADD       12            WR9POW            9 0
592100931210     C*
592200931223     C                   ENDDO                                                  <-- 1 ---
592300931210     C*
592400931210     C                   ENDSR
592500940628%%%  C********************************************************************
592600940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
592700940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
592800940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
592900940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
593000940628%%%  C********************************************************************
593100940628%%%  C     RO0GES        BEGSR
593200940628%%%  C*
593300940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
593400940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
593500940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
593600940628%%%  C*
593700940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
593800940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
593900940628%%%  C*
594000940628%%%  C* CICLO FINO A CHE:
594100940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
594200940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
594300940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
594400050922%%%  C     WR0POO        ANDGe     WPODSP
594500940628%%%  C*
594600940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
594700940628%%%  C                   MOVEL     'N'           WSLITT            1
594800940628%%%  C*
594900940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
595000940628%%%  C*
595100940628%%%  C     WSLITT        IFEQ      'N'
595200940628%%%  C                   SUB       12            WR0NOI            5 0
595300940628%%%  C                   END
595400940628%%%  C                   SUB       12            WR0POO            9 0
595500940628%%%  C*
595600940628%%%  C                   ENDDO                                                  <-- 1 ---
595700940628%%%  C*
595800940628%%%  C                   ADD       12            WR0POO            9 0
595900940628%%%  C*
596000940628%%%  C                   ENDSR
596100960911     C********************************************************************
596200960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
596300960911     C*            INPUT:  -W10POI (PROGR.ORE)
596400960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
596500960911     C********************************************************************
596600960911     C     R10FES        BEGSR
596700960911     C*
596800960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
596900960911     C                   Z-ADD     W10POI        W10POO            9 0
597000960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
597100960911     C                   MOVEL     'N'           WSLITT
597200960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
597300960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
597400960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
597500960911     C                   ADD       12            WPOROU
597600960911     C                   MOVEL     'N'           WSLITT
597700960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
597800960911     C                   ENDDO                                                  <-- 2 ---
597900960911     C                   Z-ADD     WPOROU        W10POO
598000960911     C*
598100960911     C                   ENDSR
598200930325     C********************************************************************
598300930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
598400930325     C********************************************************************
598500930325     C     CHKGIA        BEGSR
598600930406     C*
598700960129     C* APERTURA/CHIUSURA GIACENZA
598800930326     C     WPODAG        IFNE      *ZEROS
598900930406     C     WPODCG        ORNE      *ZEROS
599000930407     C     WPOROU        IFGE      WPODAG
599100930407     C     WPOROU        ANDLE     WPODCG
599200930325     C                   MOVEL     'S'           WSLITT
599300930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
599400930325     C                   END
599500930326     C                   END
599600960129     C*
599700960129     C* EVENTI GIACENZA
599800960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
599900960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
600000960129     C                   Z-ADD     *ZEROS        G                 3 0
600100960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
600200960129     C     G             ANDLT     IG
600300960129     C                   ADD       1             G
600400960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
600500960129     C     WPOROU        ANDLE     GIF(G)
600600091009     c
600700091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
600800091009     c*  l'orario dell'evento
600900091009     c                   select
601000091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
601100091009     c* Se orario entro le 10.30  va bene
601200091014     c*  solo la prima volta per evento cioè quando controllo
601300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
601400091014     c*  e non devo dare errore
601500091014     C                   MOVEL     'OSC  '       WR3TAB
601600091014     C                   eval      WR3HHI=evo(gip(G))
601700091014     C                   EXSR      RO3ORE
601800091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
601900091014     c*  in ritardo come per "E")
602000091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
602100091014     c                              wr3matpom='P'
602200091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
602300091009     C                   MOVEL     'S'           WSLITT
602400091009     c                   endif
602500091014     c                   eval      SalvaEveG=gip(G)
602600091009     c
602700091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
602800091009     c* Se orario entro le 10.30  va bene
602900091014     c*  solo la prima volta per evento cioè quando controllo
603000091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
603100091014     c*  e non devo dare errore
603200091014     C                   MOVEL     'OSC  '       WR3TAB
603300091014     C                   eval      WR3HHI=evo(gip(G))
603400091014     C                   EXSR      RO3ORE
603500091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
603600091014     c*  in ritardo come per "E")
603700091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
603800091014     c                              wr3matpom='P'
603900091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
604000091009     C                   MOVEL     'S'           WSLITT
604100091009     c                   endif
604200091014     c                   eval      SalvaEveG=gip(G)
604300091009     c
604400091009     c                   other
604500960129     C                   MOVEL     'S'           WSLITT
604600960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
604700091009     c                   endsl
604800091009
604900960129     C                   ENDIF                                                  <-- 3 ---
605000960129     C                   ENDDO                                                  <-- 2 ---
605100960129     C                   ENDIF                                                  <-- 1 ---
605200960129     C                   ENDIF                                                  <-- 0 ---
605300930406     C*
605400930325     C                   ENDSR
605500060420     C********************************************************************
605600930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
605700930325     C********************************************************************
605800930325     C     CHKLAV        BEGSR
605900930325     C*
606000941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
606100930421     C                   Z-ADD     *ZEROS        L                 3 0
606200930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
606300930421     C     L             ANDLT     IL
606400930421     C                   ADD       1             L
606500930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
606600930421     C     WPOROU        ANDLE     LAF(L)
606700090928     c
606800090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
606900091009     c*  l'orario dell'evento
607000091009     c                   select
607100091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
607200091009     c* Se orario entro le 10.30  va bene
607300091014     c*  solo la prima volta per evento cioè quando controllo
607400091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
607500091014     c*  e non devo dare errore
607600091014     C                   MOVEL     'OSC  '       WR3TAB
607700091014     C                   eval      WR3HHI=evo(lap(L))
607800091014     C                   EXSR      RO3ORE
607900091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
608000091014     c*  in ritardo come per "E")
608100091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
608200091014     c                              wr3matpom='P'
608300091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
608400091009     C                   MOVEL     'S'           WSLITT
608500091009     c                   endif
608600091014     c                   eval      SalvaEveL=lap(L)
608700091009     c
608800091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
608900091009     c* Se orario entro le 10.30  va bene
609000091014     c*  solo la prima volta per evento cioè quando controllo
609100091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
609200091014     c*  e non devo dare errore
609300091014     C                   MOVEL     'OSC  '       WR3TAB
609400091014     C                   eval      WR3HHI=evo(lap(L))
609500091014     C                   EXSR      RO3ORE
609600091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
609700091014     c*  in ritardo come per "E")
609800091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
609900091014     c                              wr3matpom='P'
610000091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
610100091009     C                   MOVEL     'S'           WSLITT
610200091009     c                   endif
610300091014     c                   eval      SalvaEveL=lap(L)
610400090928     c
610500091009     c                   other
610600930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
610700091009     C                   MOVEL     'S'           WSLITT
610800091009     c                   endsl
610900090928     c
611000930421     C                   END                                                    <-- 3 ---
611100931223     C                   ENDDO                                                  <-- 2 ---
611200930421     C                   END                                                    <-- 1 ---
611300930325     C*
611400930325     C                   ENDSR
611500060420     C********************************************************************
611600060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
611700060420     c*                   che Allungano
611800060420     C********************************************************************
611900060420     C     CHKCEPA       BEGSR
612000060420     C*
612100060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
612200060420     C                   Z-ADD     *ZEROS        p                 3 0
612300060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
612400060420     C     p             ANDLT     IpA
612500060420     C                   ADD       1             P
612600060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
612700060420     C     WPOROU        ANDLE     paif(P)
612800060420     C                   MOVEL     'S'           WSLITT
612900060420     C                   ADD       12            W98EA
613000060420     C                   END                                                    <-- 3 ---
613100060420     C                   ENDDO                                                  <-- 2 ---
613200060420     C                   END                                                    <-- 1 ---
613300060420     C*
613400060420     C                   ENDSR
613500930414     C********************************************************************
613600930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
613700930414     C********************************************************************
613800930414     C     CHKCNT        BEGSR
613900930511     C*
614000930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
614100930611     C     WIC           IFEQ      'I'                                          *INTERNO
614200930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
614300930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
614400930611     C                   ELSE
614500930611     C     WPOROU        IFGE      WPOAII
614600930611     C     WPOROU        ANDLE     WPOAFI
614700930611     C                   MOVEL     'S'           WSLITT
614800930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
614900930611     C                   END
615000930611     C                   END
615100930611     C                   ELSE                                                   *CLIENTE
615200930611     C     WPOAIC        IFEQ      *ZEROS
615300930611     C     WPOAFC        ANDEQ     *ZEROS
615400930611     C                   ELSE
615500930611     C     WPOROU        IFGE      WPOAIC
615600930611     C     WPOROU        ANDLE     WPOAFC
615700930611     C                   MOVEL     'S'           WSLITT
615800930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
615900930611     C                   END
616000930611     C                   END
616100930611     C                   END
616200930414     C*
616300930414     C                   ENDSR
616400930511     C********************************************************************
616500930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
616600930511     C********************************************************************
616700930511     C     CHKFED        BEGSR
616800930511     C*
616900930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
617000930611     C     WIC           IFEQ      'I'                                          *INTERNO
617100930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
617200930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
617300930611     C                   ELSE
617400930611     C     WPOROU        IFGE      WPOFII
617500930611     C     WPOROU        ANDLE     WPOFFI
617600930611     C                   MOVEL     'S'           WSLITT
617700930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
617800930611     C                   END
617900930611     C                   END
618000930611     C                   ELSE                                                   *CLIENTE
618100930611     C     WPOFIC        IFEQ      *ZEROS
618200930611     C     WPOFFC        ANDEQ     *ZEROS
618300930611     C                   ELSE
618400930611     C     WPOROU        IFGE      WPOFIC
618500930611     C     WPOROU        ANDLE     WPOFFC
618600930611     C                   MOVEL     'S'           WSLITT
618700930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
618800930611     C                   END
618900930611     C                   END
619000930611     C                   END
619100930511     C*
619200930511     C                   ENDSR
619300930325     C********************************************************************
619400930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
619500930325     C********************************************************************
619600930325     C     CHKFES        BEGSR
619700060526     c* se già passata la data, non controllo wporou
619800060526     c                   if        wpodata>0
619900060526     c                   z-add     wpodata       w12dat
620000060526     c                   clear                   w12res
620100060526     c                   else
620200931130     C*
620300931130     C* TRASFORMA IL  PROGR.ORE IN DATA
620400931130     C                   Z-ADD     WPOROU        WORE
620500931130     C                   EXSR      OREDAT
620600941227     C                   Z-ADD     WDAT          W12DAT            8 0
620700931130     C                   Z-ADD     WRES          W12RES            3 0
620800060526     c                   endif
620900930325     C*
621000941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
621100930325     C* E' FESTIVO
621200090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
621300090108     C**                 Z-ADD     D98LNP        CLNTFP
621400090108     C**                 Z-ADD     *ZEROS        CLNTFA
621500090108     C**                 ENDIF
621600090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
621700090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
621800090108     c**   è possibile rendere festivo in terminal per un altro terminal
621900090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
622000930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
622100090108     C     WTC           oreq      'P'                                          *TRAZIONE
622200941227     C                   Z-ADD     D98TFP        CLNTFP
622300941227     C                   Z-ADD     D98TFA        CLNTFA
622400941228     C                   ENDIF
622500941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
622600941227     C                   Z-ADD     *ZEROS        CLNTFP
622700941227     C                   Z-ADD     D98LNA        CLNTFA
622800941228     C                   ENDIF
622900941227     C                   Z-ADD     W12DAT        DSDAT8
623000941227     C                   Z-ADD     DSAAS         CLNANN
623100941227     C                   Z-ADD     DSMES         CLNMES
623200930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
623300930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
623400930421     C                   Z-ADD     DSGIO         G
623500930415     C     W12RES        IFEQ      *ZEROS
623600941227     C                   MOVEL     CLNMAT        GFEDS
623700930415     C                   ELSE
623800941227     C                   MOVEL     CLNPOM        GFEDS
623900930415     C                   END
624000930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
624100930405     C                   MOVEL     *BLANKS       WC1Y
624200930421     C                   MOVEL     GFE(G)        WC1Y              8
624300930421     C                   Z-ADD     1             I
624400930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
624500930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
624600930405     C     WIC           IFEQ      'I'
624700930421     C     I1Y(I)        ANDEQ     'S'
624800930405     C                   MOVEL     'S'           WSLITT
624900930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
625000930405     C                   END
625100930405     C     WIC           IFEQ      'C'
625200930421     C     E1Y(I)        ANDEQ     'S'
625300930405     C                   MOVEL     'S'           WSLITT
625400930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
625500930405     C                   END
625600970204     C     WIC           IFEQ      'X'
625700970204     C     E1Y(I)        ANDEQ     'S'
625800970204     C                   MOVEL     'S'           WSLITT
625900970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
626000970204     C                   END
626100930405     C                   ELSE                                                   <-- 3 -->
626200930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
626300930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
626400930405     C                   END                                                    <-- 3 ---
626500930405     C                   END                                                    <-- 2 ---
626600930405     C                   END                                                    <-- 1 ---
626700930325     C*
626800060526     c                   clear                   wpodata
626900930325     C                   ENDSR
627000931201     C********************************************************************
627100931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
627200931201     C********************************************************************
627300931201     C     CHKTUC        BEGSR
627400931201     C*
627500931201     C     WIC           IFEQ      'I'                                          --- 1 -->
627600931201     C                   DO        TI            T                 3 0          --- 2 -->
627700931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
627800931201     C     WPOROU        ANDLE     TIF(T)
627900931201     C                   MOVEL     'S'           WSLITT
628000931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
628100931201     C                   LEAVE                                                  *VA A FINE DO
628200931201     C                   ENDIF                                                  <-- 3 ---
628300931201     C                   ENDDO                                                  <-- 2 ---
628400931201     C                   ELSE                                                   <-- 1 -->
628500931201     C                   DO        TC            T                 3 0          --- 2 -->
628600931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
628700931201     C     WPOROU        ANDLE     TCF(T)
628800931201     C                   MOVEL     'S'           WSLITT
628900931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
629000931201     C                   LEAVE                                                  *VA A FINE DO
629100931201     C                   ENDIF                                                  <-- 3 ---
629200931201     C                   ENDDO                                                  <-- 2 ---
629300931201     C                   ENDIF                                                  <-- 1 ---
629400931201     C*
629500931201     C                   ENDSR
629600060418     C********************************************************************
629700060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
629800060418     c*          - eventi particolari che giustificano
629900060418     C********************************************************************
630000060418     C     CHKCEPG       BEGSR
630100060418     C*
630200060418     C     WIC           IFEQ      'I'                                          --- 1 -->
630300060420     C                   DO        PGI           p                 3 0          --- 2 -->
630400060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
630500060420     C     WPOROU        ANDLE     PGIF(p)
630600060418     C                   MOVEL     'S'           WSLITT
630700060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
630800060418     C                   LEAVE                                                  *VA A FINE DO
630900060418     C                   ENDIF                                                  <-- 3 ---
631000060418     C                   ENDDO                                                  <-- 2 ---
631100060418     C                   ELSE                                                   <-- 1 -->
631200060420     C                   DO        PGC           p                 3 0          --- 2 -->
631300060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
631400060420     C     WPOROU        ANDLE     PGCF(p)
631500060418     C                   MOVEL     'S'           WSLITT
631600060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
631700060418     C                   LEAVE                                                  *VA A FINE DO
631800060418     C                   ENDIF                                                  <-- 3 ---
631900060418     C                   ENDDO                                                  <-- 2 ---
632000060418     C                   ENDIF                                                  <-- 1 ---
632100060418     C*
632200060418     C                   ENDSR
632300930512     C********************************************************************
632400931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
632500930512     C********************************************************************
632600931130     C     DATTUC        BEGSR
632700931214     C*
632800931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
632900931214     C                   Z-ADD     12            WNOADD            9 0
633000930325     C*
633100931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
633200931125     C     D98GC2        ORNE      *BLANKS
633300931130     C*
633400931213     C                   Z-ADD     WPOROU        WORE
633500931130     C                   EXSR      OREDAT                                       *ORE->DATA
633600931130     C                   Z-ADD     WDAT          W12DAT
633700931130     C                   Z-ADD     WRES          W12RES
633800930405     C*
633900941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
634000930405     C* DELLA SETTIMANA
634100941227     C                   Z-ADD     W12DAT        DSDAT8
634200941227     C                   Z-ADD     DSAAS         CLNANN
634300941227     C                   Z-ADD     DSMES         CLNMES
634400941227     C                   Z-ADD     *ZEROS        CLNTFP
634500941227     C                   Z-ADD     *ZEROS        CLNTFA
634600930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
634700930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
634800941227     C                   MOVEL     CLNMAT        GSEDS
634900960129     C                   Z-ADD     DSGIO         GI                6 0
635000960129     C                   MOVEL     GSE(GI)       WGSE              1
635100140911     c
635200140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
635300140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
635400140911     c                   if        wtuctut='S'
635500140911     c                   eval      wtuctut='P'
635600140911     c                   endif
635700930326     C*
635800931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
635900931213     C                   DO        4             IT                3 0          --- 3 -->
636000931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
636100931213     C                   MOVEL     TUC(IT)       W93GSE            1
636200931213     C                   MOVE      TUC(IT)       W93GMP            1
636300930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
636400930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
636500931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
636600930512     C     W93GSE        OREQ      ' '
636700140911     c     WTUCTUT       andeq     ' '
636800140911     c
636900930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
637000931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
637100930401     C     W93GMP        OREQ      ' '
637200930325     C                   MOVEL     'S'           WSLITT
637300931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
637400931213     C                   ELSE                                                   <-- 6 -->
637500001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
637600001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
637700001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
637800050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
637900050908     C     Usacons       IFNE      'S'                                          --- 7 -->
638000141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
638100141107     c*    orari giorno per giorno
638200141107     c                   if        OOR2FCSGL<>' '
638300141108     c                             and w12dat<>sav_12dat
638400141107     c                   eval      or_datarif=w12dat
638500141107     c                   eval      or_DDC    =w12dat
638600141107     c                   exsr      Reper_tempi
638700141108     c                   eval      sav_12dat=w12dat
638800141107     c                   endif
638900141107     c
639000930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
639100930325     C     W93GMP        IFEQ      'M'
639200930326     C     W12RES        ANDEQ     *ZEROS
639300141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
639400141111     c                             or OOR2STfS =0
639500141105     c                             or (tuc(IT)<>gc1part and
639600141105     c                              tuc(IT)<>gc2part)
639700140825     C                   MOVEL     'S'           WSLITT
639800140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
639900140911     c* slitto solo una volta  se chiuso tutti " M"
640000140911     c                   if        W93GSE=' '
640100140911     c                   eval      wTUCTUT='S'
640200140911     c                   endif
640300140911     c
640400140825     c                   endif
640500930325     C                   END
640600930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
640700930325     C     W93GMP        IFEQ      'P'
640800930326     C     W12RES        ANDNE     *ZEROS
640900140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
641000140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
641100141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
641200141111     c                             or OOR2STiS =0
641300141105     c                             or (tuc(IT)<>gc1part and
641400141105     c                              tuc(IT)<>gc2part)
641500930325     C                   MOVEL     'S'           WSLITT
641600140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
641700140911     c* slitto solo una volta  se chiuso tutti " P"
641800140911     c                   if        W93GSE=' '
641900140911     c                   eval      wTUCTUT='S'
642000140911     c                   endif
642100140825     c                   endif
642200140825     c
642300931213     C                   ENDIF
642400001128     C                   ENDIF                                                  <-- 7 ---
642500931213     C                   ENDIF                                                  <-- 6 ---
642600931213     C                   ENDIF                                                  <-- 5 ---
642700931213     C                   ENDIF                                                  <-- 4 ---
642800931213     C                   ENDDO                                                  <-- 3 ---
642900930405     C*
643000931213     C                   ENDIF                                                  <-- 2 ---
643100931213     C                   ENDIF                                                  <-- 1 ---
643200140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
643300140911     c                   if        wtuctut='P'  and wslitt='N'
643400140911     c                   clear                   wtuctut
643500140911     c                   endif
643600140911     c
643700930325     C                   ENDSR
643800060406     C********************************************************************
643900060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
644000060406     C********************************************************************
644100060406     C     DATCEPG       BEGSR
644200060406     C*
644300060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
644400060406     C                   Z-ADD     12            WNOADD            9 0
644500060406     C*
644600060406    1C                   if        wevpag='S'
644700060406     C*
644800060406     C                   Z-ADD     WPOROU        WORE
644900060406     C                   EXSR      OREDAT                                       *ORE->DATA
645000060406     C                   Z-ADD     WDAT          W12DAT
645100060406     C                   Z-ADD     WRES          W12RES
645200060418     c                   z-add     1             P
645300060406     c* Verifico se nella data esiste un evento Part che giustifica
645400060418     c     w12dat        lookup    pvd(P)                                 30
645500060418    2c                   dow       *in30 and pvf(P)<>'G'
645600060418     c                   add       1             P
645700060420     c     w12dat        lookup    pvd(P)                                 30
645800060406    2c                   enddo
645900060406     c
646000060406     c* 30 on --> TROVATO !!
646100060406    2c                   if        *in30
646200060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
646300060406     C                   MOVEL     'S'           WSLITT
646400060406     c                   z-add     24            wnoadd
646500060406    3C                   ENDIF
646600060406     C*
646700060406    2C                   ENDIF                                                  <-- 2 ---
646800060406    1C                   ENDIF                                                  <-- 1 ---
646900060406     C*
647000060406     C                   ENDSR
647100931210     C********************************************************************
647200931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
647300931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
647400931210     C********************************************************************
647500050805     C***  CHKDCR        BEGSR
647600931210     C*
647700050805     C***  WPOCRI        IFNE      *ZEROS
647800050805     C***  WPOROU        IFGE      WPOCRI
647900050805     C***  WPOROU        ANDLT     WPOCRF
648000050805     C***                ELSE
648100050805     C***                MOVEL     'S'           WSLITT
648200050805     C***                END
648300050805     C***                END
648400931210     C*
648500050805     C***                ENDSR
648600140822     C********************************************************************
648700140822     C*  Reperimento tempi di consegna per decidere se slittare
648800140822     C********************************************************************
648900140822     C     Reper_tempi   BEGSR
649000140822
649100140822     c                   clear                   trulorSds
649200140825     c                   clear                   trulor2ds
649300140822     c                   eval       IOREtfp = d98tfp
649400140822     c                   eval       IOREtfa = d98tfa
649500140825     c                   eval       IOREdti = d98dti
649600140825     c                   eval       IOREhti = d98hti
649700140825     c                   eval       IOREdcr = s98dcr
649800140825     c                   eval       IOREhcr = s98hcr
649900140825     c                   eval       IOREtcr = s98tcr
650000140825     c                   eval       IOREDEI = d98dei
650100140825     c                   eval       IOREOEI = d98oei
650200140825     c                   eval       IOREfil = d98lna
650300140825     c                   eval       IOREcap = d98cad
650400140825     c                   eval       IOREloc = d98lod
650500140825     c                   eval       IOREnar = d98nzd
650600140825     c
650700140825     c* Data di Riferimento:
650800140825     c* Se impostata la consegnabilità --> prendo questa
650900140822     c                   select
651000140825     c                   when      d98dfc>0
651100141107     c                   if        or_datarif<d98dfc and or_datarif>0
651200141107     c                   eval      ioredta=or_datarif
651300141107     c                   else
651400141107     c                   eval      ioredta=d98dfc
651500141107     c                   endif
651600140825     c* altrimenti data di consegna
651700140825     c                   when      d98dcm>0
651800141107     c                   if        or_datarif<d98dcm and or_datarif>0
651900141107     c                   eval      ioredta=or_datarif
652000141107     c                   else
652100140825     c                   eval      ioredta=d98dcm
652200141107     c                   endif
652300140825     c
652400140822     c                   other
652500140825     c* altrimenti data teorica consegna se > = oggi
652600140825     C                   time                    wrkoggiiso
652700140825     C     *iso          move      wrkoggiiso    ioredta
652800140822     c* se < di oggi, imposto oggi
652900140825     c                   if        d98dei>ioredta
653000140825     c                   eval      ioredta=d98dei
653100140822     c                   endif
653200141107     c
653300141107     c                   if        or_datarif<ioredta and or_datarif>0
653400141107     c                   eval      ioredta=or_datarif
653500141107     c                   endif
653600141107     c
653700140822     c                   endsl
653800140822     c
653900140825     c                   eval       IOREtser = 'C'
654000141107     c                   if         or_ddc>0 and or_ddc<d98ddc
654100141107     c                   eval       IOREDDC=or_ddc
654200141107     c                   eval       IOREnDC=8888888
654300141107     c                   eval       IOREcons = 'S'
654400141107     c                   else
654500140825     c                   eval       IOREDDC=d98ddc
654600140825     c                   eval       IOREnDC=wd98ndc
654700140825     c                   eval       IOREcons = d98cons
654800141107     c                   endif
654900141017     c                   eval       IOREtsp = d98tsp
655000140822     c
655100140822     c                   call      'TRULORSR'
655200140822     c                   parm                    kpjba
655300140822     c                   parm                    trulorSds
655400140825     c                   parm                    trulor2ds
655500140825     c
655600140825     c                   eval      wtrulor= 'S'
655700141107     c                   clear                   or_datarif        8 0
655800141107     c                   clear                   or_ddc            8 0
655900140822     C                   ENDSR
656000950406     C********************************************************************
656100950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
656200950406     C********************************************************************
656300950406     C     DATORE        BEGSR
656400950406     C*
656500950406     C                   Z-ADD     *ZEROS        WORE
656600950406     C     WDAT          IFNE      0
656700950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
656800950406     C     WAA           DIV       4             WQUA              4 0
656900950406     C                   MVR                     WRES
657000950406     C     WAA           MULT      365           WGIO              9 0
657100950406     C                   ADD       WQUA          WGIO
657200950406     C                   ADD       WGG           WGIO
657300950406     C     WMM           SUB       1             MC                3 0
657400950406     C                   DO        MC            WX                3 0
657500950406     C                   ADD       MES(WX)       WGIO
657600950406     C                   END
657700950406     C     WRES          IFEQ      0
657800950406     C     WMM           ANDLE     2
657900950406     C                   SUB       1             WGIO
658000950406     C                   END
658100950406     C     WGIO          MULT      24            WORE              9 0
658200950406     C                   ENDIF
658300950406     C*
658400950406     C                   ENDSR
658500930323     C********************************************************************
658600930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
658700930323     C********************************************************************
658800930323     C     OREDAT        BEGSR
658900930323     C*
659000950407     C                   SETOFF                                       717273
659100950407     C                   SETOFF                                       7475
659200930323     C     WORE          DIV       24            WGIO
659300930323     C                   MVR                     WRES              3 0
659400950406     C*
659500950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
659600950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
659700950406     C*  TROVO ANNO
659800950406     C     WGIO          DIV       365           WAA
659900950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
660000950406     C                   MVR                     WREST                71
660100950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
660200950406     C     WGIO          DIV       365           WAA
660300950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
660400950406     C*                                                    0 = 31/12/PR
660500950406     C   75
660600950406     CAN 71              DO                                                     BIS. E NON ERA
660700950406     C     WAA           SUB       1             WAA
660800950406     C                   Z-ADD     365           WREST
660900950406     C                   END
661000950406     C*
661100950406     C     WAA           DIV       4             WNBIS                                      T.
661200950406     C                   MVR                     WRBIS             3 073
661300950406     C   73
661400950406     CANN71                                                                     NON PIU' BISEST
661500950406     CORN73              ADD       1             WREST                          E'BISESTILE
661600950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
661700950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
661800950406     C  N73              SUB       1             $PM(1)
661900950406     C                   Z-ADD     1             WI                2 0
662000950406     C* TROVO IL MESE E GIORNI
662100950406     C     WREST         LOOKUP    $PM(WI)                            74  73
662200950406     C                   MOVE      WI            WMM
662300950406     C     WI            COMP      1                                      72
662400950406     C  N72
662500950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
662600950406     C   74              SUB       1             WI
662700950406     C  N72
662800950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
662900950406     C   72              MOVE      WREST         WGG
663000950406     C                   ADD       1900          WAA
663100950406     C*
663200930323     C*
663300930323     C                   ENDSR
6634009407151507 C********************************************************************
6635009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6636009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6637009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6638009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6639009407151507 C********************************************************************
6640009407151507 C     FINGIO        BEGSR
6641009407151507 C*
6642009407151507 C     WPORE         DIV       24            WGIO
6643009407151507 C                   MVR                     WRES
6644009407151507 C     WRES          IFEQ      *ZEROS
6645009407151507 C                   Z-ADD     12            WNORE             3 0
6646009407151507 C                   ELSE
6647009407151507 C                   Z-ADD     *ZEROS        WNORE
6648009407151507 C                   ENDIF
6649009407151507 C*
6650009407151507 C                   ENDSR
665100930517     C********************************************************************
665200971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
665300930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
665400930517     C********************************************************************
665500930517     C     CERCAL        BEGSR
665600930517     C*
665700930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
665800941227     C                   MOVE      CLNTFP        DCATFP
665900941227     C                   MOVE      CLNTFA        DCATFA
666000941227     C                   MOVE      CLNANN        DCAANN
666100941227     C                   MOVE      CLNMES        DCAMES
666200930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
666300930517     C                   Z-ADD     1             I
666400930517     C     DCALEN        LOOKUP    CAK(I)                                 99
666500930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
666600930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
666700941227     C     KEYCLN        CHAIN     AZCLN01L                           99
666800930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
666900930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
667000930517     C                   MOVEL     '1'           D98ERR
667100930517     C                   MOVEL     MSG(3)        D98MSG
667200930517     C                   ELSE                                                   <-- 2 -->
667300970401     C* CONTROLLA SE ANNULLATO
667400970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
667500970401     C                   MOVEL     '1'           D98ERR
667600970401     C                   MOVEL     MSG(3)        D98MSG
667700970401     C                   ELSE                                                   <-- 3 -->
667800930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
667900930517     C                   ADD       1             WINDCA            6 0
668000970401     C     WINDCA        IFLE      200                                          --- 4 -->
668100970401     C     WINDCA        ANDGT     *ZEROS
668200930517     C                   Z-ADD     WINDCA        I
668300930517     C                   MOVEL     DCALEN        CAK(I)
668400941227     C                   MOVEL     CLNMAT        CAM(I)
668500941227     C                   MOVEL     CLNPOM        CAP(I)
668600970401     C                   END                                                    <-- 4 ---
668700970401     C                   ENDIF                                                  <-- 3 ---
668800930517     C                   END                                                    <-- 2 ---
668900930517     C                   ELSE                                                   <-- 1 -->
669000930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
669100941227     C                   MOVEL     CAM(I)        CLNMAT
669200941227     C                   MOVEL     CAP(I)        CLNPOM
669300930517     C                   END                                                    <-- 1 ---
669400930517     C*
669500930517     C                   ENDSR
669600010821     C********************************************************************
669700010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
669800010821     C********************************************************************
669900010821     C     CHKVCA        BEGSR
670000010821     C*
670100010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
670200010821     C     D98CAS        IFGT      *ZEROS
670300010821     C     D98VCA        ANDEQ     *BLANKS
670400010821     C                   MOVEL     'ITL'         D98VCA
670500010821     C                   ENDIF
670600010822     C*
670700010822     C     D98VCA        IFNE      'ITL'
670800010822     C     D98VCA        ANDNE     'EUR'
670900010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
671000010822     C                   Z-ADD     D98CAS        INPIMP
671100010822     C                   MOVEL     D98VCA        INPDIV
671200010822     C                   EXSR      EXECNV
671300010822     C                   Z-ADD     OUTIMP        D98CAS
671400010822     C                   MOVEL     §GEDCN        D98VCA
671500010822     C                   ENDIF
671600010821     C*
671700010821     C                   ENDSR
671800010821     C********************************************************************
671900010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
672000010821     C********************************************************************
672100010821     C     EXECNV        BEGSR
672200010821     C*
672300010821     C                   CLEAR                   YEURDS
672400010821     C*
672500010821     C                   MOVEL     INPDIV        YECDVI
672600010821     C                   Z-ADD     INPIMP        YECIMI
672700010821     C                   MOVEL     §GEDCN        YECDVO
672800010821     C                   MOVEL     '3'           YECDCO
672900010821     C                   MOVEL     'H'           YECTAR
673000010821     C                   CALL      'YEURCO'
673100010821     C                   PARM                    YEURDS
673200010821     C*
673300010821     C                   Z-ADD     YECIMO        OUTIMP
673400010821     C*
673500010821     C                   ENDSR
673600011211     C*------------------------------------------------------------------------*
673700011211     C* Controllo importo massimo contrassegno.
673800011211     C*------------------------------------------------------------------------*
673900011211     C     REPMAXCSB     BEGSR
674000011211     C*
674100011211     C                   CLEAR                   Trul21DS
674200011211     C*
674300160420     c                   eval      i21cbo = d98cbo
674400011211     C                   EVAL      I21TSp = d98TSP
674500011211     C                   EVAL      I21NzD = d98NZD
674600011211     C                   EVAL      I21LnP = d98LNP
674700011211     C                   EVAL      I21LnA = d98LNA
674800011211     C                   EVAL      I21Ksc = d98KSC
674900011211     C                   EVAL      I21TiC = d98TIC
675000011211     C                   EVAL      I21Imp = d98CAS
675100011211     C                   EVAL      I21Div = d98VCA
675200160420     c                   eval      i21cbo = d98cbo
675300011211     C*
675400011211     C                   CALL      'TRUL21R'
675500011211     C                   PARM                    Trul21DS
675600160420
675700160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
675800160420     c                   if        wtrasm='N'
675900160420     c                   if        o21fca <>*blanks
676000160420     c                   eval      §b4ltca = 'S'
676100160420     c                   endif
676200160420     c                   endif
676300011211     C*
676400011211     C                   ENDSR
676500011211     C*------------------------------------------------------------------------*
676600011211
676700930414     C********************************************************************
676800930414     C*   RUTINZ - ROUTINE INIZIALE
676900930414     C********************************************************************
677000930414     C     RUTINZ        BEGSR
677100930414     C*
677200930414     C* PARAMETRI IN INPUT ALLA ROUTINE
677300930414     C     *ENTRY        PLIST
677400970731     C                   PARM                    DS99
677500941227     C*
677600941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
677700941227     C     KEYBLP        KLIST
677800941227     C                   KFLD                    BLPAAS
677900941227     C                   KFLD                    BLPLNP
678000941227     C                   KFLD                    BLPNRS
678100941227     C                   KFLD                    BLPNSP
678200050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
678300941227     C     KEYGCP        KLIST
678400050405     C                   KFLD                    KWGCPAAS
678500050405     C                   KFLD                    KWGCPLNP
678600050405     C                   KFLD                    KWGCPNRS
678700050405     C                   KFLD                    KWGCPNSP
678800050405     C                   KFLD                    KWGCPFRG
678900050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
679000031125     C     KEYGC_P       KLIST
679100050405     C                   KFLD                    KWGCPAAS
679200050405     C                   KFLD                    KWGCPLNP
679300050405     C                   KFLD                    KWGCPNRS
679400050405     C                   KFLD                    KWGCPNSP
679500970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
679600150120     C**   KEYBLT        KLIST
679700150120     C**                 KFLD                    BLTAAS
679800150120     C**                 KFLD                    BLTLNP
679900150120     C**                 KFLD                    BLTNRS
680000150120     C**                 KFLD                    BLTNSP
680100941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
680200941227     C     KEYARB        KLIST
680300941227     C                   KFLD                    ARBAAS
680400941227     C                   KFLD                    ARBLNP
680500941227     C                   KFLD                    ARBNRS
680600941227     C                   KFLD                    ARBNSP
680700051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
680800941228     C     KEYAR9        KLIST
680900941228     C                   KFLD                    AR9AAS
681000941228     C                   KFLD                    AR9LNP
681100941228     C                   KFLD                    AR9NRS
681200941228     C                   KFLD                    AR9NSP
681300941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
681400050405     C     KEYGCP21      KLIST
681500050405     C                   KFLD                    KWGCPAAS
681600050405     C                   KFLD                    KWGCPLNP
681700050405     C                   KFLD                    KWGCPNRS
681800050405     C                   KFLD                    KWGCPNSP
681900050405     C                   KFLD                    KWGCPFRG
682000970912     C* CHIAVE LETTURA FNART01L - PARZIALE
682100150120     C***  KEYART        KLIST
682200150120     C***                KFLD                    ARTAAS
682300150120     C***                KFLD                    ARTLNP
682400150120     C***                KFLD                    ARTNRS
682500150120     C***                KFLD                    ARTNSP
682600950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
682700950102     C     KEYLBL        KLIST
682800950102     C                   KFLD                    LBLAAN
682900950102     C                   KFLD                    LBLLPN
683000950102     C                   KFLD                    LBLNRN
683100950102     C                   KFLD                    LBLNSN
683200930414     C*
683300070827     c     kAR5          Klist
683400070827     c                   Kfld                    d98Aas
683500070827     c                   Kfld                    d98Lnp
683600070827     c                   Kfld                    d98nrs
683700070827     c                   Kfld                    d98nsp
683800070827     c                   Kfld                    kAr5Trd
683900150325     c     kAR4          Klist
684000150325     c                   Kfld                    d98Aas
684100150325     c                   Kfld                    d98Lnp
684200150325     c                   Kfld                    d98nrs
684300150325     c                   Kfld                    d98nsp
684400150325     c                   Kfld                    kAr4Trc
684500150218     c*
684600150218     c     kart          Klist
684700150218     c                   Kfld                    d98Aas
684800150218     c                   Kfld                    d98Lnp
684900150218     c                   Kfld                    d98nrs
685000150218     c                   Kfld                    d98nsp
685100150218     c     kbrv          Klist
685200150218     c                   Kfld                    artfls
685300150218     c                   Kfld                    artlna
685400150218     c                   Kfld                    artnrs
685500150218     c                   Kfld                    artnsc
685600150325     C     Kfapd         klist
685700150325     C                   kfld                    kpdtip
685800150325     C                   kfld                    kpdpdr
685900930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
686000930414     C     KEYTAB        KLIST
686100930414     C                   KFLD                    KTBUTE
686200930414     C                   KFLD                    KTBCOD
686300930414     C                   KFLD                    KTBKEY
686400060406     C     Ktab          KLIST
686500060406     C                   KFLD                    KTBUTE
686600060406     C                   KFLD                    KTBCOD
686700930414     C*
686800941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
686900941227     C     KEYCLN        KLIST
687000941227     C                   KFLD                    CLNTFP
687100941227     C                   KFLD                    CLNTFA
687200941227     C                   KFLD                    CLNANN
687300941227     C                   KFLD                    CLNMES
687400941227     C*
687500941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
687600941227     C     KEYCAS        KLIST
687700941227     C                   KFLD                    CASPTA
687800941227     C                   KFLD                    CASLNP
687900941227     C                   KFLD                    CASLNA
688000941227     C                   KFLD                    CASTSP
688100941227     C                   KFLD                    CASDDE
688200930420     C*
688300990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
688400930420     C     KEYEVB        KLIST
688500930420     C                   KFLD                    KEBAAS
688600930420     C                   KFLD                    KEBLNP
688700930420     C                   KFLD                    KEBNRS
688800930420     C                   KFLD                    KEBNSP
688900010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
689000010821     C     KTBE          KLIST
689100010821     C                   KFLD                    TBECOD
689200010821     C                   KFLD                    TBEKE1
689300010821     C                   KFLD                    TBEKE2
689400930414     C*
689500010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
689600930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
689700930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
689800930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
689900930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
690000930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
690100930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
690200930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
690300930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
690400930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
690500010821     C     *LIKE         DEFINE    D98CAS        INPIMP
690600010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
690700010821     C     *LIKE         DEFINE    D98VCA        INPDIV
690800930414     C*
690900930414     C* RECUPERO IL CODICE DELL'AZIENDA
691000930414     C                   Z-ADD     1             CODUT             1 0
691100970701     C                   Z-ADD     *ZEROS        I                 6 0
691200930414     C*
691300930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
691400930414     C                   EXSR      AZZERA
691500930414     C*
691600930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
691700941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
691800941227     C                   MOVEL     'S'           CTABSN            1
691900930414     C                   EXSR      CARTAB
692000941227     C                   ENDIF
692100041116     c* Se passato umero di spedizioni apro i files , altrimenti no
692200041116     c                   if        d98nsp>0 and wapri=' '
692300041116     c                   open      fnblp01l
692400050405     c                   open      tigcp01l
692500150120     c***                open      fnblt01l
692600041116     c                   open      fnarb01l
692700051115     c                   open      fiar901l
692800150325     c                   open      fiar401l
692900050405     c                   open      tigcp21l
693000150218     c                   open      fnart01l
693100150218     c                   open      fnbrv07l
693200041116     c                   open      fnlbl01l
693300041116     c                   open      fnevb01l
693400070827     c                   open      fiar501l
693500041116     c                   eval      wapri='S'
693600041116     c                   endif
693700930414     C*
693800930414     C                   ENDSR
693900941227     C*------------------------------------------------------------------------*
694000941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
694100941227     C*------------------------------------------------------------------------*
694200941227     C     SUBDAT        BEGSR
694300941227     C*
694400941227     C                   TIME                    DTTIME
694500941227     C                   Z-ADD     DTANN         DSANN
694600941227     C                   Z-ADD     DTMES         DSMES
694700941227     C                   Z-ADD     DTGIO         DSGIO
694800941227     C                   Z-ADD     DTSEC         DSSEC
694900941227     C*
695000941227     C                   ENDSR
695100930414     C********************************************************************
695200930414     C*   CARTAB - CARICA LE TABELLE
695300930414     C********************************************************************
695400930414     C     CARTAB        BEGSR
695500930414     C*
695600930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
695700930414     C     D98ERR        IFEQ      '0'
695800930421     C                   Z-ADD     0             I
695900930414     C                   Z-ADD     CODUT         KTBUTE
696000930414     C                   MOVEL     '1Y'          KTBCOD
696100930414     C                   MOVEL     *BLANKS       KTBKEY
696200930414     C     KEYTAB        SETLL     TABEL00F                           99
696300930414     C  N99              READ      TABEL00F                               99
696400930414     C     *IN99         IFEQ      '1'
696500930414     C                   MOVEL     '1'           D98ERR
696600930414     C                   MOVEL     MSG(5)        D98MSG
696700930414     C                   END
696800930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
696900930414     C     TBLCOD        ANDEQ     '1Y'
697000930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
697100930421     C                   ADD       1             I
697200930421     C                   MOVEL     TBLKEY        C1Y(I)
697300930414     C                   MOVEL     TBLUNI        DS1Y
697400930421     C                   MOVEL     §1YFEI        I1Y(I)
697500930421     C                   MOVEL     §1YFEC        E1Y(I)
697600930414     C                   END                                                    <-- 3 ---
697700930414     C                   READ      TABEL00F                               99
697800930414     C                   END                                                    <-- 2 ---
697900930414     C                   END
698000930414     C*
698100930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
698200930414     C     D98ERR        IFEQ      '0'
698300930421     C                   Z-ADD     0             I
698400930414     C                   Z-ADD     CODUT         KTBUTE
698500930414     C                   MOVEL     '1X'          KTBCOD
698600930414     C                   MOVEL     *BLANKS       KTBKEY
698700930414     C     KEYTAB        SETLL     TABEL00F                           99
698800930414     C  N99              READ      TABEL00F                               99
698900930414     C     *IN99         IFEQ      '1'
699000930414     C                   MOVEL     '1'           D98ERR
699100930414     C                   MOVEL     MSG(1)        D98MSG
699200930414     C                   END
699300930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
699400930414     C     TBLCOD        ANDEQ     '1X'
699500930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
699600930421     C                   ADD       1             I
699700930421     C                   MOVEL     TBLKEY        C1X(I)
699800930414     C                   MOVEL     TBLUNI        DS1X
699900930421     C                   Z-ADD     §1XHHG        G1X(I)
700000930421     C                   Z-ADD     §1XHHO        O1X(I)
700100930421     C                   Z-ADD     §1XHHM        M1X(I)
700200930421     C                   Z-ADD     §1XHHP        P1X(I)
700300930421     C                   Z-ADD     §1XHHI        I1X(I)
700400930421     C                   Z-ADD     §1XHHS        S1X(I)
700500930414     C                   END                                                    <-- 3 ---
700600930414     C                   READ      TABEL00F                               99
700700931223     C                   ENDDO                                                  <-- 2 ---
700800050523     c
700900050523     c* controllo riempimento skiere
701000050523     c                   clear                   trul0sds
701100050523     c                   eval      i0sski='C1X/G1X/O1X/...'
701200050523     c                   eval      i0sele=%elem(c1x)
701300050523     c                   eval      i0spie=i
701400050523     c                   eval      i0sfile='TABEL00F  '
701500050523     c                   eval      i0scod=' 1X  '
701600050523     c                   eval      i0spgm='TNSD99R'
701700050523     c                   movel     trul0sds      kpjbu
701800050523     c                   call      'TRUL0SR'
701900050523     c                   parm                    kpjba
702000050523     C                   END
702100930421     C*
702200930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
702300930421     C     D98ERR        IFEQ      '0'
702400930421     C                   Z-ADD     0             I
702500930421     C                   Z-ADD     CODUT         KTBUTE
702600930421     C                   MOVEL     '2A'          KTBCOD
702700060406     C     Ktab          SETLL     TABEL00F                           99
702800060406     C  N99ktab          READe     TABEL00F                               99
702900930421     C     *IN99         IFEQ      '1'
703000930421     C                   MOVEL     '1'           D98ERR
703100930421     C                   MOVEL     MSG(8)        D98MSG
703200930421     C                   END
703300930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
703400930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
703500930421     C                   ADD       1             I
703600930421     C                   MOVEL     TBLKEY        C2A(I)
703700060406     C                   MOVEL     TBLUNI        ds2a
703800060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
703900060406     C                   MOVEL     w003a         D2A(I)
704000930421     C                   END                                                    <-- 3 ---
704100060406     C     ktab          READe     TABEL00F                               99
704200931223     C                   ENDDO                                                  <-- 2 ---
704300050523     c* controllo riempimento skiere
704400050523     c                   clear                   trul0sds
704500050523     c                   eval      i0sski='C2A/D2A'
704600050523     c                   eval      i0sele=%elem(c2a)
704700050523     c                   eval      i0spie=i
704800050523     c                   eval      i0sfile='TABEL00F  '
704900060406     c                   eval      i0scod='2A/2Z'
705000050523     c                   eval      i0spgm='TNSD99R'
705100050523     c                   movel     trul0sds      kpjbu
705200050523     c                   call      'TRUL0SR'
705300050523     c                   parm                    kpjba
705400930421     C                   END
705500060406     c* Tabella  2Z
705600060406     C     D98ERR        IFEQ      '0'
705700060406     C                   Z-ADD     CODUT         KTBUTE
705800060406     C                   MOVEL     '2Z'          KTBCOD
705900060406     C     Ktab          SETLL     TABEL00F                           99
706000060406     C  N99ktab          READe     TABEL00F                               99
706100060406     C     *IN99         IFEQ      '1'
706200060406     C                   MOVEL     '1'           D98ERR
706300060406     C                   MOVEL     MSG(33)       D98MSG
706400060406     C                   END
706500060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
706600060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
706700060406     C                   Z-ADD     1             I
706800060406     C                   MOVEL     TBLKEY        w003a
706900060406     c     w003a         lookup    c2a(i)                                 99
707000060406     C     *IN99         IFEQ      *off
707100060406     C                   MOVEL     '1'           D98ERR
707200060406     C                   MOVEL     MSG(34)       D98MSG
707300060406     C                   Else
707400060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
707500060406     C                   END                                                    <-- 3 ---
707600060406     C                   END                                                    <-- 3 ---
707700060406     C     ktab          READe     TABEL00F                               99
707800060406     C                   ENDDO                                                  <-- 2 ---
707900060406     C                   END
708000941227     C*
708100941227     C* CARICA TABELLA CODICI BOLLA -3A-
708200941227     C     D98ERR        IFEQ      '0'
708300941228     C                   Z-ADD     0             I
708400941227     C                   Z-ADD     CODUT         KTBUTE
708500941227     C                   MOVEL     '3A'          KTBCOD
708600941227     C                   MOVEL     *BLANKS       KTBKEY
708700941227     C     KEYTAB        SETLL     TABEL00F                           99
708800941227     C  N99              READ      TABEL00F                               99
708900941227     C     *IN99         IFEQ      '1'
709000941227     C                   MOVEL     '1'           D98ERR
709100941227     C                   MOVEL     MSG(22)       D98MSG
709200941227     C                   END
709300941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
709400941227     C     TBLCOD        ANDEQ     '3A'
709500941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
709600941227     C                   ADD       1             I
709700941227     C                   MOVEL     TBLKEY        C3A(I)
709800941227     C                   MOVEL     TBLUNI        DS3A
709900941227     C                   MOVEL     §3ATB1        T3A(I)
710000941228     C                   Z-ADD     §3AFCA        F3A(I)
710100990308     C                   MOVEL     §3ARBL        R3A(I)
710200941227     C                   END                                                    <-- 3 ---
710300941227     C                   READ      TABEL00F                               99
710400941227     C                   END                                                    <-- 2 ---
710500050523     c* controllo riempimento skiere
710600050523     c                   clear                   trul0sds
710700050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
710800050523     c                   eval      i0sele=%elem(c3a)
710900050523     c                   eval      i0spie=i
711000050523     c                   eval      i0sfile='TABEL00F  '
711100050523     c                   eval      i0scod=' 3A  '
711200050523     c                   eval      i0spgm='TNSD99R'
711300050523     c                   movel     trul0sds      kpjbu
711400050523     c                   call      'TRUL0SR'
711500050523     c                   parm                    kpjba
711600941227     C                   END
711700971210     C*
711800020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
711900020226     C                   Z-ADD     *ZEROS        J                 3 0
712000160331     C                   Z-ADD     *ZEROS        D                 3 0
712100971210IF  0C     D98ERR        IFEQ      '0'
712200971210     C     *LOVAL        SETLL     AZORG01L
712300971210     C                   READ      AZORG01L                               98
712400971210DO  1C     *IN98         DOWEQ     *OFF
712500971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
712600971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
712700971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
712800020627     C                   MOVEL     ORGDE3        OG143
712900020702     C*
713000020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
713100020702     C                   CLEAR                   DNTW
713200020702     C*
713300020702     C                   MOVEL(P)  'NTW'         TBECOD
713400020702     C                   MOVEL(P)  §OGNTW        TBEKE1
713500020702     C     KTBE          CHAIN     TNTBE01L
713600020702     C                   IF        %found(TNTBE01L)
713700020702     C                   MOVEL     TBEUNI        DNTW
713800020702     C                   ENDIF
713900020702     C*
714000020702     C                   MOVEL     §NTWFIE       FL1(F)
714100020627     C*
714200020226     C* VERIFICO SE FILIALE FEDEX
714300020226     C                   IF        §OGNTW = 'FED'
714400020226     C                   ADD       1             J
714500020226     C                   Z-ADD     ORGFIL        FILFED(J)
714600020226     C                   ENDIF
714700160331     C* VERIFICO SE FILIALE DPD
714800160331     C                   IF        §OGNTW = 'DPD'
714900160331     C                   ADD       1             D
715000160331     C                   Z-ADD     ORGFIL        FILDPD(D)
715100160331     C                   ENDIF
715200020226     C*
715300971210E   2C                   ENDIF
715400971210     C                   READ      AZORG01L                               98
715500971210E   1C                   ENDDO
715600971210E   0C                   ENDIF
715700010821     C*
715800010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
715900010821     C                   MOVEL     'GED'         TBECOD
716000070514     C                   MOVEL     *BLANKS       TBEKE1
716100070514     C                   MOVEL     '1'           TBEKE1
716200010821     C                   MOVEL     *BLANKS       TBEKE2
716300010821     C     KTBE          CHAIN     TNTBE01L                           21
716400070514     C  N21              MOVEL     TBEUNI        DGED
716500150326     c
716600150326     C* REPERISCO LA tabella flag operativi per tabella OLP
716700150326     c                   clear                   dvpo
716800150326     C                   MOVEL     'VPO'         TBECOD
716900150326     C                   MOVEL     *BLANKS       TBEKE2
717000150326     C                   MOVEL(P)  'VPO'         TBEKE1
717100150326     C     KTBE          CHAIN     TNTBE01L                           21
717200150326     C  N21              MOVEL     TBEUNI        DVPO
717300150326     C*
717400031201     C*
717500031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
717600031201     C     D98ERR        IFEQ      '0'
717700060403     C                   Z-ADD     0             I7QW              3 0
717800031201     C                   Z-ADD     CODUT         KTBUTE
717900031201     C                   MOVEL     '7Q'          KTBCOD
718000031201     C                   MOVEL     *BLANKS       KTBKEY
718100031201     C     KEYTAB        SETLL     TABEL00F                           99
718200031201     C  N99              READ      TABEL00F                               99
718300031201     C     *IN99         IFEQ      '1'
718400031201     C                   MOVEL     '1'           D98ERR
718500031201     C                   MOVEL     MSG(8)        D98MSG
718600031201     C                   ENDIF
718700031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
718800031201     C     TBLCOD        ANDEQ     '7Q'
718900031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
719000031201     C                   MOVEL     TBLUNI        DS7Q
719100031201     C                   IF        §7Q4D1 = 'R' OR
719200031201     C                             §7Q4D2 = 'R' OR
719300031201     C                             §7Q4D3 = 'R' OR
719400031201     C                             §7Q4D4 = 'R' OR
719500031201     C                             §7Q4D5 = 'R'
719600031201     C                   ADD       1             I7QW
719700031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
719800031201     C                   ENDIF                                                  <-- 3 ---
719900031201     C                   ENDIF                                                  <-- 3 ---
720000031201     C                   READ      TABEL00F                               99
720100031201     C                   ENDDO                                                  <-- 2 ---
720200031201     C                   ENDIF
720300031201     C*
720400031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
720500031201     C     D98ERR        IFEQ      '0'
720600031201     C                   Z-ADD     CODUT         KTBUTE
720700031201     C                   MOVEL     '7W'          KTBCOD
720800031201     C                   MOVEL     *BLANKS       KTBKEY
720900031201     C     KEYTAB        SETLL     TABEL00F                           99
721000031201     C  N99              READ      TABEL00F                               99
721100031201     C     *IN99         IFEQ      '1'
721200031201     C                   MOVEL     '1'           D98ERR
721300031201     C                   MOVEL     MSG(8)        D98MSG
721400031201     C                   ENDIF
721500031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
721600031201     C     TBLCOD        ANDEQ     '7W'
721700031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
721800031201     C                   MOVEL     TBLUNI        DS7W
721900031201     C                   IF        §7W6D1 = 'C' OR
722000031201     C                             §7W6D2 = 'C' OR
722100031201     C                             §7W6D3 = 'C' OR
722200031201     C                             §7W6D4 = 'C' OR
722300031201     C                             §7W6D5 = 'C'
722400031201     C                   ADD       1             I7QW
722500031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
722600031201     C                   ENDIF                                                  <-- 3 ---
722700031201     C                   ENDIF                                                  <-- 3 ---
722800031201     C                   READ      TABEL00F                               99
722900031201     C                   ENDDO                                                  <-- 2 ---
723000050523     c* controllo riempimento skiere
723100050523     c                   clear                   trul0sds
723200050523     c                   eval      i0sski='SK7QW'
723300050523     c                   eval      i0sele=%elem(sk7qw)
723400050523     c                   eval      i0spie=i7qw
723500050523     c                   eval      i0sfile='TABEL00F  '
723600050523     c                   eval      i0scod='7Q-7W'
723700050523     c                   eval      i0spgm='TNSD99R'
723800050523     c                   movel     trul0sds      kpjbu
723900050523     c                   call      'TRUL0SR'
724000050523     c                   parm                    kpjba
724100031201     C                   ENDIF
724200930414     C*
724300930414     C                   ENDSR
724400930414     C********************************************************************
724500930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
724600930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
724700930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
724800930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
724900930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
725000930414     C********************************************************************
725100930414     C     RO3ORE        BEGSR
725200930414     C*
725300930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
725400930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
725500930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
725600050902     c                   movel     ' '           WR3MATPOM         1
725700931209     C                   Z-ADD     *ZEROS        WR3HHO
725800931209     C                   Z-ADD     *ZEROS        WR3HHA
725900930414     C                   MOVE      WR3TAB        WR3COD
726000930604     C                   MOVEL     D98LNA        WR3COD            8
726100930421     C                   Z-ADD     1             I
726200930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
726300930414     C     *IN99         IFEQ      '0'
726400930414     C                   MOVE      WR3TAB        WR3COD
726500930414     C                   MOVEL     '   '         WR3COD            8
726600930421     C                   Z-ADD     1             I
726700930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
726800930414     C                   END
726900930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
727000930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
727100930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
727200930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
727300930414     C                   ELSE                                                   <-- 2 -->
727400930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
727500930421     C                   Z-ADD     M1X(I)        WR3HHO
727600930510     C                   Z-ADD     I1X(I)        WR3HHA
727700050902     c                   movel     'M'           WR3MATPOM         1
727800930414     C                   END
727900930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
728000930421     C                   Z-ADD     P1X(I)        WR3HHO
728100930510     C                   Z-ADD     S1X(I)        WR3HHA
728200050902     c                   movel     'P'           WR3MATPOM
728300930414     C                   END
728400930414     C                   END                                                    <-- 2 ---
728500930414     C                   END                                                    <-- 1 ---
728600930414     C*
728700930414     C                   ENDSR
728800930423     C********************************************************************
728900930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
729000930423     C********************************************************************
729100930423     C     W98D98        BEGSR
729200930423     C*
729300941228     C*
729400941228     C* ORE REALI PARTENZA INTERNE
729500941228     C                   Z-ADD     W98RPI        W98               3 0
729600941228     C                   EXSR      W98999
729700941228     C                   Z-ADD     D98           D98RPI
729800941228     C* ORE REALI PARTENZA CLIENTE
729900941228     C                   Z-ADD     W98RPC        W98
730000941228     C                   EXSR      W98999
730100941228     C                   Z-ADD     D98           D98RPC
730200930423     C* ORE REALI TRAZIONE INTERNE
730300941228     C                   Z-ADD     W98RTI        W98
730400941228     C                   EXSR      W98999
730500941228     C                   Z-ADD     D98           D98RTI
730600930423     C* ORE REALI TRAZIONE CLIENTE
730700941228     C                   Z-ADD     W98RTC        W98
730800941228     C                   EXSR      W98999
730900941228     C                   Z-ADD     D98           D98RTC
731000930423     C* ORE REALI CONSEGNA INTERNE
731100941228     C                   Z-ADD     W98RCI        W98
731200941228     C                   EXSR      W98999
731300941228     C                   Z-ADD     D98           D98RCI
731400930423     C* ORE REALI CONSEGNA CLIENTE
731500941228     C                   Z-ADD     W98RCC        W98
731600941228     C                   EXSR      W98999
731700941228     C                   Z-ADD     D98           D98RCC
731800930423     C* ORE REALI RESA INTERNE
731900941228     C                   Z-ADD     W98RRI        W98
732000941228     C                   EXSR      W98999
732100941228     C                   Z-ADD     D98           D98RRI
732200930423     C* ORE REALI RESA CLIENTE
732300941228     C                   Z-ADD     W98RRC        W98
732400941228     C                   EXSR      W98999
732500941228     C                   Z-ADD     D98           D98RRC
732600941228     C* ORE DIFF. PARTENZA INTERNE
732700941228     C                   Z-ADD     W98FPI        W98
732800941228     C                   EXSR      W98999
732900941228     C                   Z-ADD     D98           D98FPI
733000941228     C* ORE DIFF. PARTENZA CLIENTE
733100941228     C                   Z-ADD     W98FPC        W98
733200941228     C                   EXSR      W98999
733300941228     C                   Z-ADD     D98           D98FPC
733400930423     C* ORE DIFF. TRAZIONE INTERNE
733500941228     C                   Z-ADD     W98FTI        W98
733600941228     C                   EXSR      W98999
733700941228     C                   Z-ADD     D98           D98FTI
733800930423     C* ORE DIFF. TRAZIONE CLIENTE
733900941228     C                   Z-ADD     W98FTC        W98
734000941228     C                   EXSR      W98999
734100941228     C                   Z-ADD     D98           D98FTC
734200930423     C* ORE DIFF. CONSEGNA INTERNE
734300941228     C                   Z-ADD     W98FCI        W98
734400941228     C                   EXSR      W98999
734500941228     C                   Z-ADD     D98           D98FCI
734600930423     C* ORE DIFF. CONSEGNA CLIENTE
734700941228     C                   Z-ADD     W98FCC        W98
734800941228     C                   EXSR      W98999
734900941228     C                   Z-ADD     D98           D98FCC
735000930423     C* ORE DIFF. RESA INTERNE
735100941228     C                   Z-ADD     W98FRI        W98
735200941228     C                   EXSR      W98999
735300941228     C                   Z-ADD     D98           D98FRI
735400930423     C* ORE DIFF. RESA CLIENTE
735500941228     C                   Z-ADD     W98FRC        W98
735600941228     C                   EXSR      W98999
735700941228     C                   Z-ADD     D98           D98FRC
735800941228     C* ORE NON CONF. PARTENZA INTERNE
735900941228     C     W98NPI        IFGT      999
736000941228     C                   Z-ADD     999           D98NPI
736100941228     C                   ELSE
736200941228     C                   Z-ADD     W98NPI        D98NPI
736300941228     C                   END
736400941228     C* ORE NON CONF. PARTENZA CLIENTE
736500941228     C     W98NPC        IFGT      999
736600941228     C                   Z-ADD     999           D98NPC
736700941228     C                   ELSE
736800941228     C                   Z-ADD     W98NPC        D98NPC
736900941228     C                   END
737000930423     C* ORE NON CONF. TRAZIONE INTERNE
737100930423     C     W98NTI        IFGT      999
737200930423     C                   Z-ADD     999           D98NTI
737300930423     C                   ELSE
737400930423     C                   Z-ADD     W98NTI        D98NTI
737500930423     C                   END
737600930423     C* ORE NON CONF. TRAZIONE CLIENTE
737700930423     C     W98NTC        IFGT      999
737800930423     C                   Z-ADD     999           D98NTC
737900930423     C                   ELSE
738000930423     C                   Z-ADD     W98NTC        D98NTC
738100930423     C                   END
738200930423     C* ORE NON CONF. CONSEGNA INTERNE
738300930423     C     W98NCI        IFGT      999
738400930423     C                   Z-ADD     999           D98NCI
738500930423     C                   ELSE
738600930423     C                   Z-ADD     W98NCI        D98NCI
738700930423     C                   END
738800930423     C* ORE NON CONF. CONSEGNA CLIENTE
738900930423     C     W98NCC        IFGT      999
739000930423     C                   Z-ADD     999           D98NCC
739100930423     C                   ELSE
739200930423     C                   Z-ADD     W98NCC        D98NCC
739300930423     C                   END
739400930423     C* ORE NON CONF. RESA INTERNE
739500930423     C     W98NRI        IFGT      999
739600930423     C                   Z-ADD     999           D98NRI
739700930423     C                   ELSE
739800930423     C                   Z-ADD     W98NRI        D98NRI
739900930423     C                   END
740000930423     C* ORE NON CONF. RESA CLIENTE
740100930423     C     W98NRC        IFGT      999
740200930423     C                   Z-ADD     999           D98NRC
740300930423     C                   ELSE
740400930423     C                   Z-ADD     W98NRC        D98NRC
740500930423     C                   END
740600930423     C* ORE X TURNO CHIUSURA INTERNE
740700941228     C                   Z-ADD     W98HHI        W98
740800941228     C                   EXSR      W98999
740900941228     C                   Z-ADD     D98           D98HHI
741000930423     C* ORE X TURNO CHIUSURA CLIENTE
741100941228     C                   Z-ADD     W98HHC        W98
741200941228     C                   EXSR      W98999
741300941228     C                   Z-ADD     D98           D98HHC
741400060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
741500060418     C                   Z-ADD     W98EGI        W98
741600060418     C                   EXSR      W98999
741700060418     C                   Z-ADD     D98           D98hEGI
741800060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
741900060418     C                   Z-ADD     W98EGC        W98
742000060418     C                   EXSR      W98999
742100060418     C                   Z-ADD     D98           D98hEGC
742200060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
742300060420     C                   Z-ADD     W98EAI        W98
742400060420     C                   EXSR      W98999
742500060420     C                   Z-ADD     D98           D98hEAI
742600060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
742700060420     C                   Z-ADD     W98EAC        W98
742800060420     C                   EXSR      W98999
742900060420     C                   Z-ADD     D98           D98hEAC
743000930423     C* ORE X LASC. AVVISO INTERNE
743100941228     C                   Z-ADD     W98HLI        W98
743200941228     C                   EXSR      W98999
743300941228     C                   Z-ADD     D98           D98HLI
743400930423     C* ORE X LASC. AVVISO CLIENTE
743500941228     C                   Z-ADD     W98HLC        W98
743600941228     C                   EXSR      W98999
743700941228     C                   Z-ADD     D98           D98HLC
743800930423     C* ORE X GIACENZA INTERNE
743900941228     C                   Z-ADD     W98HGI        W98
744000941228     C                   EXSR      W98999
744100941228     C                   Z-ADD     D98           D98HGI
744200930423     C* ORE X GIACENZA CLIENTE
744300941228     C                   Z-ADD     W98HGC        W98
744400941228     C                   EXSR      W98999
744500941228     C                   Z-ADD     D98           D98HGC
744600941228     C* ORE X FEST. PART. INTERNE
744700941228     C                   Z-ADD     W98HPI        W98
744800941228     C                   EXSR      W98999
744900941228     C                   Z-ADD     D98           D98HPI
745000941228     C* ORE X FEST. PART. CLIENTE
745100941228     C                   Z-ADD     W98HPC        W98
745200941228     C                   EXSR      W98999
745300941228     C                   Z-ADD     D98           D98HPC
745400930423     C* ORE X FEST. TRAZ. INTERNE
745500941228     C                   Z-ADD     W98HFI        W98
745600941228     C                   EXSR      W98999
745700941228     C                   Z-ADD     D98           D98HFI
745800930423     C* ORE X FEST. TRAZ. CLIENTE
745900941228     C                   Z-ADD     W98HFC        W98
746000941228     C                   EXSR      W98999
746100941228     C                   Z-ADD     D98           D98HFC
746200930423     C* ORE X FEST. CONS. INTERNE
746300941228     C                   Z-ADD     W98HEI        W98
746400941228     C                   EXSR      W98999
746500941228     C                   Z-ADD     D98           D98HEI
746600930423     C* ORE X FEST. CONS. CLIENTE
746700941228     C                   Z-ADD     W98HEC        W98
746800941228     C                   EXSR      W98999
746900941228     C                   Z-ADD     D98           D98HEC
747000930511     C* ORE X F.DEPOSITO INTERNE
747100941228     C                   Z-ADD     W98HDI        W98
747200941228     C                   EXSR      W98999
747300941228     C                   Z-ADD     D98           D98HDI
747400930511     C* ORE X F.DEPOSITO CLIENTE
747500941228     C                   Z-ADD     W98HDC        W98
747600941228     C                   EXSR      W98999
747700941228     C                   Z-ADD     D98           D98HDC
747800930511     C* ORE X CONTRASSEGNO INTERNE
747900941228     C                   Z-ADD     W98HOI        W98
748000941228     C                   EXSR      W98999
748100941228     C                   Z-ADD     D98           D98HOI
748200930511     C* ORE X CONTRASSEGNO CLIENTE
748300941228     C                   Z-ADD     W98HOC        W98
748400941228     C                   EXSR      W98999
748500941228     C                   Z-ADD     D98           D98HOC
748600930914     C* ORE DI CONSEGNABILITA' INTERNE
748700941228     C                   Z-ADD     W98OCI        W98
748800941228     C                   EXSR      W98000
748900941228     C                   Z-ADD     D98           D98OCI
749000930914     C* ORE DI CONSEGNABILITA' CLIENTE
749100941228     C                   Z-ADD     W98OCC        W98
749200941228     C                   EXSR      W98000
749300941228     C                   Z-ADD     D98           D98OCC
749400071122     C
749500071122     c* Per i codici bolla M prepagati annullati, azzero tutto
749600071122     c                   if        d98cbo='M '
749700071122     c                   clear                   d98rpi
749800071122     c                   clear                   d98rpc
749900071122     c                   clear                   d98rti
750000071122     c                   clear                   d98rtc
750100071122     c                   clear                   d98rci
750200071122     c                   clear                   d98rcc
750300071122     c                   clear                   d98rri
750400071122     c                   clear                   d98rrc
750500071122     c                   clear                   d98fpi
750600071122     c                   clear                   d98fpc
750700071122     c                   clear                   d98fti
750800071122     c                   clear                   d98ftc
750900071122     c                   clear                   d98fci
751000071122     c                   clear                   d98fcc
751100071122     c                   clear                   d98fri
751200071122     c                   clear                   d98frc
751300071122     c                   clear                   d98npi
751400071122     c                   clear                   d98npc
751500071122     c                   clear                   d98nti
751600071122     c                   clear                   d98ntc
751700071122     c                   clear                   d98nci
751800071122     c                   clear                   d98ncc
751900071122     c                   clear                   d98nri
752000071122     c                   clear                   d98nrc
752100071122     c                   clear                   d98OCi
752200071122     c                   clear                   d98OCC
752300071122     c                   endif
752400930423     C*
752500100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
752600100308     c*  calcolo
752700100308     c                   eval      d98dcr=s98dcr
752800100308     c                   eval      d98hcr=s98hcr
752900100308     c                   eval      d98tcr=s98tcr
753000160405     c
753100160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
753200160405     c*  perchè il pgm usa il campo verificado se =10 )
753300160405     c                   if        wVP='S'
753400160405     c                   add       24            d98tci
753500160405     c                   add       24            d98tcc
753600160405     c                   add       24            d98tri
753700160405     c                   add       24            d98trc
753800160405     c                   endif
753900100308     c
754000930423     C                   ENDSR
754100941228     C***********************************************************************
754200941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
754300941228     C***********************************************************************
754400941228     C     W98999        BEGSR
754500941228     C     W98           IFLT      -999
754600941228     C                   Z-ADD     -999          D98               3 0
754700941228     C                   ELSE
754800941228     C     W98           IFGT      999
754900941228     C                   Z-ADD     999           D98
755000941228     C                   ELSE
755100941228     C                   Z-ADD     W98           D98
755200941228     C                   END
755300941228     C                   END
755400941228     C                   ENDSR
755500941228     C***********************************************************************
755600941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
755700941228     C***********************************************************************
755800941228     C     W98000        BEGSR
755900941228     C     W98           IFLE      *ZEROS
756000941228     C                   Z-ADD     *ZEROS        D98
756100941228     C                   ELSE
756200941228     C     W98           IFGT      999
756300941228     C                   Z-ADD     999           D98
756400941228     C                   ELSE
756500941228     C                   Z-ADD     W98           D98
756600941228     C                   END
756700941228     C                   END
756800941228     C                   ENDSR
756900930305     C***********************************************************************
757000930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
757100930305     C***********************************************************************
757200930305     C     AZZERA        BEGSR
757300150219     c* data e ora del momento
757400150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
757500150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
757600150219     C                   Z-ADD     DTORA4        WHHODI            4 0
757700930305     C*
757800141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
757900150603     C                   MOVEL     'N'           opendan
758000141107     c                   clear                   or_ddc
758100141108     c                   clear                   sav_12dat         8 0
758200141107     c                   clear                   or_datarif
758300140402     c                   clear                   wpick2            1
758400140402     c                   clear                   Primotipo
758500090928     c                   clear                   Primodev
758600090928     c                   clear                   Primohev
758700070827     c                   clear                   CallAR5
758800071122     c                   clear                   d98cbo
758900100308     c                   clear                   s98dcr
759000100308     c                   clear                   s98hcr
759100100308     c                   clear                   s98tcr
759200141106     c                   clear                   nosomma           1
759300141229     c                   clear                   wtrasm
759400150324     c                   clear                   avvcmc
759500150325     c                   clear                   DSbl4L
759600160401     c                   clear                   dsbl4I
759700150401     c                   clear                   fnlv82ds
759800150331     c                   clear                   DS7R
759900150605     c                   clear                   wDEE              1
760000150629     c                   clear                   wtsogliaE
760100930323     C                   MOVEL     '0'           D98ERR
760200930323     C                   MOVEL     *BLANKS       D98MSG
760300931124     C                   Z-ADD     *ZEROS        D98TPI
760400931124     C                   Z-ADD     *ZEROS        D98TPC
760500930325     C                   Z-ADD     *ZEROS        D98TTI
760600930325     C                   Z-ADD     *ZEROS        D98TTC
760700930325     C                   Z-ADD     *ZEROS        D98TCI
760800930325     C                   Z-ADD     *ZEROS        D98TCC
760900090827     C                   Z-ADD     *ZEROS        w99TCI
761000090827     C                   Z-ADD     *ZEROS        w99TCC
761100930325     C                   Z-ADD     *ZEROS        D98TRI
761200930325     C                   Z-ADD     *ZEROS        D98TRC
761300090921     C                   Z-ADD     *ZEROS        w99TRI
761400090921     C                   Z-ADD     *ZEROS        w99TRC
761500931124     C                   Z-ADD     *ZEROS        D98RPI
761600931124     C                   Z-ADD     *ZEROS        D98RPC
761700930405     C                   Z-ADD     *ZEROS        D98RTI
761800930405     C                   Z-ADD     *ZEROS        D98RTC
761900930405     C                   Z-ADD     *ZEROS        D98RCI
762000930405     C                   Z-ADD     *ZEROS        D98RCC
762100930405     C                   Z-ADD     *ZEROS        D98RRI
762200930405     C                   Z-ADD     *ZEROS        D98RRC
762300931124     C                   Z-ADD     *ZEROS        D98FPI
762400931124     C                   Z-ADD     *ZEROS        D98FPC
762500930325     C                   Z-ADD     *ZEROS        D98FTI
762600930325     C                   Z-ADD     *ZEROS        D98FTC
762700930325     C                   Z-ADD     *ZEROS        D98FCI
762800930325     C                   Z-ADD     *ZEROS        D98FCC
762900930325     C                   Z-ADD     *ZEROS        D98FRI
763000930325     C                   Z-ADD     *ZEROS        D98FRC
763100931124     C                   MOVEL     *BLANKS       D98CPI
763200931124     C                   MOVEL     *BLANKS       D98CPC
763300930325     C                   MOVEL     *BLANKS       D98CTI
763400930325     C                   MOVEL     *BLANKS       D98CTC
763500930325     C                   MOVEL     *BLANKS       D98CCI
763600930325     C                   MOVEL     *BLANKS       D98CCC
763700930325     C                   MOVEL     *BLANKS       D98CRI
763800930325     C                   MOVEL     *BLANKS       D98CRC
763900931124     C                   Z-ADD     *ZEROS        D98NPI
764000931124     C                   Z-ADD     *ZEROS        D98NPC
764100930325     C                   Z-ADD     *ZEROS        D98NTI
764200930325     C                   Z-ADD     *ZEROS        D98NTC
764300930325     C                   Z-ADD     *ZEROS        D98NCI
764400930325     C                   Z-ADD     *ZEROS        D98NCC
764500930325     C                   Z-ADD     *ZEROS        D98NRI
764600930325     C                   Z-ADD     *ZEROS        D98NRC
764700930414     C                   Z-ADD     *ZEROS        D98HHI
764800930414     C                   Z-ADD     *ZEROS        D98HHC
764900930414     C                   Z-ADD     *ZEROS        D98HOI
765000930414     C                   Z-ADD     *ZEROS        D98HOC
765100930414     C                   Z-ADD     *ZEROS        D98HLI
765200930414     C                   Z-ADD     *ZEROS        D98HLC
765300930414     C                   Z-ADD     *ZEROS        D98HGI
765400930414     C                   Z-ADD     *ZEROS        D98HGC
765500931124     C                   Z-ADD     *ZEROS        D98HPI
765600931124     C                   Z-ADD     *ZEROS        D98HPC
765700930414     C                   Z-ADD     *ZEROS        D98HFI
765800930414     C                   Z-ADD     *ZEROS        D98HFC
765900930416     C                   Z-ADD     *ZEROS        D98HEI
766000930416     C                   Z-ADD     *ZEROS        D98HEC
766100930414     C                   Z-ADD     *ZEROS        D98HRI
766200930414     C                   Z-ADD     *ZEROS        D98HRC
766300930511     C                   Z-ADD     *ZEROS        D98HDI
766400930511     C                   Z-ADD     *ZEROS        D98HDC
766500930325     C                   Z-ADD     *ZEROS        D98DCI
766600930325     C                   Z-ADD     *ZEROS        D98HCI
766700930325     C                   Z-ADD     *ZEROS        D98DCE
766800930325     C                   Z-ADD     *ZEROS        D98HCE
766900931125     C                   Z-ADD     *ZEROS        D98OCI
767000931125     C                   Z-ADD     *ZEROS        D98OCC
767100931202     C                   Z-ADD     *ZEROS        D98DEI
767200931202     C                   Z-ADD     *ZEROS        D98OEI
767300931202     C                   Z-ADD     *ZEROS        D98DEE
767400931202     C                   Z-ADD     *ZEROS        D98OEE
767500151019     c                   CLEAR                   D98FLP
767600150325     C                   clear                   D98dtctd
767700150326     C                   clear                   D98picklim
767800150325     C                   clear                   D98spcdee
767900150331     C                   clear                   fddata
768000140825     c
768100141017     c                   clear                   d98ddc
768200141017     c                   clear                   d98cons
768300160506     c                   clear                   d98disf
768400141017     c                   clear                   wd98ndc
768500140910     c                   clear                   wd98fbc
768600150119     c                   clear                   wd98acc
768700140825     c                   clear                   wtrulor
768800160401     c                   clear                   wVP               1
768900150219     c
769000150219     c                   clear                   IMPdata
769100150219     c                   clear                   IMPfgs
769200150219     c                   clear                   DISdata
769300150219     c                   clear                   DISfgs
769400150219     c                   clear                   DISora
769500150219     c                   clear                   DISnpg
769600150219     c                   clear                   IDnpg
769700150219     c                   clear                   IDdata
769800150219     c                   clear                   IDora
769900150219     c                   clear                   IDfgs
770000931125     C* CAMPI VARI
770100931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
770200140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
770300140825      *   DALLA LETTURA DI QUESTI FILE                               *
770400931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
770500931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
770600931125     C     D98NSP        IFNE      *ZEROS
770700930525     C                   MOVEL     *BLANKS       D98TSP
770800941228     C                   MOVEL     *BLANKS       D98CAD
770900970728     C                   MOVEL     *BLANKS       D98LOD
771000930525     C                   Z-ADD     *ZEROS        D98TFP
771100930525     C                   Z-ADD     *ZEROS        D98TFA
771200930525     C                   Z-ADD     *ZEROS        D98LNA
771300930525     C                   Z-ADD     *ZEROS        D98DSP
771400941228     C                   Z-ADD     *ZEROS        D98MGS
771500930525     C                   Z-ADD     *ZEROS        D98DTI
771600930525     C                   Z-ADD     *ZEROS        D98HTI
771700930525     C                   Z-ADD     *ZEROS        D98DCM
771800930525     C                   Z-ADD     *ZEROS        D98HMC
771900930525     C                   MOVEL     *BLANKS       D98GC1
772000930525     C                   MOVEL     *BLANKS       D98GC2
772100010820     C                   Z-ADD     *ZEROS        D98CAS
772200010821     C                   MOVEL     *BLANKS       D98VCA
772300930525     C                   MOVEL     *BLANKS       D98TIC
772400930525     C                   Z-ADD     *ZEROS        D98DCR
772500930525     C                   Z-ADD     *ZEROS        D98HCR
772600930525     C                   MOVEL     *BLANKS       D98TCR
772700050914     C                   MOVEL     *BLANKS       Consideraora      1
772800930525     C                   MOVEL     *BLANKS       D98CMC
772900930525     C                   Z-ADD     *ZEROS        D98DAG
773000930525     C                   Z-ADD     *ZEROS        D98DCG
773100930525     C                   MOVEL     *BLANKS       D98FFD
773200930525     C                   Z-ADD     *ZEROS        D98NCD
773300930525     C                   MOVEL     *BLANKS       D98FNS
773400931124     C                   Z-ADD     *ZEROS        D98DAM
773500940628     C                   MOVEL     *BLANKS       D98FIN
773600940628     C                   Z-ADD     *ZEROS        D98KSC
773700940628     C                   MOVEL     *BLANKS       D98TBL
773800931125     C                   END
773900960229     C     D98TLA        IFEQ      'T'
774000960229     C                   Z-ADD     *ZEROS        D98DTI
774100960229     C                   Z-ADD     *ZEROS        D98HTI
774200960229     C                   ENDIF
774300931125     C* CAMPI WORK
774400930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
774500930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
774600150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
774700930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
774800941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
774900960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
775000930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
775100930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
775200940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
775300930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
775400150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
775500931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
775600931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
775700150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
775800930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
775900930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
776000930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
776100930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
776200930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
776300940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
776400941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
776500930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
776600050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
776700050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
776800140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
776900930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
777000930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
777100930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
777200930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
777300930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
777400930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
777500930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
777600930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
777700930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
777800930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
777900930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
778000930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
778100930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
778200930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
778300140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
778400931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
778500060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
778600960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
778700931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
778800140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
778900140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
779000150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
779100140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
779200941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
779300970304     C******************** Z-ADD*ZEROS    W98DBR  80
779400970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
779500150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
779600050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
779700930414     C*
779800941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
779900941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
780000930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
780100930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
780200930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
780300930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
780400930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
780500930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
780600941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
780700941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
780800930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
780900930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
781000930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
781100930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
781200930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
781300930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
781400941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
781500941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
781600930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
781700930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
781800930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
781900930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
782000930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
782100930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
782200930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
782300930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
782400060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
782500060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
782600060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
782700060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
782800930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
782900930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
783000930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
783100930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
783200930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
783300930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
783400930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
783500930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
783600930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
783700930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
783800930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
783900930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
784000930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
784100930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
784200941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
784300941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
784400930423     C*
784500930511     C                   Z-ADD     *ZEROS        W98HF             5 0
784600930423     C                   Z-ADD     *ZEROS        W98HH             5 0
784700060418     C                   Z-ADD     *ZEROS        W98EG             5 0
784800060420     C                   Z-ADD     *ZEROS        W98EA             5 0
784900930423     C                   Z-ADD     *ZEROS        W98HG             5 0
785000930423     C                   Z-ADD     *ZEROS        W98HL             5 0
785100930423     C                   Z-ADD     *ZEROS        W98HO             5 0
785200930511     C                   Z-ADD     *ZEROS        W98HD             5 0
785300930611     C*
785400930611     C                   Z-ADD     *ZEROS        WORELA            3 0
785500960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
785600930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
785700930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
785800931207     C                   Z-ADD     *ZEROS        WORECR            3 0
785900090902     c
786000090902     c                   clear                   wmancaTRAZ
786100150113     c                   clear                   wcaldpcee         1
786200150220     c                   clear                   evemic            2
786300930305     C*
786400060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
786500060406     C                   clear                   LAF                            *LASC.AVVISO FINE
786600091014     C                   clear                   LAp                            *posizione l.avviso
786700060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
786800060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
786900060406c    c                   clear                   LCon
787000060406     C                   clear                   EVD                            *DATA EVENTO
787100060406     C                   clear                   EVO                            *ORA EVENTO
787200060406     C                   clear                   EVC                            *CAUSALE EVENTO
787300131218     C                   clear                   EVT
787400060406     C                   clear                   pVD                            *DATA EVENTO
787500060406     C                   clear                   pVO                            *ORA EVENTO
787600060406     C                   clear                   pVC                            *CAUSALE EVENTO
787700060406     C                   clear                   pVF                            *CAUSALE EVENTO
787800060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
787900060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
788000060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
788100060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
788200060406     C                   clear                   COV                            *DATE FINE L.A. E GI
788300060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
788400060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
788500091014     C                   clear                   GIp                            *posizione vento gia
788600060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
788700060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
788800060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
788900931213     C*
789000060406     C                   clear                   TUC                            *TURNI CHIUSURA
789100931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
789200931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
789300060406     c
789400060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
789500060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
789600060406     c                   clear                   pgii
789700060406     c                   clear                   pgif
789800060406     c                   clear                   pgci
789900060406     c                   clear                   pgcf
790000060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
790100060420     c                   clear                   paii
790200060420     c                   clear                   paif
790300930420     C*
790400930305     C                   ENDSR
790500930512     C***********************************************************************
790600930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
790700930512     C***********************************************************************
790800930512     C     FORCON        BEGSR
790900930512     C*
791000931124     C                   Z-ADD     *ZEROS        D98RPI
791100931124     C                   Z-ADD     *ZEROS        D98RPC
791200930512     C                   Z-ADD     *ZEROS        D98RTI
791300930512     C                   Z-ADD     *ZEROS        D98RTC
791400930512     C                   Z-ADD     *ZEROS        D98RCI
791500930512     C                   Z-ADD     *ZEROS        D98RCC
791600930512     C                   Z-ADD     *ZEROS        D98RRI
791700930512     C                   Z-ADD     *ZEROS        D98RRC
791800931124     C                   Z-ADD     *ZEROS        D98FPI
791900931124     C                   Z-ADD     *ZEROS        D98FPC
792000930512     C                   Z-ADD     *ZEROS        D98FTI
792100930512     C                   Z-ADD     *ZEROS        D98FTC
792200930512     C                   Z-ADD     *ZEROS        D98FCI
792300930512     C                   Z-ADD     *ZEROS        D98FCC
792400930512     C                   Z-ADD     *ZEROS        D98FRI
792500930512     C                   Z-ADD     *ZEROS        D98FRC
792600931124     C                   MOVEL     'S'           D98CPI
792700931124     C                   MOVEL     'S'           D98CPC
792800930512     C                   MOVEL     'S'           D98CTI
792900930512     C                   MOVEL     'S'           D98CTC
793000930512     C                   MOVEL     'S'           D98CCI
793100930512     C                   MOVEL     'S'           D98CCC
793200930512     C                   MOVEL     'S'           D98CRI
793300930512     C                   MOVEL     'S'           D98CRC
793400931124     C                   Z-ADD     *ZEROS        D98NPI
793500931124     C                   Z-ADD     *ZEROS        D98NPC
793600930512     C                   Z-ADD     *ZEROS        D98NTI
793700930512     C                   Z-ADD     *ZEROS        D98NTC
793800930512     C                   Z-ADD     *ZEROS        D98NCI
793900930512     C                   Z-ADD     *ZEROS        D98NCC
794000930512     C                   Z-ADD     *ZEROS        D98NRI
794100930512     C                   Z-ADD     *ZEROS        D98NRC
794200930512     C                   Z-ADD     *ZEROS        D98HHI
794300930512     C                   Z-ADD     *ZEROS        D98HHC
794400930512     C                   Z-ADD     *ZEROS        D98HOI
794500930512     C                   Z-ADD     *ZEROS        D98HOC
794600930512     C                   Z-ADD     *ZEROS        D98HLI
794700930512     C                   Z-ADD     *ZEROS        D98HLC
794800930512     C                   Z-ADD     *ZEROS        D98HGI
794900930512     C                   Z-ADD     *ZEROS        D98HGC
795000931124     C                   Z-ADD     *ZEROS        D98HPI
795100931124     C                   Z-ADD     *ZEROS        D98HPC
795200930512     C                   Z-ADD     *ZEROS        D98HFI
795300930512     C                   Z-ADD     *ZEROS        D98HFC
795400930512     C                   Z-ADD     *ZEROS        D98HEI
795500930512     C                   Z-ADD     *ZEROS        D98HEC
795600930512     C                   Z-ADD     *ZEROS        D98HRI
795700930512     C                   Z-ADD     *ZEROS        D98HRC
795800930512     C                   Z-ADD     *ZEROS        D98HDI
795900930512     C                   Z-ADD     *ZEROS        D98HDC
796000930520     C*
796100930512     C                   ENDSR
796200930305     C********************************************************************
796300971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
796400950406031059090120151181212243273304334365
796500950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
796600950406312831303130313130313031
796700930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
796800971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
796900971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
797000971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
797100971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
797200971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
797300971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
797400971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
797500971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
797600971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
797700971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
797800971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
797900971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
798000971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
798100971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
798200971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
798300971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
798400971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
798500971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
798600971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
798700971127TNSD99R- Bolla inesistente.                                               BOIN20
798800971127TNSD99R-     Cap inesistente.                                             CAPI21
798900971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
799000971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
799100971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
799200971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
799300971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
799400971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
799500971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
799600971127TNSD99R- Contrassegno non trovato.                                        CONT29
799700971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
799800990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
799900990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
800000060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
800100060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
800200971127TNSD99R- 35                                                                   35
800300971127TNSD99R- 36                                                                   36
800400971127TNSD99R- 37                                                                   37
800500971127TNSD99R- 38                                                                   38
800600971127TNSD99R- 39                                                                   39
800700971127TNSD99R- 40                                                                   40
