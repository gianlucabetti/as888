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
084300161107$$$$ C                   movel     gcpfas        D98gfas
084400050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
084500960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
084600960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
084700050405     C* O UGUALE A 35 INVECE DI 40)
084800960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
084900050405     C     GCPFAS        ANDGE     40
085000941227     C                   Z-ADD     GCPDCG        D98DCG
085100950112     C                   Z-ADD     GCPDCG        W98DCG
085200960123     C                   ENDIF
085300960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
085400960729     C     GCPFAS        ANDGE     35
085500070202     C     GCPFAS        ANDne     36
085600960729     C                   Z-ADD     GCPDCG        D98DCG
085700960729     C                   Z-ADD     GCPDCG        W98DCG
085800960729     C                   ENDIF
085900941228     C                   ENDIF
086000941228     C                   ENDIF                                                  <-- 1 ---
086100970911     C*
086200970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
086300970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
086400970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
086500050922     C*****              Z-ADD     D98DTI        W98DTI
086600971210     C                   Z-ADD     D98LNP        F
086700971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
086800970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
086900970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
087000150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
087100150120     c                   if        arbdcp=0 and d98dfc=0
087200150120     C                   Z-ADD     d98dam        D98DTI
087300150120     c                   endif
087400150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
087500150120     C                   Z-ADD     d98dam        D98DTI
087600150120     c                   endif
087700150120     c                   ENDIF
087800150120     c***
087900150120     C***                Z-ADD     D98AAS        ARTAAS
088000150120     C***                Z-ADD     D98LNP        ARTLNP
088100150120     C***                Z-ADD     D98NRS        ARTNRS
088200150120     C***                Z-ADD     D98NSP        ARTNSP
088300150120     C***  KEYART        SETLL     FNART01L                           99
088400150120     C**N99KEYART        READE     FNART01L                               99
088500150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
088600150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
088700970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
088800150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
088900970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
089000970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
089100150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
089200150120     C***  ARTDCM        ANDEQ     *ZEROS
089300970912     C* SE PRIMO REC. TROVATO, MEM. DATA
089400150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
089500150120     C***                Z-ADD     ARTDAM        D98DTI
089600150120     C***                ELSE                                                   <-- 6 -->
089700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
089800150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
089900150120     C***                Z-ADD     ARTDAM        D98DTI
090000150120     C***                ENDIF                                                  <-- 7 ---
090100150120     C***                ENDIF                                                  <-- 6 ---
090200150120     C***                ENDIF                                                  <-- 5 ---
090300970911     C* C'E' DATA CONSEGNAB.
090400150120     C***                ELSE                                                   <-- 4 -->
090500970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
090600970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
090700150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
090800150120     C***  ARTDAM        ANDLE     D98DFC
090900970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
091000970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
091100150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
091200150120     C***  ARTDCM        ORGT      D98DFC
091300970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
091400150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
091500150120     C***                Z-ADD     ARTDAM        D98DTI
091600150120     C***                ELSE                                                   <-- 7 -->
091700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
091800150120     C***  ARTDAM        IFLT      D98DTI
091900150120     C***                Z-ADD     ARTDAM        D98DTI
092000150120     C***                ENDIF
092100150120     C***                ENDIF                                                  <-- 7 ---
092200150120     C***                ENDIF                                                  <-- 6 ---
092300150120     C***                ENDIF                                                  <-- 5 ---
092400150120     C***                ENDIF                                                  <-- 4 ---
092500150120     C***                ENDIF                                                  <-- 3 ---
092600150120     C***  KEYART        READE     FNART01L                               99
092700150120     C***                ENDDO                                                  <-- 2 ---
092800150120     C***                ENDIF                                                  <-- 1 ---
092900941227     C*
093000941227     C                   ENDSR
093100150325     C********************************************************************
093200150325     C*   Verifico se bolla in distinta
093300150325     C********************************************************************
093400150325     c     IN_dist       BEGSR
093500150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
093600150325     c                   if         ARBfbc = *blanks and
093700150325     c                               ARBddc > 0 and
093800150325     c                               ARBndc > 0 and ARBdcm=0
093900150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
094000150325     c                   if        not %open(fiapd01l)
094100150325     c                   open      fiapd01l
094200150325     c                   endif
094300150325     c
094400150325     C                   move      'A'           kpdtip
094500150325     c                   eval      kpdpdr=arbpdc
094600150325     C     kfapd         chain     fiapd01l
094700150325     c
094800150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
094900150325     c                   eval       d98cons = 'S'
095000160506     c                   else
095100160506     c                   eval       d98disf = 'F'
095200150325     c                   endif
095300150325     c                   endif
095400150325     c                   ENDSR
095500941227     C********************************************************************
095600941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
095700941227     C********************************************************************
095800941227     C     LETBLP        BEGSR
095900941227     C*
096000941227     C                   Z-ADD     D98AAS        BLPAAS
096100941227     C                   Z-ADD     D98LNP        BLPLNP
096200941227     C                   Z-ADD     D98NRS        BLPNRS
096300941227     C                   Z-ADD     D98NSP        BLPNSP
096400941227     C     KEYBLP        CHAIN     FNBLP01L                           99
096500941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
096600941227     C                   MOVEL     '1'           D98ERR
096700941227     C                   MOVEL     MSG(20)       D98MSG
096800941227     C                   END                                                    <-- 1 ---
096900941228     C     *IN99         IFEQ      '0'                                                    *TROV.
097000041028     C     KEYBLP        CHAIN     FNarb01L
097100041028     c                   if        %found(fnarb01l)
097200041028     c                   eval      wesarb='S'
097300041028     c                   else
097400041028     c                   eval      wesarb='N'
097500150219     c                   eval      wtrasm='D'
097600041028     c                   endif
097700141216     c
097800141216     c* se non trasmessa la bolla in arrivo memorizzo
097900150219     c                   if        blpft1= ' '
098000141229     c                   eval      wtrasm='N'
098100141216     c                   endif
098200141216     c
098300110923     C                   MOVEL     BLPxco        D98xco
098400110923     C                   MOVEL     BLPTSP        D98TSP
098500130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
098600130529     c                   if        i98tsp_for<>' '
098700130529     C                   MOVEL     i98tsp_for    D98TSP
098800130529     c                   endif
098900960209     C                   MOVEL     BLPNZD        D98NZD
099000941228     C                   MOVEL     BLPCAD        D98CAD
099100970701     C                   MOVEL     BLPLOD        D98LOD
099200941227     C                   Z-ADD     BLPTFP        D98TFP
099300941227     C                   Z-ADD     BLPTFA        D98TFA
099400941227     C                   Z-ADD     BLPLNA        D98LNA
099500941227     C                   Z-ADD     BLPAAS        DSAAS
099600941227     C                   Z-ADD     BLPMGS        DSMGS
099700941227     C                   Z-ADD     DSDAT8        D98DSP
099800941227     C                   Z-ADD     BLPMGS        D98MGS
099900051124     C                   movel     BLPTC1        D98TC1
100000051124     C                   movel     BLPTC2        D98TC2
100100041028     c                   if        wesarb='S'
100200041028     C                   Z-ADD     arbDTI        D98DTI
100300041028     C                   Z-ADD     arbHTI        D98HTI
100400041028     C                   Z-ADD     arbDCM        D98DCM
100500041028     C                   Z-ADD     arbHMC        D98HMC
100600041028     C                   Z-ADD     arbDAM        D98DAM
100700050901     C                   MOVEL     arbcca        d98cca
100800140820     C                   MOVEL     arbddc        d98ddc
100900140911     C                   z-add     arbndc        wd98ndc
101000140910     C                   MOVEL     arbfbc        wd98fbc
101100150119     c                   movel     arbacc        wd98acc
101200150325     c                   exsr      IN_DIST
101300140820     c*
101400041028     c                   else
101500941227     C                   Z-ADD     BLPDTI        D98DTI
101600941227     C                   Z-ADD     BLPHTI        D98HTI
101700941227     C                   Z-ADD     BLPDCM        D98DCM
101800941227     C                   Z-ADD     BLPHMC        D98HMC
101900041028     C                   Z-ADD     BLPDAM        D98DAM
102000050901     C                   MOVEL     BLPcca        d98cca
102100140820     C                   MOVEL     BLPddc        d98ddc
102200140825     C                   MOVEL     BLPndc        wd98ndc
102300140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
102400140820     c                   clear                   d98cons
102500041028     c                   endif
102600941227     C                   MOVEL     BLPGC1        D98GC1
102700941227     C                   MOVEL     BLPGC2        D98GC2
102800941227     C                   Z-ADD     BLPDCR        D98DCR
102900941227     C                   Z-ADD     BLPHCR        D98HCR
103000941227     C                   MOVEL     BLPTCR        D98TCR
103100100308     c* Salvo i campi perchè in alcuni casi li clearo
103200100308     C                   Z-ADD     blpDCR        s98DCR
103300100308     C                   Z-ADD     blpHCR        s98HCR
103400100308     C                   MOVEL     blpTCR        s98TCR
103500100308     c
103600941227     C                   MOVEL     *BLANKS       D98CMC
103700941227     C                   MOVEL     BLPFFD        D98FFD
103800941227     C                   Z-ADD     BLPNCD        D98NCD
103900941227     C                   MOVEL     BLPFNS        D98FNS
104000941228     C                   Z-ADD     BLPDUC        D98DUC
104100150508     C                   Z-ADD     BLPDPC        w98DPC
104200970310     C                   Z-ADD     *ZEROS        W98HUC
104300071122     c                   movel     blpcbo        d98cbo
104400150331     c*
104500150331     c* Se in fermo deposito vedo se presente particlarità consegna
104600150331     c                   if        blpffd='S' and blpgma<>*blanks
104700150331     C                   Z-ADD     CODUT         KTBUTE
104800150331     C                   MOVEL     '7R'          KTBCOD
104900150331     C                   MOVEL(P)  blpgma        KTBKEY
105000150331     C     KEYTAB        chain     TABEL00F
105100150331     c                   if        %found(tabel00f)
105200150331     c                   movel     tbluni        ds7r
105300150331     c                   endif
105400150331     c                   endif
105500150331     c
105600970304     C                   Z-ADD     BLPDBR        D98DBR
105700070514     c                   eval      d98fdrt=' '
105800070514     c
105900141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
106000070514     C                   Z-ADD     blpDrt        D98DBR
106100070514     c                   eval      d98fdrt='S'
106200141215     c********           endif
106300070514     c*
106400941227     C                   MOVEL     BLPFIN        D98FIN
106500130826     C****               Z-ADD     BLPKSC        D98KSC
106600990308     C                   MOVEL     BLPFBR        WFBR
106700031201     C                   MOVEL     BLPGGA        WGGA
106800941227     C                   Z-ADD     1             I
106900941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
107000941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
107100941227     C                   MOVEL     '1'           D98ERR
107200941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
107300941227     C                   ELSE                                                   <-- 2 -->
107400941227     C                   MOVEL     T3A(I)        D98TBL
107500941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
107600990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
107700941227     C                   END                                                    <-- 2 ---
107800130826     c                   if        %subst(d98tbl:1:1) ='F'
107900130826     C                   Z-ADD     blpKSC        D98KSC
108000130826     c                   else
108100130826     c
108200130826     c                   if        blpccm>0
108300130826     C                   Z-ADD     blpccm        D98KSC
108400130826     c                   endif
108500130826     c                   if        d98ksc=0
108600130826     c                   z-add     8888          d98ksc
108700130826     c                   movel     d98lnp        d98ksc
108800130826     c                   endif
108900130826     c                   endif
109000130826     c
109100941227     C                   END
109200941228     C*
109300051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
109400950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
109500941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
109600941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
109700051115     C                   Z-ADD     D98AAS        ar9AAS
109800051115     C                   Z-ADD     D98LNP        ar9LNP
109900051115     C                   Z-ADD     D98NRS        ar9NRS
110000051115     C                   Z-ADD     D98NSP        ar9NSP
110100051115     C     KEYar9        CHAIN     Fiar901L                           99
110200950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
110300950102     C                   MOVEL     'S'           WFLASS
110400950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
110500950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
110600950102     C                   Z-ADD     D98AAS        LBLAAN
110700950102     C                   Z-ADD     D98LNP        LBLLPN
110800950102     C                   Z-ADD     D98NRS        LBLNRN
110900950102     C                   Z-ADD     D98NSP        LBLNSN
111000950102     C     KEYLBL        CHAIN     FNLBL01L                           99
111100950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
111200051115     C                   Z-ADD     LBLAAP        ar9AAS
111300051115     C                   Z-ADD     LBLLPP        ar9LNP
111400051115     C                   Z-ADD     LBLNRP        ar9NRS
111500051115     C                   Z-ADD     LBLNSP        ar9NSP
111600051115     C     KEYar9        CHAIN     Fiar901L                           99
111700950102     C     *IN99         IFEQ      '0'
111800950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
111900950102     C                   ENDIF
112000950102     C                   ENDIF                                                  <-- 3 ---
112100950102     C                   ENDIF                                                  <-- 2 ---
112200950102     C*
112300950102     C* CONTRASSEGNO TROVATO
112400150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
112500051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
112600051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
112700051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
112800941228     C                   ENDIF                                                  <-- 2 ---
112900941228     C                   ENDIF                                                  <-- 1 ---
113000941227     C*
113100050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
113200941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
113300050405     C                   Z-ADD     D98AAS        KWGCPAAS
113400050405     C                   Z-ADD     D98LNP        KWGCPLNP
113500050405     C                   Z-ADD     D98NRS        KWGCPNRS
113600050405     C                   Z-ADD     D98NSP        KWGCPNSP
113700050405     C                   Z-ADD     *ZEROS        KWGCPFRG
113800050405     C     KEYGCP        CHAIN     TIGCP01L                           99
113900941228     C     *IN99         IFEQ      '0'                                          *TROV.
114000941227     C     GCPATB        ANDNE     'A'
114100960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
114200960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
114300960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
114400960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
114500161107$$$$ C                   movel     gcpfas        D98Gfas
114600050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
114700960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
114800960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
114900050405     C* O UGUALE A 35 INVECE DI 40)
115000960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
115100050405     C     GCPFAS        ANDGE     40
115200941227     C                   Z-ADD     GCPDCG        D98DCG
115300950112     C                   Z-ADD     GCPDCG        W98DCG
115400960123     C                   ENDIF
115500960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
115600960729     C     GCPFAS        ANDGE     35
115700070202     C     GCPFAS        ANDne     36
115800960729     C                   Z-ADD     GCPDCG        D98DCG
115900960729     C                   Z-ADD     GCPDCG        W98DCG
116000960729     C                   ENDIF
116100941228     C                   ENDIF
116200941228     C                   ENDIF                                                  <-- 1 ---
116300970912     C*
116400970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
116500970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
116600970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
116700050922     C******             Z-ADD     D98DTI        W98DTI
116800971210     C                   Z-ADD     D98LNP        F
116900971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
117000970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
117100970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
117200150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
117300150120     c                   if        blpdcp=0 and d98dfc=0
117400150120     C                   Z-ADD     d98dam        D98DTI
117500150120     c                   endif
117600150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
117700150120     C                   Z-ADD     d98dam        D98DTI
117800150120     c                   endif
117900150120     c                   ENDIF
118000150120     c*
118100150120     C***                Z-ADD     D98AAS        BLTAAS
118200150120     C****               Z-ADD     D98LNP        BLTLNP
118300150120     C***                Z-ADD     D98NRS        BLTNRS
118400150120     C***                Z-ADD     D98NSP        BLTNSP
118500150120     C***  KEYBLT        SETLL     FNBLT01L                           99
118600150120     C**N99KEYBLT        READE     FNBLT01L                               99
118700150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
118800150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
118900970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
119000150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
119100970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
119200970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
119300150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
119400150120     C**   BLTDCM        ANDEQ     *ZEROS
119500970912     C* SE PRIMO REC. TROVATO, MEM. DATA
119600150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
119700150120     C**                 Z-ADD     BLTDAM        D98DTI
119800150120     C**                 ELSE                                                   <-- 6 -->
119900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
120000150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
120100150120     C**                 Z-ADD     BLTDAM        D98DTI
120200150120     C**                 ENDIF                                                  <-- 7 ---
120300150120     C**                 ENDIF                                                  <-- 6 ---
120400150120     C**                 ENDIF                                                  <-- 5 ---
120500970912     C* C'E' DATA CONSEGNAB.
120600150120     **                  ELSE                                                   <-- 4 -->
120700970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
120800970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
120900150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
121000150120     C**   BLTDAM        ANDLT     D98DFC
121100970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
121200970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
121300150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
121400150120     C**   BLTDCM        ORGT      D98DFC
121500970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
121600150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
121700150120     C**                 Z-ADD     BLTDAM        D98DTI
121800150120     C**                 ELSE                                                   <-- 7 -->
121900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
122000150120     C**   BLTDAM        IFLT      D98DTI
122100150120     C**                 Z-ADD     BLTDAM        D98DTI
122200150120     C**                 ENDIF
122300150120     C**                 ENDIF                                                  <-- 7 ---
122400150120     C**                 ENDIF                                                  <-- 6 ---
122500150120     C**                 ENDIF                                                  <-- 5 ---
122600150120     C**                 ENDIF                                                  <-- 4 ---
122700150120     C**                 ENDIF                                                  <-- 3 ---
122800150120     C**   KEYBLT        READE     FNBLT01L                               99
122900150120     C**                 ENDDO                                                  <-- 2 ---
123000150120     C**                 ENDIF                                                  <-- 1 ---
123100941227     C*
123200941227     C                   ENDSR
123300930323     C********************************************************************
123400941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
123500930323     C********************************************************************
123600930323     C     RTTEOR        BEGSR
123700941227     C*
123800941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
123900941228     C     D98ERR        IFEQ      '0'
124000941227     C                   EXSR      RTPART
124100941227     C                   ENDIF
124200941227     C*
124300941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
124400941228     C     D98ERR        IFEQ      '0'
124500150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
124600150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
124700150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
124800150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
124900150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
125000941227     C                   EXSR      RTTRAZ
125100150219     c                   if        d98err<>'1'
125200150219     C                   Z-ADD     CASTIN        D98TTI
125300150219     C                   Z-ADD     CASTCN        D98TTC
125400150219     C                   endif
125500150219     C                   ENDIF
125600941227     C*
125700941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
125800941228     C     D98ERR        IFEQ      '0'
125900941227     C                   EXSR      RTCONS
126000941227     C                   ENDIF
126100941227     C*
126200941228     C     D98ERR        IFEQ      '0'
126300941227     C*
126400941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
126500941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
126600941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
126700090921     c                   z-add     d98tri        w99tri
126800090921     c* per la DS
126900090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
127000090921     c* per i calcoli interi
127100090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
127200090921     c
127300941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
127400941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
127500941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
127600090921     c                   z-add     d98trc        w99trc
127700090921     c* per la DS
127800090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
127900090921     c* per i calcoli interi
128000090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
128100941227     C*
128200941227     C                   ENDIF
128300930323     C*
128400930323     C                   ENDSR
128500941227     C********************************************************************
128600941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
128700941227     C********************************************************************
128800941227     C     RTPART        BEGSR
128900941228     C*
129000941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
129100941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
129200941228     C                   MOVEL     D98TSP        WTSP              1
129300130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
129400130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
129500050301     C*******
129600050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
129700050301     C*
129800050301     C*    W001          IFEQ      'A'
129900050301     C*    W0040         OREQ      8888
130000050301     C*                  MOVEL     'E'           WTSP
130100050301     C*                  ENDIF
130200050301     C*******
130300941227     C*
130400941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
130500941227     C                   MOVEL     'N'           WTRAOK            1
130600941227     C                   MOVEL     'P'           CASPTA
130700941227     C                   Z-ADD     D98LNP        CASLNP
130800941227     C                   Z-ADD     *ZEROS        CASLNA
130900941228     C                   MOVEL     WTSP          CASTSP
131000941228     C                   Z-ADD     99999999      CASDDE
131100941227     C     KEYCAS        SETGT     AZCAS01L                             99
131200941227     C  N99              READP     AZCAS01L                               99
131300941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
131400941227     C     WTRAOK        ANDEQ     'N'
131500941227     C     CASLNP        ANDEQ     D98LNP
131600950102     C     CASTSP        ANDEQ     WTSP
131700941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
131800941227     C     CASATB        ANDEQ     *BLANKS
131900941227     C                   MOVEL     'S'           WTRAOK
132000941227     C                   ELSE                                                   <-- 2 -->
132100941227     C                   READP     AZCAS01L                               99
132200941227     C                   ENDIF                                                  <-- 2 ---
132300941227     C                   ENDDO                                                  <-- 1 ---
132400941227     C*
132500941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
132600941227     C     WTRAOK        IFEQ      'N'
132700941227     C                   MOVEL     '1'           D98ERR
132800941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
132900941227     C                   ELSE
133000941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
133100941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
133200941227     C                   END
133300970214     C*
133400140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
133500140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
133600140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
133700140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
133800140403     C**                 Z-ADD     D98LNA        F
133900140403     C**   D98ERR        IFEQ      '0'
134000140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
134100140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
134200140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
134300140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
134400140403     C**                 ENDIF
134500140403     C**                 ENDIF
134600140402     c
134700140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
134800140402     c*  se tempi  = 0
134900140402     c                   if        wpick2='S' and d98tpi<=0
135000140402     c                   add       24            D98TPI
135100140402     c                   endif
135200941227     C*
135300941227     C                   ENDSR
135400941227     C********************************************************************
135500941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
135600941227     C********************************************************************
135700941227     C     RTTRAZ        BEGSR
135800941227     C*
135900941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
136000150219     c
136100150219     C                   Z-ADD     D98LNA        LTA(1)
136200150219     C                   Z-ADD     D98LNA        LTA(2)
136300150219     C                   Z-ADD     D98TFA        LTA(3)
136400941227     C                   Z-ADD     D98TFA        LTA(4)
136500941227     C*
136600941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
136700941227     C                   MOVEL     'N'           WTRAOK            1
136800941227     C                   Z-ADD     1             K                 3 0
136900941227     C*
137000941227     C* CICLO FINO A:
137100941227     C*    - FINE SCHIERA
137200941227     C*    - TROVATO TEMPI VALIDI
137300941227     C     K             DOUGT     4                                            --- 1 -->
137400941227     C     WTRAOK        OREQ      'S'
137500941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
137600941227     C     LTA(K)        ORNE      *ZEROS
137700941227     C                   MOVEL     'T'           CASPTA
137800941227     C                   Z-ADD     LTP(K)        CASLNP
137900941227     C                   Z-ADD     LTA(K)        CASLNA
138000941227     C                   MOVEL     D98TSP        CASTSP
138100941228     C                   Z-ADD     99999999      CASDDE
138200941227     C     KEYCAS        SETGT     AZCAS01L                             99
138300941227     C  N99              READP     AZCAS01L                               99
138400941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
138500941227     C     WTRAOK        ANDEQ     'N'
138600941227     C     LTP(K)        ANDEQ     CASLNP
138700941227     C     LTA(K)        ANDEQ     CASLNA
138800941227     C     D98TSP        ANDEQ     CASTSP
138900960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
139000941227     C     CASATB        ANDEQ     *BLANKS
139100941227     C                   MOVEL     'S'           WTRAOK
139200941227     C                   ELSE                                                   <-- 4 -->
139300941227     C                   READP     AZCAS01L                               99
139400941227     C                   ENDIF                                                  <-- 4 ---
139500941227     C                   ENDDO                                                  <-- 3 ---
139600941227     C                   ENDIF                                                  <-- 2 ---
139700941227     C                   ADD       1             K
139800941227     C                   ENDDO                                                  <-- 1 ---
139900941227     C*
140000941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
140100941227     C     WTRAOK        IFEQ      'N'
140200941227     C                   MOVEL     '1'           D98ERR
140300941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
140400150219     C***                ELSE
140500150219     C***                Z-ADD     CASTIN        D98TTI
140600150219     C***                Z-ADD     CASTCN        D98TTC
140700941227     C                   END
140800941227     C*
140900941227     C                   ENDSR
141000941227     C********************************************************************
141100941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
141200941227     C********************************************************************
141300941227     C     RTCONS        BEGSR
141400150629     c                   clear                   WTsogliaE         1
141500970701     C*
141600970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
141700970701     C                   CLEAR                   SI95
141800970701     C                   MOVEL     '3'           I95TCN
141900970701     C                   MOVEL     D98NZD        I95NAR
142000970701     C                   MOVEL     D98CAD        I95CAP
142100970701     C                   MOVEL     D98LOD        I95LOC
142200970701     C                   MOVEL     D98DSP        I95DAT
142300970701     C                   MOVEL     D98TSP        I95TSP
142400980602     C                   MOVEL     'S'           I95FRT
142500150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
142600150511     c                   if        d98ffd='S'
142700150511     c                   eval      i95tsp='C'
142800150511     c                   endif
142900020226     C*
143000020226     C* VERIFICO SE LNA E' FILIALE FEDEX
143100020226     C                   IF        D98LNA > *ZEROS
143200020226     C     D98LNA        LOOKUP    FILFED                                 55
143300020226     C                   IF        *IN55 = *ON
143400020226     C                   EVAL      I95FI1 = 'S'
143500020226     C                   ENDIF
143600020226     C                   ENDIF
143700020226     C*
143800020226     C                   CALL      'TISI95R'
143900970701     C                   PARM                    SI95
144000941227     C*
144100941228     C* SE NON TROVATO, ERRORE
144200970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
144300941227     C                   MOVEL     '1'           D98ERR
144400970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
144500941227     C                   ELSE                                                   <-- 1 -->
144600941227     C*
144700941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
144800941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
144900941227     C     CASTIN        DIV       24            WDIV              3 0
145000941227     C                   MVR                     WRES              3 0
145100941227     C     WRES          IFEQ      *ZEROS
145200970701     C                   Z-ADD     O950IN        D98TCI
145300941227     C                   ELSE
145400970701     C                   Z-ADD     O951IN        D98TCI
145500941227     C                   END
145600941227     C*
145700941227     C     CASTCN        DIV       24            WDIV                     99
145800941227     C                   MVR                     WRES
145900941227     C     WRES          IFEQ      *ZEROS
146000970701     C                   Z-ADD     O950CN        D98TCC
146100150629     c                   eval      wTSogliaE='0'
146200941227     C                   ELSE
146300970701     C                   Z-ADD     O951CN        D98TCC
146400150629     c                   eval      wTSogliaE='1'
146500941227     C                   END
146600941227     C*
146700150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
146800941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
146900150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
147000150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
147100150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
147200150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
147300150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
147400941227     C*
147500941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
147600941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
147700970701     C     O95ISO        IFEQ      'I'
147800941227     C     D98FIN        OREQ      'I'
147900970701     C                   ADD       O95TIS        D98TCI
148000970701     C                   ADD       O95TIS        D98TCC
148100941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
148200970701     C                   ADD       O95TIS        W990IN
148300970701     C                   ADD       O95TIS        W991IN
148400150629     C                   ADD       O95TIS        W990CN
148500150629     C                   ADD       O95TIS        W991CN
148600941227     C                   END
148700090827     c
148800090827     c* Sistemo i tempi di consegna se non sono multipli di 12
148900090827     c                   exsr      SisCONS
149000160404     c
149100160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
149200160404     c                   if        wVP='S'
149300160404     c                   add       24            w991IN
149400160404     c                   add       24            w991CN
149500160404     c                   add       24            w990IN
149600160404     c                   add       24            w990CN
149700160404     c                   endif
149800941228     C*
149900941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
150000941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
150100941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
150200941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
150300941228     C                   Z-ADD     D98TCC        W99CCE            3 0
150400941228     C                   ELSE                                                   <-- 3 -->
150500970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
150600970701     C                   CLEAR                   SI95
150700970701     C                   MOVEL     '3'           I95TCN
150800970701     C                   MOVEL     D98NZD        I95NAR
150900970701     C                   MOVEL     D98CAD        I95CAP
151000970701     C                   MOVEL     D98LOD        I95LOC
151100970701     C                   MOVEL     D98DSP        I95DAT
151200970701     C                   MOVEL     'E'           I95TSP
151300980602     C                   MOVEL     'S'           I95FRT
151400020226     C*
151500020226     C* VERIFICO SE LNA E' FILIALE FEDEX
151600020226     C                   IF        D98LNA > *ZEROS
151700020226     C     D98LNA        LOOKUP    FILFED                                 55
151800020226     C                   IF        *IN55 = *ON
151900020226     C                   EVAL      I95FI1 = 'S'
152000020226     C                   ENDIF
152100020226     C                   ENDIF
152200020226     C*
152300020226     C                   CALL      'TISI95R'
152400970701     C                   PARM                    SI95
152500970701     C*
152600941228     C* SE NON TROVATO, ERRORE
152700970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
152800941228     C                   MOVEL     '1'           D98ERR
152900970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
153000941228     C                   ELSE                                                   <-- 4 -->
153100941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
153200941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
153300941228     C     CASTCN        DIV       24            WDIV                     99
153400941228     C                   MVR                     WRES
153500941228     C     WRES          IFEQ      *ZEROS
153600970701     C                   Z-ADD     O950CN        W99CCE
153700941228     C                   ELSE
153800970701     C                   Z-ADD     O951CN        W99CCE
153900941228     C                   END
154000941228     C                   ENDIF                                                  <-- 4 ---
154100941228     C                   ENDIF                                                  <-- 3 ---
154200941228     C                   ENDIF                                                  <-- 2 ---
154300941227     C*
154400941227     C                   END                                                    <-- 1 ---
154500941227     C*
154600941227     C                   ENDSR
154700090827     C********************************************************************
154800090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
154900090827     C********************************************************************
155000090827     C     SisCONS       BEGSR
155100090827     c                   eval      w99tci=d98tci
155200090827     c                   if        d98tci=10
155300090827     c                   z-add     12            w99tci
155400090827     c                   endif
155500090827     c
155600090827     c                   eval      w99tcc=d98tcc
155700090827     c                   if        d98tcc=10
155800090827     c                   z-add     12            w99tcc
155900090827     c                   endif
156000160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
156100160404     c                   if        wVP='S'
156200160404     c                   add       24            w99tcc
156300160404     c                   add       24            w99tci
156400160404     c                   endif
156500090827     c
156600090827     C                   ENDSR
156700930420     C********************************************************************
156800930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
156900930420     C********************************************************************
157000930420     C     CAREVB        BEGSR
157100930420     C*
157200930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
157300930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
157400060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
157500930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
157600930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
157700060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
157800060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
157900930420     C                   Z-ADD     D98AAS        KEBAAS
158000930420     C                   Z-ADD     D98LNP        KEBLNP
158100930420     C                   Z-ADD     D98NRS        KEBNRS
158200930420     C                   Z-ADD     D98NSP        KEBNSP
158300990810     C     KEYEVB        SETLL     FNEVB01L                           99
158400060406     C  N99keyevb        READe     FNEVB01L                               99
158500060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
158600930421     C     D98ERR        ANDEQ     '0'
158700060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
158800930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
158900930421     C                   Z-ADD     1             I
159000930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
159100060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
159200930421     C                   MOVEL     '1'           D98ERR
159300930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
159400060406   x3C                   ELSE                                                   <-- 3 -->
159500060406     C                   MOVEL     D2A(I)        usa2A
159600060406     C                   MOVEL     D2Z(I)        usa2Z
159700930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
159800060406    4C     §2AFTC        IFEQ      'A'
159900930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
160000060406    4C                   END
160100060406    4C     §2AFTC        IFEQ      'G'
160200930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
160300060406    4C                   END
160400150216     c* Se si tratta di MIC verifico se uguale alla sped.
160500150303    4c                   if        evbcev='MIC' or evbcev='NIC'
160600150303    5c                   if        %subst(evbnot:1:7) >*zeros
160700150303     c
160800150303    6c                   if        evbcev='MIC' and
160900150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
161000150216     c                   eval      evemic='SI'
161100150303    6c                   endif
161200150303     c
161300150303    6c                   if        evbcev='NIC' and
161400150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
161500150216     c                   eval      evemic='  '
161600150303    6c                   endif
161700150303     c
161800150303   x5c                   else
161900150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
162000150303     c                   eval      evemic='SI'
162100150303    6c                   endif
162200150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
162300150303     c                   eval      evemic='  '
162400150303    6c                   endif
162500150303
162600150303    5c                   endif
162700150303    4c                   endif
162800150216     c
162900060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
163000930421     C     §2AFTC        OREQ      'G'
163100930421     C                   ADD       1             IE
163200990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
163300930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
163400930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
163500131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
163600090928     c
163700090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
163800091014     c**                 if        ie=1
163900091014     c**                 eval      Primotipo=§2aftc
164000091014     c**                 eval      Primodev=evbdev
164100091014     c**                 eval      Primohev=evbhev
164200091014     c**                 endif
164300090928     c
164400060406    4C                   END                                                    <-- 4 ---
164500060406     c* Verifico se si tratta di evento particolare
164600060406    4c                   if        §2zCEP<>' '
164700060526     c* non carico l'evento se cade in giorno festivo
164800060526     c                   z-add     evbdev        wpodata
164900060526     C                   EVAL      WTC='C'
165000060526     C                   EVAL      WIC='C'
165100060526     c                   eval      wslitt='N'
165200060526     c                   exsr      chkfes
165300060526     c
165400060526     c* Wslitt='N' non festivo
165500060526   4ac                   if        wslitt='N'
165600060526     c
165700060406    5c                   if        §2zfep='A'
165800060406     c                   eval      wevpaA='S'
165900060406    5C                   END                                                    <-- 3 ---
166000060406    5c                   if        §2zfep='G'
166100060406     c                   eval      wevpaG='S'
166200060406    5C                   END                                                    <-- 3 ---
166300060406     c                   add       1             ip
166400060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
166500060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
166600060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
166700060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
166800060526   4aC                   ENDif                                                  <-- 3 ---
166900060526    4C                   END                                                    <-- 3 ---
167000060406    3C                   END                                                    <-- 3 ---
167100060406    2C                   END                                                    <-- 2 ---
167200060406     C     keyevb        READe     FNEVB01L                               99
167300060406    1C                   ENDDO                                                  <-- 1 ---
167400050523     c* controllo riempimento skiere
167500050523     c                   clear                   trul0sds
167600050523     c                   eval      i0sski='EVD/EVO/EVC'
167700050523     c                   eval      i0sele=%elem(EVD)
167800050523     c                   eval      i0spie=ie
167900050523     c                   eval      i0sfile='FNEVB01L  '
168000050523     c                   eval      i0scod='     '
168100050523     c                   eval      i0spgm='TNSD99R'
168200050523     c                   movel     trul0sds      kpjbu
168300050523     c                   call      'TRUL0SR'
168400050523     c                   parm                    kpjba
168500930420     C*
168600060406     c* controllo riempimento skiere
168700060406     c                   clear                   trul0sds
168800060406     c                   eval      i0sski='PVD/PVO/PVC'
168900060406     c                   eval      i0sele=%elem(PVD)
169000060406     c                   eval      i0spie=ie
169100060406     c                   eval      i0sfile='FNEVB01L  '
169200060406     c                   eval      i0scod='     '
169300060406     c                   eval      i0spgm='TNSD99R'
169400060406     c                   movel     trul0sds      kpjbu
169500060406     c                   call      'TRUL0SR'
169600060406     c                   parm                    kpjba
169700060406     C*
169800930420     C                   ENDSR
169900931213     C********************************************************************
170000931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
170100931213     C********************************************************************
170200931213     C     CARTUC        BEGSR
170300931213     C*
170400931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
170500931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
170600931214     C     D98GC2        ANDEQ     *BLANKS
170700931213     C                   ELSE                                                   <-- 1 -->
170800931213     C*
170900931213     C                   MOVE      D98GC1        TUC(1)
171000931214     C                   MOVE      D98GC2        TUC(2)
171100931213     C*
171200931213     C                   END                                                    <-- 1 ---
171300931213     C*
171400931213     C                   ENDSR
171500970304     C********************************************************************
171600970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
171700970304     C********************************************************************
171800970304     C     DATRIF        BEGSR
171900970304     C*
172000050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
172100050922     c*  partenza
172200970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
172300970304     C*
172400970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
172500070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
172600070605     C***                MOVEL     '1'           D98ERR
172700070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
172800070605     C***                ELSE                                                   <-- 2 -->
172900970304     C*
173000970304     C                   Z-ADD     D98DRF        WD1
173100970304     C                   Z-ADD     D98ORF        WO1
173200970310     C* DATA PARTENZA ULTIMO COLLO
173300970310     C     D98DRF        IFLE      D98DUC
173400050928     c     d98duc        oreq      0
173500970310     C                   Z-ADD     D98DRF        D98DUC
173600970310     C                   Z-ADD     D98ORF        W98HUC
173700070605     C                   ENDIF
173800970304     C* DATA ARRIVO TRAZIONE
173900050922     C***                Z-ADD     D98DTI        WD2
174000050922     C***                Z-ADD     D98HTI        WO2
174100050922     C***  WDO1          IFLE      WDO2
174200050922     C***                Z-ADD     D98DRF        D98DTI
174300050922     C***                Z-ADD     D98ORF        D98HTI
174400050922     C***                ENDIF
174500970304     C* DATA CONSEGNA
174600050922     C***                Z-ADD     D98DCM        WD2
174700050922     C***                Z-ADD     D98HMC        WO2
174800050922     C***  WDO1          IFLE      WDO2
174900050922     C***                Z-ADD     D98DRF        D98DCM
175000050922     C***                Z-ADD     D98ORF        D98HMC
175100050922     C***                ENDIF
175200970304     C*
175300070605     C***                ENDIF                                                  <-- 2 ---
175400970304     C                   ENDIF                                                  <-- 1 ---
175500970304     C*
175600970304     C                   ENDSR
175700940901     C********************************************************************
175800940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
175900940901     C********************************************************************
176000940901     C     TDTIPO        BEGSR
176100940901     C*
176200940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
176300940902     C                   Z-ADD     D98TTI        W98TTI            3 0
176400941011     C*
176500941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
176600941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
176700941011     C     W98TTI        DIV       24            WDIV              3 0
176800941011     C                   MVR                     WRES              3 0
176900941011     C     WRES          IFEQ      *ZEROS
177000941011     C                   Z-ADD     *ZEROS        WASTA             9 0
177100941011     C                   ELSE
177200941011     C                   Z-ADD     12            WASTA
177300941011     C                   END
177400940901     C*
177500940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
177600940901     C     D98DTI        IFNE      *ZEROS
177700940901     C                   Z-ADD     D98DTI        WDAT
177800940901     C                   EXSR      DATORE
177900960911     C                   Z-ADD     WORE          WPODTI
178000960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
178100960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
178200960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
178300960911     C                   Z-ADD     WPODTI        W10POI            9 0
178400960911     C                   EXSR      R10FES
178500960911     C                   Z-ADD     W10POO        WPODTI
178600960911     C* AGGIUNGO LE ORA D'ASTA
178700960911     C                   ADD       WASTA         WPODTI
178800960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
178900960911     C     W10POI        IFEQ      W10POO
179000940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
179100940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
179200940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
179300940901     C                   EXSR      RO3ORE
179400940901     C                   ADD       WR3HHO        WPODTI
179500940901     C                   ADD       WR3HHO        W98TTI
179600940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
179700940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
179800940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
179900940901     C                   EXSR      RO3ORE
180000940901     C                   ADD       WR3HHO        WPODTI
180100940901     C                   ADD       WR3HHO        W98TTI
180200960911     C                   ENDIF                                                  *SE NON FESTIVO
180300940901     C                   END
180400940901     C*
180500940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
180600940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
180700940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
180800940901     C     W98TTI        DIV       24            WDIV              3 0
180900940901     C                   MVR                     WRES              3 0
181000940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
181100941228     C                   Z-ADD     W990IN        D98TCI
181200160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
181300160405     c*  per cui le sottraggo, per riagguingerle alla fine
181400160405     c                   if        wVP='S'
181500160405     c                   sub       24            D98TCI
181600160405     c                   endif
181700160405     c
181800150630     c                   eval      wTSogliaE='0'
181900940901     C                   ELSE                                                   <-- 2 -->
182000941228     C                   Z-ADD     W991IN        D98TCI
182100160405     c                   if        wVP='S'
182200160405     c                   sub       24            D98TCI
182300160405     c                   endif
182400150630     c                   eval      wTSogliaE='1'
182500940908     C                   ENDIF                                                  <-- 2 ---
182600090827     c* Sistemo i tempi di consegna se non sono multipli di 12
182700090827     c                   exsr      SisCONS
182800090827     c*
182900940908     C* RICALCOLO TEMPO DI RESA INTERNO
183000950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
183100950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
183200090921     c                   z-add     d98tri        w99tri
183300090921     c* per la DS
183400090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
183500090921     c* per i calcoli interni
183600090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
183700940908     C                   ENDIF                                                  <-- 1 ---
183800940901     C*
183900940901     C                   ENDSR
184000930325     C********************************************************************
184100930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
184200930325     C********************************************************************
184300930325     C     TDATPO        BEGSR
184400930406     C*
184500930420     C* TRASFORMO LA DATA ODIERNA
184600930416     C                   Z-ADD     WDAODI        WDAT
184700930416     C                   EXSR      DATORE
184800050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
184900960911     C                   MOVEL     'OSC  '       WR3TAB
185000960911     C                   Z-ADD     WHHODI        WR3HHI
185100960911     C                   EXSR      RO3ORE
185200050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
185300050922     c                   if        wr3hho=12
185400050922     c     wpoatt        add       12            wpoattdata24
185500050922     c                   else
185600050922     c                   z-add     wpoatt        wpoattdata24
185700050922     c                   endif
185800930325     C*
185900930420     C* TRASFORMO LA DATA DI SPEDIZIONE
186000931125     C     D98DSP        IFEQ      *ZEROS
186100930326     C                   MOVEL     '1'           D98ERR
186200930326     C                   MOVEL     MSG(2)        D98MSG
186300930326     C                   ELSE
186400931125     C                   Z-ADD     D98DSP        WDAT
186500930325     C                   EXSR      DATORE
186600930406     C                   Z-ADD     WORE          WPODSP
186700930326     C                   END
186800930414     C*
186900930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
187000930414     C                   MOVEL     'LIM  '       WR3TAB
187100930414     C                   Z-ADD     *ZEROS        WR3HHI
187200930414     C                   EXSR      RO3ORE
187300931222     C     WR3HHO        MULT      10            WTM060            6 0
187400931222     C     WPODSP        ADD       WTM060        WPOLIM
187500140822     c
187600930325     C*
187700930420     C* TRASFORMO LA DATA DI CONSEGNA
187800931125     C     D98DCM        IFNE      *ZEROS
187900931125     C                   Z-ADD     D98DCM        WDAT
188000930325     C                   EXSR      DATORE
188100930326     C                   Z-ADD     WORE          WPODCM
188200930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
188300930326     C                   MOVEL     'OSC  '       WR3TAB
188400931125     C                   Z-ADD     D98HMC        WR3HHI
188500930326     C                   EXSR      RO3ORE
188600930326     C                   ADD       WR3HHO        WPODCM
188700930326     C                   END
188800941228     C*
188900941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
189000941228     C     D98DUC        IFNE      *ZEROS
189100941228     C                   Z-ADD     D98DUC        WDAT
189200941228     C                   EXSR      DATORE
189300941228     C                   Z-ADD     WORE          WPODUC
189400970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
189500970310     C                   MOVEL     'OSP  '       WR3TAB
189600970310     C                   Z-ADD     W98HUC        WR3HHI
189700970310     C                   EXSR      RO3ORE
189800970310     C     D98DRF        IFNE      *ZEROS
189900050928     c     w98huc        andgt     1200
190000970310     C                   SUB       12            WR3HHO
190100970310     C                   ENDIF
190200970310     C                   ADD       WR3HHO        WPODUC
190300941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
190400070605     C***  WPODSP        IFGT      WPODUC
190500070605     C***                Z-ADD     WPODSP        WPODUC
190600070605     C***                ENDIF
190700941228     C                   ENDIF
190800960402     C*
190900141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
191000970304     C     D98DBR        IFNE      *ZEROS
191100970304     C                   Z-ADD     D98DBR        WDAT
191200960402     C                   EXSR      DATORE
191300960402     C                   Z-ADD     WORE          WPODBR
191400070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
191500960402     C                   ELSE
191600960402     C                   Z-ADD     WPODSP        WPODBR
191700960402     C                   ENDIF
191800930913     C*
191900960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
192000930913     C* CONSEGNABILITA'
192100960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
192200950407     C*
192300930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
192400931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
192500931125     C                   Z-ADD     D98DIC        WDAT
192600930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
192700930913     C                   Z-ADD     WORE          WPODIC
192800930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
192900931125     C                   Z-ADD     D98OIC        WR3HHI
193000930913     C                   EXSR      RO3ORE
193100930913     C                   ADD       WR3HHO        WPODIC
193200931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
193300950407     C*
193400930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
193500931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
193600931125     C                   Z-ADD     D98DFC        WDAT
193700930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
193800930913     C                   Z-ADD     WORE          WPODFC
193900140822     C                   Z-ADD     WORE          WPODFC_00
194000930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
194100931125     C                   Z-ADD     D98OFC        WR3HHI
194200930913     C                   EXSR      RO3ORE
194300930913     C                   ADD       WR3HHO        WPODFC
194400931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
194500930913     C*
194600930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
194700930913     C     D98ERR        IFEQ      '0'
194800930913     C     WPODFC        IFEQ      *ZEROS
194900930913     C     WPODIC        ANDNE     *ZEROS
195000930913     C                   MOVEL     '1'           D98ERR
195100930913     C                   MOVEL     MSG(17)       D98MSG
195200930913     C                   ENDIF
195300930913     C                   ENDIF
195400930907     C*
195500930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
195600930913     C     D98ERR        IFEQ      '0'
195700930913     C     WPODIC        IFGT      WPODFC
195800930907     C                   MOVEL     '1'           D98ERR
195900930913     C                   MOVEL     MSG(18)       D98MSG
196000930913     C                   ENDIF
196100930913     C                   ENDIF
196200950407     C*
196300950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
196400930511     C*
196500930511     C                   ENDSR
196600950110     C********************************************************************
196700950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
196800950110     C********************************************************************
196900950110     C     CALDPP        BEGSR
197000950110     C*
197100950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
197200950110     C                   MOVEL     'P'           WTC                            *PARTENZA
197300950110     C                   MOVEL     'I'           WIC                            *INTERNA
197400960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
197500950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
197600950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
197700950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
197800950110     C*
197900950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
198000950110     C                   MOVEL     'P'           WTC                            *PARTENZA
198100950110     C                   MOVEL     'C'           WIC                            *CLIENTE
198200950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
198300950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
198400950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
198500950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
198600960229     C*
198700960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
198800050922     c
198900050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
199000050922     c
199100050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
199200090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
199300050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
199400090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
199500050922%%%  C*  ritardo alla partenza.
199600050922     c* Il tutto solo se la data di oggi > della
199700050922     c*  data teorica di partenza determinata
199800050922     c*
199900950110%%%  C     D98ERR        IFEQ      '0'
200000950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
200100950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
200200950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
200300050922     c     wpoattData24  andgt     wpoppi
200400960911%%%  C********             MOVEL'OSC  '   WR3TAB
200500960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
200600960911%%%  C********             EXSR RO3ORE
200700960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
200800050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
200900090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
201000090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
201100950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
201200050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
201300050922     c* La nuova data calcolata non può essere < della minima prevista
201400050922     c                   if        wr0poo>wpoppi
201500050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
201600050922     c                   endif
201700050922     c
201800950110%%%  C                   ENDIF
201900950110%%%  C                   ENDIF
202000950110%%%  C*
202100950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
202200050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
202300050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
202400050915%%%  C*  alla prtenza.
202500950110%%%  C     D98ERR        IFEQ      '0'
202600950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
202700950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
202800950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
202900950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
203000950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
203100950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
203200050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
203300950110%%%  C                   ENDIF
203400950110%%%  C                   ENDIF
203500950110%%%  C*
203600950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
203700950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
203800950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
203900950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
204000950110%%%  C     D98ERR        IFEQ      '0'
204100950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
204200950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
204300950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
204400950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
204500090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
204600090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
204700950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
204800050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
204900950110%%%  C                   ENDIF
205000950110%%%  C                   ENDIF
205100960229     C*
205200960229     C                   ENDIF                                                  <-- 1 ---
205300950110     C*
205400950110     C                   ENDSR
205500930604     C********************************************************************
205600930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
205700930604     C********************************************************************
205800930604     C     CALDPT        BEGSR
205900090902     c                   clear                   RicalcoloTRAZ     1
206000930604     C*
206100930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
206200931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
206300931129     C                   MOVEL     'I'           WIC                            *INTERNA
206400050915     c* se non presente la data partenza ultimo collo, uso la teorica
206500950110     C     D98DUC        IFEQ      *ZEROS
206600950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
206700950110     C                   ELSE
206800950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
206900950110     C                   ENDIF
207000930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
207100930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
207200930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
207300930604     C*
207400930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
207500931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
207600931129     C                   MOVEL     'C'           WIC                            *CLIENTE
207700950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
207800930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
207900930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
208000930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
208100150508
208200150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
208300150508     c* se presente la data partenza PRIMO  collo --> la uso
208400150508     c                   if        w98dpc=0
208500150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
208600150508     c                   else
208700150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
208800150508     C                   MOVEL     'C'           WIC                            *INTERNA
208900150508     C                   Z-ADD     W98DPC        WDAT
209000150508     C                   EXSR      DATORE
209100150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
209200150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
209300150508     C                   EXSR      RO4CDP
209400150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
209500150508     c                   endif
209600960229     C*
209700960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
209800940624%%%  C*
209900050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
210000050915
210100940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
210200050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
210300050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
210400050915     c*   attribuito alla trazione
210500940624%%%  C     D98ERR        IFEQ      '0'
210600940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
210700940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
210800090903     c     wpoatt        andge     wpopti
210900050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
211000090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
211100090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
211200940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
211300050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
211400050922     c                   if        wr0poo>wpopti
211500050922     c                   eval      wpopti=wr0poo
211600050922%%%  C                   endif
211700090902     c*
211800090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
211900090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
212000090902     c*  della trazione udate e utime
212100090902     c                   eval      RicalcoloTRAZ='S'
212200090902     c
212300940624%%%  C                   ENDIF
212400940624%%%  C                   ENDIF
212500940628%%%  C*
212600050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
212700050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
212800050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
212900940628%%%  C     D98ERR        IFEQ      '0'
213000940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
213100940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
213200940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
213300090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
213400090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
213500940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
213600050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
213700940628%%%  C                   ENDIF
213800940628%%%  C                   ENDIF
213900960229     C*
214000960229     C                   ENDIF                                                  <-- 1 ---
214100930604     C*
214200930604     C                   ENDSR
214300930511     C********************************************************************
214400930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
214500930511     C********************************************************************
214600930511     C     CALDCR        BEGSR
214700050914     c                   clear                   consideraora
214800100308     c
214900100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
215000100308     c*
215100100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
215200100308     c* 1) se presente solo ORA --> ignoro sempre
215300100308     c* 2) se presente data e ora IL --> ignoro l'ora
215400100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
215500100309     c***                if        d98tsp='E'
215600100309     c***                clear                   d98dcr
215700100309     c***                clear                   d98hcr
215800100309     c**+                clear                   d98tcr
215900100309     c***                endif
216000100308     c
216100100308     c                   if        d98hcr>0
216200100308     c
216300100308     c                   if        d98dcr=0
216400100308     c                   clear                   d98hcr
216500100308     c                   endif
216600100308
216700100308     c                   if        d98dcr>0 and d98tcr=' '
216800100308     c                   clear                   d98hcr
216900100308     c                   endif
217000100308     c                   endif
217100941228     C*
217200941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
217300941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
217400050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
217500050901     c*  (per uguale si intende entrambe nel mattino o entrambe
217600050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
217700050901     c*  (EDPES 1/9/05)
217800050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
217900050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
218000050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
218100050831     C***                Z-ADD     D98DCM        D98DCR
218200050831     C***                ENDIF
218300050901     c*
218400070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
218500050914     c
218600100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
218700100308     c***                clear                   d98hcr
218800100308     c***                clear                   d98tcr
218900100308     c***                endif
219000050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
219100050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
219200070521     c*  richiesta dal cliente
219300100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
219400100308     C***                MOVEL     'OSC  '       WR3TAB
219500100308     C***                Z-ADD     D98HMC        WR3HHI
219600100308     C***                EXSR      RO3ORE
219700100308     c***                z-add     wr3hho        salvaoreHMC
219800050901     c
219900100308     C***                Z-ADD     D98Hcr        WR3HHI
220000100308     C***                EXSR      RO3ORE
220100100308     c***                z-add     wr3hho        salvaoreHCR
220200100308     c***
220300050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
220400100308    2c***                if        salvaoreHMC=salvaoreHCR
220500100308     C***                eval      Consideraora='S'
220600100308     c***                else
220700100308     C***                eval      Consideraora=' '
220800100308     c***                clear                   d98hcr
220900100308    2c***                endif
221000100308    1c***                endif
221100930511     C*
221200930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
221300931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
221400931125     C                   Z-ADD     D98DCR        WDAT
221500930325     C                   EXSR      DATORE
221600930326     C                   Z-ADD     WORE          WPODCR
221700930420     C*
221800930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
221900930420     C*
222000931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
222100930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
222200930326     C                   MOVEL     'CRD  '       WR3TAB
222300931125     C                   Z-ADD     D98HCR        WR3HHI
222400930419     C                   EXSR      RO3ORE
222500930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
222600940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
222700050902     c
222800931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
222900930420     C                   MOVEL     'CRD1 '       WR3TAB
223000930420     C                   Z-ADD     *ZEROS        WR3HHI
223100930420     C                   EXSR      RO3ORE
223200931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
223300090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
223400090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
223500050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
223600060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
223700050907     c                   z-add     worecr        oreILoDOPO
223800931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
223900931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
224000931207     C                   MOVEL     'C'           WIC                            *CLIENTE
224100931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
224200150330     C                   Z-ADD     WORECR        WR4NOI
224300931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
224400931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
224500931209     C     WPOCRF        SUB       12            WR7POI            9 0
224600931209     C                   MOVEL     'X'           WIC
224700931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
224800931209     C                   ADD       12            WR7POO
224900931207     C     WR7POO        IFGT      WPOCRF
225000931207     C                   Z-ADD     WR7POO        WPOCRF
225100931207     C                   END
225200150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
225300150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
225400150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
225500150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
225600150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
225700150330     C                   MOVEL     'C'           WIC                            *CLIENTE
225800150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
225900150330     C                   Z-ADD     w99TCC        WR4NOI
226000150330     c                   sub       12            wr4noi
226100150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
226200150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
226300150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
226400150330     c
226500150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
226600150330     C                   MOVEL     'X'           WIC
226700150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
226800150330     C******             ADD       12            WR7POO
226900150330     C     WR7POO        IFGT      WPOCRF_dee
227000150330     C                   Z-ADD     WR7POO        WPOCRF_dee
227100150330     C                   END
227200050902     c
227300050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
227400050902     c*  sono le ore di fine della non consegnabilità
227500050905     c* Se aggiungendo 1 finisco in giorno festivo
227600050905     c*  cerco primo giorno non festivo
227700050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
227800050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
227900050907     C****               MOVEL     'C'           WIC                            *CLIENTE
228000050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
228100050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
228200050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
228300050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
228400050907     c
228500050907     c                   add       1             wpocri
228600050902     c
228700050902   FFC                   Z-ADD     WPOCRI        WPOCRC
228800050902     c
228900930325     C                   END
229000150330     C*
229100931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
229200930420     C* DATA SPED. = DATA INIZIO RANGE
229300930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
229400940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
229500940324£$   C                   Z-ADD     WPOCRI        WPOCRC
229600930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
229700930420     C                   MOVEL     'CRP  '       WR3TAB
229800931125     C                   Z-ADD     D98HCR        WR3HHI
229900930420     C                   EXSR      RO3ORE
230000931207     C     WPODCR        ADD       WR3HHO        WPOCRF
230100931207     C                   SUB       24            WPOCRF                         *FINE RANGE
230200931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
230300931207     C* ANDANDO INDIETRO NEL TEMPO
230400050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
230500050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
230600050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
230700050907     c* data utile venedì sera
230800130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
230900130411     C                   MOVEL     'X'           WIC                            *CLIENTE
231000931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
231100931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
231200931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
231300930326     C                   END
231400930526     C*
231500931125     C     D98TCR        IFEQ      'P'
231600931125     C     D98TCR        OREQ      'D'
231700930526     C                   ELSE                                                   *CONS.RICH.IL ..
231800940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
231900940324£$   C                   Z-ADD     WPODCR        WPOCRC
232000930526     C* DATA SPED. = DATA INIZIO RANGE
232100050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
232200050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
232300050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
232400930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
232500930526     C                   MOVEL     'CRI  '       WR3TAB
232600931125     C                   Z-ADD     D98HCR        WR3HHI
232700930526     C                   EXSR      RO3ORE
232800050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
232900050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
233000050907     c                   z-add     wr3hho        oreILoDOPO
233100930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
233200050902     c
233300050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
233400050902     c                   if        wr3MATPOM='P' and d98hcr>0
233500050902     c                   add       12            wpocri
233600100308     c* tolgo 12 ore dalle ore conseganbili
233700050907     c                   sub       12            oreILoDOPO
233800050902     c                   endif
233900050902     c
234000931209     C     WPOCRF        SUB       12            WR7POI            9 0
234100931209     C                   MOVEL     'X'           WIC
234200931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
234300931209     C                   ADD       12            WR7POO
234400931209     C     WR7POO        IFGT      WPOCRF
234500931209     C                   Z-ADD     WR7POO        WPOCRF
234600931209     C                   END
234700050905     c
234800050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
234900050905     c*  sono le ore di fine della non consegnabilità
235000050905     c* Se aggiungendo 1 finisco in giorno festivo
235100050905     c*  cerco primo giorno non festivo
235200050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
235300050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
235400050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
235500050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
235600050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
235700050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
235800050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
235900050907     c
236000050907     c                   add       1             wpocri
236100050905     c
236200050905   FFC                   Z-ADD     WPOCRI        WPOCRC
236300050905     c
236400930526     C                   END
236500931206     C*
236600931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
236700931206     C* LA IMPOSTO COME LA CONS.TEORICA
236800140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
236900140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
237000931206     C                   ELSE
237100931206     C                   Z-ADD     WPOCRF        WPOCR1
237200931206     C                   END
237300150608     c* ai fini del calcolo affidabilità
237400150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
237500150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
237600931206     C                   ELSE
237700931206     C                   Z-ADD     WPOCRF        WPOCR2
237800931206     C                   END
237900150608     c* ai fini della data prevista consegna cliente
238000150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
238100150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
238200150608     C                   ELSE
238300150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
238400150608     C                   END
238500150331     c
238600150331     c                   if        wpocrf_dee>0
238700150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
238800150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
238900150330     C                   END
239000150331     C                   END
239100150331     C*
239200150331     c                   if        wpocrf_dee=0
239300150608     C     wpocr2_dee    sub       24            WPOCRF_dee
239400150331     c                   endif
239500150331     c
239600930526     C                   END                                                    <-- 1 ---
239700930511     C*
239800930511     C                   ENDSR
239900930511     C********************************************************************
240000930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
240100930511     C********************************************************************
240200930511     C     CALLAV        BEGSR
240300930511     C*
240400941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
240500930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
240600930421     C*
240700930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
240800930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
240900930421     C     1             DO        IE            E                 3 0          --- 2 -->
241000930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
241100930421     C                   Z-ADD     1             I
241200930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
241300060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
241400930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
241500930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
241600930421     C                   ADD       1             IL
241700930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
241800930421     C                   EXSR      DATORE
241900930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
242000091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
242100091014     c* Mi salvo la posizione dell'evento
242200091014     C                   Z-ADD     E             LAp(IL)
242300930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
242400930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
242500930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
242600930421     C                   EXSR      RO3ORE
242700930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
242800930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
242900930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
243000930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
243100091015     c**                 if        §2agla>1
243200091015     c**                 sub       1             §2agla
243300930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
243400091015     c**                 else
243500091015     c**                 clear                   whh
243600091015     c**                 endif
243700930510     C                   ADD       WHH           WORELA
243800940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
243900930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
244000090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
244100150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
244200930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
244300931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
244400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
244500930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
244600930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
244700930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
244800930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
244900931209     C                   Z-ADD     LAF(IL)       WR7POI
245000931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
245100931202     C     WR7POO        IFGT      LAF(IL)
245200931201     C                   Z-ADD     WR7POO        LAF(IL)
245300931202     C                   END
245400930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
245500930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
245600091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
245700930510     C                   END                                                    <-- 3 ---
245800931223     C                   ENDDO                                                  <-- 2 ---
245900930421     C*
246000930421     C                   END                                                    <-- 1 ---
246100930618££$$ C*
246200930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
246300931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
246400941228££$$ C     1             DO        IL            I
246500930618££$$ C     LAF(I)        IFGT      WPOLAF
246600930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
246700930618££$$ C                   END
246800931223££$$ C                   ENDDO
246900930511     C*
247000930511     C                   ENDSR
247100940324£$   C********************************************************************
247200940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
247300940324£$   C********************************************************************
247400940324£$   C     CALLAC        BEGSR
247500941229     C*
247600950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
247700950407     C*
247800950210     C                   Z-ADD     WORELA        WTM060
247900941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
248000941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
248100941229     C     D98DAG        IFEQ      *ZEROS
248200050520     c     §2aftg        andeq     'S'
248300090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
248400090827     C     WORELA        ADD       w99TCC        WTM060            6 0
248500160404     c                   if        wVP='S'
248600160404     c                   sub       24            wTM060
248700160404     c                   endif
248800941229     C                   ENDIF
248900941229     C*
249000940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
249100940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
249200941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
249300941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
249400941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
249500940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
249600941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
249700050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
249800050520     c                   movel     §2aftg        LCON(IL)
249900950407     C*
250000950407     C                   ENDIF
250100960129     C*
250200940324£$   C                   ENDSR
250300930511     C********************************************************************
250400930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
250500930511     C********************************************************************
250600930511     C     CALGIA        BEGSR
250700960129     C*
250800960129     C*-------------------------------------------------------------------
250900960129     C* EVENTI GIACENZA
251000960129     C*-------------------------------------------------------------------
251100960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
251200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
251300960129     C*
251400050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
251500041116     c     d98nsp        ifgt      0
251600031125     C                   EXSR      CARGIAC
251700041116     c                   endif
251800031125     C*
251900960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
252000960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
252100960129     C     1             DO        IE            E                 3 0          --- 2 -->
252200960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
252300960129     C                   Z-ADD     1             I
252400960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
252500060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
252600960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
252700960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
252800960129     C                   ADD       1             IG
252900960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
253000960129     C                   EXSR      DATORE
253100960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
253200140606     c* Mi salvo la posizione dell'evento
253300140606     C                   Z-ADD     E             gip(IG)
253400960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
253500960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
253600960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
253700960129     C                   EXSR      RO3ORE
253800960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
253900960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
254000960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
254100960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
254200960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
254300960129     C                   ADD       WHH           WOREGI
254400960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
254500960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
254600090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
254700090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
254800160404     c                   if        wVP='S'
254900160404     c                   sub       24            wOREGI
255000160404     c                   endif
255100960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
255200960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
255300960129     C                   MOVEL     'C'           WIC                            *CLIENTE
255400960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
255500960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
255600960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
255700960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
255800960129     C                   Z-ADD     GIF(IG)       WR7POI
255900960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
256000031125     C     WR7POO        IFGT      GIF(IG)
256100031125     C                   Z-ADD     WR7POO        GIF(IG)
256200031125     C                   ENDIF
256300031201     C*
256400031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
256500031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
256600031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
256700031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
256800031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
256900031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
257000031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
257100031201     C                   IF        WPOATT > GIF(IG)
257200031201     C*
257300031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
257400031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
257500031201     C                   IF        WGGA = *BLANKS
257600031201     C                   SETOFF                                       54
257700031201     C                   ELSE
257800031201     C     WGGA          LOOKUP    SK7QW                                  54
257900031201     C                   ENDIF
258000031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
258100031201     C                   MOVEL     '1'           FLGGIACOK
258200031201     C                   ELSE
258300140127     c
258400031125     C*
258500031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
258600031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
258700031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
258800031125     C                   MOVEL     '0'           FLGGIACOK         1
258900031125     C*
259000031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
259100031125     C     1             DO        IAG           EG                3 0
259200031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
259300031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
259400031125     C                   EXSR      DATORE
259500031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
259600031125     C*
259700031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
259800031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
259900031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
260000031125     C                   EXSR      DATORE
260100031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
260200031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
260300031229     C* => NN LO CONSIDERO
260400031229     C                   IF        EVC(E+1) = 'G' AND
260500031229     C                             WPROXGII = WGCPDORE
260600031229     C                   MOVEL     '1'           FLGGIACOK
260700031229     C                   ENDIF
260800031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
260900031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
261000031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
261100031125     C                   EXSR      RO3ORE
261200031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
261300031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
261400031125     C* DELL'EVENTO GIACENZA CORRENTE
261500031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
261600060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
261700031125     C                   MOVEL     '1'           FLGGIACOK
261800031125     C                   LEAVE
261900031125     C                   ENDIF
262000031125     C                   ELSE
262100031229     C                   IF        EVC(E)  = 'G' AND
262200031229     C                             GII(IG) = WGCPDORE
262300031229     C                   MOVEL     '1'           FLGGIACOK
262400031229     C                   LEAVE
262500031229     C                   ENDIF
262600031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
262700031125     C                   MOVEL     '1'           FLGGIACOK
262800031125     C                   LEAVE
262900031125     C                   ENDIF
263000031125     C                   ENDIF
263100031125     C                   ENDDO
263200031201     C                   ENDIF
263300140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
263400140127     C                   IF        FLGGIACOK = '0'  and
263500140127     c                             d98dcm=evd(E)
263600140127     C                   MOVEL     '1'           FLGGIACOK
263700140127     c                   endif
263800031125     C*
263900031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
264000031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
264100031125     C* DELL'EVENTO GIACENZA CORRENTE
264200031125     C                   IF        FLGGIACOK = '0'
264300031125     C                   Z-ADD     *ZEROS        GIF(IG)
264400031125     C                   ELSE
264500031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
264600031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
264700031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
264800031125     C                   ENDIF
264900031125     C                   ELSE
265000031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
265100031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
265200031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
265300031125     C                   ENDIF
265400031125     C*
265500960129     C                   ENDIF                                                  <-- 3 ---
265600960129     C                   ENDDO                                                  <-- 2 ---
265700960129     C*
265800960129     C                   ENDIF                                                  <-- 1 ---
265900960129££$$ C*
266000960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
266100960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
266200960129££$$ C     1             DO        IG            I
266300960129££$$ C     GIF(I)        IFGT      WPOGIF
266400960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
266500960129££$$ C                   ENDIF
266600960129££$$ C                   ENDDO
266700941229     C*
266800941229     C*-------------------------------------------------------------------
266900941229     C* APERTURA GIACENZA
267000941229     C*-------------------------------------------------------------------
267100960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
267200960129     C*
267300960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
267400931125     C                   Z-ADD     D98DAG        WDAT
267500930325     C                   EXSR      DATORE
267600950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
267700960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
267800950413£££  C                   Z-ADD     WORE          WPODAG
267900960129$$£  C*********************ENDIF
268000930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
268100930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
268200930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
268300930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
268400930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
268500941229     C*
268600941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
268700941229     C                   Z-ADD     WPODAG        WPOAGC
268800960129     C*
268900960129     C                   ENDIF                                                  <-- 1 ---
269000941229     C*
269100941229     C*-------------------------------------------------------------------
269200930511     C* CHIUSURA GIACENZA
269300941229     C*-------------------------------------------------------------------
269400930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
269500930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
269600931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
269700931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
269800931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
269900931201     C                   Z-ADD     D98DCM        W98DCG
270000930416     C                   ELSE
270100930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
270200930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
270300931201     C                   Z-ADD     WDAODI        W98DCG
270400061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
270500061123     c*  chiusura giacenza, come già fa in altri posti
270600061123     c                   if        d98dfc>0
270700061123     C                   Z-ADD     d98dfc        W98DCG
270800061123     c                   endif
270900061123     c
271000930409     C                   END
271100930409     C                   END
271200930409     C                   END
271300930414     C* TRASF.DATA CHIUSURA
271400931201     C     W98DCG        IFNE      *ZEROS
271500931201     C                   Z-ADD     W98DCG        WDAT
271600930325     C                   EXSR      DATORE
271700930326     C                   Z-ADD     WORE          WPODCG
271800930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
271900930407     C* GIORNO DA ESCLUDERE
272000150216     C                   ADD       12            WPODCG
272100150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
272200150123     c*  il giorno di chiusura dovrebe già essere protetto
272300941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
272400941229     C     WPOCGC        IFEQ      *ZEROS
272500941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
272600941229     C                   ENDIF
272700930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
272800930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
272900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
273000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
273100150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
273200150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
273300160404     c                   if        wVP='S'
273400160404     c                   sub       24            wr4NOI
273500160404     c                   endif
273600930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
273700930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
273800930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
273900931209     C                   Z-ADD     WPODCG        WR7POI
274000931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
274100931202     C     WR7POO        IFGT      WPODCG
274200931201     C                   Z-ADD     WR7POO        WPODCG
274300930326     C                   END
274400931202     C                   END
274500930511     C*
274600930511     C                   ENDSR
274700031125     C********************************************************************
274800031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
274900031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
275000031125     C*             DEGLI EVENTI DI TIPO GIACENZA
275100031125     C********************************************************************
275200031125     C     CARGIAC       BEGSR
275300031125     C*
275400031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
275500031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
275600031125     C*
275700050405     C                   Z-ADD     D98AAS        KWGCPAAS
275800050405     C                   Z-ADD     D98LNP        KWGCPLNP
275900050405     C                   Z-ADD     D98NRS        KWGCPNRS
276000050405     C                   Z-ADD     D98NSP        KWGCPNSP
276100031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
276200050405     C     KEYGC_P       SETLL     TIGCP01L
276300050405     C                   IF        %equal(TIGCP01L)
276400050405     C     KEYGC_P       READE     TIGCP01L
276500050405     C                   DOW       not %eof(TIGCP01L)
276600031125     C                   ADD       1             IAG
276700031125     C                   EVAL      GCPD(IAG) = GCPDUR
276800050405     C     KEYGC_P       READE     TIGCP01L
276900031125     C                   ENDDO
277000031125     C                   ENDIF
277100031125     C                   ELSE
277200050405     C     KEYGC_P       SETLL     TIGCP21L
277300050405     C                   IF        %equal(TIGCP21L)
277400050405     C     KEYGC_P       READE     TIGCP21L
277500050405     C                   DOW       not %eof(TIGCP21L)
277600031125     C                   ADD       1             IAG
277700031125     C                   EVAL      GCPD(IAG) = GCPDUR
277800050405     C     KEYGC_P       READE     TIGCP21L
277900031125     C                   ENDDO
278000031125     C                   ENDIF
278100031125     C                   ENDIF
278200031125     C*
278300031125     C                   ENDSR
278400941229     C********************************************************************
278500960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
278600941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
278700941229     C********************************************************************
278800960129     C     CALGIC        BEGSR
278900941229     C*
279000960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
279100960129     C*
279200960129     C                   Z-ADD     WOREGI        WTM060
279300960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
279400960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
279500960129     C     D98DAG        IFEQ      *ZEROS
279600960129$$£  C     WOREGI        ADD       12            WTM060            6 0
279700960129     C                   ENDIF
279800960129     C*
279900960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
280000960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
280100960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
280200960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
280300960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
280400960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
280500960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
280600960129     C*
280700960129     C                   ENDIF
280800941229     C*
280900941229     C                   ENDSR
281000060418     C********************************************************************
281100060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
281200060418     c*            l'affidabilità
281300060418     C********************************************************************
281400060418     C     CALCEPA       BEGSR
281500060418     C*
281600060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
281700060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
281800060420     c                   clear                   numggcepa         3 0
281900060418     C*
282000060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
282100060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
282200060420     c                   eval      wgiroepa='0'
282300060418     c                   eval      wfine=' '
282400060418     c
282500060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
282600060420    2C     W             DOwle     IP                                           --- 2 -->
282700060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
282800060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
282900060418     c
283000060420    4c                   if        wgiroepa='0'
283100060420     c                   exsr      ElaInizCEPA
283200060418   x4c                   else
283300060420    5c                   if        pvd(W)>wdat
283400060418     c                   eval      wfine='S'
283500060418    5c                   endif
283600060418    4c                   endif
283700060418     c
283800060420    4c                   if        wfine=' ' and pvd(W)=wdat
283900060420     C*
284000060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
284100060420     c*  finale
284200060420     c                   add       1             numggcepa
284300060420     c
284400060420     C                   z-add     24            WORELA
284500060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
284600060418     c
284700060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
284800060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
284900060418     C                   MOVEL     'C'           WIC                            *CLIENTE
285000060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
285100060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
285200060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
285300060418     c
285400060418     c* Trasformo le ore in data e verifico se c'e' altro evento
285500060418     c*  in sta data che allunga ancora
285600060420     c                   z-add     wr4poo        wore
285700060418     c                   EXSR      OREDAT
285800060418    4c                   endif
285900060418     c
286000060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
286100060418    4c                   if        wfine='S'
286200060418     c                   EXSR      ELAFINECEPA
286300060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
286400060420     c*  inizio range
286500060420     c                   else
286600060420     c                   add       1             w
286700060418    4C                   ENDif                                                  <-- 3 ---
286800060420     c
286900060420     c                   else
287000060420     c                   add       1             w
287100060418    3C                   ENDif                                                  <-- 3 ---
287200060418    2C                   ENDDO                                                  <-- 2 ---
287300060418     c
287400060420     c                   if        wgiroepa='1'
287500060418     c                   EXSR      ELAFINECEPA
287600060418     c                   endif
287700060418     C*
287800060418    1C                   END                                                    <-- 1 ---
287900060418££$$ C*
288000060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
288100060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
288200060420££$$ C     1             DO        Ipa           w
288300060420££$$ C     paif(w)       IFGT      WPOpaif
288400060420££$$ C                   Z-ADD     paif(w)       WPOpaif
288500060418££$$ C                   END
288600060418££$$ C                   ENDDO
288700060418     C*
288800060418     C                   ENDSR
288900060420     C********************************************************************
289000060420     c* Elabora Inizio range eventi particolari che allungano
289100060420     C********************************************************************
289200060420     c     ElaInizCEPA   BEGSR
289300060420     C                   ADD       1             IPA
289400060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
289500060420     C                   EXSR      DATORE
289600060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
289700061017     c
289800060420     c                   eval      wgiroepa='1'
289900060420     c                   eval      wdat=pvd(w)
290000060420     C                   ENDSR
290100060418     C********************************************************************
290200060418     c* Elabora fine range eventi particolari che allungano
290300060418     C********************************************************************
290400060418     c     ElafineCEPA   BEGSR
290500060420     c     numggcepa     mult      24            worela
290600060420     c                   sub       12            worela
290700090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
290800090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
290900160404     c                   if        wVP='S'
291000160404     c                   sub       24            WORELA
291100160404     c                   endif
291200060418     c
291300060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
291400060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
291500060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
291600060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
291700060418     C                   Z-ADD     paif(IPA)     WR7POI
291800060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
291900060418    5C     WR7POO        IFGT      paif(IPA)
292000060418     C                   Z-ADD     WR7POO        paif(IPA)
292100060418    5C                   END
292200061017     c*Sommo 24 ore di inizio giornata
292300061017     c*            : non ho capito perchè ma ci vogliono
292400061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
292500090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
292600090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
292700160404     c                   if        wVP='S'
292800160404     c                   sub       24            paii(IPA)
292900160404     c                   endif
293000061017     c* Se tempi di consegna inferiori alle 24 ore , devo
293100061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
293200061017     c*  partire prima
293300061017     c****               if        d98tcc<24
293400061017     c****               sub       12            paii(ipa)
293500061017     c****               endif
293600060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
293700060418     c* Imposto nuovo giro
293800060420     c                   eval      wgiroepa='0'
293900060418     c                   eval      wfine=' '
294000060420     c                   clear                   numggcepa         3 0
294100060418     c                   ENDSR
294200930511     C********************************************************************
294300930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
294400930511     C********************************************************************
294500010822     C     CALASS        BEGSR
294600010821     C*
294700011211     C                   EXSR      REPMAXCSB
294800930511     C*
294900930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
295000010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
295100011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
295200930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
295300930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
295400930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
295500930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
295600930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
295700930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
295800930511     C                   ELSE                                                   <-- 4 -->
2959009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
296000930512     C                   END                                                    <-- 4 ---
296100930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
296200930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
296300930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
296400930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
296500930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
296600930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
296700930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
296800930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
296900930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
297000930413     C                   END                                                    <-- 2 ---
297100011211     C                   END                                                    <-- 1 ---
297200930406     C*
297300930325     C                   ENDSR
297400930608     C********************************************************************
297500930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
297600930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2977009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
297800930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
297900930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
298000930608     C********************************************************************
298100930608     C     RDRASS        BEGSR
298200930608     C*
298300930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2984009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2985009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2986009407151507 C*********************EXSR RO3ORE
2987009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
298800930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2989009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2990009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2991009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2992009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2993009407191907 C                   Z-ADD     WPOASI        WPOROU
2994009407191907 C                   EXSR      CHKFES
2995009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2996009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2997009407191907 C                   ELSE                                                   *E' FERIALE
2998009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2999009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3000009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
3001009407191907 C                   ENDIF
300200930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
300300930608     C                   MOVEL     'ASS1 '       WR3TAB
300400930608     C                   Z-ADD     *ZEROS        WR3HHI
300500930608     C                   EXSR      RO3ORE
300600930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
300700930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
300800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
300900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
301000930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
301100930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
301200930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
301300930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
301400930608     C*
301500930608     C                   ENDSR
301600930511     C********************************************************************
301700930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
301800930511     C********************************************************************
301900930511     C     CALFED        BEGSR
302000150331     c                   clear                   fddata
302100150331     c                   clear                   WPOFDdata
302200930511     C*
302300051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
302400931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
302500051108     C     D98FFD        oreq      'M'                                          --- 1 -->
302600930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
302700930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
302800930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
302900930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
303000150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
303100150331     c                   if        §ar5dfd>*zeros
303200150331     c                   movel     §ar5dfd       fddata            8 0
303300150331     c                   endif
303400150331     c                   if        fddata>0 and ((d98dti>0 and
303500150331     c                             fddata>d98dti) or d98dti=0)
303600150331     C                   Z-ADD     fddata        WDAT
303700150331     C                   EXSR      DATORE
303800150331     C                   Z-ADD     WORE          WPOfddata
303900150331     c                   endif
304000150331     c
304100930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
304200930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
304300930511     C                   ELSE                                                   <-- 2 -->
3044009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
304500930512     C                   END                                                    <-- 2 ---
304600150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3047001504081507 C                   Z-ADD     WPOfddata     WPOFDI
304800150331     c                   endif
304900150331
305000930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
305100930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
305200930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
305300930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
305400930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
305500930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
305600930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
305700930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
305800930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
305900930511     C                   END                                                    <-- 1 ---
306000930511     C*
306100930511     C                   ENDSR
306200930608     C********************************************************************
306300930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
306400930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3065009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
306600930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
306700930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
306800930608     C********************************************************************
306900930608     C     RDRFED        BEGSR
307000930608     C*
307100930608     C* CALCOLO ORE DEL FERMO DEPOSITO
307200930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3073009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3074001503311907 C                   Z-ADD     WPOFDI        WPOROU
3075001503311907 C                   EXSR      CHKFES
3076001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3077001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3078001503311907 C                   ELSE                                                   *E' FERIALE
3079001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3080001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3081001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3082001503311907 C                   ENDIF
308300150331     c* Se presente la particolarità consegna con più giorni di fermo depo
308400150331     c*  uso questi
308500150331     c                   if        §7rggfd>*zeros
308600150424     c                   movel     §7rggfd       wggfd             2 0
308700150331     c     wggfd         mult      24            Wnore
308800150331     c                   add       wnore         worefd
308900150331     c                   else
309000930608     C                   MOVEL     'FED1 '       WR3TAB
309100930608     C                   Z-ADD     *ZEROS        WR3HHI
309200930608     C                   EXSR      RO3ORE
309300940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
309400150331     c                   endif
309500930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
309600931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
309700931129     C                   MOVEL     'C'           WIC                            *CLIENTE
309800930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
309900930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
310000930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
310100930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
310200930608     C*
310300930608     C                   ENDSR
310400931201     C********************************************************************
310500931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
310600931201     C********************************************************************
310700931201     C     CALDTE        BEGSR
310800140825     c
310900140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
311000140825     c*  che tiene conto solo dei festivi per restituire in output
311100140825     c*  la data teorica di consegna
311200140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
311300140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
311400140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
311500140825     C                   ELSE
311600140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
311700140825     C                   END
311800140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
311900141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
312000141106     c*  quindi passo un flag
312100141106     c                   eval      nosomma='N'
312200140825     C                   EXSR      RO4CDP
312300141106     c                   eval      nosomma=' '
312400140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
312500140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
312600140825     c*  di consegna
312700140825     c                   EXSR      TRAS_DEI
312800140825     c
312900140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
313000140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
313100140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
313200140825     c                             ='M' or %subst(d98gc2:2:1)='P')
313300140825     c                   exsr      Reper_tempi
313400140825     c                   endif
313500931201     C*
313600140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
313700140825     c*  che tiene conto di:
313800140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
313900150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
314000931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
314100931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
314200931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
314300931201     C                   ELSE
314400931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
314500931201     C                   END
314600090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314700090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314800931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
314900140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
315000931201     C*
315100150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
315200931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
315300931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
315400090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
315500931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
315600150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
315700150608     c
315800150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
315900150608     c*  che non tiene conto dei tempi del C/Ass
316000150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
316100150609     c*
316200150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
316300150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
316400150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
316500150630     c                   Z-ADD     WPODTI        WR6POI
316600150609     c                   else
316700150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
316800150609     c                   endif
316900150609     c
317000150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
317100150608     c                   eval      wdee='S'
317200150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
317300150608     c                   eval      wdee=' '
317400150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
317500141017
317600141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
317700150219     C**                 EVAL      WTC='C'
317800150219     C**                 EVAL      WIC='C'
317900150219     c**                 z-add     wpoattdata00  WPOROU
318000150219     c**                 z-add     wdaodi        wpodata
318100150219     c**                 exsr      chkfes
318200150219     c**                 if        wslitt='S'
318300150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
318400150219     c**                 z-add     24            wr6noi
318500150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
318600150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
318700150219     c**                 else
318800150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
318900150219     c**                 endif
319000141204     c* Ricalcolo WPOrifDatLav
319100141229     C**                 MOVEL     'OSC  '       WR3TAB
319200141229     C**                 Z-ADD     WHHODI        WR3HHI
319300141229     C**                 EXSR      RO3ORE
319400141229     c**
319500141229     C**                 EVAL      WTC='C'
319600141229     C**                 EVAL      WIC='C'
319700141229     c**                 z-add     wpoattdata00  WPOROU
319800141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
319900141229     c**                 z-add     wr3hho        wr6noi
320000141229     c**                 add       12            wr6noi
320100141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
320200141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
320300141229     c
320400141017     c
320500141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
320600141017     c*  ma sempre devo slittare i festivi
320700141017     C* CONSEGNABILITA'
320800150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
320900150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
321000150219     C**                 EVAL      WTC='C'
321100150219     C**                 EVAL      WIC='C'
321200150219     c**                 z-add     d98dfc        WPOdata
321300150219     c**                 exsr      chkfes
321400150219     c**                 if        wslitt='S'
321500150219     c**                 z-add     wpodfc_00     Wr6poi
321600150219     c**                 z-add     24            wr6noi
321700150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
321800150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
321900150219     c**                 else
322000150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
322100150219     c**                 endif
322200150219     c**                 endif
322300931201     C*
322400931201     C                   ENDSR
322500930511     C********************************************************************
322600930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
322700930511     C********************************************************************
322800930511     C     CALDPC        BEGSR
322900930325     C*
323000930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
323100930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
323200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
323300930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
323400930325     C                   ELSE
323500930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
323600930407     C                   END
323700090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
323800090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
323900930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
324000090902     c
324100931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
324200090901
324300930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
324400930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
324500930423     C* A QUELLA DI RICHIESTA CONSEGNA,
324600930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
324700931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
324800931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
324900931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
325000931206     C                   END
325100930423     C                   END
325200930325     C*
325300930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
325400930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
325500930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
325600090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325700090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325800930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
325900931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
326000930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
326100930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
326200930423     C* A QUELLA DI RICHIESTA CONSEGNA,
326300930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
326400931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
326500931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
326600931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
326700931206     C                   END
326800930423     C                   END
326900140822     C*
327000930324     C                   ENDSR
327100150113     C********************************************************************
327200150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
327300150113     C********************************************************************
327400150113     c     Cal_DEE       BEGSR
327500150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
327600150508     c                   clear                   wtempi00C         1
327700151007     c                   clear                   wtempi12C         1
327800150722     c                   clear                   woreOLP
327900151009     c                   clear                   wdiff
328000150219     c
328100150219     c* 1) Se presente l'arrivo della trazione lo uso
328200150219    1c                   select
328300150219     c                   when      wmancatraz=' '  and d98dti>0
328400150630     c                   Z-ADD     WPODTI        WR1POI
328500150219     c
328600150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
328700150219    1c                   when      IDdata>0
328800150325     c                   exsr      CalcoloIMP
328900151008     c* Passo in output il terminal di disguido
329000151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
329100151008     c                   eval      d98flp=%editc(LTP(2):'X')
329200151008     c                   endif
329300151008
329400150528     c* stato IMP/disguido solo se >  alla data di oggi
329500151006     c                   if        wr1poi > wpoattdata00 and
329600151006     c                             (wr1poi-wpoattdata00)>12
329700150427     c                   eval      d98spcdee='IMP'
329800151008     c                   endif
329900151006     c                   if        wr1poi > wpoattdata00 and
330000151006     c                             (wr1poi-wpoattdata00)=12
330100151006     c                             and w991In>12
330200151006     c                   eval      d98spcdee='IMP'
330300151008     c                   endif
330400150219     c
330500150219     c                   other
330600150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
330700150225     c* devo far finta che sia arrivato.
330800150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
330900150508     c*  che tiene conto della data partenza PRIMO  collo
331000150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
331100150508     c*
331200150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
331300150508     c                   if        wr1poi >=wpoattdata00
331400150508     c                              and WPOPTC_DEE > WPOPTC
331500150508     c                   eval      d98spcdee='IMP'
331600150508     c                   endif
331700150219     c                   endsl
331800150113     c
331900150219     c* se il calcolo è < di oggi --> imposto oggi
332000150113     c                   if        wr1poi < wpoattdata00
332100150113     c                   eval      wr1poi= wpoattdata00
332200150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
332300150508     c                   eval      wTempi00C='S'
332400151009     c                   clear                   wdiff
332500150113     c                   endif
332600150217     c
332700150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
332800150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
332900150630     c                   if        wr1poi>wpoattdata00
333000150630     c                   eval      wdiff=wr1poi-wpoattdata00
333100150630     c                   endif
333200151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
333300151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
333400151006     c                   eval      wTempi12C='S'
333500151006     c                   endif
333600160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
333700151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
333800151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
333900160404     c                             and idfgs>0
334000151009     C     wdiff         DIV       24            WDIV
334100151009     C                   MVR                     WRES
334200151009     C                   IF        wres>0
334300151009     c                   eval      wTempi12C='S'
334400151009     c                   else
334500151009     c                   eval      wTempi00C='S'
334600151009     c                   endif
334700151009     c                   endif
334800151009     c
334900151006     c
335000151006     c                   select
335100151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
335200151006     c                             w991in>12 and
335300151006     c                             (deeindist<>'S' or d98cons<>'S'
335400151006     c                             or d98ddc>wdaodi)
335500151006     c
335600151006     c* Se stato IMP lo mantengo per avere messaggio diverso
335700151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
335800151006     c                   if        d98spcdee<>'IMP'
335900151006     c                   eval      d98spcdee='PRV'
336000151006     c                   endif
336100151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
336200150330     c                             (deeindist<>'S' or d98cons<>'S'
336300150330     c                             or d98ddc>wdaodi)
336400150427     c
336500150427     c* Se stato IMP lo mantengo per avere messaggio diverso
336600150427     c                   if        d98spcdee<>'IMP'
336700150304     c                   eval      d98spcdee='PRV'
336800150427     c                   endif
336900150427     c
337000151006   x1c                   other
337100150330     c                   exsr      CAL_OLP
337200150217     c
337300150528     c* Se passato l'orario oppure data distinta < data di oggi
337400150508     c* 1) spedizione in distinta          --> in consegna
337500150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
337600150528     c                             d98ddc<wdaodi)
337700150508    3c                   IF        deeindist='S' and d98cons='S'
337800150508     c                   eval      d98spcdee= 'DDC'
337900150508
338000150508   x3c                   else
338100150507     c
338200150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
338300150722     c*  entro sera
338400150630     c                   if        wr1poi>wpoattdata00
338500150722     c                   z-add     12            woreOLP           3 0
338600150722     c***                add       12            wr1poi
338700150630    4c                   else
338800150507    4c                   if        wr1poi<=wpoattdata00
338900150722     c                   add       24            woreOLP
339000150722     c***                add       24            wr1poi
339100150507    4c                   endif
339200150630    4c                   endif
339300150507     c
339400150722     c****               eval      d98spcdee='OLP'
339500150507     c* memorizzo orario limite picking
339600150507     c                   eval      d98picklim=%editc(picklim:'X')
339700150507    3c                   endif
339800150507    2c                   endif
339900150507     c
340000151006    1c                   endsl
340100150217     c
340200150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
340300150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
340400150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
340500150113     C                   Z-ADD     Wr1poi        W10POI            9 0
340600150113     C                   EXSR      R10FES
340700150113     C                   Z-ADD     W10POO        wr1poi
340800150113     C
340900151006     c                   select
341000151006    1c                   when      wtempi12C='S'
341100151006     c                   z-add     W991IN        wr1noi
341200151006     c                   if        woreOLP>0
341300151006     c                   add       woreOLP       wr1noi
341400151006     c                   eval      d98spcdee='OLP'
341500160404     c                   else
341600160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
341700160404     c**                 if        wVP='S'
341800160404     c**                 add       24            wr1noi
341900160404     c**                 endif
342000151006     c                   endif
342100160404
342200151006    1c                   when      wtempi00C='S'
342300150630     c***                z-add     W990CN        wr1noi
342400150630     c                   z-add     W990IN        wr1noi
342500150722     c
342600150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
342700150722     c*  quindi sottraggo 24
342800151009     c                   if        wr1noi>24  and wdiff=0
342900150722     c                   sub       24            wr1noi
343000150722     c                   endif
343100150722     c
343200150722     c* se passato orario limite picking, slitto in avanti
343300150722     c                   if        woreOLP>0
343400150722     c                   add       woreOLP       wr1noi
343500150722     c                   eval      d98spcdee='OLP'
343600160404     c                   else
343700160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
343800160404     c**                 if        wVP='S'
343900160404     c**                 add       24            wr1noi
344000160404     c**                 endif
344100150722     c                   endif
344200150722     c
344300151006   x1c                   other
344400150722     c
344500150714     c                   if        wmancatraz=' '  and d98dti>0
344600150630     c                   z-add     w99tci        wr1noi
344700150714     c                   else
344800150714     c                   z-add     w99tcc        wr1noi
344900150714     c                   endif
345000160404
345100160401     c
345200150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
345300150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
345400150722     c                   if        woreOLP=24 and wr1noi<=24
345500150722     c                   add       woreOLP       wr1noi
345600150722     c                   eval      d98spcdee='OLP'
345700150722     c                   endif
345800150722     c                   if        woreOLP=12 and wr1noi<24
345900150722     c                   add       woreOLP       wr1noi
346000150722     c                   eval      d98spcdee='OLP'
346100150722     c                   endif
346200150722     c
346300151006    1c                   endsl
346400150629     c
346500150722    1c**                 if        WR1NOI=10
346600150722     c**                 z-add     12            wr1noi
346700150722     c**                 endif
346800150722     c
346900150722     c     wr1noi        div       12            wdiv
347000150722     c                   mvr                     wres
347100150722     c                   if        wres >0
347200150722     c                   add       2             wr1noi
347300150722     c                   endif
347400150722     c
347500150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
347600150629    1c******             endif
347700150508     c
347800150605     c                   eval      wdee='S'
347900150113     C                   EXSR      RO1CDP
348000150605     c                   eval      wdee=' '
348100150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
348200150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
348300150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
348400150113     C* A QUELLA DI RICHIESTA CONSEGNA,
348500150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
348600150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
348700150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
348800150331     c*  2) IL        -> è il giorno indicato
348900150331     C     D98DCR        IFNE      *ZEROS
349000150331     C     D98tcr        andne     'P'
349100150331     C***  WPOdpcee      IFLE      WPOCR2
349200150331     C***  WPOCR2        SUB       12            WPOdpcee
349300150330     c
349400150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
349500150330     c*   dare la prima data utile di consegna
349600150330     C     WPOdpcee      IFLE      WPOCRF_dee
349700150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
349800150330     C                   z-add     WPOCRF_dee    WPOdpcee
349900150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
350000150409     c                   if        wpodpcee>wpoattdata00
350100150409     c                   eval      d98spcdee='PRV'
350200150409     c                   endif
350300150113     C                   END
350400150331     c
350500150220     c
350600150113     C                   END
350700150220     c
350800150113     c                   ENDSR
350900150330     C********************************************************************
351000150330     C*   Calcolo orario da tabella OLP
351100150330     C********************************************************************
351200150330     c     Cal_OLP       BEGSR
351300150330     c                   eval      picklim=0900
351400150330     c* Dopo l'ora limite picking, passo al giorno dopo
351500150330     C                   MOVEL(P)  'OLP'         TBECOD
351600150330     C                   MOVEL(P)  d98lna        TBEKE1
351700150330     C                   MOVEL(P)  wdaodi        TBEKE2
351800150330     C     KTBE          CHAIN     TNTBE01L
351900150330     C                   IF        not %found(TNTBE01L)
352000150330     c                   clear                   tbeke2
352100150330     C     KTBE          CHAIN     TNTBE01L
352200150330     c                   endif
352300150330     C                   IF        not %found(TNTBE01L)
352400150330     C                   MOVEL(P)  '000'         TBEKE1
352500150330     C     KTBE          CHAIN     TNTBE01L
352600150330     c                   endif
352700150330     c
352800150330     C                   IF        %found(TNTBE01L) and
352900150330     c                             %subst(tbeuni:1:4)>*zeros
353000150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
353100150330     c                   endif
353200150330     c                   ENDSR
353300150325     C********************************************************************
353400150325     C*   Riclcolo data se colli in IMP/disguido
353500150325     C********************************************************************
353600150325     c     CalcoloIMP    BEGSR
353700150325     C                   Z-ADD     IDdata        WDAT
353800150325     C                   EXSR      DATORE
353900150325     C                   Z-ADD     WORE          WR4POI
354000150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
354100151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
354200150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
354300150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
354400150325     c* calcolo il terminal di partenza
354500150325     c                   clear                   fnlv55ds
354600150325     c                   eval      d55tla='L'
354700150325     c                   eval      d55tpt='P'
354800150325     c                   eval      d55lin=IDfgs
354900150325     c                   eval      d55drf=IDdata
355000150325     c                   call      'FNLV55R'
355100150325     c                   parm                    fnlv55ds
355200150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
355300150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
355400150325     c                   EXSR      RTTRAZ
355500150325    3c                   if        d98err<>'1'
355600150325     C                   Z-ADD     CASTcn        wr4noi
355700150325     c                   else
355800150325     C                   Z-ADD     d98ttc        wr4noi
355900150325    3C                   endif
356000150325
356100150325   x2c                   else
356200150325     C                   Z-ADD     d98ttc        wr4noi
356300150325    2C                   endif
356400150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
356500150325     c*  perchè partirà un giorno dopo
356600150325    3c                   if        IDnpg='P'
356700150325     c                   add       24            WR4noi
356800150325    3c                   endif
356900150325     c
357000150325     c                   exsr      ro4cdp
357100150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
357200150325     c                   ENDSR
357300930604     C********************************************************************
357400930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
357500930604     C********************************************************************
357600930604     C     TPODAT        BEGSR
357700960229     C*
357800960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
357900960229     C*
358000960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
358100960229     C     WPOPTI        IFEQ      *ZEROS
358200960229     C                   Z-ADD     *ZEROS        D98DTI
358300960229     C                   Z-ADD     *ZEROS        D98HTI
358400960229     C                   ELSE
358500960229     C                   Z-ADD     WPOPTI        WORE
358600960229     C                   EXSR      OREDAT
358700960229     C                   Z-ADD     WDAT          D98DTI
358800960229     C     WRES          MULT      100           D98HTI
358900960229     C                   END
359000960229     C*
359100960229     C                   ELSE                                                   <-- 1 -->
359200930604     C*
359300931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
359400140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
359500140825     c*******            exsr      Tras_DEI
359600140825     c
359700930604     C*
359800150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
359900140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
360000140820     c* è data da :
360100140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
360200140820     c*  Tenere  conto, ovvero slittare in avanti, per :
360300140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
360400140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
360500140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
360600140911     c*
360700140820     c*  Da azzerare nei seguenti casi:
360800150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
360900150324     c* 4) bolla export    7) Cod manca consegna
361000140911     c* 8) bolla con DCR DOPOil o PRIMAdel
361100150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
361200150119     c*10) se completamente non arrivata e collo in IMP
361300150119     c
361400140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
361500150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
361600140925     C*  - FD   - export -
361700150217     c                   movel     'S'           DEEinDIST         1
361800150113     c                   eval      wcaldpcee='S'
361900150304     c                   eval      D98SPCDEE='PRV'
362000150324     c                   clear                   avvcmc            1
362100150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
362200150323     c                   if        d98cmc<>*blanks
362300150324     C                   Z-ADD     1             II
362400150326     C     d98cmc        LOOKUP    C2A(II)                                99
362500150324     C                   MOVEL     D2A(II)       usa2A
362600150324    4C     §2AFTC        IFEQ      'A'
362700150326     c                   eval      avvcmc='A'
362800150323     c                   endif
362900150324    4C     §2AFTC        IFEQ      'G'
363000150326     c                   eval      avvcmc='G'
363100150324     c                   endif
363200150324     c                   endif
363300140820     c
363400140911     c                   z-add     d98lna        EX
363500150113     c*  la azzero nei casi previsti                 0
363600150601    1c                   select
363700150119     c* 1) Se la bolla e consegnata
363800140925     c                   when      d98dcm>0
363900150113     c                   clear                   wcaldpcee
364000150216     c                   eval      d98spcdee='CON'
364100150217     c                   movel     ' '           DEEinDIST         1
364200150323     c* 2) Se pratica di danno
364300150603     c                   when      flgdan='S'
364400150323     c                   clear                   wcaldpcee
364500150323     c                   eval      d98spcdee='CA '
364600150323     c                   eval      deeindist='S'
364700150324     c* 3) Se bloccata/rimpallo
364800150114     c                   when      wd98fbc='B'
364900150113     c                   clear                   wcaldpcee
365000150216     c                   eval      d98spcdee='BLO'
365100150217     c                   movel     ' '           DEEinDIST         1
365200150324     c* 4) Se bolla export
365300150324     c                   when      fl1(EX)='E'
365400150324     c                   clear                   wcaldpcee
365500150324     c                   movel     ' '           DEEinDIST         1
365600150324     c                   eval      d98spcdee='EXP'
365700150324     c* 5) Se giacente  o in attesa di apertura giacenza
365800150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
365900150324     c                   clear                   wcaldpcee
366000150324     c                   eval      d98spcdee='GIA'
366100150324     c                   eval      deeindist='S'
366200150119     c* 7) Se codice manca consegna
366300150326     c                   when      d98cmc<>*blanks and avvcmc='A'
366400150324     c                   clear                   wpodpcee
366500150324     c                   clear                   wcaldpcee
366600150324     c                   movel     ' '           DEEinDIST         1
366700150324     c                   eval      d98spcdee='ATT'
366800150410     c* 7) Bolla in sospensione consegna
366900150410     c                   when      §B4LBGE='S'
367000150410     c                   clear                   wpodpcee
367100150410     c                   clear                   wcaldpcee
367200150410     c                   movel     ' '           DEEinDIST         1
367300150410     c                   eval      d98spcdee='SOS'
367400150616     c
367500150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
367600161031     c                   when      wrbl='R'  or d98cbo='M '
367700150616     c                   clear                   wcaldpcee
367800150616     c                   eval      d98spcdee='   '
367900150616     c                   eval      deeindist='S'
368000150601   x1c                   other
368100150325     c*
368200150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
368300150331     c                   if        d98ffd='S'
368400150601    2c                   select
368500150403     c* FD escluso dai contatti
368600150601    2c                   when      §B4LTFD= 'N'
368700150403     c                   eval      d98spcdee='FD3'
368800150403     c                   clear                   wcaldpcee
368900150403     c                   movel     ' '           DEEinDIST         1
369000150403
369100150403     c* FD non contattato ( o da ricontattare)
369200150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
369300150401     c                             olv82sts<>'1'
369400150323     c                   clear                   wcaldpcee
369500150323     c                   movel     ' '           DEEinDIST         1
369600150323     c                   eval      d98spcdee='FFD'
369700150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
369800150424     c                   eval      wpodpcee=wpoffi
369900150403
370000150403     c* FD contattato e     IN distinta
370100150601    2c                   when      d98cons='S'
370200150401     c                   eval      d98spcdee='FD2'
370300150401     c                   clear                   wcaldpcee
370400150401     c                   movel     ' '           DEEinDIST         1
370500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
370600150424     c                   eval      wpodpcee=wpoffi
370700150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
370800150427     c*  ritirare la merce
370900150427     C                   MOVEL     'C'           WTC                            *PARTENZA
371000150427     C                   MOVEL     'C'           WIC
371100150427     C                   Z-ADD     D98ddc        WDAT
371200150427     C                   EXSR      DATORE
371300150427     c                   z-add     wore          wr4poi
371400150427     c                   z-add     24            wr4noi
371500150427     C                   EXSR      RO4CDP
371600150427     c                   z-add     wr4poo        wore
371700150427     c                   exsr      OREDAT
371800150427     c                   movel     wdat          D98DTCTD
371900150601   x2c                   other
372000150403     c* FD contattato e NON in distinta
372100150331     c                   eval      d98spcdee='FD1'
372200150408     c                   eval      wpodpcee=wpoffi
372300150331     c                   clear                   wcaldpcee
372400150331     c                   movel     ' '           DEEinDIST         1
372500150601    2c                   endsl
372600150331     c
372700150331     c                   endif
372800150403     c
372900150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
373000150601    2c                   if        (d98tc1='A' or d98tc2='A' or
373100150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
373200150601     c                             and d98ffd= ' '
373300150325     c                   clear                   wcaldpcee
373400150325     c                   movel     ' '           DEEinDIST         1
373500150601    3c                   if        §B4LBAS= 'N'
373600150427     c                   eval      d98spcdee='AP3'
373700150403     c                   else
373800150403     c                   eval      d98spcdee='APP'
373900150601    3c                   endif
374000150601    2c                   endif
374100150325     c*   se la bolla ha il C/Assegno verifico se da contattare
374200150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
374300150325     c                   clear                   wcaldpcee
374400150325     c                   movel     ' '           DEEinDIST         1
374500150601    3c                   if        §B4LBAS= 'N'
374600150427     c                   eval      d98spcdee='CA3'
374700150403     c                   else
374800150403     c                   eval      d98spcdee='CAS'
374900150601    3c                   endif
375000150601    2c                   endif
375100150325     c* Per questi 3 casi devo impostare la data contatto destinatario
375200150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
375300150325     c                             d98spcdee='CAS'
375400150601    2c                   select
375500150601    2c                   when      d98dti>0 and wmancatraz= ' '
375600150325     c                   movel     d98dti        d98dtctd
375700150403     c* se piene le ore già portate ad un giorno lavorativo
375800150403     c                   if        wpodti>0
375900150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
376000150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
376100150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
376200150403     C                   Z-ADD     WPODTI        W10POI            9 0
376300150403     C                   EXSR      R10FES
376400150403     c                   z-add     w10poo        wore
376500150403     C                   EXSR      OREDAT
376600150403     C                   movel     WDAT          D98dtctd
376700150403     c                   endif
376800150403     c
376900150601    2c                   when      d98dam>0
377000150421     C                   Z-ADD     D98dam        WDAT
377100150421     C                   EXSR      DATORE
377200150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
377300150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
377400150421     C                   Z-ADD     Wore          W10POI            9 0
377500150421     C                   EXSR      R10FES
377600150421     c                   z-add     w10poo        wore
377700150421     C                   EXSR      OREDAT
377800150421     C                   movel     WDAT          D98dtctd
377900150601   x2c                   other
378000150325     c* data teorica di arrivo tenendo conto dell'IMP
378100160426     c                   if        d98nsp>0
378200150325     c                   exsr      letbrv
378300160426     c                   endif
378400150325     c                   if        idDATA>0
378500150325     c                   exsr      CalcoloIMP
378600150325     c                   z-add     wr1poi        WORE
378700150325     c                   else
378800150325     c                   z-add     wpoptc        WORE
378900150325     c                   endif
379000150325     C                   EXSR      OREDAT
379100150325     c                   movel     wdat          d98dtctd
379200150601    2c                   endsl
379300150325     c
379400150325     c                   endif
379500150325     c
379600150601    1c                   endsl
379700140820     c
379800150113     c* Se la bolla non è in distinta calcolo la data se previsto
379900150220     c                   if        wcaldpcee='S' and
380000150220     c                             (d98cons<>'S'  or (d98cons='S' and
380100150220     c                             evemic<>'SI'))
380200150219     c
380300150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
380400150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
380500150219     c                   if        d98nsp>0 and wtrasm<>'D' and
380600150219     c                             d98dam=0
380700150219     c                   exsr      letBRV
380800150219     c                   endif
380900150219     c
381000150217     c                   eval      d98spcdee='PRG'
381100150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
381200150320     c                             d98tc1='S' or d98tc2='S')
381300150320     c                   eval      d98spcdee='APP'
381400150320     c                   endif
381500150320     c
381600150113     c                   exsr      cal_DEE
381700150113     c                   endif
381800150113     c
381900140820     C     WPOdpcee      IFEQ      *ZEROS
382000931202     C                   Z-ADD     *ZEROS        D98DEE
382100931202     C                   Z-ADD     *ZEROS        D98OEE
382200930604     C                   ELSE
382300140820     C                   Z-ADD     WPOdpcee      WORE
382400141021     C******             SUB       12            WORE
382500930604     C                   EXSR      OREDAT
382600931202     C                   Z-ADD     WDAT          D98DEE
382700090903     c                   if        d98tcc<>10
382800090903     C                   ADD       12            WRES
382900090903     c                   else
383000090903     C                   ADD       10            WRES
383100090903     c                   endif
383200090903
383300931202     C     WRES          MULT      100           D98OEE
383400090903     c
383500090903     c                   if        d98oee=1000
383600090903     c                   eval      d98oee=1030
383700090903     c                   endif
383800140925     C                   END
383900150311     c
384000150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
384100150311     c                   select
384200150311     c                   when      d98spcdee='OLP'
384300140820     c
384400150220     c* Se bolla in distinta --> data prevista = data distinta
384500150311     c                   when      deeindist='S'  and
384600150113     c                             d98cons='S'
384700150220
384800150225     c*  non la propongo senza MIC e se presente disguido o IMP
384900150220     c                   if        d98dee>d98ddc and evemic<>'SI'
385000150220     c                             and IDdata>0
385100150220     c                   else
385200150220
385300140820     c                   eval      d98dee=d98ddc
385400140925     c                   clear                   d98oee
385500150320     c* se la data è > di oggi stato PRV se non è in IMP
385600150320     c                   if        d98dee>wdaodi
385700150320     c                   if        d98spcdee<>'IMP'
385800150311     c                   eval      d98spcdee='PRV'
385900150320     c                   endif
386000150320     c
386100150225     c                   else
386200150217     c* se c'è il MIC
386300150217     c                   if        evemic='SI'
386400150217     c                   eval      d98spcdee='DDC'
386500150225     c                   endif
386600150217     c                   endif
386700150220     c                   endif
386800150217     c
386900150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
387000150722     c                   eval      d98spcdee='PRV'
387100150311     c                   endsl
387200931202     C*
387300931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
387400931202     C     WPODCI        IFEQ      *ZEROS
387500931202     C                   Z-ADD     *ZEROS        D98DCI
387600931202     C                   Z-ADD     *ZEROS        D98HCI
387700931202     C                   ELSE
387800931202     C                   Z-ADD     WPODCI        WORE
387900931202     C                   EXSR      OREDAT
388000931202     C                   Z-ADD     WDAT          D98DCI
388100090902     c                   if        d98tci<>10
388200931202     C                   ADD       12            WRES
388300090902     c                   else
388400090902     C                   ADD       10            WRES
388500090902     c                   endif
388600090902
388700931202     C     WRES          MULT      100           D98HCI
388800090901     c
388900090903     c                   if        d98hci=1000
389000090901     c                   eval      d98hci=1030
389100090901     c                   endif
389200090901     c
389300931202     C                   END
389400931202     C*
389500931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
389600931202     C     WPODCE        IFEQ      *ZEROS
389700931202     C                   Z-ADD     *ZEROS        D98DCE
389800931202     C                   Z-ADD     *ZEROS        D98HCE
389900931202     C                   ELSE
390000931202     C                   Z-ADD     WPODCE        WORE
390100931202//// C**************       SUB  12        WORE
390200931202     C                   EXSR      OREDAT
390300931202     C                   Z-ADD     WDAT          D98DCE
390400090902     c                   if        d98tcc >10
390500931202     C                   ADD       12            WRES
390600090902     c                   else
390700090902     C                   ADD       10            WRES
390800090902     c                   endif
390900090902     c
391000931202     C     WRES          MULT      100           D98HCE
391100090902     c
391200090903     c                   if        d98hce=1000
391300090902     c                   eval      d98hce=1030
391400090902     c                   endif
391500090902     c
391600931202     C                   END
391700960229     C*
391800960229     C                   ENDIF                                                  <-- 1 ---
391900930604     C*
392000930604     C                   ENDSR
392100150218     C********************************************************************
392200150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
392300150218     C********************************************************************
392400150218     c     LetBRV        BEGSR
392500150219     c                   clear                   bollaOK           1
392600150219     c                   clear                   IDnpg
392700150219     c                   clear                   IDdata
392800150219     c                   clear                   IDora
392900150219     c                   clear                   IDfgs
393000150219     c
393100150218     C     Kart          SETLL     FNART01L
393200150218     C     Kart          READE     FNART01L
393300151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
393400150219     c                   clear                   DISfgs
393500150219     c                   clear                   DISdata
393600150219     c                   clear                   DISnpg
393700150219     c                   clear                   DISora
393800150219     c                   clear                   IMPdata
393900150219     c                   clear                   IMPfgs
394000150508     c* per ogni collo verifico se in IMP o disguidato
394100150218     c* al primo che NON trovo, esco come --> OK
394200150218     c     kbrv          setll     fnbrv07l
394300150218     c     kbrv          reade     fnbrv07l
394400150219    2c                   dow       not %eof(fnbrv07l)
394500150219    3c                   if        brvatr='D'  or brvnpg=3
394600150218     c* prendo la data della spunta
394700150218     c                   clear                   fnlv53ds
394800150219     c                   eval      d53tla='L'
394900150218     C                   MOVEL     BRvNPG        D53NPG
395000150218     C                   Z-ADD     BRvNFV        D53NFV
395100150218     C                   MOVEL     BRvFGS        D53FGS
395200150218     C                   CALL      'FNLV53R'
395300150218     C                   PARM                    fnlv53ds
395400150218    4c                   if        d53err<>*blanks
395500150219    5c                   if        brvdcs>0
395600150218     c                   eval      d53dfv=brvdcs
395700150218     c                   else
395800150219    6c                   if        brvdfs>0
395900150218     c                   eval      d53dfv=brvdfs
396000150219    6c                   endif
396100150219    5c                   endif
396200150219    4c                   endif
396300150218     c
396400150219    4c                   select
396500150219     c* se si tratta di DISGUIDO
396600150218     c                   when      brvatr='D'
396700150219     c
396800150219    5c                   if        d53dfv=DISdata
396900150219     c* Verifico l'ora
397000150219    6c                   if        brvhcs>0 and brvhcs>DISora
397100150219     c                   eval      DISdata=d53dfv
397200150219     c                   eval      DISfgs =brvfgs
397300150219     c                   eval      DISnpg =brvnpg
397400150219     c                   eval      DISora =brvhcs
397500150219   x6c                   else
397600150219    7c                   if        brvhms>0 and brvhms>DISora
397700150219     c                   eval      DISdata=d53dfv
397800150219     c                   eval      DISfgs =brvfgs
397900150219     c                   eval      DISnpg =brvnpg
398000150219     c                   eval      DISora =brvhms
398100150219    7c                   endif
398200150219    6c                   endif
398300150219    5c                   endif
398400150219     c
398500150219    5c                   if        d53dfv>DISdata
398600150219     c                   eval      DISdata=d53dfv
398700150219     c                   eval      DISfgs =brvfgs
398800150219     c                   eval      DISnpg =brvnpg
398900150219     c                   eval      DISora =brvhcs
399000150219     c                   if        disora=0
399100150219     c                   eval      DISora =brvhms
399200150219     c                   endif
399300150219    5c                   endif
399400150219     c
399500151009     c* se si tratta di IMP
399600151009    4c                   when      d53spg='P' and d53dfv>impdata
399700151009     c                   eval      IMPdata=d53dfv
399800151009     c                   eval      IMPfgs =brvfgs
399900150219    4c                   endsl
400000150219    3c                   endif
400100151009
400200150219     c     kbrv          reade     fnbrv07l
400300150219    2c                   enddo
400400150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
400500150219     c*  esco dal ciclo: la bolla è OK
400600150219    2c                   if        impdata=0 and disdata=0
400700150219     c                   eval      bollaOK='S'
400800150219     c                   clear                   IDdata
400900150219     c                   clear                   IDfgs
401000150219     c                   clear                   IDora
401100150219     c                   clear                   IDnpg
401200150219   x2c                   else
401300150219     c* memorizzo la data più alta
401400150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
401500150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
401600150219     c                   eval      IDdata=IMPdata
401700150219     c                   eval      IDfgs=IMPfgs
401800150219     c                   eval      IDora=0
401900150219     c                   eval      IDnpg='P'
402000150219    3c                   endif
402100150219    3c                   if        (DISdata>0 and DISdata=IDdata
402200150219     c                             and IDnpg<>'P' and DISora>IDora) or
402300150219     c                             (DISdata>0 and DISdata>IDdata)
402400150219     c                   eval      IDdata=DISdata
402500150219     c                   eval      IDfgs=DISfgs
402600150219     c                   eval      IDora=DISora
402700150219     c                   eval      IDnpg=%char(DISnpg)
402800150219    3c                   endif
402900150219    2c                   endif
403000150218     c
403100150218     C     Kart          READE     FNART01L
403200150219    1c                   enddo
403300150219     c
403400150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
403500150508     c**                 if        d98duc> 0 and d98duc>IDdata
403600150508     c**                 eval      bollaOK='S'
403700150508     c**                 clear                   IDdata
403800150508     c**                 clear                   IDfgs
403900150508     c**                 clear                   IDora
404000150508     c**                 clear                   IDnpg
404100150508     c**                 endif
404200150219     c
404300150218     c
404400150218     C                   ENDSR
404500140825     C********************************************************************
404600140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
404700140825     C********************************************************************
404800140825     c     TRAS_DEI      BEGSR
404900140825     C     WPODEI        IFEQ      *ZEROS
405000140825     C                   Z-ADD     *ZEROS        D98DEI
405100140825     C                   Z-ADD     *ZEROS        D98OEI
405200140825     C                   ELSE
405300140825     C                   Z-ADD     WPODEI        WORE
405400140825     C                   SUB       12            WORE
405500140825     C                   EXSR      OREDAT
405600140825     c
405700140825     C                   Z-ADD     WDAT          D98DEI
405800140825     c                   if        d98tci<>10
405900140825     C                   ADD       12            WRES
406000140825     c                   else
406100140825     C                   ADD       10            WRES
406200140825     c                   endif
406300140825     c
406400140825     C     WRES          MULT      100           D98OEI
406500140825     c
406600140825     c                   if        d98oei=1000
406700140825     c                   eval      d98oei=1030
406800140825     c                   endif
406900140825     c
407000140825     C                   END
407100140825     c                   ENDSR
407200930604     C********************************************************************
407300930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
407400930604     C********************************************************************
407500930604     C     CHKFOR        BEGSR
407600930604     C*
407700930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
407800930604     C* E QUINDI FORZO LA CONFORMITA' A SI
407900941227     C     D98ERR        IFEQ      '0'
408000930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
408100930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
408200930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
408300930604     C                   MOVEL     '1'           D98ERR
408400930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
408500930604     C                   END
408600930604     C                   END
408700990212     C*
408800990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
408900990212     C* E QUINDI FORZO LA CONFORMITA' A SI
409000990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
409100990212     C     D98ERR        IFEQ      '0'
409200990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
409300141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
409400990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
409500990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
409600990212     C                   MOVEL     '1'           D98ERR
409700990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
409800990212     C                   END
409900990212     C                   END
410000990308     C*
410100990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
410200990308     C* E QUINDI FORZO LA CONFORMITA' A SI
410300100614     C**   D98ERR        IFEQ      '0'
410400100614     C**   WFBR          IFEQ      'S'                                          *RESO
410500100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
410600100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
410700100614     C**                 MOVEL     '1'           D98ERR
410800100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
410900100614     C**                 END
411000100614     C**                 END
411100930604     C*
411200930604     C                   ENDSR
411300990212     C********************************************************************
411400990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
411500990212     C********************************************************************
411600990212     C     CHKDAN        BEGSR
411700990212     C*
411800990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
411900150603     c                   movel     'N'           opendan           1
412000040809      *
412100040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
412200040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
412300040809      * legate alla spedizione che passo
412400040809     c                   clear                   fidn12ds
412500040809     c                   eval      i12aas = D98AAS
412600040809     c                   eval      i12lnp = D98LNP
412700040809     c                   eval      i12nrs = D98NRS
412800040809     c                   eval      i12nsp = D98NSP
412900040809     c                   eval      i12fel = 'E'
413000040809     c                   eval      i12fan = 'E'
413100040809     c*
413200040809     c                   call      'FIDN12R'
413300040809     c                   parm                    fidn12ds
413400040809     c*
413500040809     c* se non ci sono errori
413600040809     c                   if        o12err = ' '
413700040809     c* se numero di CA trovate maggiore di zero
413800040809     c                   if        o12nca > 0
413900040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
414000150603     c
414100150603     c***                z-add     1             aa                2 0
414200150603     c***                dow       aa <= o12nca
414300150603     C* APERTA
414400150603     C***  skdch(aa)     IFEQ      0
414500150603     c***                movel     'S'           opendan
414600150603     c***                endif
414700150603     c*
414800150603     c***                add       1             aa
414900150603     c***                enddo
415000150603     c
415100040809     c                   endif
415200040809     c                   endif
415300990212     C*
415400990212     C                   ENDSR
415500941228     C********************************************************************
415600941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
415700941228     C********************************************************************
415800941228     C     CALORP        BEGSR
415900941228     C*
416000941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
416100941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
416200941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
416300960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
416400950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
416500950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
416600950110     C                   ELSE
416700950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
416800950110     C                   ENDIF
416900941228     C                   EXSR      RO2COP
417000941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
417100941228     C* SPED. IL GIORNO DOPO
417200941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
417300941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
417400941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
417500941228     C*
417600941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
417700941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
417800941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
417900941228     C                   Z-ADD     WPODSP        WR2POI
418000950110     C                   Z-ADD     WPOPPC        WR2POF
418100941228     C                   EXSR      RO2COP
418200941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
418300941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
418400941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
418500941228     C*
418600941228     C                   ENDSR
418700930414     C********************************************************************
418800930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
418900930414     C********************************************************************
419000930414     C     CALORT        BEGSR
419100930414     C*
419200930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
419300931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
419400930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
419500931126     C                   Z-ADD     *ZEROS        W98HF
419600950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
419700950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
419800950110     C                   ELSE
419900950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
420000950110     C                   ENDIF
420100931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
420200931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
420300930608     C                   ELSE
420400931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
420500930608     C                   END
420600931126     C                   EXSR      RO2COV
420700931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
420800930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
420900930414     C*
421000930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
421100930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
421200930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
421300931126     C                   Z-ADD     *ZEROS        W98HF
421400950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
421500931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
421600931126     C                   EXSR      RO2COV
421700931126     C                   Z-ADD     WR2NOV        W98RTC
421800930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
421900930414     C*
422000930414     C                   ENDSR
422100930414     C********************************************************************
422200930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
422300930414     C********************************************************************
422400930414     C     CALORC        BEGSR
422500930511     C*
422600930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
422700930511     C* I TEMPI DI RITARDO AD OGGI
422800930511     C     WPODCM        IFEQ      *ZEROS
422900960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
423000930511     C                   END
423100940117     C*
423200940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
423300940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
423400940117     C* ALLA DATA DI FINE CONSEGNABILITA'
423500940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
423600940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
423700940118££££ C     WPODFC        IFNE      *ZEROS
423800940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
423900940118££££ C                   END
424000930414     C*
424100930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
424200930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
424300930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
424400931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
424500931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
424600930608     C                   ELSE
424700931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
424800930608     C                   END
424900931126     C                   Z-ADD     WPODCM        WR5POF            9 0
425000931126     C                   EXSR      RO5COV
425100931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
425200930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
425300930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
425400060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
425500060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
425600930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
425700930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
425800930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
425900930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
426000930414     C*
426100930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
426200930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
426300930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
426400931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
426500931126     C                   Z-ADD     WPODCM        WR5POF            9 0
426600931126     C                   EXSR      RO5COV
426700931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
426800930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
426900930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
427000060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
427100060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
427200930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
427300930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
427400930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
427500930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
427600930414     C*
427700930414     C                   ENDSR
427800930414     C********************************************************************
427900930414     C*   CALORR - CALCOLA ORE REALI DI RESA
428000930414     C********************************************************************
428100930414     C     CALORR        BEGSR
428200930414     C*
428300950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
428400950110     C                   ADD       W98RTI        W98RRI
428500941228     C                   ADD       W98RCI        W98RRI
428600950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
428700950110     C                   ADD       W98RTC        W98RRC
428800941228     C                   ADD       W98RCC        W98RRC
428900930414     C*
429000930414     C                   ENDSR
429100930420     C********************************************************************
429200930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
429300930420     C********************************************************************
429400930420     C     CALDIF        BEGSR
429500930420     C*
429600941228     C* PARTENZA
429700941228     C     W98RPI        SUB       D98TPI        W98FPI
429800941228     C     W98RPC        SUB       D98TPC        W98FPC
429900941228     C*
430000930420     C* TRAZIONE
430100930423     C     W98RTI        SUB       D98TTI        W98FTI
430200930423     C     W98RTC        SUB       D98TTC        W98FTC
430300930420     C*
430400930420     C* CONSEGNA
430500090827     C***  W98RCI        SUB       D98TCI        W98FCI
430600090827     C     W98RCI        SUB       w99TCI        W98FCI
430700090827     C***  W98RCC        SUB       D98TCC        W98FCC
430800090827     C     W98RCC        SUB       w99TCC        W98FCC
430900930420     C*
431000930420     C* RESA
431100090921     C***  W98RRI        SUB       D98TRI        W98FRI
431200090921     C     W98RRI        SUB       w99TRI        W98FRI
431300090921     c
431400090921     C***  W98RRC        SUB       D98TRC        W98FRC
431500090921     C     W98RRC        SUB       w99TRC        W98FRC
431600930420     C*
431700930420     C                   ENDSR
431800930420     C********************************************************************
431900930420     C*   CALCNF - CALCOLA LE CONFORMITA'
432000930420     C********************************************************************
432100930420     C     CALCNF        BEGSR
432200941228     C*
432300941228     C*-------------------------------------------------------------------
432400941228     C* CONFORMITA' PARTENZA  INTERNA
432500941228     C*-------------------------------------------------------------------
432600941228     C                   Z-ADD     W98FPI        W98NPI
432700941228     C     W98FPI        IFLE      *ZEROS
432800941228     C                   MOVEL     'S'           D98CPI
432900941228     C                   ELSE
433000941228     C                   MOVEL     'N'           D98CPI
433100941228     C                   END
433200941228     C*
433300941228     C*-------------------------------------------------------------------
433400941228     C* CONFORMITA' PARTENZA  CLIENTE
433500941228     C*-------------------------------------------------------------------
433600941228     C                   Z-ADD     W98FPC        W98NPC
433700941228     C     W98FPC        IFLE      *ZEROS
433800941228     C                   MOVEL     'S'           D98CPC
433900941228     C                   ELSE
434000941228     C                   MOVEL     'N'           D98CPC
434100941228     C                   END
434200930420     C*
434300930420     C*-------------------------------------------------------------------
434400930420     C* CONFORMITA' TRAZIONE  INTERNA
434500930420     C*-------------------------------------------------------------------
434600930510     C                   Z-ADD     W98FTI        W98NTI
434700930423     C     W98FTI        IFLE      *ZEROS
434800930420     C                   MOVEL     'S'           D98CTI
434900930420     C                   ELSE
435000930420     C                   MOVEL     'N'           D98CTI
435100930420     C                   END
435200930420     C*
435300930420     C*-------------------------------------------------------------------
435400930420     C* CONFORMITA' TRAZIONE  CLIENTE
435500930420     C*-------------------------------------------------------------------
435600930510     C                   Z-ADD     W98FTC        W98NTC
435700930423     C     W98FTC        IFLE      *ZEROS
435800930420     C                   MOVEL     'S'           D98CTC
435900930420     C                   ELSE
436000930420     C                   MOVEL     'N'           D98CTC
436100930420     C                   END
436200930420     C*
436300930420     C*-------------------------------------------------------------------
436400930420     C* CONFORMITA' CONSEGNA  INTERNA
436500930420     C*-------------------------------------------------------------------
436600150226     c                   if        wmancatraz='S'
436700150226     c                   clear                   d98dti
436800150226     c                   clear                   d98hti
436900150226     c                   endif
437000150226     c
437100930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
437200930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
437300930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
437400050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
437500050914     c                   if        consideraora='S'
437600050914     c     w98fci        sub       24            w98nci
437700050914     c                   add       24            d98hrc
437800050914     c                   else
437900930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
438000050914     c                   endif
438100080116     c
438200110923     c                   select
438300080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
438400080116     c*  --> trasformo il ritardo in 0
438500090827     c                   when      d98cca='2'
438600080116     c                   z-add     0             w98nci
438700090827
438800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
438900080116     c*  e sto leggendo la bolla in arrivo
439000080116     c*  --> trasformo il ritardo = 0
439100090827     c                   when      d98cca='1' and d98tbo='A'
439200080116     c                   z-add     0             w98nci
439300090902     c
439400090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
439500090902     c                   when      wmancaTRAZ='S'
439600090902     c                   z-add     0             w98nci
439700090902     c                   clear                   d98dti
439800090902     c                   clear                   d98hti
439900110923     c
440000110923     c* Ignoro ritardo se pefect order
440100130221     c                   when      d98xco >*zeros
440200110923     c                   z-add     0             w98nci
440300090827     c
440400090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
440500091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
440600091014     c*  controllo ora soglia  consegna da tabella OSC)
440700091009     c                   when      d98tci=10   and
440800091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
440900091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
441000091014     c*
441100091014     c                             d98hci=1030
441200091014     C                   MOVEL     'OSC  '       WR3TAB
441300091014     C                   eval      WR3HHI=d98hmc
441400091014     C                   EXSR      RO3ORE
441500131218
441600091014     c                   if        wr3matpom='M' and d98hmc>1030
441700131218     c                   exsr      ctrLAVH1030
441800131218
441900131218     c                   if        wlav1030<>'S'
442000091014     c                   add       12            w98nci
442100091014     c                   endif
442200131218     c                   endif
442300090928     c
442400090827     c                   endsl
442500050914     c
442600930524     C                   ELSE                                                   <-- 1 -->
442700930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
442800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
442900930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
443000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
443100930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
443200930608     C                   ELSE
443300930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
443400930608     C                   END
443500930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
443600930525     C                   EXSR      RO2COV
443700930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
443800930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
443900930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
444000930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
444100931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
444200930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
444300930608     C                   ELSE
444400930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
444500930608     C                   END
444600931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
444700930525     C                   EXSR      RO2COV
444800930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
444900930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
445000930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
445100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
445200930525     C* E' IL RITARDO
445300930610     C     WNOCRI        SUB       W98RCI        W98NCI
445400050901     c
445500050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
445600050901     c*  --> trasformo il ritardo in anticipo
445700050901     c                   if        d98cca='2'
445800080116     c***                z-sub     w98nci        w98nci
445900080116     c                   z-add     0             w98nci
446000050901     c                   endif
446100111018     c* Ignoro ritardo se pefect order
446200130221     c                   if        d98xco >*zeros
446300111018     c                   z-add     0             w98nci
446400111018     c                   endif
446500061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
446600061121     c*  e sto leggendo la bolla in arrivo
446700061121     c*  --> trasformo il ritardo in anticipo
446800061121     c                   if        d98cca='1' and d98tbo='A'
446900080116     c***                z-sub     w98nci        w98nci
447000080116     c                   z-add     0             w98nci
447100061121     c                   endif
447200070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
447300070827     c*  mittente, --> consegnato in anticipo
447400070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
447500140403     c                   if        Callar5=' '
447600070827     c                   exsr      LeggiAR5
447700140403     c                   endif
447800070827     c
447900070827     c                   if        dcrpart<>'S'
448000070824     c                   z-sub     w98nci        w98nci
448100070824     c                   endif
448200070827     c
448300070824     c                   endif
448400050907     c
448500050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
448600050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
448700050907     c*  lo trasformo in anticipo perchè non sono in ritardo
448800050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
448900050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
449000050916     c***                if        d98dfc>0
449100070828     c                   if        d98dcm=0  and w98nci>0
449200050907     c                   z-sub     w98nci        w98nci
449300050907     c                   sub       oreILoDOPO    w98nci
449400050907     c                   endif
449500050901     c
449600930525     C                   ELSE                                                   <-- 2 -->
449700070521     c* non do ritardo se la consegna reale cade nel giorno
449800070521     c*  della consegna richiesta se presente IL
449900100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
450000070521     c                             wpodcm<=wpocr1
450100070521     c                   clear                   w98nci
450200100224     c                   else                                                   ---2a -->
450300930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
450400090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
450500090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
450600930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
450700930525     C* E' IL RITARDO O L'ANTICIPO
450800930525     C     W98RCI        SUB       WNOCRF        W98NCI
450900070115     c
451000070115     c* non do ritardo se comunque la consegna reale cade nel giorno
451100070115     c*  della consegna richiesta se presente IL
451200070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
451300070521     c***                          wpodcm<=wpocr1  and w98nci>0
451400070521     c***                clear                   w98nci
451500070521     c***                endif
451600070115     c
451700930525     C                   ELSE                                                   <-- 3 -->
451800930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
451900930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
452000930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
452100930525     C                   END                                                    <-- 3 ---
452200100224     C                   END                                                    <-- 2a---
452300100224
452400100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
452500100224     c                   select
452600100224     c                   when      d98cca='2'
452700100224     c                   z-add     0             w98nci
452800100224
452900100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
453000100224     c*  e sto leggendo la bolla in arrivo
453100100224     c                   when      d98cca='1' and d98tbo='A'
453200100224     c                   z-add     0             w98nci
453300100224     c                   endsl
453400100224     c
453500100224     C                   END                                                    <-- 2 ---
453600070521     C                   END                                                    <-- 1 ---
453700070521     c
453800970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
453900970915     C*/         WAZNCI    IFEQ 'S'
454000970915     C*/         W98NCI    ANDLT*ZEROS
454100970915     C*/                   Z-ADD*ZEROS    W98NCI
454200970915     C*/                   ENDIF
454300970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
454400050922     c* lo faccio solo se non c'e' la data cons richiesta
454500970915     C     D98DTI        IFEQ      *ZEROS
454600050922     c     d98dcr        andeq     *zeros
454700970915     C                   Z-ADD     *ZEROS        W98NCI
454800970915     C                   ENDIF
454900930524     C*
455000930524     C* CONTROLLO CONFORMITA
455100930423     C     W98NCI        IFLE      *ZEROS
455200930524     C                   MOVEL     'S'           D98CCI
455300930420     C                   ELSE
455400930524     C                   MOVEL     'N'           D98CCI
455500930420     C                   END
455600930420     C*
455700930420     C*-------------------------------------------------------------------
455800930420     C* CONFORMITA' CONSEGNA  CLIENTE
455900930420     C*-------------------------------------------------------------------
456000930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
456100930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
456200930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
456300050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
456400050914     c                   if        consideraora='S'
456500050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
456600050914     c                   else
456700050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
456800050914     c                   endif
456900080116     c
457000080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
457100080116     c*  --> trasformo il ritardo =0
457200090827     c                   select
457300090827     c                   when      d98cca='2'
457400080116     c                   z-add     0             w98ncc
457500090827
457600080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
457700080116     c*  e sto leggendo la bolla in arrivo
457800080116     c*  --> trasformo il ritardo =0
457900090827     c                   when      d98cca='1' and d98tbo='A'
458000080116     c                   z-add     0             w98ncc
458100090902     c
458200090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
458300090902     c                   when      wmancaTRAZ='S'
458400090902     c                   z-add     0             w98ncc
458500110923     c* Ignoro ritardo se Perfect Order
458600130221     c                   when      d98xco >*zeros
458700110923     c                   z-add     0             w98ncc
458800090902     c
458900090827     c
459000090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
459100090827     c* sono  in ritardo di 12 ore
459200091009     c                   when      d98tcc=10  and
459300091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
459400091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
459500091014     c*
459600091014     c                             d98hce=1030
459700091014     C                   MOVEL     'OSC  '       WR3TAB
459800091014     C                   eval      WR3HHI=d98hmc
459900091014     C                   EXSR      RO3ORE
460000091014     c                   if        wr3matpom='M' and d98hmc>1030
460100131218     c                   exsr      ctrLAVH1030
460200131218
460300131218     c                   if        wlav1030<>'S'
460400131218     c                   add       12            w98ncc
460500131218     c                   endif
460600131218     c****               add       12            w98ncc
460700091014     c                   endif
460800131218     c
460900091014     c***                z-add     12            w98ncc
461000090827     c                   endsl
461100050914     c
461200930524     C                   ELSE                                                   <-- 1 -->
461300930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
461400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
461500930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
461600930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
461700930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
461800930525     C                   EXSR      RO2COV
461900930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
462000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
462100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
462200930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
462300930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
462400931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
462500930525     C                   EXSR      RO2COV
462600930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
462700930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
462800930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
462900930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
463000930525     C* E' IL RITARDO
463100930525     C     WNOCRI        SUB       W98RCC        W98NCC
463200050901     c
463300050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
463400050901     c*  --> trasformo il ritardo in anticipo
463500050901     c                   if        d98cca='2'
463600080116     c***                z-sub     w98ncc        w98ncc
463700080116     c                   z-add     0             w98ncc
463800050901     c                   endif
463900111018     c* Ignoro ritardo se pefect order
464000130221     c                   if        d98xco >*zeros
464100111018     c                   z-add     0             w98ncc
464200111018     c                   endif
464300061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
464400061121     c*  e sto leggendo la bolla in arrivo
464500061121     c*  --> trasformo il ritardo in anticipo
464600061121     c                   if        d98cca='1' and d98tbo='A'
464700080116     c***                z-sub     w98ncc        w98ncc
464800080116     c                   z-add     0             w98ncc
464900061121     c                   endif
465000070824     c
465100070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
465200070827     c*  mittente, --> consegnato in anticipo
465300070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
465400070827     c* SBR per vedere se tassativa
465500070827     c                   if        Callar5=' '
465600070827     c                   EXSR      LeggiAR5
465700070827     c                   endif
465800070827     c                   if        dcrpart<>'S'
465900070824     c                   z-sub     w98ncc        w98ncc
466000070824     c                   endif
466100070824     c                   endif
466200050907     c
466300050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
466400050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
466500050907     c*  lo trasformo in anticipo perchè non sono in ritardo
466600050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
466700050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
466800050916     c***                if        d98dfc>0
466900070828     c                   if        d98dcm=0 and w98ncc>0
467000050907     c                   z-sub     w98ncc        w98ncc
467100050907     c                   sub       oreILoDOPO    w98ncc
467200050907     c                   endif
467300050901     c
467400930525     C                   ELSE                                                   <-- 2 -->
467500070521     c* Non do ritardo se la consegna reale cade nel giorno
467600070521     c*  della consegna richiesta se presente IL
467700100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
467800070521     c                             wpodcm<=wpocr2
467900070521     c                   clear                   w98ncc
468000100224     c                   else                                                   <--2a -->
468100070521     c
468200930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
468300090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
468400090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
468500930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
468600930525     C* E' IL RITARDO O L'ANTICIPO
468700930525     C     W98RCC        SUB       WNOCRF        W98NCC
468800070115     c
468900070115     c* non do ritardo se comunque la consegna reale cade nel giorno
469000070115     c*  della consegna richiesta se presente IL
469100070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
469200070521     c***                          wpodcm<=wpocr2 and w98ncc>0
469300070521     c***                clear                   w98ncc
469400070521     c***                endif
469500070115     c
469600930525     C                   ELSE                                                   <-- 3 -->
469700930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
469800930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
469900930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
470000930525     C                   END                                                    <-- 3 ---
470100100224     C                   END                                                    <-- 2a---
470200100224
470300100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
470400100224     c                   select
470500100224     c                   when      d98cca='2'
470600100224     c                   z-add     0             w98nci
470700100224
470800100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
470900100224     c*  e sto leggendo la bolla in arrivo
471000100224     c                   when      d98cca='1' and d98tbo='A'
471100100224     c                   z-add     0             w98nci
471200100224     c                   endsl
471300100224     c
471400100224     C                   END                                                    <-- 2 ---
471500070521     C                   END                                                    <-- 1 ---
471600930524     C*
471700930524     C* CONTROLLO CONFORMITA
471800930423     C     W98NCC        IFLE      *ZEROS
471900930524     C                   MOVEL     'S'           D98CCC
472000930420     C                   ELSE
472100930524     C                   MOVEL     'N'           D98CCC
472200930420     C                   END
472300930420     C*
472400930420     C*-------------------------------------------------------------------
472500930420     C* CONFORMITA' RESA INTERNA
472600930420     C*-------------------------------------------------------------------
472700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
472800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
472900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
473000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
473100050914     c                   if        consideraora='S'
473200050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
473300050914     c                   else
473400050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
473500050914     c                   endif
473600080116     c
473700080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
473800090827     c                   select
473900090827     c                   when      d98cca='2' and w98nri>0
474000080116     c                   z-add     0             w98nri
474100090827     c
474200080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
474300080116     c*  e sto leggendo la bolla in arrivo
474400090827     c                   when      d98cca='1' and d98tbo='A' and
474500080116     c                                            w98nri>0
474600080116     c                   z-add     0             w98nri
474700110923     c
474800110923     c* Ignoro ritardo se Perfect Order
474900130221     c                   when      d98xco >*zeros
475000110923     c                   z-add     0             w98nri
475100110923     c
475200090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
475300090827     c* sono  in ritardo di 12 ore
475400091009     c                   when      d98tci=10  and
475500091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
475600091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
475700091014     c*
475800091014     c                             d98hci=1030
475900091014     C                   MOVEL     'OSC  '       WR3TAB
476000091014     C                   eval      WR3HHI=d98hmc
476100091014     C                   EXSR      RO3ORE
476200091014     c                   if        wr3matpom='M' and d98hmc>1030
476300131218     c                   exsr      ctrLAVH1030
476400131218
476500131218     c                   if        wlav1030<>'S'
476600091014     c                   add       12            w98nri
476700091014     c                   endif
476800131218     c                   endif
476900091014     c
477000091014     c***                z-add     12            w98nri
477100090827     c                   endsl
477200090827
477300930524     C                   ELSE                                                   <-- 1 -->
477400930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
477500930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
477600930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
477700931206     C     WPODCM        ANDLE     WPOCR1
477800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
477900930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
478000930525     C                   Z-ADD     WPODCM        WR2POI            9 0
478100931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
478200930525     C                   EXSR      RO2COV
478300930525     C     WR2NOV        IFNE      *ZEROS
478400930525     C     WR2NOV        MULT      -1            W98NRI
478500930525     C                   END
478600930524     C                   ELSE                                                   <-- 2 -->
478700930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
478800930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
478900930524     C     WPODCM        IFLT      WPOCRI
479000930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
479100930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
479200930524     C                   ELSE
479300931206     C                   Z-ADD     WPOCR1        WPORFI
479400930524     C                   Z-ADD     WPODCM        WPORFF
479500930524     C                   END
479600930524     C* CALCOLO ORE DI DIFFERENZA
479700930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
479800930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
479900930524     C                   Z-ADD     WPORFI        WR2POI            9 0
480000930524     C                   Z-ADD     WPORFF        WR2POF            9 0
480100930524     C                   EXSR      RO2COV
480200930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
480300050902     c
480400050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
480500100224     c                   if        d98cca='2' and w98nri>0
480600100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
480700080116     c***                z-sub     w98nri        w98nri
480800080116     c                   z-add     0             w98nri
480900050902     c                   endif
481000111018     c* Ignoro ritardo se pefect order
481100130221     c                   if        d98xco >*zeros
481200111018     c                   z-add     0             w98nri
481300111018     c                   endif
481400061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
481500061121     c*  e sto leggendo la bolla in arrivo
481600100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
481700100224     c***                if        d98cca='1' and d98tbo='A' and
481800100224     c***                                         w98nri>0 and wpodcm<wpocri
481900080116     c***                z-sub     w98nri        w98nri
482000080116     c                   z-add     0             w98nri
482100061121     c                   endif
482200070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
482300070827     c*  mittente, --> consegnato in anticipo
482400070828     c                   if        d98tcr=' ' and d98nsp>0  and
482500070828     c                                            w98nri>0 and wpodcm<wpocri
482600070827     c                   if        CallAR5=' '
482700070827     c                   EXSR      LeggiAR5
482800070824     c                   endif
482900070827     c                   if        dcrpart<>'S'
483000070824     c                   z-sub     w98nri        w98nri
483100070824     c                   endif
483200070824     c                   endif
483300050907     c
483400050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
483500050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
483600050907     c*  lo trasformo in anticipo perchè non sono in ritardo
483700050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
483800050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
483900050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
484000050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
484100050907     c                   z-sub     w98nri        w98nri
484200050907     c                   sub       oreILoDOPO    w98nri
484300050907     c                   endif
484400050907     c
484500050907     C                   END                                                    <-- 2 ---
484600930524     C                   END                                                    <-- 1 ---
484700930420     C* CALCOLO LE ORE DI RESA
484800930420     C* CONTROLLO CONFORMITA
484900930423     C     W98NRI        IFLE      *ZEROS
485000930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
485100930420     C                   ELSE
485200930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
485300930420     C                   END
485400930420     C*
485500930420     C*-------------------------------------------------------------------
485600930420     C* CONFORMITA' RESA CLIENTE
485700930420     C*-------------------------------------------------------------------
485800930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
485900930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
486000930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
486100050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
486200050914     c                   if        consideraora='S'
486300050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
486400050914     c                   else
486500050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
486600050914     c                   endif
486700090827     c
486800080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
486900090827     c                   select
487000090827     c                   when      d98cca='2' and w98nrc>0
487100080116     c                   z-add     0             w98nrc
487200090827
487300080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
487400080116     c*  e sto leggendo la bolla in arrivo
487500090827     c                   when      d98cca='1' and d98tbo='A' and
487600080116     c                                            w98nrc>0
487700080116     c                   z-add     0             w98nrc
487800110923     c
487900110923     c* Ignoro ritardo se Perfect Order
488000130221     c                   when      d98xco >*zeros
488100110923     c                   z-add     0             w98nrc
488200090827     c
488300090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
488400090827     c* sono  in ritardo di 12 ore
488500091009     c                   when      d98tcc=10  and
488600091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
488700091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
488800091014     c                             d98hce=1030
488900091014     C                   MOVEL     'OSC  '       WR3TAB
489000091014     C                   eval      WR3HHI=d98hmc
489100091014     C                   EXSR      RO3ORE
489200091014     c                   if        wr3matpom='M' and d98hmc>1030
489300131218     c                   exsr      ctrLAVH1030
489400131218
489500131218     c                   if        wlav1030<>'S'
489600091014     c                   add       12            w98nrc
489700091014     c                   endif
489800131218     c                   endif
489900091014     c
490000091014     c**                 z-add     12            w98nrc
490100090827     c                   endsl
490200090827     c
490300930524     C                   ELSE                                                   <-- 1 -->
490400930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
490500930524     C* DI DIFFERENZA SONO UGUALI A ZERO
490600930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
490700931206     C     WPODCM        ANDLE     WPOCR2
490800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
490900930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
491000930525     C                   Z-ADD     WPODCM        WR2POI            9 0
491100931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
491200930525     C                   EXSR      RO2COV
491300930525     C     WR2NOV        IFNE      *ZEROS
491400930525     C     WR2NOV        MULT      -1            W98NRC
491500930525     C                   END
491600930524     C                   ELSE                                                   <-- 2 -->
491700930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
491800930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
491900930524     C     WPODCM        IFLT      WPOCRI
492000930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
492100930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
492200930524     C                   ELSE
492300931206     C                   Z-ADD     WPOCR2        WPORFI
492400930524     C                   Z-ADD     WPODCM        WPORFF
492500930524     C                   END
492600930524     C* CALCOLO ORE DI DIFFERENZA
492700930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
492800930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
492900930524     C                   Z-ADD     WPORFI        WR2POI            9 0
493000930524     C                   Z-ADD     WPORFF        WR2POF            9 0
493100930524     C                   EXSR      RO2COV
493200930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
493300050902     c
493400050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
493500100224     c                   if        d98cca='2' and w98nrc>0
493600100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
493700080116     c***                z-sub     w98nrc        w98nrc
493800080116     c                   z-add     0             w98nrc
493900050902     c                   endif
494000111018     c* Ignoro ritardo se pefect order
494100130221     c                   if        d98xco >*zeros
494200111018     c                   z-add     0             w98nrc
494300111018     c                   endif
494400070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
494500061121     c*  e sto leggendo la bolla in arrivo
494600100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
494700100224     c***                if        d98cca='1' and d98tbo='A' and
494800100224     c***                                         w98nrc>0 and wpodcm<wpocri
494900080116     c***                z-sub     w98nrc        w98nrc
495000080116     c                   z-add     0             w98nrc
495100061121     c                   endif
495200070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
495300070827     c*  mittente, --> consegnato in anticipo
495400070828     c                   if        d98tcr=' ' and d98nsp>0 and
495500070828     c                                            w98nrc>0 and wpodcm<wpocri
495600070827     c                   if        CallAR5=' '
495700070827     c                   EXSR      LeggiAR5
495800070827     c                   endif
495900070827     c                   if        dcrpart<>'S'
496000070824     c                   z-sub     w98nrc        w98nrc
496100070824     c                   endif
496200070824     c                   endif
496300050907     c
496400050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
496500050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
496600050907     c*  lo trasformo in anticipo perchè non sono in ritardo
496700050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
496800050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
496900050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
497000050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
497100050907     c                   z-sub     w98nrc        w98nrc
497200050907     c                   sub       oreILoDOPO    w98nrc
497300050907     c                   endif
497400050907     c
497500050907     C                   END                                                    <-- 2 ---
497600930524     C                   END                                                    <-- 1 ---
497700930420     C* CALCOLO LE ORE DI RESA
497800930420     C* CONTROLLO CONFORMITA
497900930423     C     W98NRC        IFLE      *ZEROS
498000930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
498100930420     C                   ELSE
498200930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
498300930420     C                   END
498400930420     C*
498500930420     C                   ENDSR
498600070827     c
498700131218     c********************************************************************
498800131218     c     CtrlavH1030   BEGSR
498900131218     c********************************************************************
499000131218     c                   clear                   wlav1030          1
499100140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
499200131218     c                   z-add     1             xx                4 0
499300131218     c                   dow       evc(xx)<>*blanks
499400140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
499500140127     c                   if        evd(xx)=d98dcm
499600131218     c                   if        evo(xx)<=1030
499700131218     c                   eval      wlav1030='S'
499800131218     c                   leave
499900131218     c                   endif
500000131218     c                   endif
500100131218     c                   add       1             xx
500200131218     c                   enddo
500300131218     c                   ENDSR
500400070827     c********************************************************************
500500070827     c* Controllo record GEN di FIAR5
500600070827     c********************************************************************
500700070827     c     LeggiAR5      BEGSR
500800070827     c                   clear                   DCRPART           1
500900141104     c                   clear                   dar5gen
501000141104     c                   clear                   gc1PART           2
501100141104     c                   clear                   gc2PART           2
501200070827     c     kar5          chain     fiar501l
501300070827    1c                   if        not %found
501400070827     c                   eval      dcrpart='S'
501500141104     c                   eval      gc1part=d98gc1
501600141104     c                   eval      gc2part=d98gc2
501700070827   x1c                   else
501800070827     c* i dati devono essere quelli immessi dalla partenza
501900070827     c                   movel     ar5uni        dar5gen
502000070827     c
502100070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
502200070827     c*  ma il record GEN è stato scritto per altri motivi
502300070827    2c                   if        §ar5mdp='S'
502400070827     c                   movel     d98dcr        alfadcr           8
502500141104     c                   eval      gc1part=§ar5gc1
502600141104     c                   eval      gc2part=§ar5gc2
502700070827     c
502800070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
502900070827     c                   eval      dcrpart='S'
503000070827    3c                   endif
503100070827     c
503200070827   x2c                   else
503300070827     c                   eval      dcrpart='S'
503400141106     c                   eval      gc1part=d98gc1
503500141106     c                   eval      gc2part=d98gc2
503600070827    2c                   endif
503700070827    1c                   endif
503800070827     c
503900140402     c* Memorizzo il flag pickup 2
504000140402     c                   eval      wpick2=§AR5PK2
504100140402     c
504200150401     c*
504300150414     c**                 if        d98cas>0 or d98ffd<>' ' or
504400150414     c**                           d98tc1='A'  or d98tc2='A' or
504500150414     c**                           d98tc1='S'  or d98tc2='S'
504600160331     c                   eval      kar4trc='L'
504700150401     c     kar4          chain     fiar401l
504800160420    1c                   if        %found(fiar401l)
504900150401     c                   eval      dsbl4L=ar4not
505000160420   x1c                   else
505100160420    1c                   endif
505200160420
505300150401     c* Solo per fermo deposito verifico se inviato alert
505400150401     c                   if        d98ffd<>' '
505500150401     c                   clear                   fnlv82ds
505600150401     c                   eval      ILV82AAS=d98aas
505700150401     c                   eval      ILV82LNP=d98lnp
505800150401     c                   eval      ILV82NRS=d98nrs
505900150401     c                   eval      ILV82NSP=d98nsp
506000150401     c                   eval      ILV82TIPA='FD'
506100150401     c                   call      'FNLV82R'
506200150401     c                   parm                    fnlv82ds
506300150401     c                   endif
506400150414     c**                 endif
506500150401
506600150401     c                   eval      callar5='S'
506700160331     c
506800160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
506900160331     c                   if        %lookup(d98lnp:fildpd)>0
507000160331     c                   eval      kar4trc='I'
507100160331     c     kar4          chain     fiar401l
507200160331     c                   if        %found(fiar401l)
507300160331     c                   eval      dsbl4I=ar4not
507400160331     c                   endif
507500160401     c                   if        §b4trd='1' or §b4trd='2'
507600160401     c                   eval      wVP='S'
507700160401     c                   endif
507800160401     c
507900160331     c                   endif
508000150401
508100070827     c                   ENDSR
508200930913     C********************************************************************
508300930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
508400930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
508500930913     C*            DATA/ORA (FINE RANGE)
508600930913     C********************************************************************
508700930913     C     CALCNS        BEGSR
508800930914     C*
508900930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
509000930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
509100930914     C     WPODTI        IFEQ      *ZEROS
509200960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
509300930914     C                   ELSE
509400930914     C                   Z-ADD     WPODTI        WPOIRC
509500930914     C                   END
509600930914     C*
509700930914     C* SE NON ANCORA CONSEGNATA
509800930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
509900050922     c* in realtà nella data consegna c'e' la data fine calcolo
510000050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
510100930914     C     WPODCM        IFEQ      *ZEROS
510200050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
510300930914     C                   ELSE
510400930916     C                   Z-ADD     WPODCM        WPOFRC
510500930914     C                   END
510600930914     C*
510700930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
510800930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
510900930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
511000930914     C     WPODFC        ANDLT     WPOIRC
511100930914     C                   ELSE
511200930914     C*
511300930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
511400930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
511500930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
511600930914     C     WPODFC        ANDGT     WPOFRC
511700930914     C                   ELSE
511800930916     C*
511900930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
512000930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
512100930916     C     WPODFC        ANDGT     WPOFRC
512200930916     C                   ELSE
512300930916     C*
512400930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
512500930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
512600930916     C     WPODFC        ANDLT     WPOIRC
512700930916     C                   ELSE
512800930914     C*
512900930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
513000930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
513100930914     C     WPODIC        IFNE      *ZEROS
513200930914     C     WPODIC        ANDGT     WPOFRC
513300930914     C                   Z-ADD     WPOFRC        WPODIC
513400930914     C                   ENDIF
513500930914     C*
513600930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
513700930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
513800930914     C     WPODFC        IFGT      WPOFRC
513900930914     C                   Z-ADD     WPOFRC        WPODFC
514000930914     C                   ENDIF
514100930914     C*
514200930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
514300930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
514400930914     C     WPODIC        IFNE      *ZEROS
514500930914     C     WPODIC        ANDLT     WPOIRC
514600930914     C                   Z-ADD     WPOIRC        WPODIC
514700930914     C                   ENDIF
514800930914     C*
514900930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
515000930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
515100930914     C     WPODFC        IFLT      WPOIRC
515200930914     C                   Z-ADD     WPOIRC        WPODFC
515300930914     C                   ENDIF
515400930914     C*
515500930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
515600931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
515700930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
515800930916     C*
515900931210     C***********WPODIC    IFNE *ZEROS
516000931210     C***********WPODFC    ANDNE*ZEROS
516100931210     C*********************ADD  12        WPODFC
516200931210     C*********************ENDIF
516300930914     C     WPODIC        IFEQ      *ZEROS
516400930914     C     WPODFC        ANDNE     *ZEROS
516500931130     C                   Z-ADD     WPODFC        WPODIC
516600931210     C*********************ADD  12        WPODFC
516700930916     C                   ENDIF
516800930913     C*
516900930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
517000930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
517100930914     C* INTERNA
517200050908     c                   eval      Usacons='S'
517300930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
517400930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
517500931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
517600931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
517700940324     C                   EXSR      RO9COV
517800931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
517900930914     C* CLIENTE
518000930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
518100931210     C                   EXSR      RO9COV
518200931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
518300050908     c                   eval      Usacons=' '
518400930914     C*
518500930916     C                   ENDIF                                                  <-- 5 ---
518600930916     C                   ENDIF                                                  <-- 4 ---
518700930916     C                   ENDIF                                                  <-- 3 ---
518800930914     C                   ENDIF                                                  <-- 2 ---
518900930914     C                   ENDIF                                                  <-- 1 ---
519000930913     C*
519100930913     C                   ENDSR
519200930325     C********************************************************************
519300930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
519400930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
519500930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
519600930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
519700930325     C********************************************************************
519800930325     C     RO1CDP        BEGSR
519900930414     C*
520000931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
520100931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
520200931126     C*
520300931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
520400931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
520500150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
520600150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
520700931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
520800060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
520900930326     C*
521000930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
521100931130     C                   Z-ADD     WR1POI        WR1POO            9 0
521200930325     C*
521300930325     C* CICLO FINO A CHE:
521400930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
521500930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
521600930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
521700930406     C     WR1POO        ANDLT     WPOLIM
521800930414     C*
521900930407     C                   Z-ADD     WR1POO        WPOROU            9 0
522000930325     C                   MOVEL     'N'           WSLITT            1
522100930414     C*
522200930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
522300060420     C*
522400931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
522500150605     c     wdee          andeq     ' '
522600930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
522700930414     C                   END
522800930511     C*
522900931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
523000150608     c     wdee          andeq     ' '
523100930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
523200930511     C                   END
523300930414     C*
523400930325     C     WSLITT        IFEQ      'N'
523500930423     C                   SUB       12            WR1NOI            5 0
523600930325     C                   END
523700931130     C                   ADD       12            WR1POO            9 0
523800930414     C*
523900931223     C                   ENDDO                                                  <-- 1 ---
524000931126     C*
524100931126     C                   ELSE                                                   <-- 0 -->
524200931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
524300931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
524400931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
524500960129     C     WPOLAF        IFGT      WPODCG
524600960129     C                   Z-ADD     WPOLAF        WTM090            9 0
524700960129     C                   ELSE
524800960129     C                   Z-ADD     WPODCG        WTM090
524900960129     C                   ENDIF
525000960129     C     WPOGIF        IFGT      WTM090
525100960129     C                   Z-ADD     WPOGIF        WTM090
525200960129     C                   ENDIF
525300060420     C     WPOpaif       IFGT      WTM090
525400060420     C                   Z-ADD     WPOpaif       WTM090
525500060420     C                   ENDIF
525600960129     C*
525700960129     C     12            ADD       WTM090        WR1POO
525800960129     C                   ENDIF                                                  <-- 0 ---
525900930512     C*
526000931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
526100931202     C     WR1POO        SUB       12            WR7POI
526200931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
526300931202     C                   ADD       12            WR7POO
526400931202     C     WR7POO        IFGT      WR1POO
526500931202     C                   Z-ADD     WR7POO        WR1POO
526600931202     C                   END
526700930406     C*
526800930325     C                   ENDSR
526900930414     C********************************************************************
527000930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
527100930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
527200930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
527300930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
527400930414     C********************************************************************
527500930414     C     RO2COV        BEGSR
527600091014     c                   clear                   salvaEveL
527700091014     c                   clear                   salvaEveG
527800931130     C*
527900930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
528000930414     C                   Z-ADD     *ZEROS        WR2NOV
528100930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
528200931130     C                   Z-ADD     WR2POI        WR2POW
528300930414     C*
528400930414     C* CICLO FINO A CHE:
528500930414     C*   - DATA INIZIO SUPERATO DATA FINE
528600930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
528700931223     C     WR2POW        ANDLT     WPOLIM
528800930414     C*
528900930414     C                   Z-ADD     WR2POW        WPOROU
529000930414     C                   MOVEL     'N'           WSLITT            1
529100930414     C*
529200930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
529300930414     C*
529400930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
529500930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
529600930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
529700930414     C                   END
529800930414     C*
529900930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
530000930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
530100930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
530200930618     C                   END
530300060420     c
530400060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
530500060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
530600060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
530700060420     C                   END
530800060420     c
530900931201     C*
531000931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
531100931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
531200931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
531300931201     C                   END
531400060418     c
531500060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
531600060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
531700060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
531800060418     C                   END
531900930414     C*
532000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
532100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
532200930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
532300930414     C                   END
532400930511     C*
532500930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
532600930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
532700930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
532800930511     C                   END
532900930414     C*
533000930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
533100930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
533200930414     C     WSLITT        IFEQ      'N'
533300930423     C                   ADD       12            WR2NOV            5 0
533400930414     C                   END
533500931130     C                   ADD       12            WR2POW            9 0
533600930414     C*
533700931223     C                   ENDDO                                                  <-- 1 ---
533800930414     C*
533900930414     C                   ENDSR
534000941228     C********************************************************************
534100941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
534200941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
534300941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
534400941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
534500941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
534600941228     C********************************************************************
534700941228     C     RO2COP        BEGSR
534800941228     C*
534900941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
535000941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
535100941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
535200941228     C                   Z-ADD     *ZEROS        WR2NOV
535300941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
535400941228     C                   Z-ADD     WR2POI        WR2POW
535500941228     C*
535600941228     C* CICLO FINO A CHE:
535700941228     C*   - DATA INIZIO SUPERATO DATA FINE
535800941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
535900941228     C*
536000950215     C                   Z-ADD     WR2POW        WPOROU
536100941228     C                   MOVEL     'N'           WSLITT
536200941228     C* TRASFORMA IL PROGR. ORE IN DATA
536300950215     C*******              Z-ADDWR2POW    WORE
536400950215     C*******              EXSR OREDAT
536500950215     C*******              Z-ADDWDAT      W12DAT  80
536600950215     C*******              Z-ADDWRES      W12RES  30
536700941228     C*
536800941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
536900941228     C*
537000941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
537100941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
537200941228     C     WSLITT        IFEQ      'N'
537300941228     C                   ADD       12            WR2NOV            5 0
537400941228     C                   END
537500941228     C                   ADD       12            WR2POW            9 0
537600941228     C*
537700941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
537800941228     C* ENTRI IN LOOP
537900941228     C     WR2NOV        IFGT      999
538000941228     C                   LEAVE                                                  *VA A ENDDO
538100941228     C                   END
538200941228     C*
538300941228     C                   ENDDO                                                  <-- 1 ---
538400941228     C*
538500941228     C                   ENDSR
538600930510     C********************************************************************
538700930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
538800930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
538900930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
539000930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
539100050905     c*                     il num ore deve essere un multiplo di 12
539200050905     c*                     oppure 1 ora
539300930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
539400930510     C********************************************************************
539500930510     C     RO4CDP        BEGSR
539600930510     C*
539700930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
539800930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
539900050905     c                   movel     '1'           wgiro             1
540000930510     C*
540100930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
540200930510     C                   Z-ADD     WR4POI        WR4POO            9 0
540300930510     C*
540400930510     C* CICLO FINO A CHE:
540500930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
540600930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
540700930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
540800930510     C     WR4POO        ANDLT     WPOLIM
540900930510     C*
541000141106     c                   if        nosomma<>'N'
541100931130     C                   ADD       12            WR4POO            9 0
541200141106     c                   endif
541300930510     C                   Z-ADD     WR4POO        WPOROU            9 0
541400930510     C                   MOVEL     'N'           WSLITT            1
541500930510     C*
541600930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
541700050905     c
541800050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
541900930510     C     WSLITT        IFEQ      'N'
542000050905     c
542100050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
542200050905     c*  se cado in giorno non festivo
542300050905     c                   if        wr4noi=1 and wgiro='1'
542400050905     c                   sub       12            wr4poo
542500050905     c                   add       wr4noi        wr4poo
542600050905     c                   endif
542700050905     c*
542800050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
542900050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
543000050905     c*  calcolate dal pgm
543100050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
543200050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
543300050905     c*  del giorno sucessivo non festivo
543400050905     c                   if        wr4noi=1 and wgiro<>'1'
543500050905     c                   add       wr4noi        wr4poo
543600050905     c                   endif
543700050905     c
543800930510     C                   SUB       12            WR4NOI            5 0
543900930510     C                   END
544000141106     c
544100141106     c                   if        nosomma= 'N'
544200141106     C                   ADD       12            WR4POO            9 0
544300141106     c                   endif
544400930510     C*
544500050905     c                   eval      wgiro='2'
544600931223     C                   ENDDO                                                  <-- 1 ---
544700930510     C*
544800930510     C                   ENDSR
544900931130     C********************************************************************
545000931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
545100931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
545200931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
545300931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
545400931130     C********************************************************************
545500931130     C     RO5COV        BEGSR
545600931130     C*
545700931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
545800060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
545900060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
546000060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
546100931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
546200931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
546300931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
546400931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
546500931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
546600931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
546700931130     C                   Z-ADD     *ZEROS        WR5NOV
546800940714     C*
546900940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
547000940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
547100931207     C*
547200931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
547300940908     C     WIC           IFEQ      'I'
547400090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
547500090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
547600931207     C                   ELSE
547700090827     C*****              Z-ADD     D98TCC        WPOTCT
547800090827     C                   Z-ADD     w99TCC        WPOTCT
547900931207     C                   END
548000931130     C*
548100931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
548200060420     C                   clear                   COV
548300931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
548400931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
548500931207     C                   Z-ADD     2             CO                3 0
548600931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
548700931207     C                   EXSR      RO5SUB
548800981102     C                   DO        99            IC                3 0
548900931207     C     LAF(IC)       IFNE      *ZEROS
549000931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
549100931207     C                   EXSR      RO5SUB
549200931207     C                   ENDIF
549300931207     C                   ENDDO
549400060420     c
549500060420     C                   DO        99            IC                3 0
549600060420     C     paif(IC)      IFNE      *ZEROS
549700060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
549800060420     C                   EXSR      RO5SUB
549900060420     C                   ENDIF
550000060420     C                   ENDDO
550100931207     C* ORDINA LA SCHIERA COV
550200931207     C                   SORTA     COV
550300931130     C*
550400931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
550500931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
550600981102     C                   DO        98            IC                             --- 1 -->
550700931207     C     IC            ADD       1             I1C               3 0
550800931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
550900931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
551000931207     C     COV(I1C)      OREQ      *ZEROS
551100931207     C                   ELSE
551200931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
551300931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
551400931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
551500931130     C                   EXSR      RO2COV
551600931207     C                   ADD       WR2NOV        WR5NOV            9 0
551700931209     C* SE NON SONO IN DATA CONSEGNA
551800931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
551900931207     C     WR5NOV        IFLT      WPOTCT
552000931209     C     COV(I1C)      ANDNE     WPODCM
5521009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5522009709111009 C     WPOTCT        SUB       12            WR5NOV
552300931130     C                   END
552400931207     C                   ENDIF                                                  <-- 2 ---
552500931207     C                   ENDDO                                                  <-- 1 ---
552600940714     C                   ENDIF                                                  <-- 0 ---
552700931130     C*
552800931130     C                   ENDSR
552900931207     C********************************************************************
553000931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
553100931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
553200931207     C********************************************************************
553300931207     C     RO5SUB        BEGSR
553400931207     C*
553500931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
553600931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
553700931207     C     WR5COV        ANDLT     WR5POF
553800931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
553900931207     C                   MOVEL     'N'           WR5PRE            1
554000981102     C*---                 DO   30        I
554100981102     C                   DO        99            I
554200931207     C     WR5COV        IFEQ      COV(I)
554300931207     C                   MOVEL     'S'           WR5PRE
554400931207     C                   LEAVE
554500931207     C                   ENDIF
554600931207     C                   ENDDO
554700931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
554800931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
554900931207     C                   ADD       1             CO                3 0
555000931207     C                   Z-ADD     WR5COV        COV(CO)
555100931207     C                   ENDIF                                                  <-- 2 ---
555200931207     C                   ENDIF                                                  <-- 1 ---
555300931207     C*
555400931207     C                   ENDSR
555500931201     C********************************************************************
555600060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
555700931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
555800931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
555900931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
556000931201     C********************************************************************
556100931201     C     RO6CDT        BEGSR
556200931201     C*
556300931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
556400931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
556500931201     C*
556600931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
556700931201     C                   Z-ADD     WR6POI        WR6POO            9 0
556800931201     C*
556900931201     C* CICLO FINO A CHE:
557000931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
557100931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
557200931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
557300931201     C     WR6POO        ANDLT     WPOLIM
557400931201     C*
557500931201     C                   Z-ADD     WR6POO        WPOROU            9 0
557600931201     C                   MOVEL     'N'           WSLITT            1
557700931201     C*
557800931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
557900931201     C*
558000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
558100150605     c     wdee          andeq     ' '
558200931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
558300931201     C                   END
558400931201     C*
558500931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
558600150608     c     wdee          andeq     ' '
558700931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
558800931201     C                   END
558900931201     C*
559000931201     C     WSLITT        IFEQ      'N'
559100931201     C                   SUB       12            WR6NOI            5 0
559200931201     C                   END
559300931201     C                   ADD       12            WR6POO            9 0
559400931201     C*
559500931223     C                   ENDDO                                                  <-- 1 ---
559600931201     C*
559700931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
559800931202     C     WR6POO        SUB       12            WR7POI
559900931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
560000931207     C                   ADD       12            WR7POO
560100931202     C     WR7POO        IFGT      WR6POO
560200931201     C                   Z-ADD     WR7POO        WR6POO
560300931202     C                   END
560400931201     C*
560500931201     C                   ENDSR
560600931201     C********************************************************************
560700060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
560800060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
560900060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
561000060407     C*            INPUT:  -WR7POI (PROGR.ORE)
561100931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
561200931201     C********************************************************************
561300931201     C     RO7TCF        BEGSR
561400060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
561500060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
561600060407     C                   MOVEL     'N'           WSLITT
561700931201     C*
561800931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
561900931201     C                   Z-ADD     WR7POI        WR7POO            9 0
562000931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
562100931201     C* COINCIDA CON UN TURNO DI CHIUSURA
562200060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
562300931201     C     D98GC2        ANDEQ     *BLANKS
562400060407   x1C                   ELSE                                                   <-- 1 -->
562500931214     C*
562600931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
562700931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
562800931214     C*
562900931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
563000931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
563100931223     C     WPOROU        ANDLT     WPOLIM
563200931214     C* MEMORIZZO INIZIO E FINE RANGE
563300931202     C     WPOTI         IFEQ      *ZEROS
563400931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
563500931202     C                   END
563600931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
563700931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
563800931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
563900931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
564000931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
564100931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
564200931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
564300931201     C                   MOVEL     'N'           WSLITT
564400931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
564500060407     c* Se non trovat, controllo se c'e' evento part che giustifica
564600060407     c                   if        wslitt='N'
564700060407     c                   exsr      datcepg
564800060407     c                   endif
564900060407     c
565000931201     C                   ENDDO                                                  <-- 2 ---
565100931214     C*
565200931201     C                   Z-ADD     WPOROU        WR7POO
565300931214     C*
565400931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
565500931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
565600931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
565700931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
565800931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
565900931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
566000931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
566100931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
566200931209     C                   ELSE                                                   <-- 3 -->
566300931209     C     WIC           IFEQ      'I'
566400931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
566500931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
566600931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
566700931201     C                   ELSE
566800931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
566900931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
567000931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
567100931201     C                   ENDIF
567200931209     C                   ENDIF                                                  <-- 3 ---
567300931209     C                   ENDIF                                                  <-- 2 ---
567400931214     C*
567500060407    1C                   ENDIF                                                  <-- 1 ---
567600060407     c
567700060407     c* Se non trovato turno di chiusura, verifico se c'e'
567800060407     c*  evento particolare che GIUSTIFICA
567900060407     c                   if        wpoti=0
568000060418     C                   EXSR      RO7CEPG
568100060407     c                   endif
568200931201     C*
568300931201     C                   ENDSR
568400060406     C********************************************************************
568500060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
568600060406     c*                        che giustifica
568700060407     C*            INPUT:-WR7POI (PROGR.ORE)
568800060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
568900060406     C********************************************************************
569000060407     C     RO7CEPG       BEGSR
569100060406     C*
569200060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
569300060407     C                   Z-ADD     WR7POI        WR7POO            9 0
569400060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
569500060406     C* coincide
569600060406    1C                   if        wevpag='S'                                   <-- 1 -->
569700060406     C*
569800060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
569900060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
570000060406     C                   MOVEL     'N'           WSLITT
570100060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
570200060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
570300060406     C*
570400060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
570500060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
570600060406     C     WPOROU        ANDLT     WPOLIM
570700060406     C* MEMORIZZO INIZIO E FINE RANGE
570800060406    3C     WPOTI         IFEQ      *ZEROS
570900060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
571000060406    3C                   END
571100060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
571200060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
571300060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
571400060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
571500060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
571600060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
571700060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
571800060406     C                   MOVEL     'N'           WSLITT
571900060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
572000060406     C* Se non trovato,
572100060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
572200060406    3c                   if        wslitt='N'
572300060406     c                   exsr      DATTUC
572400060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
572500060406    2C                   ENDDO                                                  <-- 2 ---
572600060406     C*
572700060407     C                   Z-ADD     WPOROU        WR7POO
572800060406     C*
572900060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
573000060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
573100060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
573200060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
573300060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
573400060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
573500060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
573600060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
573700060406   x3C                   ELSE                                                   <-- 3 -->
573800060406    4C     WIC           IFEQ      'I'
573900060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
574000060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
574100060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
574200060406   x4C                   ELSE
574300060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
574400060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
574500060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
574600060406    4C                   ENDIF
574700060406    3C                   ENDIF                                                  <-- 3 ---
574800060406    2C                   ENDIF                                                  <-- 2 ---
574900060406     C*
575000060406    1C                   ENDIF                                                  <-- 1 ---
575100060406     C*
575200060406     C                   ENDSR
575300931207     C********************************************************************
575400931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
575500931207     C*            INPUT:  -WR8POI (PROGR.ORE)
575600931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
575700931207     C********************************************************************
575800931207     C     RO8FES        BEGSR
575900130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
576000130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
576100931207     C*
576200931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
576300130411     C     WR8POI        SUB       12            WR8POO            9 0
576400931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
576500931207     C                   MOVEL     'N'           WSLITT
576600130411     c
576700130411     c* Non salto i festivi ???
576800130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
576900130411     c
577000931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
577100130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
577200130411     C     WPOROU        ANDGT     WPODSP
577300130411     C                   SUB       12            WPOROU
577400130411     C                   MOVEL     'N'           WSLITT
577500130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
577600130411     C                   ENDDO                                                  <-- 2 ---
577700130411     c
577800130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
577900130411     C     D98GC2        ANDEQ     *BLANKS
578000130411   x1C                   ELSE                                                   <-- 1 -->
578100130411     c* eseguo controllo per i turni di chiusura
578200130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
578300130411     C*
578400130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
578500130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
578600130411     C     WPOROU        ANDgT     WPOdsp
578700130411     C* MEMORIZZO INIZIO E FINE RANGE
578800130411     C     WPOTf         IFEQ      *ZEROS
578900130411     C                   z-add     wporou        WPOTf
579000130411     C                   END
579100140826     C     WPOROU        SUB       wnoadd        WPOROU
579200130411     c
579300130411     C                   MOVEL     'N'           WSLITT
579400130411     c                   exsr      dattuc
579500130411     c                   if        wslitt='N'
579600130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
579700140826     c* Se slitta per festivo --> tolgo sempre 12 ore
579800140826     c                   z-add     12            WNOADD
579900130411     c                   endif
580000130411     C* SLITTA
580100140826     C     wporou        add       12            WPOTi
580200130411    2C                   ENDDO                                                  <-- 2 ---
580300130411     c
580400130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
580500130411     c
580600130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
580700130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
580800130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
580900130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
581000130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
581100130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
581200130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
581300130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
581400130411   x3C                   ELSE                                                   <-- 3 -->
581500130411    4C     WIC           IFEQ      'I'
581600130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
581700130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
581800130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
581900130411   x4C                   ELSE
582000130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
582100130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
582200130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
582300130411    4C                   ENDIF
582400130411    3C                   ENDIF                                                  <-- 3 ---
582500130411    2C                   ENDIF                                                  <-- 3 ---
582600130411     C*
582700130411    1C                   ENDIF                                                  <-- 1 ---
582800130411
582900130411     C**   WPOROU        ADD       12            WR8POO
583000130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
583100130411     c                   z-add     wr8poi        wr8poo
583200931207     C*
583300931207     C                   ENDSR
583400931210     C********************************************************************
583500931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
583600940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
583700931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
583800931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
583900931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
584000931210     C********************************************************************
584100931210     C     RO9COV        BEGSR
584200931210     C*
584300931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
584400931210     C                   Z-ADD     *ZEROS        WR9NOV
584500931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
584600931210     C                   Z-ADD     WR9POI        WR9POW
584700931210     C* CICLO FINO A CHE:
584800931210     C*   - DATA INIZIO SUPERATO DATA FINE
584900931223     C*   - RAGGIUNTO LIMITE MASSIMO
585000931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
585100931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
585200931223     C     WR9POW        ANDLT     WPOLIM
585300931210     C*
585400931210     C                   Z-ADD     WR9POW        WPOROU
585500931210     C                   MOVEL     'N'           WSLITT            1
585600940324     C* FESTIVI
585700931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
585800940324     C* GIACENZA
585900931214     C     WSLITT        IFEQ      'N'
586000941229£$   C     WPOAGC        IFNE      *ZEROS
586100940324£$   C     WPOCGC        ORNE      *ZEROS
586200941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
586300940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
586400940324£$   C                   MOVEL     'S'           WSLITT
586500940324£$   C                   END
586600940324£$   C                   END
586700940324£$   C                   END
586800960129     C* EVENTI GIACENZA
586900960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
587000960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
587100960129     C                   Z-ADD     *ZEROS        G                 3 0
587200960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
587300960129     C     G             ANDLT     IG
587400960129     C                   ADD       1             G
587500960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
587600960209     C     WPOROU        ANDLE     GCF(G)
587700960129     C                   MOVEL     'S'           WSLITT
587800960129     C                   ENDIF                                                  <-- 3 ---
587900960129     C                   ENDDO                                                  <-- 2 ---
588000960129     C                   ENDIF                                                  <-- 1 ---
588100960129     C                   ENDIF                                                  <-- 0 ---
588200940324     C* LASCIATO AVVISO
588300931214     C     WSLITT        IFEQ      'N'
588400941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
588500940324£$   C                   Z-ADD     *ZEROS        L                 3 0
588600940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
588700940324£$   C     L             ANDLT     IL
588800940324£$   C                   ADD       1             L
588900941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
589000941229£$   C     WPOROU        ANDLE     LCF(L)
589100050520     c     LCon(L)       andeq     'S'
589200940324£$   C                   MOVEL     'S'           WSLITT
589300940324£$   C                   END                                                    <-- 3 ---
589400940324£$   C                   ENDDO                                                  <-- 2 ---
589500940324£$   C                   END                                                    <-- 1 ---
589600931210     C                   END
589700940324     C* TURNO CHIUSURA
589800140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
589900140826     c*  di sistemare le ore da sommare 12 o 24...
590000931214     C     WSLITT        IFEQ      'N'
590100931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
590200931210     C                   END
590300940324     C* DATA CONSEGNA RICHIESTA
590400050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
590500050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
590600050907     c
590700931214     C     WSLITT        IFEQ      'N'
590800050907     c                   add       1             wporou
590900940324£$   C     WPOCRI        IFNE      *ZEROS
591000940324£$   C     WPOROU        IFLT      WPOCRC
591100940324£$   C                   MOVEL     'S'           WSLITT
591200940324£$   C                   END
591300940324£4   C                   END
591400050907     c                   sub       1             wporou
591500931210     C                   END
591600931210     C*
591700931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
591800931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
591900931210     C     WSLITT        IFEQ      'N'
592000931210     C                   ADD       12            WR9NOV            5 0
592100931210     C                   END
592200931210     C                   ADD       12            WR9POW            9 0
592300931210     C*
592400931223     C                   ENDDO                                                  <-- 1 ---
592500931210     C*
592600931210     C                   ENDSR
592700940628%%%  C********************************************************************
592800940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
592900940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
593000940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
593100940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
593200940628%%%  C********************************************************************
593300940628%%%  C     RO0GES        BEGSR
593400940628%%%  C*
593500940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
593600940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
593700940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
593800940628%%%  C*
593900940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
594000940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
594100940628%%%  C*
594200940628%%%  C* CICLO FINO A CHE:
594300940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
594400940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
594500940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
594600050922%%%  C     WR0POO        ANDGe     WPODSP
594700940628%%%  C*
594800940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
594900940628%%%  C                   MOVEL     'N'           WSLITT            1
595000940628%%%  C*
595100940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
595200940628%%%  C*
595300940628%%%  C     WSLITT        IFEQ      'N'
595400940628%%%  C                   SUB       12            WR0NOI            5 0
595500940628%%%  C                   END
595600940628%%%  C                   SUB       12            WR0POO            9 0
595700940628%%%  C*
595800940628%%%  C                   ENDDO                                                  <-- 1 ---
595900940628%%%  C*
596000940628%%%  C                   ADD       12            WR0POO            9 0
596100940628%%%  C*
596200940628%%%  C                   ENDSR
596300960911     C********************************************************************
596400960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
596500960911     C*            INPUT:  -W10POI (PROGR.ORE)
596600960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
596700960911     C********************************************************************
596800960911     C     R10FES        BEGSR
596900960911     C*
597000960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
597100960911     C                   Z-ADD     W10POI        W10POO            9 0
597200960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
597300960911     C                   MOVEL     'N'           WSLITT
597400960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
597500960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
597600960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
597700960911     C                   ADD       12            WPOROU
597800960911     C                   MOVEL     'N'           WSLITT
597900960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
598000960911     C                   ENDDO                                                  <-- 2 ---
598100960911     C                   Z-ADD     WPOROU        W10POO
598200960911     C*
598300960911     C                   ENDSR
598400930325     C********************************************************************
598500930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
598600930325     C********************************************************************
598700930325     C     CHKGIA        BEGSR
598800930406     C*
598900960129     C* APERTURA/CHIUSURA GIACENZA
599000930326     C     WPODAG        IFNE      *ZEROS
599100930406     C     WPODCG        ORNE      *ZEROS
599200930407     C     WPOROU        IFGE      WPODAG
599300930407     C     WPOROU        ANDLE     WPODCG
599400930325     C                   MOVEL     'S'           WSLITT
599500930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
599600930325     C                   END
599700930326     C                   END
599800960129     C*
599900960129     C* EVENTI GIACENZA
600000960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
600100960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
600200960129     C                   Z-ADD     *ZEROS        G                 3 0
600300960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
600400960129     C     G             ANDLT     IG
600500960129     C                   ADD       1             G
600600960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
600700960129     C     WPOROU        ANDLE     GIF(G)
600800091009     c
600900091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
601000091009     c*  l'orario dell'evento
601100091009     c                   select
601200091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
601300091009     c* Se orario entro le 10.30  va bene
601400091014     c*  solo la prima volta per evento cioè quando controllo
601500091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
601600091014     c*  e non devo dare errore
601700091014     C                   MOVEL     'OSC  '       WR3TAB
601800091014     C                   eval      WR3HHI=evo(gip(G))
601900091014     C                   EXSR      RO3ORE
602000091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
602100091014     c*  in ritardo come per "E")
602200091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
602300091014     c                              wr3matpom='P'
602400091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
602500091009     C                   MOVEL     'S'           WSLITT
602600091009     c                   endif
602700091014     c                   eval      SalvaEveG=gip(G)
602800091009     c
602900091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
603000091009     c* Se orario entro le 10.30  va bene
603100091014     c*  solo la prima volta per evento cioè quando controllo
603200091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
603300091014     c*  e non devo dare errore
603400091014     C                   MOVEL     'OSC  '       WR3TAB
603500091014     C                   eval      WR3HHI=evo(gip(G))
603600091014     C                   EXSR      RO3ORE
603700091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
603800091014     c*  in ritardo come per "E")
603900091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
604000091014     c                              wr3matpom='P'
604100091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
604200091009     C                   MOVEL     'S'           WSLITT
604300091009     c                   endif
604400091014     c                   eval      SalvaEveG=gip(G)
604500091009     c
604600091009     c                   other
604700960129     C                   MOVEL     'S'           WSLITT
604800960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
604900091009     c                   endsl
605000091009
605100960129     C                   ENDIF                                                  <-- 3 ---
605200960129     C                   ENDDO                                                  <-- 2 ---
605300960129     C                   ENDIF                                                  <-- 1 ---
605400960129     C                   ENDIF                                                  <-- 0 ---
605500930406     C*
605600930325     C                   ENDSR
605700060420     C********************************************************************
605800930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
605900930325     C********************************************************************
606000930325     C     CHKLAV        BEGSR
606100930325     C*
606200941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
606300930421     C                   Z-ADD     *ZEROS        L                 3 0
606400930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
606500930421     C     L             ANDLT     IL
606600930421     C                   ADD       1             L
606700930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
606800930421     C     WPOROU        ANDLE     LAF(L)
606900090928     c
607000090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
607100091009     c*  l'orario dell'evento
607200091009     c                   select
607300091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
607400091009     c* Se orario entro le 10.30  va bene
607500091014     c*  solo la prima volta per evento cioè quando controllo
607600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
607700091014     c*  e non devo dare errore
607800091014     C                   MOVEL     'OSC  '       WR3TAB
607900091014     C                   eval      WR3HHI=evo(lap(L))
608000091014     C                   EXSR      RO3ORE
608100091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
608200091014     c*  in ritardo come per "E")
608300091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
608400091014     c                              wr3matpom='P'
608500091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
608600091009     C                   MOVEL     'S'           WSLITT
608700091009     c                   endif
608800091014     c                   eval      SalvaEveL=lap(L)
608900091009     c
609000091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
609100091009     c* Se orario entro le 10.30  va bene
609200091014     c*  solo la prima volta per evento cioè quando controllo
609300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
609400091014     c*  e non devo dare errore
609500091014     C                   MOVEL     'OSC  '       WR3TAB
609600091014     C                   eval      WR3HHI=evo(lap(L))
609700091014     C                   EXSR      RO3ORE
609800091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
609900091014     c*  in ritardo come per "E")
610000091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
610100091014     c                              wr3matpom='P'
610200091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
610300091009     C                   MOVEL     'S'           WSLITT
610400091009     c                   endif
610500091014     c                   eval      SalvaEveL=lap(L)
610600090928     c
610700091009     c                   other
610800930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
610900091009     C                   MOVEL     'S'           WSLITT
611000091009     c                   endsl
611100090928     c
611200930421     C                   END                                                    <-- 3 ---
611300931223     C                   ENDDO                                                  <-- 2 ---
611400930421     C                   END                                                    <-- 1 ---
611500930325     C*
611600930325     C                   ENDSR
611700060420     C********************************************************************
611800060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
611900060420     c*                   che Allungano
612000060420     C********************************************************************
612100060420     C     CHKCEPA       BEGSR
612200060420     C*
612300060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
612400060420     C                   Z-ADD     *ZEROS        p                 3 0
612500060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
612600060420     C     p             ANDLT     IpA
612700060420     C                   ADD       1             P
612800060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
612900060420     C     WPOROU        ANDLE     paif(P)
613000060420     C                   MOVEL     'S'           WSLITT
613100060420     C                   ADD       12            W98EA
613200060420     C                   END                                                    <-- 3 ---
613300060420     C                   ENDDO                                                  <-- 2 ---
613400060420     C                   END                                                    <-- 1 ---
613500060420     C*
613600060420     C                   ENDSR
613700930414     C********************************************************************
613800930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
613900930414     C********************************************************************
614000930414     C     CHKCNT        BEGSR
614100930511     C*
614200930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
614300930611     C     WIC           IFEQ      'I'                                          *INTERNO
614400930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
614500930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
614600930611     C                   ELSE
614700930611     C     WPOROU        IFGE      WPOAII
614800930611     C     WPOROU        ANDLE     WPOAFI
614900930611     C                   MOVEL     'S'           WSLITT
615000930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
615100930611     C                   END
615200930611     C                   END
615300930611     C                   ELSE                                                   *CLIENTE
615400930611     C     WPOAIC        IFEQ      *ZEROS
615500930611     C     WPOAFC        ANDEQ     *ZEROS
615600930611     C                   ELSE
615700930611     C     WPOROU        IFGE      WPOAIC
615800930611     C     WPOROU        ANDLE     WPOAFC
615900930611     C                   MOVEL     'S'           WSLITT
616000930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
616100930611     C                   END
616200930611     C                   END
616300930611     C                   END
616400930414     C*
616500930414     C                   ENDSR
616600930511     C********************************************************************
616700930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
616800930511     C********************************************************************
616900930511     C     CHKFED        BEGSR
617000930511     C*
617100930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
617200930611     C     WIC           IFEQ      'I'                                          *INTERNO
617300930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
617400930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
617500930611     C                   ELSE
617600930611     C     WPOROU        IFGE      WPOFII
617700930611     C     WPOROU        ANDLE     WPOFFI
617800930611     C                   MOVEL     'S'           WSLITT
617900930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
618000930611     C                   END
618100930611     C                   END
618200930611     C                   ELSE                                                   *CLIENTE
618300930611     C     WPOFIC        IFEQ      *ZEROS
618400930611     C     WPOFFC        ANDEQ     *ZEROS
618500930611     C                   ELSE
618600930611     C     WPOROU        IFGE      WPOFIC
618700930611     C     WPOROU        ANDLE     WPOFFC
618800930611     C                   MOVEL     'S'           WSLITT
618900930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
619000930611     C                   END
619100930611     C                   END
619200930611     C                   END
619300930511     C*
619400930511     C                   ENDSR
619500930325     C********************************************************************
619600930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
619700930325     C********************************************************************
619800930325     C     CHKFES        BEGSR
619900060526     c* se già passata la data, non controllo wporou
620000060526     c                   if        wpodata>0
620100060526     c                   z-add     wpodata       w12dat
620200060526     c                   clear                   w12res
620300060526     c                   else
620400931130     C*
620500931130     C* TRASFORMA IL  PROGR.ORE IN DATA
620600931130     C                   Z-ADD     WPOROU        WORE
620700931130     C                   EXSR      OREDAT
620800941227     C                   Z-ADD     WDAT          W12DAT            8 0
620900931130     C                   Z-ADD     WRES          W12RES            3 0
621000060526     c                   endif
621100930325     C*
621200941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
621300930325     C* E' FESTIVO
621400090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
621500090108     C**                 Z-ADD     D98LNP        CLNTFP
621600090108     C**                 Z-ADD     *ZEROS        CLNTFA
621700090108     C**                 ENDIF
621800090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
621900090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
622000090108     c**   è possibile rendere festivo in terminal per un altro terminal
622100090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
622200930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
622300090108     C     WTC           oreq      'P'                                          *TRAZIONE
622400941227     C                   Z-ADD     D98TFP        CLNTFP
622500941227     C                   Z-ADD     D98TFA        CLNTFA
622600941228     C                   ENDIF
622700941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
622800941227     C                   Z-ADD     *ZEROS        CLNTFP
622900941227     C                   Z-ADD     D98LNA        CLNTFA
623000941228     C                   ENDIF
623100941227     C                   Z-ADD     W12DAT        DSDAT8
623200941227     C                   Z-ADD     DSAAS         CLNANN
623300941227     C                   Z-ADD     DSMES         CLNMES
623400930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
623500930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
623600930421     C                   Z-ADD     DSGIO         G
623700930415     C     W12RES        IFEQ      *ZEROS
623800941227     C                   MOVEL     CLNMAT        GFEDS
623900930415     C                   ELSE
624000941227     C                   MOVEL     CLNPOM        GFEDS
624100930415     C                   END
624200930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
624300930405     C                   MOVEL     *BLANKS       WC1Y
624400930421     C                   MOVEL     GFE(G)        WC1Y              8
624500930421     C                   Z-ADD     1             I
624600930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
624700930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
624800930405     C     WIC           IFEQ      'I'
624900930421     C     I1Y(I)        ANDEQ     'S'
625000930405     C                   MOVEL     'S'           WSLITT
625100930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
625200930405     C                   END
625300930405     C     WIC           IFEQ      'C'
625400930421     C     E1Y(I)        ANDEQ     'S'
625500930405     C                   MOVEL     'S'           WSLITT
625600930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
625700930405     C                   END
625800970204     C     WIC           IFEQ      'X'
625900970204     C     E1Y(I)        ANDEQ     'S'
626000970204     C                   MOVEL     'S'           WSLITT
626100970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
626200970204     C                   END
626300930405     C                   ELSE                                                   <-- 3 -->
626400930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
626500930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
626600930405     C                   END                                                    <-- 3 ---
626700930405     C                   END                                                    <-- 2 ---
626800930405     C                   END                                                    <-- 1 ---
626900930325     C*
627000060526     c                   clear                   wpodata
627100930325     C                   ENDSR
627200931201     C********************************************************************
627300931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
627400931201     C********************************************************************
627500931201     C     CHKTUC        BEGSR
627600931201     C*
627700931201     C     WIC           IFEQ      'I'                                          --- 1 -->
627800931201     C                   DO        TI            T                 3 0          --- 2 -->
627900931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
628000931201     C     WPOROU        ANDLE     TIF(T)
628100931201     C                   MOVEL     'S'           WSLITT
628200931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
628300931201     C                   LEAVE                                                  *VA A FINE DO
628400931201     C                   ENDIF                                                  <-- 3 ---
628500931201     C                   ENDDO                                                  <-- 2 ---
628600931201     C                   ELSE                                                   <-- 1 -->
628700931201     C                   DO        TC            T                 3 0          --- 2 -->
628800931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
628900931201     C     WPOROU        ANDLE     TCF(T)
629000931201     C                   MOVEL     'S'           WSLITT
629100931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
629200931201     C                   LEAVE                                                  *VA A FINE DO
629300931201     C                   ENDIF                                                  <-- 3 ---
629400931201     C                   ENDDO                                                  <-- 2 ---
629500931201     C                   ENDIF                                                  <-- 1 ---
629600931201     C*
629700931201     C                   ENDSR
629800060418     C********************************************************************
629900060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
630000060418     c*          - eventi particolari che giustificano
630100060418     C********************************************************************
630200060418     C     CHKCEPG       BEGSR
630300060418     C*
630400060418     C     WIC           IFEQ      'I'                                          --- 1 -->
630500060420     C                   DO        PGI           p                 3 0          --- 2 -->
630600060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
630700060420     C     WPOROU        ANDLE     PGIF(p)
630800060418     C                   MOVEL     'S'           WSLITT
630900060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
631000060418     C                   LEAVE                                                  *VA A FINE DO
631100060418     C                   ENDIF                                                  <-- 3 ---
631200060418     C                   ENDDO                                                  <-- 2 ---
631300060418     C                   ELSE                                                   <-- 1 -->
631400060420     C                   DO        PGC           p                 3 0          --- 2 -->
631500060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
631600060420     C     WPOROU        ANDLE     PGCF(p)
631700060418     C                   MOVEL     'S'           WSLITT
631800060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
631900060418     C                   LEAVE                                                  *VA A FINE DO
632000060418     C                   ENDIF                                                  <-- 3 ---
632100060418     C                   ENDDO                                                  <-- 2 ---
632200060418     C                   ENDIF                                                  <-- 1 ---
632300060418     C*
632400060418     C                   ENDSR
632500930512     C********************************************************************
632600931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
632700930512     C********************************************************************
632800931130     C     DATTUC        BEGSR
632900931214     C*
633000931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
633100931214     C                   Z-ADD     12            WNOADD            9 0
633200930325     C*
633300931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
633400931125     C     D98GC2        ORNE      *BLANKS
633500931130     C*
633600931213     C                   Z-ADD     WPOROU        WORE
633700931130     C                   EXSR      OREDAT                                       *ORE->DATA
633800931130     C                   Z-ADD     WDAT          W12DAT
633900931130     C                   Z-ADD     WRES          W12RES
634000930405     C*
634100941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
634200930405     C* DELLA SETTIMANA
634300941227     C                   Z-ADD     W12DAT        DSDAT8
634400941227     C                   Z-ADD     DSAAS         CLNANN
634500941227     C                   Z-ADD     DSMES         CLNMES
634600941227     C                   Z-ADD     *ZEROS        CLNTFP
634700941227     C                   Z-ADD     *ZEROS        CLNTFA
634800930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
634900930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
635000941227     C                   MOVEL     CLNMAT        GSEDS
635100960129     C                   Z-ADD     DSGIO         GI                6 0
635200960129     C                   MOVEL     GSE(GI)       WGSE              1
635300140911     c
635400140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
635500140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
635600140911     c                   if        wtuctut='S'
635700140911     c                   eval      wtuctut='P'
635800140911     c                   endif
635900930326     C*
636000931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
636100931213     C                   DO        4             IT                3 0          --- 3 -->
636200931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
636300931213     C                   MOVEL     TUC(IT)       W93GSE            1
636400931213     C                   MOVE      TUC(IT)       W93GMP            1
636500930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
636600930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
636700931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
636800930512     C     W93GSE        OREQ      ' '
636900140911     c     WTUCTUT       andeq     ' '
637000140911     c
637100930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
637200931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
637300930401     C     W93GMP        OREQ      ' '
637400930325     C                   MOVEL     'S'           WSLITT
637500931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
637600931213     C                   ELSE                                                   <-- 6 -->
637700001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
637800001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
637900001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
638000050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
638100050908     C     Usacons       IFNE      'S'                                          --- 7 -->
638200141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
638300141107     c*    orari giorno per giorno
638400141107     c                   if        OOR2FCSGL<>' '
638500141108     c                             and w12dat<>sav_12dat
638600141107     c                   eval      or_datarif=w12dat
638700141107     c                   eval      or_DDC    =w12dat
638800141107     c                   exsr      Reper_tempi
638900141108     c                   eval      sav_12dat=w12dat
639000141107     c                   endif
639100141107     c
639200930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
639300930325     C     W93GMP        IFEQ      'M'
639400930326     C     W12RES        ANDEQ     *ZEROS
639500141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
639600141111     c                             or OOR2STfS =0
639700141105     c                             or (tuc(IT)<>gc1part and
639800141105     c                              tuc(IT)<>gc2part)
639900140825     C                   MOVEL     'S'           WSLITT
640000140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
640100140911     c* slitto solo una volta  se chiuso tutti " M"
640200140911     c                   if        W93GSE=' '
640300140911     c                   eval      wTUCTUT='S'
640400140911     c                   endif
640500140911     c
640600140825     c                   endif
640700930325     C                   END
640800930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
640900930325     C     W93GMP        IFEQ      'P'
641000930326     C     W12RES        ANDNE     *ZEROS
641100140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
641200140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
641300141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
641400141111     c                             or OOR2STiS =0
641500141105     c                             or (tuc(IT)<>gc1part and
641600141105     c                              tuc(IT)<>gc2part)
641700930325     C                   MOVEL     'S'           WSLITT
641800140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
641900140911     c* slitto solo una volta  se chiuso tutti " P"
642000140911     c                   if        W93GSE=' '
642100140911     c                   eval      wTUCTUT='S'
642200140911     c                   endif
642300140825     c                   endif
642400140825     c
642500931213     C                   ENDIF
642600001128     C                   ENDIF                                                  <-- 7 ---
642700931213     C                   ENDIF                                                  <-- 6 ---
642800931213     C                   ENDIF                                                  <-- 5 ---
642900931213     C                   ENDIF                                                  <-- 4 ---
643000931213     C                   ENDDO                                                  <-- 3 ---
643100930405     C*
643200931213     C                   ENDIF                                                  <-- 2 ---
643300931213     C                   ENDIF                                                  <-- 1 ---
643400140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
643500140911     c                   if        wtuctut='P'  and wslitt='N'
643600140911     c                   clear                   wtuctut
643700140911     c                   endif
643800140911     c
643900930325     C                   ENDSR
644000060406     C********************************************************************
644100060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
644200060406     C********************************************************************
644300060406     C     DATCEPG       BEGSR
644400060406     C*
644500060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
644600060406     C                   Z-ADD     12            WNOADD            9 0
644700060406     C*
644800060406    1C                   if        wevpag='S'
644900060406     C*
645000060406     C                   Z-ADD     WPOROU        WORE
645100060406     C                   EXSR      OREDAT                                       *ORE->DATA
645200060406     C                   Z-ADD     WDAT          W12DAT
645300060406     C                   Z-ADD     WRES          W12RES
645400060418     c                   z-add     1             P
645500060406     c* Verifico se nella data esiste un evento Part che giustifica
645600060418     c     w12dat        lookup    pvd(P)                                 30
645700060418    2c                   dow       *in30 and pvf(P)<>'G'
645800060418     c                   add       1             P
645900060420     c     w12dat        lookup    pvd(P)                                 30
646000060406    2c                   enddo
646100060406     c
646200060406     c* 30 on --> TROVATO !!
646300060406    2c                   if        *in30
646400060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
646500060406     C                   MOVEL     'S'           WSLITT
646600060406     c                   z-add     24            wnoadd
646700060406    3C                   ENDIF
646800060406     C*
646900060406    2C                   ENDIF                                                  <-- 2 ---
647000060406    1C                   ENDIF                                                  <-- 1 ---
647100060406     C*
647200060406     C                   ENDSR
647300931210     C********************************************************************
647400931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
647500931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
647600931210     C********************************************************************
647700050805     C***  CHKDCR        BEGSR
647800931210     C*
647900050805     C***  WPOCRI        IFNE      *ZEROS
648000050805     C***  WPOROU        IFGE      WPOCRI
648100050805     C***  WPOROU        ANDLT     WPOCRF
648200050805     C***                ELSE
648300050805     C***                MOVEL     'S'           WSLITT
648400050805     C***                END
648500050805     C***                END
648600931210     C*
648700050805     C***                ENDSR
648800140822     C********************************************************************
648900140822     C*  Reperimento tempi di consegna per decidere se slittare
649000140822     C********************************************************************
649100140822     C     Reper_tempi   BEGSR
649200140822
649300140822     c                   clear                   trulorSds
649400140825     c                   clear                   trulor2ds
649500140822     c                   eval       IOREtfp = d98tfp
649600140822     c                   eval       IOREtfa = d98tfa
649700140825     c                   eval       IOREdti = d98dti
649800140825     c                   eval       IOREhti = d98hti
649900140825     c                   eval       IOREdcr = s98dcr
650000140825     c                   eval       IOREhcr = s98hcr
650100140825     c                   eval       IOREtcr = s98tcr
650200140825     c                   eval       IOREDEI = d98dei
650300140825     c                   eval       IOREOEI = d98oei
650400140825     c                   eval       IOREfil = d98lna
650500140825     c                   eval       IOREcap = d98cad
650600140825     c                   eval       IOREloc = d98lod
650700140825     c                   eval       IOREnar = d98nzd
650800140825     c
650900140825     c* Data di Riferimento:
651000140825     c* Se impostata la consegnabilità --> prendo questa
651100140822     c                   select
651200140825     c                   when      d98dfc>0
651300141107     c                   if        or_datarif<d98dfc and or_datarif>0
651400141107     c                   eval      ioredta=or_datarif
651500141107     c                   else
651600141107     c                   eval      ioredta=d98dfc
651700141107     c                   endif
651800140825     c* altrimenti data di consegna
651900140825     c                   when      d98dcm>0
652000141107     c                   if        or_datarif<d98dcm and or_datarif>0
652100141107     c                   eval      ioredta=or_datarif
652200141107     c                   else
652300140825     c                   eval      ioredta=d98dcm
652400141107     c                   endif
652500140825     c
652600140822     c                   other
652700140825     c* altrimenti data teorica consegna se > = oggi
652800140825     C                   time                    wrkoggiiso
652900140825     C     *iso          move      wrkoggiiso    ioredta
653000140822     c* se < di oggi, imposto oggi
653100140825     c                   if        d98dei>ioredta
653200140825     c                   eval      ioredta=d98dei
653300140822     c                   endif
653400141107     c
653500141107     c                   if        or_datarif<ioredta and or_datarif>0
653600141107     c                   eval      ioredta=or_datarif
653700141107     c                   endif
653800141107     c
653900140822     c                   endsl
654000140822     c
654100140825     c                   eval       IOREtser = 'C'
654200141107     c                   if         or_ddc>0 and or_ddc<d98ddc
654300141107     c                   eval       IOREDDC=or_ddc
654400141107     c                   eval       IOREnDC=8888888
654500141107     c                   eval       IOREcons = 'S'
654600141107     c                   else
654700140825     c                   eval       IOREDDC=d98ddc
654800140825     c                   eval       IOREnDC=wd98ndc
654900140825     c                   eval       IOREcons = d98cons
655000141107     c                   endif
655100141017     c                   eval       IOREtsp = d98tsp
655200140822     c
655300140822     c                   call      'TRULORSR'
655400140822     c                   parm                    kpjba
655500140822     c                   parm                    trulorSds
655600140825     c                   parm                    trulor2ds
655700140825     c
655800140825     c                   eval      wtrulor= 'S'
655900141107     c                   clear                   or_datarif        8 0
656000141107     c                   clear                   or_ddc            8 0
656100140822     C                   ENDSR
656200950406     C********************************************************************
656300950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
656400950406     C********************************************************************
656500950406     C     DATORE        BEGSR
656600950406     C*
656700950406     C                   Z-ADD     *ZEROS        WORE
656800950406     C     WDAT          IFNE      0
656900950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
657000950406     C     WAA           DIV       4             WQUA              4 0
657100950406     C                   MVR                     WRES
657200950406     C     WAA           MULT      365           WGIO              9 0
657300950406     C                   ADD       WQUA          WGIO
657400950406     C                   ADD       WGG           WGIO
657500950406     C     WMM           SUB       1             MC                3 0
657600950406     C                   DO        MC            WX                3 0
657700950406     C                   ADD       MES(WX)       WGIO
657800950406     C                   END
657900950406     C     WRES          IFEQ      0
658000950406     C     WMM           ANDLE     2
658100950406     C                   SUB       1             WGIO
658200950406     C                   END
658300950406     C     WGIO          MULT      24            WORE              9 0
658400950406     C                   ENDIF
658500950406     C*
658600950406     C                   ENDSR
658700930323     C********************************************************************
658800930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
658900930323     C********************************************************************
659000930323     C     OREDAT        BEGSR
659100930323     C*
659200950407     C                   SETOFF                                       717273
659300950407     C                   SETOFF                                       7475
659400930323     C     WORE          DIV       24            WGIO
659500930323     C                   MVR                     WRES              3 0
659600950406     C*
659700950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
659800950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
659900950406     C*  TROVO ANNO
660000950406     C     WGIO          DIV       365           WAA
660100950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
660200950406     C                   MVR                     WREST                71
660300950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
660400950406     C     WGIO          DIV       365           WAA
660500950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
660600950406     C*                                                    0 = 31/12/PR
660700950406     C   75
660800950406     CAN 71              DO                                                     BIS. E NON ERA
660900950406     C     WAA           SUB       1             WAA
661000950406     C                   Z-ADD     365           WREST
661100950406     C                   END
661200950406     C*
661300950406     C     WAA           DIV       4             WNBIS                                      T.
661400950406     C                   MVR                     WRBIS             3 073
661500950406     C   73
661600950406     CANN71                                                                     NON PIU' BISEST
661700950406     CORN73              ADD       1             WREST                          E'BISESTILE
661800950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
661900950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
662000950406     C  N73              SUB       1             $PM(1)
662100950406     C                   Z-ADD     1             WI                2 0
662200950406     C* TROVO IL MESE E GIORNI
662300950406     C     WREST         LOOKUP    $PM(WI)                            74  73
662400950406     C                   MOVE      WI            WMM
662500950406     C     WI            COMP      1                                      72
662600950406     C  N72
662700950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
662800950406     C   74              SUB       1             WI
662900950406     C  N72
663000950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
663100950406     C   72              MOVE      WREST         WGG
663200950406     C                   ADD       1900          WAA
663300950406     C*
663400930323     C*
663500930323     C                   ENDSR
6636009407151507 C********************************************************************
6637009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6638009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6639009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6640009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6641009407151507 C********************************************************************
6642009407151507 C     FINGIO        BEGSR
6643009407151507 C*
6644009407151507 C     WPORE         DIV       24            WGIO
6645009407151507 C                   MVR                     WRES
6646009407151507 C     WRES          IFEQ      *ZEROS
6647009407151507 C                   Z-ADD     12            WNORE             3 0
6648009407151507 C                   ELSE
6649009407151507 C                   Z-ADD     *ZEROS        WNORE
6650009407151507 C                   ENDIF
6651009407151507 C*
6652009407151507 C                   ENDSR
665300930517     C********************************************************************
665400971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
665500930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
665600930517     C********************************************************************
665700930517     C     CERCAL        BEGSR
665800930517     C*
665900930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
666000941227     C                   MOVE      CLNTFP        DCATFP
666100941227     C                   MOVE      CLNTFA        DCATFA
666200941227     C                   MOVE      CLNANN        DCAANN
666300941227     C                   MOVE      CLNMES        DCAMES
666400930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
666500930517     C                   Z-ADD     1             I
666600930517     C     DCALEN        LOOKUP    CAK(I)                                 99
666700930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
666800930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
666900941227     C     KEYCLN        CHAIN     AZCLN01L                           99
667000930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
667100930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
667200930517     C                   MOVEL     '1'           D98ERR
667300930517     C                   MOVEL     MSG(3)        D98MSG
667400930517     C                   ELSE                                                   <-- 2 -->
667500970401     C* CONTROLLA SE ANNULLATO
667600970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
667700970401     C                   MOVEL     '1'           D98ERR
667800970401     C                   MOVEL     MSG(3)        D98MSG
667900970401     C                   ELSE                                                   <-- 3 -->
668000930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
668100930517     C                   ADD       1             WINDCA            6 0
668200970401     C     WINDCA        IFLE      200                                          --- 4 -->
668300970401     C     WINDCA        ANDGT     *ZEROS
668400930517     C                   Z-ADD     WINDCA        I
668500930517     C                   MOVEL     DCALEN        CAK(I)
668600941227     C                   MOVEL     CLNMAT        CAM(I)
668700941227     C                   MOVEL     CLNPOM        CAP(I)
668800970401     C                   END                                                    <-- 4 ---
668900970401     C                   ENDIF                                                  <-- 3 ---
669000930517     C                   END                                                    <-- 2 ---
669100930517     C                   ELSE                                                   <-- 1 -->
669200930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
669300941227     C                   MOVEL     CAM(I)        CLNMAT
669400941227     C                   MOVEL     CAP(I)        CLNPOM
669500930517     C                   END                                                    <-- 1 ---
669600930517     C*
669700930517     C                   ENDSR
669800010821     C********************************************************************
669900010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
670000010821     C********************************************************************
670100010821     C     CHKVCA        BEGSR
670200010821     C*
670300010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
670400010821     C     D98CAS        IFGT      *ZEROS
670500010821     C     D98VCA        ANDEQ     *BLANKS
670600010821     C                   MOVEL     'ITL'         D98VCA
670700010821     C                   ENDIF
670800010822     C*
670900010822     C     D98VCA        IFNE      'ITL'
671000010822     C     D98VCA        ANDNE     'EUR'
671100010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
671200010822     C                   Z-ADD     D98CAS        INPIMP
671300010822     C                   MOVEL     D98VCA        INPDIV
671400010822     C                   EXSR      EXECNV
671500010822     C                   Z-ADD     OUTIMP        D98CAS
671600010822     C                   MOVEL     §GEDCN        D98VCA
671700010822     C                   ENDIF
671800010821     C*
671900010821     C                   ENDSR
672000010821     C********************************************************************
672100010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
672200010821     C********************************************************************
672300010821     C     EXECNV        BEGSR
672400010821     C*
672500010821     C                   CLEAR                   YEURDS
672600010821     C*
672700010821     C                   MOVEL     INPDIV        YECDVI
672800010821     C                   Z-ADD     INPIMP        YECIMI
672900010821     C                   MOVEL     §GEDCN        YECDVO
673000010821     C                   MOVEL     '3'           YECDCO
673100010821     C                   MOVEL     'H'           YECTAR
673200010821     C                   CALL      'YEURCO'
673300010821     C                   PARM                    YEURDS
673400010821     C*
673500010821     C                   Z-ADD     YECIMO        OUTIMP
673600010821     C*
673700010821     C                   ENDSR
673800011211     C*------------------------------------------------------------------------*
673900011211     C* Controllo importo massimo contrassegno.
674000011211     C*------------------------------------------------------------------------*
674100011211     C     REPMAXCSB     BEGSR
674200011211     C*
674300011211     C                   CLEAR                   Trul21DS
674400011211     C*
674500160420     c                   eval      i21cbo = d98cbo
674600011211     C                   EVAL      I21TSp = d98TSP
674700011211     C                   EVAL      I21NzD = d98NZD
674800011211     C                   EVAL      I21LnP = d98LNP
674900011211     C                   EVAL      I21LnA = d98LNA
675000011211     C                   EVAL      I21Ksc = d98KSC
675100011211     C                   EVAL      I21TiC = d98TIC
675200011211     C                   EVAL      I21Imp = d98CAS
675300011211     C                   EVAL      I21Div = d98VCA
675400160420     c                   eval      i21cbo = d98cbo
675500011211     C*
675600011211     C                   CALL      'TRUL21R'
675700011211     C                   PARM                    Trul21DS
675800160420
675900160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
676000160420     c                   if        wtrasm='N'
676100160420     c                   if        o21fca <>*blanks
676200160420     c                   eval      §b4ltca = 'S'
676300160420     c                   endif
676400160420     c                   endif
676500011211     C*
676600011211     C                   ENDSR
676700011211     C*------------------------------------------------------------------------*
676800011211
676900930414     C********************************************************************
677000930414     C*   RUTINZ - ROUTINE INIZIALE
677100930414     C********************************************************************
677200930414     C     RUTINZ        BEGSR
677300930414     C*
677400930414     C* PARAMETRI IN INPUT ALLA ROUTINE
677500930414     C     *ENTRY        PLIST
677600970731     C                   PARM                    DS99
677700941227     C*
677800941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
677900941227     C     KEYBLP        KLIST
678000941227     C                   KFLD                    BLPAAS
678100941227     C                   KFLD                    BLPLNP
678200941227     C                   KFLD                    BLPNRS
678300941227     C                   KFLD                    BLPNSP
678400050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
678500941227     C     KEYGCP        KLIST
678600050405     C                   KFLD                    KWGCPAAS
678700050405     C                   KFLD                    KWGCPLNP
678800050405     C                   KFLD                    KWGCPNRS
678900050405     C                   KFLD                    KWGCPNSP
679000050405     C                   KFLD                    KWGCPFRG
679100050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
679200031125     C     KEYGC_P       KLIST
679300050405     C                   KFLD                    KWGCPAAS
679400050405     C                   KFLD                    KWGCPLNP
679500050405     C                   KFLD                    KWGCPNRS
679600050405     C                   KFLD                    KWGCPNSP
679700970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
679800150120     C**   KEYBLT        KLIST
679900150120     C**                 KFLD                    BLTAAS
680000150120     C**                 KFLD                    BLTLNP
680100150120     C**                 KFLD                    BLTNRS
680200150120     C**                 KFLD                    BLTNSP
680300941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
680400941227     C     KEYARB        KLIST
680500941227     C                   KFLD                    ARBAAS
680600941227     C                   KFLD                    ARBLNP
680700941227     C                   KFLD                    ARBNRS
680800941227     C                   KFLD                    ARBNSP
680900051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
681000941228     C     KEYAR9        KLIST
681100941228     C                   KFLD                    AR9AAS
681200941228     C                   KFLD                    AR9LNP
681300941228     C                   KFLD                    AR9NRS
681400941228     C                   KFLD                    AR9NSP
681500941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
681600050405     C     KEYGCP21      KLIST
681700050405     C                   KFLD                    KWGCPAAS
681800050405     C                   KFLD                    KWGCPLNP
681900050405     C                   KFLD                    KWGCPNRS
682000050405     C                   KFLD                    KWGCPNSP
682100050405     C                   KFLD                    KWGCPFRG
682200970912     C* CHIAVE LETTURA FNART01L - PARZIALE
682300150120     C***  KEYART        KLIST
682400150120     C***                KFLD                    ARTAAS
682500150120     C***                KFLD                    ARTLNP
682600150120     C***                KFLD                    ARTNRS
682700150120     C***                KFLD                    ARTNSP
682800950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
682900950102     C     KEYLBL        KLIST
683000950102     C                   KFLD                    LBLAAN
683100950102     C                   KFLD                    LBLLPN
683200950102     C                   KFLD                    LBLNRN
683300950102     C                   KFLD                    LBLNSN
683400930414     C*
683500070827     c     kAR5          Klist
683600070827     c                   Kfld                    d98Aas
683700070827     c                   Kfld                    d98Lnp
683800070827     c                   Kfld                    d98nrs
683900070827     c                   Kfld                    d98nsp
684000070827     c                   Kfld                    kAr5Trd
684100150325     c     kAR4          Klist
684200150325     c                   Kfld                    d98Aas
684300150325     c                   Kfld                    d98Lnp
684400150325     c                   Kfld                    d98nrs
684500150325     c                   Kfld                    d98nsp
684600150325     c                   Kfld                    kAr4Trc
684700150218     c*
684800150218     c     kart          Klist
684900150218     c                   Kfld                    d98Aas
685000150218     c                   Kfld                    d98Lnp
685100150218     c                   Kfld                    d98nrs
685200150218     c                   Kfld                    d98nsp
685300150218     c     kbrv          Klist
685400150218     c                   Kfld                    artfls
685500150218     c                   Kfld                    artlna
685600150218     c                   Kfld                    artnrs
685700150218     c                   Kfld                    artnsc
685800150325     C     Kfapd         klist
685900150325     C                   kfld                    kpdtip
686000150325     C                   kfld                    kpdpdr
686100930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
686200930414     C     KEYTAB        KLIST
686300930414     C                   KFLD                    KTBUTE
686400930414     C                   KFLD                    KTBCOD
686500930414     C                   KFLD                    KTBKEY
686600060406     C     Ktab          KLIST
686700060406     C                   KFLD                    KTBUTE
686800060406     C                   KFLD                    KTBCOD
686900930414     C*
687000941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
687100941227     C     KEYCLN        KLIST
687200941227     C                   KFLD                    CLNTFP
687300941227     C                   KFLD                    CLNTFA
687400941227     C                   KFLD                    CLNANN
687500941227     C                   KFLD                    CLNMES
687600941227     C*
687700941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
687800941227     C     KEYCAS        KLIST
687900941227     C                   KFLD                    CASPTA
688000941227     C                   KFLD                    CASLNP
688100941227     C                   KFLD                    CASLNA
688200941227     C                   KFLD                    CASTSP
688300941227     C                   KFLD                    CASDDE
688400930420     C*
688500990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
688600930420     C     KEYEVB        KLIST
688700930420     C                   KFLD                    KEBAAS
688800930420     C                   KFLD                    KEBLNP
688900930420     C                   KFLD                    KEBNRS
689000930420     C                   KFLD                    KEBNSP
689100010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
689200010821     C     KTBE          KLIST
689300010821     C                   KFLD                    TBECOD
689400010821     C                   KFLD                    TBEKE1
689500010821     C                   KFLD                    TBEKE2
689600930414     C*
689700010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
689800930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
689900930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
690000930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
690100930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
690200930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
690300930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
690400930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
690500930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
690600930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
690700010821     C     *LIKE         DEFINE    D98CAS        INPIMP
690800010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
690900010821     C     *LIKE         DEFINE    D98VCA        INPDIV
691000930414     C*
691100930414     C* RECUPERO IL CODICE DELL'AZIENDA
691200930414     C                   Z-ADD     1             CODUT             1 0
691300970701     C                   Z-ADD     *ZEROS        I                 6 0
691400930414     C*
691500930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
691600930414     C                   EXSR      AZZERA
691700930414     C*
691800930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
691900941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
692000941227     C                   MOVEL     'S'           CTABSN            1
692100930414     C                   EXSR      CARTAB
692200941227     C                   ENDIF
692300041116     c* Se passato umero di spedizioni apro i files , altrimenti no
692400041116     c                   if        d98nsp>0 and wapri=' '
692500041116     c                   open      fnblp01l
692600050405     c                   open      tigcp01l
692700150120     c***                open      fnblt01l
692800041116     c                   open      fnarb01l
692900051115     c                   open      fiar901l
693000150325     c                   open      fiar401l
693100050405     c                   open      tigcp21l
693200150218     c                   open      fnart01l
693300150218     c                   open      fnbrv07l
693400041116     c                   open      fnlbl01l
693500041116     c                   open      fnevb01l
693600070827     c                   open      fiar501l
693700041116     c                   eval      wapri='S'
693800041116     c                   endif
693900930414     C*
694000930414     C                   ENDSR
694100941227     C*------------------------------------------------------------------------*
694200941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
694300941227     C*------------------------------------------------------------------------*
694400941227     C     SUBDAT        BEGSR
694500941227     C*
694600941227     C                   TIME                    DTTIME
694700941227     C                   Z-ADD     DTANN         DSANN
694800941227     C                   Z-ADD     DTMES         DSMES
694900941227     C                   Z-ADD     DTGIO         DSGIO
695000941227     C                   Z-ADD     DTSEC         DSSEC
695100941227     C*
695200941227     C                   ENDSR
695300930414     C********************************************************************
695400930414     C*   CARTAB - CARICA LE TABELLE
695500930414     C********************************************************************
695600930414     C     CARTAB        BEGSR
695700930414     C*
695800930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
695900930414     C     D98ERR        IFEQ      '0'
696000930421     C                   Z-ADD     0             I
696100930414     C                   Z-ADD     CODUT         KTBUTE
696200930414     C                   MOVEL     '1Y'          KTBCOD
696300930414     C                   MOVEL     *BLANKS       KTBKEY
696400930414     C     KEYTAB        SETLL     TABEL00F                           99
696500930414     C  N99              READ      TABEL00F                               99
696600930414     C     *IN99         IFEQ      '1'
696700930414     C                   MOVEL     '1'           D98ERR
696800930414     C                   MOVEL     MSG(5)        D98MSG
696900930414     C                   END
697000930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
697100930414     C     TBLCOD        ANDEQ     '1Y'
697200930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
697300930421     C                   ADD       1             I
697400930421     C                   MOVEL     TBLKEY        C1Y(I)
697500930414     C                   MOVEL     TBLUNI        DS1Y
697600930421     C                   MOVEL     §1YFEI        I1Y(I)
697700930421     C                   MOVEL     §1YFEC        E1Y(I)
697800930414     C                   END                                                    <-- 3 ---
697900930414     C                   READ      TABEL00F                               99
698000930414     C                   END                                                    <-- 2 ---
698100930414     C                   END
698200930414     C*
698300930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
698400930414     C     D98ERR        IFEQ      '0'
698500930421     C                   Z-ADD     0             I
698600930414     C                   Z-ADD     CODUT         KTBUTE
698700930414     C                   MOVEL     '1X'          KTBCOD
698800930414     C                   MOVEL     *BLANKS       KTBKEY
698900930414     C     KEYTAB        SETLL     TABEL00F                           99
699000930414     C  N99              READ      TABEL00F                               99
699100930414     C     *IN99         IFEQ      '1'
699200930414     C                   MOVEL     '1'           D98ERR
699300930414     C                   MOVEL     MSG(1)        D98MSG
699400930414     C                   END
699500930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
699600930414     C     TBLCOD        ANDEQ     '1X'
699700930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
699800930421     C                   ADD       1             I
699900930421     C                   MOVEL     TBLKEY        C1X(I)
700000930414     C                   MOVEL     TBLUNI        DS1X
700100930421     C                   Z-ADD     §1XHHG        G1X(I)
700200930421     C                   Z-ADD     §1XHHO        O1X(I)
700300930421     C                   Z-ADD     §1XHHM        M1X(I)
700400930421     C                   Z-ADD     §1XHHP        P1X(I)
700500930421     C                   Z-ADD     §1XHHI        I1X(I)
700600930421     C                   Z-ADD     §1XHHS        S1X(I)
700700930414     C                   END                                                    <-- 3 ---
700800930414     C                   READ      TABEL00F                               99
700900931223     C                   ENDDO                                                  <-- 2 ---
701000050523     c
701100050523     c* controllo riempimento skiere
701200050523     c                   clear                   trul0sds
701300050523     c                   eval      i0sski='C1X/G1X/O1X/...'
701400050523     c                   eval      i0sele=%elem(c1x)
701500050523     c                   eval      i0spie=i
701600050523     c                   eval      i0sfile='TABEL00F  '
701700050523     c                   eval      i0scod=' 1X  '
701800050523     c                   eval      i0spgm='TNSD99R'
701900050523     c                   movel     trul0sds      kpjbu
702000050523     c                   call      'TRUL0SR'
702100050523     c                   parm                    kpjba
702200050523     C                   END
702300930421     C*
702400930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
702500930421     C     D98ERR        IFEQ      '0'
702600930421     C                   Z-ADD     0             I
702700930421     C                   Z-ADD     CODUT         KTBUTE
702800930421     C                   MOVEL     '2A'          KTBCOD
702900060406     C     Ktab          SETLL     TABEL00F                           99
703000060406     C  N99ktab          READe     TABEL00F                               99
703100930421     C     *IN99         IFEQ      '1'
703200930421     C                   MOVEL     '1'           D98ERR
703300930421     C                   MOVEL     MSG(8)        D98MSG
703400930421     C                   END
703500930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
703600930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
703700930421     C                   ADD       1             I
703800930421     C                   MOVEL     TBLKEY        C2A(I)
703900060406     C                   MOVEL     TBLUNI        ds2a
704000060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
704100060406     C                   MOVEL     w003a         D2A(I)
704200930421     C                   END                                                    <-- 3 ---
704300060406     C     ktab          READe     TABEL00F                               99
704400931223     C                   ENDDO                                                  <-- 2 ---
704500050523     c* controllo riempimento skiere
704600050523     c                   clear                   trul0sds
704700050523     c                   eval      i0sski='C2A/D2A'
704800050523     c                   eval      i0sele=%elem(c2a)
704900050523     c                   eval      i0spie=i
705000050523     c                   eval      i0sfile='TABEL00F  '
705100060406     c                   eval      i0scod='2A/2Z'
705200050523     c                   eval      i0spgm='TNSD99R'
705300050523     c                   movel     trul0sds      kpjbu
705400050523     c                   call      'TRUL0SR'
705500050523     c                   parm                    kpjba
705600930421     C                   END
705700060406     c* Tabella  2Z
705800060406     C     D98ERR        IFEQ      '0'
705900060406     C                   Z-ADD     CODUT         KTBUTE
706000060406     C                   MOVEL     '2Z'          KTBCOD
706100060406     C     Ktab          SETLL     TABEL00F                           99
706200060406     C  N99ktab          READe     TABEL00F                               99
706300060406     C     *IN99         IFEQ      '1'
706400060406     C                   MOVEL     '1'           D98ERR
706500060406     C                   MOVEL     MSG(33)       D98MSG
706600060406     C                   END
706700060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
706800060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
706900060406     C                   Z-ADD     1             I
707000060406     C                   MOVEL     TBLKEY        w003a
707100060406     c     w003a         lookup    c2a(i)                                 99
707200060406     C     *IN99         IFEQ      *off
707300060406     C                   MOVEL     '1'           D98ERR
707400060406     C                   MOVEL     MSG(34)       D98MSG
707500060406     C                   Else
707600060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
707700060406     C                   END                                                    <-- 3 ---
707800060406     C                   END                                                    <-- 3 ---
707900060406     C     ktab          READe     TABEL00F                               99
708000060406     C                   ENDDO                                                  <-- 2 ---
708100060406     C                   END
708200941227     C*
708300941227     C* CARICA TABELLA CODICI BOLLA -3A-
708400941227     C     D98ERR        IFEQ      '0'
708500941228     C                   Z-ADD     0             I
708600941227     C                   Z-ADD     CODUT         KTBUTE
708700941227     C                   MOVEL     '3A'          KTBCOD
708800941227     C                   MOVEL     *BLANKS       KTBKEY
708900941227     C     KEYTAB        SETLL     TABEL00F                           99
709000941227     C  N99              READ      TABEL00F                               99
709100941227     C     *IN99         IFEQ      '1'
709200941227     C                   MOVEL     '1'           D98ERR
709300941227     C                   MOVEL     MSG(22)       D98MSG
709400941227     C                   END
709500941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
709600941227     C     TBLCOD        ANDEQ     '3A'
709700941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
709800941227     C                   ADD       1             I
709900941227     C                   MOVEL     TBLKEY        C3A(I)
710000941227     C                   MOVEL     TBLUNI        DS3A
710100941227     C                   MOVEL     §3ATB1        T3A(I)
710200941228     C                   Z-ADD     §3AFCA        F3A(I)
710300990308     C                   MOVEL     §3ARBL        R3A(I)
710400941227     C                   END                                                    <-- 3 ---
710500941227     C                   READ      TABEL00F                               99
710600941227     C                   END                                                    <-- 2 ---
710700050523     c* controllo riempimento skiere
710800050523     c                   clear                   trul0sds
710900050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
711000050523     c                   eval      i0sele=%elem(c3a)
711100050523     c                   eval      i0spie=i
711200050523     c                   eval      i0sfile='TABEL00F  '
711300050523     c                   eval      i0scod=' 3A  '
711400050523     c                   eval      i0spgm='TNSD99R'
711500050523     c                   movel     trul0sds      kpjbu
711600050523     c                   call      'TRUL0SR'
711700050523     c                   parm                    kpjba
711800941227     C                   END
711900971210     C*
712000020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
712100020226     C                   Z-ADD     *ZEROS        J                 3 0
712200160331     C                   Z-ADD     *ZEROS        D                 3 0
712300971210IF  0C     D98ERR        IFEQ      '0'
712400971210     C     *LOVAL        SETLL     AZORG01L
712500971210     C                   READ      AZORG01L                               98
712600971210DO  1C     *IN98         DOWEQ     *OFF
712700971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
712800971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
712900971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
713000020627     C                   MOVEL     ORGDE3        OG143
713100020702     C*
713200020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
713300020702     C                   CLEAR                   DNTW
713400020702     C*
713500020702     C                   MOVEL(P)  'NTW'         TBECOD
713600020702     C                   MOVEL(P)  §OGNTW        TBEKE1
713700020702     C     KTBE          CHAIN     TNTBE01L
713800020702     C                   IF        %found(TNTBE01L)
713900020702     C                   MOVEL     TBEUNI        DNTW
714000020702     C                   ENDIF
714100020702     C*
714200020702     C                   MOVEL     §NTWFIE       FL1(F)
714300020627     C*
714400020226     C* VERIFICO SE FILIALE FEDEX
714500020226     C                   IF        §OGNTW = 'FED'
714600020226     C                   ADD       1             J
714700020226     C                   Z-ADD     ORGFIL        FILFED(J)
714800020226     C                   ENDIF
714900160331     C* VERIFICO SE FILIALE DPD
715000160331     C                   IF        §OGNTW = 'DPD'
715100160331     C                   ADD       1             D
715200160331     C                   Z-ADD     ORGFIL        FILDPD(D)
715300160331     C                   ENDIF
715400020226     C*
715500971210E   2C                   ENDIF
715600971210     C                   READ      AZORG01L                               98
715700971210E   1C                   ENDDO
715800971210E   0C                   ENDIF
715900010821     C*
716000010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
716100010821     C                   MOVEL     'GED'         TBECOD
716200070514     C                   MOVEL     *BLANKS       TBEKE1
716300070514     C                   MOVEL     '1'           TBEKE1
716400010821     C                   MOVEL     *BLANKS       TBEKE2
716500010821     C     KTBE          CHAIN     TNTBE01L                           21
716600070514     C  N21              MOVEL     TBEUNI        DGED
716700150326     c
716800150326     C* REPERISCO LA tabella flag operativi per tabella OLP
716900150326     c                   clear                   dvpo
717000150326     C                   MOVEL     'VPO'         TBECOD
717100150326     C                   MOVEL     *BLANKS       TBEKE2
717200150326     C                   MOVEL(P)  'VPO'         TBEKE1
717300150326     C     KTBE          CHAIN     TNTBE01L                           21
717400150326     C  N21              MOVEL     TBEUNI        DVPO
717500150326     C*
717600031201     C*
717700031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
717800031201     C     D98ERR        IFEQ      '0'
717900060403     C                   Z-ADD     0             I7QW              3 0
718000031201     C                   Z-ADD     CODUT         KTBUTE
718100031201     C                   MOVEL     '7Q'          KTBCOD
718200031201     C                   MOVEL     *BLANKS       KTBKEY
718300031201     C     KEYTAB        SETLL     TABEL00F                           99
718400031201     C  N99              READ      TABEL00F                               99
718500031201     C     *IN99         IFEQ      '1'
718600031201     C                   MOVEL     '1'           D98ERR
718700031201     C                   MOVEL     MSG(8)        D98MSG
718800031201     C                   ENDIF
718900031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
719000031201     C     TBLCOD        ANDEQ     '7Q'
719100031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
719200031201     C                   MOVEL     TBLUNI        DS7Q
719300031201     C                   IF        §7Q4D1 = 'R' OR
719400031201     C                             §7Q4D2 = 'R' OR
719500031201     C                             §7Q4D3 = 'R' OR
719600031201     C                             §7Q4D4 = 'R' OR
719700031201     C                             §7Q4D5 = 'R'
719800031201     C                   ADD       1             I7QW
719900031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
720000031201     C                   ENDIF                                                  <-- 3 ---
720100031201     C                   ENDIF                                                  <-- 3 ---
720200031201     C                   READ      TABEL00F                               99
720300031201     C                   ENDDO                                                  <-- 2 ---
720400031201     C                   ENDIF
720500031201     C*
720600031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
720700031201     C     D98ERR        IFEQ      '0'
720800031201     C                   Z-ADD     CODUT         KTBUTE
720900031201     C                   MOVEL     '7W'          KTBCOD
721000031201     C                   MOVEL     *BLANKS       KTBKEY
721100031201     C     KEYTAB        SETLL     TABEL00F                           99
721200031201     C  N99              READ      TABEL00F                               99
721300031201     C     *IN99         IFEQ      '1'
721400031201     C                   MOVEL     '1'           D98ERR
721500031201     C                   MOVEL     MSG(8)        D98MSG
721600031201     C                   ENDIF
721700031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
721800031201     C     TBLCOD        ANDEQ     '7W'
721900031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
722000031201     C                   MOVEL     TBLUNI        DS7W
722100031201     C                   IF        §7W6D1 = 'C' OR
722200031201     C                             §7W6D2 = 'C' OR
722300031201     C                             §7W6D3 = 'C' OR
722400031201     C                             §7W6D4 = 'C' OR
722500031201     C                             §7W6D5 = 'C'
722600031201     C                   ADD       1             I7QW
722700031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
722800031201     C                   ENDIF                                                  <-- 3 ---
722900031201     C                   ENDIF                                                  <-- 3 ---
723000031201     C                   READ      TABEL00F                               99
723100031201     C                   ENDDO                                                  <-- 2 ---
723200050523     c* controllo riempimento skiere
723300050523     c                   clear                   trul0sds
723400050523     c                   eval      i0sski='SK7QW'
723500050523     c                   eval      i0sele=%elem(sk7qw)
723600050523     c                   eval      i0spie=i7qw
723700050523     c                   eval      i0sfile='TABEL00F  '
723800050523     c                   eval      i0scod='7Q-7W'
723900050523     c                   eval      i0spgm='TNSD99R'
724000050523     c                   movel     trul0sds      kpjbu
724100050523     c                   call      'TRUL0SR'
724200050523     c                   parm                    kpjba
724300031201     C                   ENDIF
724400930414     C*
724500930414     C                   ENDSR
724600930414     C********************************************************************
724700930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
724800930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
724900930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
725000930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
725100930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
725200930414     C********************************************************************
725300930414     C     RO3ORE        BEGSR
725400930414     C*
725500930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
725600930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
725700930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
725800050902     c                   movel     ' '           WR3MATPOM         1
725900931209     C                   Z-ADD     *ZEROS        WR3HHO
726000931209     C                   Z-ADD     *ZEROS        WR3HHA
726100930414     C                   MOVE      WR3TAB        WR3COD
726200930604     C                   MOVEL     D98LNA        WR3COD            8
726300930421     C                   Z-ADD     1             I
726400930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
726500930414     C     *IN99         IFEQ      '0'
726600930414     C                   MOVE      WR3TAB        WR3COD
726700930414     C                   MOVEL     '   '         WR3COD            8
726800930421     C                   Z-ADD     1             I
726900930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
727000930414     C                   END
727100930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
727200930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
727300930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
727400930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
727500930414     C                   ELSE                                                   <-- 2 -->
727600930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
727700930421     C                   Z-ADD     M1X(I)        WR3HHO
727800930510     C                   Z-ADD     I1X(I)        WR3HHA
727900050902     c                   movel     'M'           WR3MATPOM         1
728000930414     C                   END
728100930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
728200930421     C                   Z-ADD     P1X(I)        WR3HHO
728300930510     C                   Z-ADD     S1X(I)        WR3HHA
728400050902     c                   movel     'P'           WR3MATPOM
728500930414     C                   END
728600930414     C                   END                                                    <-- 2 ---
728700930414     C                   END                                                    <-- 1 ---
728800930414     C*
728900930414     C                   ENDSR
729000930423     C********************************************************************
729100930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
729200930423     C********************************************************************
729300930423     C     W98D98        BEGSR
729400930423     C*
729500941228     C*
729600941228     C* ORE REALI PARTENZA INTERNE
729700941228     C                   Z-ADD     W98RPI        W98               3 0
729800941228     C                   EXSR      W98999
729900941228     C                   Z-ADD     D98           D98RPI
730000941228     C* ORE REALI PARTENZA CLIENTE
730100941228     C                   Z-ADD     W98RPC        W98
730200941228     C                   EXSR      W98999
730300941228     C                   Z-ADD     D98           D98RPC
730400930423     C* ORE REALI TRAZIONE INTERNE
730500941228     C                   Z-ADD     W98RTI        W98
730600941228     C                   EXSR      W98999
730700941228     C                   Z-ADD     D98           D98RTI
730800930423     C* ORE REALI TRAZIONE CLIENTE
730900941228     C                   Z-ADD     W98RTC        W98
731000941228     C                   EXSR      W98999
731100941228     C                   Z-ADD     D98           D98RTC
731200930423     C* ORE REALI CONSEGNA INTERNE
731300941228     C                   Z-ADD     W98RCI        W98
731400941228     C                   EXSR      W98999
731500941228     C                   Z-ADD     D98           D98RCI
731600930423     C* ORE REALI CONSEGNA CLIENTE
731700941228     C                   Z-ADD     W98RCC        W98
731800941228     C                   EXSR      W98999
731900941228     C                   Z-ADD     D98           D98RCC
732000930423     C* ORE REALI RESA INTERNE
732100941228     C                   Z-ADD     W98RRI        W98
732200941228     C                   EXSR      W98999
732300941228     C                   Z-ADD     D98           D98RRI
732400930423     C* ORE REALI RESA CLIENTE
732500941228     C                   Z-ADD     W98RRC        W98
732600941228     C                   EXSR      W98999
732700941228     C                   Z-ADD     D98           D98RRC
732800941228     C* ORE DIFF. PARTENZA INTERNE
732900941228     C                   Z-ADD     W98FPI        W98
733000941228     C                   EXSR      W98999
733100941228     C                   Z-ADD     D98           D98FPI
733200941228     C* ORE DIFF. PARTENZA CLIENTE
733300941228     C                   Z-ADD     W98FPC        W98
733400941228     C                   EXSR      W98999
733500941228     C                   Z-ADD     D98           D98FPC
733600930423     C* ORE DIFF. TRAZIONE INTERNE
733700941228     C                   Z-ADD     W98FTI        W98
733800941228     C                   EXSR      W98999
733900941228     C                   Z-ADD     D98           D98FTI
734000930423     C* ORE DIFF. TRAZIONE CLIENTE
734100941228     C                   Z-ADD     W98FTC        W98
734200941228     C                   EXSR      W98999
734300941228     C                   Z-ADD     D98           D98FTC
734400930423     C* ORE DIFF. CONSEGNA INTERNE
734500941228     C                   Z-ADD     W98FCI        W98
734600941228     C                   EXSR      W98999
734700941228     C                   Z-ADD     D98           D98FCI
734800930423     C* ORE DIFF. CONSEGNA CLIENTE
734900941228     C                   Z-ADD     W98FCC        W98
735000941228     C                   EXSR      W98999
735100941228     C                   Z-ADD     D98           D98FCC
735200930423     C* ORE DIFF. RESA INTERNE
735300941228     C                   Z-ADD     W98FRI        W98
735400941228     C                   EXSR      W98999
735500941228     C                   Z-ADD     D98           D98FRI
735600930423     C* ORE DIFF. RESA CLIENTE
735700941228     C                   Z-ADD     W98FRC        W98
735800941228     C                   EXSR      W98999
735900941228     C                   Z-ADD     D98           D98FRC
736000941228     C* ORE NON CONF. PARTENZA INTERNE
736100941228     C     W98NPI        IFGT      999
736200941228     C                   Z-ADD     999           D98NPI
736300941228     C                   ELSE
736400941228     C                   Z-ADD     W98NPI        D98NPI
736500941228     C                   END
736600941228     C* ORE NON CONF. PARTENZA CLIENTE
736700941228     C     W98NPC        IFGT      999
736800941228     C                   Z-ADD     999           D98NPC
736900941228     C                   ELSE
737000941228     C                   Z-ADD     W98NPC        D98NPC
737100941228     C                   END
737200930423     C* ORE NON CONF. TRAZIONE INTERNE
737300930423     C     W98NTI        IFGT      999
737400930423     C                   Z-ADD     999           D98NTI
737500930423     C                   ELSE
737600930423     C                   Z-ADD     W98NTI        D98NTI
737700930423     C                   END
737800930423     C* ORE NON CONF. TRAZIONE CLIENTE
737900930423     C     W98NTC        IFGT      999
738000930423     C                   Z-ADD     999           D98NTC
738100930423     C                   ELSE
738200930423     C                   Z-ADD     W98NTC        D98NTC
738300930423     C                   END
738400930423     C* ORE NON CONF. CONSEGNA INTERNE
738500930423     C     W98NCI        IFGT      999
738600930423     C                   Z-ADD     999           D98NCI
738700930423     C                   ELSE
738800930423     C                   Z-ADD     W98NCI        D98NCI
738900930423     C                   END
739000930423     C* ORE NON CONF. CONSEGNA CLIENTE
739100930423     C     W98NCC        IFGT      999
739200930423     C                   Z-ADD     999           D98NCC
739300930423     C                   ELSE
739400930423     C                   Z-ADD     W98NCC        D98NCC
739500930423     C                   END
739600930423     C* ORE NON CONF. RESA INTERNE
739700930423     C     W98NRI        IFGT      999
739800930423     C                   Z-ADD     999           D98NRI
739900930423     C                   ELSE
740000930423     C                   Z-ADD     W98NRI        D98NRI
740100930423     C                   END
740200930423     C* ORE NON CONF. RESA CLIENTE
740300930423     C     W98NRC        IFGT      999
740400930423     C                   Z-ADD     999           D98NRC
740500930423     C                   ELSE
740600930423     C                   Z-ADD     W98NRC        D98NRC
740700930423     C                   END
740800930423     C* ORE X TURNO CHIUSURA INTERNE
740900941228     C                   Z-ADD     W98HHI        W98
741000941228     C                   EXSR      W98999
741100941228     C                   Z-ADD     D98           D98HHI
741200930423     C* ORE X TURNO CHIUSURA CLIENTE
741300941228     C                   Z-ADD     W98HHC        W98
741400941228     C                   EXSR      W98999
741500941228     C                   Z-ADD     D98           D98HHC
741600060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
741700060418     C                   Z-ADD     W98EGI        W98
741800060418     C                   EXSR      W98999
741900060418     C                   Z-ADD     D98           D98hEGI
742000060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
742100060418     C                   Z-ADD     W98EGC        W98
742200060418     C                   EXSR      W98999
742300060418     C                   Z-ADD     D98           D98hEGC
742400060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
742500060420     C                   Z-ADD     W98EAI        W98
742600060420     C                   EXSR      W98999
742700060420     C                   Z-ADD     D98           D98hEAI
742800060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
742900060420     C                   Z-ADD     W98EAC        W98
743000060420     C                   EXSR      W98999
743100060420     C                   Z-ADD     D98           D98hEAC
743200930423     C* ORE X LASC. AVVISO INTERNE
743300941228     C                   Z-ADD     W98HLI        W98
743400941228     C                   EXSR      W98999
743500941228     C                   Z-ADD     D98           D98HLI
743600930423     C* ORE X LASC. AVVISO CLIENTE
743700941228     C                   Z-ADD     W98HLC        W98
743800941228     C                   EXSR      W98999
743900941228     C                   Z-ADD     D98           D98HLC
744000930423     C* ORE X GIACENZA INTERNE
744100941228     C                   Z-ADD     W98HGI        W98
744200941228     C                   EXSR      W98999
744300941228     C                   Z-ADD     D98           D98HGI
744400930423     C* ORE X GIACENZA CLIENTE
744500941228     C                   Z-ADD     W98HGC        W98
744600941228     C                   EXSR      W98999
744700941228     C                   Z-ADD     D98           D98HGC
744800941228     C* ORE X FEST. PART. INTERNE
744900941228     C                   Z-ADD     W98HPI        W98
745000941228     C                   EXSR      W98999
745100941228     C                   Z-ADD     D98           D98HPI
745200941228     C* ORE X FEST. PART. CLIENTE
745300941228     C                   Z-ADD     W98HPC        W98
745400941228     C                   EXSR      W98999
745500941228     C                   Z-ADD     D98           D98HPC
745600930423     C* ORE X FEST. TRAZ. INTERNE
745700941228     C                   Z-ADD     W98HFI        W98
745800941228     C                   EXSR      W98999
745900941228     C                   Z-ADD     D98           D98HFI
746000930423     C* ORE X FEST. TRAZ. CLIENTE
746100941228     C                   Z-ADD     W98HFC        W98
746200941228     C                   EXSR      W98999
746300941228     C                   Z-ADD     D98           D98HFC
746400930423     C* ORE X FEST. CONS. INTERNE
746500941228     C                   Z-ADD     W98HEI        W98
746600941228     C                   EXSR      W98999
746700941228     C                   Z-ADD     D98           D98HEI
746800930423     C* ORE X FEST. CONS. CLIENTE
746900941228     C                   Z-ADD     W98HEC        W98
747000941228     C                   EXSR      W98999
747100941228     C                   Z-ADD     D98           D98HEC
747200930511     C* ORE X F.DEPOSITO INTERNE
747300941228     C                   Z-ADD     W98HDI        W98
747400941228     C                   EXSR      W98999
747500941228     C                   Z-ADD     D98           D98HDI
747600930511     C* ORE X F.DEPOSITO CLIENTE
747700941228     C                   Z-ADD     W98HDC        W98
747800941228     C                   EXSR      W98999
747900941228     C                   Z-ADD     D98           D98HDC
748000930511     C* ORE X CONTRASSEGNO INTERNE
748100941228     C                   Z-ADD     W98HOI        W98
748200941228     C                   EXSR      W98999
748300941228     C                   Z-ADD     D98           D98HOI
748400930511     C* ORE X CONTRASSEGNO CLIENTE
748500941228     C                   Z-ADD     W98HOC        W98
748600941228     C                   EXSR      W98999
748700941228     C                   Z-ADD     D98           D98HOC
748800930914     C* ORE DI CONSEGNABILITA' INTERNE
748900941228     C                   Z-ADD     W98OCI        W98
749000941228     C                   EXSR      W98000
749100941228     C                   Z-ADD     D98           D98OCI
749200930914     C* ORE DI CONSEGNABILITA' CLIENTE
749300941228     C                   Z-ADD     W98OCC        W98
749400941228     C                   EXSR      W98000
749500941228     C                   Z-ADD     D98           D98OCC
749600071122     C
749700071122     c* Per i codici bolla M prepagati annullati, azzero tutto
749800071122     c                   if        d98cbo='M '
749900071122     c                   clear                   d98rpi
750000071122     c                   clear                   d98rpc
750100071122     c                   clear                   d98rti
750200071122     c                   clear                   d98rtc
750300071122     c                   clear                   d98rci
750400071122     c                   clear                   d98rcc
750500071122     c                   clear                   d98rri
750600071122     c                   clear                   d98rrc
750700071122     c                   clear                   d98fpi
750800071122     c                   clear                   d98fpc
750900071122     c                   clear                   d98fti
751000071122     c                   clear                   d98ftc
751100071122     c                   clear                   d98fci
751200071122     c                   clear                   d98fcc
751300071122     c                   clear                   d98fri
751400071122     c                   clear                   d98frc
751500071122     c                   clear                   d98npi
751600071122     c                   clear                   d98npc
751700071122     c                   clear                   d98nti
751800071122     c                   clear                   d98ntc
751900071122     c                   clear                   d98nci
752000071122     c                   clear                   d98ncc
752100071122     c                   clear                   d98nri
752200071122     c                   clear                   d98nrc
752300071122     c                   clear                   d98OCi
752400071122     c                   clear                   d98OCC
752500071122     c                   endif
752600930423     C*
752700100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
752800100308     c*  calcolo
752900100308     c                   eval      d98dcr=s98dcr
753000100308     c                   eval      d98hcr=s98hcr
753100100308     c                   eval      d98tcr=s98tcr
753200160405     c
753300160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
753400160405     c*  perchè il pgm usa il campo verificado se =10 )
753500160405     c                   if        wVP='S'
753600160405     c                   add       24            d98tci
753700160405     c                   add       24            d98tcc
753800160405     c                   add       24            d98tri
753900160405     c                   add       24            d98trc
754000160405     c                   endif
754100100308     c
754200930423     C                   ENDSR
754300941228     C***********************************************************************
754400941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
754500941228     C***********************************************************************
754600941228     C     W98999        BEGSR
754700941228     C     W98           IFLT      -999
754800941228     C                   Z-ADD     -999          D98               3 0
754900941228     C                   ELSE
755000941228     C     W98           IFGT      999
755100941228     C                   Z-ADD     999           D98
755200941228     C                   ELSE
755300941228     C                   Z-ADD     W98           D98
755400941228     C                   END
755500941228     C                   END
755600941228     C                   ENDSR
755700941228     C***********************************************************************
755800941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
755900941228     C***********************************************************************
756000941228     C     W98000        BEGSR
756100941228     C     W98           IFLE      *ZEROS
756200941228     C                   Z-ADD     *ZEROS        D98
756300941228     C                   ELSE
756400941228     C     W98           IFGT      999
756500941228     C                   Z-ADD     999           D98
756600941228     C                   ELSE
756700941228     C                   Z-ADD     W98           D98
756800941228     C                   END
756900941228     C                   END
757000941228     C                   ENDSR
757100930305     C***********************************************************************
757200930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
757300930305     C***********************************************************************
757400930305     C     AZZERA        BEGSR
757500150219     c* data e ora del momento
757600150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
757700150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
757800150219     C                   Z-ADD     DTORA4        WHHODI            4 0
757900930305     C*
758000141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
758100150603     C                   MOVEL     'N'           opendan
758200141107     c                   clear                   or_ddc
758300141108     c                   clear                   sav_12dat         8 0
758400141107     c                   clear                   or_datarif
758500140402     c                   clear                   wpick2            1
758600140402     c                   clear                   Primotipo
758700090928     c                   clear                   Primodev
758800090928     c                   clear                   Primohev
758900070827     c                   clear                   CallAR5
759000071122     c                   clear                   d98cbo
759100100308     c                   clear                   s98dcr
759200100308     c                   clear                   s98hcr
759300100308     c                   clear                   s98tcr
759400141106     c                   clear                   nosomma           1
759500141229     c                   clear                   wtrasm
759600150324     c                   clear                   avvcmc
759700150325     c                   clear                   DSbl4L
759800160401     c                   clear                   dsbl4I
759900150401     c                   clear                   fnlv82ds
760000150331     c                   clear                   DS7R
760100150605     c                   clear                   wDEE              1
760200150629     c                   clear                   wtsogliaE
760300930323     C                   MOVEL     '0'           D98ERR
760400930323     C                   MOVEL     *BLANKS       D98MSG
760500931124     C                   Z-ADD     *ZEROS        D98TPI
760600931124     C                   Z-ADD     *ZEROS        D98TPC
760700930325     C                   Z-ADD     *ZEROS        D98TTI
760800930325     C                   Z-ADD     *ZEROS        D98TTC
760900930325     C                   Z-ADD     *ZEROS        D98TCI
761000930325     C                   Z-ADD     *ZEROS        D98TCC
761100090827     C                   Z-ADD     *ZEROS        w99TCI
761200090827     C                   Z-ADD     *ZEROS        w99TCC
761300930325     C                   Z-ADD     *ZEROS        D98TRI
761400930325     C                   Z-ADD     *ZEROS        D98TRC
761500090921     C                   Z-ADD     *ZEROS        w99TRI
761600090921     C                   Z-ADD     *ZEROS        w99TRC
761700931124     C                   Z-ADD     *ZEROS        D98RPI
761800931124     C                   Z-ADD     *ZEROS        D98RPC
761900930405     C                   Z-ADD     *ZEROS        D98RTI
762000930405     C                   Z-ADD     *ZEROS        D98RTC
762100930405     C                   Z-ADD     *ZEROS        D98RCI
762200930405     C                   Z-ADD     *ZEROS        D98RCC
762300930405     C                   Z-ADD     *ZEROS        D98RRI
762400930405     C                   Z-ADD     *ZEROS        D98RRC
762500931124     C                   Z-ADD     *ZEROS        D98FPI
762600931124     C                   Z-ADD     *ZEROS        D98FPC
762700930325     C                   Z-ADD     *ZEROS        D98FTI
762800930325     C                   Z-ADD     *ZEROS        D98FTC
762900930325     C                   Z-ADD     *ZEROS        D98FCI
763000930325     C                   Z-ADD     *ZEROS        D98FCC
763100930325     C                   Z-ADD     *ZEROS        D98FRI
763200930325     C                   Z-ADD     *ZEROS        D98FRC
763300931124     C                   MOVEL     *BLANKS       D98CPI
763400931124     C                   MOVEL     *BLANKS       D98CPC
763500930325     C                   MOVEL     *BLANKS       D98CTI
763600930325     C                   MOVEL     *BLANKS       D98CTC
763700930325     C                   MOVEL     *BLANKS       D98CCI
763800930325     C                   MOVEL     *BLANKS       D98CCC
763900930325     C                   MOVEL     *BLANKS       D98CRI
764000930325     C                   MOVEL     *BLANKS       D98CRC
764100931124     C                   Z-ADD     *ZEROS        D98NPI
764200931124     C                   Z-ADD     *ZEROS        D98NPC
764300930325     C                   Z-ADD     *ZEROS        D98NTI
764400930325     C                   Z-ADD     *ZEROS        D98NTC
764500930325     C                   Z-ADD     *ZEROS        D98NCI
764600930325     C                   Z-ADD     *ZEROS        D98NCC
764700930325     C                   Z-ADD     *ZEROS        D98NRI
764800930325     C                   Z-ADD     *ZEROS        D98NRC
764900930414     C                   Z-ADD     *ZEROS        D98HHI
765000930414     C                   Z-ADD     *ZEROS        D98HHC
765100930414     C                   Z-ADD     *ZEROS        D98HOI
765200930414     C                   Z-ADD     *ZEROS        D98HOC
765300930414     C                   Z-ADD     *ZEROS        D98HLI
765400930414     C                   Z-ADD     *ZEROS        D98HLC
765500930414     C                   Z-ADD     *ZEROS        D98HGI
765600930414     C                   Z-ADD     *ZEROS        D98HGC
765700931124     C                   Z-ADD     *ZEROS        D98HPI
765800931124     C                   Z-ADD     *ZEROS        D98HPC
765900930414     C                   Z-ADD     *ZEROS        D98HFI
766000930414     C                   Z-ADD     *ZEROS        D98HFC
766100930416     C                   Z-ADD     *ZEROS        D98HEI
766200930416     C                   Z-ADD     *ZEROS        D98HEC
766300930414     C                   Z-ADD     *ZEROS        D98HRI
766400930414     C                   Z-ADD     *ZEROS        D98HRC
766500930511     C                   Z-ADD     *ZEROS        D98HDI
766600930511     C                   Z-ADD     *ZEROS        D98HDC
766700930325     C                   Z-ADD     *ZEROS        D98DCI
766800930325     C                   Z-ADD     *ZEROS        D98HCI
766900930325     C                   Z-ADD     *ZEROS        D98DCE
767000930325     C                   Z-ADD     *ZEROS        D98HCE
767100931125     C                   Z-ADD     *ZEROS        D98OCI
767200931125     C                   Z-ADD     *ZEROS        D98OCC
767300931202     C                   Z-ADD     *ZEROS        D98DEI
767400931202     C                   Z-ADD     *ZEROS        D98OEI
767500931202     C                   Z-ADD     *ZEROS        D98DEE
767600931202     C                   Z-ADD     *ZEROS        D98OEE
767700151019     c                   CLEAR                   D98FLP
767800161107     c                   CLEAR                   D98Gfas
767900150325     C                   clear                   D98dtctd
768000150326     C                   clear                   D98picklim
768100150325     C                   clear                   D98spcdee
768200150331     C                   clear                   fddata
768300140825     c
768400141017     c                   clear                   d98ddc
768500141017     c                   clear                   d98cons
768600160506     c                   clear                   d98disf
768700141017     c                   clear                   wd98ndc
768800140910     c                   clear                   wd98fbc
768900150119     c                   clear                   wd98acc
769000140825     c                   clear                   wtrulor
769100160401     c                   clear                   wVP               1
769200150219     c
769300150219     c                   clear                   IMPdata
769400150219     c                   clear                   IMPfgs
769500150219     c                   clear                   DISdata
769600150219     c                   clear                   DISfgs
769700150219     c                   clear                   DISora
769800150219     c                   clear                   DISnpg
769900150219     c                   clear                   IDnpg
770000150219     c                   clear                   IDdata
770100150219     c                   clear                   IDora
770200150219     c                   clear                   IDfgs
770300931125     C* CAMPI VARI
770400931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
770500140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
770600140825      *   DALLA LETTURA DI QUESTI FILE                               *
770700931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
770800931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
770900931125     C     D98NSP        IFNE      *ZEROS
771000930525     C                   MOVEL     *BLANKS       D98TSP
771100941228     C                   MOVEL     *BLANKS       D98CAD
771200970728     C                   MOVEL     *BLANKS       D98LOD
771300930525     C                   Z-ADD     *ZEROS        D98TFP
771400930525     C                   Z-ADD     *ZEROS        D98TFA
771500930525     C                   Z-ADD     *ZEROS        D98LNA
771600930525     C                   Z-ADD     *ZEROS        D98DSP
771700941228     C                   Z-ADD     *ZEROS        D98MGS
771800930525     C                   Z-ADD     *ZEROS        D98DTI
771900930525     C                   Z-ADD     *ZEROS        D98HTI
772000930525     C                   Z-ADD     *ZEROS        D98DCM
772100930525     C                   Z-ADD     *ZEROS        D98HMC
772200930525     C                   MOVEL     *BLANKS       D98GC1
772300930525     C                   MOVEL     *BLANKS       D98GC2
772400010820     C                   Z-ADD     *ZEROS        D98CAS
772500010821     C                   MOVEL     *BLANKS       D98VCA
772600930525     C                   MOVEL     *BLANKS       D98TIC
772700930525     C                   Z-ADD     *ZEROS        D98DCR
772800930525     C                   Z-ADD     *ZEROS        D98HCR
772900930525     C                   MOVEL     *BLANKS       D98TCR
773000050914     C                   MOVEL     *BLANKS       Consideraora      1
773100930525     C                   MOVEL     *BLANKS       D98CMC
773200930525     C                   Z-ADD     *ZEROS        D98DAG
773300930525     C                   Z-ADD     *ZEROS        D98DCG
773400930525     C                   MOVEL     *BLANKS       D98FFD
773500930525     C                   Z-ADD     *ZEROS        D98NCD
773600930525     C                   MOVEL     *BLANKS       D98FNS
773700931124     C                   Z-ADD     *ZEROS        D98DAM
773800940628     C                   MOVEL     *BLANKS       D98FIN
773900940628     C                   Z-ADD     *ZEROS        D98KSC
774000940628     C                   MOVEL     *BLANKS       D98TBL
774100931125     C                   END
774200960229     C     D98TLA        IFEQ      'T'
774300960229     C                   Z-ADD     *ZEROS        D98DTI
774400960229     C                   Z-ADD     *ZEROS        D98HTI
774500960229     C                   ENDIF
774600931125     C* CAMPI WORK
774700930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
774800930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
774900150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
775000930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
775100941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
775200960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
775300930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
775400930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
775500940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
775600930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
775700150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
775800931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
775900931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
776000150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
776100930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
776200930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
776300930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
776400930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
776500930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
776600940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
776700941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
776800930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
776900050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
777000050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
777100140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
777200930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
777300930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
777400930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
777500930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
777600930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
777700930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
777800930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
777900930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
778000930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
778100930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
778200930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
778300930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
778400930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
778500930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
778600140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
778700931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
778800060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
778900960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
779000931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
779100140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
779200140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
779300150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
779400140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
779500941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
779600970304     C******************** Z-ADD*ZEROS    W98DBR  80
779700970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
779800150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
779900050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
780000930414     C*
780100941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
780200941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
780300930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
780400930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
780500930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
780600930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
780700930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
780800930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
780900941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
781000941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
781100930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
781200930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
781300930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
781400930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
781500930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
781600930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
781700941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
781800941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
781900930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
782000930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
782100930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
782200930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
782300930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
782400930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
782500930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
782600930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
782700060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
782800060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
782900060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
783000060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
783100930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
783200930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
783300930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
783400930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
783500930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
783600930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
783700930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
783800930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
783900930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
784000930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
784100930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
784200930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
784300930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
784400930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
784500941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
784600941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
784700930423     C*
784800930511     C                   Z-ADD     *ZEROS        W98HF             5 0
784900930423     C                   Z-ADD     *ZEROS        W98HH             5 0
785000060418     C                   Z-ADD     *ZEROS        W98EG             5 0
785100060420     C                   Z-ADD     *ZEROS        W98EA             5 0
785200930423     C                   Z-ADD     *ZEROS        W98HG             5 0
785300930423     C                   Z-ADD     *ZEROS        W98HL             5 0
785400930423     C                   Z-ADD     *ZEROS        W98HO             5 0
785500930511     C                   Z-ADD     *ZEROS        W98HD             5 0
785600930611     C*
785700930611     C                   Z-ADD     *ZEROS        WORELA            3 0
785800960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
785900930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
786000930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
786100931207     C                   Z-ADD     *ZEROS        WORECR            3 0
786200090902     c
786300090902     c                   clear                   wmancaTRAZ
786400150113     c                   clear                   wcaldpcee         1
786500150220     c                   clear                   evemic            2
786600930305     C*
786700060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
786800060406     C                   clear                   LAF                            *LASC.AVVISO FINE
786900091014     C                   clear                   LAp                            *posizione l.avviso
787000060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
787100060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
787200060406c    c                   clear                   LCon
787300060406     C                   clear                   EVD                            *DATA EVENTO
787400060406     C                   clear                   EVO                            *ORA EVENTO
787500060406     C                   clear                   EVC                            *CAUSALE EVENTO
787600131218     C                   clear                   EVT
787700060406     C                   clear                   pVD                            *DATA EVENTO
787800060406     C                   clear                   pVO                            *ORA EVENTO
787900060406     C                   clear                   pVC                            *CAUSALE EVENTO
788000060406     C                   clear                   pVF                            *CAUSALE EVENTO
788100060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
788200060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
788300060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
788400060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
788500060406     C                   clear                   COV                            *DATE FINE L.A. E GI
788600060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
788700060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
788800091014     C                   clear                   GIp                            *posizione vento gia
788900060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
789000060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
789100060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
789200931213     C*
789300060406     C                   clear                   TUC                            *TURNI CHIUSURA
789400931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
789500931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
789600060406     c
789700060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
789800060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
789900060406     c                   clear                   pgii
790000060406     c                   clear                   pgif
790100060406     c                   clear                   pgci
790200060406     c                   clear                   pgcf
790300060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
790400060420     c                   clear                   paii
790500060420     c                   clear                   paif
790600930420     C*
790700930305     C                   ENDSR
790800930512     C***********************************************************************
790900930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
791000930512     C***********************************************************************
791100930512     C     FORCON        BEGSR
791200930512     C*
791300931124     C                   Z-ADD     *ZEROS        D98RPI
791400931124     C                   Z-ADD     *ZEROS        D98RPC
791500930512     C                   Z-ADD     *ZEROS        D98RTI
791600930512     C                   Z-ADD     *ZEROS        D98RTC
791700930512     C                   Z-ADD     *ZEROS        D98RCI
791800930512     C                   Z-ADD     *ZEROS        D98RCC
791900930512     C                   Z-ADD     *ZEROS        D98RRI
792000930512     C                   Z-ADD     *ZEROS        D98RRC
792100931124     C                   Z-ADD     *ZEROS        D98FPI
792200931124     C                   Z-ADD     *ZEROS        D98FPC
792300930512     C                   Z-ADD     *ZEROS        D98FTI
792400930512     C                   Z-ADD     *ZEROS        D98FTC
792500930512     C                   Z-ADD     *ZEROS        D98FCI
792600930512     C                   Z-ADD     *ZEROS        D98FCC
792700930512     C                   Z-ADD     *ZEROS        D98FRI
792800930512     C                   Z-ADD     *ZEROS        D98FRC
792900931124     C                   MOVEL     'S'           D98CPI
793000931124     C                   MOVEL     'S'           D98CPC
793100930512     C                   MOVEL     'S'           D98CTI
793200930512     C                   MOVEL     'S'           D98CTC
793300930512     C                   MOVEL     'S'           D98CCI
793400930512     C                   MOVEL     'S'           D98CCC
793500930512     C                   MOVEL     'S'           D98CRI
793600930512     C                   MOVEL     'S'           D98CRC
793700931124     C                   Z-ADD     *ZEROS        D98NPI
793800931124     C                   Z-ADD     *ZEROS        D98NPC
793900930512     C                   Z-ADD     *ZEROS        D98NTI
794000930512     C                   Z-ADD     *ZEROS        D98NTC
794100930512     C                   Z-ADD     *ZEROS        D98NCI
794200930512     C                   Z-ADD     *ZEROS        D98NCC
794300930512     C                   Z-ADD     *ZEROS        D98NRI
794400930512     C                   Z-ADD     *ZEROS        D98NRC
794500930512     C                   Z-ADD     *ZEROS        D98HHI
794600930512     C                   Z-ADD     *ZEROS        D98HHC
794700930512     C                   Z-ADD     *ZEROS        D98HOI
794800930512     C                   Z-ADD     *ZEROS        D98HOC
794900930512     C                   Z-ADD     *ZEROS        D98HLI
795000930512     C                   Z-ADD     *ZEROS        D98HLC
795100930512     C                   Z-ADD     *ZEROS        D98HGI
795200930512     C                   Z-ADD     *ZEROS        D98HGC
795300931124     C                   Z-ADD     *ZEROS        D98HPI
795400931124     C                   Z-ADD     *ZEROS        D98HPC
795500930512     C                   Z-ADD     *ZEROS        D98HFI
795600930512     C                   Z-ADD     *ZEROS        D98HFC
795700930512     C                   Z-ADD     *ZEROS        D98HEI
795800930512     C                   Z-ADD     *ZEROS        D98HEC
795900930512     C                   Z-ADD     *ZEROS        D98HRI
796000930512     C                   Z-ADD     *ZEROS        D98HRC
796100930512     C                   Z-ADD     *ZEROS        D98HDI
796200930512     C                   Z-ADD     *ZEROS        D98HDC
796300930520     C*
796400930512     C                   ENDSR
796500930305     C********************************************************************
796600971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
796700950406031059090120151181212243273304334365
796800950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
796900950406312831303130313130313031
797000930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
797100971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
797200971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
797300971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
797400971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
797500971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
797600971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
797700971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
797800971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
797900971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
798000971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
798100971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
798200971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
798300971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
798400971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
798500971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
798600971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
798700971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
798800971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
798900971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
799000971127TNSD99R- Bolla inesistente.                                               BOIN20
799100971127TNSD99R-     Cap inesistente.                                             CAPI21
799200971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
799300971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
799400971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
799500971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
799600971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
799700971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
799800971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
799900971127TNSD99R- Contrassegno non trovato.                                        CONT29
800000971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
800100990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
800200990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
800300060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
800400060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
800500971127TNSD99R- 35                                                                   35
800600971127TNSD99R- 36                                                                   36
800700971127TNSD99R- 37                                                                   37
800800971127TNSD99R- 38                                                                   38
800900971127TNSD99R- 39                                                                   39
801000971127TNSD99R- 40                                                                   40
