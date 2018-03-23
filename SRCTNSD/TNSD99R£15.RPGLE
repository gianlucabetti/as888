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
074300170104     c
074400170105     C                   Z-ADD     ARBDBR        d98dfv
074500141215     c                   eval      d98fdrt=' '
074600070511     c
074700141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
074800070511     c* La data ritiro è presente solo su fnblp
074900070511     C     KEYARB        CHAIN     FNblp01L                           99
075000141215     c                   if        %found(fnblp01l)
075100070511     C                   Z-ADD     blpDrt        D98DBR
075200070511     c                   eval      d98fdrt='S'
075300070511     c                   endif
075400141215     c******             endif
075500070511     c
075600110923     C                   movel     ARBxco        D98xco
075700110923     C                   Z-ADD     ARBDAM        D98DAM
075800941227     C                   MOVEL     ARBFIN        D98FIN
075900990308     C                   MOVEL     ARBFBR        WFBR
076000031201     C                   MOVEL     ARBGGA        WGGA
076100050901     C                   MOVEL     ARBcca        d98cca
076200051124     C                   MOVEL     ARBTC1        d98TC1
076300051124     C                   MOVEL     ARBTC2        d98TC2
076400941227     C                   Z-ADD     1             I
076500941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
076600941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
076700941227     C                   MOVEL     '1'           D98ERR
076800941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
076900941227     C                   ELSE                                                   <-- 2 -->
077000941227     C                   MOVEL     T3A(I)        D98TBL
077100941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
077200990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
077300941227     C                   END                                                    <-- 2 ---
077400130826
077500130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
077600130826     c                   if        %subst(d98tbl:1:1) ='F'
077700130826     C                   Z-ADD     ARBKSC        D98KSC
077800130826     c                   else
077900130826     c
078000130826     c                   if        arbccm>0
078100130826     C                   Z-ADD     ARBccm        D98KSC
078200130826     c                   endif
078300130826     c                   if        d98ksc=0
078400130826     c                   z-add     8888          d98ksc
078500130826     c                   movel     d98lnp        d98ksc
078600130826     c                   endif
078700130826     c                   endif
078800130826     c
078900941227     C                   END                                                    <-- 1 ---
079000941228     C*
079100051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
079200950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
079300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
079400941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
079500941228     C                   Z-ADD     D98AAS        AR9AAS
079600941228     C                   Z-ADD     D98LNP        AR9LNP
079700941228     C                   Z-ADD     D98NRS        AR9NRS
079800941228     C                   Z-ADD     D98NSP        AR9NSP
079900051115     C     KEYAR9        CHAIN     FiAR901L                           99
080000950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
080100950102     C                   MOVEL     'S'           WFLASS
080200950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
080300950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
080400950102     C                   Z-ADD     D98AAS        LBLAAN
080500950102     C                   Z-ADD     D98LNP        LBLLPN
080600950102     C                   Z-ADD     D98NRS        LBLNRN
080700950102     C                   Z-ADD     D98NSP        LBLNSN
080800950102     C     KEYLBL        CHAIN     FNLBL01L                           99
080900950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
081000950102     C                   Z-ADD     LBLAAP        AR9AAS
081100950102     C                   Z-ADD     LBLLPP        AR9LNP
081200950102     C                   Z-ADD     LBLNRP        AR9NRS
081300950102     C                   Z-ADD     LBLNSP        AR9NSP
081400051115     C     KEYAR9        CHAIN     FiAR901L                           99
081500950102     C     *IN99         IFEQ      '0'
081600950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
081700950102     C                   ENDIF
081800950102     C                   ENDIF                                                  <-- 3 ---
081900950102     C                   ENDIF                                                  <-- 2 ---
082000950102     C*
082100950102     C* CONTRASSEGNO NON TROVATO
082200950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
082300941228     C                   ELSE                                                   <-- 2 -->
082400950102     C* CONTRASSEGNO TROVATO
082500941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
082600010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
082700010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
082800941228     C                   ENDIF                                                  <-- 2 ---
082900941228     C                   ENDIF                                                  <-- 1 ---
083000150325     c
083100941227     C*
083200050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
083300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
083400050405     C                   Z-ADD     D98AAS        KWGCPAAS
083500050405     C                   Z-ADD     D98LNP        KWGCPLNP
083600050405     C                   Z-ADD     D98NRS        KWGCPNRS
083700050405     C                   Z-ADD     D98NSP        KWGCPNSP
083800050405     C                   Z-ADD     *ZEROS        KWGCPFRG
083900050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
084000941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
084100941227     C     GCPATB        ANDNE     'A'
084200960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
084300960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
084400960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
084500161107$$$$ C                   movel     gcpfas        D98gfas
084600050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
084700960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
084800960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
084900050405     C* O UGUALE A 35 INVECE DI 40)
085000960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
085100050405     C     GCPFAS        ANDGE     40
085200941227     C                   Z-ADD     GCPDCG        D98DCG
085300950112     C                   Z-ADD     GCPDCG        W98DCG
085400960123     C                   ENDIF
085500960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
085600960729     C     GCPFAS        ANDGE     35
085700070202     C     GCPFAS        ANDne     36
085800960729     C                   Z-ADD     GCPDCG        D98DCG
085900960729     C                   Z-ADD     GCPDCG        W98DCG
086000960729     C                   ENDIF
086100941228     C                   ENDIF
086200941228     C                   ENDIF                                                  <-- 1 ---
086300970911     C*
086400970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
086500970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
086600970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
086700050922     C*****              Z-ADD     D98DTI        W98DTI
086800971210     C                   Z-ADD     D98LNP        F
086900971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
087000970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
087100970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
087200150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
087300150120     c                   if        arbdcp=0 and d98dfc=0
087400150120     C                   Z-ADD     d98dam        D98DTI
087500150120     c                   endif
087600150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
087700150120     C                   Z-ADD     d98dam        D98DTI
087800150120     c                   endif
087900150120     c                   ENDIF
088000150120     c***
088100150120     C***                Z-ADD     D98AAS        ARTAAS
088200150120     C***                Z-ADD     D98LNP        ARTLNP
088300150120     C***                Z-ADD     D98NRS        ARTNRS
088400150120     C***                Z-ADD     D98NSP        ARTNSP
088500150120     C***  KEYART        SETLL     FNART01L                           99
088600150120     C**N99KEYART        READE     FNART01L                               99
088700150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
088800150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
088900970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
089000150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
089100970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
089200970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
089300150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
089400150120     C***  ARTDCM        ANDEQ     *ZEROS
089500970912     C* SE PRIMO REC. TROVATO, MEM. DATA
089600150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
089700150120     C***                Z-ADD     ARTDAM        D98DTI
089800150120     C***                ELSE                                                   <-- 6 -->
089900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
090000150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
090100150120     C***                Z-ADD     ARTDAM        D98DTI
090200150120     C***                ENDIF                                                  <-- 7 ---
090300150120     C***                ENDIF                                                  <-- 6 ---
090400150120     C***                ENDIF                                                  <-- 5 ---
090500970911     C* C'E' DATA CONSEGNAB.
090600150120     C***                ELSE                                                   <-- 4 -->
090700970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
090800970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
090900150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
091000150120     C***  ARTDAM        ANDLE     D98DFC
091100970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
091200970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
091300150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
091400150120     C***  ARTDCM        ORGT      D98DFC
091500970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
091600150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
091700150120     C***                Z-ADD     ARTDAM        D98DTI
091800150120     C***                ELSE                                                   <-- 7 -->
091900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
092000150120     C***  ARTDAM        IFLT      D98DTI
092100150120     C***                Z-ADD     ARTDAM        D98DTI
092200150120     C***                ENDIF
092300150120     C***                ENDIF                                                  <-- 7 ---
092400150120     C***                ENDIF                                                  <-- 6 ---
092500150120     C***                ENDIF                                                  <-- 5 ---
092600150120     C***                ENDIF                                                  <-- 4 ---
092700150120     C***                ENDIF                                                  <-- 3 ---
092800150120     C***  KEYART        READE     FNART01L                               99
092900150120     C***                ENDDO                                                  <-- 2 ---
093000150120     C***                ENDIF                                                  <-- 1 ---
093100941227     C*
093200941227     C                   ENDSR
093300150325     C********************************************************************
093400150325     C*   Verifico se bolla in distinta
093500150325     C********************************************************************
093600150325     c     IN_dist       BEGSR
093700150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
093800150325     c                   if         ARBfbc = *blanks and
093900150325     c                               ARBddc > 0 and
094000150325     c                               ARBndc > 0 and ARBdcm=0
094100150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
094200150325     c                   if        not %open(fiapd01l)
094300150325     c                   open      fiapd01l
094400150325     c                   endif
094500150325     c
094600150325     C                   move      'A'           kpdtip
094700150325     c                   eval      kpdpdr=arbpdc
094800150325     C     kfapd         chain     fiapd01l
094900150325     c
095000150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
095100150325     c                   eval       d98cons = 'S'
095200160506     c                   else
095300160506     c                   eval       d98disf = 'F'
095400150325     c                   endif
095500150325     c                   endif
095600150325     c                   ENDSR
095700941227     C********************************************************************
095800941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
095900941227     C********************************************************************
096000941227     C     LETBLP        BEGSR
096100941227     C*
096200941227     C                   Z-ADD     D98AAS        BLPAAS
096300941227     C                   Z-ADD     D98LNP        BLPLNP
096400941227     C                   Z-ADD     D98NRS        BLPNRS
096500941227     C                   Z-ADD     D98NSP        BLPNSP
096600941227     C     KEYBLP        CHAIN     FNBLP01L                           99
096700941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
096800941227     C                   MOVEL     '1'           D98ERR
096900941227     C                   MOVEL     MSG(20)       D98MSG
097000941227     C                   END                                                    <-- 1 ---
097100941228     C     *IN99         IFEQ      '0'                                                    *TROV.
097200041028     C     KEYBLP        CHAIN     FNarb01L
097300041028     c                   if        %found(fnarb01l)
097400041028     c                   eval      wesarb='S'
097500041028     c                   else
097600041028     c                   eval      wesarb='N'
097700150219     c                   eval      wtrasm='D'
097800041028     c                   endif
097900141216     c
098000141216     c* se non trasmessa la bolla in arrivo memorizzo
098100150219     c                   if        blpft1= ' '
098200141229     c                   eval      wtrasm='N'
098300141216     c                   endif
098400141216     c
098500110923     C                   MOVEL     BLPxco        D98xco
098600110923     C                   MOVEL     BLPTSP        D98TSP
098700130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
098800130529     c                   if        i98tsp_for<>' '
098900130529     C                   MOVEL     i98tsp_for    D98TSP
099000130529     c                   endif
099100960209     C                   MOVEL     BLPNZD        D98NZD
099200941228     C                   MOVEL     BLPCAD        D98CAD
099300970701     C                   MOVEL     BLPLOD        D98LOD
099400941227     C                   Z-ADD     BLPTFP        D98TFP
099500941227     C                   Z-ADD     BLPTFA        D98TFA
099600941227     C                   Z-ADD     BLPLNA        D98LNA
099700941227     C                   Z-ADD     BLPAAS        DSAAS
099800941227     C                   Z-ADD     BLPMGS        DSMGS
099900941227     C                   Z-ADD     DSDAT8        D98DSP
100000941227     C                   Z-ADD     BLPMGS        D98MGS
100100170104
100200170104     c* se si tratta di prepagato NON confermato, devo fare finta che sia bollettato oggi
100300170104     c                   if        wtrasm='N'  and blppdr=0
100400170104     c                   movel     wdaodi        d98dsp
100500170104     c                   z-add     wdaodi        d98mgs
100600170104     c                   endif
100700170104     c
100800051124     C                   movel     BLPTC1        D98TC1
100900051124     C                   movel     BLPTC2        D98TC2
101000041028     c                   if        wesarb='S'
101100041028     C                   Z-ADD     arbDTI        D98DTI
101200041028     C                   Z-ADD     arbHTI        D98HTI
101300041028     C                   Z-ADD     arbDCM        D98DCM
101400041028     C                   Z-ADD     arbHMC        D98HMC
101500041028     C                   Z-ADD     arbDAM        D98DAM
101600050901     C                   MOVEL     arbcca        d98cca
101700140820     C                   MOVEL     arbddc        d98ddc
101800140911     C                   z-add     arbndc        wd98ndc
101900140910     C                   MOVEL     arbfbc        wd98fbc
102000150119     c                   movel     arbacc        wd98acc
102100150325     c                   exsr      IN_DIST
102200140820     c*
102300041028     c                   else
102400941227     C                   Z-ADD     BLPDTI        D98DTI
102500941227     C                   Z-ADD     BLPHTI        D98HTI
102600941227     C                   Z-ADD     BLPDCM        D98DCM
102700941227     C                   Z-ADD     BLPHMC        D98HMC
102800041028     C                   Z-ADD     BLPDAM        D98DAM
102900050901     C                   MOVEL     BLPcca        d98cca
103000140820     C                   MOVEL     BLPddc        d98ddc
103100140825     C                   MOVEL     BLPndc        wd98ndc
103200140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
103300140820     c                   clear                   d98cons
103400041028     c                   endif
103500941227     C                   MOVEL     BLPGC1        D98GC1
103600941227     C                   MOVEL     BLPGC2        D98GC2
103700941227     C                   Z-ADD     BLPDCR        D98DCR
103800941227     C                   Z-ADD     BLPHCR        D98HCR
103900941227     C                   MOVEL     BLPTCR        D98TCR
104000100308     c* Salvo i campi perchè in alcuni casi li clearo
104100100308     C                   Z-ADD     blpDCR        s98DCR
104200100308     C                   Z-ADD     blpHCR        s98HCR
104300100308     C                   MOVEL     blpTCR        s98TCR
104400100308     c
104500941227     C                   MOVEL     *BLANKS       D98CMC
104600941227     C                   MOVEL     BLPFFD        D98FFD
104700941227     C                   Z-ADD     BLPNCD        D98NCD
104800941227     C                   MOVEL     BLPFNS        D98FNS
104900941228     C                   Z-ADD     BLPDUC        D98DUC
105000150508     C                   Z-ADD     BLPDPC        w98DPC
105100970310     C                   Z-ADD     *ZEROS        W98HUC
105200071122     c                   movel     blpcbo        d98cbo
105300150331     c*
105400150331     c* Se in fermo deposito vedo se presente particlarità consegna
105500150331     c                   if        blpffd='S' and blpgma<>*blanks
105600150331     C                   Z-ADD     CODUT         KTBUTE
105700150331     C                   MOVEL     '7R'          KTBCOD
105800150331     C                   MOVEL(P)  blpgma        KTBKEY
105900150331     C     KEYTAB        chain     TABEL00F
106000150331     c                   if        %found(tabel00f)
106100150331     c                   movel     tbluni        ds7r
106200150331     c                   endif
106300150331     c                   endif
106400150331     c
106500970304     C                   Z-ADD     BLPDBR        D98DBR
106600170105     C                   Z-ADD     BLPDBR        d98dfv
106700070514     c                   eval      d98fdrt=' '
106800070514     c
106900141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
107000070514     C                   Z-ADD     blpDrt        D98DBR
107100070514     c                   eval      d98fdrt='S'
107200141215     c********           endif
107300070514     c*
107400941227     C                   MOVEL     BLPFIN        D98FIN
107500130826     C****               Z-ADD     BLPKSC        D98KSC
107600990308     C                   MOVEL     BLPFBR        WFBR
107700031201     C                   MOVEL     BLPGGA        WGGA
107800941227     C                   Z-ADD     1             I
107900941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
108000941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
108100941227     C                   MOVEL     '1'           D98ERR
108200941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
108300941227     C                   ELSE                                                   <-- 2 -->
108400941227     C                   MOVEL     T3A(I)        D98TBL
108500941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
108600990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
108700941227     C                   END                                                    <-- 2 ---
108800130826     c                   if        %subst(d98tbl:1:1) ='F'
108900130826     C                   Z-ADD     blpKSC        D98KSC
109000130826     c                   else
109100130826     c
109200130826     c                   if        blpccm>0
109300130826     C                   Z-ADD     blpccm        D98KSC
109400130826     c                   endif
109500130826     c                   if        d98ksc=0
109600130826     c                   z-add     8888          d98ksc
109700130826     c                   movel     d98lnp        d98ksc
109800130826     c                   endif
109900130826     c                   endif
110000130826     c
110100941227     C                   END
110200941228     C*
110300051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
110400950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
110500941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
110600941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
110700051115     C                   Z-ADD     D98AAS        ar9AAS
110800051115     C                   Z-ADD     D98LNP        ar9LNP
110900051115     C                   Z-ADD     D98NRS        ar9NRS
111000051115     C                   Z-ADD     D98NSP        ar9NSP
111100051115     C     KEYar9        CHAIN     Fiar901L                           99
111200950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
111300950102     C                   MOVEL     'S'           WFLASS
111400950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
111500950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
111600950102     C                   Z-ADD     D98AAS        LBLAAN
111700950102     C                   Z-ADD     D98LNP        LBLLPN
111800950102     C                   Z-ADD     D98NRS        LBLNRN
111900950102     C                   Z-ADD     D98NSP        LBLNSN
112000950102     C     KEYLBL        CHAIN     FNLBL01L                           99
112100950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
112200051115     C                   Z-ADD     LBLAAP        ar9AAS
112300051115     C                   Z-ADD     LBLLPP        ar9LNP
112400051115     C                   Z-ADD     LBLNRP        ar9NRS
112500051115     C                   Z-ADD     LBLNSP        ar9NSP
112600051115     C     KEYar9        CHAIN     Fiar901L                           99
112700950102     C     *IN99         IFEQ      '0'
112800950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
112900950102     C                   ENDIF
113000950102     C                   ENDIF                                                  <-- 3 ---
113100950102     C                   ENDIF                                                  <-- 2 ---
113200950102     C*
113300950102     C* CONTRASSEGNO TROVATO
113400150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
113500051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
113600051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
113700051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
113800941228     C                   ENDIF                                                  <-- 2 ---
113900941228     C                   ENDIF                                                  <-- 1 ---
114000941227     C*
114100050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
114200941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
114300050405     C                   Z-ADD     D98AAS        KWGCPAAS
114400050405     C                   Z-ADD     D98LNP        KWGCPLNP
114500050405     C                   Z-ADD     D98NRS        KWGCPNRS
114600050405     C                   Z-ADD     D98NSP        KWGCPNSP
114700050405     C                   Z-ADD     *ZEROS        KWGCPFRG
114800050405     C     KEYGCP        CHAIN     TIGCP01L                           99
114900941228     C     *IN99         IFEQ      '0'                                          *TROV.
115000941227     C     GCPATB        ANDNE     'A'
115100960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
115200960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
115300960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
115400960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
115500161107$$$$ C                   movel     gcpfas        D98Gfas
115600050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
115700960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
115800960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
115900050405     C* O UGUALE A 35 INVECE DI 40)
116000960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
116100050405     C     GCPFAS        ANDGE     40
116200941227     C                   Z-ADD     GCPDCG        D98DCG
116300950112     C                   Z-ADD     GCPDCG        W98DCG
116400960123     C                   ENDIF
116500960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
116600960729     C     GCPFAS        ANDGE     35
116700070202     C     GCPFAS        ANDne     36
116800960729     C                   Z-ADD     GCPDCG        D98DCG
116900960729     C                   Z-ADD     GCPDCG        W98DCG
117000960729     C                   ENDIF
117100941228     C                   ENDIF
117200941228     C                   ENDIF                                                  <-- 1 ---
117300970912     C*
117400970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
117500970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
117600970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
117700050922     C******             Z-ADD     D98DTI        W98DTI
117800971210     C                   Z-ADD     D98LNP        F
117900971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
118000970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
118100970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
118200150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
118300150120     c                   if        blpdcp=0 and d98dfc=0
118400150120     C                   Z-ADD     d98dam        D98DTI
118500150120     c                   endif
118600150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
118700150120     C                   Z-ADD     d98dam        D98DTI
118800150120     c                   endif
118900150120     c                   ENDIF
119000150120     c*
119100150120     C***                Z-ADD     D98AAS        BLTAAS
119200150120     C****               Z-ADD     D98LNP        BLTLNP
119300150120     C***                Z-ADD     D98NRS        BLTNRS
119400150120     C***                Z-ADD     D98NSP        BLTNSP
119500150120     C***  KEYBLT        SETLL     FNBLT01L                           99
119600150120     C**N99KEYBLT        READE     FNBLT01L                               99
119700150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
119800150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
119900970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
120000150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
120100970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
120200970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
120300150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
120400150120     C**   BLTDCM        ANDEQ     *ZEROS
120500970912     C* SE PRIMO REC. TROVATO, MEM. DATA
120600150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
120700150120     C**                 Z-ADD     BLTDAM        D98DTI
120800150120     C**                 ELSE                                                   <-- 6 -->
120900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
121000150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
121100150120     C**                 Z-ADD     BLTDAM        D98DTI
121200150120     C**                 ENDIF                                                  <-- 7 ---
121300150120     C**                 ENDIF                                                  <-- 6 ---
121400150120     C**                 ENDIF                                                  <-- 5 ---
121500970912     C* C'E' DATA CONSEGNAB.
121600150120     **                  ELSE                                                   <-- 4 -->
121700970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
121800970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
121900150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
122000150120     C**   BLTDAM        ANDLT     D98DFC
122100970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
122200970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
122300150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
122400150120     C**   BLTDCM        ORGT      D98DFC
122500970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
122600150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
122700150120     C**                 Z-ADD     BLTDAM        D98DTI
122800150120     C**                 ELSE                                                   <-- 7 -->
122900970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
123000150120     C**   BLTDAM        IFLT      D98DTI
123100150120     C**                 Z-ADD     BLTDAM        D98DTI
123200150120     C**                 ENDIF
123300150120     C**                 ENDIF                                                  <-- 7 ---
123400150120     C**                 ENDIF                                                  <-- 6 ---
123500150120     C**                 ENDIF                                                  <-- 5 ---
123600150120     C**                 ENDIF                                                  <-- 4 ---
123700150120     C**                 ENDIF                                                  <-- 3 ---
123800150120     C**   KEYBLT        READE     FNBLT01L                               99
123900150120     C**                 ENDDO                                                  <-- 2 ---
124000150120     C**                 ENDIF                                                  <-- 1 ---
124100941227     C*
124200941227     C                   ENDSR
124300930323     C********************************************************************
124400941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
124500930323     C********************************************************************
124600930323     C     RTTEOR        BEGSR
124700941227     C*
124800941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
124900941228     C     D98ERR        IFEQ      '0'
125000941227     C                   EXSR      RTPART
125100941227     C                   ENDIF
125200941227     C*
125300941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
125400941228     C     D98ERR        IFEQ      '0'
125500150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
125600150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
125700150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
125800150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
125900150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
126000941227     C                   EXSR      RTTRAZ
126100150219     c                   if        d98err<>'1'
126200150219     C                   Z-ADD     CASTIN        D98TTI
126300150219     C                   Z-ADD     CASTCN        D98TTC
126400150219     C                   endif
126500150219     C                   ENDIF
126600941227     C*
126700941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
126800941228     C     D98ERR        IFEQ      '0'
126900941227     C                   EXSR      RTCONS
127000941227     C                   ENDIF
127100941227     C*
127200941228     C     D98ERR        IFEQ      '0'
127300941227     C*
127400941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
127500941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
127600941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
127700090921     c                   z-add     d98tri        w99tri
127800090921     c* per la DS
127900090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
128000090921     c* per i calcoli interi
128100090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
128200090921     c
128300941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
128400941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
128500941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
128600090921     c                   z-add     d98trc        w99trc
128700090921     c* per la DS
128800090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
128900090921     c* per i calcoli interi
129000090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
129100941227     C*
129200941227     C                   ENDIF
129300930323     C*
129400930323     C                   ENDSR
129500941227     C********************************************************************
129600941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
129700941227     C********************************************************************
129800941227     C     RTPART        BEGSR
129900941228     C*
130000941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
130100941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
130200941228     C                   MOVEL     D98TSP        WTSP              1
130300130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
130400130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
130500050301     C*******
130600050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
130700050301     C*
130800050301     C*    W001          IFEQ      'A'
130900050301     C*    W0040         OREQ      8888
131000050301     C*                  MOVEL     'E'           WTSP
131100050301     C*                  ENDIF
131200050301     C*******
131300941227     C*
131400941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
131500941227     C                   MOVEL     'N'           WTRAOK            1
131600941227     C                   MOVEL     'P'           CASPTA
131700941227     C                   Z-ADD     D98LNP        CASLNP
131800941227     C                   Z-ADD     *ZEROS        CASLNA
131900941228     C                   MOVEL     WTSP          CASTSP
132000941228     C                   Z-ADD     99999999      CASDDE
132100941227     C     KEYCAS        SETGT     AZCAS01L                             99
132200941227     C  N99              READP     AZCAS01L                               99
132300941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
132400941227     C     WTRAOK        ANDEQ     'N'
132500941227     C     CASLNP        ANDEQ     D98LNP
132600950102     C     CASTSP        ANDEQ     WTSP
132700941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
132800941227     C     CASATB        ANDEQ     *BLANKS
132900941227     C                   MOVEL     'S'           WTRAOK
133000941227     C                   ELSE                                                   <-- 2 -->
133100941227     C                   READP     AZCAS01L                               99
133200941227     C                   ENDIF                                                  <-- 2 ---
133300941227     C                   ENDDO                                                  <-- 1 ---
133400941227     C*
133500941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
133600941227     C     WTRAOK        IFEQ      'N'
133700941227     C                   MOVEL     '1'           D98ERR
133800941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
133900941227     C                   ELSE
134000941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
134100941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
134200941227     C                   END
134300970214     C*
134400140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
134500140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
134600140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
134700140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
134800140403     C**                 Z-ADD     D98LNA        F
134900140403     C**   D98ERR        IFEQ      '0'
135000140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
135100140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
135200140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
135300140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
135400140403     C**                 ENDIF
135500140403     C**                 ENDIF
135600140402     c
135700140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
135800140402     c*  se tempi  = 0
135900140402     c                   if        wpick2='S' and d98tpi<=0
136000140402     c                   add       24            D98TPI
136100140402     c                   endif
136200941227     C*
136300941227     C                   ENDSR
136400941227     C********************************************************************
136500941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
136600941227     C********************************************************************
136700941227     C     RTTRAZ        BEGSR
136800941227     C*
136900941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
137000150219     c
137100150219     C                   Z-ADD     D98LNA        LTA(1)
137200150219     C                   Z-ADD     D98LNA        LTA(2)
137300150219     C                   Z-ADD     D98TFA        LTA(3)
137400941227     C                   Z-ADD     D98TFA        LTA(4)
137500941227     C*
137600941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
137700941227     C                   MOVEL     'N'           WTRAOK            1
137800941227     C                   Z-ADD     1             K                 3 0
137900941227     C*
138000941227     C* CICLO FINO A:
138100941227     C*    - FINE SCHIERA
138200941227     C*    - TROVATO TEMPI VALIDI
138300941227     C     K             DOUGT     4                                            --- 1 -->
138400941227     C     WTRAOK        OREQ      'S'
138500941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
138600941227     C     LTA(K)        ORNE      *ZEROS
138700941227     C                   MOVEL     'T'           CASPTA
138800941227     C                   Z-ADD     LTP(K)        CASLNP
138900941227     C                   Z-ADD     LTA(K)        CASLNA
139000941227     C                   MOVEL     D98TSP        CASTSP
139100941228     C                   Z-ADD     99999999      CASDDE
139200941227     C     KEYCAS        SETGT     AZCAS01L                             99
139300941227     C  N99              READP     AZCAS01L                               99
139400941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
139500941227     C     WTRAOK        ANDEQ     'N'
139600941227     C     LTP(K)        ANDEQ     CASLNP
139700941227     C     LTA(K)        ANDEQ     CASLNA
139800941227     C     D98TSP        ANDEQ     CASTSP
139900960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
140000941227     C     CASATB        ANDEQ     *BLANKS
140100941227     C                   MOVEL     'S'           WTRAOK
140200941227     C                   ELSE                                                   <-- 4 -->
140300941227     C                   READP     AZCAS01L                               99
140400941227     C                   ENDIF                                                  <-- 4 ---
140500941227     C                   ENDDO                                                  <-- 3 ---
140600941227     C                   ENDIF                                                  <-- 2 ---
140700941227     C                   ADD       1             K
140800941227     C                   ENDDO                                                  <-- 1 ---
140900941227     C*
141000941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
141100941227     C     WTRAOK        IFEQ      'N'
141200941227     C                   MOVEL     '1'           D98ERR
141300941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
141400150219     C***                ELSE
141500150219     C***                Z-ADD     CASTIN        D98TTI
141600150219     C***                Z-ADD     CASTCN        D98TTC
141700941227     C                   END
141800941227     C*
141900941227     C                   ENDSR
142000941227     C********************************************************************
142100941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
142200941227     C********************************************************************
142300941227     C     RTCONS        BEGSR
142400150629     c                   clear                   WTsogliaE         1
142500970701     C*
142600970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
142700970701     C                   CLEAR                   SI95
142800970701     C                   MOVEL     '3'           I95TCN
142900970701     C                   MOVEL     D98NZD        I95NAR
143000970701     C                   MOVEL     D98CAD        I95CAP
143100970701     C                   MOVEL     D98LOD        I95LOC
143200970701     C                   MOVEL     D98DSP        I95DAT
143300970701     C                   MOVEL     D98TSP        I95TSP
143400980602     C                   MOVEL     'S'           I95FRT
143500150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
143600150511     c                   if        d98ffd='S'
143700150511     c                   eval      i95tsp='C'
143800150511     c                   endif
143900020226     C*
144000020226     C* VERIFICO SE LNA E' FILIALE FEDEX
144100020226     C                   IF        D98LNA > *ZEROS
144200020226     C     D98LNA        LOOKUP    FILFED                                 55
144300020226     C                   IF        *IN55 = *ON
144400020226     C                   EVAL      I95FI1 = 'S'
144500020226     C                   ENDIF
144600020226     C                   ENDIF
144700020226     C*
144800020226     C                   CALL      'TISI95R'
144900970701     C                   PARM                    SI95
145000941227     C*
145100941228     C* SE NON TROVATO, ERRORE
145200970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
145300941227     C                   MOVEL     '1'           D98ERR
145400970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
145500941227     C                   ELSE                                                   <-- 1 -->
145600941227     C*
145700941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
145800941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
145900941227     C     CASTIN        DIV       24            WDIV              3 0
146000941227     C                   MVR                     WRES              3 0
146100941227     C     WRES          IFEQ      *ZEROS
146200970701     C                   Z-ADD     O950IN        D98TCI
146300941227     C                   ELSE
146400970701     C                   Z-ADD     O951IN        D98TCI
146500941227     C                   END
146600941227     C*
146700941227     C     CASTCN        DIV       24            WDIV                     99
146800941227     C                   MVR                     WRES
146900941227     C     WRES          IFEQ      *ZEROS
147000970701     C                   Z-ADD     O950CN        D98TCC
147100150629     c                   eval      wTSogliaE='0'
147200941227     C                   ELSE
147300970701     C                   Z-ADD     O951CN        D98TCC
147400150629     c                   eval      wTSogliaE='1'
147500941227     C                   END
147600941227     C*
147700150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
147800941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
147900150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
148000150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
148100150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
148200150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
148300150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
148400941227     C*
148500941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
148600941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
148700970701     C     O95ISO        IFEQ      'I'
148800941227     C     D98FIN        OREQ      'I'
148900970701     C                   ADD       O95TIS        D98TCI
149000970701     C                   ADD       O95TIS        D98TCC
149100941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
149200970701     C                   ADD       O95TIS        W990IN
149300970701     C                   ADD       O95TIS        W991IN
149400150629     C                   ADD       O95TIS        W990CN
149500150629     C                   ADD       O95TIS        W991CN
149600941227     C                   END
149700090827     c
149800090827     c* Sistemo i tempi di consegna se non sono multipli di 12
149900090827     c                   exsr      SisCONS
150000160404     c
150100160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
150200160404     c                   if        wVP='S'
150300160404     c                   add       24            w991IN
150400160404     c                   add       24            w991CN
150500160404     c                   add       24            w990IN
150600160404     c                   add       24            w990CN
150700160404     c                   endif
150800941228     C*
150900941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
151000941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
151100941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
151200941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
151300941228     C                   Z-ADD     D98TCC        W99CCE            3 0
151400941228     C                   ELSE                                                   <-- 3 -->
151500970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
151600970701     C                   CLEAR                   SI95
151700970701     C                   MOVEL     '3'           I95TCN
151800970701     C                   MOVEL     D98NZD        I95NAR
151900970701     C                   MOVEL     D98CAD        I95CAP
152000970701     C                   MOVEL     D98LOD        I95LOC
152100970701     C                   MOVEL     D98DSP        I95DAT
152200970701     C                   MOVEL     'E'           I95TSP
152300980602     C                   MOVEL     'S'           I95FRT
152400020226     C*
152500020226     C* VERIFICO SE LNA E' FILIALE FEDEX
152600020226     C                   IF        D98LNA > *ZEROS
152700020226     C     D98LNA        LOOKUP    FILFED                                 55
152800020226     C                   IF        *IN55 = *ON
152900020226     C                   EVAL      I95FI1 = 'S'
153000020226     C                   ENDIF
153100020226     C                   ENDIF
153200020226     C*
153300020226     C                   CALL      'TISI95R'
153400970701     C                   PARM                    SI95
153500970701     C*
153600941228     C* SE NON TROVATO, ERRORE
153700970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
153800941228     C                   MOVEL     '1'           D98ERR
153900970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
154000941228     C                   ELSE                                                   <-- 4 -->
154100941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
154200941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
154300941228     C     CASTCN        DIV       24            WDIV                     99
154400941228     C                   MVR                     WRES
154500941228     C     WRES          IFEQ      *ZEROS
154600970701     C                   Z-ADD     O950CN        W99CCE
154700941228     C                   ELSE
154800970701     C                   Z-ADD     O951CN        W99CCE
154900941228     C                   END
155000941228     C                   ENDIF                                                  <-- 4 ---
155100941228     C                   ENDIF                                                  <-- 3 ---
155200941228     C                   ENDIF                                                  <-- 2 ---
155300941227     C*
155400941227     C                   END                                                    <-- 1 ---
155500941227     C*
155600941227     C                   ENDSR
155700090827     C********************************************************************
155800090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
155900090827     C********************************************************************
156000090827     C     SisCONS       BEGSR
156100090827     c                   eval      w99tci=d98tci
156200090827     c                   if        d98tci=10
156300090827     c                   z-add     12            w99tci
156400090827     c                   endif
156500090827     c
156600090827     c                   eval      w99tcc=d98tcc
156700090827     c                   if        d98tcc=10
156800090827     c                   z-add     12            w99tcc
156900090827     c                   endif
157000160404     c* se si tratta di un vedi pacco DPD o ex vedi pacco aggiungo 24 ore
157100160404     c                   if        wVP='S'
157200160404     c                   add       24            w99tcc
157300160404     c                   add       24            w99tci
157400160404     c                   endif
157500090827     c
157600090827     C                   ENDSR
157700930420     C********************************************************************
157800930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
157900930420     C********************************************************************
158000930420     C     CAREVB        BEGSR
158100930420     C*
158200930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
158300930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
158400060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
158500930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
158600930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
158700060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
158800060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
158900930420     C                   Z-ADD     D98AAS        KEBAAS
159000930420     C                   Z-ADD     D98LNP        KEBLNP
159100930420     C                   Z-ADD     D98NRS        KEBNRS
159200930420     C                   Z-ADD     D98NSP        KEBNSP
159300990810     C     KEYEVB        SETLL     FNEVB01L                           99
159400060406     C  N99keyevb        READe     FNEVB01L                               99
159500060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
159600930421     C     D98ERR        ANDEQ     '0'
159700060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
159800930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
159900930421     C                   Z-ADD     1             I
160000930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
160100060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
160200930421     C                   MOVEL     '1'           D98ERR
160300930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
160400060406   x3C                   ELSE                                                   <-- 3 -->
160500060406     C                   MOVEL     D2A(I)        usa2A
160600060406     C                   MOVEL     D2Z(I)        usa2Z
160700930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
160800060406    4C     §2AFTC        IFEQ      'A'
160900930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
161000060406    4C                   END
161100060406    4C     §2AFTC        IFEQ      'G'
161200930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
161300060406    4C                   END
161400150216     c* Se si tratta di MIC verifico se uguale alla sped.
161500150303    4c                   if        evbcev='MIC' or evbcev='NIC'
161600150303    5c                   if        %subst(evbnot:1:7) >*zeros
161700150303     c
161800150303    6c                   if        evbcev='MIC' and
161900150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
162000150216     c                   eval      evemic='SI'
162100150303    6c                   endif
162200150303     c
162300150303    6c                   if        evbcev='NIC' and
162400150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
162500150216     c                   eval      evemic='  '
162600150303    6c                   endif
162700150303     c
162800150303   x5c                   else
162900150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
163000150303     c                   eval      evemic='SI'
163100150303    6c                   endif
163200150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
163300150303     c                   eval      evemic='  '
163400150303    6c                   endif
163500150303
163600150303    5c                   endif
163700150303    4c                   endif
163800150216     c
163900060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
164000930421     C     §2AFTC        OREQ      'G'
164100930421     C                   ADD       1             IE
164200990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
164300930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
164400930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
164500131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
164600090928     c
164700090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
164800091014     c**                 if        ie=1
164900091014     c**                 eval      Primotipo=§2aftc
165000091014     c**                 eval      Primodev=evbdev
165100091014     c**                 eval      Primohev=evbhev
165200091014     c**                 endif
165300090928     c
165400060406    4C                   END                                                    <-- 4 ---
165500060406     c* Verifico se si tratta di evento particolare
165600060406    4c                   if        §2zCEP<>' '
165700060526     c* non carico l'evento se cade in giorno festivo
165800060526     c                   z-add     evbdev        wpodata
165900060526     C                   EVAL      WTC='C'
166000060526     C                   EVAL      WIC='C'
166100060526     c                   eval      wslitt='N'
166200060526     c                   exsr      chkfes
166300060526     c
166400060526     c* Wslitt='N' non festivo
166500060526   4ac                   if        wslitt='N'
166600060526     c
166700060406    5c                   if        §2zfep='A'
166800060406     c                   eval      wevpaA='S'
166900060406    5C                   END                                                    <-- 3 ---
167000060406    5c                   if        §2zfep='G'
167100060406     c                   eval      wevpaG='S'
167200060406    5C                   END                                                    <-- 3 ---
167300060406     c                   add       1             ip
167400060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
167500060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
167600060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
167700060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
167800060526   4aC                   ENDif                                                  <-- 3 ---
167900060526    4C                   END                                                    <-- 3 ---
168000060406    3C                   END                                                    <-- 3 ---
168100060406    2C                   END                                                    <-- 2 ---
168200060406     C     keyevb        READe     FNEVB01L                               99
168300060406    1C                   ENDDO                                                  <-- 1 ---
168400050523     c* controllo riempimento skiere
168500050523     c                   clear                   trul0sds
168600050523     c                   eval      i0sski='EVD/EVO/EVC'
168700050523     c                   eval      i0sele=%elem(EVD)
168800050523     c                   eval      i0spie=ie
168900050523     c                   eval      i0sfile='FNEVB01L  '
169000050523     c                   eval      i0scod='     '
169100050523     c                   eval      i0spgm='TNSD99R'
169200050523     c                   movel     trul0sds      kpjbu
169300050523     c                   call      'TRUL0SR'
169400050523     c                   parm                    kpjba
169500930420     C*
169600060406     c* controllo riempimento skiere
169700060406     c                   clear                   trul0sds
169800060406     c                   eval      i0sski='PVD/PVO/PVC'
169900060406     c                   eval      i0sele=%elem(PVD)
170000060406     c                   eval      i0spie=ie
170100060406     c                   eval      i0sfile='FNEVB01L  '
170200060406     c                   eval      i0scod='     '
170300060406     c                   eval      i0spgm='TNSD99R'
170400060406     c                   movel     trul0sds      kpjbu
170500060406     c                   call      'TRUL0SR'
170600060406     c                   parm                    kpjba
170700060406     C*
170800930420     C                   ENDSR
170900931213     C********************************************************************
171000931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
171100931213     C********************************************************************
171200931213     C     CARTUC        BEGSR
171300931213     C*
171400931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
171500931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
171600931214     C     D98GC2        ANDEQ     *BLANKS
171700931213     C                   ELSE                                                   <-- 1 -->
171800931213     C*
171900931213     C                   MOVE      D98GC1        TUC(1)
172000931214     C                   MOVE      D98GC2        TUC(2)
172100931213     C*
172200931213     C                   END                                                    <-- 1 ---
172300931213     C*
172400931213     C                   ENDSR
172500970304     C********************************************************************
172600970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
172700970304     C********************************************************************
172800970304     C     DATRIF        BEGSR
172900970304     C*
173000050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
173100050922     c*  partenza
173200970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
173300970304     C*
173400970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
173500070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
173600070605     C***                MOVEL     '1'           D98ERR
173700070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
173800070605     C***                ELSE                                                   <-- 2 -->
173900970304     C*
174000970304     C                   Z-ADD     D98DRF        WD1
174100970304     C                   Z-ADD     D98ORF        WO1
174200970310     C* DATA PARTENZA ULTIMO COLLO
174300970310     C     D98DRF        IFLE      D98DUC
174400050928     c     d98duc        oreq      0
174500970310     C                   Z-ADD     D98DRF        D98DUC
174600970310     C                   Z-ADD     D98ORF        W98HUC
174700070605     C                   ENDIF
174800970304     C* DATA ARRIVO TRAZIONE
174900050922     C***                Z-ADD     D98DTI        WD2
175000050922     C***                Z-ADD     D98HTI        WO2
175100050922     C***  WDO1          IFLE      WDO2
175200050922     C***                Z-ADD     D98DRF        D98DTI
175300050922     C***                Z-ADD     D98ORF        D98HTI
175400050922     C***                ENDIF
175500970304     C* DATA CONSEGNA
175600050922     C***                Z-ADD     D98DCM        WD2
175700050922     C***                Z-ADD     D98HMC        WO2
175800050922     C***  WDO1          IFLE      WDO2
175900050922     C***                Z-ADD     D98DRF        D98DCM
176000050922     C***                Z-ADD     D98ORF        D98HMC
176100050922     C***                ENDIF
176200970304     C*
176300070605     C***                ENDIF                                                  <-- 2 ---
176400970304     C                   ENDIF                                                  <-- 1 ---
176500970304     C*
176600970304     C                   ENDSR
176700940901     C********************************************************************
176800940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
176900940901     C********************************************************************
177000940901     C     TDTIPO        BEGSR
177100940901     C*
177200940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
177300940902     C                   Z-ADD     D98TTI        W98TTI            3 0
177400941011     C*
177500941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
177600941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
177700941011     C     W98TTI        DIV       24            WDIV              3 0
177800941011     C                   MVR                     WRES              3 0
177900941011     C     WRES          IFEQ      *ZEROS
178000941011     C                   Z-ADD     *ZEROS        WASTA             9 0
178100941011     C                   ELSE
178200941011     C                   Z-ADD     12            WASTA
178300941011     C                   END
178400940901     C*
178500940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
178600940901     C     D98DTI        IFNE      *ZEROS
178700940901     C                   Z-ADD     D98DTI        WDAT
178800940901     C                   EXSR      DATORE
178900960911     C                   Z-ADD     WORE          WPODTI
179000960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
179100960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
179200960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
179300960911     C                   Z-ADD     WPODTI        W10POI            9 0
179400960911     C                   EXSR      R10FES
179500960911     C                   Z-ADD     W10POO        WPODTI
179600960911     C* AGGIUNGO LE ORA D'ASTA
179700960911     C                   ADD       WASTA         WPODTI
179800960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
179900960911     C     W10POI        IFEQ      W10POO
180000940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
180100940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
180200940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
180300940901     C                   EXSR      RO3ORE
180400940901     C                   ADD       WR3HHO        WPODTI
180500940901     C                   ADD       WR3HHO        W98TTI
180600940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
180700940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
180800940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
180900940901     C                   EXSR      RO3ORE
181000940901     C                   ADD       WR3HHO        WPODTI
181100940901     C                   ADD       WR3HHO        W98TTI
181200960911     C                   ENDIF                                                  *SE NON FESTIVO
181300940901     C                   END
181400940901     C*
181500940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
181600940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
181700940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
181800940901     C     W98TTI        DIV       24            WDIV              3 0
181900940901     C                   MVR                     WRES              3 0
182000940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
182100941228     C                   Z-ADD     W990IN        D98TCI
182200160405     c* il campo in questo momento non deve avere le 24 ore in più del VP
182300160405     c*  per cui le sottraggo, per riagguingerle alla fine
182400160405     c                   if        wVP='S'
182500160405     c                   sub       24            D98TCI
182600160405     c                   endif
182700160405     c
182800150630     c                   eval      wTSogliaE='0'
182900940901     C                   ELSE                                                   <-- 2 -->
183000941228     C                   Z-ADD     W991IN        D98TCI
183100160405     c                   if        wVP='S'
183200160405     c                   sub       24            D98TCI
183300160405     c                   endif
183400150630     c                   eval      wTSogliaE='1'
183500940908     C                   ENDIF                                                  <-- 2 ---
183600090827     c* Sistemo i tempi di consegna se non sono multipli di 12
183700090827     c                   exsr      SisCONS
183800090827     c*
183900940908     C* RICALCOLO TEMPO DI RESA INTERNO
184000950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
184100950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
184200090921     c                   z-add     d98tri        w99tri
184300090921     c* per la DS
184400090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
184500090921     c* per i calcoli interni
184600090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
184700940908     C                   ENDIF                                                  <-- 1 ---
184800940901     C*
184900940901     C                   ENDSR
185000930325     C********************************************************************
185100930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
185200930325     C********************************************************************
185300930325     C     TDATPO        BEGSR
185400930406     C*
185500930420     C* TRASFORMO LA DATA ODIERNA
185600930416     C                   Z-ADD     WDAODI        WDAT
185700930416     C                   EXSR      DATORE
185800050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
185900960911     C                   MOVEL     'OSC  '       WR3TAB
186000960911     C                   Z-ADD     WHHODI        WR3HHI
186100960911     C                   EXSR      RO3ORE
186200050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
186300050922     c                   if        wr3hho=12
186400050922     c     wpoatt        add       12            wpoattdata24
186500050922     c                   else
186600050922     c                   z-add     wpoatt        wpoattdata24
186700050922     c                   endif
186800930325     C*
186900930420     C* TRASFORMO LA DATA DI SPEDIZIONE
187000931125     C     D98DSP        IFEQ      *ZEROS
187100930326     C                   MOVEL     '1'           D98ERR
187200930326     C                   MOVEL     MSG(2)        D98MSG
187300930326     C                   ELSE
187400931125     C                   Z-ADD     D98DSP        WDAT
187500930325     C                   EXSR      DATORE
187600930406     C                   Z-ADD     WORE          WPODSP
187700930326     C                   END
187800930414     C*
187900930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
188000930414     C                   MOVEL     'LIM  '       WR3TAB
188100930414     C                   Z-ADD     *ZEROS        WR3HHI
188200930414     C                   EXSR      RO3ORE
188300931222     C     WR3HHO        MULT      10            WTM060            6 0
188400931222     C     WPODSP        ADD       WTM060        WPOLIM
188500140822     c
188600930325     C*
188700930420     C* TRASFORMO LA DATA DI CONSEGNA
188800931125     C     D98DCM        IFNE      *ZEROS
188900931125     C                   Z-ADD     D98DCM        WDAT
189000930325     C                   EXSR      DATORE
189100930326     C                   Z-ADD     WORE          WPODCM
189200930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
189300930326     C                   MOVEL     'OSC  '       WR3TAB
189400931125     C                   Z-ADD     D98HMC        WR3HHI
189500930326     C                   EXSR      RO3ORE
189600930326     C                   ADD       WR3HHO        WPODCM
189700930326     C                   END
189800941228     C*
189900941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
190000941228     C     D98DUC        IFNE      *ZEROS
190100941228     C                   Z-ADD     D98DUC        WDAT
190200941228     C                   EXSR      DATORE
190300941228     C                   Z-ADD     WORE          WPODUC
190400970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
190500970310     C                   MOVEL     'OSP  '       WR3TAB
190600970310     C                   Z-ADD     W98HUC        WR3HHI
190700970310     C                   EXSR      RO3ORE
190800970310     C     D98DRF        IFNE      *ZEROS
190900050928     c     w98huc        andgt     1200
191000970310     C                   SUB       12            WR3HHO
191100970310     C                   ENDIF
191200970310     C                   ADD       WR3HHO        WPODUC
191300941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
191400070605     C***  WPODSP        IFGT      WPODUC
191500070605     C***                Z-ADD     WPODSP        WPODUC
191600070605     C***                ENDIF
191700941228     C                   ENDIF
191800960402     C*
191900141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
192000970304     C     D98DBR        IFNE      *ZEROS
192100970304     C                   Z-ADD     D98DBR        WDAT
192200960402     C                   EXSR      DATORE
192300960402     C                   Z-ADD     WORE          WPODBR
192400070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
192500960402     C                   ELSE
192600960402     C                   Z-ADD     WPODSP        WPODBR
192700960402     C                   ENDIF
192800930913     C*
192900960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
193000930913     C* CONSEGNABILITA'
193100960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
193200950407     C*
193300930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
193400931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
193500931125     C                   Z-ADD     D98DIC        WDAT
193600930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
193700930913     C                   Z-ADD     WORE          WPODIC
193800930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
193900931125     C                   Z-ADD     D98OIC        WR3HHI
194000930913     C                   EXSR      RO3ORE
194100930913     C                   ADD       WR3HHO        WPODIC
194200931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
194300950407     C*
194400930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
194500931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
194600931125     C                   Z-ADD     D98DFC        WDAT
194700930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
194800930913     C                   Z-ADD     WORE          WPODFC
194900140822     C                   Z-ADD     WORE          WPODFC_00
195000930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
195100931125     C                   Z-ADD     D98OFC        WR3HHI
195200930913     C                   EXSR      RO3ORE
195300930913     C                   ADD       WR3HHO        WPODFC
195400931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
195500930913     C*
195600930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
195700930913     C     D98ERR        IFEQ      '0'
195800930913     C     WPODFC        IFEQ      *ZEROS
195900930913     C     WPODIC        ANDNE     *ZEROS
196000930913     C                   MOVEL     '1'           D98ERR
196100930913     C                   MOVEL     MSG(17)       D98MSG
196200930913     C                   ENDIF
196300930913     C                   ENDIF
196400930907     C*
196500930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
196600930913     C     D98ERR        IFEQ      '0'
196700930913     C     WPODIC        IFGT      WPODFC
196800930907     C                   MOVEL     '1'           D98ERR
196900930913     C                   MOVEL     MSG(18)       D98MSG
197000930913     C                   ENDIF
197100930913     C                   ENDIF
197200950407     C*
197300950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
197400930511     C*
197500930511     C                   ENDSR
197600950110     C********************************************************************
197700950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
197800950110     C********************************************************************
197900950110     C     CALDPP        BEGSR
198000950110     C*
198100950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
198200950110     C                   MOVEL     'P'           WTC                            *PARTENZA
198300950110     C                   MOVEL     'I'           WIC                            *INTERNA
198400960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
198500950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
198600950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
198700950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
198800950110     C*
198900950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
199000950110     C                   MOVEL     'P'           WTC                            *PARTENZA
199100950110     C                   MOVEL     'C'           WIC                            *CLIENTE
199200950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
199300170104     C* SE TEMPO PARTENZA =0 sommo comunque 1 ore per slittare dal
199400170104     c*  festivo se fosse data festiva la data sped e non bortderizzata
199500170104     c*  in questa data
199600170104     c                   if        d98tpc=0
199700170104     c                   z-add     1             WR4NOI
199800170104     c                   else
199900950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
200000170104     c                   endif
200100950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
200200950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
200300170104     c
200400170104     c                   if        d98tpc=0
200500170104     c                   sub       1             wpoppc
200600170104     c                   endif
200700960229     C*
200800960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
200900050922     c
201000050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
201100050922     c
201200050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
201300090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
201400050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
201500090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
201600050922%%%  C*  ritardo alla partenza.
201700050922     c* Il tutto solo se la data di oggi > della
201800050922     c*  data teorica di partenza determinata
201900050922     c*
202000950110%%%  C     D98ERR        IFEQ      '0'
202100950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
202200950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
202300950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
202400050922     c     wpoattData24  andgt     wpoppi
202500960911%%%  C********             MOVEL'OSC  '   WR3TAB
202600960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
202700960911%%%  C********             EXSR RO3ORE
202800960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
202900050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
203000090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
203100090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
203200950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
203300050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
203400050922     c* La nuova data calcolata non può essere < della minima prevista
203500050922     c                   if        wr0poo>wpoppi
203600050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
203700050922     c                   endif
203800050922     c
203900950110%%%  C                   ENDIF
204000950110%%%  C                   ENDIF
204100950110%%%  C*
204200950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
204300050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
204400050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
204500050915%%%  C*  alla prtenza.
204600950110%%%  C     D98ERR        IFEQ      '0'
204700950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
204800950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
204900950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
205000950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
205100950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
205200950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
205300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
205400950110%%%  C                   ENDIF
205500950110%%%  C                   ENDIF
205600950110%%%  C*
205700950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
205800950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
205900950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
206000950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
206100950110%%%  C     D98ERR        IFEQ      '0'
206200950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
206300950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
206400950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
206500950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
206600090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
206700090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
206800950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
206900050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
207000950110%%%  C                   ENDIF
207100950110%%%  C                   ENDIF
207200960229     C*
207300960229     C                   ENDIF                                                  <-- 1 ---
207400950110     C*
207500950110     C                   ENDSR
207600930604     C********************************************************************
207700930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
207800930604     C********************************************************************
207900930604     C     CALDPT        BEGSR
208000090902     c                   clear                   RicalcoloTRAZ     1
208100930604     C*
208200930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
208300931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
208400931129     C                   MOVEL     'I'           WIC                            *INTERNA
208500050915     c* se non presente la data partenza ultimo collo, uso la teorica
208600950110     C     D98DUC        IFEQ      *ZEROS
208700950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
208800950110     C                   ELSE
208900950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
209000950110     C                   ENDIF
209100930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
209200930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
209300930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
209400930604     C*
209500930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
209600931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
209700931129     C                   MOVEL     'C'           WIC                            *CLIENTE
209800950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
209900930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
210000930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
210100930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
210200150508
210300150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
210400150508     c* se presente la data partenza PRIMO  collo --> la uso
210500150508     c                   if        w98dpc=0
210600150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
210700150508     c                   else
210800150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
210900150508     C                   MOVEL     'C'           WIC                            *INTERNA
211000150508     C                   Z-ADD     W98DPC        WDAT
211100150508     C                   EXSR      DATORE
211200150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
211300150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
211400150508     C                   EXSR      RO4CDP
211500150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
211600150508     c                   endif
211700960229     C*
211800960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
211900940624%%%  C*
212000050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
212100050915
212200940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
212300050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
212400050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
212500050915     c*   attribuito alla trazione
212600940624%%%  C     D98ERR        IFEQ      '0'
212700940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
212800940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
212900090903     c     wpoatt        andge     wpopti
213000050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
213100090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
213200090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
213300940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
213400050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
213500050922     c                   if        wr0poo>wpopti
213600050922     c                   eval      wpopti=wr0poo
213700050922%%%  C                   endif
213800090902     c*
213900090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
214000090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
214100090902     c*  della trazione udate e utime
214200090902     c                   eval      RicalcoloTRAZ='S'
214300090902     c
214400940624%%%  C                   ENDIF
214500940624%%%  C                   ENDIF
214600940628%%%  C*
214700050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
214800050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
214900050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
215000940628%%%  C     D98ERR        IFEQ      '0'
215100940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
215200940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
215300940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
215400090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
215500090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
215600940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
215700050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
215800940628%%%  C                   ENDIF
215900940628%%%  C                   ENDIF
216000960229     C*
216100960229     C                   ENDIF                                                  <-- 1 ---
216200930604     C*
216300930604     C                   ENDSR
216400930511     C********************************************************************
216500930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
216600930511     C********************************************************************
216700930511     C     CALDCR        BEGSR
216800050914     c                   clear                   consideraora
216900100308     c
217000100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
217100100308     c*
217200100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
217300100308     c* 1) se presente solo ORA --> ignoro sempre
217400100308     c* 2) se presente data e ora IL --> ignoro l'ora
217500100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
217600100309     c***                if        d98tsp='E'
217700100309     c***                clear                   d98dcr
217800100309     c***                clear                   d98hcr
217900100309     c**+                clear                   d98tcr
218000100309     c***                endif
218100100308     c
218200100308     c                   if        d98hcr>0
218300100308     c
218400100308     c                   if        d98dcr=0
218500100308     c                   clear                   d98hcr
218600100308     c                   endif
218700100308
218800100308     c                   if        d98dcr>0 and d98tcr=' '
218900100308     c                   clear                   d98hcr
219000100308     c                   endif
219100100308     c                   endif
219200941228     C*
219300941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
219400941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
219500050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
219600050901     c*  (per uguale si intende entrambe nel mattino o entrambe
219700050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
219800050901     c*  (EDPES 1/9/05)
219900050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
220000050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
220100050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
220200050831     C***                Z-ADD     D98DCM        D98DCR
220300050831     C***                ENDIF
220400050901     c*
220500070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
220600050914     c
220700100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
220800100308     c***                clear                   d98hcr
220900100308     c***                clear                   d98tcr
221000100308     c***                endif
221100050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
221200050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
221300070521     c*  richiesta dal cliente
221400100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
221500100308     C***                MOVEL     'OSC  '       WR3TAB
221600100308     C***                Z-ADD     D98HMC        WR3HHI
221700100308     C***                EXSR      RO3ORE
221800100308     c***                z-add     wr3hho        salvaoreHMC
221900050901     c
222000100308     C***                Z-ADD     D98Hcr        WR3HHI
222100100308     C***                EXSR      RO3ORE
222200100308     c***                z-add     wr3hho        salvaoreHCR
222300100308     c***
222400050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
222500100308    2c***                if        salvaoreHMC=salvaoreHCR
222600100308     C***                eval      Consideraora='S'
222700100308     c***                else
222800100308     C***                eval      Consideraora=' '
222900100308     c***                clear                   d98hcr
223000100308    2c***                endif
223100100308    1c***                endif
223200930511     C*
223300930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
223400931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
223500931125     C                   Z-ADD     D98DCR        WDAT
223600930325     C                   EXSR      DATORE
223700930326     C                   Z-ADD     WORE          WPODCR
223800930420     C*
223900930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
224000930420     C*
224100931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
224200930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
224300930326     C                   MOVEL     'CRD  '       WR3TAB
224400931125     C                   Z-ADD     D98HCR        WR3HHI
224500930419     C                   EXSR      RO3ORE
224600930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
224700940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
224800050902     c
224900931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
225000930420     C                   MOVEL     'CRD1 '       WR3TAB
225100930420     C                   Z-ADD     *ZEROS        WR3HHI
225200930420     C                   EXSR      RO3ORE
225300931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
225400090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
225500090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
225600050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
225700060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
225800050907     c                   z-add     worecr        oreILoDOPO
225900931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
226000931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
226100931207     C                   MOVEL     'C'           WIC                            *CLIENTE
226200931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
226300150330     C                   Z-ADD     WORECR        WR4NOI
226400931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
226500931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
226600931209     C     WPOCRF        SUB       12            WR7POI            9 0
226700931209     C                   MOVEL     'X'           WIC
226800931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
226900931209     C                   ADD       12            WR7POO
227000931207     C     WR7POO        IFGT      WPOCRF
227100931207     C                   Z-ADD     WR7POO        WPOCRF
227200931207     C                   END
227300150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
227400150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
227500150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
227600150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
227700150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
227800150330     C                   MOVEL     'C'           WIC                            *CLIENTE
227900150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
228000150330     C                   Z-ADD     w99TCC        WR4NOI
228100150330     c                   sub       12            wr4noi
228200150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
228300150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
228400150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
228500150330     c
228600150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
228700150330     C                   MOVEL     'X'           WIC
228800150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
228900150330     C******             ADD       12            WR7POO
229000150330     C     WR7POO        IFGT      WPOCRF_dee
229100150330     C                   Z-ADD     WR7POO        WPOCRF_dee
229200150330     C                   END
229300050902     c
229400050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
229500050902     c*  sono le ore di fine della non consegnabilità
229600050905     c* Se aggiungendo 1 finisco in giorno festivo
229700050905     c*  cerco primo giorno non festivo
229800050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
229900050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
230000050907     C****               MOVEL     'C'           WIC                            *CLIENTE
230100050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
230200050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
230300050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
230400050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
230500050907     c
230600050907     c                   add       1             wpocri
230700050902     c
230800050902   FFC                   Z-ADD     WPOCRI        WPOCRC
230900050902     c
231000930325     C                   END
231100150330     C*
231200931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
231300930420     C* DATA SPED. = DATA INIZIO RANGE
231400930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
231500940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
231600940324£$   C                   Z-ADD     WPOCRI        WPOCRC
231700930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
231800930420     C                   MOVEL     'CRP  '       WR3TAB
231900931125     C                   Z-ADD     D98HCR        WR3HHI
232000930420     C                   EXSR      RO3ORE
232100931207     C     WPODCR        ADD       WR3HHO        WPOCRF
232200931207     C                   SUB       24            WPOCRF                         *FINE RANGE
232300931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
232400931207     C* ANDANDO INDIETRO NEL TEMPO
232500050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
232600050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
232700050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
232800050907     c* data utile venedì sera
232900130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
233000130411     C                   MOVEL     'X'           WIC                            *CLIENTE
233100931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
233200931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
233300931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
233400930326     C                   END
233500930526     C*
233600931125     C     D98TCR        IFEQ      'P'
233700931125     C     D98TCR        OREQ      'D'
233800930526     C                   ELSE                                                   *CONS.RICH.IL ..
233900940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
234000940324£$   C                   Z-ADD     WPODCR        WPOCRC
234100930526     C* DATA SPED. = DATA INIZIO RANGE
234200050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
234300050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
234400050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
234500930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
234600930526     C                   MOVEL     'CRI  '       WR3TAB
234700931125     C                   Z-ADD     D98HCR        WR3HHI
234800930526     C                   EXSR      RO3ORE
234900050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
235000050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
235100050907     c                   z-add     wr3hho        oreILoDOPO
235200930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
235300050902     c
235400050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
235500050902     c                   if        wr3MATPOM='P' and d98hcr>0
235600050902     c                   add       12            wpocri
235700100308     c* tolgo 12 ore dalle ore conseganbili
235800050907     c                   sub       12            oreILoDOPO
235900050902     c                   endif
236000050902     c
236100931209     C     WPOCRF        SUB       12            WR7POI            9 0
236200931209     C                   MOVEL     'X'           WIC
236300931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
236400931209     C                   ADD       12            WR7POO
236500931209     C     WR7POO        IFGT      WPOCRF
236600931209     C                   Z-ADD     WR7POO        WPOCRF
236700931209     C                   END
236800050905     c
236900050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
237000050905     c*  sono le ore di fine della non consegnabilità
237100050905     c* Se aggiungendo 1 finisco in giorno festivo
237200050905     c*  cerco primo giorno non festivo
237300050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
237400050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
237500050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
237600050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
237700050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
237800050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
237900050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
238000050907     c
238100050907     c                   add       1             wpocri
238200050905     c
238300050905   FFC                   Z-ADD     WPOCRI        WPOCRC
238400050905     c
238500930526     C                   END
238600931206     C*
238700931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
238800931206     C* LA IMPOSTO COME LA CONS.TEORICA
238900140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
239000140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
239100931206     C                   ELSE
239200931206     C                   Z-ADD     WPOCRF        WPOCR1
239300931206     C                   END
239400150608     c* ai fini del calcolo affidabilità
239500150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
239600150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
239700931206     C                   ELSE
239800931206     C                   Z-ADD     WPOCRF        WPOCR2
239900931206     C                   END
240000150608     c* ai fini della data prevista consegna cliente
240100150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
240200150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
240300150608     C                   ELSE
240400150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
240500150608     C                   END
240600150331     c
240700150331     c                   if        wpocrf_dee>0
240800150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
240900150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
241000150330     C                   END
241100150331     C                   END
241200150331     C*
241300150331     c                   if        wpocrf_dee=0
241400150608     C     wpocr2_dee    sub       24            WPOCRF_dee
241500150331     c                   endif
241600150331     c
241700930526     C                   END                                                    <-- 1 ---
241800930511     C*
241900930511     C                   ENDSR
242000930511     C********************************************************************
242100930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
242200930511     C********************************************************************
242300930511     C     CALLAV        BEGSR
242400930511     C*
242500941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
242600930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
242700930421     C*
242800930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
242900930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
243000930421     C     1             DO        IE            E                 3 0          --- 2 -->
243100930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
243200930421     C                   Z-ADD     1             I
243300930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
243400060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
243500930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
243600930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
243700930421     C                   ADD       1             IL
243800930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
243900930421     C                   EXSR      DATORE
244000930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
244100091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
244200091014     c* Mi salvo la posizione dell'evento
244300091014     C                   Z-ADD     E             LAp(IL)
244400930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
244500930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
244600930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
244700930421     C                   EXSR      RO3ORE
244800930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
244900930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
245000930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
245100930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
245200091015     c**                 if        §2agla>1
245300091015     c**                 sub       1             §2agla
245400930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
245500091015     c**                 else
245600091015     c**                 clear                   whh
245700091015     c**                 endif
245800930510     C                   ADD       WHH           WORELA
245900940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
246000930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
246100090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
246200150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
246300930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
246400931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
246500931129     C                   MOVEL     'C'           WIC                            *CLIENTE
246600930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
246700930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
246800930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
246900930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
247000931209     C                   Z-ADD     LAF(IL)       WR7POI
247100931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
247200931202     C     WR7POO        IFGT      LAF(IL)
247300931201     C                   Z-ADD     WR7POO        LAF(IL)
247400931202     C                   END
247500930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
247600930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
247700091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
247800930510     C                   END                                                    <-- 3 ---
247900931223     C                   ENDDO                                                  <-- 2 ---
248000930421     C*
248100930421     C                   END                                                    <-- 1 ---
248200930618££$$ C*
248300930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
248400931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
248500941228££$$ C     1             DO        IL            I
248600930618££$$ C     LAF(I)        IFGT      WPOLAF
248700930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
248800930618££$$ C                   END
248900931223££$$ C                   ENDDO
249000930511     C*
249100930511     C                   ENDSR
249200940324£$   C********************************************************************
249300940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
249400940324£$   C********************************************************************
249500940324£$   C     CALLAC        BEGSR
249600941229     C*
249700950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
249800950407     C*
249900950210     C                   Z-ADD     WORELA        WTM060
250000941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
250100941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
250200941229     C     D98DAG        IFEQ      *ZEROS
250300050520     c     §2aftg        andeq     'S'
250400090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
250500090827     C     WORELA        ADD       w99TCC        WTM060            6 0
250600160404     c                   if        wVP='S'
250700160404     c                   sub       24            wTM060
250800160404     c                   endif
250900941229     C                   ENDIF
251000941229     C*
251100940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
251200940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
251300941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
251400941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
251500941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
251600940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
251700941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
251800050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
251900050520     c                   movel     §2aftg        LCON(IL)
252000950407     C*
252100950407     C                   ENDIF
252200960129     C*
252300940324£$   C                   ENDSR
252400930511     C********************************************************************
252500930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
252600930511     C********************************************************************
252700930511     C     CALGIA        BEGSR
252800960129     C*
252900960129     C*-------------------------------------------------------------------
253000960129     C* EVENTI GIACENZA
253100960129     C*-------------------------------------------------------------------
253200960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
253300960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
253400960129     C*
253500050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
253600041116     c     d98nsp        ifgt      0
253700031125     C                   EXSR      CARGIAC
253800041116     c                   endif
253900031125     C*
254000960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
254100960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
254200960129     C     1             DO        IE            E                 3 0          --- 2 -->
254300960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
254400960129     C                   Z-ADD     1             I
254500960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
254600060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
254700960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
254800960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
254900960129     C                   ADD       1             IG
255000960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
255100960129     C                   EXSR      DATORE
255200960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
255300140606     c* Mi salvo la posizione dell'evento
255400140606     C                   Z-ADD     E             gip(IG)
255500960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
255600960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
255700960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
255800960129     C                   EXSR      RO3ORE
255900960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
256000960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
256100960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
256200960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
256300960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
256400960129     C                   ADD       WHH           WOREGI
256500960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
256600960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
256700090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
256800090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
256900160404     c                   if        wVP='S'
257000160404     c                   sub       24            wOREGI
257100160404     c                   endif
257200960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
257300960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
257400960129     C                   MOVEL     'C'           WIC                            *CLIENTE
257500960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
257600960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
257700960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
257800960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
257900960129     C                   Z-ADD     GIF(IG)       WR7POI
258000960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
258100031125     C     WR7POO        IFGT      GIF(IG)
258200031125     C                   Z-ADD     WR7POO        GIF(IG)
258300031125     C                   ENDIF
258400031201     C*
258500031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
258600031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
258700031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
258800031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
258900031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
259000031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
259100031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
259200031201     C                   IF        WPOATT > GIF(IG)
259300031201     C*
259400031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
259500031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
259600031201     C                   IF        WGGA = *BLANKS
259700031201     C                   SETOFF                                       54
259800031201     C                   ELSE
259900031201     C     WGGA          LOOKUP    SK7QW                                  54
260000031201     C                   ENDIF
260100031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
260200031201     C                   MOVEL     '1'           FLGGIACOK
260300031201     C                   ELSE
260400140127     c
260500031125     C*
260600031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
260700031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
260800031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
260900031125     C                   MOVEL     '0'           FLGGIACOK         1
261000031125     C*
261100031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
261200031125     C     1             DO        IAG           EG                3 0
261300031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
261400031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
261500031125     C                   EXSR      DATORE
261600031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
261700031125     C*
261800031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
261900031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
262000031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
262100031125     C                   EXSR      DATORE
262200031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
262300031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
262400031229     C* => NN LO CONSIDERO
262500031229     C                   IF        EVC(E+1) = 'G' AND
262600031229     C                             WPROXGII = WGCPDORE
262700031229     C                   MOVEL     '1'           FLGGIACOK
262800031229     C                   ENDIF
262900031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
263000031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
263100031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
263200031125     C                   EXSR      RO3ORE
263300031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
263400031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
263500031125     C* DELL'EVENTO GIACENZA CORRENTE
263600031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
263700060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
263800031125     C                   MOVEL     '1'           FLGGIACOK
263900031125     C                   LEAVE
264000031125     C                   ENDIF
264100031125     C                   ELSE
264200031229     C                   IF        EVC(E)  = 'G' AND
264300031229     C                             GII(IG) = WGCPDORE
264400031229     C                   MOVEL     '1'           FLGGIACOK
264500031229     C                   LEAVE
264600031229     C                   ENDIF
264700031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
264800031125     C                   MOVEL     '1'           FLGGIACOK
264900031125     C                   LEAVE
265000031125     C                   ENDIF
265100031125     C                   ENDIF
265200031125     C                   ENDDO
265300031201     C                   ENDIF
265400140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
265500140127     C                   IF        FLGGIACOK = '0'  and
265600140127     c                             d98dcm=evd(E)
265700140127     C                   MOVEL     '1'           FLGGIACOK
265800140127     c                   endif
265900031125     C*
266000031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
266100031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
266200031125     C* DELL'EVENTO GIACENZA CORRENTE
266300031125     C                   IF        FLGGIACOK = '0'
266400031125     C                   Z-ADD     *ZEROS        GIF(IG)
266500031125     C                   ELSE
266600031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
266700031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
266800031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
266900031125     C                   ENDIF
267000031125     C                   ELSE
267100031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
267200031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
267300031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
267400031125     C                   ENDIF
267500031125     C*
267600960129     C                   ENDIF                                                  <-- 3 ---
267700960129     C                   ENDDO                                                  <-- 2 ---
267800960129     C*
267900960129     C                   ENDIF                                                  <-- 1 ---
268000960129££$$ C*
268100960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
268200960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
268300960129££$$ C     1             DO        IG            I
268400960129££$$ C     GIF(I)        IFGT      WPOGIF
268500960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
268600960129££$$ C                   ENDIF
268700960129££$$ C                   ENDDO
268800941229     C*
268900941229     C*-------------------------------------------------------------------
269000941229     C* APERTURA GIACENZA
269100941229     C*-------------------------------------------------------------------
269200960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
269300960129     C*
269400960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
269500931125     C                   Z-ADD     D98DAG        WDAT
269600930325     C                   EXSR      DATORE
269700950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
269800960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
269900950413£££  C                   Z-ADD     WORE          WPODAG
270000960129$$£  C*********************ENDIF
270100930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
270200930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
270300930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
270400930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
270500930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
270600941229     C*
270700941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
270800941229     C                   Z-ADD     WPODAG        WPOAGC
270900960129     C*
271000960129     C                   ENDIF                                                  <-- 1 ---
271100941229     C*
271200941229     C*-------------------------------------------------------------------
271300930511     C* CHIUSURA GIACENZA
271400941229     C*-------------------------------------------------------------------
271500930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
271600930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
271700931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
271800931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
271900931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
272000931201     C                   Z-ADD     D98DCM        W98DCG
272100930416     C                   ELSE
272200930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
272300930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
272400931201     C                   Z-ADD     WDAODI        W98DCG
272500061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
272600061123     c*  chiusura giacenza, come già fa in altri posti
272700061123     c                   if        d98dfc>0
272800061123     C                   Z-ADD     d98dfc        W98DCG
272900061123     c                   endif
273000061123     c
273100930409     C                   END
273200930409     C                   END
273300930409     C                   END
273400930414     C* TRASF.DATA CHIUSURA
273500931201     C     W98DCG        IFNE      *ZEROS
273600931201     C                   Z-ADD     W98DCG        WDAT
273700930325     C                   EXSR      DATORE
273800930326     C                   Z-ADD     WORE          WPODCG
273900930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
274000930407     C* GIORNO DA ESCLUDERE
274100150216     C                   ADD       12            WPODCG
274200150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
274300150123     c*  il giorno di chiusura dovrebe già essere protetto
274400941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
274500941229     C     WPOCGC        IFEQ      *ZEROS
274600941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
274700941229     C                   ENDIF
274800930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
274900930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
275000931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
275100931129     C                   MOVEL     'C'           WIC                            *CLIENTE
275200150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
275300150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
275400160404     c                   if        wVP='S'
275500160404     c                   sub       24            wr4NOI
275600160404     c                   endif
275700930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
275800930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
275900930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
276000931209     C                   Z-ADD     WPODCG        WR7POI
276100931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
276200931202     C     WR7POO        IFGT      WPODCG
276300931201     C                   Z-ADD     WR7POO        WPODCG
276400930326     C                   END
276500931202     C                   END
276600930511     C*
276700930511     C                   ENDSR
276800031125     C********************************************************************
276900031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
277000031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
277100031125     C*             DEGLI EVENTI DI TIPO GIACENZA
277200031125     C********************************************************************
277300031125     C     CARGIAC       BEGSR
277400031125     C*
277500031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
277600031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
277700031125     C*
277800050405     C                   Z-ADD     D98AAS        KWGCPAAS
277900050405     C                   Z-ADD     D98LNP        KWGCPLNP
278000050405     C                   Z-ADD     D98NRS        KWGCPNRS
278100050405     C                   Z-ADD     D98NSP        KWGCPNSP
278200031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
278300050405     C     KEYGC_P       SETLL     TIGCP01L
278400050405     C                   IF        %equal(TIGCP01L)
278500050405     C     KEYGC_P       READE     TIGCP01L
278600050405     C                   DOW       not %eof(TIGCP01L)
278700031125     C                   ADD       1             IAG
278800031125     C                   EVAL      GCPD(IAG) = GCPDUR
278900050405     C     KEYGC_P       READE     TIGCP01L
279000031125     C                   ENDDO
279100031125     C                   ENDIF
279200031125     C                   ELSE
279300050405     C     KEYGC_P       SETLL     TIGCP21L
279400050405     C                   IF        %equal(TIGCP21L)
279500050405     C     KEYGC_P       READE     TIGCP21L
279600050405     C                   DOW       not %eof(TIGCP21L)
279700031125     C                   ADD       1             IAG
279800031125     C                   EVAL      GCPD(IAG) = GCPDUR
279900050405     C     KEYGC_P       READE     TIGCP21L
280000031125     C                   ENDDO
280100031125     C                   ENDIF
280200031125     C                   ENDIF
280300031125     C*
280400031125     C                   ENDSR
280500941229     C********************************************************************
280600960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
280700941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
280800941229     C********************************************************************
280900960129     C     CALGIC        BEGSR
281000941229     C*
281100960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
281200960129     C*
281300960129     C                   Z-ADD     WOREGI        WTM060
281400960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
281500960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
281600960129     C     D98DAG        IFEQ      *ZEROS
281700960129$$£  C     WOREGI        ADD       12            WTM060            6 0
281800960129     C                   ENDIF
281900960129     C*
282000960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
282100960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
282200960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
282300960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
282400960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
282500960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
282600960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
282700960129     C*
282800960129     C                   ENDIF
282900941229     C*
283000941229     C                   ENDSR
283100060418     C********************************************************************
283200060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
283300060418     c*            l'affidabilità
283400060418     C********************************************************************
283500060418     C     CALCEPA       BEGSR
283600060418     C*
283700060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
283800060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
283900060420     c                   clear                   numggcepa         3 0
284000060418     C*
284100060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
284200060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
284300060420     c                   eval      wgiroepa='0'
284400060418     c                   eval      wfine=' '
284500060418     c
284600060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
284700060420    2C     W             DOwle     IP                                           --- 2 -->
284800060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
284900060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
285000060418     c
285100060420    4c                   if        wgiroepa='0'
285200060420     c                   exsr      ElaInizCEPA
285300060418   x4c                   else
285400060420    5c                   if        pvd(W)>wdat
285500060418     c                   eval      wfine='S'
285600060418    5c                   endif
285700060418    4c                   endif
285800060418     c
285900060420    4c                   if        wfine=' ' and pvd(W)=wdat
286000060420     C*
286100060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
286200060420     c*  finale
286300060420     c                   add       1             numggcepa
286400060420     c
286500060420     C                   z-add     24            WORELA
286600060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
286700060418     c
286800060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
286900060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
287000060418     C                   MOVEL     'C'           WIC                            *CLIENTE
287100060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
287200060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
287300060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
287400060418     c
287500060418     c* Trasformo le ore in data e verifico se c'e' altro evento
287600060418     c*  in sta data che allunga ancora
287700060420     c                   z-add     wr4poo        wore
287800060418     c                   EXSR      OREDAT
287900060418    4c                   endif
288000060418     c
288100060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
288200060418    4c                   if        wfine='S'
288300060418     c                   EXSR      ELAFINECEPA
288400060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
288500060420     c*  inizio range
288600060420     c                   else
288700060420     c                   add       1             w
288800060418    4C                   ENDif                                                  <-- 3 ---
288900060420     c
289000060420     c                   else
289100060420     c                   add       1             w
289200060418    3C                   ENDif                                                  <-- 3 ---
289300060418    2C                   ENDDO                                                  <-- 2 ---
289400060418     c
289500060420     c                   if        wgiroepa='1'
289600060418     c                   EXSR      ELAFINECEPA
289700060418     c                   endif
289800060418     C*
289900060418    1C                   END                                                    <-- 1 ---
290000060418££$$ C*
290100060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
290200060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
290300060420££$$ C     1             DO        Ipa           w
290400060420££$$ C     paif(w)       IFGT      WPOpaif
290500060420££$$ C                   Z-ADD     paif(w)       WPOpaif
290600060418££$$ C                   END
290700060418££$$ C                   ENDDO
290800060418     C*
290900060418     C                   ENDSR
291000060420     C********************************************************************
291100060420     c* Elabora Inizio range eventi particolari che allungano
291200060420     C********************************************************************
291300060420     c     ElaInizCEPA   BEGSR
291400060420     C                   ADD       1             IPA
291500060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
291600060420     C                   EXSR      DATORE
291700060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
291800061017     c
291900060420     c                   eval      wgiroepa='1'
292000060420     c                   eval      wdat=pvd(w)
292100060420     C                   ENDSR
292200060418     C********************************************************************
292300060418     c* Elabora fine range eventi particolari che allungano
292400060418     C********************************************************************
292500060418     c     ElafineCEPA   BEGSR
292600060420     c     numggcepa     mult      24            worela
292700060420     c                   sub       12            worela
292800090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
292900090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
293000160404     c                   if        wVP='S'
293100160404     c                   sub       24            WORELA
293200160404     c                   endif
293300060418     c
293400060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
293500060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
293600060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
293700060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
293800060418     C                   Z-ADD     paif(IPA)     WR7POI
293900060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
294000060418    5C     WR7POO        IFGT      paif(IPA)
294100060418     C                   Z-ADD     WR7POO        paif(IPA)
294200060418    5C                   END
294300061017     c*Sommo 24 ore di inizio giornata
294400061017     c*            : non ho capito perchè ma ci vogliono
294500061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
294600090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
294700090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
294800160404     c                   if        wVP='S'
294900160404     c                   sub       24            paii(IPA)
295000160404     c                   endif
295100061017     c* Se tempi di consegna inferiori alle 24 ore , devo
295200061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
295300061017     c*  partire prima
295400061017     c****               if        d98tcc<24
295500061017     c****               sub       12            paii(ipa)
295600061017     c****               endif
295700060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
295800060418     c* Imposto nuovo giro
295900060420     c                   eval      wgiroepa='0'
296000060418     c                   eval      wfine=' '
296100060420     c                   clear                   numggcepa         3 0
296200060418     c                   ENDSR
296300930511     C********************************************************************
296400930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
296500930511     C********************************************************************
296600010822     C     CALASS        BEGSR
296700010821     C*
296800011211     C                   EXSR      REPMAXCSB
296900930511     C*
297000930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
297100010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
297200011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
297300930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
297400930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
297500930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
297600930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
297700930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
297800930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
297900930511     C                   ELSE                                                   <-- 4 -->
2980009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
298100930512     C                   END                                                    <-- 4 ---
298200930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
298300930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
298400930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
298500930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
298600930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
298700930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
298800930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
298900930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
299000930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
299100930413     C                   END                                                    <-- 2 ---
299200011211     C                   END                                                    <-- 1 ---
299300930406     C*
299400930325     C                   ENDSR
299500930608     C********************************************************************
299600930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
299700930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2998009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
299900930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
300000930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
300100930608     C********************************************************************
300200930608     C     RDRASS        BEGSR
300300930608     C*
300400930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
3005009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
3006009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
3007009407151507 C*********************EXSR RO3ORE
3008009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
300900930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
3010009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
3011009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
3012009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
3013009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3014009407191907 C                   Z-ADD     WPOASI        WPOROU
3015009407191907 C                   EXSR      CHKFES
3016009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3017009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
3018009407191907 C                   ELSE                                                   *E' FERIALE
3019009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
3020009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3021009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
3022009407191907 C                   ENDIF
302300930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
302400930608     C                   MOVEL     'ASS1 '       WR3TAB
302500930608     C                   Z-ADD     *ZEROS        WR3HHI
302600930608     C                   EXSR      RO3ORE
302700930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
302800930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
302900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
303000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
303100930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
303200930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
303300930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
303400930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
303500930608     C*
303600930608     C                   ENDSR
303700930511     C********************************************************************
303800930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
303900930511     C********************************************************************
304000930511     C     CALFED        BEGSR
304100150331     c                   clear                   fddata
304200150331     c                   clear                   WPOFDdata
304300930511     C*
304400051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
304500931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
304600051108     C     D98FFD        oreq      'M'                                          --- 1 -->
304700930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
304800930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
304900930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
305000930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
305100150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
305200150331     c                   if        §ar5dfd>*zeros
305300150331     c                   movel     §ar5dfd       fddata            8 0
305400150331     c                   endif
305500150331     c                   if        fddata>0 and ((d98dti>0 and
305600150331     c                             fddata>d98dti) or d98dti=0)
305700150331     C                   Z-ADD     fddata        WDAT
305800150331     C                   EXSR      DATORE
305900150331     C                   Z-ADD     WORE          WPOfddata
306000150331     c                   endif
306100150331     c
306200930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
306300930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
306400930511     C                   ELSE                                                   <-- 2 -->
3065009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
306600930512     C                   END                                                    <-- 2 ---
306700150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3068001504081507 C                   Z-ADD     WPOfddata     WPOFDI
306900150331     c                   endif
307000150331
307100930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
307200930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
307300930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
307400930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
307500930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
307600930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
307700930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
307800930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
307900930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
308000930511     C                   END                                                    <-- 1 ---
308100930511     C*
308200930511     C                   ENDSR
308300930608     C********************************************************************
308400930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
308500930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3086009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
308700930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
308800930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
308900930608     C********************************************************************
309000930608     C     RDRFED        BEGSR
309100930608     C*
309200930608     C* CALCOLO ORE DEL FERMO DEPOSITO
309300930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3094009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3095001503311907 C                   Z-ADD     WPOFDI        WPOROU
3096001503311907 C                   EXSR      CHKFES
3097001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3098001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3099001503311907 C                   ELSE                                                   *E' FERIALE
3100001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3101001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3102001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3103001503311907 C                   ENDIF
310400150331     c* Se presente la particolarità consegna con più giorni di fermo depo
310500150331     c*  uso questi
310600150331     c                   if        §7rggfd>*zeros
310700150424     c                   movel     §7rggfd       wggfd             2 0
310800150331     c     wggfd         mult      24            Wnore
310900150331     c                   add       wnore         worefd
311000150331     c                   else
311100930608     C                   MOVEL     'FED1 '       WR3TAB
311200930608     C                   Z-ADD     *ZEROS        WR3HHI
311300930608     C                   EXSR      RO3ORE
311400940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
311500150331     c                   endif
311600930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
311700931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
311800931129     C                   MOVEL     'C'           WIC                            *CLIENTE
311900930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
312000930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
312100930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
312200930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
312300930608     C*
312400930608     C                   ENDSR
312500931201     C********************************************************************
312600931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
312700931201     C********************************************************************
312800931201     C     CALDTE        BEGSR
312900140825     c
313000140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
313100140825     c*  che tiene conto solo dei festivi per restituire in output
313200140825     c*  la data teorica di consegna
313300140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
313400140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
313500140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
313600140825     C                   ELSE
313700140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
313800140825     C                   END
313900140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
314000141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
314100141106     c*  quindi passo un flag
314200141106     c                   eval      nosomma='N'
314300140825     C                   EXSR      RO4CDP
314400141106     c                   eval      nosomma=' '
314500140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
314600140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
314700140825     c*  di consegna
314800140825     c                   EXSR      TRAS_DEI
314900140825     c
315000140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
315100140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
315200140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
315300140825     c                             ='M' or %subst(d98gc2:2:1)='P')
315400140825     c                   exsr      Reper_tempi
315500140825     c                   endif
315600931201     C*
315700140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
315800140825     c*  che tiene conto di:
315900140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
316000150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
316100931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
316200931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
316300931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
316400931201     C                   ELSE
316500931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
316600931201     C                   END
316700090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
316800090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
316900931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
317000140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
317100931201     C*
317200150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
317300931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
317400931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
317500090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
317600931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
317700150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
317800150608     c
317900150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
318000150608     c*  che non tiene conto dei tempi del C/Ass
318100150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
318200150609     c*
318300150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
318400150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
318500150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
318600150630     c                   Z-ADD     WPODTI        WR6POI
318700150609     c                   else
318800150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
318900150609     c                   endif
319000150609     c
319100150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
319200150608     c                   eval      wdee='S'
319300150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
319400150608     c                   eval      wdee=' '
319500150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
319600141017
319700141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
319800150219     C**                 EVAL      WTC='C'
319900150219     C**                 EVAL      WIC='C'
320000150219     c**                 z-add     wpoattdata00  WPOROU
320100150219     c**                 z-add     wdaodi        wpodata
320200150219     c**                 exsr      chkfes
320300150219     c**                 if        wslitt='S'
320400150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
320500150219     c**                 z-add     24            wr6noi
320600150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
320700150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
320800150219     c**                 else
320900150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
321000150219     c**                 endif
321100141204     c* Ricalcolo WPOrifDatLav
321200141229     C**                 MOVEL     'OSC  '       WR3TAB
321300141229     C**                 Z-ADD     WHHODI        WR3HHI
321400141229     C**                 EXSR      RO3ORE
321500141229     c**
321600141229     C**                 EVAL      WTC='C'
321700141229     C**                 EVAL      WIC='C'
321800141229     c**                 z-add     wpoattdata00  WPOROU
321900141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
322000141229     c**                 z-add     wr3hho        wr6noi
322100141229     c**                 add       12            wr6noi
322200141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
322300141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
322400141229     c
322500141017     c
322600141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
322700141017     c*  ma sempre devo slittare i festivi
322800141017     C* CONSEGNABILITA'
322900150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
323000150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
323100150219     C**                 EVAL      WTC='C'
323200150219     C**                 EVAL      WIC='C'
323300150219     c**                 z-add     d98dfc        WPOdata
323400150219     c**                 exsr      chkfes
323500150219     c**                 if        wslitt='S'
323600150219     c**                 z-add     wpodfc_00     Wr6poi
323700150219     c**                 z-add     24            wr6noi
323800150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
323900150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
324000150219     c**                 else
324100150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
324200150219     c**                 endif
324300150219     c**                 endif
324400931201     C*
324500931201     C                   ENDSR
324600930511     C********************************************************************
324700930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
324800930511     C********************************************************************
324900930511     C     CALDPC        BEGSR
325000930325     C*
325100930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
325200930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
325300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
325400930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
325500930325     C                   ELSE
325600930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
325700930407     C                   END
325800090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
325900090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
326000930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
326100090902     c
326200931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
326300090901
326400930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
326500930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
326600930423     C* A QUELLA DI RICHIESTA CONSEGNA,
326700930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
326800931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
326900931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
327000931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
327100931206     C                   END
327200930423     C                   END
327300930325     C*
327400930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
327500930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
327600930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
327700090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
327800090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
327900930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
328000931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
328100930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
328200930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
328300930423     C* A QUELLA DI RICHIESTA CONSEGNA,
328400930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
328500931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
328600931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
328700931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
328800931206     C                   END
328900930423     C                   END
329000140822     C*
329100930324     C                   ENDSR
329200150113     C********************************************************************
329300150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
329400150113     C********************************************************************
329500150113     c     Cal_DEE       BEGSR
329600150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
329700150508     c                   clear                   wtempi00C         1
329800151007     c                   clear                   wtempi12C         1
329900150722     c                   clear                   woreOLP
330000151009     c                   clear                   wdiff
330100150219     c
330200150219     c* 1) Se presente l'arrivo della trazione lo uso
330300150219    1c                   select
330400150219     c                   when      wmancatraz=' '  and d98dti>0
330500150630     c                   Z-ADD     WPODTI        WR1POI
330600150219     c
330700150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
330800150219    1c                   when      IDdata>0
330900150325     c                   exsr      CalcoloIMP
331000151008     c* Passo in output il terminal di disguido
331100151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
331200151008     c                   eval      d98flp=%editc(LTP(2):'X')
331300151008     c                   endif
331400151008
331500150528     c* stato IMP/disguido solo se >  alla data di oggi
331600151006     c                   if        wr1poi > wpoattdata00 and
331700151006     c                             (wr1poi-wpoattdata00)>12
331800150427     c                   eval      d98spcdee='IMP'
331900151008     c                   endif
332000151006     c                   if        wr1poi > wpoattdata00 and
332100151006     c                             (wr1poi-wpoattdata00)=12
332200151006     c                             and w991In>12
332300151006     c                   eval      d98spcdee='IMP'
332400151008     c                   endif
332500150219     c
332600150219     c                   other
332700150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
332800150225     c* devo far finta che sia arrivato.
332900150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
333000150508     c*  che tiene conto della data partenza PRIMO  collo
333100150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
333200150508     c*
333300150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
333400150508     c                   if        wr1poi >=wpoattdata00
333500150508     c                              and WPOPTC_DEE > WPOPTC
333600150508     c                   eval      d98spcdee='IMP'
333700150508     c                   endif
333800150219     c                   endsl
333900150113     c
334000150219     c* se il calcolo è < di oggi --> imposto oggi
334100150113     c                   if        wr1poi < wpoattdata00
334200150113     c                   eval      wr1poi= wpoattdata00
334300150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
334400150508     c                   eval      wTempi00C='S'
334500151009     c                   clear                   wdiff
334600150113     c                   endif
334700150217     c
334800150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
334900150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
335000150630     c                   if        wr1poi>wpoattdata00
335100150630     c                   eval      wdiff=wr1poi-wpoattdata00
335200150630     c                   endif
335300151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
335400151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
335500151006     c                   eval      wTempi12C='S'
335600151006     c                   endif
335700160404     c* Se la differenza è > 12 ma oltre soglia devo usare i tempi giusti
335800151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
335900151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
336000160404     c                             and idfgs>0
336100151009     C     wdiff         DIV       24            WDIV
336200151009     C                   MVR                     WRES
336300151009     C                   IF        wres>0
336400151009     c                   eval      wTempi12C='S'
336500151009     c                   else
336600151009     c                   eval      wTempi00C='S'
336700151009     c                   endif
336800151009     c                   endif
336900151009     c
337000151006     c
337100151006     c                   select
337200151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
337300151006     c                             w991in>12 and
337400151006     c                             (deeindist<>'S' or d98cons<>'S'
337500151006     c                             or d98ddc>wdaodi)
337600151006     c
337700151006     c* Se stato IMP lo mantengo per avere messaggio diverso
337800151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
337900151006     c                   if        d98spcdee<>'IMP'
338000151006     c                   eval      d98spcdee='PRV'
338100151006     c                   endif
338200151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
338300150330     c                             (deeindist<>'S' or d98cons<>'S'
338400150330     c                             or d98ddc>wdaodi)
338500150427     c
338600150427     c* Se stato IMP lo mantengo per avere messaggio diverso
338700150427     c                   if        d98spcdee<>'IMP'
338800150304     c                   eval      d98spcdee='PRV'
338900150427     c                   endif
339000150427     c
339100151006   x1c                   other
339200150330     c                   exsr      CAL_OLP
339300150217     c
339400150528     c* Se passato l'orario oppure data distinta < data di oggi
339500150508     c* 1) spedizione in distinta          --> in consegna
339600150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
339700150528     c                             d98ddc<wdaodi)
339800150508    3c                   IF        deeindist='S' and d98cons='S'
339900150508     c                   eval      d98spcdee= 'DDC'
340000150508
340100150508   x3c                   else
340200150507     c
340300150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
340400150722     c*  entro sera
340500150630     c                   if        wr1poi>wpoattdata00
340600150722     c                   z-add     12            woreOLP           3 0
340700150722     c***                add       12            wr1poi
340800150630    4c                   else
340900150507    4c                   if        wr1poi<=wpoattdata00
341000150722     c                   add       24            woreOLP
341100150722     c***                add       24            wr1poi
341200150507    4c                   endif
341300150630    4c                   endif
341400150507     c
341500150722     c****               eval      d98spcdee='OLP'
341600150507     c* memorizzo orario limite picking
341700150507     c                   eval      d98picklim=%editc(picklim:'X')
341800150507    3c                   endif
341900150507    2c                   endif
342000150507     c
342100151006    1c                   endsl
342200150217     c
342300150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
342400150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
342500150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
342600150113     C                   Z-ADD     Wr1poi        W10POI            9 0
342700150113     C                   EXSR      R10FES
342800150113     C                   Z-ADD     W10POO        wr1poi
342900150113     C
343000151006     c                   select
343100151006    1c                   when      wtempi12C='S'
343200151006     c                   z-add     W991IN        wr1noi
343300151006     c                   if        woreOLP>0
343400151006     c                   add       woreOLP       wr1noi
343500151006     c                   eval      d98spcdee='OLP'
343600160404     c                   else
343700160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
343800160404     c**                 if        wVP='S'
343900160404     c**                 add       24            wr1noi
344000160404     c**                 endif
344100151006     c                   endif
344200160404
344300151006    1c                   when      wtempi00C='S'
344400150630     c***                z-add     W990CN        wr1noi
344500150630     c                   z-add     W990IN        wr1noi
344600150722     c
344700150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
344800150722     c*  quindi sottraggo 24
344900151009     c                   if        wr1noi>24  and wdiff=0
345000150722     c                   sub       24            wr1noi
345100150722     c                   endif
345200150722     c
345300150722     c* se passato orario limite picking, slitto in avanti
345400150722     c                   if        woreOLP>0
345500150722     c                   add       woreOLP       wr1noi
345600150722     c                   eval      d98spcdee='OLP'
345700160404     c                   else
345800160404     c* aggiungo 24 ore se la bolla è un VEDI PACCO o EX VEDI PACCO DPD
345900160404     c**                 if        wVP='S'
346000160404     c**                 add       24            wr1noi
346100160404     c**                 endif
346200150722     c                   endif
346300150722     c
346400151006   x1c                   other
346500150722     c
346600150714     c                   if        wmancatraz=' '  and d98dti>0
346700150630     c                   z-add     w99tci        wr1noi
346800150714     c                   else
346900150714     c                   z-add     w99tcc        wr1noi
347000150714     c                   endif
347100160404
347200160401     c
347300150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
347400150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
347500150722     c                   if        woreOLP=24 and wr1noi<=24
347600150722     c                   add       woreOLP       wr1noi
347700150722     c                   eval      d98spcdee='OLP'
347800150722     c                   endif
347900150722     c                   if        woreOLP=12 and wr1noi<24
348000150722     c                   add       woreOLP       wr1noi
348100150722     c                   eval      d98spcdee='OLP'
348200150722     c                   endif
348300150722     c
348400151006    1c                   endsl
348500150629     c
348600150722    1c**                 if        WR1NOI=10
348700150722     c**                 z-add     12            wr1noi
348800150722     c**                 endif
348900150722     c
349000150722     c     wr1noi        div       12            wdiv
349100150722     c                   mvr                     wres
349200150722     c                   if        wres >0
349300150722     c                   add       2             wr1noi
349400150722     c                   endif
349500150722     c
349600150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
349700150629    1c******             endif
349800150508     c
349900150605     c                   eval      wdee='S'
350000150113     C                   EXSR      RO1CDP
350100150605     c                   eval      wdee=' '
350200150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
350300150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
350400150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
350500150113     C* A QUELLA DI RICHIESTA CONSEGNA,
350600150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
350700150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
350800150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
350900150331     c*  2) IL        -> è il giorno indicato
351000150331     C     D98DCR        IFNE      *ZEROS
351100150331     C     D98tcr        andne     'P'
351200150331     C***  WPOdpcee      IFLE      WPOCR2
351300150331     C***  WPOCR2        SUB       12            WPOdpcee
351400150330     c
351500150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
351600150330     c*   dare la prima data utile di consegna
351700150330     C     WPOdpcee      IFLE      WPOCRF_dee
351800150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
351900150330     C                   z-add     WPOCRF_dee    WPOdpcee
352000150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
352100150409     c                   if        wpodpcee>wpoattdata00
352200150409     c                   eval      d98spcdee='PRV'
352300150409     c                   endif
352400150113     C                   END
352500150331     c
352600150220     c
352700150113     C                   END
352800150220     c
352900150113     c                   ENDSR
353000150330     C********************************************************************
353100150330     C*   Calcolo orario da tabella OLP
353200150330     C********************************************************************
353300150330     c     Cal_OLP       BEGSR
353400150330     c                   eval      picklim=0900
353500150330     c* Dopo l'ora limite picking, passo al giorno dopo
353600150330     C                   MOVEL(P)  'OLP'         TBECOD
353700150330     C                   MOVEL(P)  d98lna        TBEKE1
353800150330     C                   MOVEL(P)  wdaodi        TBEKE2
353900150330     C     KTBE          CHAIN     TNTBE01L
354000150330     C                   IF        not %found(TNTBE01L)
354100150330     c                   clear                   tbeke2
354200150330     C     KTBE          CHAIN     TNTBE01L
354300150330     c                   endif
354400150330     C                   IF        not %found(TNTBE01L)
354500150330     C                   MOVEL(P)  '000'         TBEKE1
354600150330     C     KTBE          CHAIN     TNTBE01L
354700150330     c                   endif
354800150330     c
354900150330     C                   IF        %found(TNTBE01L) and
355000150330     c                             %subst(tbeuni:1:4)>*zeros
355100150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
355200150330     c                   endif
355300150330     c                   ENDSR
355400150325     C********************************************************************
355500150325     C*   Riclcolo data se colli in IMP/disguido
355600150325     C********************************************************************
355700150325     c     CalcoloIMP    BEGSR
355800150325     C                   Z-ADD     IDdata        WDAT
355900150325     C                   EXSR      DATORE
356000150325     C                   Z-ADD     WORE          WR4POI
356100150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
356200151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
356300150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
356400150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
356500150325     c* calcolo il terminal di partenza
356600150325     c                   clear                   fnlv55ds
356700150325     c                   eval      d55tla='L'
356800150325     c                   eval      d55tpt='P'
356900150325     c                   eval      d55lin=IDfgs
357000150325     c                   eval      d55drf=IDdata
357100150325     c                   call      'FNLV55R'
357200150325     c                   parm                    fnlv55ds
357300150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
357400150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
357500150325     c                   EXSR      RTTRAZ
357600150325    3c                   if        d98err<>'1'
357700150325     C                   Z-ADD     CASTcn        wr4noi
357800150325     c                   else
357900150325     C                   Z-ADD     d98ttc        wr4noi
358000150325    3C                   endif
358100150325
358200150325   x2c                   else
358300150325     C                   Z-ADD     d98ttc        wr4noi
358400150325    2C                   endif
358500150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
358600150325     c*  perchè partirà un giorno dopo
358700150325    3c                   if        IDnpg='P'
358800150325     c                   add       24            WR4noi
358900150325    3c                   endif
359000150325     c
359100150325     c                   exsr      ro4cdp
359200150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
359300150325     c                   ENDSR
359400930604     C********************************************************************
359500930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
359600930604     C********************************************************************
359700930604     C     TPODAT        BEGSR
359800960229     C*
359900960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
360000960229     C*
360100960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
360200960229     C     WPOPTI        IFEQ      *ZEROS
360300960229     C                   Z-ADD     *ZEROS        D98DTI
360400960229     C                   Z-ADD     *ZEROS        D98HTI
360500960229     C                   ELSE
360600960229     C                   Z-ADD     WPOPTI        WORE
360700960229     C                   EXSR      OREDAT
360800960229     C                   Z-ADD     WDAT          D98DTI
360900960229     C     WRES          MULT      100           D98HTI
361000960229     C                   END
361100960229     C*
361200960229     C                   ELSE                                                   <-- 1 -->
361300930604     C*
361400931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
361500140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
361600140825     c*******            exsr      Tras_DEI
361700140825     c
361800930604     C*
361900150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
362000140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
362100140820     c* è data da :
362200140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
362300140820     c*  Tenere  conto, ovvero slittare in avanti, per :
362400140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
362500140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
362600140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
362700140911     c*
362800140820     c*  Da azzerare nei seguenti casi:
362900150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
363000150324     c* 4) bolla export    7) Cod manca consegna
363100140911     c* 8) bolla con DCR DOPOil o PRIMAdel
363200150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
363300150119     c*10) se completamente non arrivata e collo in IMP
363400150119     c
363500140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
363600150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
363700140925     C*  - FD   - export -
363800150217     c                   movel     'S'           DEEinDIST         1
363900150113     c                   eval      wcaldpcee='S'
364000150304     c                   eval      D98SPCDEE='PRV'
364100150324     c                   clear                   avvcmc            1
364200150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
364300150323     c                   if        d98cmc<>*blanks
364400150324     C                   Z-ADD     1             II
364500150326     C     d98cmc        LOOKUP    C2A(II)                                99
364600150324     C                   MOVEL     D2A(II)       usa2A
364700150324    4C     §2AFTC        IFEQ      'A'
364800150326     c                   eval      avvcmc='A'
364900150323     c                   endif
365000150324    4C     §2AFTC        IFEQ      'G'
365100150326     c                   eval      avvcmc='G'
365200150324     c                   endif
365300150324     c                   endif
365400140820     c
365500140911     c                   z-add     d98lna        EX
365600150113     c*  la azzero nei casi previsti                 0
365700150601    1c                   select
365800150119     c* 1) Se la bolla e consegnata
365900140925     c                   when      d98dcm>0
366000150113     c                   clear                   wcaldpcee
366100150216     c                   eval      d98spcdee='CON'
366200150217     c                   movel     ' '           DEEinDIST         1
366300150323     c* 2) Se pratica di danno
366400150603     c                   when      flgdan='S'
366500150323     c                   clear                   wcaldpcee
366600150323     c                   eval      d98spcdee='CA '
366700150323     c                   eval      deeindist='S'
366800150324     c* 3) Se bloccata/rimpallo
366900150114     c                   when      wd98fbc='B'
367000150113     c                   clear                   wcaldpcee
367100150216     c                   eval      d98spcdee='BLO'
367200150217     c                   movel     ' '           DEEinDIST         1
367300150324     c* 4) Se bolla export
367400150324     c                   when      fl1(EX)='E'
367500150324     c                   clear                   wcaldpcee
367600150324     c                   movel     ' '           DEEinDIST         1
367700150324     c                   eval      d98spcdee='EXP'
367800150324     c* 5) Se giacente  o in attesa di apertura giacenza
367900150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
368000150324     c                   clear                   wcaldpcee
368100150324     c                   eval      d98spcdee='GIA'
368200150324     c                   eval      deeindist='S'
368300150119     c* 7) Se codice manca consegna
368400150326     c                   when      d98cmc<>*blanks and avvcmc='A'
368500150324     c                   clear                   wpodpcee
368600150324     c                   clear                   wcaldpcee
368700150324     c                   movel     ' '           DEEinDIST         1
368800150324     c                   eval      d98spcdee='ATT'
368900150410     c* 7) Bolla in sospensione consegna
369000150410     c                   when      §B4LBGE='S'
369100150410     c                   clear                   wpodpcee
369200150410     c                   clear                   wcaldpcee
369300150410     c                   movel     ' '           DEEinDIST         1
369400150410     c                   eval      d98spcdee='SOS'
369500150616     c
369600150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
369700161031     c                   when      wrbl='R'  or d98cbo='M '
369800150616     c                   clear                   wcaldpcee
369900150616     c                   eval      d98spcdee='   '
370000150616     c                   eval      deeindist='S'
370100150601   x1c                   other
370200150325     c*
370300150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
370400150331     c                   if        d98ffd='S'
370500150601    2c                   select
370600150403     c* FD escluso dai contatti
370700150601    2c                   when      §B4LTFD= 'N'
370800150403     c                   eval      d98spcdee='FD3'
370900150403     c                   clear                   wcaldpcee
371000150403     c                   movel     ' '           DEEinDIST         1
371100150403
371200150403     c* FD non contattato ( o da ricontattare)
371300150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
371400150401     c                             olv82sts<>'1'
371500150323     c                   clear                   wcaldpcee
371600150323     c                   movel     ' '           DEEinDIST         1
371700150323     c                   eval      d98spcdee='FFD'
371800150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
371900150424     c                   eval      wpodpcee=wpoffi
372000150403
372100150403     c* FD contattato e     IN distinta
372200150601    2c                   when      d98cons='S'
372300150401     c                   eval      d98spcdee='FD2'
372400150401     c                   clear                   wcaldpcee
372500150401     c                   movel     ' '           DEEinDIST         1
372600150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
372700150424     c                   eval      wpodpcee=wpoffi
372800150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
372900150427     c*  ritirare la merce
373000150427     C                   MOVEL     'C'           WTC                            *PARTENZA
373100150427     C                   MOVEL     'C'           WIC
373200150427     C                   Z-ADD     D98ddc        WDAT
373300150427     C                   EXSR      DATORE
373400150427     c                   z-add     wore          wr4poi
373500150427     c                   z-add     24            wr4noi
373600150427     C                   EXSR      RO4CDP
373700150427     c                   z-add     wr4poo        wore
373800150427     c                   exsr      OREDAT
373900150427     c                   movel     wdat          D98DTCTD
374000150601   x2c                   other
374100150403     c* FD contattato e NON in distinta
374200150331     c                   eval      d98spcdee='FD1'
374300150408     c                   eval      wpodpcee=wpoffi
374400150331     c                   clear                   wcaldpcee
374500150331     c                   movel     ' '           DEEinDIST         1
374600150601    2c                   endsl
374700150331     c
374800150331     c                   endif
374900150403     c
375000150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
375100150601    2c                   if        (d98tc1='A' or d98tc2='A' or
375200150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
375300150601     c                             and d98ffd= ' '
375400150325     c                   clear                   wcaldpcee
375500150325     c                   movel     ' '           DEEinDIST         1
375600150601    3c                   if        §B4LBAS= 'N'
375700150427     c                   eval      d98spcdee='AP3'
375800150403     c                   else
375900150403     c                   eval      d98spcdee='APP'
376000150601    3c                   endif
376100150601    2c                   endif
376200150325     c*   se la bolla ha il C/Assegno verifico se da contattare
376300150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
376400150325     c                   clear                   wcaldpcee
376500150325     c                   movel     ' '           DEEinDIST         1
376600150601    3c                   if        §B4LBAS= 'N'
376700150427     c                   eval      d98spcdee='CA3'
376800150403     c                   else
376900150403     c                   eval      d98spcdee='CAS'
377000150601    3c                   endif
377100150601    2c                   endif
377200150325     c* Per questi 3 casi devo impostare la data contatto destinatario
377300150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
377400150325     c                             d98spcdee='CAS'
377500150601    2c                   select
377600150601    2c                   when      d98dti>0 and wmancatraz= ' '
377700150325     c                   movel     d98dti        d98dtctd
377800150403     c* se piene le ore già portate ad un giorno lavorativo
377900150403     c                   if        wpodti>0
378000150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
378100150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
378200150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
378300150403     C                   Z-ADD     WPODTI        W10POI            9 0
378400150403     C                   EXSR      R10FES
378500150403     c                   z-add     w10poo        wore
378600150403     C                   EXSR      OREDAT
378700150403     C                   movel     WDAT          D98dtctd
378800150403     c                   endif
378900150403     c
379000150601    2c                   when      d98dam>0
379100150421     C                   Z-ADD     D98dam        WDAT
379200150421     C                   EXSR      DATORE
379300150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
379400150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
379500150421     C                   Z-ADD     Wore          W10POI            9 0
379600150421     C                   EXSR      R10FES
379700150421     c                   z-add     w10poo        wore
379800150421     C                   EXSR      OREDAT
379900150421     C                   movel     WDAT          D98dtctd
380000150601   x2c                   other
380100150325     c* data teorica di arrivo tenendo conto dell'IMP
380200160426     c                   if        d98nsp>0
380300150325     c                   exsr      letbrv
380400160426     c                   endif
380500150325     c                   if        idDATA>0
380600150325     c                   exsr      CalcoloIMP
380700150325     c                   z-add     wr1poi        WORE
380800150325     c                   else
380900150325     c                   z-add     wpoptc        WORE
381000150325     c                   endif
381100150325     C                   EXSR      OREDAT
381200150325     c                   movel     wdat          d98dtctd
381300150601    2c                   endsl
381400150325     c
381500150325     c                   endif
381600150325     c
381700150601    1c                   endsl
381800140820     c
381900150113     c* Se la bolla non è in distinta calcolo la data se previsto
382000150220     c                   if        wcaldpcee='S' and
382100150220     c                             (d98cons<>'S'  or (d98cons='S' and
382200150220     c                             evemic<>'SI'))
382300150219     c
382400150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
382500150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
382600150219     c                   if        d98nsp>0 and wtrasm<>'D' and
382700150219     c                             d98dam=0
382800150219     c                   exsr      letBRV
382900150219     c                   endif
383000150219     c
383100150217     c                   eval      d98spcdee='PRG'
383200150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
383300150320     c                             d98tc1='S' or d98tc2='S')
383400150320     c                   eval      d98spcdee='APP'
383500150320     c                   endif
383600150320     c
383700150113     c                   exsr      cal_DEE
383800150113     c                   endif
383900150113     c
384000140820     C     WPOdpcee      IFEQ      *ZEROS
384100931202     C                   Z-ADD     *ZEROS        D98DEE
384200931202     C                   Z-ADD     *ZEROS        D98OEE
384300930604     C                   ELSE
384400140820     C                   Z-ADD     WPOdpcee      WORE
384500141021     C******             SUB       12            WORE
384600930604     C                   EXSR      OREDAT
384700931202     C                   Z-ADD     WDAT          D98DEE
384800090903     c                   if        d98tcc<>10
384900090903     C                   ADD       12            WRES
385000090903     c                   else
385100090903     C                   ADD       10            WRES
385200090903     c                   endif
385300090903
385400931202     C     WRES          MULT      100           D98OEE
385500090903     c
385600090903     c                   if        d98oee=1000
385700090903     c                   eval      d98oee=1030
385800090903     c                   endif
385900140925     C                   END
386000150311     c
386100150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
386200150311     c                   select
386300150311     c                   when      d98spcdee='OLP'
386400140820     c
386500150220     c* Se bolla in distinta --> data prevista = data distinta
386600150311     c                   when      deeindist='S'  and
386700150113     c                             d98cons='S'
386800150220
386900150225     c*  non la propongo senza MIC e se presente disguido o IMP
387000150220     c                   if        d98dee>d98ddc and evemic<>'SI'
387100150220     c                             and IDdata>0
387200150220     c                   else
387300150220
387400140820     c                   eval      d98dee=d98ddc
387500140925     c                   clear                   d98oee
387600150320     c* se la data è > di oggi stato PRV se non è in IMP
387700150320     c                   if        d98dee>wdaodi
387800150320     c                   if        d98spcdee<>'IMP'
387900150311     c                   eval      d98spcdee='PRV'
388000150320     c                   endif
388100150320     c
388200150225     c                   else
388300150217     c* se c'è il MIC
388400150217     c                   if        evemic='SI'
388500150217     c                   eval      d98spcdee='DDC'
388600150225     c                   endif
388700150217     c                   endif
388800150220     c                   endif
388900150217     c
389000150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
389100150722     c                   eval      d98spcdee='PRV'
389200150311     c                   endsl
389300931202     C*
389400931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
389500931202     C     WPODCI        IFEQ      *ZEROS
389600931202     C                   Z-ADD     *ZEROS        D98DCI
389700931202     C                   Z-ADD     *ZEROS        D98HCI
389800931202     C                   ELSE
389900931202     C                   Z-ADD     WPODCI        WORE
390000931202     C                   EXSR      OREDAT
390100931202     C                   Z-ADD     WDAT          D98DCI
390200090902     c                   if        d98tci<>10
390300931202     C                   ADD       12            WRES
390400090902     c                   else
390500090902     C                   ADD       10            WRES
390600090902     c                   endif
390700090902
390800931202     C     WRES          MULT      100           D98HCI
390900090901     c
391000090903     c                   if        d98hci=1000
391100090901     c                   eval      d98hci=1030
391200090901     c                   endif
391300090901     c
391400931202     C                   END
391500931202     C*
391600931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
391700931202     C     WPODCE        IFEQ      *ZEROS
391800931202     C                   Z-ADD     *ZEROS        D98DCE
391900931202     C                   Z-ADD     *ZEROS        D98HCE
392000931202     C                   ELSE
392100931202     C                   Z-ADD     WPODCE        WORE
392200931202//// C**************       SUB  12        WORE
392300931202     C                   EXSR      OREDAT
392400931202     C                   Z-ADD     WDAT          D98DCE
392500090902     c                   if        d98tcc >10
392600931202     C                   ADD       12            WRES
392700090902     c                   else
392800090902     C                   ADD       10            WRES
392900090902     c                   endif
393000090902     c
393100931202     C     WRES          MULT      100           D98HCE
393200090902     c
393300090903     c                   if        d98hce=1000
393400090902     c                   eval      d98hce=1030
393500090902     c                   endif
393600090902     c
393700931202     C                   END
393800960229     C*
393900960229     C                   ENDIF                                                  <-- 1 ---
394000930604     C*
394100930604     C                   ENDSR
394200150218     C********************************************************************
394300150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
394400150218     C********************************************************************
394500150218     c     LetBRV        BEGSR
394600150219     c                   clear                   bollaOK           1
394700150219     c                   clear                   IDnpg
394800150219     c                   clear                   IDdata
394900150219     c                   clear                   IDora
395000150219     c                   clear                   IDfgs
395100150219     c
395200150218     C     Kart          SETLL     FNART01L
395300150218     C     Kart          READE     FNART01L
395400151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
395500150219     c                   clear                   DISfgs
395600150219     c                   clear                   DISdata
395700150219     c                   clear                   DISnpg
395800150219     c                   clear                   DISora
395900150219     c                   clear                   IMPdata
396000150219     c                   clear                   IMPfgs
396100150508     c* per ogni collo verifico se in IMP o disguidato
396200150218     c* al primo che NON trovo, esco come --> OK
396300150218     c     kbrv          setll     fnbrv07l
396400150218     c     kbrv          reade     fnbrv07l
396500150219    2c                   dow       not %eof(fnbrv07l)
396600150219    3c                   if        brvatr='D'  or brvnpg=3
396700150218     c* prendo la data della spunta
396800150218     c                   clear                   fnlv53ds
396900150219     c                   eval      d53tla='L'
397000150218     C                   MOVEL     BRvNPG        D53NPG
397100150218     C                   Z-ADD     BRvNFV        D53NFV
397200150218     C                   MOVEL     BRvFGS        D53FGS
397300150218     C                   CALL      'FNLV53R'
397400150218     C                   PARM                    fnlv53ds
397500150218    4c                   if        d53err<>*blanks
397600150219    5c                   if        brvdcs>0
397700150218     c                   eval      d53dfv=brvdcs
397800150218     c                   else
397900150219    6c                   if        brvdfs>0
398000150218     c                   eval      d53dfv=brvdfs
398100150219    6c                   endif
398200150219    5c                   endif
398300150219    4c                   endif
398400150218     c
398500150219    4c                   select
398600150219     c* se si tratta di DISGUIDO
398700150218     c                   when      brvatr='D'
398800150219     c
398900150219    5c                   if        d53dfv=DISdata
399000150219     c* Verifico l'ora
399100150219    6c                   if        brvhcs>0 and brvhcs>DISora
399200150219     c                   eval      DISdata=d53dfv
399300150219     c                   eval      DISfgs =brvfgs
399400150219     c                   eval      DISnpg =brvnpg
399500150219     c                   eval      DISora =brvhcs
399600150219   x6c                   else
399700150219    7c                   if        brvhms>0 and brvhms>DISora
399800150219     c                   eval      DISdata=d53dfv
399900150219     c                   eval      DISfgs =brvfgs
400000150219     c                   eval      DISnpg =brvnpg
400100150219     c                   eval      DISora =brvhms
400200150219    7c                   endif
400300150219    6c                   endif
400400150219    5c                   endif
400500150219     c
400600150219    5c                   if        d53dfv>DISdata
400700150219     c                   eval      DISdata=d53dfv
400800150219     c                   eval      DISfgs =brvfgs
400900150219     c                   eval      DISnpg =brvnpg
401000150219     c                   eval      DISora =brvhcs
401100150219     c                   if        disora=0
401200150219     c                   eval      DISora =brvhms
401300150219     c                   endif
401400150219    5c                   endif
401500150219     c
401600151009     c* se si tratta di IMP
401700151009    4c                   when      d53spg='P' and d53dfv>impdata
401800151009     c                   eval      IMPdata=d53dfv
401900151009     c                   eval      IMPfgs =brvfgs
402000150219    4c                   endsl
402100150219    3c                   endif
402200151009
402300150219     c     kbrv          reade     fnbrv07l
402400150219    2c                   enddo
402500150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
402600150219     c*  esco dal ciclo: la bolla è OK
402700150219    2c                   if        impdata=0 and disdata=0
402800150219     c                   eval      bollaOK='S'
402900150219     c                   clear                   IDdata
403000150219     c                   clear                   IDfgs
403100150219     c                   clear                   IDora
403200150219     c                   clear                   IDnpg
403300150219   x2c                   else
403400150219     c* memorizzo la data più alta
403500150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
403600150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
403700150219     c                   eval      IDdata=IMPdata
403800150219     c                   eval      IDfgs=IMPfgs
403900150219     c                   eval      IDora=0
404000150219     c                   eval      IDnpg='P'
404100150219    3c                   endif
404200150219    3c                   if        (DISdata>0 and DISdata=IDdata
404300150219     c                             and IDnpg<>'P' and DISora>IDora) or
404400150219     c                             (DISdata>0 and DISdata>IDdata)
404500150219     c                   eval      IDdata=DISdata
404600150219     c                   eval      IDfgs=DISfgs
404700150219     c                   eval      IDora=DISora
404800150219     c                   eval      IDnpg=%char(DISnpg)
404900150219    3c                   endif
405000150219    2c                   endif
405100150218     c
405200150218     C     Kart          READE     FNART01L
405300150219    1c                   enddo
405400150219     c
405500150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
405600150508     c**                 if        d98duc> 0 and d98duc>IDdata
405700150508     c**                 eval      bollaOK='S'
405800150508     c**                 clear                   IDdata
405900150508     c**                 clear                   IDfgs
406000150508     c**                 clear                   IDora
406100150508     c**                 clear                   IDnpg
406200150508     c**                 endif
406300150219     c
406400150218     c
406500150218     C                   ENDSR
406600140825     C********************************************************************
406700140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
406800140825     C********************************************************************
406900140825     c     TRAS_DEI      BEGSR
407000140825     C     WPODEI        IFEQ      *ZEROS
407100140825     C                   Z-ADD     *ZEROS        D98DEI
407200140825     C                   Z-ADD     *ZEROS        D98OEI
407300140825     C                   ELSE
407400140825     C                   Z-ADD     WPODEI        WORE
407500140825     C                   SUB       12            WORE
407600140825     C                   EXSR      OREDAT
407700140825     c
407800140825     C                   Z-ADD     WDAT          D98DEI
407900140825     c                   if        d98tci<>10
408000140825     C                   ADD       12            WRES
408100140825     c                   else
408200140825     C                   ADD       10            WRES
408300140825     c                   endif
408400140825     c
408500140825     C     WRES          MULT      100           D98OEI
408600140825     c
408700140825     c                   if        d98oei=1000
408800140825     c                   eval      d98oei=1030
408900140825     c                   endif
409000140825     c
409100140825     C                   END
409200140825     c                   ENDSR
409300930604     C********************************************************************
409400930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
409500930604     C********************************************************************
409600930604     C     CHKFOR        BEGSR
409700930604     C*
409800930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
409900930604     C* E QUINDI FORZO LA CONFORMITA' A SI
410000941227     C     D98ERR        IFEQ      '0'
410100930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
410200930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
410300930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
410400930604     C                   MOVEL     '1'           D98ERR
410500930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
410600930604     C                   END
410700930604     C                   END
410800990212     C*
410900990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
411000990212     C* E QUINDI FORZO LA CONFORMITA' A SI
411100990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
411200990212     C     D98ERR        IFEQ      '0'
411300990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
411400141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
411500990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
411600990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
411700990212     C                   MOVEL     '1'           D98ERR
411800990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
411900990212     C                   END
412000990212     C                   END
412100990308     C*
412200990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
412300990308     C* E QUINDI FORZO LA CONFORMITA' A SI
412400100614     C**   D98ERR        IFEQ      '0'
412500100614     C**   WFBR          IFEQ      'S'                                          *RESO
412600100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
412700100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
412800100614     C**                 MOVEL     '1'           D98ERR
412900100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
413000100614     C**                 END
413100100614     C**                 END
413200930604     C*
413300930604     C                   ENDSR
413400990212     C********************************************************************
413500990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
413600990212     C********************************************************************
413700990212     C     CHKDAN        BEGSR
413800990212     C*
413900990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
414000150603     c                   movel     'N'           opendan           1
414100040809      *
414200040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
414300040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
414400040809      * legate alla spedizione che passo
414500040809     c                   clear                   fidn12ds
414600040809     c                   eval      i12aas = D98AAS
414700040809     c                   eval      i12lnp = D98LNP
414800040809     c                   eval      i12nrs = D98NRS
414900040809     c                   eval      i12nsp = D98NSP
415000040809     c                   eval      i12fel = 'E'
415100040809     c                   eval      i12fan = 'E'
415200040809     c*
415300040809     c                   call      'FIDN12R'
415400040809     c                   parm                    fidn12ds
415500040809     c*
415600040809     c* se non ci sono errori
415700040809     c                   if        o12err = ' '
415800040809     c* se numero di CA trovate maggiore di zero
415900040809     c                   if        o12nca > 0
416000040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
416100150603     c
416200161215     c                   z-add     1             aa                2 0
416300161215     c                   dow       aa <= o12nca
416400150603     C* APERTA
416500161215     C***  skdch(aa)     IFgt      0
416600161215     c****               movel     'S'           opendan
416700161215     c***                endif
416800161215     c                   if        skdca(aa)>0 and skdca(aa)>d98dac
416900161215     c                   eval      d98dac =skdca(aa)
417000161215     c                   eval      d98dcc =skdch(aa)
417100161215     c                   endif
417200150603     c*
417300161215     c                   add       1             aa
417400161215     c                   enddo
417500150603     c
417600040809     c                   endif
417700040809     c                   endif
417800990212     C*
417900990212     C                   ENDSR
418000941228     C********************************************************************
418100941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
418200941228     C********************************************************************
418300941228     C     CALORP        BEGSR
418400941228     C*
418500941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
418600941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
418700941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
418800960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
418900950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
419000950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
419100950110     C                   ELSE
419200950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
419300950110     C                   ENDIF
419400941228     C                   EXSR      RO2COP
419500941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
419600941228     C* SPED. IL GIORNO DOPO
419700941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
419800941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
419900941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
420000941228     C*
420100941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
420200941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
420300941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
420400941228     C                   Z-ADD     WPODSP        WR2POI
420500950110     C                   Z-ADD     WPOPPC        WR2POF
420600941228     C                   EXSR      RO2COP
420700941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
420800941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
420900941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
421000941228     C*
421100941228     C                   ENDSR
421200930414     C********************************************************************
421300930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
421400930414     C********************************************************************
421500930414     C     CALORT        BEGSR
421600930414     C*
421700930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
421800931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
421900930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
422000931126     C                   Z-ADD     *ZEROS        W98HF
422100950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
422200950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
422300950110     C                   ELSE
422400950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
422500950110     C                   ENDIF
422600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
422700931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
422800930608     C                   ELSE
422900931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
423000930608     C                   END
423100931126     C                   EXSR      RO2COV
423200931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
423300930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
423400930414     C*
423500930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
423600930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
423700930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
423800931126     C                   Z-ADD     *ZEROS        W98HF
423900950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
424000931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
424100931126     C                   EXSR      RO2COV
424200931126     C                   Z-ADD     WR2NOV        W98RTC
424300930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
424400930414     C*
424500930414     C                   ENDSR
424600930414     C********************************************************************
424700930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
424800930414     C********************************************************************
424900930414     C     CALORC        BEGSR
425000930511     C*
425100930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
425200930511     C* I TEMPI DI RITARDO AD OGGI
425300930511     C     WPODCM        IFEQ      *ZEROS
425400960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
425500930511     C                   END
425600940117     C*
425700940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
425800940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
425900940117     C* ALLA DATA DI FINE CONSEGNABILITA'
426000940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
426100940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
426200940118££££ C     WPODFC        IFNE      *ZEROS
426300940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
426400940118££££ C                   END
426500930414     C*
426600930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
426700930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
426800930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
426900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
427000931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
427100930608     C                   ELSE
427200931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
427300930608     C                   END
427400931126     C                   Z-ADD     WPODCM        WR5POF            9 0
427500931126     C                   EXSR      RO5COV
427600931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
427700930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
427800930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
427900060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
428000060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
428100930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
428200930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
428300930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
428400930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
428500930414     C*
428600930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
428700930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
428800930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
428900931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
429000931126     C                   Z-ADD     WPODCM        WR5POF            9 0
429100931126     C                   EXSR      RO5COV
429200931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
429300930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
429400930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
429500060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
429600060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
429700930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
429800930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
429900930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
430000930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
430100930414     C*
430200930414     C                   ENDSR
430300930414     C********************************************************************
430400930414     C*   CALORR - CALCOLA ORE REALI DI RESA
430500930414     C********************************************************************
430600930414     C     CALORR        BEGSR
430700930414     C*
430800950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
430900950110     C                   ADD       W98RTI        W98RRI
431000941228     C                   ADD       W98RCI        W98RRI
431100950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
431200950110     C                   ADD       W98RTC        W98RRC
431300941228     C                   ADD       W98RCC        W98RRC
431400930414     C*
431500930414     C                   ENDSR
431600930420     C********************************************************************
431700930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
431800930420     C********************************************************************
431900930420     C     CALDIF        BEGSR
432000930420     C*
432100941228     C* PARTENZA
432200941228     C     W98RPI        SUB       D98TPI        W98FPI
432300941228     C     W98RPC        SUB       D98TPC        W98FPC
432400941228     C*
432500930420     C* TRAZIONE
432600930423     C     W98RTI        SUB       D98TTI        W98FTI
432700930423     C     W98RTC        SUB       D98TTC        W98FTC
432800930420     C*
432900930420     C* CONSEGNA
433000090827     C***  W98RCI        SUB       D98TCI        W98FCI
433100090827     C     W98RCI        SUB       w99TCI        W98FCI
433200090827     C***  W98RCC        SUB       D98TCC        W98FCC
433300090827     C     W98RCC        SUB       w99TCC        W98FCC
433400930420     C*
433500930420     C* RESA
433600090921     C***  W98RRI        SUB       D98TRI        W98FRI
433700090921     C     W98RRI        SUB       w99TRI        W98FRI
433800090921     c
433900090921     C***  W98RRC        SUB       D98TRC        W98FRC
434000170104     c
434100170104     c*** se le ore reali interne di RESA usate sono < delle ore reali cliente, uso le
434200170104     c*    reali interne
434300170104     c                   if        w98rri<w98rrc
434400170104     C     W98RRI        SUB       w99TRC        W98FRC
434500170104     c                   else
434600170104     C     W98RRC        SUB       w99TRC        W98FRC
434700170104     c                   endif
434800930420     C*
434900930420     C                   ENDSR
435000930420     C********************************************************************
435100930420     C*   CALCNF - CALCOLA LE CONFORMITA'
435200930420     C********************************************************************
435300930420     C     CALCNF        BEGSR
435400941228     C*
435500941228     C*-------------------------------------------------------------------
435600941228     C* CONFORMITA' PARTENZA  INTERNA
435700941228     C*-------------------------------------------------------------------
435800941228     C                   Z-ADD     W98FPI        W98NPI
435900941228     C     W98FPI        IFLE      *ZEROS
436000941228     C                   MOVEL     'S'           D98CPI
436100941228     C                   ELSE
436200941228     C                   MOVEL     'N'           D98CPI
436300941228     C                   END
436400941228     C*
436500941228     C*-------------------------------------------------------------------
436600941228     C* CONFORMITA' PARTENZA  CLIENTE
436700941228     C*-------------------------------------------------------------------
436800941228     C                   Z-ADD     W98FPC        W98NPC
436900941228     C     W98FPC        IFLE      *ZEROS
437000941228     C                   MOVEL     'S'           D98CPC
437100941228     C                   ELSE
437200941228     C                   MOVEL     'N'           D98CPC
437300941228     C                   END
437400930420     C*
437500930420     C*-------------------------------------------------------------------
437600930420     C* CONFORMITA' TRAZIONE  INTERNA
437700930420     C*-------------------------------------------------------------------
437800930510     C                   Z-ADD     W98FTI        W98NTI
437900930423     C     W98FTI        IFLE      *ZEROS
438000930420     C                   MOVEL     'S'           D98CTI
438100930420     C                   ELSE
438200930420     C                   MOVEL     'N'           D98CTI
438300930420     C                   END
438400930420     C*
438500930420     C*-------------------------------------------------------------------
438600930420     C* CONFORMITA' TRAZIONE  CLIENTE
438700930420     C*-------------------------------------------------------------------
438800930510     C                   Z-ADD     W98FTC        W98NTC
438900930423     C     W98FTC        IFLE      *ZEROS
439000930420     C                   MOVEL     'S'           D98CTC
439100930420     C                   ELSE
439200930420     C                   MOVEL     'N'           D98CTC
439300930420     C                   END
439400930420     C*
439500930420     C*-------------------------------------------------------------------
439600930420     C* CONFORMITA' CONSEGNA  INTERNA
439700930420     C*-------------------------------------------------------------------
439800150226     c                   if        wmancatraz='S'
439900150226     c                   clear                   d98dti
440000150226     c                   clear                   d98hti
440100150226     c                   endif
440200150226     c
440300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
440400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
440500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
440600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
440700050914     c                   if        consideraora='S'
440800050914     c     w98fci        sub       24            w98nci
440900050914     c                   add       24            d98hrc
441000050914     c                   else
441100930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
441200050914     c                   endif
441300080116     c
441400110923     c                   select
441500080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
441600080116     c*  --> trasformo il ritardo in 0
441700090827     c                   when      d98cca='2'
441800080116     c                   z-add     0             w98nci
441900090827
442000080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
442100080116     c*  e sto leggendo la bolla in arrivo
442200080116     c*  --> trasformo il ritardo = 0
442300090827     c                   when      d98cca='1' and d98tbo='A'
442400080116     c                   z-add     0             w98nci
442500090902     c
442600090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
442700090902     c                   when      wmancaTRAZ='S'
442800090902     c                   z-add     0             w98nci
442900090902     c                   clear                   d98dti
443000090902     c                   clear                   d98hti
443100110923     c
443200110923     c* Ignoro ritardo se pefect order
443300130221     c                   when      d98xco >*zeros
443400110923     c                   z-add     0             w98nci
443500090827     c
443600090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
443700091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
443800091014     c*  controllo ora soglia  consegna da tabella OSC)
443900091009     c                   when      d98tci=10   and
444000091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
444100091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
444200091014     c*
444300091014     c                             d98hci=1030
444400091014     C                   MOVEL     'OSC  '       WR3TAB
444500091014     C                   eval      WR3HHI=d98hmc
444600091014     C                   EXSR      RO3ORE
444700131218
444800091014     c                   if        wr3matpom='M' and d98hmc>1030
444900131218     c                   exsr      ctrLAVH1030
445000131218
445100131218     c                   if        wlav1030<>'S'
445200091014     c                   add       12            w98nci
445300091014     c                   endif
445400131218     c                   endif
445500090928     c
445600090827     c                   endsl
445700050914     c
445800930524     C                   ELSE                                                   <-- 1 -->
445900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
446000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
446100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
446200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
446300930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
446400930608     C                   ELSE
446500930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
446600930608     C                   END
446700930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
446800930525     C                   EXSR      RO2COV
446900930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
447000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
447100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
447200930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
447300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
447400930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
447500930608     C                   ELSE
447600930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
447700930608     C                   END
447800931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
447900930525     C                   EXSR      RO2COV
448000930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
448100930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
448200930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
448300930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
448400930525     C* E' IL RITARDO
448500930610     C     WNOCRI        SUB       W98RCI        W98NCI
448600050901     c
448700050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
448800050901     c*  --> trasformo il ritardo in anticipo
448900050901     c                   if        d98cca='2'
449000080116     c***                z-sub     w98nci        w98nci
449100080116     c                   z-add     0             w98nci
449200050901     c                   endif
449300111018     c* Ignoro ritardo se pefect order
449400130221     c                   if        d98xco >*zeros
449500111018     c                   z-add     0             w98nci
449600111018     c                   endif
449700061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
449800061121     c*  e sto leggendo la bolla in arrivo
449900061121     c*  --> trasformo il ritardo in anticipo
450000061121     c                   if        d98cca='1' and d98tbo='A'
450100080116     c***                z-sub     w98nci        w98nci
450200080116     c                   z-add     0             w98nci
450300061121     c                   endif
450400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
450500070827     c*  mittente, --> consegnato in anticipo
450600070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
450700140403     c                   if        Callar5=' '
450800070827     c                   exsr      LeggiAR5
450900140403     c                   endif
451000070827     c
451100070827     c                   if        dcrpart<>'S'
451200070824     c                   z-sub     w98nci        w98nci
451300070824     c                   endif
451400070827     c
451500070824     c                   endif
451600050907     c
451700050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
451800050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
451900050907     c*  lo trasformo in anticipo perchè non sono in ritardo
452000050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
452100050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
452200050916     c***                if        d98dfc>0
452300070828     c                   if        d98dcm=0  and w98nci>0
452400050907     c                   z-sub     w98nci        w98nci
452500050907     c                   sub       oreILoDOPO    w98nci
452600050907     c                   endif
452700050901     c
452800930525     C                   ELSE                                                   <-- 2 -->
452900070521     c* non do ritardo se la consegna reale cade nel giorno
453000070521     c*  della consegna richiesta se presente IL
453100100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
453200070521     c                             wpodcm<=wpocr1
453300070521     c                   clear                   w98nci
453400100224     c                   else                                                   ---2a -->
453500930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
453600090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
453700090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
453800930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
453900930525     C* E' IL RITARDO O L'ANTICIPO
454000930525     C     W98RCI        SUB       WNOCRF        W98NCI
454100070115     c
454200070115     c* non do ritardo se comunque la consegna reale cade nel giorno
454300070115     c*  della consegna richiesta se presente IL
454400070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
454500070521     c***                          wpodcm<=wpocr1  and w98nci>0
454600070521     c***                clear                   w98nci
454700070521     c***                endif
454800070115     c
454900930525     C                   ELSE                                                   <-- 3 -->
455000930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
455100930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
455200930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
455300930525     C                   END                                                    <-- 3 ---
455400100224     C                   END                                                    <-- 2a---
455500100224
455600100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
455700100224     c                   select
455800100224     c                   when      d98cca='2'
455900100224     c                   z-add     0             w98nci
456000100224
456100100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
456200100224     c*  e sto leggendo la bolla in arrivo
456300100224     c                   when      d98cca='1' and d98tbo='A'
456400100224     c                   z-add     0             w98nci
456500100224     c                   endsl
456600100224     c
456700100224     C                   END                                                    <-- 2 ---
456800070521     C                   END                                                    <-- 1 ---
456900070521     c
457000970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
457100970915     C*/         WAZNCI    IFEQ 'S'
457200970915     C*/         W98NCI    ANDLT*ZEROS
457300970915     C*/                   Z-ADD*ZEROS    W98NCI
457400970915     C*/                   ENDIF
457500970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
457600050922     c* lo faccio solo se non c'e' la data cons richiesta
457700970915     C     D98DTI        IFEQ      *ZEROS
457800050922     c     d98dcr        andeq     *zeros
457900970915     C                   Z-ADD     *ZEROS        W98NCI
458000970915     C                   ENDIF
458100930524     C*
458200930524     C* CONTROLLO CONFORMITA
458300930423     C     W98NCI        IFLE      *ZEROS
458400930524     C                   MOVEL     'S'           D98CCI
458500930420     C                   ELSE
458600930524     C                   MOVEL     'N'           D98CCI
458700930420     C                   END
458800930420     C*
458900930420     C*-------------------------------------------------------------------
459000930420     C* CONFORMITA' CONSEGNA  CLIENTE
459100930420     C*-------------------------------------------------------------------
459200930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
459300930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
459400930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
459500050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
459600050914     c                   if        consideraora='S'
459700050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
459800050914     c                   else
459900050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
460000050914     c                   endif
460100080116     c
460200080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
460300080116     c*  --> trasformo il ritardo =0
460400090827     c                   select
460500090827     c                   when      d98cca='2'
460600080116     c                   z-add     0             w98ncc
460700090827
460800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
460900080116     c*  e sto leggendo la bolla in arrivo
461000080116     c*  --> trasformo il ritardo =0
461100090827     c                   when      d98cca='1' and d98tbo='A'
461200080116     c                   z-add     0             w98ncc
461300090902     c
461400090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
461500090902     c                   when      wmancaTRAZ='S'
461600090902     c                   z-add     0             w98ncc
461700110923     c* Ignoro ritardo se Perfect Order
461800130221     c                   when      d98xco >*zeros
461900110923     c                   z-add     0             w98ncc
462000090902     c
462100090827     c
462200090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
462300090827     c* sono  in ritardo di 12 ore
462400091009     c                   when      d98tcc=10  and
462500091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
462600091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
462700091014     c*
462800091014     c                             d98hce=1030
462900091014     C                   MOVEL     'OSC  '       WR3TAB
463000091014     C                   eval      WR3HHI=d98hmc
463100091014     C                   EXSR      RO3ORE
463200091014     c                   if        wr3matpom='M' and d98hmc>1030
463300131218     c                   exsr      ctrLAVH1030
463400131218
463500131218     c                   if        wlav1030<>'S'
463600131218     c                   add       12            w98ncc
463700131218     c                   endif
463800131218     c****               add       12            w98ncc
463900091014     c                   endif
464000131218     c
464100091014     c***                z-add     12            w98ncc
464200090827     c                   endsl
464300050914     c
464400930524     C                   ELSE                                                   <-- 1 -->
464500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
464600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
464700930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
464800930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
464900930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
465000930525     C                   EXSR      RO2COV
465100930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
465200930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
465300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
465400930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
465500930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
465600931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
465700930525     C                   EXSR      RO2COV
465800930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
465900930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
466000930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
466100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
466200930525     C* E' IL RITARDO
466300930525     C     WNOCRI        SUB       W98RCC        W98NCC
466400050901     c
466500050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
466600050901     c*  --> trasformo il ritardo in anticipo
466700050901     c                   if        d98cca='2'
466800080116     c***                z-sub     w98ncc        w98ncc
466900080116     c                   z-add     0             w98ncc
467000050901     c                   endif
467100111018     c* Ignoro ritardo se pefect order
467200130221     c                   if        d98xco >*zeros
467300111018     c                   z-add     0             w98ncc
467400111018     c                   endif
467500061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
467600061121     c*  e sto leggendo la bolla in arrivo
467700061121     c*  --> trasformo il ritardo in anticipo
467800061121     c                   if        d98cca='1' and d98tbo='A'
467900080116     c***                z-sub     w98ncc        w98ncc
468000080116     c                   z-add     0             w98ncc
468100061121     c                   endif
468200070824     c
468300070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
468400070827     c*  mittente, --> consegnato in anticipo
468500070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
468600070827     c* SBR per vedere se tassativa
468700070827     c                   if        Callar5=' '
468800070827     c                   EXSR      LeggiAR5
468900070827     c                   endif
469000070827     c                   if        dcrpart<>'S'
469100070824     c                   z-sub     w98ncc        w98ncc
469200070824     c                   endif
469300070824     c                   endif
469400050907     c
469500050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
469600050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
469700050907     c*  lo trasformo in anticipo perchè non sono in ritardo
469800050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
469900050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
470000050916     c***                if        d98dfc>0
470100070828     c                   if        d98dcm=0 and w98ncc>0
470200050907     c                   z-sub     w98ncc        w98ncc
470300050907     c                   sub       oreILoDOPO    w98ncc
470400050907     c                   endif
470500050901     c
470600930525     C                   ELSE                                                   <-- 2 -->
470700070521     c* Non do ritardo se la consegna reale cade nel giorno
470800070521     c*  della consegna richiesta se presente IL
470900100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
471000070521     c                             wpodcm<=wpocr2
471100070521     c                   clear                   w98ncc
471200100224     c                   else                                                   <--2a -->
471300070521     c
471400930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
471500090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
471600090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
471700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
471800930525     C* E' IL RITARDO O L'ANTICIPO
471900930525     C     W98RCC        SUB       WNOCRF        W98NCC
472000070115     c
472100070115     c* non do ritardo se comunque la consegna reale cade nel giorno
472200070115     c*  della consegna richiesta se presente IL
472300070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
472400070521     c***                          wpodcm<=wpocr2 and w98ncc>0
472500070521     c***                clear                   w98ncc
472600070521     c***                endif
472700070115     c
472800930525     C                   ELSE                                                   <-- 3 -->
472900930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
473000930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
473100930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
473200930525     C                   END                                                    <-- 3 ---
473300100224     C                   END                                                    <-- 2a---
473400100224
473500100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
473600100224     c                   select
473700100224     c                   when      d98cca='2'
473800100224     c                   z-add     0             w98nci
473900100224
474000100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
474100100224     c*  e sto leggendo la bolla in arrivo
474200100224     c                   when      d98cca='1' and d98tbo='A'
474300100224     c                   z-add     0             w98nci
474400100224     c                   endsl
474500100224     c
474600100224     C                   END                                                    <-- 2 ---
474700070521     C                   END                                                    <-- 1 ---
474800930524     C*
474900930524     C* CONTROLLO CONFORMITA
475000930423     C     W98NCC        IFLE      *ZEROS
475100930524     C                   MOVEL     'S'           D98CCC
475200930420     C                   ELSE
475300930524     C                   MOVEL     'N'           D98CCC
475400930420     C                   END
475500930420     C*
475600930420     C*-------------------------------------------------------------------
475700930420     C* CONFORMITA' RESA INTERNA
475800930420     C*-------------------------------------------------------------------
475900930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
476000930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
476100930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
476200050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
476300050914     c                   if        consideraora='S'
476400050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
476500050914     c                   else
476600050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
476700050914     c                   endif
476800080116     c
476900080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
477000090827     c                   select
477100090827     c                   when      d98cca='2' and w98nri>0
477200080116     c                   z-add     0             w98nri
477300090827     c
477400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
477500080116     c*  e sto leggendo la bolla in arrivo
477600090827     c                   when      d98cca='1' and d98tbo='A' and
477700080116     c                                            w98nri>0
477800080116     c                   z-add     0             w98nri
477900110923     c
478000110923     c* Ignoro ritardo se Perfect Order
478100130221     c                   when      d98xco >*zeros
478200110923     c                   z-add     0             w98nri
478300110923     c
478400090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
478500090827     c* sono  in ritardo di 12 ore
478600091009     c                   when      d98tci=10  and
478700091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
478800091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
478900091014     c*
479000091014     c                             d98hci=1030
479100091014     C                   MOVEL     'OSC  '       WR3TAB
479200091014     C                   eval      WR3HHI=d98hmc
479300091014     C                   EXSR      RO3ORE
479400091014     c                   if        wr3matpom='M' and d98hmc>1030
479500131218     c                   exsr      ctrLAVH1030
479600131218
479700131218     c                   if        wlav1030<>'S'
479800091014     c                   add       12            w98nri
479900091014     c                   endif
480000131218     c                   endif
480100091014     c
480200091014     c***                z-add     12            w98nri
480300090827     c                   endsl
480400090827
480500930524     C                   ELSE                                                   <-- 1 -->
480600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
480700930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
480800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
480900931206     C     WPODCM        ANDLE     WPOCR1
481000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
481100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
481200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
481300931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
481400930525     C                   EXSR      RO2COV
481500930525     C     WR2NOV        IFNE      *ZEROS
481600930525     C     WR2NOV        MULT      -1            W98NRI
481700930525     C                   END
481800930524     C                   ELSE                                                   <-- 2 -->
481900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
482000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
482100930524     C     WPODCM        IFLT      WPOCRI
482200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
482300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
482400930524     C                   ELSE
482500931206     C                   Z-ADD     WPOCR1        WPORFI
482600930524     C                   Z-ADD     WPODCM        WPORFF
482700930524     C                   END
482800930524     C* CALCOLO ORE DI DIFFERENZA
482900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
483000930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
483100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
483200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
483300930524     C                   EXSR      RO2COV
483400930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
483500050902     c
483600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
483700100224     c                   if        d98cca='2' and w98nri>0
483800100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
483900080116     c***                z-sub     w98nri        w98nri
484000080116     c                   z-add     0             w98nri
484100050902     c                   endif
484200111018     c* Ignoro ritardo se pefect order
484300130221     c                   if        d98xco >*zeros
484400111018     c                   z-add     0             w98nri
484500111018     c                   endif
484600061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
484700061121     c*  e sto leggendo la bolla in arrivo
484800100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
484900100224     c***                if        d98cca='1' and d98tbo='A' and
485000100224     c***                                         w98nri>0 and wpodcm<wpocri
485100080116     c***                z-sub     w98nri        w98nri
485200080116     c                   z-add     0             w98nri
485300061121     c                   endif
485400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
485500070827     c*  mittente, --> consegnato in anticipo
485600070828     c                   if        d98tcr=' ' and d98nsp>0  and
485700070828     c                                            w98nri>0 and wpodcm<wpocri
485800070827     c                   if        CallAR5=' '
485900070827     c                   EXSR      LeggiAR5
486000070824     c                   endif
486100070827     c                   if        dcrpart<>'S'
486200070824     c                   z-sub     w98nri        w98nri
486300070824     c                   endif
486400070824     c                   endif
486500050907     c
486600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
486700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
486800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
486900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
487000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
487100050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
487200050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
487300050907     c                   z-sub     w98nri        w98nri
487400050907     c                   sub       oreILoDOPO    w98nri
487500050907     c                   endif
487600050907     c
487700050907     C                   END                                                    <-- 2 ---
487800930524     C                   END                                                    <-- 1 ---
487900930420     C* CALCOLO LE ORE DI RESA
488000930420     C* CONTROLLO CONFORMITA
488100930423     C     W98NRI        IFLE      *ZEROS
488200930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
488300930420     C                   ELSE
488400930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
488500930420     C                   END
488600930420     C*
488700930420     C*-------------------------------------------------------------------
488800930420     C* CONFORMITA' RESA CLIENTE
488900930420     C*-------------------------------------------------------------------
489000930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
489100930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
489200930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
489300050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
489400050914     c                   if        consideraora='S'
489500050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
489600050914     c                   else
489700050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
489800050914     c                   endif
489900090827     c
490000080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
490100090827     c                   select
490200090827     c                   when      d98cca='2' and w98nrc>0
490300080116     c                   z-add     0             w98nrc
490400090827
490500080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
490600080116     c*  e sto leggendo la bolla in arrivo
490700090827     c                   when      d98cca='1' and d98tbo='A' and
490800080116     c                                            w98nrc>0
490900080116     c                   z-add     0             w98nrc
491000110923     c
491100110923     c* Ignoro ritardo se Perfect Order
491200130221     c                   when      d98xco >*zeros
491300110923     c                   z-add     0             w98nrc
491400090827     c
491500090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
491600090827     c* sono  in ritardo di 12 ore
491700091009     c                   when      d98tcc=10  and
491800091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
491900091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
492000091014     c                             d98hce=1030
492100091014     C                   MOVEL     'OSC  '       WR3TAB
492200091014     C                   eval      WR3HHI=d98hmc
492300091014     C                   EXSR      RO3ORE
492400091014     c                   if        wr3matpom='M' and d98hmc>1030
492500131218     c                   exsr      ctrLAVH1030
492600131218
492700131218     c                   if        wlav1030<>'S'
492800091014     c                   add       12            w98nrc
492900091014     c                   endif
493000131218     c                   endif
493100091014     c
493200091014     c**                 z-add     12            w98nrc
493300090827     c                   endsl
493400090827     c
493500930524     C                   ELSE                                                   <-- 1 -->
493600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
493700930524     C* DI DIFFERENZA SONO UGUALI A ZERO
493800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
493900931206     C     WPODCM        ANDLE     WPOCR2
494000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
494100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
494200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
494300931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
494400930525     C                   EXSR      RO2COV
494500930525     C     WR2NOV        IFNE      *ZEROS
494600930525     C     WR2NOV        MULT      -1            W98NRC
494700930525     C                   END
494800930524     C                   ELSE                                                   <-- 2 -->
494900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
495000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
495100930524     C     WPODCM        IFLT      WPOCRI
495200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
495300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
495400930524     C                   ELSE
495500931206     C                   Z-ADD     WPOCR2        WPORFI
495600930524     C                   Z-ADD     WPODCM        WPORFF
495700930524     C                   END
495800930524     C* CALCOLO ORE DI DIFFERENZA
495900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
496000930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
496100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
496200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
496300930524     C                   EXSR      RO2COV
496400930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
496500050902     c
496600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
496700100224     c                   if        d98cca='2' and w98nrc>0
496800100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
496900080116     c***                z-sub     w98nrc        w98nrc
497000080116     c                   z-add     0             w98nrc
497100050902     c                   endif
497200111018     c* Ignoro ritardo se pefect order
497300130221     c                   if        d98xco >*zeros
497400111018     c                   z-add     0             w98nrc
497500111018     c                   endif
497600070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
497700061121     c*  e sto leggendo la bolla in arrivo
497800100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
497900100224     c***                if        d98cca='1' and d98tbo='A' and
498000100224     c***                                         w98nrc>0 and wpodcm<wpocri
498100080116     c***                z-sub     w98nrc        w98nrc
498200080116     c                   z-add     0             w98nrc
498300061121     c                   endif
498400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
498500070827     c*  mittente, --> consegnato in anticipo
498600070828     c                   if        d98tcr=' ' and d98nsp>0 and
498700070828     c                                            w98nrc>0 and wpodcm<wpocri
498800070827     c                   if        CallAR5=' '
498900070827     c                   EXSR      LeggiAR5
499000070827     c                   endif
499100070827     c                   if        dcrpart<>'S'
499200070824     c                   z-sub     w98nrc        w98nrc
499300070824     c                   endif
499400070824     c                   endif
499500050907     c
499600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
499700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
499800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
499900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
500000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
500100050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
500200050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
500300050907     c                   z-sub     w98nrc        w98nrc
500400050907     c                   sub       oreILoDOPO    w98nrc
500500050907     c                   endif
500600050907     c
500700050907     C                   END                                                    <-- 2 ---
500800930524     C                   END                                                    <-- 1 ---
500900930420     C* CALCOLO LE ORE DI RESA
501000930420     C* CONTROLLO CONFORMITA
501100930423     C     W98NRC        IFLE      *ZEROS
501200930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
501300930420     C                   ELSE
501400930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
501500930420     C                   END
501600930420     C*
501700930420     C                   ENDSR
501800070827     c
501900131218     c********************************************************************
502000131218     c     CtrlavH1030   BEGSR
502100131218     c********************************************************************
502200131218     c                   clear                   wlav1030          1
502300140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
502400131218     c                   z-add     1             xx                4 0
502500131218     c                   dow       evc(xx)<>*blanks
502600140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
502700140127     c                   if        evd(xx)=d98dcm
502800131218     c                   if        evo(xx)<=1030
502900131218     c                   eval      wlav1030='S'
503000131218     c                   leave
503100131218     c                   endif
503200131218     c                   endif
503300131218     c                   add       1             xx
503400131218     c                   enddo
503500131218     c                   ENDSR
503600070827     c********************************************************************
503700070827     c* Controllo record GEN di FIAR5
503800070827     c********************************************************************
503900070827     c     LeggiAR5      BEGSR
504000070827     c                   clear                   DCRPART           1
504100141104     c                   clear                   dar5gen
504200141104     c                   clear                   gc1PART           2
504300141104     c                   clear                   gc2PART           2
504400070827     c     kar5          chain     fiar501l
504500070827    1c                   if        not %found
504600070827     c                   eval      dcrpart='S'
504700141104     c                   eval      gc1part=d98gc1
504800141104     c                   eval      gc2part=d98gc2
504900070827   x1c                   else
505000070827     c* i dati devono essere quelli immessi dalla partenza
505100070827     c                   movel     ar5uni        dar5gen
505200070827     c
505300070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
505400070827     c*  ma il record GEN è stato scritto per altri motivi
505500070827    2c                   if        §ar5mdp='S'
505600070827     c                   movel     d98dcr        alfadcr           8
505700141104     c                   eval      gc1part=§ar5gc1
505800141104     c                   eval      gc2part=§ar5gc2
505900070827     c
506000070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
506100070827     c                   eval      dcrpart='S'
506200070827    3c                   endif
506300070827     c
506400070827   x2c                   else
506500070827     c                   eval      dcrpart='S'
506600141106     c                   eval      gc1part=d98gc1
506700141106     c                   eval      gc2part=d98gc2
506800070827    2c                   endif
506900070827    1c                   endif
507000070827     c
507100140402     c* Memorizzo il flag pickup 2
507200140402     c                   eval      wpick2=§AR5PK2
507300140402     c
507400150401     c*
507500150414     c**                 if        d98cas>0 or d98ffd<>' ' or
507600150414     c**                           d98tc1='A'  or d98tc2='A' or
507700150414     c**                           d98tc1='S'  or d98tc2='S'
507800160331     c                   eval      kar4trc='L'
507900150401     c     kar4          chain     fiar401l
508000160420    1c                   if        %found(fiar401l)
508100150401     c                   eval      dsbl4L=ar4not
508200160420   x1c                   else
508300160420    1c                   endif
508400160420
508500150401     c* Solo per fermo deposito verifico se inviato alert
508600150401     c                   if        d98ffd<>' '
508700150401     c                   clear                   fnlv82ds
508800150401     c                   eval      ILV82AAS=d98aas
508900150401     c                   eval      ILV82LNP=d98lnp
509000150401     c                   eval      ILV82NRS=d98nrs
509100150401     c                   eval      ILV82NSP=d98nsp
509200150401     c                   eval      ILV82TIPA='FD'
509300150401     c                   call      'FNLV82R'
509400150401     c                   parm                    fnlv82ds
509500150401     c                   endif
509600150414     c**                 endif
509700150401
509800150401     c                   eval      callar5='S'
509900160331     c
510000160331     c* Per import DPD verifico se bolla vedi pacco o EX vedio pacco
510100160331     c                   if        %lookup(d98lnp:fildpd)>0
510200160331     c                   eval      kar4trc='I'
510300160331     c     kar4          chain     fiar401l
510400160331     c                   if        %found(fiar401l)
510500160331     c                   eval      dsbl4I=ar4not
510600160331     c                   endif
510700160401     c                   if        §b4trd='1' or §b4trd='2'
510800160401     c                   eval      wVP='S'
510900160401     c                   endif
511000160401     c
511100160331     c                   endif
511200150401
511300070827     c                   ENDSR
511400930913     C********************************************************************
511500930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
511600930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
511700930913     C*            DATA/ORA (FINE RANGE)
511800930913     C********************************************************************
511900930913     C     CALCNS        BEGSR
512000930914     C*
512100930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
512200930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
512300930914     C     WPODTI        IFEQ      *ZEROS
512400960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
512500930914     C                   ELSE
512600930914     C                   Z-ADD     WPODTI        WPOIRC
512700930914     C                   END
512800930914     C*
512900930914     C* SE NON ANCORA CONSEGNATA
513000930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
513100050922     c* in realtà nella data consegna c'e' la data fine calcolo
513200050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
513300930914     C     WPODCM        IFEQ      *ZEROS
513400050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
513500930914     C                   ELSE
513600930916     C                   Z-ADD     WPODCM        WPOFRC
513700930914     C                   END
513800930914     C*
513900930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
514000930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
514100930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
514200930914     C     WPODFC        ANDLT     WPOIRC
514300930914     C                   ELSE
514400930914     C*
514500930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
514600930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
514700930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
514800930914     C     WPODFC        ANDGT     WPOFRC
514900930914     C                   ELSE
515000930916     C*
515100930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
515200930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
515300930916     C     WPODFC        ANDGT     WPOFRC
515400930916     C                   ELSE
515500930916     C*
515600930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
515700930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
515800930916     C     WPODFC        ANDLT     WPOIRC
515900930916     C                   ELSE
516000930914     C*
516100930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
516200930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
516300930914     C     WPODIC        IFNE      *ZEROS
516400930914     C     WPODIC        ANDGT     WPOFRC
516500930914     C                   Z-ADD     WPOFRC        WPODIC
516600930914     C                   ENDIF
516700930914     C*
516800930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
516900930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
517000930914     C     WPODFC        IFGT      WPOFRC
517100930914     C                   Z-ADD     WPOFRC        WPODFC
517200930914     C                   ENDIF
517300930914     C*
517400930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
517500930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
517600930914     C     WPODIC        IFNE      *ZEROS
517700930914     C     WPODIC        ANDLT     WPOIRC
517800930914     C                   Z-ADD     WPOIRC        WPODIC
517900930914     C                   ENDIF
518000930914     C*
518100930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
518200930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
518300930914     C     WPODFC        IFLT      WPOIRC
518400930914     C                   Z-ADD     WPOIRC        WPODFC
518500930914     C                   ENDIF
518600930914     C*
518700930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
518800931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
518900930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
519000930916     C*
519100931210     C***********WPODIC    IFNE *ZEROS
519200931210     C***********WPODFC    ANDNE*ZEROS
519300931210     C*********************ADD  12        WPODFC
519400931210     C*********************ENDIF
519500930914     C     WPODIC        IFEQ      *ZEROS
519600930914     C     WPODFC        ANDNE     *ZEROS
519700931130     C                   Z-ADD     WPODFC        WPODIC
519800931210     C*********************ADD  12        WPODFC
519900930916     C                   ENDIF
520000930913     C*
520100930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
520200930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
520300930914     C* INTERNA
520400050908     c                   eval      Usacons='S'
520500930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
520600930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
520700931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
520800931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
520900940324     C                   EXSR      RO9COV
521000931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
521100930914     C* CLIENTE
521200930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
521300931210     C                   EXSR      RO9COV
521400931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
521500050908     c                   eval      Usacons=' '
521600930914     C*
521700930916     C                   ENDIF                                                  <-- 5 ---
521800930916     C                   ENDIF                                                  <-- 4 ---
521900930916     C                   ENDIF                                                  <-- 3 ---
522000930914     C                   ENDIF                                                  <-- 2 ---
522100930914     C                   ENDIF                                                  <-- 1 ---
522200930913     C*
522300930913     C                   ENDSR
522400930325     C********************************************************************
522500930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
522600930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
522700930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
522800930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
522900930325     C********************************************************************
523000930325     C     RO1CDP        BEGSR
523100930414     C*
523200931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
523300931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
523400931126     C*
523500931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
523600931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
523700150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
523800150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
523900931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
524000060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
524100930326     C*
524200930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
524300931130     C                   Z-ADD     WR1POI        WR1POO            9 0
524400930325     C*
524500930325     C* CICLO FINO A CHE:
524600930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
524700930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
524800930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
524900930406     C     WR1POO        ANDLT     WPOLIM
525000930414     C*
525100930407     C                   Z-ADD     WR1POO        WPOROU            9 0
525200930325     C                   MOVEL     'N'           WSLITT            1
525300930414     C*
525400930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
525500060420     C*
525600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
525700150605     c     wdee          andeq     ' '
525800930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
525900930414     C                   END
526000930511     C*
526100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
526200150608     c     wdee          andeq     ' '
526300930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
526400930511     C                   END
526500930414     C*
526600930325     C     WSLITT        IFEQ      'N'
526700930423     C                   SUB       12            WR1NOI            5 0
526800930325     C                   END
526900931130     C                   ADD       12            WR1POO            9 0
527000930414     C*
527100931223     C                   ENDDO                                                  <-- 1 ---
527200931126     C*
527300931126     C                   ELSE                                                   <-- 0 -->
527400931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
527500931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
527600931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
527700960129     C     WPOLAF        IFGT      WPODCG
527800960129     C                   Z-ADD     WPOLAF        WTM090            9 0
527900960129     C                   ELSE
528000960129     C                   Z-ADD     WPODCG        WTM090
528100960129     C                   ENDIF
528200960129     C     WPOGIF        IFGT      WTM090
528300960129     C                   Z-ADD     WPOGIF        WTM090
528400960129     C                   ENDIF
528500060420     C     WPOpaif       IFGT      WTM090
528600060420     C                   Z-ADD     WPOpaif       WTM090
528700060420     C                   ENDIF
528800960129     C*
528900960129     C     12            ADD       WTM090        WR1POO
529000960129     C                   ENDIF                                                  <-- 0 ---
529100930512     C*
529200931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
529300931202     C     WR1POO        SUB       12            WR7POI
529400931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
529500931202     C                   ADD       12            WR7POO
529600931202     C     WR7POO        IFGT      WR1POO
529700931202     C                   Z-ADD     WR7POO        WR1POO
529800931202     C                   END
529900930406     C*
530000930325     C                   ENDSR
530100930414     C********************************************************************
530200930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
530300930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
530400930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
530500930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
530600930414     C********************************************************************
530700930414     C     RO2COV        BEGSR
530800091014     c                   clear                   salvaEveL
530900091014     c                   clear                   salvaEveG
531000931130     C*
531100930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
531200930414     C                   Z-ADD     *ZEROS        WR2NOV
531300930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
531400931130     C                   Z-ADD     WR2POI        WR2POW
531500930414     C*
531600930414     C* CICLO FINO A CHE:
531700930414     C*   - DATA INIZIO SUPERATO DATA FINE
531800930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
531900931223     C     WR2POW        ANDLT     WPOLIM
532000930414     C*
532100930414     C                   Z-ADD     WR2POW        WPOROU
532200930414     C                   MOVEL     'N'           WSLITT            1
532300930414     C*
532400930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
532500930414     C*
532600930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
532700930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
532800930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
532900930414     C                   END
533000930414     C*
533100930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
533200930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
533300930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
533400930618     C                   END
533500060420     c
533600060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
533700060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
533800060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
533900060420     C                   END
534000060420     c
534100931201     C*
534200931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
534300931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
534400931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
534500931201     C                   END
534600060418     c
534700060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
534800060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
534900060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
535000060418     C                   END
535100930414     C*
535200930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
535300930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
535400930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
535500930414     C                   END
535600930511     C*
535700930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
535800930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
535900930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
536000930511     C                   END
536100930414     C*
536200930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
536300930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
536400930414     C     WSLITT        IFEQ      'N'
536500930423     C                   ADD       12            WR2NOV            5 0
536600930414     C                   END
536700931130     C                   ADD       12            WR2POW            9 0
536800930414     C*
536900931223     C                   ENDDO                                                  <-- 1 ---
537000930414     C*
537100930414     C                   ENDSR
537200941228     C********************************************************************
537300941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
537400941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
537500941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
537600941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
537700941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
537800941228     C********************************************************************
537900941228     C     RO2COP        BEGSR
538000941228     C*
538100941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
538200941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
538300941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
538400941228     C                   Z-ADD     *ZEROS        WR2NOV
538500941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
538600941228     C                   Z-ADD     WR2POI        WR2POW
538700941228     C*
538800941228     C* CICLO FINO A CHE:
538900941228     C*   - DATA INIZIO SUPERATO DATA FINE
539000941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
539100941228     C*
539200950215     C                   Z-ADD     WR2POW        WPOROU
539300941228     C                   MOVEL     'N'           WSLITT
539400941228     C* TRASFORMA IL PROGR. ORE IN DATA
539500950215     C*******              Z-ADDWR2POW    WORE
539600950215     C*******              EXSR OREDAT
539700950215     C*******              Z-ADDWDAT      W12DAT  80
539800950215     C*******              Z-ADDWRES      W12RES  30
539900941228     C*
540000941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
540100941228     C*
540200941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
540300941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
540400941228     C     WSLITT        IFEQ      'N'
540500941228     C                   ADD       12            WR2NOV            5 0
540600941228     C                   END
540700941228     C                   ADD       12            WR2POW            9 0
540800941228     C*
540900941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
541000941228     C* ENTRI IN LOOP
541100941228     C     WR2NOV        IFGT      999
541200941228     C                   LEAVE                                                  *VA A ENDDO
541300941228     C                   END
541400941228     C*
541500941228     C                   ENDDO                                                  <-- 1 ---
541600941228     C*
541700941228     C                   ENDSR
541800930510     C********************************************************************
541900930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
542000930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
542100930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
542200930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
542300050905     c*                     il num ore deve essere un multiplo di 12
542400050905     c*                     oppure 1 ora
542500930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
542600930510     C********************************************************************
542700930510     C     RO4CDP        BEGSR
542800930510     C*
542900930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
543000930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
543100050905     c                   movel     '1'           wgiro             1
543200930510     C*
543300930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
543400930510     C                   Z-ADD     WR4POI        WR4POO            9 0
543500930510     C*
543600930510     C* CICLO FINO A CHE:
543700930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
543800930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
543900930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
544000930510     C     WR4POO        ANDLT     WPOLIM
544100930510     C*
544200141106     c                   if        nosomma<>'N'
544300931130     C                   ADD       12            WR4POO            9 0
544400141106     c                   endif
544500930510     C                   Z-ADD     WR4POO        WPOROU            9 0
544600930510     C                   MOVEL     'N'           WSLITT            1
544700930510     C*
544800930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
544900050905     c
545000050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
545100930510     C     WSLITT        IFEQ      'N'
545200050905     c
545300050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
545400050905     c*  se cado in giorno non festivo
545500050905     c                   if        wr4noi=1 and wgiro='1'
545600050905     c                   sub       12            wr4poo
545700050905     c                   add       wr4noi        wr4poo
545800050905     c                   endif
545900050905     c*
546000050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
546100050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
546200050905     c*  calcolate dal pgm
546300050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
546400050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
546500050905     c*  del giorno sucessivo non festivo
546600050905     c                   if        wr4noi=1 and wgiro<>'1'
546700050905     c                   add       wr4noi        wr4poo
546800050905     c                   endif
546900050905     c
547000930510     C                   SUB       12            WR4NOI            5 0
547100930510     C                   END
547200141106     c
547300141106     c                   if        nosomma= 'N'
547400141106     C                   ADD       12            WR4POO            9 0
547500141106     c                   endif
547600930510     C*
547700050905     c                   eval      wgiro='2'
547800931223     C                   ENDDO                                                  <-- 1 ---
547900930510     C*
548000930510     C                   ENDSR
548100931130     C********************************************************************
548200931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
548300931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
548400931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
548500931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
548600931130     C********************************************************************
548700931130     C     RO5COV        BEGSR
548800931130     C*
548900931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
549000060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
549100060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
549200060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
549300931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
549400931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
549500931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
549600931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
549700931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
549800931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
549900931130     C                   Z-ADD     *ZEROS        WR5NOV
550000940714     C*
550100940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
550200940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
550300931207     C*
550400931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
550500940908     C     WIC           IFEQ      'I'
550600090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
550700090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
550800931207     C                   ELSE
550900090827     C*****              Z-ADD     D98TCC        WPOTCT
551000090827     C                   Z-ADD     w99TCC        WPOTCT
551100931207     C                   END
551200931130     C*
551300931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
551400060420     C                   clear                   COV
551500931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
551600931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
551700931207     C                   Z-ADD     2             CO                3 0
551800931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
551900931207     C                   EXSR      RO5SUB
552000981102     C                   DO        99            IC                3 0
552100931207     C     LAF(IC)       IFNE      *ZEROS
552200931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
552300931207     C                   EXSR      RO5SUB
552400931207     C                   ENDIF
552500931207     C                   ENDDO
552600060420     c
552700060420     C                   DO        99            IC                3 0
552800060420     C     paif(IC)      IFNE      *ZEROS
552900060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
553000060420     C                   EXSR      RO5SUB
553100060420     C                   ENDIF
553200060420     C                   ENDDO
553300931207     C* ORDINA LA SCHIERA COV
553400931207     C                   SORTA     COV
553500931130     C*
553600931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
553700931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
553800981102     C                   DO        98            IC                             --- 1 -->
553900931207     C     IC            ADD       1             I1C               3 0
554000931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
554100931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
554200931207     C     COV(I1C)      OREQ      *ZEROS
554300931207     C                   ELSE
554400931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
554500931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
554600931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
554700931130     C                   EXSR      RO2COV
554800931207     C                   ADD       WR2NOV        WR5NOV            9 0
554900931209     C* SE NON SONO IN DATA CONSEGNA
555000931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
555100931207     C     WR5NOV        IFLT      WPOTCT
555200931209     C     COV(I1C)      ANDNE     WPODCM
5553009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5554009709111009 C     WPOTCT        SUB       12            WR5NOV
555500931130     C                   END
555600931207     C                   ENDIF                                                  <-- 2 ---
555700931207     C                   ENDDO                                                  <-- 1 ---
555800940714     C                   ENDIF                                                  <-- 0 ---
555900931130     C*
556000931130     C                   ENDSR
556100931207     C********************************************************************
556200931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
556300931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
556400931207     C********************************************************************
556500931207     C     RO5SUB        BEGSR
556600931207     C*
556700931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
556800931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
556900931207     C     WR5COV        ANDLT     WR5POF
557000931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
557100931207     C                   MOVEL     'N'           WR5PRE            1
557200981102     C*---                 DO   30        I
557300981102     C                   DO        99            I
557400931207     C     WR5COV        IFEQ      COV(I)
557500931207     C                   MOVEL     'S'           WR5PRE
557600931207     C                   LEAVE
557700931207     C                   ENDIF
557800931207     C                   ENDDO
557900931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
558000931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
558100931207     C                   ADD       1             CO                3 0
558200931207     C                   Z-ADD     WR5COV        COV(CO)
558300931207     C                   ENDIF                                                  <-- 2 ---
558400931207     C                   ENDIF                                                  <-- 1 ---
558500931207     C*
558600931207     C                   ENDSR
558700931201     C********************************************************************
558800060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
558900931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
559000931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
559100931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
559200931201     C********************************************************************
559300931201     C     RO6CDT        BEGSR
559400931201     C*
559500931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
559600931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
559700931201     C*
559800931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
559900931201     C                   Z-ADD     WR6POI        WR6POO            9 0
560000931201     C*
560100931201     C* CICLO FINO A CHE:
560200931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
560300931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
560400931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
560500931201     C     WR6POO        ANDLT     WPOLIM
560600931201     C*
560700931201     C                   Z-ADD     WR6POO        WPOROU            9 0
560800931201     C                   MOVEL     'N'           WSLITT            1
560900931201     C*
561000931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
561100931201     C*
561200931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
561300150605     c     wdee          andeq     ' '
561400931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
561500931201     C                   END
561600931201     C*
561700931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
561800150608     c     wdee          andeq     ' '
561900931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
562000931201     C                   END
562100931201     C*
562200931201     C     WSLITT        IFEQ      'N'
562300931201     C                   SUB       12            WR6NOI            5 0
562400931201     C                   END
562500931201     C                   ADD       12            WR6POO            9 0
562600931201     C*
562700931223     C                   ENDDO                                                  <-- 1 ---
562800931201     C*
562900931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
563000931202     C     WR6POO        SUB       12            WR7POI
563100931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
563200931207     C                   ADD       12            WR7POO
563300931202     C     WR7POO        IFGT      WR6POO
563400931201     C                   Z-ADD     WR7POO        WR6POO
563500931202     C                   END
563600931201     C*
563700931201     C                   ENDSR
563800931201     C********************************************************************
563900060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
564000060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
564100060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
564200060407     C*            INPUT:  -WR7POI (PROGR.ORE)
564300931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
564400931201     C********************************************************************
564500931201     C     RO7TCF        BEGSR
564600060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
564700060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
564800060407     C                   MOVEL     'N'           WSLITT
564900931201     C*
565000931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
565100931201     C                   Z-ADD     WR7POI        WR7POO            9 0
565200931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
565300931201     C* COINCIDA CON UN TURNO DI CHIUSURA
565400060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
565500931201     C     D98GC2        ANDEQ     *BLANKS
565600060407   x1C                   ELSE                                                   <-- 1 -->
565700931214     C*
565800931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
565900931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
566000931214     C*
566100931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
566200931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
566300931223     C     WPOROU        ANDLT     WPOLIM
566400931214     C* MEMORIZZO INIZIO E FINE RANGE
566500931202     C     WPOTI         IFEQ      *ZEROS
566600931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
566700931202     C                   END
566800931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
566900931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
567000931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
567100931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
567200931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
567300931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
567400931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
567500931201     C                   MOVEL     'N'           WSLITT
567600931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
567700060407     c* Se non trovat, controllo se c'e' evento part che giustifica
567800060407     c                   if        wslitt='N'
567900060407     c                   exsr      datcepg
568000060407     c                   endif
568100060407     c
568200931201     C                   ENDDO                                                  <-- 2 ---
568300931214     C*
568400931201     C                   Z-ADD     WPOROU        WR7POO
568500931214     C*
568600931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
568700931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
568800931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
568900931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
569000931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
569100931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
569200931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
569300931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
569400931209     C                   ELSE                                                   <-- 3 -->
569500931209     C     WIC           IFEQ      'I'
569600931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
569700931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
569800931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
569900931201     C                   ELSE
570000931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
570100931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
570200931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
570300931201     C                   ENDIF
570400931209     C                   ENDIF                                                  <-- 3 ---
570500931209     C                   ENDIF                                                  <-- 2 ---
570600931214     C*
570700060407    1C                   ENDIF                                                  <-- 1 ---
570800060407     c
570900060407     c* Se non trovato turno di chiusura, verifico se c'e'
571000060407     c*  evento particolare che GIUSTIFICA
571100060407     c                   if        wpoti=0
571200060418     C                   EXSR      RO7CEPG
571300060407     c                   endif
571400931201     C*
571500931201     C                   ENDSR
571600060406     C********************************************************************
571700060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
571800060406     c*                        che giustifica
571900060407     C*            INPUT:-WR7POI (PROGR.ORE)
572000060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
572100060406     C********************************************************************
572200060407     C     RO7CEPG       BEGSR
572300060406     C*
572400060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
572500060407     C                   Z-ADD     WR7POI        WR7POO            9 0
572600060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
572700060406     C* coincide
572800060406    1C                   if        wevpag='S'                                   <-- 1 -->
572900060406     C*
573000060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
573100060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
573200060406     C                   MOVEL     'N'           WSLITT
573300060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
573400060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
573500060406     C*
573600060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
573700060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
573800060406     C     WPOROU        ANDLT     WPOLIM
573900060406     C* MEMORIZZO INIZIO E FINE RANGE
574000060406    3C     WPOTI         IFEQ      *ZEROS
574100060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
574200060406    3C                   END
574300060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
574400060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
574500060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
574600060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
574700060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
574800060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
574900060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
575000060406     C                   MOVEL     'N'           WSLITT
575100060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
575200060406     C* Se non trovato,
575300060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
575400060406    3c                   if        wslitt='N'
575500060406     c                   exsr      DATTUC
575600060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
575700060406    2C                   ENDDO                                                  <-- 2 ---
575800060406     C*
575900060407     C                   Z-ADD     WPOROU        WR7POO
576000060406     C*
576100060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
576200060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
576300060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
576400060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
576500060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
576600060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
576700060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
576800060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
576900060406   x3C                   ELSE                                                   <-- 3 -->
577000060406    4C     WIC           IFEQ      'I'
577100060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
577200060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
577300060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
577400060406   x4C                   ELSE
577500060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
577600060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
577700060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
577800060406    4C                   ENDIF
577900060406    3C                   ENDIF                                                  <-- 3 ---
578000060406    2C                   ENDIF                                                  <-- 2 ---
578100060406     C*
578200060406    1C                   ENDIF                                                  <-- 1 ---
578300060406     C*
578400060406     C                   ENDSR
578500931207     C********************************************************************
578600931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
578700931207     C*            INPUT:  -WR8POI (PROGR.ORE)
578800931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
578900931207     C********************************************************************
579000931207     C     RO8FES        BEGSR
579100130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
579200130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
579300931207     C*
579400931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
579500130411     C     WR8POI        SUB       12            WR8POO            9 0
579600931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
579700931207     C                   MOVEL     'N'           WSLITT
579800130411     c
579900130411     c* Non salto i festivi ???
580000130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
580100130411     c
580200931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
580300130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
580400130411     C     WPOROU        ANDGT     WPODSP
580500130411     C                   SUB       12            WPOROU
580600130411     C                   MOVEL     'N'           WSLITT
580700130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
580800130411     C                   ENDDO                                                  <-- 2 ---
580900130411     c
581000130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
581100130411     C     D98GC2        ANDEQ     *BLANKS
581200130411   x1C                   ELSE                                                   <-- 1 -->
581300130411     c* eseguo controllo per i turni di chiusura
581400130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
581500130411     C*
581600130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
581700130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
581800130411     C     WPOROU        ANDgT     WPOdsp
581900130411     C* MEMORIZZO INIZIO E FINE RANGE
582000130411     C     WPOTf         IFEQ      *ZEROS
582100130411     C                   z-add     wporou        WPOTf
582200130411     C                   END
582300140826     C     WPOROU        SUB       wnoadd        WPOROU
582400130411     c
582500130411     C                   MOVEL     'N'           WSLITT
582600130411     c                   exsr      dattuc
582700130411     c                   if        wslitt='N'
582800130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
582900140826     c* Se slitta per festivo --> tolgo sempre 12 ore
583000140826     c                   z-add     12            WNOADD
583100130411     c                   endif
583200130411     C* SLITTA
583300140826     C     wporou        add       12            WPOTi
583400130411    2C                   ENDDO                                                  <-- 2 ---
583500130411     c
583600130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
583700130411     c
583800130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
583900130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
584000130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
584100130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
584200130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
584300130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
584400130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
584500130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
584600130411   x3C                   ELSE                                                   <-- 3 -->
584700130411    4C     WIC           IFEQ      'I'
584800130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
584900130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
585000130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
585100130411   x4C                   ELSE
585200130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
585300130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
585400130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
585500130411    4C                   ENDIF
585600130411    3C                   ENDIF                                                  <-- 3 ---
585700130411    2C                   ENDIF                                                  <-- 3 ---
585800130411     C*
585900130411    1C                   ENDIF                                                  <-- 1 ---
586000130411
586100130411     C**   WPOROU        ADD       12            WR8POO
586200130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
586300130411     c                   z-add     wr8poi        wr8poo
586400931207     C*
586500931207     C                   ENDSR
586600931210     C********************************************************************
586700931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
586800940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
586900931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
587000931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
587100931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
587200931210     C********************************************************************
587300931210     C     RO9COV        BEGSR
587400931210     C*
587500931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
587600931210     C                   Z-ADD     *ZEROS        WR9NOV
587700931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
587800931210     C                   Z-ADD     WR9POI        WR9POW
587900931210     C* CICLO FINO A CHE:
588000931210     C*   - DATA INIZIO SUPERATO DATA FINE
588100931223     C*   - RAGGIUNTO LIMITE MASSIMO
588200931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
588300931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
588400931223     C     WR9POW        ANDLT     WPOLIM
588500931210     C*
588600931210     C                   Z-ADD     WR9POW        WPOROU
588700931210     C                   MOVEL     'N'           WSLITT            1
588800940324     C* FESTIVI
588900931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
589000940324     C* GIACENZA
589100931214     C     WSLITT        IFEQ      'N'
589200941229£$   C     WPOAGC        IFNE      *ZEROS
589300940324£$   C     WPOCGC        ORNE      *ZEROS
589400941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
589500940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
589600940324£$   C                   MOVEL     'S'           WSLITT
589700940324£$   C                   END
589800940324£$   C                   END
589900940324£$   C                   END
590000960129     C* EVENTI GIACENZA
590100960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
590200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
590300960129     C                   Z-ADD     *ZEROS        G                 3 0
590400960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
590500960129     C     G             ANDLT     IG
590600960129     C                   ADD       1             G
590700960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
590800960209     C     WPOROU        ANDLE     GCF(G)
590900960129     C                   MOVEL     'S'           WSLITT
591000960129     C                   ENDIF                                                  <-- 3 ---
591100960129     C                   ENDDO                                                  <-- 2 ---
591200960129     C                   ENDIF                                                  <-- 1 ---
591300960129     C                   ENDIF                                                  <-- 0 ---
591400940324     C* LASCIATO AVVISO
591500931214     C     WSLITT        IFEQ      'N'
591600941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
591700940324£$   C                   Z-ADD     *ZEROS        L                 3 0
591800940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
591900940324£$   C     L             ANDLT     IL
592000940324£$   C                   ADD       1             L
592100941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
592200941229£$   C     WPOROU        ANDLE     LCF(L)
592300050520     c     LCon(L)       andeq     'S'
592400940324£$   C                   MOVEL     'S'           WSLITT
592500940324£$   C                   END                                                    <-- 3 ---
592600940324£$   C                   ENDDO                                                  <-- 2 ---
592700940324£$   C                   END                                                    <-- 1 ---
592800931210     C                   END
592900940324     C* TURNO CHIUSURA
593000140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
593100140826     c*  di sistemare le ore da sommare 12 o 24...
593200931214     C     WSLITT        IFEQ      'N'
593300931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
593400931210     C                   END
593500940324     C* DATA CONSEGNA RICHIESTA
593600050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
593700050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
593800050907     c
593900931214     C     WSLITT        IFEQ      'N'
594000050907     c                   add       1             wporou
594100940324£$   C     WPOCRI        IFNE      *ZEROS
594200940324£$   C     WPOROU        IFLT      WPOCRC
594300940324£$   C                   MOVEL     'S'           WSLITT
594400940324£$   C                   END
594500940324£4   C                   END
594600050907     c                   sub       1             wporou
594700931210     C                   END
594800931210     C*
594900931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
595000931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
595100931210     C     WSLITT        IFEQ      'N'
595200931210     C                   ADD       12            WR9NOV            5 0
595300931210     C                   END
595400931210     C                   ADD       12            WR9POW            9 0
595500931210     C*
595600931223     C                   ENDDO                                                  <-- 1 ---
595700931210     C*
595800931210     C                   ENDSR
595900940628%%%  C********************************************************************
596000940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
596100940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
596200940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
596300940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
596400940628%%%  C********************************************************************
596500940628%%%  C     RO0GES        BEGSR
596600940628%%%  C*
596700940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
596800940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
596900940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
597000940628%%%  C*
597100940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
597200940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
597300940628%%%  C*
597400940628%%%  C* CICLO FINO A CHE:
597500940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
597600940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
597700940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
597800050922%%%  C     WR0POO        ANDGe     WPODSP
597900940628%%%  C*
598000940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
598100940628%%%  C                   MOVEL     'N'           WSLITT            1
598200940628%%%  C*
598300940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
598400940628%%%  C*
598500940628%%%  C     WSLITT        IFEQ      'N'
598600940628%%%  C                   SUB       12            WR0NOI            5 0
598700940628%%%  C                   END
598800940628%%%  C                   SUB       12            WR0POO            9 0
598900940628%%%  C*
599000940628%%%  C                   ENDDO                                                  <-- 1 ---
599100940628%%%  C*
599200940628%%%  C                   ADD       12            WR0POO            9 0
599300940628%%%  C*
599400940628%%%  C                   ENDSR
599500960911     C********************************************************************
599600960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
599700960911     C*            INPUT:  -W10POI (PROGR.ORE)
599800960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
599900960911     C********************************************************************
600000960911     C     R10FES        BEGSR
600100960911     C*
600200960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
600300960911     C                   Z-ADD     W10POI        W10POO            9 0
600400960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
600500960911     C                   MOVEL     'N'           WSLITT
600600960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
600700960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
600800960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
600900960911     C                   ADD       12            WPOROU
601000960911     C                   MOVEL     'N'           WSLITT
601100960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
601200960911     C                   ENDDO                                                  <-- 2 ---
601300960911     C                   Z-ADD     WPOROU        W10POO
601400960911     C*
601500960911     C                   ENDSR
601600930325     C********************************************************************
601700930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
601800930325     C********************************************************************
601900930325     C     CHKGIA        BEGSR
602000930406     C*
602100960129     C* APERTURA/CHIUSURA GIACENZA
602200930326     C     WPODAG        IFNE      *ZEROS
602300930406     C     WPODCG        ORNE      *ZEROS
602400930407     C     WPOROU        IFGE      WPODAG
602500930407     C     WPOROU        ANDLE     WPODCG
602600930325     C                   MOVEL     'S'           WSLITT
602700930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
602800930325     C                   END
602900930326     C                   END
603000960129     C*
603100960129     C* EVENTI GIACENZA
603200960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
603300960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
603400960129     C                   Z-ADD     *ZEROS        G                 3 0
603500960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
603600960129     C     G             ANDLT     IG
603700960129     C                   ADD       1             G
603800960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
603900960129     C     WPOROU        ANDLE     GIF(G)
604000091009     c
604100091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
604200091009     c*  l'orario dell'evento
604300091009     c                   select
604400091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
604500091009     c* Se orario entro le 10.30  va bene
604600091014     c*  solo la prima volta per evento cioè quando controllo
604700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
604800091014     c*  e non devo dare errore
604900091014     C                   MOVEL     'OSC  '       WR3TAB
605000091014     C                   eval      WR3HHI=evo(gip(G))
605100091014     C                   EXSR      RO3ORE
605200091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
605300091014     c*  in ritardo come per "E")
605400091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
605500091014     c                              wr3matpom='P'
605600091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
605700091009     C                   MOVEL     'S'           WSLITT
605800091009     c                   endif
605900091014     c                   eval      SalvaEveG=gip(G)
606000091009     c
606100091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
606200091009     c* Se orario entro le 10.30  va bene
606300091014     c*  solo la prima volta per evento cioè quando controllo
606400091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
606500091014     c*  e non devo dare errore
606600091014     C                   MOVEL     'OSC  '       WR3TAB
606700091014     C                   eval      WR3HHI=evo(gip(G))
606800091014     C                   EXSR      RO3ORE
606900091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
607000091014     c*  in ritardo come per "E")
607100091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
607200091014     c                              wr3matpom='P'
607300091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
607400091009     C                   MOVEL     'S'           WSLITT
607500091009     c                   endif
607600091014     c                   eval      SalvaEveG=gip(G)
607700091009     c
607800091009     c                   other
607900960129     C                   MOVEL     'S'           WSLITT
608000960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
608100091009     c                   endsl
608200091009
608300960129     C                   ENDIF                                                  <-- 3 ---
608400960129     C                   ENDDO                                                  <-- 2 ---
608500960129     C                   ENDIF                                                  <-- 1 ---
608600960129     C                   ENDIF                                                  <-- 0 ---
608700930406     C*
608800930325     C                   ENDSR
608900060420     C********************************************************************
609000930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
609100930325     C********************************************************************
609200930325     C     CHKLAV        BEGSR
609300930325     C*
609400941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
609500930421     C                   Z-ADD     *ZEROS        L                 3 0
609600930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
609700930421     C     L             ANDLT     IL
609800930421     C                   ADD       1             L
609900930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
610000930421     C     WPOROU        ANDLE     LAF(L)
610100090928     c
610200090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
610300091009     c*  l'orario dell'evento
610400091009     c                   select
610500091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
610600091009     c* Se orario entro le 10.30  va bene
610700091014     c*  solo la prima volta per evento cioè quando controllo
610800091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
610900091014     c*  e non devo dare errore
611000091014     C                   MOVEL     'OSC  '       WR3TAB
611100091014     C                   eval      WR3HHI=evo(lap(L))
611200091014     C                   EXSR      RO3ORE
611300091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
611400091014     c*  in ritardo come per "E")
611500091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
611600091014     c                              wr3matpom='P'
611700091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
611800091009     C                   MOVEL     'S'           WSLITT
611900091009     c                   endif
612000091014     c                   eval      SalvaEveL=lap(L)
612100091009     c
612200091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
612300091009     c* Se orario entro le 10.30  va bene
612400091014     c*  solo la prima volta per evento cioè quando controllo
612500091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
612600091014     c*  e non devo dare errore
612700091014     C                   MOVEL     'OSC  '       WR3TAB
612800091014     C                   eval      WR3HHI=evo(lap(L))
612900091014     C                   EXSR      RO3ORE
613000091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
613100091014     c*  in ritardo come per "E")
613200091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
613300091014     c                              wr3matpom='P'
613400091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
613500091009     C                   MOVEL     'S'           WSLITT
613600091009     c                   endif
613700091014     c                   eval      SalvaEveL=lap(L)
613800090928     c
613900091009     c                   other
614000930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
614100091009     C                   MOVEL     'S'           WSLITT
614200091009     c                   endsl
614300090928     c
614400930421     C                   END                                                    <-- 3 ---
614500931223     C                   ENDDO                                                  <-- 2 ---
614600930421     C                   END                                                    <-- 1 ---
614700930325     C*
614800930325     C                   ENDSR
614900060420     C********************************************************************
615000060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
615100060420     c*                   che Allungano
615200060420     C********************************************************************
615300060420     C     CHKCEPA       BEGSR
615400060420     C*
615500060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
615600060420     C                   Z-ADD     *ZEROS        p                 3 0
615700060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
615800060420     C     p             ANDLT     IpA
615900060420     C                   ADD       1             P
616000060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
616100060420     C     WPOROU        ANDLE     paif(P)
616200060420     C                   MOVEL     'S'           WSLITT
616300060420     C                   ADD       12            W98EA
616400060420     C                   END                                                    <-- 3 ---
616500060420     C                   ENDDO                                                  <-- 2 ---
616600060420     C                   END                                                    <-- 1 ---
616700060420     C*
616800060420     C                   ENDSR
616900930414     C********************************************************************
617000930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
617100930414     C********************************************************************
617200930414     C     CHKCNT        BEGSR
617300930511     C*
617400930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
617500930611     C     WIC           IFEQ      'I'                                          *INTERNO
617600930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
617700930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
617800930611     C                   ELSE
617900930611     C     WPOROU        IFGE      WPOAII
618000930611     C     WPOROU        ANDLE     WPOAFI
618100930611     C                   MOVEL     'S'           WSLITT
618200930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
618300930611     C                   END
618400930611     C                   END
618500930611     C                   ELSE                                                   *CLIENTE
618600930611     C     WPOAIC        IFEQ      *ZEROS
618700930611     C     WPOAFC        ANDEQ     *ZEROS
618800930611     C                   ELSE
618900930611     C     WPOROU        IFGE      WPOAIC
619000930611     C     WPOROU        ANDLE     WPOAFC
619100930611     C                   MOVEL     'S'           WSLITT
619200930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
619300930611     C                   END
619400930611     C                   END
619500930611     C                   END
619600930414     C*
619700930414     C                   ENDSR
619800930511     C********************************************************************
619900930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
620000930511     C********************************************************************
620100930511     C     CHKFED        BEGSR
620200930511     C*
620300930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
620400930611     C     WIC           IFEQ      'I'                                          *INTERNO
620500930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
620600930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
620700930611     C                   ELSE
620800930611     C     WPOROU        IFGE      WPOFII
620900930611     C     WPOROU        ANDLE     WPOFFI
621000930611     C                   MOVEL     'S'           WSLITT
621100930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
621200930611     C                   END
621300930611     C                   END
621400930611     C                   ELSE                                                   *CLIENTE
621500930611     C     WPOFIC        IFEQ      *ZEROS
621600930611     C     WPOFFC        ANDEQ     *ZEROS
621700930611     C                   ELSE
621800930611     C     WPOROU        IFGE      WPOFIC
621900930611     C     WPOROU        ANDLE     WPOFFC
622000930611     C                   MOVEL     'S'           WSLITT
622100930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
622200930611     C                   END
622300930611     C                   END
622400930611     C                   END
622500930511     C*
622600930511     C                   ENDSR
622700930325     C********************************************************************
622800930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
622900930325     C********************************************************************
623000930325     C     CHKFES        BEGSR
623100060526     c* se già passata la data, non controllo wporou
623200060526     c                   if        wpodata>0
623300060526     c                   z-add     wpodata       w12dat
623400060526     c                   clear                   w12res
623500060526     c                   else
623600931130     C*
623700931130     C* TRASFORMA IL  PROGR.ORE IN DATA
623800931130     C                   Z-ADD     WPOROU        WORE
623900931130     C                   EXSR      OREDAT
624000941227     C                   Z-ADD     WDAT          W12DAT            8 0
624100931130     C                   Z-ADD     WRES          W12RES            3 0
624200060526     c                   endif
624300930325     C*
624400941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
624500930325     C* E' FESTIVO
624600170117     c* l'affidabilità partenza ora fa un doppio tentativo
624700170117    1C     WTC           IFEQ      'P'                                          *PARTENZA
624800170117     C                   Z-ADD     D98LNP        CLNTFP
624900170117     C                   Z-ADD     *ZEROS        CLNTFA
625000170117     c                   exsr      azcln_fes
625100170117     c* se non è festivo guardo anche la trazione
625200170117     c                   if        wslitt<>'S'
625300170117     C                   Z-ADD     D98TFP        CLNTFP
625400170117     C                   Z-ADD     D98TFA        CLNTFA
625500170117     c                   exsr      azcln_fes
625600170117     C                   ENDIF
625700170117     c
625800170117   x1C                   Else
625900170117     c
626000090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
626100090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
626200090108     c**   è possibile rendere festivo in terminal per un altro terminal
626300090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
626400170117     c
626500170117     c** ES 17/01/17 - Faccio doppio tentativo: prima con la filiale e poi con
626600170117     c**               la trazione , se filiale non festiva
626700930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
626800170117     C***  WTC           oreq      'P'                                          *TRAZIONE
626900941227     C                   Z-ADD     D98TFP        CLNTFP
627000941227     C                   Z-ADD     D98TFA        CLNTFA
627100941228     C                   ENDIF
627200941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
627300941227     C                   Z-ADD     *ZEROS        CLNTFP
627400941227     C                   Z-ADD     D98LNA        CLNTFA
627500941228     C                   ENDIF
627600170117     c
627700170117     c                   exsr      azcln_fes
627800170117    1c                   endif
627900930325     C*
628000060526     c                   clear                   wpodata
628100930325     C                   ENDSR
628200170117
628300170117     C********************************************************************
628400170117     c* Verifica se giorno festivo su calendario
628500170117     C********************************************************************
628600170117     c     Azcln_fes     BEGSR
628700170117     C                   Z-ADD     W12DAT        DSDAT8
628800170117     C                   Z-ADD     DSAAS         CLNANN
628900170117     C                   Z-ADD     DSMES         CLNMES
629000170117     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
629100170117     C     D98ERR        IFEQ      '0'                                          --- 1 -->
629200170117     C                   Z-ADD     DSGIO         G
629300170117     C     W12RES        IFEQ      *ZEROS
629400170117     C                   MOVEL     CLNMAT        GFEDS
629500170117     C                   ELSE
629600170117     C                   MOVEL     CLNPOM        GFEDS
629700170117     C                   END
629800170117     c
629900170117     c* se sto verificando i tempi partenza o trazione e
630000170117     c*  la data che sto controllando corrisponde alla data spedizione,
630100170117     c*  la considero sempre lavorativa
630200170117     c                   if        (wtc='P' or wtc='T') and w12dat=d98dfv
630300170117     c                   else
630400170117     c
630500170117     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
630600170117     C                   MOVEL     *BLANKS       WC1Y
630700170117     C                   MOVEL     GFE(G)        WC1Y              8
630800170117     C                   Z-ADD     1             I
630900170117     C     WC1Y          LOOKUP    C1Y(I)                                 99
631000170117     C     *IN99         IFEQ      '1'                                          --- 3 -->
631100170117     C     WIC           IFEQ      'I'
631200170117     C     I1Y(I)        ANDEQ     'S'
631300170117     C                   MOVEL     'S'           WSLITT
631400170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
631500170117     C                   END
631600170117     C     WIC           IFEQ      'C'
631700170117     C     E1Y(I)        ANDEQ     'S'
631800170117     C                   MOVEL     'S'           WSLITT
631900170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
632000170117     C                   END
632100170117     C     WIC           IFEQ      'X'
632200170117     C     E1Y(I)        ANDEQ     'S'
632300170117     C                   MOVEL     'S'           WSLITT
632400170117     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
632500170117     C                   END
632600170117     C                   ELSE                                                   <-- 3 -->
632700170117     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
632800170117     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
632900170117     C                   END                                                    <-- 3 ---
633000170117     C                   END                                                    <-- 2 ---
633100170117     C                   END                                                    <-- 1 ---
633200170117     c
633300170117     c                   endif
633400170117     c                   ENDSR
633500931201     C********************************************************************
633600931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
633700931201     C********************************************************************
633800931201     C     CHKTUC        BEGSR
633900931201     C*
634000931201     C     WIC           IFEQ      'I'                                          --- 1 -->
634100931201     C                   DO        TI            T                 3 0          --- 2 -->
634200931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
634300931201     C     WPOROU        ANDLE     TIF(T)
634400931201     C                   MOVEL     'S'           WSLITT
634500931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
634600931201     C                   LEAVE                                                  *VA A FINE DO
634700931201     C                   ENDIF                                                  <-- 3 ---
634800931201     C                   ENDDO                                                  <-- 2 ---
634900931201     C                   ELSE                                                   <-- 1 -->
635000931201     C                   DO        TC            T                 3 0          --- 2 -->
635100931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
635200931201     C     WPOROU        ANDLE     TCF(T)
635300931201     C                   MOVEL     'S'           WSLITT
635400931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
635500931201     C                   LEAVE                                                  *VA A FINE DO
635600931201     C                   ENDIF                                                  <-- 3 ---
635700931201     C                   ENDDO                                                  <-- 2 ---
635800931201     C                   ENDIF                                                  <-- 1 ---
635900931201     C*
636000931201     C                   ENDSR
636100060418     C********************************************************************
636200060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
636300060418     c*          - eventi particolari che giustificano
636400060418     C********************************************************************
636500060418     C     CHKCEPG       BEGSR
636600060418     C*
636700060418     C     WIC           IFEQ      'I'                                          --- 1 -->
636800060420     C                   DO        PGI           p                 3 0          --- 2 -->
636900060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
637000060420     C     WPOROU        ANDLE     PGIF(p)
637100060418     C                   MOVEL     'S'           WSLITT
637200060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
637300060418     C                   LEAVE                                                  *VA A FINE DO
637400060418     C                   ENDIF                                                  <-- 3 ---
637500060418     C                   ENDDO                                                  <-- 2 ---
637600060418     C                   ELSE                                                   <-- 1 -->
637700060420     C                   DO        PGC           p                 3 0          --- 2 -->
637800060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
637900060420     C     WPOROU        ANDLE     PGCF(p)
638000060418     C                   MOVEL     'S'           WSLITT
638100060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
638200060418     C                   LEAVE                                                  *VA A FINE DO
638300060418     C                   ENDIF                                                  <-- 3 ---
638400060418     C                   ENDDO                                                  <-- 2 ---
638500060418     C                   ENDIF                                                  <-- 1 ---
638600060418     C*
638700060418     C                   ENDSR
638800930512     C********************************************************************
638900931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
639000930512     C********************************************************************
639100931130     C     DATTUC        BEGSR
639200931214     C*
639300931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
639400931214     C                   Z-ADD     12            WNOADD            9 0
639500930325     C*
639600931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
639700931125     C     D98GC2        ORNE      *BLANKS
639800931130     C*
639900931213     C                   Z-ADD     WPOROU        WORE
640000931130     C                   EXSR      OREDAT                                       *ORE->DATA
640100931130     C                   Z-ADD     WDAT          W12DAT
640200931130     C                   Z-ADD     WRES          W12RES
640300930405     C*
640400941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
640500930405     C* DELLA SETTIMANA
640600941227     C                   Z-ADD     W12DAT        DSDAT8
640700941227     C                   Z-ADD     DSAAS         CLNANN
640800941227     C                   Z-ADD     DSMES         CLNMES
640900941227     C                   Z-ADD     *ZEROS        CLNTFP
641000941227     C                   Z-ADD     *ZEROS        CLNTFA
641100930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
641200930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
641300941227     C                   MOVEL     CLNMAT        GSEDS
641400960129     C                   Z-ADD     DSGIO         GI                6 0
641500960129     C                   MOVEL     GSE(GI)       WGSE              1
641600140911     c
641700140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
641800140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
641900140911     c                   if        wtuctut='S'
642000140911     c                   eval      wtuctut='P'
642100140911     c                   endif
642200930326     C*
642300931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
642400931213     C                   DO        4             IT                3 0          --- 3 -->
642500931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
642600931213     C                   MOVEL     TUC(IT)       W93GSE            1
642700931213     C                   MOVE      TUC(IT)       W93GMP            1
642800930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
642900930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
643000931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
643100930512     C     W93GSE        OREQ      ' '
643200140911     c     WTUCTUT       andeq     ' '
643300140911     c
643400930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
643500931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
643600930401     C     W93GMP        OREQ      ' '
643700930325     C                   MOVEL     'S'           WSLITT
643800931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
643900931213     C                   ELSE                                                   <-- 6 -->
644000001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
644100001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
644200001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
644300050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
644400050908     C     Usacons       IFNE      'S'                                          --- 7 -->
644500141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
644600141107     c*    orari giorno per giorno
644700141107     c                   if        OOR2FCSGL<>' '
644800141108     c                             and w12dat<>sav_12dat
644900141107     c                   eval      or_datarif=w12dat
645000141107     c                   eval      or_DDC    =w12dat
645100141107     c                   exsr      Reper_tempi
645200141108     c                   eval      sav_12dat=w12dat
645300141107     c                   endif
645400141107     c
645500930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
645600930325     C     W93GMP        IFEQ      'M'
645700930326     C     W12RES        ANDEQ     *ZEROS
645800141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
645900141111     c                             or OOR2STfS =0
646000141105     c                             or (tuc(IT)<>gc1part and
646100141105     c                              tuc(IT)<>gc2part)
646200140825     C                   MOVEL     'S'           WSLITT
646300140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
646400140911     c* slitto solo una volta  se chiuso tutti " M"
646500140911     c                   if        W93GSE=' '
646600140911     c                   eval      wTUCTUT='S'
646700140911     c                   endif
646800140911     c
646900140825     c                   endif
647000930325     C                   END
647100930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
647200930325     C     W93GMP        IFEQ      'P'
647300930326     C     W12RES        ANDNE     *ZEROS
647400140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
647500140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
647600141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
647700141111     c                             or OOR2STiS =0
647800141105     c                             or (tuc(IT)<>gc1part and
647900141105     c                              tuc(IT)<>gc2part)
648000930325     C                   MOVEL     'S'           WSLITT
648100140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
648200140911     c* slitto solo una volta  se chiuso tutti " P"
648300140911     c                   if        W93GSE=' '
648400140911     c                   eval      wTUCTUT='S'
648500140911     c                   endif
648600140825     c                   endif
648700140825     c
648800931213     C                   ENDIF
648900001128     C                   ENDIF                                                  <-- 7 ---
649000931213     C                   ENDIF                                                  <-- 6 ---
649100931213     C                   ENDIF                                                  <-- 5 ---
649200931213     C                   ENDIF                                                  <-- 4 ---
649300931213     C                   ENDDO                                                  <-- 3 ---
649400930405     C*
649500931213     C                   ENDIF                                                  <-- 2 ---
649600931213     C                   ENDIF                                                  <-- 1 ---
649700140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
649800140911     c                   if        wtuctut='P'  and wslitt='N'
649900140911     c                   clear                   wtuctut
650000140911     c                   endif
650100140911     c
650200930325     C                   ENDSR
650300060406     C********************************************************************
650400060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
650500060406     C********************************************************************
650600060406     C     DATCEPG       BEGSR
650700060406     C*
650800060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
650900060406     C                   Z-ADD     12            WNOADD            9 0
651000060406     C*
651100060406    1C                   if        wevpag='S'
651200060406     C*
651300060406     C                   Z-ADD     WPOROU        WORE
651400060406     C                   EXSR      OREDAT                                       *ORE->DATA
651500060406     C                   Z-ADD     WDAT          W12DAT
651600060406     C                   Z-ADD     WRES          W12RES
651700060418     c                   z-add     1             P
651800060406     c* Verifico se nella data esiste un evento Part che giustifica
651900060418     c     w12dat        lookup    pvd(P)                                 30
652000060418    2c                   dow       *in30 and pvf(P)<>'G'
652100060418     c                   add       1             P
652200060420     c     w12dat        lookup    pvd(P)                                 30
652300060406    2c                   enddo
652400060406     c
652500060406     c* 30 on --> TROVATO !!
652600060406    2c                   if        *in30
652700060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
652800060406     C                   MOVEL     'S'           WSLITT
652900060406     c                   z-add     24            wnoadd
653000060406    3C                   ENDIF
653100060406     C*
653200060406    2C                   ENDIF                                                  <-- 2 ---
653300060406    1C                   ENDIF                                                  <-- 1 ---
653400060406     C*
653500060406     C                   ENDSR
653600931210     C********************************************************************
653700931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
653800931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
653900931210     C********************************************************************
654000050805     C***  CHKDCR        BEGSR
654100931210     C*
654200050805     C***  WPOCRI        IFNE      *ZEROS
654300050805     C***  WPOROU        IFGE      WPOCRI
654400050805     C***  WPOROU        ANDLT     WPOCRF
654500050805     C***                ELSE
654600050805     C***                MOVEL     'S'           WSLITT
654700050805     C***                END
654800050805     C***                END
654900931210     C*
655000050805     C***                ENDSR
655100140822     C********************************************************************
655200140822     C*  Reperimento tempi di consegna per decidere se slittare
655300140822     C********************************************************************
655400140822     C     Reper_tempi   BEGSR
655500140822
655600140822     c                   clear                   trulorSds
655700140825     c                   clear                   trulor2ds
655800140822     c                   eval       IOREtfp = d98tfp
655900140822     c                   eval       IOREtfa = d98tfa
656000140825     c                   eval       IOREdti = d98dti
656100140825     c                   eval       IOREhti = d98hti
656200140825     c                   eval       IOREdcr = s98dcr
656300140825     c                   eval       IOREhcr = s98hcr
656400140825     c                   eval       IOREtcr = s98tcr
656500140825     c                   eval       IOREDEI = d98dei
656600140825     c                   eval       IOREOEI = d98oei
656700140825     c                   eval       IOREfil = d98lna
656800140825     c                   eval       IOREcap = d98cad
656900140825     c                   eval       IOREloc = d98lod
657000140825     c                   eval       IOREnar = d98nzd
657100140825     c
657200140825     c* Data di Riferimento:
657300140825     c* Se impostata la consegnabilità --> prendo questa
657400140822     c                   select
657500140825     c                   when      d98dfc>0
657600141107     c                   if        or_datarif<d98dfc and or_datarif>0
657700141107     c                   eval      ioredta=or_datarif
657800141107     c                   else
657900141107     c                   eval      ioredta=d98dfc
658000141107     c                   endif
658100140825     c* altrimenti data di consegna
658200140825     c                   when      d98dcm>0
658300141107     c                   if        or_datarif<d98dcm and or_datarif>0
658400141107     c                   eval      ioredta=or_datarif
658500141107     c                   else
658600140825     c                   eval      ioredta=d98dcm
658700141107     c                   endif
658800140825     c
658900140822     c                   other
659000140825     c* altrimenti data teorica consegna se > = oggi
659100140825     C                   time                    wrkoggiiso
659200140825     C     *iso          move      wrkoggiiso    ioredta
659300140822     c* se < di oggi, imposto oggi
659400140825     c                   if        d98dei>ioredta
659500140825     c                   eval      ioredta=d98dei
659600140822     c                   endif
659700141107     c
659800141107     c                   if        or_datarif<ioredta and or_datarif>0
659900141107     c                   eval      ioredta=or_datarif
660000141107     c                   endif
660100141107     c
660200140822     c                   endsl
660300140822     c
660400140825     c                   eval       IOREtser = 'C'
660500141107     c                   if         or_ddc>0 and or_ddc<d98ddc
660600141107     c                   eval       IOREDDC=or_ddc
660700141107     c                   eval       IOREnDC=8888888
660800141107     c                   eval       IOREcons = 'S'
660900141107     c                   else
661000140825     c                   eval       IOREDDC=d98ddc
661100140825     c                   eval       IOREnDC=wd98ndc
661200140825     c                   eval       IOREcons = d98cons
661300141107     c                   endif
661400141017     c                   eval       IOREtsp = d98tsp
661500140822     c
661600140822     c                   call      'TRULORSR'
661700140822     c                   parm                    kpjba
661800140822     c                   parm                    trulorSds
661900140825     c                   parm                    trulor2ds
662000140825     c
662100140825     c                   eval      wtrulor= 'S'
662200141107     c                   clear                   or_datarif        8 0
662300141107     c                   clear                   or_ddc            8 0
662400140822     C                   ENDSR
662500950406     C********************************************************************
662600950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
662700950406     C********************************************************************
662800950406     C     DATORE        BEGSR
662900950406     C*
663000950406     C                   Z-ADD     *ZEROS        WORE
663100950406     C     WDAT          IFNE      0
663200950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
663300950406     C     WAA           DIV       4             WQUA              4 0
663400950406     C                   MVR                     WRES
663500950406     C     WAA           MULT      365           WGIO              9 0
663600950406     C                   ADD       WQUA          WGIO
663700950406     C                   ADD       WGG           WGIO
663800950406     C     WMM           SUB       1             MC                3 0
663900950406     C                   DO        MC            WX                3 0
664000950406     C                   ADD       MES(WX)       WGIO
664100950406     C                   END
664200950406     C     WRES          IFEQ      0
664300950406     C     WMM           ANDLE     2
664400950406     C                   SUB       1             WGIO
664500950406     C                   END
664600950406     C     WGIO          MULT      24            WORE              9 0
664700950406     C                   ENDIF
664800950406     C*
664900950406     C                   ENDSR
665000930323     C********************************************************************
665100930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
665200930323     C********************************************************************
665300930323     C     OREDAT        BEGSR
665400930323     C*
665500950407     C                   SETOFF                                       717273
665600950407     C                   SETOFF                                       7475
665700930323     C     WORE          DIV       24            WGIO
665800930323     C                   MVR                     WRES              3 0
665900950406     C*
666000950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
666100950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
666200950406     C*  TROVO ANNO
666300950406     C     WGIO          DIV       365           WAA
666400950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
666500950406     C                   MVR                     WREST                71
666600950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
666700950406     C     WGIO          DIV       365           WAA
666800950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
666900950406     C*                                                    0 = 31/12/PR
667000950406     C   75
667100950406     CAN 71              DO                                                     BIS. E NON ERA
667200950406     C     WAA           SUB       1             WAA
667300950406     C                   Z-ADD     365           WREST
667400950406     C                   END
667500950406     C*
667600950406     C     WAA           DIV       4             WNBIS                                      T.
667700950406     C                   MVR                     WRBIS             3 073
667800950406     C   73
667900950406     CANN71                                                                     NON PIU' BISEST
668000950406     CORN73              ADD       1             WREST                          E'BISESTILE
668100950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
668200950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
668300950406     C  N73              SUB       1             $PM(1)
668400950406     C                   Z-ADD     1             WI                2 0
668500950406     C* TROVO IL MESE E GIORNI
668600950406     C     WREST         LOOKUP    $PM(WI)                            74  73
668700950406     C                   MOVE      WI            WMM
668800950406     C     WI            COMP      1                                      72
668900950406     C  N72
669000950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
669100950406     C   74              SUB       1             WI
669200950406     C  N72
669300950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
669400950406     C   72              MOVE      WREST         WGG
669500950406     C                   ADD       1900          WAA
669600950406     C*
669700930323     C*
669800930323     C                   ENDSR
6699009407151507 C********************************************************************
6700009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6701009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6702009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6703009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6704009407151507 C********************************************************************
6705009407151507 C     FINGIO        BEGSR
6706009407151507 C*
6707009407151507 C     WPORE         DIV       24            WGIO
6708009407151507 C                   MVR                     WRES
6709009407151507 C     WRES          IFEQ      *ZEROS
6710009407151507 C                   Z-ADD     12            WNORE             3 0
6711009407151507 C                   ELSE
6712009407151507 C                   Z-ADD     *ZEROS        WNORE
6713009407151507 C                   ENDIF
6714009407151507 C*
6715009407151507 C                   ENDSR
671600930517     C********************************************************************
671700971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
671800930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
671900930517     C********************************************************************
672000930517     C     CERCAL        BEGSR
672100930517     C*
672200930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
672300941227     C                   MOVE      CLNTFP        DCATFP
672400941227     C                   MOVE      CLNTFA        DCATFA
672500941227     C                   MOVE      CLNANN        DCAANN
672600941227     C                   MOVE      CLNMES        DCAMES
672700930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
672800930517     C                   Z-ADD     1             I
672900930517     C     DCALEN        LOOKUP    CAK(I)                                 99
673000930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
673100930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
673200941227     C     KEYCLN        CHAIN     AZCLN01L                           99
673300930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
673400930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
673500930517     C                   MOVEL     '1'           D98ERR
673600930517     C                   MOVEL     MSG(3)        D98MSG
673700930517     C                   ELSE                                                   <-- 2 -->
673800970401     C* CONTROLLA SE ANNULLATO
673900970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
674000970401     C                   MOVEL     '1'           D98ERR
674100970401     C                   MOVEL     MSG(3)        D98MSG
674200970401     C                   ELSE                                                   <-- 3 -->
674300930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
674400930517     C                   ADD       1             WINDCA            6 0
674500970401     C     WINDCA        IFLE      200                                          --- 4 -->
674600970401     C     WINDCA        ANDGT     *ZEROS
674700930517     C                   Z-ADD     WINDCA        I
674800930517     C                   MOVEL     DCALEN        CAK(I)
674900941227     C                   MOVEL     CLNMAT        CAM(I)
675000941227     C                   MOVEL     CLNPOM        CAP(I)
675100970401     C                   END                                                    <-- 4 ---
675200970401     C                   ENDIF                                                  <-- 3 ---
675300930517     C                   END                                                    <-- 2 ---
675400930517     C                   ELSE                                                   <-- 1 -->
675500930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
675600941227     C                   MOVEL     CAM(I)        CLNMAT
675700941227     C                   MOVEL     CAP(I)        CLNPOM
675800930517     C                   END                                                    <-- 1 ---
675900930517     C*
676000930517     C                   ENDSR
676100010821     C********************************************************************
676200010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
676300010821     C********************************************************************
676400010821     C     CHKVCA        BEGSR
676500010821     C*
676600010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
676700010821     C     D98CAS        IFGT      *ZEROS
676800010821     C     D98VCA        ANDEQ     *BLANKS
676900010821     C                   MOVEL     'ITL'         D98VCA
677000010821     C                   ENDIF
677100010822     C*
677200010822     C     D98VCA        IFNE      'ITL'
677300010822     C     D98VCA        ANDNE     'EUR'
677400010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
677500010822     C                   Z-ADD     D98CAS        INPIMP
677600010822     C                   MOVEL     D98VCA        INPDIV
677700010822     C                   EXSR      EXECNV
677800010822     C                   Z-ADD     OUTIMP        D98CAS
677900010822     C                   MOVEL     §GEDCN        D98VCA
678000010822     C                   ENDIF
678100010821     C*
678200010821     C                   ENDSR
678300010821     C********************************************************************
678400010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
678500010821     C********************************************************************
678600010821     C     EXECNV        BEGSR
678700010821     C*
678800010821     C                   CLEAR                   YEURDS
678900010821     C*
679000010821     C                   MOVEL     INPDIV        YECDVI
679100010821     C                   Z-ADD     INPIMP        YECIMI
679200010821     C                   MOVEL     §GEDCN        YECDVO
679300010821     C                   MOVEL     '3'           YECDCO
679400010821     C                   MOVEL     'H'           YECTAR
679500010821     C                   CALL      'YEURCO'
679600010821     C                   PARM                    YEURDS
679700010821     C*
679800010821     C                   Z-ADD     YECIMO        OUTIMP
679900010821     C*
680000010821     C                   ENDSR
680100011211     C*------------------------------------------------------------------------*
680200011211     C* Controllo importo massimo contrassegno.
680300011211     C*------------------------------------------------------------------------*
680400011211     C     REPMAXCSB     BEGSR
680500011211     C*
680600011211     C                   CLEAR                   Trul21DS
680700011211     C*
680800160420     c                   eval      i21cbo = d98cbo
680900011211     C                   EVAL      I21TSp = d98TSP
681000011211     C                   EVAL      I21NzD = d98NZD
681100011211     C                   EVAL      I21LnP = d98LNP
681200011211     C                   EVAL      I21LnA = d98LNA
681300011211     C                   EVAL      I21Ksc = d98KSC
681400011211     C                   EVAL      I21TiC = d98TIC
681500011211     C                   EVAL      I21Imp = d98CAS
681600011211     C                   EVAL      I21Div = d98VCA
681700160420     c                   eval      i21cbo = d98cbo
681800011211     C*
681900011211     C                   CALL      'TRUL21R'
682000011211     C                   PARM                    Trul21DS
682100160420
682200160420     c* bolla in partenza--> devo avere anche il flag di abilitazione C/Ass
682300160420     c                   if        wtrasm='N'
682400160420     c                   if        o21fca <>*blanks
682500160420     c                   eval      §b4ltca = 'S'
682600160420     c                   endif
682700160420     c                   endif
682800011211     C*
682900011211     C                   ENDSR
683000011211     C*------------------------------------------------------------------------*
683100011211
683200930414     C********************************************************************
683300930414     C*   RUTINZ - ROUTINE INIZIALE
683400930414     C********************************************************************
683500930414     C     RUTINZ        BEGSR
683600930414     C*
683700930414     C* PARAMETRI IN INPUT ALLA ROUTINE
683800930414     C     *ENTRY        PLIST
683900970731     C                   PARM                    DS99
684000941227     C*
684100941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
684200941227     C     KEYBLP        KLIST
684300941227     C                   KFLD                    BLPAAS
684400941227     C                   KFLD                    BLPLNP
684500941227     C                   KFLD                    BLPNRS
684600941227     C                   KFLD                    BLPNSP
684700050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
684800941227     C     KEYGCP        KLIST
684900050405     C                   KFLD                    KWGCPAAS
685000050405     C                   KFLD                    KWGCPLNP
685100050405     C                   KFLD                    KWGCPNRS
685200050405     C                   KFLD                    KWGCPNSP
685300050405     C                   KFLD                    KWGCPFRG
685400050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
685500031125     C     KEYGC_P       KLIST
685600050405     C                   KFLD                    KWGCPAAS
685700050405     C                   KFLD                    KWGCPLNP
685800050405     C                   KFLD                    KWGCPNRS
685900050405     C                   KFLD                    KWGCPNSP
686000970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
686100150120     C**   KEYBLT        KLIST
686200150120     C**                 KFLD                    BLTAAS
686300150120     C**                 KFLD                    BLTLNP
686400150120     C**                 KFLD                    BLTNRS
686500150120     C**                 KFLD                    BLTNSP
686600941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
686700941227     C     KEYARB        KLIST
686800941227     C                   KFLD                    ARBAAS
686900941227     C                   KFLD                    ARBLNP
687000941227     C                   KFLD                    ARBNRS
687100941227     C                   KFLD                    ARBNSP
687200051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
687300941228     C     KEYAR9        KLIST
687400941228     C                   KFLD                    AR9AAS
687500941228     C                   KFLD                    AR9LNP
687600941228     C                   KFLD                    AR9NRS
687700941228     C                   KFLD                    AR9NSP
687800941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
687900050405     C     KEYGCP21      KLIST
688000050405     C                   KFLD                    KWGCPAAS
688100050405     C                   KFLD                    KWGCPLNP
688200050405     C                   KFLD                    KWGCPNRS
688300050405     C                   KFLD                    KWGCPNSP
688400050405     C                   KFLD                    KWGCPFRG
688500970912     C* CHIAVE LETTURA FNART01L - PARZIALE
688600150120     C***  KEYART        KLIST
688700150120     C***                KFLD                    ARTAAS
688800150120     C***                KFLD                    ARTLNP
688900150120     C***                KFLD                    ARTNRS
689000150120     C***                KFLD                    ARTNSP
689100950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
689200950102     C     KEYLBL        KLIST
689300950102     C                   KFLD                    LBLAAN
689400950102     C                   KFLD                    LBLLPN
689500950102     C                   KFLD                    LBLNRN
689600950102     C                   KFLD                    LBLNSN
689700930414     C*
689800070827     c     kAR5          Klist
689900070827     c                   Kfld                    d98Aas
690000070827     c                   Kfld                    d98Lnp
690100070827     c                   Kfld                    d98nrs
690200070827     c                   Kfld                    d98nsp
690300070827     c                   Kfld                    kAr5Trd
690400150325     c     kAR4          Klist
690500150325     c                   Kfld                    d98Aas
690600150325     c                   Kfld                    d98Lnp
690700150325     c                   Kfld                    d98nrs
690800150325     c                   Kfld                    d98nsp
690900150325     c                   Kfld                    kAr4Trc
691000150218     c*
691100150218     c     kart          Klist
691200150218     c                   Kfld                    d98Aas
691300150218     c                   Kfld                    d98Lnp
691400150218     c                   Kfld                    d98nrs
691500150218     c                   Kfld                    d98nsp
691600150218     c     kbrv          Klist
691700150218     c                   Kfld                    artfls
691800150218     c                   Kfld                    artlna
691900150218     c                   Kfld                    artnrs
692000150218     c                   Kfld                    artnsc
692100150325     C     Kfapd         klist
692200150325     C                   kfld                    kpdtip
692300150325     C                   kfld                    kpdpdr
692400930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
692500930414     C     KEYTAB        KLIST
692600930414     C                   KFLD                    KTBUTE
692700930414     C                   KFLD                    KTBCOD
692800930414     C                   KFLD                    KTBKEY
692900060406     C     Ktab          KLIST
693000060406     C                   KFLD                    KTBUTE
693100060406     C                   KFLD                    KTBCOD
693200930414     C*
693300941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
693400941227     C     KEYCLN        KLIST
693500941227     C                   KFLD                    CLNTFP
693600941227     C                   KFLD                    CLNTFA
693700941227     C                   KFLD                    CLNANN
693800941227     C                   KFLD                    CLNMES
693900941227     C*
694000941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
694100941227     C     KEYCAS        KLIST
694200941227     C                   KFLD                    CASPTA
694300941227     C                   KFLD                    CASLNP
694400941227     C                   KFLD                    CASLNA
694500941227     C                   KFLD                    CASTSP
694600941227     C                   KFLD                    CASDDE
694700930420     C*
694800990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
694900930420     C     KEYEVB        KLIST
695000930420     C                   KFLD                    KEBAAS
695100930420     C                   KFLD                    KEBLNP
695200930420     C                   KFLD                    KEBNRS
695300930420     C                   KFLD                    KEBNSP
695400010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
695500010821     C     KTBE          KLIST
695600010821     C                   KFLD                    TBECOD
695700010821     C                   KFLD                    TBEKE1
695800010821     C                   KFLD                    TBEKE2
695900930414     C*
696000010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
696100930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
696200930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
696300930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
696400930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
696500930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
696600930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
696700930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
696800930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
696900930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
697000010821     C     *LIKE         DEFINE    D98CAS        INPIMP
697100010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
697200010821     C     *LIKE         DEFINE    D98VCA        INPDIV
697300930414     C*
697400930414     C* RECUPERO IL CODICE DELL'AZIENDA
697500930414     C                   Z-ADD     1             CODUT             1 0
697600970701     C                   Z-ADD     *ZEROS        I                 6 0
697700930414     C*
697800930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
697900930414     C                   EXSR      AZZERA
698000930414     C*
698100930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
698200941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
698300941227     C                   MOVEL     'S'           CTABSN            1
698400930414     C                   EXSR      CARTAB
698500941227     C                   ENDIF
698600041116     c* Se passato umero di spedizioni apro i files , altrimenti no
698700041116     c                   if        d98nsp>0 and wapri=' '
698800041116     c                   open      fnblp01l
698900050405     c                   open      tigcp01l
699000150120     c***                open      fnblt01l
699100041116     c                   open      fnarb01l
699200051115     c                   open      fiar901l
699300150325     c                   open      fiar401l
699400050405     c                   open      tigcp21l
699500150218     c                   open      fnart01l
699600150218     c                   open      fnbrv07l
699700041116     c                   open      fnlbl01l
699800041116     c                   open      fnevb01l
699900070827     c                   open      fiar501l
700000041116     c                   eval      wapri='S'
700100041116     c                   endif
700200930414     C*
700300930414     C                   ENDSR
700400941227     C*------------------------------------------------------------------------*
700500941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
700600941227     C*------------------------------------------------------------------------*
700700941227     C     SUBDAT        BEGSR
700800941227     C*
700900941227     C                   TIME                    DTTIME
701000941227     C                   Z-ADD     DTANN         DSANN
701100941227     C                   Z-ADD     DTMES         DSMES
701200941227     C                   Z-ADD     DTGIO         DSGIO
701300941227     C                   Z-ADD     DTSEC         DSSEC
701400941227     C*
701500941227     C                   ENDSR
701600930414     C********************************************************************
701700930414     C*   CARTAB - CARICA LE TABELLE
701800930414     C********************************************************************
701900930414     C     CARTAB        BEGSR
702000930414     C*
702100930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
702200930414     C     D98ERR        IFEQ      '0'
702300930421     C                   Z-ADD     0             I
702400930414     C                   Z-ADD     CODUT         KTBUTE
702500930414     C                   MOVEL     '1Y'          KTBCOD
702600930414     C                   MOVEL     *BLANKS       KTBKEY
702700930414     C     KEYTAB        SETLL     TABEL00F                           99
702800930414     C  N99              READ      TABEL00F                               99
702900930414     C     *IN99         IFEQ      '1'
703000930414     C                   MOVEL     '1'           D98ERR
703100930414     C                   MOVEL     MSG(5)        D98MSG
703200930414     C                   END
703300930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
703400930414     C     TBLCOD        ANDEQ     '1Y'
703500930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
703600930421     C                   ADD       1             I
703700930421     C                   MOVEL     TBLKEY        C1Y(I)
703800930414     C                   MOVEL     TBLUNI        DS1Y
703900930421     C                   MOVEL     §1YFEI        I1Y(I)
704000930421     C                   MOVEL     §1YFEC        E1Y(I)
704100930414     C                   END                                                    <-- 3 ---
704200930414     C                   READ      TABEL00F                               99
704300930414     C                   END                                                    <-- 2 ---
704400930414     C                   END
704500930414     C*
704600930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
704700930414     C     D98ERR        IFEQ      '0'
704800930421     C                   Z-ADD     0             I
704900930414     C                   Z-ADD     CODUT         KTBUTE
705000930414     C                   MOVEL     '1X'          KTBCOD
705100930414     C                   MOVEL     *BLANKS       KTBKEY
705200930414     C     KEYTAB        SETLL     TABEL00F                           99
705300930414     C  N99              READ      TABEL00F                               99
705400930414     C     *IN99         IFEQ      '1'
705500930414     C                   MOVEL     '1'           D98ERR
705600930414     C                   MOVEL     MSG(1)        D98MSG
705700930414     C                   END
705800930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
705900930414     C     TBLCOD        ANDEQ     '1X'
706000930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
706100930421     C                   ADD       1             I
706200930421     C                   MOVEL     TBLKEY        C1X(I)
706300930414     C                   MOVEL     TBLUNI        DS1X
706400930421     C                   Z-ADD     §1XHHG        G1X(I)
706500930421     C                   Z-ADD     §1XHHO        O1X(I)
706600930421     C                   Z-ADD     §1XHHM        M1X(I)
706700930421     C                   Z-ADD     §1XHHP        P1X(I)
706800930421     C                   Z-ADD     §1XHHI        I1X(I)
706900930421     C                   Z-ADD     §1XHHS        S1X(I)
707000930414     C                   END                                                    <-- 3 ---
707100930414     C                   READ      TABEL00F                               99
707200931223     C                   ENDDO                                                  <-- 2 ---
707300050523     c
707400050523     c* controllo riempimento skiere
707500050523     c                   clear                   trul0sds
707600050523     c                   eval      i0sski='C1X/G1X/O1X/...'
707700050523     c                   eval      i0sele=%elem(c1x)
707800050523     c                   eval      i0spie=i
707900050523     c                   eval      i0sfile='TABEL00F  '
708000050523     c                   eval      i0scod=' 1X  '
708100050523     c                   eval      i0spgm='TNSD99R'
708200050523     c                   movel     trul0sds      kpjbu
708300050523     c                   call      'TRUL0SR'
708400050523     c                   parm                    kpjba
708500050523     C                   END
708600930421     C*
708700930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
708800930421     C     D98ERR        IFEQ      '0'
708900930421     C                   Z-ADD     0             I
709000930421     C                   Z-ADD     CODUT         KTBUTE
709100930421     C                   MOVEL     '2A'          KTBCOD
709200060406     C     Ktab          SETLL     TABEL00F                           99
709300060406     C  N99ktab          READe     TABEL00F                               99
709400930421     C     *IN99         IFEQ      '1'
709500930421     C                   MOVEL     '1'           D98ERR
709600930421     C                   MOVEL     MSG(8)        D98MSG
709700930421     C                   END
709800930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
709900930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
710000930421     C                   ADD       1             I
710100930421     C                   MOVEL     TBLKEY        C2A(I)
710200060406     C                   MOVEL     TBLUNI        ds2a
710300060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
710400060406     C                   MOVEL     w003a         D2A(I)
710500930421     C                   END                                                    <-- 3 ---
710600060406     C     ktab          READe     TABEL00F                               99
710700931223     C                   ENDDO                                                  <-- 2 ---
710800050523     c* controllo riempimento skiere
710900050523     c                   clear                   trul0sds
711000050523     c                   eval      i0sski='C2A/D2A'
711100050523     c                   eval      i0sele=%elem(c2a)
711200050523     c                   eval      i0spie=i
711300050523     c                   eval      i0sfile='TABEL00F  '
711400060406     c                   eval      i0scod='2A/2Z'
711500050523     c                   eval      i0spgm='TNSD99R'
711600050523     c                   movel     trul0sds      kpjbu
711700050523     c                   call      'TRUL0SR'
711800050523     c                   parm                    kpjba
711900930421     C                   END
712000060406     c* Tabella  2Z
712100060406     C     D98ERR        IFEQ      '0'
712200060406     C                   Z-ADD     CODUT         KTBUTE
712300060406     C                   MOVEL     '2Z'          KTBCOD
712400060406     C     Ktab          SETLL     TABEL00F                           99
712500060406     C  N99ktab          READe     TABEL00F                               99
712600060406     C     *IN99         IFEQ      '1'
712700060406     C                   MOVEL     '1'           D98ERR
712800060406     C                   MOVEL     MSG(33)       D98MSG
712900060406     C                   END
713000060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
713100060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
713200060406     C                   Z-ADD     1             I
713300060406     C                   MOVEL     TBLKEY        w003a
713400060406     c     w003a         lookup    c2a(i)                                 99
713500060406     C     *IN99         IFEQ      *off
713600060406     C                   MOVEL     '1'           D98ERR
713700060406     C                   MOVEL     MSG(34)       D98MSG
713800060406     C                   Else
713900060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
714000060406     C                   END                                                    <-- 3 ---
714100060406     C                   END                                                    <-- 3 ---
714200060406     C     ktab          READe     TABEL00F                               99
714300060406     C                   ENDDO                                                  <-- 2 ---
714400060406     C                   END
714500941227     C*
714600941227     C* CARICA TABELLA CODICI BOLLA -3A-
714700941227     C     D98ERR        IFEQ      '0'
714800941228     C                   Z-ADD     0             I
714900941227     C                   Z-ADD     CODUT         KTBUTE
715000941227     C                   MOVEL     '3A'          KTBCOD
715100941227     C                   MOVEL     *BLANKS       KTBKEY
715200941227     C     KEYTAB        SETLL     TABEL00F                           99
715300941227     C  N99              READ      TABEL00F                               99
715400941227     C     *IN99         IFEQ      '1'
715500941227     C                   MOVEL     '1'           D98ERR
715600941227     C                   MOVEL     MSG(22)       D98MSG
715700941227     C                   END
715800941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
715900941227     C     TBLCOD        ANDEQ     '3A'
716000941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
716100941227     C                   ADD       1             I
716200941227     C                   MOVEL     TBLKEY        C3A(I)
716300941227     C                   MOVEL     TBLUNI        DS3A
716400941227     C                   MOVEL     §3ATB1        T3A(I)
716500941228     C                   Z-ADD     §3AFCA        F3A(I)
716600990308     C                   MOVEL     §3ARBL        R3A(I)
716700941227     C                   END                                                    <-- 3 ---
716800941227     C                   READ      TABEL00F                               99
716900941227     C                   END                                                    <-- 2 ---
717000050523     c* controllo riempimento skiere
717100050523     c                   clear                   trul0sds
717200050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
717300050523     c                   eval      i0sele=%elem(c3a)
717400050523     c                   eval      i0spie=i
717500050523     c                   eval      i0sfile='TABEL00F  '
717600050523     c                   eval      i0scod=' 3A  '
717700050523     c                   eval      i0spgm='TNSD99R'
717800050523     c                   movel     trul0sds      kpjbu
717900050523     c                   call      'TRUL0SR'
718000050523     c                   parm                    kpjba
718100941227     C                   END
718200971210     C*
718300020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
718400020226     C                   Z-ADD     *ZEROS        J                 3 0
718500160331     C                   Z-ADD     *ZEROS        D                 3 0
718600971210IF  0C     D98ERR        IFEQ      '0'
718700971210     C     *LOVAL        SETLL     AZORG01L
718800971210     C                   READ      AZORG01L                               98
718900971210DO  1C     *IN98         DOWEQ     *OFF
719000971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
719100971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
719200971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
719300020627     C                   MOVEL     ORGDE3        OG143
719400020702     C*
719500020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
719600020702     C                   CLEAR                   DNTW
719700020702     C*
719800020702     C                   MOVEL(P)  'NTW'         TBECOD
719900020702     C                   MOVEL(P)  §OGNTW        TBEKE1
720000020702     C     KTBE          CHAIN     TNTBE01L
720100020702     C                   IF        %found(TNTBE01L)
720200020702     C                   MOVEL     TBEUNI        DNTW
720300020702     C                   ENDIF
720400020702     C*
720500020702     C                   MOVEL     §NTWFIE       FL1(F)
720600020627     C*
720700020226     C* VERIFICO SE FILIALE FEDEX
720800020226     C                   IF        §OGNTW = 'FED'
720900020226     C                   ADD       1             J
721000020226     C                   Z-ADD     ORGFIL        FILFED(J)
721100020226     C                   ENDIF
721200160331     C* VERIFICO SE FILIALE DPD
721300160331     C                   IF        §OGNTW = 'DPD'
721400160331     C                   ADD       1             D
721500160331     C                   Z-ADD     ORGFIL        FILDPD(D)
721600160331     C                   ENDIF
721700020226     C*
721800971210E   2C                   ENDIF
721900971210     C                   READ      AZORG01L                               98
722000971210E   1C                   ENDDO
722100971210E   0C                   ENDIF
722200010821     C*
722300010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
722400010821     C                   MOVEL     'GED'         TBECOD
722500070514     C                   MOVEL     *BLANKS       TBEKE1
722600070514     C                   MOVEL     '1'           TBEKE1
722700010821     C                   MOVEL     *BLANKS       TBEKE2
722800010821     C     KTBE          CHAIN     TNTBE01L                           21
722900070514     C  N21              MOVEL     TBEUNI        DGED
723000150326     c
723100150326     C* REPERISCO LA tabella flag operativi per tabella OLP
723200150326     c                   clear                   dvpo
723300150326     C                   MOVEL     'VPO'         TBECOD
723400150326     C                   MOVEL     *BLANKS       TBEKE2
723500150326     C                   MOVEL(P)  'VPO'         TBEKE1
723600150326     C     KTBE          CHAIN     TNTBE01L                           21
723700150326     C  N21              MOVEL     TBEUNI        DVPO
723800150326     C*
723900031201     C*
724000031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
724100031201     C     D98ERR        IFEQ      '0'
724200060403     C                   Z-ADD     0             I7QW              3 0
724300031201     C                   Z-ADD     CODUT         KTBUTE
724400031201     C                   MOVEL     '7Q'          KTBCOD
724500031201     C                   MOVEL     *BLANKS       KTBKEY
724600031201     C     KEYTAB        SETLL     TABEL00F                           99
724700031201     C  N99              READ      TABEL00F                               99
724800031201     C     *IN99         IFEQ      '1'
724900031201     C                   MOVEL     '1'           D98ERR
725000031201     C                   MOVEL     MSG(8)        D98MSG
725100031201     C                   ENDIF
725200031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
725300031201     C     TBLCOD        ANDEQ     '7Q'
725400031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
725500031201     C                   MOVEL     TBLUNI        DS7Q
725600031201     C                   IF        §7Q4D1 = 'R' OR
725700031201     C                             §7Q4D2 = 'R' OR
725800031201     C                             §7Q4D3 = 'R' OR
725900031201     C                             §7Q4D4 = 'R' OR
726000031201     C                             §7Q4D5 = 'R'
726100031201     C                   ADD       1             I7QW
726200031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
726300031201     C                   ENDIF                                                  <-- 3 ---
726400031201     C                   ENDIF                                                  <-- 3 ---
726500031201     C                   READ      TABEL00F                               99
726600031201     C                   ENDDO                                                  <-- 2 ---
726700031201     C                   ENDIF
726800031201     C*
726900031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
727000031201     C     D98ERR        IFEQ      '0'
727100031201     C                   Z-ADD     CODUT         KTBUTE
727200031201     C                   MOVEL     '7W'          KTBCOD
727300031201     C                   MOVEL     *BLANKS       KTBKEY
727400031201     C     KEYTAB        SETLL     TABEL00F                           99
727500031201     C  N99              READ      TABEL00F                               99
727600031201     C     *IN99         IFEQ      '1'
727700031201     C                   MOVEL     '1'           D98ERR
727800031201     C                   MOVEL     MSG(8)        D98MSG
727900031201     C                   ENDIF
728000031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
728100031201     C     TBLCOD        ANDEQ     '7W'
728200031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
728300031201     C                   MOVEL     TBLUNI        DS7W
728400031201     C                   IF        §7W6D1 = 'C' OR
728500031201     C                             §7W6D2 = 'C' OR
728600031201     C                             §7W6D3 = 'C' OR
728700031201     C                             §7W6D4 = 'C' OR
728800031201     C                             §7W6D5 = 'C'
728900031201     C                   ADD       1             I7QW
729000031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
729100031201     C                   ENDIF                                                  <-- 3 ---
729200031201     C                   ENDIF                                                  <-- 3 ---
729300031201     C                   READ      TABEL00F                               99
729400031201     C                   ENDDO                                                  <-- 2 ---
729500050523     c* controllo riempimento skiere
729600050523     c                   clear                   trul0sds
729700050523     c                   eval      i0sski='SK7QW'
729800050523     c                   eval      i0sele=%elem(sk7qw)
729900050523     c                   eval      i0spie=i7qw
730000050523     c                   eval      i0sfile='TABEL00F  '
730100050523     c                   eval      i0scod='7Q-7W'
730200050523     c                   eval      i0spgm='TNSD99R'
730300050523     c                   movel     trul0sds      kpjbu
730400050523     c                   call      'TRUL0SR'
730500050523     c                   parm                    kpjba
730600031201     C                   ENDIF
730700930414     C*
730800930414     C                   ENDSR
730900930414     C********************************************************************
731000930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
731100930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
731200930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
731300930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
731400930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
731500930414     C********************************************************************
731600930414     C     RO3ORE        BEGSR
731700930414     C*
731800930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
731900930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
732000930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
732100050902     c                   movel     ' '           WR3MATPOM         1
732200931209     C                   Z-ADD     *ZEROS        WR3HHO
732300931209     C                   Z-ADD     *ZEROS        WR3HHA
732400930414     C                   MOVE      WR3TAB        WR3COD
732500930604     C                   MOVEL     D98LNA        WR3COD            8
732600930421     C                   Z-ADD     1             I
732700930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
732800930414     C     *IN99         IFEQ      '0'
732900930414     C                   MOVE      WR3TAB        WR3COD
733000930414     C                   MOVEL     '   '         WR3COD            8
733100930421     C                   Z-ADD     1             I
733200930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
733300930414     C                   END
733400930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
733500930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
733600930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
733700930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
733800930414     C                   ELSE                                                   <-- 2 -->
733900930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
734000930421     C                   Z-ADD     M1X(I)        WR3HHO
734100930510     C                   Z-ADD     I1X(I)        WR3HHA
734200050902     c                   movel     'M'           WR3MATPOM         1
734300930414     C                   END
734400930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
734500930421     C                   Z-ADD     P1X(I)        WR3HHO
734600930510     C                   Z-ADD     S1X(I)        WR3HHA
734700050902     c                   movel     'P'           WR3MATPOM
734800930414     C                   END
734900930414     C                   END                                                    <-- 2 ---
735000930414     C                   END                                                    <-- 1 ---
735100930414     C*
735200930414     C                   ENDSR
735300930423     C********************************************************************
735400930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
735500930423     C********************************************************************
735600930423     C     W98D98        BEGSR
735700930423     C*
735800941228     C*
735900941228     C* ORE REALI PARTENZA INTERNE
736000941228     C                   Z-ADD     W98RPI        W98               3 0
736100941228     C                   EXSR      W98999
736200941228     C                   Z-ADD     D98           D98RPI
736300941228     C* ORE REALI PARTENZA CLIENTE
736400941228     C                   Z-ADD     W98RPC        W98
736500941228     C                   EXSR      W98999
736600941228     C                   Z-ADD     D98           D98RPC
736700930423     C* ORE REALI TRAZIONE INTERNE
736800941228     C                   Z-ADD     W98RTI        W98
736900941228     C                   EXSR      W98999
737000941228     C                   Z-ADD     D98           D98RTI
737100930423     C* ORE REALI TRAZIONE CLIENTE
737200941228     C                   Z-ADD     W98RTC        W98
737300941228     C                   EXSR      W98999
737400941228     C                   Z-ADD     D98           D98RTC
737500930423     C* ORE REALI CONSEGNA INTERNE
737600941228     C                   Z-ADD     W98RCI        W98
737700941228     C                   EXSR      W98999
737800941228     C                   Z-ADD     D98           D98RCI
737900930423     C* ORE REALI CONSEGNA CLIENTE
738000941228     C                   Z-ADD     W98RCC        W98
738100941228     C                   EXSR      W98999
738200941228     C                   Z-ADD     D98           D98RCC
738300930423     C* ORE REALI RESA INTERNE
738400941228     C                   Z-ADD     W98RRI        W98
738500941228     C                   EXSR      W98999
738600941228     C                   Z-ADD     D98           D98RRI
738700930423     C* ORE REALI RESA CLIENTE
738800941228     C                   Z-ADD     W98RRC        W98
738900941228     C                   EXSR      W98999
739000941228     C                   Z-ADD     D98           D98RRC
739100941228     C* ORE DIFF. PARTENZA INTERNE
739200941228     C                   Z-ADD     W98FPI        W98
739300941228     C                   EXSR      W98999
739400941228     C                   Z-ADD     D98           D98FPI
739500941228     C* ORE DIFF. PARTENZA CLIENTE
739600941228     C                   Z-ADD     W98FPC        W98
739700941228     C                   EXSR      W98999
739800941228     C                   Z-ADD     D98           D98FPC
739900930423     C* ORE DIFF. TRAZIONE INTERNE
740000941228     C                   Z-ADD     W98FTI        W98
740100941228     C                   EXSR      W98999
740200941228     C                   Z-ADD     D98           D98FTI
740300930423     C* ORE DIFF. TRAZIONE CLIENTE
740400941228     C                   Z-ADD     W98FTC        W98
740500941228     C                   EXSR      W98999
740600941228     C                   Z-ADD     D98           D98FTC
740700930423     C* ORE DIFF. CONSEGNA INTERNE
740800941228     C                   Z-ADD     W98FCI        W98
740900941228     C                   EXSR      W98999
741000941228     C                   Z-ADD     D98           D98FCI
741100930423     C* ORE DIFF. CONSEGNA CLIENTE
741200941228     C                   Z-ADD     W98FCC        W98
741300941228     C                   EXSR      W98999
741400941228     C                   Z-ADD     D98           D98FCC
741500930423     C* ORE DIFF. RESA INTERNE
741600941228     C                   Z-ADD     W98FRI        W98
741700941228     C                   EXSR      W98999
741800941228     C                   Z-ADD     D98           D98FRI
741900930423     C* ORE DIFF. RESA CLIENTE
742000941228     C                   Z-ADD     W98FRC        W98
742100941228     C                   EXSR      W98999
742200941228     C                   Z-ADD     D98           D98FRC
742300941228     C* ORE NON CONF. PARTENZA INTERNE
742400941228     C     W98NPI        IFGT      999
742500941228     C                   Z-ADD     999           D98NPI
742600941228     C                   ELSE
742700941228     C                   Z-ADD     W98NPI        D98NPI
742800941228     C                   END
742900941228     C* ORE NON CONF. PARTENZA CLIENTE
743000941228     C     W98NPC        IFGT      999
743100941228     C                   Z-ADD     999           D98NPC
743200941228     C                   ELSE
743300941228     C                   Z-ADD     W98NPC        D98NPC
743400941228     C                   END
743500930423     C* ORE NON CONF. TRAZIONE INTERNE
743600930423     C     W98NTI        IFGT      999
743700930423     C                   Z-ADD     999           D98NTI
743800930423     C                   ELSE
743900930423     C                   Z-ADD     W98NTI        D98NTI
744000930423     C                   END
744100930423     C* ORE NON CONF. TRAZIONE CLIENTE
744200930423     C     W98NTC        IFGT      999
744300930423     C                   Z-ADD     999           D98NTC
744400930423     C                   ELSE
744500930423     C                   Z-ADD     W98NTC        D98NTC
744600930423     C                   END
744700930423     C* ORE NON CONF. CONSEGNA INTERNE
744800930423     C     W98NCI        IFGT      999
744900930423     C                   Z-ADD     999           D98NCI
745000930423     C                   ELSE
745100930423     C                   Z-ADD     W98NCI        D98NCI
745200930423     C                   END
745300930423     C* ORE NON CONF. CONSEGNA CLIENTE
745400930423     C     W98NCC        IFGT      999
745500930423     C                   Z-ADD     999           D98NCC
745600930423     C                   ELSE
745700930423     C                   Z-ADD     W98NCC        D98NCC
745800930423     C                   END
745900930423     C* ORE NON CONF. RESA INTERNE
746000930423     C     W98NRI        IFGT      999
746100930423     C                   Z-ADD     999           D98NRI
746200930423     C                   ELSE
746300930423     C                   Z-ADD     W98NRI        D98NRI
746400930423     C                   END
746500930423     C* ORE NON CONF. RESA CLIENTE
746600930423     C     W98NRC        IFGT      999
746700930423     C                   Z-ADD     999           D98NRC
746800930423     C                   ELSE
746900930423     C                   Z-ADD     W98NRC        D98NRC
747000930423     C                   END
747100930423     C* ORE X TURNO CHIUSURA INTERNE
747200941228     C                   Z-ADD     W98HHI        W98
747300941228     C                   EXSR      W98999
747400941228     C                   Z-ADD     D98           D98HHI
747500930423     C* ORE X TURNO CHIUSURA CLIENTE
747600941228     C                   Z-ADD     W98HHC        W98
747700941228     C                   EXSR      W98999
747800941228     C                   Z-ADD     D98           D98HHC
747900060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
748000060418     C                   Z-ADD     W98EGI        W98
748100060418     C                   EXSR      W98999
748200060418     C                   Z-ADD     D98           D98hEGI
748300060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
748400060418     C                   Z-ADD     W98EGC        W98
748500060418     C                   EXSR      W98999
748600060418     C                   Z-ADD     D98           D98hEGC
748700060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
748800060420     C                   Z-ADD     W98EAI        W98
748900060420     C                   EXSR      W98999
749000060420     C                   Z-ADD     D98           D98hEAI
749100060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
749200060420     C                   Z-ADD     W98EAC        W98
749300060420     C                   EXSR      W98999
749400060420     C                   Z-ADD     D98           D98hEAC
749500930423     C* ORE X LASC. AVVISO INTERNE
749600941228     C                   Z-ADD     W98HLI        W98
749700941228     C                   EXSR      W98999
749800941228     C                   Z-ADD     D98           D98HLI
749900930423     C* ORE X LASC. AVVISO CLIENTE
750000941228     C                   Z-ADD     W98HLC        W98
750100941228     C                   EXSR      W98999
750200941228     C                   Z-ADD     D98           D98HLC
750300930423     C* ORE X GIACENZA INTERNE
750400941228     C                   Z-ADD     W98HGI        W98
750500941228     C                   EXSR      W98999
750600941228     C                   Z-ADD     D98           D98HGI
750700930423     C* ORE X GIACENZA CLIENTE
750800941228     C                   Z-ADD     W98HGC        W98
750900941228     C                   EXSR      W98999
751000941228     C                   Z-ADD     D98           D98HGC
751100941228     C* ORE X FEST. PART. INTERNE
751200941228     C                   Z-ADD     W98HPI        W98
751300941228     C                   EXSR      W98999
751400941228     C                   Z-ADD     D98           D98HPI
751500941228     C* ORE X FEST. PART. CLIENTE
751600941228     C                   Z-ADD     W98HPC        W98
751700941228     C                   EXSR      W98999
751800941228     C                   Z-ADD     D98           D98HPC
751900930423     C* ORE X FEST. TRAZ. INTERNE
752000941228     C                   Z-ADD     W98HFI        W98
752100941228     C                   EXSR      W98999
752200941228     C                   Z-ADD     D98           D98HFI
752300930423     C* ORE X FEST. TRAZ. CLIENTE
752400941228     C                   Z-ADD     W98HFC        W98
752500941228     C                   EXSR      W98999
752600941228     C                   Z-ADD     D98           D98HFC
752700930423     C* ORE X FEST. CONS. INTERNE
752800941228     C                   Z-ADD     W98HEI        W98
752900941228     C                   EXSR      W98999
753000941228     C                   Z-ADD     D98           D98HEI
753100930423     C* ORE X FEST. CONS. CLIENTE
753200941228     C                   Z-ADD     W98HEC        W98
753300941228     C                   EXSR      W98999
753400941228     C                   Z-ADD     D98           D98HEC
753500930511     C* ORE X F.DEPOSITO INTERNE
753600941228     C                   Z-ADD     W98HDI        W98
753700941228     C                   EXSR      W98999
753800941228     C                   Z-ADD     D98           D98HDI
753900930511     C* ORE X F.DEPOSITO CLIENTE
754000941228     C                   Z-ADD     W98HDC        W98
754100941228     C                   EXSR      W98999
754200941228     C                   Z-ADD     D98           D98HDC
754300930511     C* ORE X CONTRASSEGNO INTERNE
754400941228     C                   Z-ADD     W98HOI        W98
754500941228     C                   EXSR      W98999
754600941228     C                   Z-ADD     D98           D98HOI
754700930511     C* ORE X CONTRASSEGNO CLIENTE
754800941228     C                   Z-ADD     W98HOC        W98
754900941228     C                   EXSR      W98999
755000941228     C                   Z-ADD     D98           D98HOC
755100930914     C* ORE DI CONSEGNABILITA' INTERNE
755200941228     C                   Z-ADD     W98OCI        W98
755300941228     C                   EXSR      W98000
755400941228     C                   Z-ADD     D98           D98OCI
755500930914     C* ORE DI CONSEGNABILITA' CLIENTE
755600941228     C                   Z-ADD     W98OCC        W98
755700941228     C                   EXSR      W98000
755800941228     C                   Z-ADD     D98           D98OCC
755900071122     C
756000071122     c* Per i codici bolla M prepagati annullati, azzero tutto
756100071122     c                   if        d98cbo='M '
756200071122     c                   clear                   d98rpi
756300071122     c                   clear                   d98rpc
756400071122     c                   clear                   d98rti
756500071122     c                   clear                   d98rtc
756600071122     c                   clear                   d98rci
756700071122     c                   clear                   d98rcc
756800071122     c                   clear                   d98rri
756900071122     c                   clear                   d98rrc
757000071122     c                   clear                   d98fpi
757100071122     c                   clear                   d98fpc
757200071122     c                   clear                   d98fti
757300071122     c                   clear                   d98ftc
757400071122     c                   clear                   d98fci
757500071122     c                   clear                   d98fcc
757600071122     c                   clear                   d98fri
757700071122     c                   clear                   d98frc
757800071122     c                   clear                   d98npi
757900071122     c                   clear                   d98npc
758000071122     c                   clear                   d98nti
758100071122     c                   clear                   d98ntc
758200071122     c                   clear                   d98nci
758300071122     c                   clear                   d98ncc
758400071122     c                   clear                   d98nri
758500071122     c                   clear                   d98nrc
758600071122     c                   clear                   d98OCi
758700071122     c                   clear                   d98OCC
758800071122     c                   endif
758900930423     C*
759000100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
759100100308     c*  calcolo
759200100308     c                   eval      d98dcr=s98dcr
759300100308     c                   eval      d98hcr=s98hcr
759400100308     c                   eval      d98tcr=s98tcr
759500160405     c
759600160405     c* Aggiungo 24 ore ai tempi di consegna se Vedi pacco ( lo faccio alla fine
759700160405     c*  perchè il pgm usa il campo verificado se =10 )
759800160405     c                   if        wVP='S'
759900160405     c                   add       24            d98tci
760000160405     c                   add       24            d98tcc
760100160405     c                   add       24            d98tri
760200160405     c                   add       24            d98trc
760300160405     c                   endif
760400100308     c
760500930423     C                   ENDSR
760600941228     C***********************************************************************
760700941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
760800941228     C***********************************************************************
760900941228     C     W98999        BEGSR
761000941228     C     W98           IFLT      -999
761100941228     C                   Z-ADD     -999          D98               3 0
761200941228     C                   ELSE
761300941228     C     W98           IFGT      999
761400941228     C                   Z-ADD     999           D98
761500941228     C                   ELSE
761600941228     C                   Z-ADD     W98           D98
761700941228     C                   END
761800941228     C                   END
761900941228     C                   ENDSR
762000941228     C***********************************************************************
762100941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
762200941228     C***********************************************************************
762300941228     C     W98000        BEGSR
762400941228     C     W98           IFLE      *ZEROS
762500941228     C                   Z-ADD     *ZEROS        D98
762600941228     C                   ELSE
762700941228     C     W98           IFGT      999
762800941228     C                   Z-ADD     999           D98
762900941228     C                   ELSE
763000941228     C                   Z-ADD     W98           D98
763100941228     C                   END
763200941228     C                   END
763300941228     C                   ENDSR
763400930305     C***********************************************************************
763500930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
763600930305     C***********************************************************************
763700930305     C     AZZERA        BEGSR
763800150219     c* data e ora del momento
763900150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
764000150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
764100150219     C                   Z-ADD     DTORA4        WHHODI            4 0
764200930305     C*
764300141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
764400150603     C                   MOVEL     'N'           opendan
764500141107     c                   clear                   or_ddc
764600141108     c                   clear                   sav_12dat         8 0
764700141107     c                   clear                   or_datarif
764800140402     c                   clear                   wpick2            1
764900140402     c                   clear                   Primotipo
765000090928     c                   clear                   Primodev
765100090928     c                   clear                   Primohev
765200070827     c                   clear                   CallAR5
765300071122     c                   clear                   d98cbo
765400100308     c                   clear                   s98dcr
765500100308     c                   clear                   s98hcr
765600100308     c                   clear                   s98tcr
765700141106     c                   clear                   nosomma           1
765800141229     c                   clear                   wtrasm
765900150324     c                   clear                   avvcmc
766000150325     c                   clear                   DSbl4L
766100160401     c                   clear                   dsbl4I
766200150401     c                   clear                   fnlv82ds
766300150331     c                   clear                   DS7R
766400150605     c                   clear                   wDEE              1
766500150629     c                   clear                   wtsogliaE
766600930323     C                   MOVEL     '0'           D98ERR
766700930323     C                   MOVEL     *BLANKS       D98MSG
766800931124     C                   Z-ADD     *ZEROS        D98TPI
766900931124     C                   Z-ADD     *ZEROS        D98TPC
767000930325     C                   Z-ADD     *ZEROS        D98TTI
767100930325     C                   Z-ADD     *ZEROS        D98TTC
767200930325     C                   Z-ADD     *ZEROS        D98TCI
767300930325     C                   Z-ADD     *ZEROS        D98TCC
767400090827     C                   Z-ADD     *ZEROS        w99TCI
767500090827     C                   Z-ADD     *ZEROS        w99TCC
767600930325     C                   Z-ADD     *ZEROS        D98TRI
767700930325     C                   Z-ADD     *ZEROS        D98TRC
767800090921     C                   Z-ADD     *ZEROS        w99TRI
767900090921     C                   Z-ADD     *ZEROS        w99TRC
768000931124     C                   Z-ADD     *ZEROS        D98RPI
768100931124     C                   Z-ADD     *ZEROS        D98RPC
768200930405     C                   Z-ADD     *ZEROS        D98RTI
768300930405     C                   Z-ADD     *ZEROS        D98RTC
768400930405     C                   Z-ADD     *ZEROS        D98RCI
768500930405     C                   Z-ADD     *ZEROS        D98RCC
768600930405     C                   Z-ADD     *ZEROS        D98RRI
768700930405     C                   Z-ADD     *ZEROS        D98RRC
768800931124     C                   Z-ADD     *ZEROS        D98FPI
768900931124     C                   Z-ADD     *ZEROS        D98FPC
769000930325     C                   Z-ADD     *ZEROS        D98FTI
769100930325     C                   Z-ADD     *ZEROS        D98FTC
769200930325     C                   Z-ADD     *ZEROS        D98FCI
769300930325     C                   Z-ADD     *ZEROS        D98FCC
769400930325     C                   Z-ADD     *ZEROS        D98FRI
769500930325     C                   Z-ADD     *ZEROS        D98FRC
769600931124     C                   MOVEL     *BLANKS       D98CPI
769700931124     C                   MOVEL     *BLANKS       D98CPC
769800930325     C                   MOVEL     *BLANKS       D98CTI
769900930325     C                   MOVEL     *BLANKS       D98CTC
770000930325     C                   MOVEL     *BLANKS       D98CCI
770100930325     C                   MOVEL     *BLANKS       D98CCC
770200930325     C                   MOVEL     *BLANKS       D98CRI
770300930325     C                   MOVEL     *BLANKS       D98CRC
770400931124     C                   Z-ADD     *ZEROS        D98NPI
770500931124     C                   Z-ADD     *ZEROS        D98NPC
770600930325     C                   Z-ADD     *ZEROS        D98NTI
770700930325     C                   Z-ADD     *ZEROS        D98NTC
770800930325     C                   Z-ADD     *ZEROS        D98NCI
770900930325     C                   Z-ADD     *ZEROS        D98NCC
771000930325     C                   Z-ADD     *ZEROS        D98NRI
771100930325     C                   Z-ADD     *ZEROS        D98NRC
771200930414     C                   Z-ADD     *ZEROS        D98HHI
771300930414     C                   Z-ADD     *ZEROS        D98HHC
771400930414     C                   Z-ADD     *ZEROS        D98HOI
771500930414     C                   Z-ADD     *ZEROS        D98HOC
771600930414     C                   Z-ADD     *ZEROS        D98HLI
771700930414     C                   Z-ADD     *ZEROS        D98HLC
771800930414     C                   Z-ADD     *ZEROS        D98HGI
771900930414     C                   Z-ADD     *ZEROS        D98HGC
772000931124     C                   Z-ADD     *ZEROS        D98HPI
772100931124     C                   Z-ADD     *ZEROS        D98HPC
772200930414     C                   Z-ADD     *ZEROS        D98HFI
772300930414     C                   Z-ADD     *ZEROS        D98HFC
772400930416     C                   Z-ADD     *ZEROS        D98HEI
772500930416     C                   Z-ADD     *ZEROS        D98HEC
772600930414     C                   Z-ADD     *ZEROS        D98HRI
772700930414     C                   Z-ADD     *ZEROS        D98HRC
772800930511     C                   Z-ADD     *ZEROS        D98HDI
772900930511     C                   Z-ADD     *ZEROS        D98HDC
773000930325     C                   Z-ADD     *ZEROS        D98DCI
773100930325     C                   Z-ADD     *ZEROS        D98HCI
773200930325     C                   Z-ADD     *ZEROS        D98DCE
773300930325     C                   Z-ADD     *ZEROS        D98HCE
773400931125     C                   Z-ADD     *ZEROS        D98OCI
773500931125     C                   Z-ADD     *ZEROS        D98OCC
773600931202     C                   Z-ADD     *ZEROS        D98DEI
773700931202     C                   Z-ADD     *ZEROS        D98OEI
773800931202     C                   Z-ADD     *ZEROS        D98DEE
773900931202     C                   Z-ADD     *ZEROS        D98OEE
774000151019     c                   CLEAR                   D98FLP
774100161107     c                   CLEAR                   D98Gfas
774200150325     C                   clear                   D98dtctd
774300150326     C                   clear                   D98picklim
774400150325     C                   clear                   D98spcdee
774500150331     C                   clear                   fddata
774600140825     c
774700141017     c                   clear                   d98ddc
774800141017     c                   clear                   d98cons
774900160506     c                   clear                   d98disf
775000141017     c                   clear                   wd98ndc
775100140910     c                   clear                   wd98fbc
775200150119     c                   clear                   wd98acc
775300140825     c                   clear                   wtrulor
775400160401     c                   clear                   wVP               1
775500150219     c
775600150219     c                   clear                   IMPdata
775700150219     c                   clear                   IMPfgs
775800150219     c                   clear                   DISdata
775900150219     c                   clear                   DISfgs
776000150219     c                   clear                   DISora
776100150219     c                   clear                   DISnpg
776200150219     c                   clear                   IDnpg
776300150219     c                   clear                   IDdata
776400150219     c                   clear                   IDora
776500150219     c                   clear                   IDfgs
776600931125     C* CAMPI VARI
776700931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
776800140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
776900140825      *   DALLA LETTURA DI QUESTI FILE                               *
777000931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
777100931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
777200931125     C     D98NSP        IFNE      *ZEROS
777300930525     C                   MOVEL     *BLANKS       D98TSP
777400941228     C                   MOVEL     *BLANKS       D98CAD
777500970728     C                   MOVEL     *BLANKS       D98LOD
777600930525     C                   Z-ADD     *ZEROS        D98TFP
777700930525     C                   Z-ADD     *ZEROS        D98TFA
777800930525     C                   Z-ADD     *ZEROS        D98LNA
777900930525     C                   Z-ADD     *ZEROS        D98DSP
778000941228     C                   Z-ADD     *ZEROS        D98MGS
778100930525     C                   Z-ADD     *ZEROS        D98DTI
778200930525     C                   Z-ADD     *ZEROS        D98HTI
778300930525     C                   Z-ADD     *ZEROS        D98DCM
778400930525     C                   Z-ADD     *ZEROS        D98HMC
778500930525     C                   MOVEL     *BLANKS       D98GC1
778600930525     C                   MOVEL     *BLANKS       D98GC2
778700010820     C                   Z-ADD     *ZEROS        D98CAS
778800010821     C                   MOVEL     *BLANKS       D98VCA
778900930525     C                   MOVEL     *BLANKS       D98TIC
779000930525     C                   Z-ADD     *ZEROS        D98DCR
779100930525     C                   Z-ADD     *ZEROS        D98HCR
779200930525     C                   MOVEL     *BLANKS       D98TCR
779300050914     C                   MOVEL     *BLANKS       Consideraora      1
779400930525     C                   MOVEL     *BLANKS       D98CMC
779500930525     C                   Z-ADD     *ZEROS        D98DAG
779600161215     C                   Z-ADD     *ZEROS        D98DCc
779700161215     C                   Z-ADD     *ZEROS        D98DAc
779800161215     C                   Z-ADD     *ZEROS        D98DCG
779900930525     C                   MOVEL     *BLANKS       D98FFD
780000930525     C                   Z-ADD     *ZEROS        D98NCD
780100930525     C                   MOVEL     *BLANKS       D98FNS
780200931124     C                   Z-ADD     *ZEROS        D98DAM
780300170105     C                   Z-ADD     *ZEROS        D98Dfv
780400940628     C                   MOVEL     *BLANKS       D98FIN
780500940628     C                   Z-ADD     *ZEROS        D98KSC
780600940628     C                   MOVEL     *BLANKS       D98TBL
780700931125     C                   END
780800960229     C     D98TLA        IFEQ      'T'
780900960229     C                   Z-ADD     *ZEROS        D98DTI
781000960229     C                   Z-ADD     *ZEROS        D98HTI
781100960229     C                   ENDIF
781200931125     C* CAMPI WORK
781300930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
781400930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
781500150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
781600930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
781700941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
781800960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
781900930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
782000930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
782100940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
782200930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
782300150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
782400931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
782500931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
782600150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
782700930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
782800930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
782900930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
783000930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
783100930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
783200940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
783300941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
783400930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
783500050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
783600050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
783700140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
783800930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
783900930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
784000930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
784100930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
784200930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
784300930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
784400930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
784500930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
784600930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
784700930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
784800930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
784900930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
785000930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
785100930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
785200140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
785300931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
785400060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
785500960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
785600931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
785700140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
785800140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
785900150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
786000140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
786100941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
786200970304     C******************** Z-ADD*ZEROS    W98DBR  80
786300970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
786400150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
786500050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
786600930414     C*
786700941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
786800941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
786900930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
787000930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
787100930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
787200930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
787300930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
787400930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
787500941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
787600941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
787700930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
787800930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
787900930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
788000930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
788100930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
788200930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
788300941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
788400941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
788500930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
788600930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
788700930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
788800930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
788900930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
789000930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
789100930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
789200930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
789300060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
789400060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
789500060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
789600060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
789700930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
789800930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
789900930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
790000930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
790100930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
790200930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
790300930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
790400930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
790500930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
790600930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
790700930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
790800930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
790900930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
791000930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
791100941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
791200941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
791300930423     C*
791400930511     C                   Z-ADD     *ZEROS        W98HF             5 0
791500930423     C                   Z-ADD     *ZEROS        W98HH             5 0
791600060418     C                   Z-ADD     *ZEROS        W98EG             5 0
791700060420     C                   Z-ADD     *ZEROS        W98EA             5 0
791800930423     C                   Z-ADD     *ZEROS        W98HG             5 0
791900930423     C                   Z-ADD     *ZEROS        W98HL             5 0
792000930423     C                   Z-ADD     *ZEROS        W98HO             5 0
792100930511     C                   Z-ADD     *ZEROS        W98HD             5 0
792200930611     C*
792300930611     C                   Z-ADD     *ZEROS        WORELA            3 0
792400960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
792500930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
792600930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
792700931207     C                   Z-ADD     *ZEROS        WORECR            3 0
792800090902     c
792900090902     c                   clear                   wmancaTRAZ
793000150113     c                   clear                   wcaldpcee         1
793100150220     c                   clear                   evemic            2
793200930305     C*
793300060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
793400060406     C                   clear                   LAF                            *LASC.AVVISO FINE
793500091014     C                   clear                   LAp                            *posizione l.avviso
793600060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
793700060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
793800060406c    c                   clear                   LCon
793900060406     C                   clear                   EVD                            *DATA EVENTO
794000060406     C                   clear                   EVO                            *ORA EVENTO
794100060406     C                   clear                   EVC                            *CAUSALE EVENTO
794200131218     C                   clear                   EVT
794300060406     C                   clear                   pVD                            *DATA EVENTO
794400060406     C                   clear                   pVO                            *ORA EVENTO
794500060406     C                   clear                   pVC                            *CAUSALE EVENTO
794600060406     C                   clear                   pVF                            *CAUSALE EVENTO
794700060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
794800060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
794900060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
795000060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
795100060406     C                   clear                   COV                            *DATE FINE L.A. E GI
795200060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
795300060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
795400091014     C                   clear                   GIp                            *posizione vento gia
795500060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
795600060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
795700060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
795800931213     C*
795900060406     C                   clear                   TUC                            *TURNI CHIUSURA
796000931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
796100931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
796200060406     c
796300060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
796400060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
796500060406     c                   clear                   pgii
796600060406     c                   clear                   pgif
796700060406     c                   clear                   pgci
796800060406     c                   clear                   pgcf
796900060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
797000060420     c                   clear                   paii
797100060420     c                   clear                   paif
797200930420     C*
797300930305     C                   ENDSR
797400930512     C***********************************************************************
797500930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
797600930512     C***********************************************************************
797700930512     C     FORCON        BEGSR
797800930512     C*
797900931124     C                   Z-ADD     *ZEROS        D98RPI
798000931124     C                   Z-ADD     *ZEROS        D98RPC
798100930512     C                   Z-ADD     *ZEROS        D98RTI
798200930512     C                   Z-ADD     *ZEROS        D98RTC
798300930512     C                   Z-ADD     *ZEROS        D98RCI
798400930512     C                   Z-ADD     *ZEROS        D98RCC
798500930512     C                   Z-ADD     *ZEROS        D98RRI
798600930512     C                   Z-ADD     *ZEROS        D98RRC
798700931124     C                   Z-ADD     *ZEROS        D98FPI
798800931124     C                   Z-ADD     *ZEROS        D98FPC
798900930512     C                   Z-ADD     *ZEROS        D98FTI
799000930512     C                   Z-ADD     *ZEROS        D98FTC
799100930512     C                   Z-ADD     *ZEROS        D98FCI
799200930512     C                   Z-ADD     *ZEROS        D98FCC
799300930512     C                   Z-ADD     *ZEROS        D98FRI
799400930512     C                   Z-ADD     *ZEROS        D98FRC
799500931124     C                   MOVEL     'S'           D98CPI
799600931124     C                   MOVEL     'S'           D98CPC
799700930512     C                   MOVEL     'S'           D98CTI
799800930512     C                   MOVEL     'S'           D98CTC
799900930512     C                   MOVEL     'S'           D98CCI
800000930512     C                   MOVEL     'S'           D98CCC
800100930512     C                   MOVEL     'S'           D98CRI
800200930512     C                   MOVEL     'S'           D98CRC
800300931124     C                   Z-ADD     *ZEROS        D98NPI
800400931124     C                   Z-ADD     *ZEROS        D98NPC
800500930512     C                   Z-ADD     *ZEROS        D98NTI
800600930512     C                   Z-ADD     *ZEROS        D98NTC
800700930512     C                   Z-ADD     *ZEROS        D98NCI
800800930512     C                   Z-ADD     *ZEROS        D98NCC
800900930512     C                   Z-ADD     *ZEROS        D98NRI
801000930512     C                   Z-ADD     *ZEROS        D98NRC
801100930512     C                   Z-ADD     *ZEROS        D98HHI
801200930512     C                   Z-ADD     *ZEROS        D98HHC
801300930512     C                   Z-ADD     *ZEROS        D98HOI
801400930512     C                   Z-ADD     *ZEROS        D98HOC
801500930512     C                   Z-ADD     *ZEROS        D98HLI
801600930512     C                   Z-ADD     *ZEROS        D98HLC
801700930512     C                   Z-ADD     *ZEROS        D98HGI
801800930512     C                   Z-ADD     *ZEROS        D98HGC
801900931124     C                   Z-ADD     *ZEROS        D98HPI
802000931124     C                   Z-ADD     *ZEROS        D98HPC
802100930512     C                   Z-ADD     *ZEROS        D98HFI
802200930512     C                   Z-ADD     *ZEROS        D98HFC
802300930512     C                   Z-ADD     *ZEROS        D98HEI
802400930512     C                   Z-ADD     *ZEROS        D98HEC
802500930512     C                   Z-ADD     *ZEROS        D98HRI
802600930512     C                   Z-ADD     *ZEROS        D98HRC
802700930512     C                   Z-ADD     *ZEROS        D98HDI
802800930512     C                   Z-ADD     *ZEROS        D98HDC
802900930520     C*
803000930512     C                   ENDSR
803100930305     C********************************************************************
803200971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
803300950406031059090120151181212243273304334365
803400950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
803500950406312831303130313130313031
803600930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
803700971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
803800971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
803900971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
804000971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
804100971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
804200971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
804300971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
804400971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
804500971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
804600971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
804700971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
804800971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
804900971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
805000971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
805100971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
805200971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
805300971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
805400971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
805500971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
805600971127TNSD99R- Bolla inesistente.                                               BOIN20
805700971127TNSD99R-     Cap inesistente.                                             CAPI21
805800971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
805900971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
806000971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
806100971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
806200971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
806300971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
806400971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
806500971127TNSD99R- Contrassegno non trovato.                                        CONT29
806600971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
806700990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
806800990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
806900060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
807000060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
807100971127TNSD99R- 35                                                                   35
807200971127TNSD99R- 36                                                                   36
807300971127TNSD99R- 37                                                                   37
807400971127TNSD99R- 38                                                                   38
807500971127TNSD99R- 39                                                                   39
807600971127TNSD99R- 40                                                                   40
