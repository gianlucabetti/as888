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
016200020226     D* SCHIERA FILIALI FEDEX
016300060406     D FILFED          S              3  0 DIM(99)
016400970701     D*****************************************************************
016500970701     D* PARAMETRI CHIAMATA PGM TISI95R (CONTROLLO CAP)
016600970701     D SI95          E DS                  EXTNAME(TISI95DS)
016700930325     D*****************************************************************
016800930405     D* DS PER RIDEFINIRE SCHIERA GIORNI SETTIMANA
016900930405     D GSEDS           DS
017000930405     D  GSE                    1     31
017100930405     D                                     DIM(31)
017200930405     D*****************************************************************
017300930405     D* DS PER RIDEFINIRE SCHIERA GIORNI FESTIVI
017400930405     D GFEDS           DS
017500930405     D  GFE                    1     31
017600930405     D                                     DIM(31)
017700150325     D*****************************************************************
017800150325     D* DS per record L del FIAR4
017900150325     D DSbl4L        E DS
018000930323     D*****************************************************************
018100930323     D* DS ESTERNA IN INPUT AL PGM
018200970731     D DS99          E DS                  EXTNAME(TNSD99DS)
018300930323     D*****************************************************************
018400930323     D* DS ESTERNA TABELLA 1X TEMPI FISSI DI DELIVERY
018500930326     D DS1X          E DS
018600930405     D*****************************************************************
018700971113     D* DS ESTERNA TABELLA 1Y SIMBOLI CALENDARIO
018800930405     D DS1Y          E DS
018900941227     D*****************************************************************
019000941227     D* DS ESTERNA TABELLA 3A (CODICI BOLLA)
019100941227     D DS3A          E DS
019200930421     D*****************************************************************
019300060406     D* DS ESTERNA TABELLA 2A CAUSALI EVENTI BOLLE
019400060406     D DS2A          E DS                  prefix(W:1)
019500020226     D*****************************************************************
019600020226     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
019700020226     D OG143         E DS
019800020702     D*****************************************************************
019900020702     D* DS PER LETTURA TABELLA "NTW"
020000020702     D DNTW          E DS
020100150326     D* DS PER LETTURA TABELLA "VPO" flag operativi
020200150326     D Dvpo          E DS
020300031201     D*****************************************************************
020400150331     D* DS ESTERNA TABELLA 7Q/7W/7R
020500031201     D DS7Q          E DS
020600031201     D DS7W          E DS
020700150331     D DS7R          E DS
020800070514     D*****************************************************************
020900140825     D* DS per reperimento orari di consegna
021000140825     D TRULORSDS     E DS
021100140825     D TRULOR2DS     E DS
021200150331     D TRULOR3DS     E DS
021300140825     D*****************************************************************
021400150219     D FNLV53DS      E DS
021500150219     D FNLV55DS      E DS
021600150401     D*****************************************************************
021700150401     D fnlv82ds      E DS
021800150219     D*****************************************************************
021900060406     D usa2a           DS
022000060406     D  §2aftc                 1      1
022100060406     D  §2agla                 2      2  0
022200060406     D  §2aFTG                 3      3
022300060406     D usa2Z           DS
022400060406     D  §2Zcep                 1      1
022500060406     D  §2Znep                 2      3
022600060406     D  §2Zdep                 4      4
022700060406     D  §2Zfep                 5      5
022800930517     D***************************************************************
022900971113     D* DS PER SCOMPORRE CHIAVE CALENDARIO
023000930517     D                 DS
023100941227     D  DCATFP                 1      3
023200941227     D  DCATFA                 4      6
023300941227     D  DCAANN                 7     10
023400941227     D  DCAMES                11     12
023500941227     D  DCALEN                 1     12
023600950406     D***************************************************************
023700950406     D* DS PER SCOMPORRE WDAT IN AAAA/MM/GG
023800950406     D                 DS
023900950406     D  WDAT                   1      8  0
024000950406     D  WAA                    1      4  0
024100950406     D  WMM                    5      6  0
024200950406     D  WGG                    7      8  0
024300970304     D***************************************************************
024400970304     D* DS PER CONFRONTI TRA DATE
024500970304     D                 DS
024600970304     D  WDO1                   1     12  0
024700970304     D  WD1                    1      8  0
024800970304     D  WO1                    9     12  0
024900970304     D                 DS
025000970304     D  WDO2                   1     12  0
025100970304     D  WD2                    1      8  0
025200970304     D  WO2                    9     12  0
025300150330     D***************************************************************
025400150330     D                 DS
025500150330     D  who                    1      2  0
025600150330     D  wmn                    3      4  0
025700150330     D  WORA                   1      4  0
025800941227     D***************************************************************
025900941227     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
026000941227     D DS§DAT          DS
026100941227     D  DSSEC                  1      2  0
026200941227     D  DSANN                  3      4  0
026300941227     D  DSMES                  5      6  0
026400941227     D  DSGIO                  7      8  0
026500941227     D  DSAAS                  1      4  0
026600941228     D  DSMGS                  5      8  0
026700941227     D  DSDAT6                 3      8  0
026800941227     D  DSDAT8                 1      8  0
026900941227     D DS§TIM          DS
027000941227     D  DTORA                  1      2  0
027100941227     D  DTMIN                  3      4  0
027200941227     D  DTSCN                  5      6  0
027300941227     D  DTGIO                  7      8  0
027400941227     D  DTMES                  9     10  0
027500941227     D  DTSEC                 11     12  0
027600941227     D  DTANN                 13     14  0
027700941227     D  DTINV                  7     14  0
027800941227     D  DTORA6                 1      6  0
027900941227     D  DTORA4                 1      4  0
028000941227     D  DTTIME                 1     14  0
028100010821     D***************************************************************
028200050523     D kpjba         E DS
028300010821     D*  CAMBIO DIVISA
028400010821     D YEURDS        E DS                  EXTNAME(YEURCODS)
028500010821     D***************************************************************
028600010821     D*  REPERIMENTO DIVISE DI CONTO
028700010821     D DGED          E DS
028800070827     D*------------------
028900070827     D DAR5GEN       E DS
029000011211     D*------------------
029100011211     D* DS REPERIMENTO IMPORTO MASSIMO CONTRASSEGNO
029200011211     D*------------------
029300011211     D TRUL21DS      E DS
029400050523     d
029500050523     d* DS x controllo riempimento schiera
029600050523     D TRUL0sDS      E DS
029700040809     D*------------------
029800040809     D* ds per il controllo della presenza di CA per la spedizione richiesta
029900040809     D*------------------
030000040809     d FIDN12DS      e ds
030100040809     d  skKey                 26    305    dim(20)
030200040809     d  skAnn                306    325    dim(20)
030300040809     d  skDca                326    485  0 dim(20)
030400040809     d  skFca                486    545  0 dim(20)
030500040809     d  skDch                546    705  0 dim(20)
030600040809     d  skCch                706    745    dim(20)
030700040809     d*
030800040809     d dsKey           ds
030900040809     d  dsaac                         4  0
031000040809     d  dsfil                         3  0
031100040809     d  dsnca                         7  0
031200041028     d wesarb          s              1
031300141216     d wtrasm          s              1
031400041116     d wapri           s              1    inz(' ')
031500050908     d Usacons         s              1    inz(' ')
031600050405     D*------------------
031700050405     D* VARIABILI D WRK RIFERITE AL DATA BASE
031800050405     D*------------------
031900050405     D KWGCPAAS        s                   like(GCPAAS)
032000050405     D KWGCPLNP        s                   like(GCPLNP)
032100050405     D KWGCPNRS        s                   like(GCPNRS)
032200050405     D KWGCPNSP        s                   like(GCPNSP)
032300050405     D KWGCPFRG        s                   like(GCPFRG)
032400050901     d
032500050901     D salvaoreHMC     s                   like(wr3hho)
032600050901     D salvaoreHCR     s                   like(wr3hho)
032700050907     D oreILoDOPO      s                   like(worecr)
032800060526     D wpodata         s                   like(w12dat)
032900070514     D wdata8          s                   like(w12dat)
033000060406     D w003a           s              3
033100060406     D w11poi          s              9  0
033200150217     D picklim         s              4  0
033300060420     D wgiroepa        s              1
033400140911     D wtuctut         s              1    inz
033500060420     D wfine           s              1
033600070827     D CallAR5         s              1    inz
033700140825     D Wtrulor         s              1    inz
033800070827     d kAr5Trd         s                   Like(Ar5Trd) inz('GEN')
033900150325     d kAr4Trc         s                   Like(Ar4Trc) inz('L')
034000071122     d d98cbo          s                   Like(arbcbo)
034100090827     d w99tci          s                   Like(d98tci)
034200090827     d w99tcc          s                   Like(d98tcc)
034300090921     d w99tri          s                   Like(d98tri)
034400090921     d w99trc          s                   Like(d98trc)
034500090928     d Primotipo       s                   Like(§2aftc)
034600090928     d Primodev        s                   Like(evbdev)
034700090928     d Primohev        s                   Like(evbhev)
034800091014     d SalvaEveL       s              3  0
034900091014     d SalvaEveG       s              3  0
035000140911     d EX              s              3  0
035100150325     d II              s              3  0
035200150630     d wdiff           s              9  0
035300100308     d s98dcr          s                   Like(d98dcr)
035400100308     d s98hcr          s                   Like(d98hcr)
035500100308     d s98tcr          s                   Like(d98tcr)
035600140825     d wd98ndc         s                   Like(arbndc)
035700140911     d wd98fbc         s                   Like(arbfbc)
035800150119     d wd98acc         s                   Like(arbacc)
035900150219     d
036000150219     d IMPdata         s                   like(d53dfv)
036100150219     d IMPfgs          s                   like(brvfgs)
036200150219     d DISfgs          s                   like(brvfgs)
036300150219     d DISdata         s                   like(d53dfv)
036400150219     d DISnpg          s                   like(brvnpg)
036500150219     d DISora          s                   like(brvhcs)
036600150219     d IDdata          s                   like(d53dfv)
036700150219     d IDora           s                   like(brvhcs)
036800150219     d IDfgs           s                   like(brvfgs)
036900150219     d IDnpg           s              1
037000150325     D kpdpdr          S                   LIKE(APDpdr)
037100150325     D kpdtip          S                   LIKE(APDtip)
037200140825     d
037300140825     D wrkoggiiso      s               d
037400930304     C*****************************************************************
037500930323     C*/ INIZIO MAIN DEL PGM
037600930304     C* ESEGUE ROUTINE INIZIALE
037700930304     C                   EXSR      RUTINZ
037800930528     C*
037900930528     C     D98ERR        IFEQ      '0'                                          --- 1 -->
038000930602     C*
038100930602     C* SE LANCIATO CON TIPO LANCIO='C' (CHIUDI FILE) VADO A FINE PGM
038200930602     C     D98TLA        IFNE      'C'                                          --- 2 -->
038300960729     C*
038400960729     C* CONTROLLO SE MI HANNO LANCIATO PER DETERMINARE LA CONSEGNABILITA'
038500960729     C                   MOVEL     'N'           FLCONS            1            *FLAG ESISTE CONSEGN
038600960729     C     D98DIC        IFGT      *ZEROS
038700960729     C     D98DFC        ORGT      *ZEROS
038800960729     C                   MOVEL     'S'           FLCONS                         *SI ESISTE CONSEGNAB
038900960729     C                   ENDIF
039000930323     C*
039100930511     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
039200931125     C* SE NON E' STATA IMPOSTATA DS DAL PGM CHIAMANTE (NSP=ZERO)
039300930420     C     D98ERR        IFEQ      '0'
039400010821     C     D98NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
039500930415     C     D98TBO        IFEQ      'A'
039600941227     C                   EXSR      LETARB                                       *LEGGE BOLLA ARRIVO
039700930323     C                   ELSE
039800941227     C                   EXSR      LETBLP                                       *LEGGE BOLLA PART.
039900930323     C                   END
040000140402     c
040100150401     c* Leggo fiar5 recod GEN  e FIAR4
040200140402     c                   exsr      leggiar5
040300140402     c
040400931206     C                   ELSE
040500931206     C* DS IMPOSTATA DAL PGM CHIAMANTE IMPOSTO CAMPI LAVORO
040600931206     C                   Z-ADD     D98DCG        W98DCG
040700990308     C                   MOVEL     *BLANKS       WRBL              1            *RAGGRUPP.BOLLE
040800990308     C                   MOVEL     *BLANKS       WFBR              1            *BOLLA DI RESO
040900031201     C                   MOVEL     *BLANKS       WGGA              2            *PARTICOLARIT' GIAC.
041000140822     C                   Z-ADD     d98DCR        s98DCR
041100140822     C                   Z-ADD     d98HCR        s98HCR
041200140822     C                   MOVEL     d98TCR        s98TCR
041300141104     c                   eval      gc1part=d98gc1
041400141104     c                   eval      gc2part=d98gc2
041500150303     c* imposto flag trasmissione bolla = 'N'
041600150303     c                   eval      wtrasm='N'
041700931125     C                   END
041800930420     C                   END
041900930528     C*
042000010821     C     D98ERR        IFEQ      '0'                                          --- 3 -->
042100050908     c* Se esiste la data consegnabilità, faccio finta che la bolla
042200050908     c*  non si consegnata, azzerando proprio la dta di consegna
042300050908     c*  perchè può trarre in inganno
042400050908     c                   if        d98dfc>0 and d98dcm>0
042500050908     c                   clear                   d98dcm
042600050908     c                   clear                   d98hmc
042700050908     c                   clear                   d98cca
042800050908     c                   endif
042900090902     c*
043000090902     c* Se non c'e' nè data consegna, ne data arrivo trazione
043100090902     c*  faccio finta che data arrivo trazione= udate utime ai fini dei calcoli
043200090902     c* in fondo al pgm dovrò azzerare i tempi di consegna interni e cliente
043300090902     c* perchè non calcolabili
043400090902     c                   clear                   wmancaTRAZ        1
043500090902     c
043600010821     C*
043700010821     C* EFFETTUO SUBITO CONSIDERAZIONI SU DIVISE (X QUESTIONE EURO)
043800010821     C                   EXSR      CHKVCA
043900141204     c
044000141204     c* Controllo se ha pratica di DANNO aperta
044100141204     c                   EXSR      CHKDAN
044200010108     C*
044300010108     C* SE BOLLA POSTA, UTILIZZA LA DATA BORDERO' ANZICHE' LA DATA SPEDIZIONE
044400091009     C**   D98TSP        IFEQ      'P'                                          *BOLLA POSTA
044500091009     C**   D98DBR        IFGT      *ZEROS                                       *DATA BORDERO' PRESE
044600091009     C**                 Z-ADD     D98DBR        D98DSP                         *DATA BORDERO' --> D
044700091009     C**                 ENDIF
044800091009     C**                 ENDIF
044900930304     C*
045000950110     C* RECUPERO I TEMPI TEORICI DI PARTENZA, TRAZIONE E DI CONSEGNA
045100930420     C     D98ERR        IFEQ      '0'
045200930323     C                   EXSR      RTTEOR
045300930420     C                   END
045400930528     C*
045500930602     C     D98ERR        IFEQ      '0'                                          --- 4 -->
045600970304     C*
045700970304     C* SE RICHIESTO LANCIO CON DATA RIFERIMENTO CONTROLLO LE DATE DA AZZERARE
045800970304     C     D98ERR        IFEQ      '0'
045900970304     C                   EXSR      DATRIF
046000970304     C                   END
046100940901     C*
046200940901     C* TRASF. DATA ARRIVO TRAZIONE E RICONSIDERO TEMPO CONSEGNA SE RITARDO
046300940901     C     D98ERR        IFEQ      '0'
046400940901     C                   EXSR      TDTIPO
046500940901     C                   END
046600930420     C*
046700930511     C* CARICO IL FILE DEGLI EVENTI BOLLA
046800930420     C     D98ERR        IFEQ      '0'
046900041116     c     d98nsp        andgt     0
047000930420     C                   EXSR      CAREVB
047100930420     C                   END
047200931213     C*
047300931213     C* CARICO LA SCHIERA DEI TURNI DI CHIUSURA
047400931213     C     D98ERR        IFEQ      '0'
047500931213     C                   EXSR      CARTUC
047600931213     C                   END
047700930528     C*
047800930602     C     D98ERR        IFEQ      '0'                                          --- 5 -->
047900930405     C*
048000940901     C* TRASFORMO LE DATE (SPED,CONS,ECC.) IN PROGRESSIVI ORE
048100930420     C     D98ERR        IFEQ      '0'
048200930405     C                   EXSR      TDATPO
048300930420     C                   END
048400930528     C*
048500930602     C     D98ERR        IFEQ      '0'                                          --- 6 -->
048600950110     C*
048700950110     C* CALCOLA LE DATE DI PREVISTA PARTENZA DELL'ULTIMO COLLO
048800950110     C     D98ERR        IFEQ      '0'
048900950110     C                   EXSR      CALDPP
049000950110     C                   END
049100930405     C*
049200930511     C* CALCOLA LE DATE DI PREVISTO ARRIVO DELLA TRAZIONE
049300930420     C     D98ERR        IFEQ      '0'
049400930511     C                   EXSR      CALDPT
049500090902     c*
049600090902     c* Ricalcolo le date facendo finta che la trazione stia arrivando
049700090903     c*  in questo momento se non presente
049800090902     c                   if        ricalcoloTRAZ='S'
049900090902     c                   eval      d98dti=WDAODI
050000090902     c                   eval      d98hti=WHHODI
050100090902     c                   eval      wmancaTRAZ='S'
050200090902     c
050300090902     C                   EXSR      TDTIPO
050400090902     C                   EXSR      CALDPP
050500090902     C                   EXSR      CALDPT
050600090902     c                   endif
050700090902     c
050800930420     C                   END
050900960229     C*
051000091009     d* D98TLA = 'T' --> in output solo i tempi di trazione
051100960229     C     D98TLA        IFNE      'T'                                          --- 6A ->
051200060407     C*
051300931206     C* CALCOLA IL RANGE PER IL CONTRASSEGNO
051400931206     C     D98ERR        IFEQ      '0'
051500931206     C                   EXSR      CALASS
051600931206     C                   END
051700931206     C*
051800931206     C* CALCOLA IL RANGE PER IL FERMO DEPOSITO
051900931206     C     D98ERR        IFEQ      '0'
052000931206     C                   EXSR      CALFED
052100931206     C                   END
052200931206     C*
052300931206     C* CALCOLA LE DATE TEORICHE DI CONSEGNA
052400931206     C     D98ERR        IFEQ      '0'
052500931206     C                   EXSR      CALDTE
052600931206     C                   END
052700930511     C*
052800930511     C* CALCOLA IL RANGE PER DATA CONSEGNA RICHIESTA
052900930511     C     D98ERR        IFEQ      '0'
053000930511     C                   EXSR      CALDCR
053100930511     C                   END
053200930511     C*
053300930511     C* CALCOLA I RANGE PER LASCIATO AVVISO
053400930511     C     D98ERR        IFEQ      '0'
053500930511     C                   EXSR      CALLAV
053600930511     C                   END
053700930511     C*
053800930511     C* CALCOLA IL RANGE PER LA GIACENZA
053900930511     C     D98ERR        IFEQ      '0'
054000930511     C                   EXSR      CALGIA
054100930511     C                   END
054200060420     C* CALCOLA IL RANGE PER Eventi Particolari che Allungano
054300060420     C     D98ERR        IFEQ      '0'
054400060420     C                   EXSR      CALcepa
054500060420     C                   END
054600930511     C*
054700930511     C* CALCOLA LE DATE DI PREVISTA CONSEGNA
054800930511     C     D98ERR        IFEQ      '0'
054900930511     C                   EXSR      CALDPC
055000930511     C                   END
055100960229     C*
055200960229     C                   ENDIF                                                  <-- 6A --
055300960229     C*
055400930602     C* TRASFORMO I PROGRESSIVI ORE IN DATE
055500930602     C                   EXSR      TPODAT
055600930602     C*
055700930914     C* CONTROLLO SE BOLLA RIENTRA IN UNO DEI CASI IN CUI NON RIESCO A CALCOLA
055800930602     C* RE I RITARDI PER CUI FORZO CONFORMITA' A SI
055900930602     C     D98ERR        IFEQ      '0'
056000960229     C     D98TLA        ANDNE     'T'
056100930602     C                   EXSR      CHKFOR
056200930602     C                   END
056300930602     C*
056400930602     C     D98ERR        IFEQ      '0'                                          --- 7 -->
056500960229     C     D98TLA        ANDNE     'T'
056600941228     C*
056700941228     C* CALCOLA LE ORE REALI DI PARTENZA
056800941228     C     D98ERR        IFEQ      '0'
056900941228     C                   EXSR      CALORP
057000941228     C                   END
057100930405     C*
057200930511     C* CALCOLA LE ORE REALI DI TRAZIONE
057300930420     C     D98ERR        IFEQ      '0'
057400930407     C                   EXSR      CALORT
057500930420     C                   END
057600930405     C*
057700930511     C* CALCOLA LE ORE REALI DI CONSEGNA
057800930420     C     D98ERR        IFEQ      '0'
057900930416     C                   EXSR      CALORC
058000930420     C                   END
058100930416     C*
058200930511     C* CALCOLA LE ORE REALI DI RESA
058300930420     C     D98ERR        IFEQ      '0'
058400930416     C                   EXSR      CALORR
058500930420     C                   END
058600930420     C*
058700930511     C* CALCOLA LE ORE DI DIFFERENZA REALI/TEORICHE
058800930420     C     D98ERR        IFEQ      '0'
058900930420     C                   EXSR      CALDIF
059000930420     C                   END
059100930420     C*
059200930511     C* CALCOLA LE CONFORMITA'
059300930420     C     D98ERR        IFEQ      '0'
059400930420     C                   EXSR      CALCNF
059500930420     C                   END
059600930913     C*
059700930913     C* CALCOLA LE ORE DI CONSEGNABILITA'
059800930913     C     D98ERR        IFEQ      '0'
059900950407     C     FLCONS        ANDEQ     'S'
060000930913     C                   EXSR      CALCNS
060100930913     C                   END
060200930423     C*
060300930511     C* PORTO I VALORI DAI CAMPI DI LAVORO AI CAMPI IN OUTPUT DELLA DS
060400930423     C     D98ERR        IFEQ      '0'
060500930423     C                   EXSR      W98D98
060600930423     C                   END
060700930528     C*
060800930602     C                   END                                                    <-- 7 ---
060900930602     C                   END                                                    <-- 6 ---
061000930528     C                   END                                                    <-- 5 ---
061100930528     C                   END                                                    <-- 4 ---
061200930528     C                   END                                                    <-- 3 ---
061300930528     C                   END                                                    <-- 2 ---
061400930528     C                   END                                                    <-- 1 ---
061500930323     C*
061600930324     C* SE PGM LANCIATO CON 'C' (LANCI MULTIPLI) CHIUDE CON LR (CHIUDE FILE)
061700930527     C* E LANCIA TUTTI I PGM CHIAMATI PER CHIUDERE I FILE DA LORO APERTI
061800930324     C* ALTRIMENTI CHIUDE CON RT (NON CHIUDE I FILE)
061900930324     C     D98TLA        IFEQ      'C'
062000970714     C                   CLEAR                   SI95
062100970701     C                   MOVEL     'C'           I95TLA
062200020226     C                   CALL      'TISI95R'
062300970701     C                   PARM                    SI95
062400040809     c*
062500040809     c                   clear                   fidn12ds
062600040809     c                   eval      i12tla = 'C'
062700040809     c                   call      'FIDN12R'
062800040809     c                   parm                    fidn12ds
062900050523     c**
063000050523     c* controllo riempimento skiera CAK solo alla fine
063100050523     c**                 clear                   trul0sds
063200050523     c**                 eval      i0sski='CAK/CAM/CAP'
063300050523     c**                 eval      i0sele=%elem(CAK)
063400050523     c**                 if        windca>i0sele
063500050523     c**                 eval      i0spie=i0sele
063600050523     c**                 else
063700050523     c**                 eval      i0spie=windca
063800050523     c**                 endif
063900050523     c**                 eval      i0sfile='AZCLN00F  '
064000050523     c**                 eval      i0scod='     '
064100050523     c**                 eval      i0spgm='TNSD99R'
064200050523     c**                 movel     trul0sds      kpjbu
064300050523     c**                 call      'TRUL0SR'
064400050523     c**                 parm                    kpjba
064500050523     c
064600930323     C                   SETON                                        LR
064700930324     C                   ELSE
064800930324     C                   SETON                                        RT
064900930323     C                   END
065000941227     C********************************************************************
065100941227     C*   LETARB - LEGGE IL FILE FNARB (BOLLE ARRIVI)
065200941227     C********************************************************************
065300941227     C     LETARB        BEGSR
065400150325     c                   clear                   dsbl4L
065500150331     c                   clear                   ds7R
065600941227     C*
065700140820     C* LEGGO IL FILE FNARB01L
065800941227     C                   Z-ADD     D98AAS        ARBAAS
065900941227     C                   Z-ADD     D98LNP        ARBLNP
066000941227     C                   Z-ADD     D98NRS        ARBNRS
066100941227     C                   Z-ADD     D98NSP        ARBNSP
066200941228     C     KEYARB        CHAIN     FNARB01L                           99
066300941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
066400941227     C                   MOVEL     '1'           D98ERR
066500941227     C                   MOVEL     MSG(20)       D98MSG                         *BOLLA INESISTENTE
066600941227     C                   END                                                    <-- 1 ---
066700941228     C     D98ERR        IFEQ      '0'                                          --- 1 --> *TROVATA
066800941227     C                   MOVEL     ARBTSP        D98TSP
066900140820     c* Se forzato da Ds lo imposto al posto di quello della bolla
067000130529     c                   if        i98tsp_for<>' '
067100130529     C                   MOVEL     i98tsp_for    D98TSP
067200130529     c                   endif
067300941228     C                   MOVEL     ARBNZD        D98NZD
067400941227     C                   MOVEL     ARBCAD        D98CAD
067500970701     C                   MOVEL     ARBLOD        D98LOD
067600941227     C                   Z-ADD     ARBTFP        D98TFP
067700941227     C                   Z-ADD     ARBTFA        D98TFA
067800941227     C                   Z-ADD     ARBLNA        D98LNA
067900941227     C                   Z-ADD     ARBAAS        DSAAS
068000941227     C                   Z-ADD     ARBMGS        DSMGS
068100941227     C                   Z-ADD     DSDAT8        D98DSP
068200941227     C                   Z-ADD     ARBMGS        D98MGS
068300941227     C                   Z-ADD     ARBDTI        D98DTI
068400941227     C                   Z-ADD     ARBHTI        D98HTI
068500941227     C                   Z-ADD     ARBDCM        D98DCM
068600941227     C                   Z-ADD     ARBHMC        D98HMC
068700941227     C                   MOVEL     ARBGC1        D98GC1
068800941227     C                   MOVEL     ARBGC2        D98GC2
068900941227     C                   Z-ADD     ARBDCR        D98DCR
069000941227     C                   Z-ADD     ARBHCR        D98HCR
069100100308     C                   MOVEL     ARBTCR        D98TCR
069200100308     c* salvo i campi perchè in alcuni casi li clearo
069300100308     C                   Z-ADD     ARBDCR        s98DCR
069400100308     C                   Z-ADD     ARBHCR        s98HCR
069500100308     C                   MOVEL     ARBTCR        s98TCR
069600100308     c
069700941227     C                   MOVEL     ARBCMC        D98CMC
069800941227     C                   MOVEL     ARBFFD        D98FFD
069900941227     C                   Z-ADD     ARBNCD        D98NCD
070000941227     C                   MOVEL     ARBFNS        D98FNS
070100941227     C                   Z-ADD     ARBDUC        D98DUC
070200150508     C                   Z-ADD     ARBDPC        W98DPC
070300970310     C                   Z-ADD     *ZEROS        W98HUC
070400071122     c
070500071122     c                   movel     arbcbo        d98cbo
070600140820     c                   z-add     arbddc        d98ddc
070700140825     c                   z-add     arbndc        wd98ndc
070800140911     c                   movel     arbfbc        wd98fbc
070900150119     c                   movel     arbacc        wd98acc
071000150325     c                   exsr      IN_dist
071100150331     c* Se in fermo deposito vedo se presente particlarità consegna
071200150331     c                   if        arbffd='S' and arbgma<>*blanks
071300150331     C                   Z-ADD     CODUT         KTBUTE
071400150331     C                   MOVEL     '7R'          KTBCOD
071500150331     C                   MOVEL(P)  arbgma        KTBKEY
071600150331     C     KEYTAB        chain     TABEL00F
071700150331     c                   if        %found(tabel00f)
071800150331     c                   movel     tbluni        ds7r
071900150331     c                   endif
072000150331     c                   endif
072100150325     c
072200070511     c
072300141215     C                   Z-ADD     ARBDBR        D98DBR
072400141215     c                   eval      d98fdrt=' '
072500070511     c
072600141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
072700070511     c* La data ritiro è presente solo su fnblp
072800070511     C     KEYARB        CHAIN     FNblp01L                           99
072900141215     c                   if        %found(fnblp01l)
073000070511     C                   Z-ADD     blpDrt        D98DBR
073100070511     c                   eval      d98fdrt='S'
073200070511     c                   endif
073300141215     c******             endif
073400070511     c
073500110923     C                   movel     ARBxco        D98xco
073600110923     C                   Z-ADD     ARBDAM        D98DAM
073700941227     C                   MOVEL     ARBFIN        D98FIN
073800990308     C                   MOVEL     ARBFBR        WFBR
073900031201     C                   MOVEL     ARBGGA        WGGA
074000050901     C                   MOVEL     ARBcca        d98cca
074100051124     C                   MOVEL     ARBTC1        d98TC1
074200051124     C                   MOVEL     ARBTC2        d98TC2
074300941227     C                   Z-ADD     1             I
074400941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
074500941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
074600941227     C                   MOVEL     '1'           D98ERR
074700941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
074800941227     C                   ELSE                                                   <-- 2 -->
074900941227     C                   MOVEL     T3A(I)        D98TBL
075000941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
075100990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
075200941227     C                   END                                                    <-- 2 ---
075300130826
075400130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
075500130826     c                   if        %subst(d98tbl:1:1) ='F'
075600130826     C                   Z-ADD     ARBKSC        D98KSC
075700130826     c                   else
075800130826     c
075900130826     c                   if        arbccm>0
076000130826     C                   Z-ADD     ARBccm        D98KSC
076100130826     c                   endif
076200130826     c                   if        d98ksc=0
076300130826     c                   z-add     8888          d98ksc
076400130826     c                   movel     d98lnp        d98ksc
076500130826     c                   endif
076600130826     c                   endif
076700130826     c
076800941227     C                   END                                                    <-- 1 ---
076900941228     C*
077000051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
077100950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
077200941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
077300941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
077400941228     C                   Z-ADD     D98AAS        AR9AAS
077500941228     C                   Z-ADD     D98LNP        AR9LNP
077600941228     C                   Z-ADD     D98NRS        AR9NRS
077700941228     C                   Z-ADD     D98NSP        AR9NSP
077800051115     C     KEYAR9        CHAIN     FiAR901L                           99
077900950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
078000950102     C                   MOVEL     'S'           WFLASS
078100950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
078200950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
078300950102     C                   Z-ADD     D98AAS        LBLAAN
078400950102     C                   Z-ADD     D98LNP        LBLLPN
078500950102     C                   Z-ADD     D98NRS        LBLNRN
078600950102     C                   Z-ADD     D98NSP        LBLNSN
078700950102     C     KEYLBL        CHAIN     FNLBL01L                           99
078800950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
078900950102     C                   Z-ADD     LBLAAP        AR9AAS
079000950102     C                   Z-ADD     LBLLPP        AR9LNP
079100950102     C                   Z-ADD     LBLNRP        AR9NRS
079200950102     C                   Z-ADD     LBLNSP        AR9NSP
079300051115     C     KEYAR9        CHAIN     FiAR901L                           99
079400950102     C     *IN99         IFEQ      '0'
079500950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
079600950102     C                   ENDIF
079700950102     C                   ENDIF                                                  <-- 3 ---
079800950102     C                   ENDIF                                                  <-- 2 ---
079900950102     C*
080000950102     C* CONTRASSEGNO NON TROVATO
080100950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
080200941228     C                   ELSE                                                   <-- 2 -->
080300950102     C* CONTRASSEGNO TROVATO
080400941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
080500010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
080600010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
080700941228     C                   ENDIF                                                  <-- 2 ---
080800941228     C                   ENDIF                                                  <-- 1 ---
080900150325     c
081000941227     C*
081100050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
081200941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
081300050405     C                   Z-ADD     D98AAS        KWGCPAAS
081400050405     C                   Z-ADD     D98LNP        KWGCPLNP
081500050405     C                   Z-ADD     D98NRS        KWGCPNRS
081600050405     C                   Z-ADD     D98NSP        KWGCPNSP
081700050405     C                   Z-ADD     *ZEROS        KWGCPFRG
081800050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
081900941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
082000941227     C     GCPATB        ANDNE     'A'
082100960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
082200960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
082300960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
082400050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
082500960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
082600960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
082700050405     C* O UGUALE A 35 INVECE DI 40)
082800960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
082900050405     C     GCPFAS        ANDGE     40
083000941227     C                   Z-ADD     GCPDCG        D98DCG
083100950112     C                   Z-ADD     GCPDCG        W98DCG
083200960123     C                   ENDIF
083300960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
083400960729     C     GCPFAS        ANDGE     35
083500070202     C     GCPFAS        ANDne     36
083600960729     C                   Z-ADD     GCPDCG        D98DCG
083700960729     C                   Z-ADD     GCPDCG        W98DCG
083800960729     C                   ENDIF
083900941228     C                   ENDIF
084000941228     C                   ENDIF                                                  <-- 1 ---
084100970911     C*
084200970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
084300970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
084400970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
084500050922     C*****              Z-ADD     D98DTI        W98DTI
084600971210     C                   Z-ADD     D98LNP        F
084700971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
084800970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
084900970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
085000150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
085100150120     c                   if        arbdcp=0 and d98dfc=0
085200150120     C                   Z-ADD     d98dam        D98DTI
085300150120     c                   endif
085400150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
085500150120     C                   Z-ADD     d98dam        D98DTI
085600150120     c                   endif
085700150120     c                   ENDIF
085800150120     c***
085900150120     C***                Z-ADD     D98AAS        ARTAAS
086000150120     C***                Z-ADD     D98LNP        ARTLNP
086100150120     C***                Z-ADD     D98NRS        ARTNRS
086200150120     C***                Z-ADD     D98NSP        ARTNSP
086300150120     C***  KEYART        SETLL     FNART01L                           99
086400150120     C**N99KEYART        READE     FNART01L                               99
086500150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
086600150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
086700970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
086800150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
086900970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
087000970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
087100150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
087200150120     C***  ARTDCM        ANDEQ     *ZEROS
087300970912     C* SE PRIMO REC. TROVATO, MEM. DATA
087400150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
087500150120     C***                Z-ADD     ARTDAM        D98DTI
087600150120     C***                ELSE                                                   <-- 6 -->
087700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
087800150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
087900150120     C***                Z-ADD     ARTDAM        D98DTI
088000150120     C***                ENDIF                                                  <-- 7 ---
088100150120     C***                ENDIF                                                  <-- 6 ---
088200150120     C***                ENDIF                                                  <-- 5 ---
088300970911     C* C'E' DATA CONSEGNAB.
088400150120     C***                ELSE                                                   <-- 4 -->
088500970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
088600970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
088700150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
088800150120     C***  ARTDAM        ANDLE     D98DFC
088900970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
089000970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
089100150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
089200150120     C***  ARTDCM        ORGT      D98DFC
089300970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
089400150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
089500150120     C***                Z-ADD     ARTDAM        D98DTI
089600150120     C***                ELSE                                                   <-- 7 -->
089700970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
089800150120     C***  ARTDAM        IFLT      D98DTI
089900150120     C***                Z-ADD     ARTDAM        D98DTI
090000150120     C***                ENDIF
090100150120     C***                ENDIF                                                  <-- 7 ---
090200150120     C***                ENDIF                                                  <-- 6 ---
090300150120     C***                ENDIF                                                  <-- 5 ---
090400150120     C***                ENDIF                                                  <-- 4 ---
090500150120     C***                ENDIF                                                  <-- 3 ---
090600150120     C***  KEYART        READE     FNART01L                               99
090700150120     C***                ENDDO                                                  <-- 2 ---
090800150120     C***                ENDIF                                                  <-- 1 ---
090900941227     C*
091000941227     C                   ENDSR
091100150325     C********************************************************************
091200150325     C*   Verifico se bolla in distinta
091300150325     C********************************************************************
091400150325     c     IN_dist       BEGSR
091500150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
091600150325     c                   if         ARBfbc = *blanks and
091700150325     c                               ARBddc > 0 and
091800150325     c                               ARBndc > 0 and ARBdcm=0
091900150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
092000150325     c                   if        not %open(fiapd01l)
092100150325     c                   open      fiapd01l
092200150325     c                   endif
092300150325     c
092400150325     C                   move      'A'           kpdtip
092500150325     c                   eval      kpdpdr=arbpdc
092600150325     C     kfapd         chain     fiapd01l
092700150325     c
092800150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
092900150325     c                   eval       d98cons = 'S'
093000150325     c                   endif
093100150325     c                   endif
093200150325     c                   ENDSR
093300941227     C********************************************************************
093400941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
093500941227     C********************************************************************
093600941227     C     LETBLP        BEGSR
093700150331     c                   clear                   dsbl4l
093800150331     c                   clear                   ds7R
093900941227     C*
094000941227     C                   Z-ADD     D98AAS        BLPAAS
094100941227     C                   Z-ADD     D98LNP        BLPLNP
094200941227     C                   Z-ADD     D98NRS        BLPNRS
094300941227     C                   Z-ADD     D98NSP        BLPNSP
094400941227     C     KEYBLP        CHAIN     FNBLP01L                           99
094500941228     C     *IN99         IFEQ      '1'                                          --- 1 --> *NON TROV.
094600941227     C                   MOVEL     '1'           D98ERR
094700941227     C                   MOVEL     MSG(20)       D98MSG
094800941227     C                   END                                                    <-- 1 ---
094900941228     C     *IN99         IFEQ      '0'                                                    *TROV.
095000041028     C     KEYBLP        CHAIN     FNarb01L
095100041028     c                   if        %found(fnarb01l)
095200041028     c                   eval      wesarb='S'
095300041028     c                   else
095400041028     c                   eval      wesarb='N'
095500150219     c                   eval      wtrasm='D'
095600041028     c                   endif
095700141216     c
095800141216     c* se non trasmessa la bolla in arrivo memorizzo
095900150219     c                   if        blpft1= ' '
096000141229     c                   eval      wtrasm='N'
096100141216     c                   endif
096200141216     c
096300110923     C                   MOVEL     BLPxco        D98xco
096400110923     C                   MOVEL     BLPTSP        D98TSP
096500130529     c* Se forzato da Ds lo imporsto al posto di quello della bolla
096600130529     c                   if        i98tsp_for<>' '
096700130529     C                   MOVEL     i98tsp_for    D98TSP
096800130529     c                   endif
096900960209     C                   MOVEL     BLPNZD        D98NZD
097000941228     C                   MOVEL     BLPCAD        D98CAD
097100970701     C                   MOVEL     BLPLOD        D98LOD
097200941227     C                   Z-ADD     BLPTFP        D98TFP
097300941227     C                   Z-ADD     BLPTFA        D98TFA
097400941227     C                   Z-ADD     BLPLNA        D98LNA
097500941227     C                   Z-ADD     BLPAAS        DSAAS
097600941227     C                   Z-ADD     BLPMGS        DSMGS
097700941227     C                   Z-ADD     DSDAT8        D98DSP
097800941227     C                   Z-ADD     BLPMGS        D98MGS
097900051124     C                   movel     BLPTC1        D98TC1
098000051124     C                   movel     BLPTC2        D98TC2
098100041028     c                   if        wesarb='S'
098200041028     C                   Z-ADD     arbDTI        D98DTI
098300041028     C                   Z-ADD     arbHTI        D98HTI
098400041028     C                   Z-ADD     arbDCM        D98DCM
098500041028     C                   Z-ADD     arbHMC        D98HMC
098600041028     C                   Z-ADD     arbDAM        D98DAM
098700050901     C                   MOVEL     arbcca        d98cca
098800140820     C                   MOVEL     arbddc        d98ddc
098900140911     C                   z-add     arbndc        wd98ndc
099000140910     C                   MOVEL     arbfbc        wd98fbc
099100150119     c                   movel     arbacc        wd98acc
099200150325     c                   exsr      IN_DIST
099300140820     c*
099400041028     c                   else
099500941227     C                   Z-ADD     BLPDTI        D98DTI
099600941227     C                   Z-ADD     BLPHTI        D98HTI
099700941227     C                   Z-ADD     BLPDCM        D98DCM
099800941227     C                   Z-ADD     BLPHMC        D98HMC
099900041028     C                   Z-ADD     BLPDAM        D98DAM
100000050901     C                   MOVEL     BLPcca        d98cca
100100140820     C                   MOVEL     BLPddc        d98ddc
100200140825     C                   MOVEL     BLPndc        wd98ndc
100300140820     c* se la bolla non esiste più in ARB dico che non è"in distinta"
100400140820     c                   clear                   d98cons
100500041028     c                   endif
100600941227     C                   MOVEL     BLPGC1        D98GC1
100700941227     C                   MOVEL     BLPGC2        D98GC2
100800941227     C                   Z-ADD     BLPDCR        D98DCR
100900941227     C                   Z-ADD     BLPHCR        D98HCR
101000941227     C                   MOVEL     BLPTCR        D98TCR
101100100308     c* Salvo i campi perchè in alcuni casi li clearo
101200100308     C                   Z-ADD     blpDCR        s98DCR
101300100308     C                   Z-ADD     blpHCR        s98HCR
101400100308     C                   MOVEL     blpTCR        s98TCR
101500100308     c
101600941227     C                   MOVEL     *BLANKS       D98CMC
101700941227     C                   MOVEL     BLPFFD        D98FFD
101800941227     C                   Z-ADD     BLPNCD        D98NCD
101900941227     C                   MOVEL     BLPFNS        D98FNS
102000941228     C                   Z-ADD     BLPDUC        D98DUC
102100150508     C                   Z-ADD     BLPDPC        w98DPC
102200970310     C                   Z-ADD     *ZEROS        W98HUC
102300071122     c                   movel     blpcbo        d98cbo
102400150331     c*
102500150331     c* Se in fermo deposito vedo se presente particlarità consegna
102600150331     c                   if        blpffd='S' and blpgma<>*blanks
102700150331     C                   Z-ADD     CODUT         KTBUTE
102800150331     C                   MOVEL     '7R'          KTBCOD
102900150331     C                   MOVEL(P)  blpgma        KTBKEY
103000150331     C     KEYTAB        chain     TABEL00F
103100150331     c                   if        %found(tabel00f)
103200150331     c                   movel     tbluni        ds7r
103300150331     c                   endif
103400150331     c                   endif
103500150331     c
103600970304     C                   Z-ADD     BLPDBR        D98DBR
103700070514     c                   eval      d98fdrt=' '
103800070514     c
103900141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
104000070514     C                   Z-ADD     blpDrt        D98DBR
104100070514     c                   eval      d98fdrt='S'
104200141215     c********           endif
104300070514     c*
104400941227     C                   MOVEL     BLPFIN        D98FIN
104500130826     C****               Z-ADD     BLPKSC        D98KSC
104600990308     C                   MOVEL     BLPFBR        WFBR
104700031201     C                   MOVEL     BLPGGA        WGGA
104800941227     C                   Z-ADD     1             I
104900941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
105000941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
105100941227     C                   MOVEL     '1'           D98ERR
105200941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
105300941227     C                   ELSE                                                   <-- 2 -->
105400941227     C                   MOVEL     T3A(I)        D98TBL
105500941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
105600990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
105700941227     C                   END                                                    <-- 2 ---
105800130826     c                   if        %subst(d98tbl:1:1) ='F'
105900130826     C                   Z-ADD     blpKSC        D98KSC
106000130826     c                   else
106100130826     c
106200130826     c                   if        blpccm>0
106300130826     C                   Z-ADD     blpccm        D98KSC
106400130826     c                   endif
106500130826     c                   if        d98ksc=0
106600130826     c                   z-add     8888          d98ksc
106700130826     c                   movel     d98lnp        d98ksc
106800130826     c                   endif
106900130826     c                   endif
107000130826     c
107100941227     C                   END
107200941228     C*
107300051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
107400950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
107500941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
107600941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
107700051115     C                   Z-ADD     D98AAS        ar9AAS
107800051115     C                   Z-ADD     D98LNP        ar9LNP
107900051115     C                   Z-ADD     D98NRS        ar9NRS
108000051115     C                   Z-ADD     D98NSP        ar9NSP
108100051115     C     KEYar9        CHAIN     Fiar901L                           99
108200950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
108300950102     C                   MOVEL     'S'           WFLASS
108400950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
108500950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
108600950102     C                   Z-ADD     D98AAS        LBLAAN
108700950102     C                   Z-ADD     D98LNP        LBLLPN
108800950102     C                   Z-ADD     D98NRS        LBLNRN
108900950102     C                   Z-ADD     D98NSP        LBLNSN
109000950102     C     KEYLBL        CHAIN     FNLBL01L                           99
109100950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
109200051115     C                   Z-ADD     LBLAAP        ar9AAS
109300051115     C                   Z-ADD     LBLLPP        ar9LNP
109400051115     C                   Z-ADD     LBLNRP        ar9NRS
109500051115     C                   Z-ADD     LBLNSP        ar9NSP
109600051115     C     KEYar9        CHAIN     Fiar901L                           99
109700950102     C     *IN99         IFEQ      '0'
109800950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
109900950102     C                   ENDIF
110000950102     C                   ENDIF                                                  <-- 3 ---
110100950102     C                   ENDIF                                                  <-- 2 ---
110200950102     C*
110300950102     C* CONTRASSEGNO TROVATO
110400150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
110500051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
110600051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
110700051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
110800941228     C                   ENDIF                                                  <-- 2 ---
110900941228     C                   ENDIF                                                  <-- 1 ---
111000941227     C*
111100050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
111200941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
111300050405     C                   Z-ADD     D98AAS        KWGCPAAS
111400050405     C                   Z-ADD     D98LNP        KWGCPLNP
111500050405     C                   Z-ADD     D98NRS        KWGCPNRS
111600050405     C                   Z-ADD     D98NSP        KWGCPNSP
111700050405     C                   Z-ADD     *ZEROS        KWGCPFRG
111800050405     C     KEYGCP        CHAIN     TIGCP01L                           99
111900941228     C     *IN99         IFEQ      '0'                                          *TROV.
112000941227     C     GCPATB        ANDNE     'A'
112100960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
112200960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
112300960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
112400960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
112500050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
112600960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
112700960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
112800050405     C* O UGUALE A 35 INVECE DI 40)
112900960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
113000050405     C     GCPFAS        ANDGE     40
113100941227     C                   Z-ADD     GCPDCG        D98DCG
113200950112     C                   Z-ADD     GCPDCG        W98DCG
113300960123     C                   ENDIF
113400960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
113500960729     C     GCPFAS        ANDGE     35
113600070202     C     GCPFAS        ANDne     36
113700960729     C                   Z-ADD     GCPDCG        D98DCG
113800960729     C                   Z-ADD     GCPDCG        W98DCG
113900960729     C                   ENDIF
114000941228     C                   ENDIF
114100941228     C                   ENDIF                                                  <-- 1 ---
114200970912     C*
114300970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
114400970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
114500970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
114600050922     C******             Z-ADD     D98DTI        W98DTI
114700971210     C                   Z-ADD     D98LNP        F
114800971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
114900970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
115000970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
115100150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
115200150120     c                   if        blpdcp=0 and d98dfc=0
115300150120     C                   Z-ADD     d98dam        D98DTI
115400150120     c                   endif
115500150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
115600150120     C                   Z-ADD     d98dam        D98DTI
115700150120     c                   endif
115800150120     c                   ENDIF
115900150120     c*
116000150120     C***                Z-ADD     D98AAS        BLTAAS
116100150120     C****               Z-ADD     D98LNP        BLTLNP
116200150120     C***                Z-ADD     D98NRS        BLTNRS
116300150120     C***                Z-ADD     D98NSP        BLTNSP
116400150120     C***  KEYBLT        SETLL     FNBLT01L                           99
116500150120     C**N99KEYBLT        READE     FNBLT01L                               99
116600150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
116700150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
116800970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
116900150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
117000970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
117100970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
117200150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
117300150120     C**   BLTDCM        ANDEQ     *ZEROS
117400970912     C* SE PRIMO REC. TROVATO, MEM. DATA
117500150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
117600150120     C**                 Z-ADD     BLTDAM        D98DTI
117700150120     C**                 ELSE                                                   <-- 6 -->
117800970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
117900150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
118000150120     C**                 Z-ADD     BLTDAM        D98DTI
118100150120     C**                 ENDIF                                                  <-- 7 ---
118200150120     C**                 ENDIF                                                  <-- 6 ---
118300150120     C**                 ENDIF                                                  <-- 5 ---
118400970912     C* C'E' DATA CONSEGNAB.
118500150120     **                  ELSE                                                   <-- 4 -->
118600970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
118700970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
118800150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
118900150120     C**   BLTDAM        ANDLT     D98DFC
119000970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
119100970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
119200150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
119300150120     C**   BLTDCM        ORGT      D98DFC
119400970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
119500150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
119600150120     C**                 Z-ADD     BLTDAM        D98DTI
119700150120     C**                 ELSE                                                   <-- 7 -->
119800970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
119900150120     C**   BLTDAM        IFLT      D98DTI
120000150120     C**                 Z-ADD     BLTDAM        D98DTI
120100150120     C**                 ENDIF
120200150120     C**                 ENDIF                                                  <-- 7 ---
120300150120     C**                 ENDIF                                                  <-- 6 ---
120400150120     C**                 ENDIF                                                  <-- 5 ---
120500150120     C**                 ENDIF                                                  <-- 4 ---
120600150120     C**                 ENDIF                                                  <-- 3 ---
120700150120     C**   KEYBLT        READE     FNBLT01L                               99
120800150120     C**                 ENDDO                                                  <-- 2 ---
120900150120     C**                 ENDIF                                                  <-- 1 ---
121000941227     C*
121100941227     C                   ENDSR
121200930323     C********************************************************************
121300941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
121400930323     C********************************************************************
121500930323     C     RTTEOR        BEGSR
121600941227     C*
121700941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
121800941228     C     D98ERR        IFEQ      '0'
121900941227     C                   EXSR      RTPART
122000941227     C                   ENDIF
122100941227     C*
122200941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
122300941228     C     D98ERR        IFEQ      '0'
122400150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
122500150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
122600150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
122700150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
122800150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
122900941227     C                   EXSR      RTTRAZ
123000150219     c                   if        d98err<>'1'
123100150219     C                   Z-ADD     CASTIN        D98TTI
123200150219     C                   Z-ADD     CASTCN        D98TTC
123300150219     C                   endif
123400150219     C                   ENDIF
123500941227     C*
123600941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
123700941228     C     D98ERR        IFEQ      '0'
123800941227     C                   EXSR      RTCONS
123900941227     C                   ENDIF
124000941227     C*
124100941228     C     D98ERR        IFEQ      '0'
124200941227     C*
124300941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
124400941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
124500941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
124600090921     c                   z-add     d98tri        w99tri
124700090921     c* per la DS
124800090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
124900090921     c* per i calcoli interi
125000090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
125100090921     c
125200941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
125300941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
125400941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
125500090921     c                   z-add     d98trc        w99trc
125600090921     c* per la DS
125700090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
125800090921     c* per i calcoli interi
125900090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
126000941227     C*
126100941227     C                   ENDIF
126200930323     C*
126300930323     C                   ENDSR
126400941227     C********************************************************************
126500941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
126600941227     C********************************************************************
126700941227     C     RTPART        BEGSR
126800941228     C*
126900941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
127000941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
127100941228     C                   MOVEL     D98TSP        WTSP              1
127200130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
127300130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
127400050301     C*******
127500050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
127600050301     C*
127700050301     C*    W001          IFEQ      'A'
127800050301     C*    W0040         OREQ      8888
127900050301     C*                  MOVEL     'E'           WTSP
128000050301     C*                  ENDIF
128100050301     C*******
128200941227     C*
128300941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
128400941227     C                   MOVEL     'N'           WTRAOK            1
128500941227     C                   MOVEL     'P'           CASPTA
128600941227     C                   Z-ADD     D98LNP        CASLNP
128700941227     C                   Z-ADD     *ZEROS        CASLNA
128800941228     C                   MOVEL     WTSP          CASTSP
128900941228     C                   Z-ADD     99999999      CASDDE
129000941227     C     KEYCAS        SETGT     AZCAS01L                             99
129100941227     C  N99              READP     AZCAS01L                               99
129200941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
129300941227     C     WTRAOK        ANDEQ     'N'
129400941227     C     CASLNP        ANDEQ     D98LNP
129500950102     C     CASTSP        ANDEQ     WTSP
129600941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
129700941227     C     CASATB        ANDEQ     *BLANKS
129800941227     C                   MOVEL     'S'           WTRAOK
129900941227     C                   ELSE                                                   <-- 2 -->
130000941227     C                   READP     AZCAS01L                               99
130100941227     C                   ENDIF                                                  <-- 2 ---
130200941227     C                   ENDDO                                                  <-- 1 ---
130300941227     C*
130400941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
130500941227     C     WTRAOK        IFEQ      'N'
130600941227     C                   MOVEL     '1'           D98ERR
130700941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
130800941227     C                   ELSE
130900941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
131000941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
131100941227     C                   END
131200970214     C*
131300140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
131400140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
131500140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
131600140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
131700140403     C**                 Z-ADD     D98LNA        F
131800140403     C**   D98ERR        IFEQ      '0'
131900140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
132000140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
132100140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
132200140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
132300140403     C**                 ENDIF
132400140403     C**                 ENDIF
132500140402     c
132600140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
132700140402     c*  se tempi  = 0
132800140402     c                   if        wpick2='S' and d98tpi<=0
132900140402     c                   add       24            D98TPI
133000140402     c                   endif
133100941227     C*
133200941227     C                   ENDSR
133300941227     C********************************************************************
133400941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
133500941227     C********************************************************************
133600941227     C     RTTRAZ        BEGSR
133700941227     C*
133800941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
133900150219     c
134000150219     C                   Z-ADD     D98LNA        LTA(1)
134100150219     C                   Z-ADD     D98LNA        LTA(2)
134200150219     C                   Z-ADD     D98TFA        LTA(3)
134300941227     C                   Z-ADD     D98TFA        LTA(4)
134400941227     C*
134500941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
134600941227     C                   MOVEL     'N'           WTRAOK            1
134700941227     C                   Z-ADD     1             K                 3 0
134800941227     C*
134900941227     C* CICLO FINO A:
135000941227     C*    - FINE SCHIERA
135100941227     C*    - TROVATO TEMPI VALIDI
135200941227     C     K             DOUGT     4                                            --- 1 -->
135300941227     C     WTRAOK        OREQ      'S'
135400941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
135500941227     C     LTA(K)        ORNE      *ZEROS
135600941227     C                   MOVEL     'T'           CASPTA
135700941227     C                   Z-ADD     LTP(K)        CASLNP
135800941227     C                   Z-ADD     LTA(K)        CASLNA
135900941227     C                   MOVEL     D98TSP        CASTSP
136000941228     C                   Z-ADD     99999999      CASDDE
136100941227     C     KEYCAS        SETGT     AZCAS01L                             99
136200941227     C  N99              READP     AZCAS01L                               99
136300941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
136400941227     C     WTRAOK        ANDEQ     'N'
136500941227     C     LTP(K)        ANDEQ     CASLNP
136600941227     C     LTA(K)        ANDEQ     CASLNA
136700941227     C     D98TSP        ANDEQ     CASTSP
136800960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
136900941227     C     CASATB        ANDEQ     *BLANKS
137000941227     C                   MOVEL     'S'           WTRAOK
137100941227     C                   ELSE                                                   <-- 4 -->
137200941227     C                   READP     AZCAS01L                               99
137300941227     C                   ENDIF                                                  <-- 4 ---
137400941227     C                   ENDDO                                                  <-- 3 ---
137500941227     C                   ENDIF                                                  <-- 2 ---
137600941227     C                   ADD       1             K
137700941227     C                   ENDDO                                                  <-- 1 ---
137800941227     C*
137900941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
138000941227     C     WTRAOK        IFEQ      'N'
138100941227     C                   MOVEL     '1'           D98ERR
138200941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
138300150219     C***                ELSE
138400150219     C***                Z-ADD     CASTIN        D98TTI
138500150219     C***                Z-ADD     CASTCN        D98TTC
138600941227     C                   END
138700941227     C*
138800941227     C                   ENDSR
138900941227     C********************************************************************
139000941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
139100941227     C********************************************************************
139200941227     C     RTCONS        BEGSR
139300150629     c                   clear                   WTsogliaE         1
139400970701     C*
139500970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
139600970701     C                   CLEAR                   SI95
139700970701     C                   MOVEL     '3'           I95TCN
139800970701     C                   MOVEL     D98NZD        I95NAR
139900970701     C                   MOVEL     D98CAD        I95CAP
140000970701     C                   MOVEL     D98LOD        I95LOC
140100970701     C                   MOVEL     D98DSP        I95DAT
140200970701     C                   MOVEL     D98TSP        I95TSP
140300980602     C                   MOVEL     'S'           I95FRT
140400150511     c* In caso di fermo deposito, forzo sempre i tempi "C"
140500150511     c                   if        d98ffd='S'
140600150511     c                   eval      i95tsp='C'
140700150511     c                   endif
140800020226     C*
140900020226     C* VERIFICO SE LNA E' FILIALE FEDEX
141000020226     C                   IF        D98LNA > *ZEROS
141100020226     C     D98LNA        LOOKUP    FILFED                                 55
141200020226     C                   IF        *IN55 = *ON
141300020226     C                   EVAL      I95FI1 = 'S'
141400020226     C                   ENDIF
141500020226     C                   ENDIF
141600020226     C*
141700020226     C                   CALL      'TISI95R'
141800970701     C                   PARM                    SI95
141900941227     C*
142000941228     C* SE NON TROVATO, ERRORE
142100970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
142200941227     C                   MOVEL     '1'           D98ERR
142300970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
142400941227     C                   ELSE                                                   <-- 1 -->
142500941227     C*
142600941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
142700941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
142800941227     C     CASTIN        DIV       24            WDIV              3 0
142900941227     C                   MVR                     WRES              3 0
143000941227     C     WRES          IFEQ      *ZEROS
143100970701     C                   Z-ADD     O950IN        D98TCI
143200941227     C                   ELSE
143300970701     C                   Z-ADD     O951IN        D98TCI
143400941227     C                   END
143500941227     C*
143600941227     C     CASTCN        DIV       24            WDIV                     99
143700941227     C                   MVR                     WRES
143800941227     C     WRES          IFEQ      *ZEROS
143900970701     C                   Z-ADD     O950CN        D98TCC
144000150629     c                   eval      wTSogliaE='0'
144100941227     C                   ELSE
144200970701     C                   Z-ADD     O951CN        D98TCC
144300150629     c                   eval      wTSogliaE='1'
144400941227     C                   END
144500941227     C*
144600150629     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
144700941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
144800150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
144900150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
145000150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
145100150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
145200150629     C                   Z-ADD     O951CN        W991CN            3 0          H.12 CLI
145300941227     C*
145400941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
145500941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
145600970701     C     O95ISO        IFEQ      'I'
145700941227     C     D98FIN        OREQ      'I'
145800970701     C                   ADD       O95TIS        D98TCI
145900970701     C                   ADD       O95TIS        D98TCC
146000941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
146100970701     C                   ADD       O95TIS        W990IN
146200970701     C                   ADD       O95TIS        W991IN
146300150629     C                   ADD       O95TIS        W990CN
146400150629     C                   ADD       O95TIS        W991CN
146500941227     C                   END
146600090827     c
146700090827     c* Sistemo i tempi di consegna se non sono multipli di 12
146800090827     c                   exsr      SisCONS
146900941228     C*
147000941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
147100941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
147200941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
147300941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
147400941228     C                   Z-ADD     D98TCC        W99CCE            3 0
147500941228     C                   ELSE                                                   <-- 3 -->
147600970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
147700970701     C                   CLEAR                   SI95
147800970701     C                   MOVEL     '3'           I95TCN
147900970701     C                   MOVEL     D98NZD        I95NAR
148000970701     C                   MOVEL     D98CAD        I95CAP
148100970701     C                   MOVEL     D98LOD        I95LOC
148200970701     C                   MOVEL     D98DSP        I95DAT
148300970701     C                   MOVEL     'E'           I95TSP
148400980602     C                   MOVEL     'S'           I95FRT
148500020226     C*
148600020226     C* VERIFICO SE LNA E' FILIALE FEDEX
148700020226     C                   IF        D98LNA > *ZEROS
148800020226     C     D98LNA        LOOKUP    FILFED                                 55
148900020226     C                   IF        *IN55 = *ON
149000020226     C                   EVAL      I95FI1 = 'S'
149100020226     C                   ENDIF
149200020226     C                   ENDIF
149300020226     C*
149400020226     C                   CALL      'TISI95R'
149500970701     C                   PARM                    SI95
149600970701     C*
149700941228     C* SE NON TROVATO, ERRORE
149800970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
149900941228     C                   MOVEL     '1'           D98ERR
150000970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
150100941228     C                   ELSE                                                   <-- 4 -->
150200941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
150300941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
150400941228     C     CASTCN        DIV       24            WDIV                     99
150500941228     C                   MVR                     WRES
150600941228     C     WRES          IFEQ      *ZEROS
150700970701     C                   Z-ADD     O950CN        W99CCE
150800941228     C                   ELSE
150900970701     C                   Z-ADD     O951CN        W99CCE
151000941228     C                   END
151100941228     C                   ENDIF                                                  <-- 4 ---
151200941228     C                   ENDIF                                                  <-- 3 ---
151300941228     C                   ENDIF                                                  <-- 2 ---
151400941227     C*
151500941227     C                   END                                                    <-- 1 ---
151600941227     C*
151700941227     C                   ENDSR
151800090827     C********************************************************************
151900090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
152000090827     C********************************************************************
152100090827     C     SisCONS       BEGSR
152200090827     c                   eval      w99tci=d98tci
152300090827     c                   if        d98tci=10
152400090827     c                   z-add     12            w99tci
152500090827     c                   endif
152600090827     c
152700090827     c                   eval      w99tcc=d98tcc
152800090827     c                   if        d98tcc=10
152900090827     c                   z-add     12            w99tcc
153000090827     c                   endif
153100090827     c
153200090827     C                   ENDSR
153300930420     C********************************************************************
153400930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
153500930420     C********************************************************************
153600930420     C     CAREVB        BEGSR
153700930420     C*
153800930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
153900930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
154000060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
154100930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
154200930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
154300060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
154400060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
154500930420     C                   Z-ADD     D98AAS        KEBAAS
154600930420     C                   Z-ADD     D98LNP        KEBLNP
154700930420     C                   Z-ADD     D98NRS        KEBNRS
154800930420     C                   Z-ADD     D98NSP        KEBNSP
154900990810     C     KEYEVB        SETLL     FNEVB01L                           99
155000060406     C  N99keyevb        READe     FNEVB01L                               99
155100060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
155200930421     C     D98ERR        ANDEQ     '0'
155300060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
155400930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
155500930421     C                   Z-ADD     1             I
155600930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
155700060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
155800930421     C                   MOVEL     '1'           D98ERR
155900930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
156000060406   x3C                   ELSE                                                   <-- 3 -->
156100060406     C                   MOVEL     D2A(I)        usa2A
156200060406     C                   MOVEL     D2Z(I)        usa2Z
156300930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
156400060406    4C     §2AFTC        IFEQ      'A'
156500930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
156600060406    4C                   END
156700060406    4C     §2AFTC        IFEQ      'G'
156800930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
156900060406    4C                   END
157000150216     c* Se si tratta di MIC verifico se uguale alla sped.
157100150303    4c                   if        evbcev='MIC' or evbcev='NIC'
157200150303    5c                   if        %subst(evbnot:1:7) >*zeros
157300150303     c
157400150303    6c                   if        evbcev='MIC' and
157500150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
157600150216     c                   eval      evemic='SI'
157700150303    6c                   endif
157800150303     c
157900150303    6c                   if        evbcev='NIC' and
158000150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
158100150216     c                   eval      evemic='  '
158200150303    6c                   endif
158300150303     c
158400150303   x5c                   else
158500150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
158600150303     c                   eval      evemic='SI'
158700150303    6c                   endif
158800150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
158900150303     c                   eval      evemic='  '
159000150303    6c                   endif
159100150303
159200150303    5c                   endif
159300150303    4c                   endif
159400150216     c
159500060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
159600930421     C     §2AFTC        OREQ      'G'
159700930421     C                   ADD       1             IE
159800990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
159900930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
160000930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
160100131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
160200090928     c
160300090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
160400091014     c**                 if        ie=1
160500091014     c**                 eval      Primotipo=§2aftc
160600091014     c**                 eval      Primodev=evbdev
160700091014     c**                 eval      Primohev=evbhev
160800091014     c**                 endif
160900090928     c
161000060406    4C                   END                                                    <-- 4 ---
161100060406     c* Verifico se si tratta di evento particolare
161200060406    4c                   if        §2zCEP<>' '
161300060526     c* non carico l'evento se cade in giorno festivo
161400060526     c                   z-add     evbdev        wpodata
161500060526     C                   EVAL      WTC='C'
161600060526     C                   EVAL      WIC='C'
161700060526     c                   eval      wslitt='N'
161800060526     c                   exsr      chkfes
161900060526     c
162000060526     c* Wslitt='N' non festivo
162100060526   4ac                   if        wslitt='N'
162200060526     c
162300060406    5c                   if        §2zfep='A'
162400060406     c                   eval      wevpaA='S'
162500060406    5C                   END                                                    <-- 3 ---
162600060406    5c                   if        §2zfep='G'
162700060406     c                   eval      wevpaG='S'
162800060406    5C                   END                                                    <-- 3 ---
162900060406     c                   add       1             ip
163000060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
163100060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
163200060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
163300060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
163400060526   4aC                   ENDif                                                  <-- 3 ---
163500060526    4C                   END                                                    <-- 3 ---
163600060406    3C                   END                                                    <-- 3 ---
163700060406    2C                   END                                                    <-- 2 ---
163800060406     C     keyevb        READe     FNEVB01L                               99
163900060406    1C                   ENDDO                                                  <-- 1 ---
164000050523     c* controllo riempimento skiere
164100050523     c                   clear                   trul0sds
164200050523     c                   eval      i0sski='EVD/EVO/EVC'
164300050523     c                   eval      i0sele=%elem(EVD)
164400050523     c                   eval      i0spie=ie
164500050523     c                   eval      i0sfile='FNEVB01L  '
164600050523     c                   eval      i0scod='     '
164700050523     c                   eval      i0spgm='TNSD99R'
164800050523     c                   movel     trul0sds      kpjbu
164900050523     c                   call      'TRUL0SR'
165000050523     c                   parm                    kpjba
165100930420     C*
165200060406     c* controllo riempimento skiere
165300060406     c                   clear                   trul0sds
165400060406     c                   eval      i0sski='PVD/PVO/PVC'
165500060406     c                   eval      i0sele=%elem(PVD)
165600060406     c                   eval      i0spie=ie
165700060406     c                   eval      i0sfile='FNEVB01L  '
165800060406     c                   eval      i0scod='     '
165900060406     c                   eval      i0spgm='TNSD99R'
166000060406     c                   movel     trul0sds      kpjbu
166100060406     c                   call      'TRUL0SR'
166200060406     c                   parm                    kpjba
166300060406     C*
166400930420     C                   ENDSR
166500931213     C********************************************************************
166600931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
166700931213     C********************************************************************
166800931213     C     CARTUC        BEGSR
166900931213     C*
167000931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
167100931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
167200931214     C     D98GC2        ANDEQ     *BLANKS
167300931213     C                   ELSE                                                   <-- 1 -->
167400931213     C*
167500931213     C                   MOVE      D98GC1        TUC(1)
167600931214     C                   MOVE      D98GC2        TUC(2)
167700931213     C*
167800931213     C                   END                                                    <-- 1 ---
167900931213     C*
168000931213     C                   ENDSR
168100970304     C********************************************************************
168200970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
168300970304     C********************************************************************
168400970304     C     DATRIF        BEGSR
168500970304     C*
168600050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
168700050922     c*  partenza
168800970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
168900970304     C*
169000970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
169100070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
169200070605     C***                MOVEL     '1'           D98ERR
169300070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
169400070605     C***                ELSE                                                   <-- 2 -->
169500970304     C*
169600970304     C                   Z-ADD     D98DRF        WD1
169700970304     C                   Z-ADD     D98ORF        WO1
169800970310     C* DATA PARTENZA ULTIMO COLLO
169900970310     C     D98DRF        IFLE      D98DUC
170000050928     c     d98duc        oreq      0
170100970310     C                   Z-ADD     D98DRF        D98DUC
170200970310     C                   Z-ADD     D98ORF        W98HUC
170300070605     C                   ENDIF
170400970304     C* DATA ARRIVO TRAZIONE
170500050922     C***                Z-ADD     D98DTI        WD2
170600050922     C***                Z-ADD     D98HTI        WO2
170700050922     C***  WDO1          IFLE      WDO2
170800050922     C***                Z-ADD     D98DRF        D98DTI
170900050922     C***                Z-ADD     D98ORF        D98HTI
171000050922     C***                ENDIF
171100970304     C* DATA CONSEGNA
171200050922     C***                Z-ADD     D98DCM        WD2
171300050922     C***                Z-ADD     D98HMC        WO2
171400050922     C***  WDO1          IFLE      WDO2
171500050922     C***                Z-ADD     D98DRF        D98DCM
171600050922     C***                Z-ADD     D98ORF        D98HMC
171700050922     C***                ENDIF
171800970304     C*
171900070605     C***                ENDIF                                                  <-- 2 ---
172000970304     C                   ENDIF                                                  <-- 1 ---
172100970304     C*
172200970304     C                   ENDSR
172300940901     C********************************************************************
172400940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
172500940901     C********************************************************************
172600940901     C     TDTIPO        BEGSR
172700940901     C*
172800940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
172900940902     C                   Z-ADD     D98TTI        W98TTI            3 0
173000941011     C*
173100941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
173200941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
173300941011     C     W98TTI        DIV       24            WDIV              3 0
173400941011     C                   MVR                     WRES              3 0
173500941011     C     WRES          IFEQ      *ZEROS
173600941011     C                   Z-ADD     *ZEROS        WASTA             9 0
173700941011     C                   ELSE
173800941011     C                   Z-ADD     12            WASTA
173900941011     C                   END
174000940901     C*
174100940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
174200940901     C     D98DTI        IFNE      *ZEROS
174300940901     C                   Z-ADD     D98DTI        WDAT
174400940901     C                   EXSR      DATORE
174500960911     C                   Z-ADD     WORE          WPODTI
174600960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
174700960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
174800960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
174900960911     C                   Z-ADD     WPODTI        W10POI            9 0
175000960911     C                   EXSR      R10FES
175100960911     C                   Z-ADD     W10POO        WPODTI
175200960911     C* AGGIUNGO LE ORA D'ASTA
175300960911     C                   ADD       WASTA         WPODTI
175400960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
175500960911     C     W10POI        IFEQ      W10POO
175600940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
175700940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
175800940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
175900940901     C                   EXSR      RO3ORE
176000940901     C                   ADD       WR3HHO        WPODTI
176100940901     C                   ADD       WR3HHO        W98TTI
176200940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
176300940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
176400940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
176500940901     C                   EXSR      RO3ORE
176600940901     C                   ADD       WR3HHO        WPODTI
176700940901     C                   ADD       WR3HHO        W98TTI
176800960911     C                   ENDIF                                                  *SE NON FESTIVO
176900940901     C                   END
177000940901     C*
177100940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
177200940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
177300940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
177400940901     C     W98TTI        DIV       24            WDIV              3 0
177500940901     C                   MVR                     WRES              3 0
177600940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
177700941228     C                   Z-ADD     W990IN        D98TCI
177800150630     c                   eval      wTSogliaE='0'
177900940901     C                   ELSE                                                   <-- 2 -->
178000941228     C                   Z-ADD     W991IN        D98TCI
178100150630     c                   eval      wTSogliaE='1'
178200940908     C                   ENDIF                                                  <-- 2 ---
178300090827     c* Sistemo i tempi di consegna se non sono multipli di 12
178400090827     c                   exsr      SisCONS
178500090827     c*
178600940908     C* RICALCOLO TEMPO DI RESA INTERNO
178700950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
178800950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
178900090921     c                   z-add     d98tri        w99tri
179000090921     c* per la DS
179100090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
179200090921     c* per i calcoli interni
179300090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
179400940908     C                   ENDIF                                                  <-- 1 ---
179500940901     C*
179600940901     C                   ENDSR
179700930325     C********************************************************************
179800930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
179900930325     C********************************************************************
180000930325     C     TDATPO        BEGSR
180100930406     C*
180200930420     C* TRASFORMO LA DATA ODIERNA
180300930416     C                   Z-ADD     WDAODI        WDAT
180400930416     C                   EXSR      DATORE
180500050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
180600960911     C                   MOVEL     'OSC  '       WR3TAB
180700960911     C                   Z-ADD     WHHODI        WR3HHI
180800960911     C                   EXSR      RO3ORE
180900050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
181000050922     c                   if        wr3hho=12
181100050922     c     wpoatt        add       12            wpoattdata24
181200050922     c                   else
181300050922     c                   z-add     wpoatt        wpoattdata24
181400050922     c                   endif
181500930325     C*
181600930420     C* TRASFORMO LA DATA DI SPEDIZIONE
181700931125     C     D98DSP        IFEQ      *ZEROS
181800930326     C                   MOVEL     '1'           D98ERR
181900930326     C                   MOVEL     MSG(2)        D98MSG
182000930326     C                   ELSE
182100931125     C                   Z-ADD     D98DSP        WDAT
182200930325     C                   EXSR      DATORE
182300930406     C                   Z-ADD     WORE          WPODSP
182400930326     C                   END
182500930414     C*
182600930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
182700930414     C                   MOVEL     'LIM  '       WR3TAB
182800930414     C                   Z-ADD     *ZEROS        WR3HHI
182900930414     C                   EXSR      RO3ORE
183000931222     C     WR3HHO        MULT      10            WTM060            6 0
183100931222     C     WPODSP        ADD       WTM060        WPOLIM
183200140822     c
183300930325     C*
183400930420     C* TRASFORMO LA DATA DI CONSEGNA
183500931125     C     D98DCM        IFNE      *ZEROS
183600931125     C                   Z-ADD     D98DCM        WDAT
183700930325     C                   EXSR      DATORE
183800930326     C                   Z-ADD     WORE          WPODCM
183900930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
184000930326     C                   MOVEL     'OSC  '       WR3TAB
184100931125     C                   Z-ADD     D98HMC        WR3HHI
184200930326     C                   EXSR      RO3ORE
184300930326     C                   ADD       WR3HHO        WPODCM
184400930326     C                   END
184500941228     C*
184600941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
184700941228     C     D98DUC        IFNE      *ZEROS
184800941228     C                   Z-ADD     D98DUC        WDAT
184900941228     C                   EXSR      DATORE
185000941228     C                   Z-ADD     WORE          WPODUC
185100970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
185200970310     C                   MOVEL     'OSP  '       WR3TAB
185300970310     C                   Z-ADD     W98HUC        WR3HHI
185400970310     C                   EXSR      RO3ORE
185500970310     C     D98DRF        IFNE      *ZEROS
185600050928     c     w98huc        andgt     1200
185700970310     C                   SUB       12            WR3HHO
185800970310     C                   ENDIF
185900970310     C                   ADD       WR3HHO        WPODUC
186000941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
186100070605     C***  WPODSP        IFGT      WPODUC
186200070605     C***                Z-ADD     WPODSP        WPODUC
186300070605     C***                ENDIF
186400941228     C                   ENDIF
186500960402     C*
186600141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
186700970304     C     D98DBR        IFNE      *ZEROS
186800970304     C                   Z-ADD     D98DBR        WDAT
186900960402     C                   EXSR      DATORE
187000960402     C                   Z-ADD     WORE          WPODBR
187100070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
187200960402     C                   ELSE
187300960402     C                   Z-ADD     WPODSP        WPODBR
187400960402     C                   ENDIF
187500930913     C*
187600960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
187700930913     C* CONSEGNABILITA'
187800960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
187900950407     C*
188000930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
188100931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
188200931125     C                   Z-ADD     D98DIC        WDAT
188300930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
188400930913     C                   Z-ADD     WORE          WPODIC
188500930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
188600931125     C                   Z-ADD     D98OIC        WR3HHI
188700930913     C                   EXSR      RO3ORE
188800930913     C                   ADD       WR3HHO        WPODIC
188900931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
189000950407     C*
189100930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
189200931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
189300931125     C                   Z-ADD     D98DFC        WDAT
189400930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
189500930913     C                   Z-ADD     WORE          WPODFC
189600140822     C                   Z-ADD     WORE          WPODFC_00
189700930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
189800931125     C                   Z-ADD     D98OFC        WR3HHI
189900930913     C                   EXSR      RO3ORE
190000930913     C                   ADD       WR3HHO        WPODFC
190100931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
190200930913     C*
190300930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
190400930913     C     D98ERR        IFEQ      '0'
190500930913     C     WPODFC        IFEQ      *ZEROS
190600930913     C     WPODIC        ANDNE     *ZEROS
190700930913     C                   MOVEL     '1'           D98ERR
190800930913     C                   MOVEL     MSG(17)       D98MSG
190900930913     C                   ENDIF
191000930913     C                   ENDIF
191100930907     C*
191200930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
191300930913     C     D98ERR        IFEQ      '0'
191400930913     C     WPODIC        IFGT      WPODFC
191500930907     C                   MOVEL     '1'           D98ERR
191600930913     C                   MOVEL     MSG(18)       D98MSG
191700930913     C                   ENDIF
191800930913     C                   ENDIF
191900950407     C*
192000950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
192100930511     C*
192200930511     C                   ENDSR
192300950110     C********************************************************************
192400950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
192500950110     C********************************************************************
192600950110     C     CALDPP        BEGSR
192700950110     C*
192800950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
192900950110     C                   MOVEL     'P'           WTC                            *PARTENZA
193000950110     C                   MOVEL     'I'           WIC                            *INTERNA
193100960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
193200950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
193300950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
193400950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
193500950110     C*
193600950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
193700950110     C                   MOVEL     'P'           WTC                            *PARTENZA
193800950110     C                   MOVEL     'C'           WIC                            *CLIENTE
193900950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
194000950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
194100950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
194200950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
194300960229     C*
194400960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
194500050922     c
194600050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
194700050922     c
194800050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
194900090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
195000050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
195100090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
195200050922%%%  C*  ritardo alla partenza.
195300050922     c* Il tutto solo se la data di oggi > della
195400050922     c*  data teorica di partenza determinata
195500050922     c*
195600950110%%%  C     D98ERR        IFEQ      '0'
195700950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
195800950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
195900950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
196000050922     c     wpoattData24  andgt     wpoppi
196100960911%%%  C********             MOVEL'OSC  '   WR3TAB
196200960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
196300960911%%%  C********             EXSR RO3ORE
196400960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
196500050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
196600090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
196700090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
196800950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
196900050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
197000050922     c* La nuova data calcolata non può essere < della minima prevista
197100050922     c                   if        wr0poo>wpoppi
197200050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
197300050922     c                   endif
197400050922     c
197500950110%%%  C                   ENDIF
197600950110%%%  C                   ENDIF
197700950110%%%  C*
197800950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
197900050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
198000050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
198100050915%%%  C*  alla prtenza.
198200950110%%%  C     D98ERR        IFEQ      '0'
198300950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
198400950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
198500950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
198600950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
198700950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
198800950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
198900050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
199000950110%%%  C                   ENDIF
199100950110%%%  C                   ENDIF
199200950110%%%  C*
199300950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
199400950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
199500950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
199600950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
199700950110%%%  C     D98ERR        IFEQ      '0'
199800950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
199900950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
200000950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
200100950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
200200090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
200300090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
200400950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
200500050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
200600950110%%%  C                   ENDIF
200700950110%%%  C                   ENDIF
200800960229     C*
200900960229     C                   ENDIF                                                  <-- 1 ---
201000950110     C*
201100950110     C                   ENDSR
201200930604     C********************************************************************
201300930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
201400930604     C********************************************************************
201500930604     C     CALDPT        BEGSR
201600090902     c                   clear                   RicalcoloTRAZ     1
201700930604     C*
201800930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
201900931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
202000931129     C                   MOVEL     'I'           WIC                            *INTERNA
202100050915     c* se non presente la data partenza ultimo collo, uso la teorica
202200950110     C     D98DUC        IFEQ      *ZEROS
202300950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
202400950110     C                   ELSE
202500950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
202600950110     C                   ENDIF
202700930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
202800930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
202900930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
203000930604     C*
203100930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
203200931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
203300931129     C                   MOVEL     'C'           WIC                            *CLIENTE
203400950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
203500930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
203600930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
203700930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
203800150508
203900150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
204000150508     c* se presente la data partenza PRIMO  collo --> la uso
204100150508     c                   if        w98dpc=0
204200150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
204300150508     c                   else
204400150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
204500150508     C                   MOVEL     'C'           WIC                            *INTERNA
204600150508     C                   Z-ADD     W98DPC        WDAT
204700150508     C                   EXSR      DATORE
204800150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
204900150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
205000150508     C                   EXSR      RO4CDP
205100150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
205200150508     c                   endif
205300960229     C*
205400960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
205500940624%%%  C*
205600050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
205700050915
205800940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
205900050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
206000050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
206100050915     c*   attribuito alla trazione
206200940624%%%  C     D98ERR        IFEQ      '0'
206300940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
206400940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
206500090903     c     wpoatt        andge     wpopti
206600050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
206700090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
206800090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
206900940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
207000050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
207100050922     c                   if        wr0poo>wpopti
207200050922     c                   eval      wpopti=wr0poo
207300050922%%%  C                   endif
207400090902     c*
207500090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
207600090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
207700090902     c*  della trazione udate e utime
207800090902     c                   eval      RicalcoloTRAZ='S'
207900090902     c
208000940624%%%  C                   ENDIF
208100940624%%%  C                   ENDIF
208200940628%%%  C*
208300050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
208400050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
208500050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
208600940628%%%  C     D98ERR        IFEQ      '0'
208700940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
208800940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
208900940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
209000090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
209100090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
209200940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
209300050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
209400940628%%%  C                   ENDIF
209500940628%%%  C                   ENDIF
209600960229     C*
209700960229     C                   ENDIF                                                  <-- 1 ---
209800930604     C*
209900930604     C                   ENDSR
210000930511     C********************************************************************
210100930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
210200930511     C********************************************************************
210300930511     C     CALDCR        BEGSR
210400050914     c                   clear                   consideraora
210500100308     c
210600100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
210700100308     c*
210800100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
210900100308     c* 1) se presente solo ORA --> ignoro sempre
211000100308     c* 2) se presente data e ora IL --> ignoro l'ora
211100100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
211200100309     c***                if        d98tsp='E'
211300100309     c***                clear                   d98dcr
211400100309     c***                clear                   d98hcr
211500100309     c**+                clear                   d98tcr
211600100309     c***                endif
211700100308     c
211800100308     c                   if        d98hcr>0
211900100308     c
212000100308     c                   if        d98dcr=0
212100100308     c                   clear                   d98hcr
212200100308     c                   endif
212300100308
212400100308     c                   if        d98dcr>0 and d98tcr=' '
212500100308     c                   clear                   d98hcr
212600100308     c                   endif
212700100308     c                   endif
212800941228     C*
212900941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
213000941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
213100050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
213200050901     c*  (per uguale si intende entrambe nel mattino o entrambe
213300050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
213400050901     c*  (EDPES 1/9/05)
213500050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
213600050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
213700050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
213800050831     C***                Z-ADD     D98DCM        D98DCR
213900050831     C***                ENDIF
214000050901     c*
214100070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
214200050914     c
214300100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
214400100308     c***                clear                   d98hcr
214500100308     c***                clear                   d98tcr
214600100308     c***                endif
214700050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
214800050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
214900070521     c*  richiesta dal cliente
215000100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
215100100308     C***                MOVEL     'OSC  '       WR3TAB
215200100308     C***                Z-ADD     D98HMC        WR3HHI
215300100308     C***                EXSR      RO3ORE
215400100308     c***                z-add     wr3hho        salvaoreHMC
215500050901     c
215600100308     C***                Z-ADD     D98Hcr        WR3HHI
215700100308     C***                EXSR      RO3ORE
215800100308     c***                z-add     wr3hho        salvaoreHCR
215900100308     c***
216000050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
216100100308    2c***                if        salvaoreHMC=salvaoreHCR
216200100308     C***                eval      Consideraora='S'
216300100308     c***                else
216400100308     C***                eval      Consideraora=' '
216500100308     c***                clear                   d98hcr
216600100308    2c***                endif
216700100308    1c***                endif
216800930511     C*
216900930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
217000931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
217100931125     C                   Z-ADD     D98DCR        WDAT
217200930325     C                   EXSR      DATORE
217300930326     C                   Z-ADD     WORE          WPODCR
217400930420     C*
217500930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
217600930420     C*
217700931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
217800930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
217900930326     C                   MOVEL     'CRD  '       WR3TAB
218000931125     C                   Z-ADD     D98HCR        WR3HHI
218100930419     C                   EXSR      RO3ORE
218200930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
218300940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
218400050902     c
218500931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
218600930420     C                   MOVEL     'CRD1 '       WR3TAB
218700930420     C                   Z-ADD     *ZEROS        WR3HHI
218800930420     C                   EXSR      RO3ORE
218900931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
219000090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
219100090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
219200050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
219300060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
219400050907     c                   z-add     worecr        oreILoDOPO
219500931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
219600931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
219700931207     C                   MOVEL     'C'           WIC                            *CLIENTE
219800931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
219900150330     C                   Z-ADD     WORECR        WR4NOI
220000931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
220100931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
220200931209     C     WPOCRF        SUB       12            WR7POI            9 0
220300931209     C                   MOVEL     'X'           WIC
220400931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
220500931209     C                   ADD       12            WR7POO
220600931207     C     WR7POO        IFGT      WPOCRF
220700931207     C                   Z-ADD     WR7POO        WPOCRF
220800931207     C                   END
220900150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
221000150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
221100150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
221200150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
221300150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
221400150330     C                   MOVEL     'C'           WIC                            *CLIENTE
221500150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
221600150330     C                   Z-ADD     w99TCC        WR4NOI
221700150330     c                   sub       12            wr4noi
221800150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
221900150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
222000150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
222100150330     c
222200150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
222300150330     C                   MOVEL     'X'           WIC
222400150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
222500150330     C******             ADD       12            WR7POO
222600150330     C     WR7POO        IFGT      WPOCRF_dee
222700150330     C                   Z-ADD     WR7POO        WPOCRF_dee
222800150330     C                   END
222900050902     c
223000050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
223100050902     c*  sono le ore di fine della non consegnabilità
223200050905     c* Se aggiungendo 1 finisco in giorno festivo
223300050905     c*  cerco primo giorno non festivo
223400050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
223500050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
223600050907     C****               MOVEL     'C'           WIC                            *CLIENTE
223700050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
223800050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
223900050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
224000050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
224100050907     c
224200050907     c                   add       1             wpocri
224300050902     c
224400050902   FFC                   Z-ADD     WPOCRI        WPOCRC
224500050902     c
224600930325     C                   END
224700150330     C*
224800931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
224900930420     C* DATA SPED. = DATA INIZIO RANGE
225000930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
225100940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
225200940324£$   C                   Z-ADD     WPOCRI        WPOCRC
225300930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
225400930420     C                   MOVEL     'CRP  '       WR3TAB
225500931125     C                   Z-ADD     D98HCR        WR3HHI
225600930420     C                   EXSR      RO3ORE
225700931207     C     WPODCR        ADD       WR3HHO        WPOCRF
225800931207     C                   SUB       24            WPOCRF                         *FINE RANGE
225900931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
226000931207     C* ANDANDO INDIETRO NEL TEMPO
226100050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
226200050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
226300050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
226400050907     c* data utile venedì sera
226500130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
226600130411     C                   MOVEL     'X'           WIC                            *CLIENTE
226700931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
226800931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
226900931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
227000930326     C                   END
227100930526     C*
227200931125     C     D98TCR        IFEQ      'P'
227300931125     C     D98TCR        OREQ      'D'
227400930526     C                   ELSE                                                   *CONS.RICH.IL ..
227500940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
227600940324£$   C                   Z-ADD     WPODCR        WPOCRC
227700930526     C* DATA SPED. = DATA INIZIO RANGE
227800050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
227900050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
228000050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
228100930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
228200930526     C                   MOVEL     'CRI  '       WR3TAB
228300931125     C                   Z-ADD     D98HCR        WR3HHI
228400930526     C                   EXSR      RO3ORE
228500050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
228600050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
228700050907     c                   z-add     wr3hho        oreILoDOPO
228800930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
228900050902     c
229000050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
229100050902     c                   if        wr3MATPOM='P' and d98hcr>0
229200050902     c                   add       12            wpocri
229300100308     c* tolgo 12 ore dalle ore conseganbili
229400050907     c                   sub       12            oreILoDOPO
229500050902     c                   endif
229600050902     c
229700931209     C     WPOCRF        SUB       12            WR7POI            9 0
229800931209     C                   MOVEL     'X'           WIC
229900931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
230000931209     C                   ADD       12            WR7POO
230100931209     C     WR7POO        IFGT      WPOCRF
230200931209     C                   Z-ADD     WR7POO        WPOCRF
230300931209     C                   END
230400050905     c
230500050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
230600050905     c*  sono le ore di fine della non consegnabilità
230700050905     c* Se aggiungendo 1 finisco in giorno festivo
230800050905     c*  cerco primo giorno non festivo
230900050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
231000050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
231100050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
231200050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
231300050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
231400050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
231500050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
231600050907     c
231700050907     c                   add       1             wpocri
231800050905     c
231900050905   FFC                   Z-ADD     WPOCRI        WPOCRC
232000050905     c
232100930526     C                   END
232200931206     C*
232300931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
232400931206     C* LA IMPOSTO COME LA CONS.TEORICA
232500140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
232600140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
232700931206     C                   ELSE
232800931206     C                   Z-ADD     WPOCRF        WPOCR1
232900931206     C                   END
233000150608     c* ai fini del calcolo affidabilità
233100150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
233200150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
233300931206     C                   ELSE
233400931206     C                   Z-ADD     WPOCRF        WPOCR2
233500931206     C                   END
233600150608     c* ai fini della data prevista consegna cliente
233700150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
233800150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
233900150608     C                   ELSE
234000150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
234100150608     C                   END
234200150331     c
234300150331     c                   if        wpocrf_dee>0
234400150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
234500150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
234600150330     C                   END
234700150331     C                   END
234800150331     C*
234900150331     c                   if        wpocrf_dee=0
235000150608     C     wpocr2_dee    sub       24            WPOCRF_dee
235100150331     c                   endif
235200150331     c
235300930526     C                   END                                                    <-- 1 ---
235400930511     C*
235500930511     C                   ENDSR
235600930511     C********************************************************************
235700930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
235800930511     C********************************************************************
235900930511     C     CALLAV        BEGSR
236000930511     C*
236100941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
236200930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
236300930421     C*
236400930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
236500930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
236600930421     C     1             DO        IE            E                 3 0          --- 2 -->
236700930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
236800930421     C                   Z-ADD     1             I
236900930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
237000060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
237100930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
237200930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
237300930421     C                   ADD       1             IL
237400930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
237500930421     C                   EXSR      DATORE
237600930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
237700091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
237800091014     c* Mi salvo la posizione dell'evento
237900091014     C                   Z-ADD     E             LAp(IL)
238000930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
238100930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
238200930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
238300930421     C                   EXSR      RO3ORE
238400930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
238500930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
238600930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
238700930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
238800091015     c**                 if        §2agla>1
238900091015     c**                 sub       1             §2agla
239000930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
239100091015     c**                 else
239200091015     c**                 clear                   whh
239300091015     c**                 endif
239400930510     C                   ADD       WHH           WORELA
239500940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
239600930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
239700090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
239800150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
239900930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
240000931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
240100931129     C                   MOVEL     'C'           WIC                            *CLIENTE
240200930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
240300930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
240400930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
240500930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
240600931209     C                   Z-ADD     LAF(IL)       WR7POI
240700931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
240800931202     C     WR7POO        IFGT      LAF(IL)
240900931201     C                   Z-ADD     WR7POO        LAF(IL)
241000931202     C                   END
241100930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
241200930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
241300091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
241400930510     C                   END                                                    <-- 3 ---
241500931223     C                   ENDDO                                                  <-- 2 ---
241600930421     C*
241700930421     C                   END                                                    <-- 1 ---
241800930618££$$ C*
241900930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
242000931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
242100941228££$$ C     1             DO        IL            I
242200930618££$$ C     LAF(I)        IFGT      WPOLAF
242300930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
242400930618££$$ C                   END
242500931223££$$ C                   ENDDO
242600930511     C*
242700930511     C                   ENDSR
242800940324£$   C********************************************************************
242900940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
243000940324£$   C********************************************************************
243100940324£$   C     CALLAC        BEGSR
243200941229     C*
243300950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
243400950407     C*
243500950210     C                   Z-ADD     WORELA        WTM060
243600941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
243700941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
243800941229     C     D98DAG        IFEQ      *ZEROS
243900050520     c     §2aftg        andeq     'S'
244000090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
244100090827     C     WORELA        ADD       w99TCC        WTM060            6 0
244200941229     C                   ENDIF
244300941229     C*
244400940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
244500940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
244600941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
244700941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
244800941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
244900940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
245000941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
245100050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
245200050520     c                   movel     §2aftg        LCON(IL)
245300950407     C*
245400950407     C                   ENDIF
245500960129     C*
245600940324£$   C                   ENDSR
245700930511     C********************************************************************
245800930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
245900930511     C********************************************************************
246000930511     C     CALGIA        BEGSR
246100960129     C*
246200960129     C*-------------------------------------------------------------------
246300960129     C* EVENTI GIACENZA
246400960129     C*-------------------------------------------------------------------
246500960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
246600960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
246700960129     C*
246800050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
246900041116     c     d98nsp        ifgt      0
247000031125     C                   EXSR      CARGIAC
247100041116     c                   endif
247200031125     C*
247300960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
247400960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
247500960129     C     1             DO        IE            E                 3 0          --- 2 -->
247600960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
247700960129     C                   Z-ADD     1             I
247800960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
247900060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
248000960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
248100960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
248200960129     C                   ADD       1             IG
248300960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
248400960129     C                   EXSR      DATORE
248500960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
248600140606     c* Mi salvo la posizione dell'evento
248700140606     C                   Z-ADD     E             gip(IG)
248800960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
248900960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
249000960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
249100960129     C                   EXSR      RO3ORE
249200960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
249300960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
249400960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
249500960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
249600960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
249700960129     C                   ADD       WHH           WOREGI
249800960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
249900960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
250000090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
250100090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
250200960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
250300960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
250400960129     C                   MOVEL     'C'           WIC                            *CLIENTE
250500960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
250600960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
250700960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
250800960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
250900960129     C                   Z-ADD     GIF(IG)       WR7POI
251000960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
251100031125     C     WR7POO        IFGT      GIF(IG)
251200031125     C                   Z-ADD     WR7POO        GIF(IG)
251300031125     C                   ENDIF
251400031201     C*
251500031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
251600031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
251700031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
251800031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
251900031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
252000031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
252100031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
252200031201     C                   IF        WPOATT > GIF(IG)
252300031201     C*
252400031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
252500031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
252600031201     C                   IF        WGGA = *BLANKS
252700031201     C                   SETOFF                                       54
252800031201     C                   ELSE
252900031201     C     WGGA          LOOKUP    SK7QW                                  54
253000031201     C                   ENDIF
253100031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
253200031201     C                   MOVEL     '1'           FLGGIACOK
253300031201     C                   ELSE
253400140127     c
253500031125     C*
253600031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
253700031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
253800031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
253900031125     C                   MOVEL     '0'           FLGGIACOK         1
254000031125     C*
254100031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
254200031125     C     1             DO        IAG           EG                3 0
254300031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
254400031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
254500031125     C                   EXSR      DATORE
254600031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
254700031125     C*
254800031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
254900031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
255000031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
255100031125     C                   EXSR      DATORE
255200031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
255300031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
255400031229     C* => NN LO CONSIDERO
255500031229     C                   IF        EVC(E+1) = 'G' AND
255600031229     C                             WPROXGII = WGCPDORE
255700031229     C                   MOVEL     '1'           FLGGIACOK
255800031229     C                   ENDIF
255900031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
256000031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
256100031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
256200031125     C                   EXSR      RO3ORE
256300031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
256400031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
256500031125     C* DELL'EVENTO GIACENZA CORRENTE
256600031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
256700060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
256800031125     C                   MOVEL     '1'           FLGGIACOK
256900031125     C                   LEAVE
257000031125     C                   ENDIF
257100031125     C                   ELSE
257200031229     C                   IF        EVC(E)  = 'G' AND
257300031229     C                             GII(IG) = WGCPDORE
257400031229     C                   MOVEL     '1'           FLGGIACOK
257500031229     C                   LEAVE
257600031229     C                   ENDIF
257700031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
257800031125     C                   MOVEL     '1'           FLGGIACOK
257900031125     C                   LEAVE
258000031125     C                   ENDIF
258100031125     C                   ENDIF
258200031125     C                   ENDDO
258300031201     C                   ENDIF
258400140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
258500140127     C                   IF        FLGGIACOK = '0'  and
258600140127     c                             d98dcm=evd(E)
258700140127     C                   MOVEL     '1'           FLGGIACOK
258800140127     c                   endif
258900031125     C*
259000031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
259100031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
259200031125     C* DELL'EVENTO GIACENZA CORRENTE
259300031125     C                   IF        FLGGIACOK = '0'
259400031125     C                   Z-ADD     *ZEROS        GIF(IG)
259500031125     C                   ELSE
259600031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
259700031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
259800031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
259900031125     C                   ENDIF
260000031125     C                   ELSE
260100031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
260200031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
260300031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
260400031125     C                   ENDIF
260500031125     C*
260600960129     C                   ENDIF                                                  <-- 3 ---
260700960129     C                   ENDDO                                                  <-- 2 ---
260800960129     C*
260900960129     C                   ENDIF                                                  <-- 1 ---
261000960129££$$ C*
261100960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
261200960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
261300960129££$$ C     1             DO        IG            I
261400960129££$$ C     GIF(I)        IFGT      WPOGIF
261500960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
261600960129££$$ C                   ENDIF
261700960129££$$ C                   ENDDO
261800941229     C*
261900941229     C*-------------------------------------------------------------------
262000941229     C* APERTURA GIACENZA
262100941229     C*-------------------------------------------------------------------
262200960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
262300960129     C*
262400960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
262500931125     C                   Z-ADD     D98DAG        WDAT
262600930325     C                   EXSR      DATORE
262700950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
262800960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
262900950413£££  C                   Z-ADD     WORE          WPODAG
263000960129$$£  C*********************ENDIF
263100930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
263200930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
263300930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
263400930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
263500930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
263600941229     C*
263700941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
263800941229     C                   Z-ADD     WPODAG        WPOAGC
263900960129     C*
264000960129     C                   ENDIF                                                  <-- 1 ---
264100941229     C*
264200941229     C*-------------------------------------------------------------------
264300930511     C* CHIUSURA GIACENZA
264400941229     C*-------------------------------------------------------------------
264500930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
264600930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
264700931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
264800931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
264900931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
265000931201     C                   Z-ADD     D98DCM        W98DCG
265100930416     C                   ELSE
265200930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
265300930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
265400931201     C                   Z-ADD     WDAODI        W98DCG
265500061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
265600061123     c*  chiusura giacenza, come già fa in altri posti
265700061123     c                   if        d98dfc>0
265800061123     C                   Z-ADD     d98dfc        W98DCG
265900061123     c                   endif
266000061123     c
266100930409     C                   END
266200930409     C                   END
266300930409     C                   END
266400930414     C* TRASF.DATA CHIUSURA
266500931201     C     W98DCG        IFNE      *ZEROS
266600931201     C                   Z-ADD     W98DCG        WDAT
266700930325     C                   EXSR      DATORE
266800930326     C                   Z-ADD     WORE          WPODCG
266900930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
267000930407     C* GIORNO DA ESCLUDERE
267100150216     C                   ADD       12            WPODCG
267200150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
267300150123     c*  il giorno di chiusura dovrebe già essere protetto
267400941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
267500941229     C     WPOCGC        IFEQ      *ZEROS
267600941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
267700941229     C                   ENDIF
267800930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
267900930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
268000931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
268100931129     C                   MOVEL     'C'           WIC                            *CLIENTE
268200150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
268300150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
268400930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
268500930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
268600930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
268700931209     C                   Z-ADD     WPODCG        WR7POI
268800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
268900931202     C     WR7POO        IFGT      WPODCG
269000931201     C                   Z-ADD     WR7POO        WPODCG
269100930326     C                   END
269200931202     C                   END
269300930511     C*
269400930511     C                   ENDSR
269500031125     C********************************************************************
269600031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
269700031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
269800031125     C*             DEGLI EVENTI DI TIPO GIACENZA
269900031125     C********************************************************************
270000031125     C     CARGIAC       BEGSR
270100031125     C*
270200031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
270300031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
270400031125     C*
270500050405     C                   Z-ADD     D98AAS        KWGCPAAS
270600050405     C                   Z-ADD     D98LNP        KWGCPLNP
270700050405     C                   Z-ADD     D98NRS        KWGCPNRS
270800050405     C                   Z-ADD     D98NSP        KWGCPNSP
270900031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
271000050405     C     KEYGC_P       SETLL     TIGCP01L
271100050405     C                   IF        %equal(TIGCP01L)
271200050405     C     KEYGC_P       READE     TIGCP01L
271300050405     C                   DOW       not %eof(TIGCP01L)
271400031125     C                   ADD       1             IAG
271500031125     C                   EVAL      GCPD(IAG) = GCPDUR
271600050405     C     KEYGC_P       READE     TIGCP01L
271700031125     C                   ENDDO
271800031125     C                   ENDIF
271900031125     C                   ELSE
272000050405     C     KEYGC_P       SETLL     TIGCP21L
272100050405     C                   IF        %equal(TIGCP21L)
272200050405     C     KEYGC_P       READE     TIGCP21L
272300050405     C                   DOW       not %eof(TIGCP21L)
272400031125     C                   ADD       1             IAG
272500031125     C                   EVAL      GCPD(IAG) = GCPDUR
272600050405     C     KEYGC_P       READE     TIGCP21L
272700031125     C                   ENDDO
272800031125     C                   ENDIF
272900031125     C                   ENDIF
273000031125     C*
273100031125     C                   ENDSR
273200941229     C********************************************************************
273300960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
273400941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
273500941229     C********************************************************************
273600960129     C     CALGIC        BEGSR
273700941229     C*
273800960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
273900960129     C*
274000960129     C                   Z-ADD     WOREGI        WTM060
274100960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
274200960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
274300960129     C     D98DAG        IFEQ      *ZEROS
274400960129$$£  C     WOREGI        ADD       12            WTM060            6 0
274500960129     C                   ENDIF
274600960129     C*
274700960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
274800960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
274900960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
275000960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
275100960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
275200960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
275300960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
275400960129     C*
275500960129     C                   ENDIF
275600941229     C*
275700941229     C                   ENDSR
275800060418     C********************************************************************
275900060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
276000060418     c*            l'affidabilità
276100060418     C********************************************************************
276200060418     C     CALCEPA       BEGSR
276300060418     C*
276400060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
276500060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
276600060420     c                   clear                   numggcepa         3 0
276700060418     C*
276800060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
276900060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
277000060420     c                   eval      wgiroepa='0'
277100060418     c                   eval      wfine=' '
277200060418     c
277300060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
277400060420    2C     W             DOwle     IP                                           --- 2 -->
277500060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
277600060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
277700060418     c
277800060420    4c                   if        wgiroepa='0'
277900060420     c                   exsr      ElaInizCEPA
278000060418   x4c                   else
278100060420    5c                   if        pvd(W)>wdat
278200060418     c                   eval      wfine='S'
278300060418    5c                   endif
278400060418    4c                   endif
278500060418     c
278600060420    4c                   if        wfine=' ' and pvd(W)=wdat
278700060420     C*
278800060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
278900060420     c*  finale
279000060420     c                   add       1             numggcepa
279100060420     c
279200060420     C                   z-add     24            WORELA
279300060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
279400060418     c
279500060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
279600060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
279700060418     C                   MOVEL     'C'           WIC                            *CLIENTE
279800060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
279900060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
280000060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
280100060418     c
280200060418     c* Trasformo le ore in data e verifico se c'e' altro evento
280300060418     c*  in sta data che allunga ancora
280400060420     c                   z-add     wr4poo        wore
280500060418     c                   EXSR      OREDAT
280600060418    4c                   endif
280700060418     c
280800060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
280900060418    4c                   if        wfine='S'
281000060418     c                   EXSR      ELAFINECEPA
281100060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
281200060420     c*  inizio range
281300060420     c                   else
281400060420     c                   add       1             w
281500060418    4C                   ENDif                                                  <-- 3 ---
281600060420     c
281700060420     c                   else
281800060420     c                   add       1             w
281900060418    3C                   ENDif                                                  <-- 3 ---
282000060418    2C                   ENDDO                                                  <-- 2 ---
282100060418     c
282200060420     c                   if        wgiroepa='1'
282300060418     c                   EXSR      ELAFINECEPA
282400060418     c                   endif
282500060418     C*
282600060418    1C                   END                                                    <-- 1 ---
282700060418££$$ C*
282800060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
282900060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
283000060420££$$ C     1             DO        Ipa           w
283100060420££$$ C     paif(w)       IFGT      WPOpaif
283200060420££$$ C                   Z-ADD     paif(w)       WPOpaif
283300060418££$$ C                   END
283400060418££$$ C                   ENDDO
283500060418     C*
283600060418     C                   ENDSR
283700060420     C********************************************************************
283800060420     c* Elabora Inizio range eventi particolari che allungano
283900060420     C********************************************************************
284000060420     c     ElaInizCEPA   BEGSR
284100060420     C                   ADD       1             IPA
284200060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
284300060420     C                   EXSR      DATORE
284400060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
284500061017     c
284600060420     c                   eval      wgiroepa='1'
284700060420     c                   eval      wdat=pvd(w)
284800060420     C                   ENDSR
284900060418     C********************************************************************
285000060418     c* Elabora fine range eventi particolari che allungano
285100060418     C********************************************************************
285200060418     c     ElafineCEPA   BEGSR
285300060420     c     numggcepa     mult      24            worela
285400060420     c                   sub       12            worela
285500090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
285600090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
285700060418     c
285800060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
285900060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
286000060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
286100060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
286200060418     C                   Z-ADD     paif(IPA)     WR7POI
286300060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
286400060418    5C     WR7POO        IFGT      paif(IPA)
286500060418     C                   Z-ADD     WR7POO        paif(IPA)
286600060418    5C                   END
286700061017     c*Sommo 24 ore di inizio giornata
286800061017     c*            : non ho capito perchè ma ci vogliono
286900061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
287000090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
287100090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
287200061017     c* Se tempi di consegna inferiori alle 24 ore , devo
287300061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
287400061017     c*  partire prima
287500061017     c****               if        d98tcc<24
287600061017     c****               sub       12            paii(ipa)
287700061017     c****               endif
287800060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
287900060418     c* Imposto nuovo giro
288000060420     c                   eval      wgiroepa='0'
288100060418     c                   eval      wfine=' '
288200060420     c                   clear                   numggcepa         3 0
288300060418     c                   ENDSR
288400930511     C********************************************************************
288500930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
288600930511     C********************************************************************
288700010822     C     CALASS        BEGSR
288800010821     C*
288900011211     C                   EXSR      REPMAXCSB
289000930511     C*
289100930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
289200010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
289300011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
289400930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
289500930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
289600930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
289700930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
289800930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
289900930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
290000930511     C                   ELSE                                                   <-- 4 -->
2901009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
290200930512     C                   END                                                    <-- 4 ---
290300930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
290400930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
290500930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
290600930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
290700930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
290800930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
290900930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
291000930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
291100930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
291200930413     C                   END                                                    <-- 2 ---
291300011211     C                   END                                                    <-- 1 ---
291400930406     C*
291500930325     C                   ENDSR
291600930608     C********************************************************************
291700930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
291800930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2919009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
292000930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
292100930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
292200930608     C********************************************************************
292300930608     C     RDRASS        BEGSR
292400930608     C*
292500930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2926009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2927009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2928009407151507 C*********************EXSR RO3ORE
2929009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
293000930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2931009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2932009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2933009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2934009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2935009407191907 C                   Z-ADD     WPOASI        WPOROU
2936009407191907 C                   EXSR      CHKFES
2937009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2938009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2939009407191907 C                   ELSE                                                   *E' FERIALE
2940009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2941009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
2942009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
2943009407191907 C                   ENDIF
294400930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
294500930608     C                   MOVEL     'ASS1 '       WR3TAB
294600930608     C                   Z-ADD     *ZEROS        WR3HHI
294700930608     C                   EXSR      RO3ORE
294800930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
294900930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
295000931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
295100931129     C                   MOVEL     'C'           WIC                            *CLIENTE
295200930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
295300930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
295400930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
295500930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
295600930608     C*
295700930608     C                   ENDSR
295800930511     C********************************************************************
295900930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
296000930511     C********************************************************************
296100930511     C     CALFED        BEGSR
296200150331     c                   clear                   fddata
296300150331     c                   clear                   WPOFDdata
296400930511     C*
296500051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
296600931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
296700051108     C     D98FFD        oreq      'M'                                          --- 1 -->
296800930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
296900930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
297000930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
297100930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
297200150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
297300150331     c                   if        §ar5dfd>*zeros
297400150331     c                   movel     §ar5dfd       fddata            8 0
297500150331     c                   endif
297600150331     c                   if        fddata>0 and ((d98dti>0 and
297700150331     c                             fddata>d98dti) or d98dti=0)
297800150331     C                   Z-ADD     fddata        WDAT
297900150331     C                   EXSR      DATORE
298000150331     C                   Z-ADD     WORE          WPOfddata
298100150331     c                   endif
298200150331     c
298300930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
298400930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
298500930511     C                   ELSE                                                   <-- 2 -->
2986009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
298700930512     C                   END                                                    <-- 2 ---
298800150331     c                   if        wpofddata>0 and wpofddata>wpofdi
2989001504081507 C                   Z-ADD     WPOfddata     WPOFDI
299000150331     c                   endif
299100150331
299200930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
299300930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
299400930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
299500930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
299600930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
299700930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
299800930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
299900930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
300000930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
300100930511     C                   END                                                    <-- 1 ---
300200930511     C*
300300930511     C                   ENDSR
300400930608     C********************************************************************
300500930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
300600930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3007009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
300800930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
300900930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
301000930608     C********************************************************************
301100930608     C     RDRFED        BEGSR
301200930608     C*
301300930608     C* CALCOLO ORE DEL FERMO DEPOSITO
301400930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3015009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3016001503311907 C                   Z-ADD     WPOFDI        WPOROU
3017001503311907 C                   EXSR      CHKFES
3018001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3019001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3020001503311907 C                   ELSE                                                   *E' FERIALE
3021001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3022001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3023001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3024001503311907 C                   ENDIF
302500150331     c* Se presente la particolarità consegna con più giorni di fermo depo
302600150331     c*  uso questi
302700150331     c                   if        §7rggfd>*zeros
302800150424     c                   movel     §7rggfd       wggfd             2 0
302900150331     c     wggfd         mult      24            Wnore
303000150331     c                   add       wnore         worefd
303100150331     c                   else
303200930608     C                   MOVEL     'FED1 '       WR3TAB
303300930608     C                   Z-ADD     *ZEROS        WR3HHI
303400930608     C                   EXSR      RO3ORE
303500940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
303600150331     c                   endif
303700930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
303800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
303900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
304000930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
304100930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
304200930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
304300930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
304400930608     C*
304500930608     C                   ENDSR
304600931201     C********************************************************************
304700931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
304800931201     C********************************************************************
304900931201     C     CALDTE        BEGSR
305000140825     c
305100140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
305200140825     c*  che tiene conto solo dei festivi per restituire in output
305300140825     c*  la data teorica di consegna
305400140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
305500140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
305600140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
305700140825     C                   ELSE
305800140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
305900140825     C                   END
306000140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
306100141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
306200141106     c*  quindi passo un flag
306300141106     c                   eval      nosomma='N'
306400140825     C                   EXSR      RO4CDP
306500141106     c                   eval      nosomma=' '
306600140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
306700140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
306800140825     c*  di consegna
306900140825     c                   EXSR      TRAS_DEI
307000140825     c
307100140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
307200140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
307300140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
307400140825     c                             ='M' or %subst(d98gc2:2:1)='P')
307500140825     c                   exsr      Reper_tempi
307600140825     c                   endif
307700931201     C*
307800140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
307900140825     c*  che tiene conto di:
308000140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
308100150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
308200931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
308300931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
308400931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
308500931201     C                   ELSE
308600931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
308700931201     C                   END
308800090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
308900090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
309000931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
309100140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
309200931201     C*
309300150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
309400931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
309500931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
309600090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
309700931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
309800150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
309900150608     c
310000150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
310100150608     c*  che non tiene conto dei tempi del C/Ass
310200150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
310300150609     c*
310400150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
310500150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
310600150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
310700150630     c                   Z-ADD     WPODTI        WR6POI
310800150609     c                   else
310900150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
311000150609     c                   endif
311100150609     c
311200150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
311300150608     c                   eval      wdee='S'
311400150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
311500150608     c                   eval      wdee=' '
311600150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
311700141017
311800141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
311900150219     C**                 EVAL      WTC='C'
312000150219     C**                 EVAL      WIC='C'
312100150219     c**                 z-add     wpoattdata00  WPOROU
312200150219     c**                 z-add     wdaodi        wpodata
312300150219     c**                 exsr      chkfes
312400150219     c**                 if        wslitt='S'
312500150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
312600150219     c**                 z-add     24            wr6noi
312700150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
312800150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
312900150219     c**                 else
313000150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
313100150219     c**                 endif
313200141204     c* Ricalcolo WPOrifDatLav
313300141229     C**                 MOVEL     'OSC  '       WR3TAB
313400141229     C**                 Z-ADD     WHHODI        WR3HHI
313500141229     C**                 EXSR      RO3ORE
313600141229     c**
313700141229     C**                 EVAL      WTC='C'
313800141229     C**                 EVAL      WIC='C'
313900141229     c**                 z-add     wpoattdata00  WPOROU
314000141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
314100141229     c**                 z-add     wr3hho        wr6noi
314200141229     c**                 add       12            wr6noi
314300141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
314400141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
314500141229     c
314600141017     c
314700141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
314800141017     c*  ma sempre devo slittare i festivi
314900141017     C* CONSEGNABILITA'
315000150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
315100150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
315200150219     C**                 EVAL      WTC='C'
315300150219     C**                 EVAL      WIC='C'
315400150219     c**                 z-add     d98dfc        WPOdata
315500150219     c**                 exsr      chkfes
315600150219     c**                 if        wslitt='S'
315700150219     c**                 z-add     wpodfc_00     Wr6poi
315800150219     c**                 z-add     24            wr6noi
315900150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
316000150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
316100150219     c**                 else
316200150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
316300150219     c**                 endif
316400150219     c**                 endif
316500931201     C*
316600931201     C                   ENDSR
316700930511     C********************************************************************
316800930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
316900930511     C********************************************************************
317000930511     C     CALDPC        BEGSR
317100930325     C*
317200930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
317300930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
317400931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
317500930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
317600930325     C                   ELSE
317700930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
317800930407     C                   END
317900090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
318000090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
318100930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
318200090902     c
318300931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
318400090901
318500930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
318600930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
318700930423     C* A QUELLA DI RICHIESTA CONSEGNA,
318800930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
318900931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
319000931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
319100931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
319200931206     C                   END
319300930423     C                   END
319400930325     C*
319500930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
319600930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
319700930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
319800090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
319900090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
320000930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
320100931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
320200930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
320300930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
320400930423     C* A QUELLA DI RICHIESTA CONSEGNA,
320500930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
320600931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
320700931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
320800931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
320900931206     C                   END
321000930423     C                   END
321100140822     C*
321200930324     C                   ENDSR
321300150113     C********************************************************************
321400150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
321500150113     C********************************************************************
321600150113     c     Cal_DEE       BEGSR
321700150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
321800150508     c                   clear                   wtempi00C         1
321900150722     c                   clear                   woreOLP
322000150219     c
322100150219     c* 1) Se presente l'arrivo della trazione lo uso
322200150219    1c                   select
322300150219     c                   when      wmancatraz=' '  and d98dti>0
322400150630     c                   Z-ADD     WPODTI        WR1POI
322500150219     c
322600150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
322700150219    1c                   when      IDdata>0
322800150325     c                   exsr      CalcoloIMP
322900150528     c* stato IMP/disguido solo se >  alla data di oggi
323000150528     c                   if        wr1poi > wpoattdata00
323100150427     c                   eval      d98spcdee='IMP'
323200150427     c                   endif
323300150219     c
323400150219     c                   other
323500150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
323600150225     c* devo far finta che sia arrivato.
323700150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
323800150508     c*  che tiene conto della data partenza PRIMO  collo
323900150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
324000150508     c*
324100150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
324200150508     c                   if        wr1poi >=wpoattdata00
324300150508     c                              and WPOPTC_DEE > WPOPTC
324400150508     c                   eval      d98spcdee='IMP'
324500150508     c                   endif
324600150219     c                   endsl
324700150113     c
324800150219     c* se il calcolo è < di oggi --> imposto oggi
324900150113     c                   if        wr1poi < wpoattdata00
325000150113     c                   eval      wr1poi= wpoattdata00
325100150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
325200150508     c                   eval      wTempi00C='S'
325300150113     c                   endif
325400150217     c
325500150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
325600150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
325700150630     c                   clear                   wdiff
325800150630     c                   if        wr1poi>wpoattdata00
325900150630     c                   eval      wdiff=wr1poi-wpoattdata00
326000150630     c                   endif
326100150630    1c                   if        wr1poi>wpoattdata00  and  wdiff>12 and
326200150330     c                             (deeindist<>'S' or d98cons<>'S'
326300150330     c                             or d98ddc>wdaodi)
326400150427     c
326500150427     c* Se stato IMP lo mantengo per avere messaggio diverso
326600150427     c                   if        d98spcdee<>'IMP'
326700150304     c                   eval      d98spcdee='PRV'
326800150427     c                   endif
326900150427     c
327000150326   x1c                   else
327100150330     c                   exsr      CAL_OLP
327200150217     c
327300150528     c* Se passato l'orario oppure data distinta < data di oggi
327400150508     c* 1) spedizione in distinta          --> in consegna
327500150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
327600150528     c                             d98ddc<wdaodi)
327700150508    3c                   IF        deeindist='S' and d98cons='S'
327800150508     c                   eval      d98spcdee= 'DDC'
327900150508
328000150508   x3c                   else
328100150507     c
328200150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
328300150722     c*  entro sera
328400150630     c                   if        wr1poi>wpoattdata00
328500150722     c                   z-add     12            woreOLP           3 0
328600150722     c***                add       12            wr1poi
328700150630    4c                   else
328800150507    4c                   if        wr1poi<=wpoattdata00
328900150722     c                   add       24            woreOLP
329000150722     c***                add       24            wr1poi
329100150507    4c                   endif
329200150630    4c                   endif
329300150507     c
329400150722     c****               eval      d98spcdee='OLP'
329500150507     c* memorizzo orario limite picking
329600150507     c                   eval      d98picklim=%editc(picklim:'X')
329700150507    3c                   endif
329800150507    2c                   endif
329900150507     c
330000150326    1c                   endif
330100150217     c
330200150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
330300150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
330400150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
330500150113     C                   Z-ADD     Wr1poi        W10POI            9 0
330600150113     C                   EXSR      R10FES
330700150113     C                   Z-ADD     W10POO        wr1poi
330800150113     C
330900150629    1c                   if        wtempi00C='S'
331000150630     c***                z-add     W990CN        wr1noi
331100150630     c                   z-add     W990IN        wr1noi
331200150722     c
331300150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
331400150722     c*  quindi sottraggo 24
331500150722     c                   if        wr1noi>24
331600150722     c                   sub       24            wr1noi
331700150722     c                   endif
331800150722     c
331900150722     c* se passato orario limite picking, slitto in avanti
332000150722     c                   if        woreOLP>0
332100150722     c                   add       woreOLP       wr1noi
332200150722     c                   eval      d98spcdee='OLP'
332300150722     c                   endif
332400150722     c
332500150629   x1c                   else
332600150722     c
332700150714     c                   if        wmancatraz=' '  and d98dti>0
332800150630     c                   z-add     w99tci        wr1noi
332900150714     c                   else
333000150714     c                   z-add     w99tcc        wr1noi
333100150714     c                   endif
333200150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
333300150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
333400150722     c                   if        woreOLP=24 and wr1noi<=24
333500150722     c                   add       woreOLP       wr1noi
333600150722     c                   eval      d98spcdee='OLP'
333700150722     c                   endif
333800150722     c                   if        woreOLP=12 and wr1noi<24
333900150722     c                   add       woreOLP       wr1noi
334000150722     c                   eval      d98spcdee='OLP'
334100150722     c                   endif
334200150722     c
334300150629    1c                   endif
334400150629     c
334500150722    1c**                 if        WR1NOI=10
334600150722     c**                 z-add     12            wr1noi
334700150722     c**                 endif
334800150722     c
334900150722     c     wr1noi        div       12            wdiv
335000150722     c                   mvr                     wres
335100150722     c                   if        wres >0
335200150722     c                   add       2             wr1noi
335300150722     c                   endif
335400150722     c
335500150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
335600150629    1c******             endif
335700150508     c
335800150605     c                   eval      wdee='S'
335900150113     C                   EXSR      RO1CDP
336000150605     c                   eval      wdee=' '
336100150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
336200150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
336300150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
336400150113     C* A QUELLA DI RICHIESTA CONSEGNA,
336500150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
336600150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
336700150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
336800150331     c*  2) IL        -> è il giorno indicato
336900150331     C     D98DCR        IFNE      *ZEROS
337000150331     C     D98tcr        andne     'P'
337100150331     C***  WPOdpcee      IFLE      WPOCR2
337200150331     C***  WPOCR2        SUB       12            WPOdpcee
337300150330     c
337400150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
337500150330     c*   dare la prima data utile di consegna
337600150330     C     WPOdpcee      IFLE      WPOCRF_dee
337700150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
337800150330     C                   z-add     WPOCRF_dee    WPOdpcee
337900150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
338000150409     c                   if        wpodpcee>wpoattdata00
338100150409     c                   eval      d98spcdee='PRV'
338200150409     c                   endif
338300150113     C                   END
338400150331     c
338500150220     c
338600150113     C                   END
338700150220     c
338800150113     c                   ENDSR
338900150330     C********************************************************************
339000150330     C*   Calcolo orario da tabella OLP
339100150330     C********************************************************************
339200150330     c     Cal_OLP       BEGSR
339300150330     c                   eval      picklim=0900
339400150330     c* Dopo l'ora limite picking, passo al giorno dopo
339500150330     C                   MOVEL(P)  'OLP'         TBECOD
339600150330     C                   MOVEL(P)  d98lna        TBEKE1
339700150330     C                   MOVEL(P)  wdaodi        TBEKE2
339800150330     C     KTBE          CHAIN     TNTBE01L
339900150330     C                   IF        not %found(TNTBE01L)
340000150330     c                   clear                   tbeke2
340100150330     C     KTBE          CHAIN     TNTBE01L
340200150330     c                   endif
340300150330     C                   IF        not %found(TNTBE01L)
340400150330     C                   MOVEL(P)  '000'         TBEKE1
340500150330     C     KTBE          CHAIN     TNTBE01L
340600150330     c                   endif
340700150330     c
340800150330     C                   IF        %found(TNTBE01L) and
340900150330     c                             %subst(tbeuni:1:4)>*zeros
341000150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
341100150330     c                   endif
341200150330     c                   ENDSR
341300150325     C********************************************************************
341400150325     C*   Riclcolo data se colli in IMP/disguido
341500150325     C********************************************************************
341600150325     c     CalcoloIMP    BEGSR
341700150325     C                   Z-ADD     IDdata        WDAT
341800150325     C                   EXSR      DATORE
341900150325     C                   Z-ADD     WORE          WR4POI
342000150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
342100150325    2c                   if        IDfgs<>D98lnp
342200150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
342300150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
342400150325     c* calcolo il terminal di partenza
342500150325     c                   clear                   fnlv55ds
342600150325     c                   eval      d55tla='L'
342700150325     c                   eval      d55tpt='P'
342800150325     c                   eval      d55lin=IDfgs
342900150325     c                   eval      d55drf=IDdata
343000150325     c                   call      'FNLV55R'
343100150325     c                   parm                    fnlv55ds
343200150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
343300150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
343400150325     c                   EXSR      RTTRAZ
343500150325    3c                   if        d98err<>'1'
343600150325     C                   Z-ADD     CASTcn        wr4noi
343700150325     c                   else
343800150325     C                   Z-ADD     d98ttc        wr4noi
343900150325    3C                   endif
344000150325
344100150325   x2c                   else
344200150325     C                   Z-ADD     d98ttc        wr4noi
344300150325    2C                   endif
344400150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
344500150325     c*  perchè partirà un giorno dopo
344600150325    3c                   if        IDnpg='P'
344700150325     c                   add       24            WR4noi
344800150325    3c                   endif
344900150325     c
345000150325     c                   exsr      ro4cdp
345100150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
345200150325     c                   ENDSR
345300930604     C********************************************************************
345400930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
345500930604     C********************************************************************
345600930604     C     TPODAT        BEGSR
345700960229     C*
345800960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
345900960229     C*
346000960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
346100960229     C     WPOPTI        IFEQ      *ZEROS
346200960229     C                   Z-ADD     *ZEROS        D98DTI
346300960229     C                   Z-ADD     *ZEROS        D98HTI
346400960229     C                   ELSE
346500960229     C                   Z-ADD     WPOPTI        WORE
346600960229     C                   EXSR      OREDAT
346700960229     C                   Z-ADD     WDAT          D98DTI
346800960229     C     WRES          MULT      100           D98HTI
346900960229     C                   END
347000960229     C*
347100960229     C                   ELSE                                                   <-- 1 -->
347200930604     C*
347300931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
347400140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
347500140825     c*******            exsr      Tras_DEI
347600140825     c
347700930604     C*
347800150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
347900140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
348000140820     c* è data da :
348100140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
348200140820     c*  Tenere  conto, ovvero slittare in avanti, per :
348300140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
348400140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
348500140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
348600140911     c*
348700140820     c*  Da azzerare nei seguenti casi:
348800150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
348900150324     c* 4) bolla export    7) Cod manca consegna
349000140911     c* 8) bolla con DCR DOPOil o PRIMAdel
349100150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
349200150119     c*10) se completamente non arrivata e collo in IMP
349300150119     c
349400140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
349500150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
349600140925     C*  - FD   - export -
349700150217     c                   movel     'S'           DEEinDIST         1
349800150113     c                   eval      wcaldpcee='S'
349900150304     c                   eval      D98SPCDEE='PRV'
350000150324     c                   clear                   avvcmc            1
350100150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
350200150323     c                   if        d98cmc<>*blanks
350300150324     C                   Z-ADD     1             II
350400150326     C     d98cmc        LOOKUP    C2A(II)                                99
350500150324     C                   MOVEL     D2A(II)       usa2A
350600150324    4C     §2AFTC        IFEQ      'A'
350700150326     c                   eval      avvcmc='A'
350800150323     c                   endif
350900150324    4C     §2AFTC        IFEQ      'G'
351000150326     c                   eval      avvcmc='G'
351100150324     c                   endif
351200150324     c                   endif
351300140820     c
351400140911     c                   z-add     d98lna        EX
351500150113     c*  la azzero nei casi previsti                 0
351600150601    1c                   select
351700150119     c* 1) Se la bolla e consegnata
351800140925     c                   when      d98dcm>0
351900150113     c                   clear                   wcaldpcee
352000150216     c                   eval      d98spcdee='CON'
352100150217     c                   movel     ' '           DEEinDIST         1
352200150323     c* 2) Se pratica di danno
352300150603     c                   when      flgdan='S'
352400150323     c                   clear                   wcaldpcee
352500150323     c                   eval      d98spcdee='CA '
352600150323     c                   eval      deeindist='S'
352700150324     c* 3) Se bloccata/rimpallo
352800150114     c                   when      wd98fbc='B'
352900150113     c                   clear                   wcaldpcee
353000150216     c                   eval      d98spcdee='BLO'
353100150217     c                   movel     ' '           DEEinDIST         1
353200150324     c* 4) Se bolla export
353300150324     c                   when      fl1(EX)='E'
353400150324     c                   clear                   wcaldpcee
353500150324     c                   movel     ' '           DEEinDIST         1
353600150324     c                   eval      d98spcdee='EXP'
353700150324     c* 5) Se giacente  o in attesa di apertura giacenza
353800150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
353900150324     c                   clear                   wcaldpcee
354000150324     c                   eval      d98spcdee='GIA'
354100150324     c                   eval      deeindist='S'
354200150119     c* 7) Se codice manca consegna
354300150326     c                   when      d98cmc<>*blanks and avvcmc='A'
354400150324     c                   clear                   wpodpcee
354500150324     c                   clear                   wcaldpcee
354600150324     c                   movel     ' '           DEEinDIST         1
354700150324     c                   eval      d98spcdee='ATT'
354800150410     c* 7) Bolla in sospensione consegna
354900150410     c                   when      §B4LBGE='S'
355000150410     c                   clear                   wpodpcee
355100150410     c                   clear                   wcaldpcee
355200150410     c                   movel     ' '           DEEinDIST         1
355300150410     c                   eval      d98spcdee='SOS'
355400150616     c
355500150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
355600150616     c                   when      wrbl='R'
355700150616     c                   clear                   wcaldpcee
355800150616     c                   eval      d98spcdee='   '
355900150616     c                   eval      deeindist='S'
356000150601   x1c                   other
356100150325     c*
356200150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
356300150331     c                   if        d98ffd='S'
356400150601    2c                   select
356500150403     c* FD escluso dai contatti
356600150601    2c                   when      §B4LTFD= 'N'
356700150403     c                   eval      d98spcdee='FD3'
356800150403     c                   clear                   wcaldpcee
356900150403     c                   movel     ' '           DEEinDIST         1
357000150403
357100150403     c* FD non contattato ( o da ricontattare)
357200150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
357300150401     c                             olv82sts<>'1'
357400150323     c                   clear                   wcaldpcee
357500150323     c                   movel     ' '           DEEinDIST         1
357600150323     c                   eval      d98spcdee='FFD'
357700150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
357800150424     c                   eval      wpodpcee=wpoffi
357900150403
358000150403     c* FD contattato e     IN distinta
358100150601    2c                   when      d98cons='S'
358200150401     c                   eval      d98spcdee='FD2'
358300150401     c                   clear                   wcaldpcee
358400150401     c                   movel     ' '           DEEinDIST         1
358500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
358600150424     c                   eval      wpodpcee=wpoffi
358700150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
358800150427     c*  ritirare la merce
358900150427     C                   MOVEL     'C'           WTC                            *PARTENZA
359000150427     C                   MOVEL     'C'           WIC
359100150427     C                   Z-ADD     D98ddc        WDAT
359200150427     C                   EXSR      DATORE
359300150427     c                   z-add     wore          wr4poi
359400150427     c                   z-add     24            wr4noi
359500150427     C                   EXSR      RO4CDP
359600150427     c                   z-add     wr4poo        wore
359700150427     c                   exsr      OREDAT
359800150427     c                   movel     wdat          D98DTCTD
359900150601   x2c                   other
360000150403     c* FD contattato e NON in distinta
360100150331     c                   eval      d98spcdee='FD1'
360200150408     c                   eval      wpodpcee=wpoffi
360300150331     c                   clear                   wcaldpcee
360400150331     c                   movel     ' '           DEEinDIST         1
360500150601    2c                   endsl
360600150331     c
360700150331     c                   endif
360800150403     c
360900150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
361000150601    2c                   if        (d98tc1='A' or d98tc2='A' or
361100150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
361200150601     c                             and d98ffd= ' '
361300150325     c                   clear                   wcaldpcee
361400150325     c                   movel     ' '           DEEinDIST         1
361500150601    3c                   if        §B4LBAS= 'N'
361600150427     c                   eval      d98spcdee='AP3'
361700150403     c                   else
361800150403     c                   eval      d98spcdee='APP'
361900150601    3c                   endif
362000150601    2c                   endif
362100150325     c*   se la bolla ha il C/Assegno verifico se da contattare
362200150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
362300150325     c                   clear                   wcaldpcee
362400150325     c                   movel     ' '           DEEinDIST         1
362500150601    3c                   if        §B4LBAS= 'N'
362600150427     c                   eval      d98spcdee='CA3'
362700150403     c                   else
362800150403     c                   eval      d98spcdee='CAS'
362900150601    3c                   endif
363000150601    2c                   endif
363100150325     c* Per questi 3 casi devo impostare la data contatto destinatario
363200150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
363300150325     c                             d98spcdee='CAS'
363400150601    2c                   select
363500150601    2c                   when      d98dti>0 and wmancatraz= ' '
363600150325     c                   movel     d98dti        d98dtctd
363700150403     c* se piene le ore già portate ad un giorno lavorativo
363800150403     c                   if        wpodti>0
363900150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
364000150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
364100150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
364200150403     C                   Z-ADD     WPODTI        W10POI            9 0
364300150403     C                   EXSR      R10FES
364400150403     c                   z-add     w10poo        wore
364500150403     C                   EXSR      OREDAT
364600150403     C                   movel     WDAT          D98dtctd
364700150403     c                   endif
364800150403     c
364900150601    2c                   when      d98dam>0
365000150421     C                   Z-ADD     D98dam        WDAT
365100150421     C                   EXSR      DATORE
365200150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
365300150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
365400150421     C                   Z-ADD     Wore          W10POI            9 0
365500150421     C                   EXSR      R10FES
365600150421     c                   z-add     w10poo        wore
365700150421     C                   EXSR      OREDAT
365800150421     C                   movel     WDAT          D98dtctd
365900150601   x2c                   other
366000150325     c* data teorica di arrivo tenendo conto dell'IMP
366100150325     c                   exsr      letbrv
366200150325     c                   if        idDATA>0
366300150325     c                   exsr      CalcoloIMP
366400150325     c                   z-add     wr1poi        WORE
366500150325     c                   else
366600150325     c                   z-add     wpoptc        WORE
366700150325     c                   endif
366800150325     C                   EXSR      OREDAT
366900150325     c                   movel     wdat          d98dtctd
367000150601    2c                   endsl
367100150325     c
367200150325     c                   endif
367300150325     c
367400150601    1c                   endsl
367500140820     c
367600150113     c* Se la bolla non è in distinta calcolo la data se previsto
367700150220     c                   if        wcaldpcee='S' and
367800150220     c                             (d98cons<>'S'  or (d98cons='S' and
367900150220     c                             evemic<>'SI'))
368000150219     c
368100150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
368200150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
368300150219     c                   if        d98nsp>0 and wtrasm<>'D' and
368400150219     c                             d98dam=0
368500150219     c                   exsr      letBRV
368600150219     c                   endif
368700150219     c
368800150217     c                   eval      d98spcdee='PRG'
368900150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
369000150320     c                             d98tc1='S' or d98tc2='S')
369100150320     c                   eval      d98spcdee='APP'
369200150320     c                   endif
369300150320     c
369400150113     c                   exsr      cal_DEE
369500150113     c                   endif
369600150113     c
369700140820     C     WPOdpcee      IFEQ      *ZEROS
369800931202     C                   Z-ADD     *ZEROS        D98DEE
369900931202     C                   Z-ADD     *ZEROS        D98OEE
370000930604     C                   ELSE
370100140820     C                   Z-ADD     WPOdpcee      WORE
370200141021     C******             SUB       12            WORE
370300930604     C                   EXSR      OREDAT
370400931202     C                   Z-ADD     WDAT          D98DEE
370500090903     c                   if        d98tcc<>10
370600090903     C                   ADD       12            WRES
370700090903     c                   else
370800090903     C                   ADD       10            WRES
370900090903     c                   endif
371000090903
371100931202     C     WRES          MULT      100           D98OEE
371200090903     c
371300090903     c                   if        d98oee=1000
371400090903     c                   eval      d98oee=1030
371500090903     c                   endif
371600140925     C                   END
371700150311     c
371800150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
371900150311     c                   select
372000150311     c                   when      d98spcdee='OLP'
372100140820     c
372200150220     c* Se bolla in distinta --> data prevista = data distinta
372300150311     c                   when      deeindist='S'  and
372400150113     c                             d98cons='S'
372500150220
372600150225     c*  non la propongo senza MIC e se presente disguido o IMP
372700150220     c                   if        d98dee>d98ddc and evemic<>'SI'
372800150220     c                             and IDdata>0
372900150220     c                   else
373000150220
373100140820     c                   eval      d98dee=d98ddc
373200140925     c                   clear                   d98oee
373300150320     c* se la data è > di oggi stato PRV se non è in IMP
373400150320     c                   if        d98dee>wdaodi
373500150320     c                   if        d98spcdee<>'IMP'
373600150311     c                   eval      d98spcdee='PRV'
373700150320     c                   endif
373800150320     c
373900150225     c                   else
374000150217     c* se c'è il MIC
374100150217     c                   if        evemic='SI'
374200150217     c                   eval      d98spcdee='DDC'
374300150225     c                   endif
374400150217     c                   endif
374500150220     c                   endif
374600150217     c
374700150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
374800150722     c                   eval      d98spcdee='PRV'
374900150311     c                   endsl
375000931202     C*
375100931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
375200931202     C     WPODCI        IFEQ      *ZEROS
375300931202     C                   Z-ADD     *ZEROS        D98DCI
375400931202     C                   Z-ADD     *ZEROS        D98HCI
375500931202     C                   ELSE
375600931202     C                   Z-ADD     WPODCI        WORE
375700931202     C                   EXSR      OREDAT
375800931202     C                   Z-ADD     WDAT          D98DCI
375900090902     c                   if        d98tci<>10
376000931202     C                   ADD       12            WRES
376100090902     c                   else
376200090902     C                   ADD       10            WRES
376300090902     c                   endif
376400090902
376500931202     C     WRES          MULT      100           D98HCI
376600090901     c
376700090903     c                   if        d98hci=1000
376800090901     c                   eval      d98hci=1030
376900090901     c                   endif
377000090901     c
377100931202     C                   END
377200931202     C*
377300931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
377400931202     C     WPODCE        IFEQ      *ZEROS
377500931202     C                   Z-ADD     *ZEROS        D98DCE
377600931202     C                   Z-ADD     *ZEROS        D98HCE
377700931202     C                   ELSE
377800931202     C                   Z-ADD     WPODCE        WORE
377900931202//// C**************       SUB  12        WORE
378000931202     C                   EXSR      OREDAT
378100931202     C                   Z-ADD     WDAT          D98DCE
378200090902     c                   if        d98tcc >10
378300931202     C                   ADD       12            WRES
378400090902     c                   else
378500090902     C                   ADD       10            WRES
378600090902     c                   endif
378700090902     c
378800931202     C     WRES          MULT      100           D98HCE
378900090902     c
379000090903     c                   if        d98hce=1000
379100090902     c                   eval      d98hce=1030
379200090902     c                   endif
379300090902     c
379400931202     C                   END
379500960229     C*
379600960229     C                   ENDIF                                                  <-- 1 ---
379700930604     C*
379800930604     C                   ENDSR
379900150218     C********************************************************************
380000150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
380100150218     C********************************************************************
380200150218     c     LetBRV        BEGSR
380300150219     c                   clear                   bollaOK           1
380400150219     c                   clear                   IDnpg
380500150219     c                   clear                   IDdata
380600150219     c                   clear                   IDora
380700150219     c                   clear                   IDfgs
380800150219     c
380900150218     C     Kart          SETLL     FNART01L
381000150218     C     Kart          READE     FNART01L
381100150219    1c                   dow       not %eof(fnart01l)
381200150219     c                   clear                   DISfgs
381300150219     c                   clear                   DISdata
381400150219     c                   clear                   DISnpg
381500150219     c                   clear                   DISora
381600150219     c                   clear                   IMPdata
381700150219     c                   clear                   IMPfgs
381800150508     c* per ogni collo verifico se in IMP o disguidato
381900150218     c* al primo che NON trovo, esco come --> OK
382000150218     c     kbrv          setll     fnbrv07l
382100150218     c     kbrv          reade     fnbrv07l
382200150219    2c                   dow       not %eof(fnbrv07l)
382300150219    3c                   if        brvatr='D'  or brvnpg=3
382400150218     c* prendo la data della spunta
382500150218     c                   clear                   fnlv53ds
382600150219     c                   eval      d53tla='L'
382700150218     C                   MOVEL     BRvNPG        D53NPG
382800150218     C                   Z-ADD     BRvNFV        D53NFV
382900150218     C                   MOVEL     BRvFGS        D53FGS
383000150218     C                   CALL      'FNLV53R'
383100150218     C                   PARM                    fnlv53ds
383200150218    4c                   if        d53err<>*blanks
383300150219    5c                   if        brvdcs>0
383400150218     c                   eval      d53dfv=brvdcs
383500150218     c                   else
383600150219    6c                   if        brvdfs>0
383700150218     c                   eval      d53dfv=brvdfs
383800150219    6c                   endif
383900150219    5c                   endif
384000150219    4c                   endif
384100150218     c
384200150219    4c                   select
384300150219     c* se si tratta di IMP --> memorizzo
384400150219    4c                   when      d53spg='P' and d53dfv>impdata
384500150218     c                   eval      IMPdata=d53dfv
384600150219     c                   eval      IMPfgs =brvfgs
384700150219     c* se si tratta di DISGUIDO
384800150218     c                   when      brvatr='D'
384900150219     c
385000150219    5c                   if        d53dfv=DISdata
385100150219     c* Verifico l'ora
385200150219    6c                   if        brvhcs>0 and brvhcs>DISora
385300150219     c                   eval      DISdata=d53dfv
385400150219     c                   eval      DISfgs =brvfgs
385500150219     c                   eval      DISnpg =brvnpg
385600150219     c                   eval      DISora =brvhcs
385700150219   x6c                   else
385800150219    7c                   if        brvhms>0 and brvhms>DISora
385900150219     c                   eval      DISdata=d53dfv
386000150219     c                   eval      DISfgs =brvfgs
386100150219     c                   eval      DISnpg =brvnpg
386200150219     c                   eval      DISora =brvhms
386300150219    7c                   endif
386400150219    6c                   endif
386500150219    5c                   endif
386600150219     c
386700150219    5c                   if        d53dfv>DISdata
386800150219     c                   eval      DISdata=d53dfv
386900150219     c                   eval      DISfgs =brvfgs
387000150219     c                   eval      DISnpg =brvnpg
387100150219     c                   eval      DISora =brvhcs
387200150219     c                   if        disora=0
387300150219     c                   eval      DISora =brvhms
387400150219     c                   endif
387500150219    5c                   endif
387600150219     c
387700150219    4c                   endsl
387800150219    3c                   endif
387900150219     c     kbrv          reade     fnbrv07l
388000150219    2c                   enddo
388100150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
388200150219     c*  esco dal ciclo: la bolla è OK
388300150219    2c                   if        impdata=0 and disdata=0
388400150219     c                   eval      bollaOK='S'
388500150219     c                   clear                   IDdata
388600150219     c                   clear                   IDfgs
388700150219     c                   clear                   IDora
388800150219     c                   clear                   IDnpg
388900150219   x2c                   else
389000150219     c* memorizzo la data più alta
389100150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
389200150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
389300150219     c                   eval      IDdata=IMPdata
389400150219     c                   eval      IDfgs=IMPfgs
389500150219     c                   eval      IDora=0
389600150219     c                   eval      IDnpg='P'
389700150219    3c                   endif
389800150219    3c                   if        (DISdata>0 and DISdata=IDdata
389900150219     c                             and IDnpg<>'P' and DISora>IDora) or
390000150219     c                             (DISdata>0 and DISdata>IDdata)
390100150219     c                   eval      IDdata=DISdata
390200150219     c                   eval      IDfgs=DISfgs
390300150219     c                   eval      IDora=DISora
390400150219     c                   eval      IDnpg=%char(DISnpg)
390500150219    3c                   endif
390600150219    2c                   endif
390700150218     c
390800150218     C     Kart          READE     FNART01L
390900150219    1c                   enddo
391000150219     c
391100150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
391200150508     c**                 if        d98duc> 0 and d98duc>IDdata
391300150508     c**                 eval      bollaOK='S'
391400150508     c**                 clear                   IDdata
391500150508     c**                 clear                   IDfgs
391600150508     c**                 clear                   IDora
391700150508     c**                 clear                   IDnpg
391800150508     c**                 endif
391900150219     c
392000150218     c
392100150218     C                   ENDSR
392200140825     C********************************************************************
392300140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
392400140825     C********************************************************************
392500140825     c     TRAS_DEI      BEGSR
392600140825     C     WPODEI        IFEQ      *ZEROS
392700140825     C                   Z-ADD     *ZEROS        D98DEI
392800140825     C                   Z-ADD     *ZEROS        D98OEI
392900140825     C                   ELSE
393000140825     C                   Z-ADD     WPODEI        WORE
393100140825     C                   SUB       12            WORE
393200140825     C                   EXSR      OREDAT
393300140825     c
393400140825     C                   Z-ADD     WDAT          D98DEI
393500140825     c                   if        d98tci<>10
393600140825     C                   ADD       12            WRES
393700140825     c                   else
393800140825     C                   ADD       10            WRES
393900140825     c                   endif
394000140825     c
394100140825     C     WRES          MULT      100           D98OEI
394200140825     c
394300140825     c                   if        d98oei=1000
394400140825     c                   eval      d98oei=1030
394500140825     c                   endif
394600140825     c
394700140825     C                   END
394800140825     c                   ENDSR
394900930604     C********************************************************************
395000930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
395100930604     C********************************************************************
395200930604     C     CHKFOR        BEGSR
395300930604     C*
395400930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
395500930604     C* E QUINDI FORZO LA CONFORMITA' A SI
395600941227     C     D98ERR        IFEQ      '0'
395700930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
395800930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
395900930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
396000930604     C                   MOVEL     '1'           D98ERR
396100930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
396200930604     C                   END
396300930604     C                   END
396400990212     C*
396500990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
396600990212     C* E QUINDI FORZO LA CONFORMITA' A SI
396700990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
396800990212     C     D98ERR        IFEQ      '0'
396900990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
397000141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
397100990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
397200990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
397300990212     C                   MOVEL     '1'           D98ERR
397400990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
397500990212     C                   END
397600990212     C                   END
397700990308     C*
397800990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
397900990308     C* E QUINDI FORZO LA CONFORMITA' A SI
398000100614     C**   D98ERR        IFEQ      '0'
398100100614     C**   WFBR          IFEQ      'S'                                          *RESO
398200100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
398300100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
398400100614     C**                 MOVEL     '1'           D98ERR
398500100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
398600100614     C**                 END
398700100614     C**                 END
398800930604     C*
398900930604     C                   ENDSR
399000990212     C********************************************************************
399100990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
399200990212     C********************************************************************
399300990212     C     CHKDAN        BEGSR
399400990212     C*
399500990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
399600150603     c                   movel     'N'           opendan           1
399700040809      *
399800040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
399900040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
400000040809      * legate alla spedizione che passo
400100040809     c                   clear                   fidn12ds
400200040809     c                   eval      i12aas = D98AAS
400300040809     c                   eval      i12lnp = D98LNP
400400040809     c                   eval      i12nrs = D98NRS
400500040809     c                   eval      i12nsp = D98NSP
400600040809     c                   eval      i12fel = 'E'
400700040809     c                   eval      i12fan = 'E'
400800040809     c*
400900040809     c                   call      'FIDN12R'
401000040809     c                   parm                    fidn12ds
401100040809     c*
401200040809     c* se non ci sono errori
401300040809     c                   if        o12err = ' '
401400040809     c* se numero di CA trovate maggiore di zero
401500040809     c                   if        o12nca > 0
401600040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
401700150603     c
401800150603     c***                z-add     1             aa                2 0
401900150603     c***                dow       aa <= o12nca
402000150603     C* APERTA
402100150603     C***  skdch(aa)     IFEQ      0
402200150603     c***                movel     'S'           opendan
402300150603     c***                endif
402400150603     c*
402500150603     c***                add       1             aa
402600150603     c***                enddo
402700150603     c
402800040809     c                   endif
402900040809     c                   endif
403000990212     C*
403100990212     C                   ENDSR
403200941228     C********************************************************************
403300941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
403400941228     C********************************************************************
403500941228     C     CALORP        BEGSR
403600941228     C*
403700941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
403800941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
403900941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
404000960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
404100950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
404200950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
404300950110     C                   ELSE
404400950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
404500950110     C                   ENDIF
404600941228     C                   EXSR      RO2COP
404700941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
404800941228     C* SPED. IL GIORNO DOPO
404900941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
405000941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
405100941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
405200941228     C*
405300941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
405400941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
405500941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
405600941228     C                   Z-ADD     WPODSP        WR2POI
405700950110     C                   Z-ADD     WPOPPC        WR2POF
405800941228     C                   EXSR      RO2COP
405900941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
406000941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
406100941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
406200941228     C*
406300941228     C                   ENDSR
406400930414     C********************************************************************
406500930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
406600930414     C********************************************************************
406700930414     C     CALORT        BEGSR
406800930414     C*
406900930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
407000931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
407100930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
407200931126     C                   Z-ADD     *ZEROS        W98HF
407300950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
407400950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
407500950110     C                   ELSE
407600950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
407700950110     C                   ENDIF
407800931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
407900931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
408000930608     C                   ELSE
408100931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
408200930608     C                   END
408300931126     C                   EXSR      RO2COV
408400931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
408500930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
408600930414     C*
408700930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
408800930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
408900930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
409000931126     C                   Z-ADD     *ZEROS        W98HF
409100950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
409200931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
409300931126     C                   EXSR      RO2COV
409400931126     C                   Z-ADD     WR2NOV        W98RTC
409500930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
409600930414     C*
409700930414     C                   ENDSR
409800930414     C********************************************************************
409900930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
410000930414     C********************************************************************
410100930414     C     CALORC        BEGSR
410200930511     C*
410300930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
410400930511     C* I TEMPI DI RITARDO AD OGGI
410500930511     C     WPODCM        IFEQ      *ZEROS
410600960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
410700930511     C                   END
410800940117     C*
410900940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
411000940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
411100940117     C* ALLA DATA DI FINE CONSEGNABILITA'
411200940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
411300940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
411400940118££££ C     WPODFC        IFNE      *ZEROS
411500940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
411600940118££££ C                   END
411700930414     C*
411800930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
411900930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
412000930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
412100931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
412200931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
412300930608     C                   ELSE
412400931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
412500930608     C                   END
412600931126     C                   Z-ADD     WPODCM        WR5POF            9 0
412700931126     C                   EXSR      RO5COV
412800931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
412900930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
413000930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
413100060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
413200060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
413300930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
413400930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
413500930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
413600930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
413700930414     C*
413800930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
413900930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
414000930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
414100931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
414200931126     C                   Z-ADD     WPODCM        WR5POF            9 0
414300931126     C                   EXSR      RO5COV
414400931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
414500930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
414600930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
414700060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
414800060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
414900930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
415000930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
415100930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
415200930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
415300930414     C*
415400930414     C                   ENDSR
415500930414     C********************************************************************
415600930414     C*   CALORR - CALCOLA ORE REALI DI RESA
415700930414     C********************************************************************
415800930414     C     CALORR        BEGSR
415900930414     C*
416000950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
416100950110     C                   ADD       W98RTI        W98RRI
416200941228     C                   ADD       W98RCI        W98RRI
416300950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
416400950110     C                   ADD       W98RTC        W98RRC
416500941228     C                   ADD       W98RCC        W98RRC
416600930414     C*
416700930414     C                   ENDSR
416800930420     C********************************************************************
416900930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
417000930420     C********************************************************************
417100930420     C     CALDIF        BEGSR
417200930420     C*
417300941228     C* PARTENZA
417400941228     C     W98RPI        SUB       D98TPI        W98FPI
417500941228     C     W98RPC        SUB       D98TPC        W98FPC
417600941228     C*
417700930420     C* TRAZIONE
417800930423     C     W98RTI        SUB       D98TTI        W98FTI
417900930423     C     W98RTC        SUB       D98TTC        W98FTC
418000930420     C*
418100930420     C* CONSEGNA
418200090827     C***  W98RCI        SUB       D98TCI        W98FCI
418300090827     C     W98RCI        SUB       w99TCI        W98FCI
418400090827     C***  W98RCC        SUB       D98TCC        W98FCC
418500090827     C     W98RCC        SUB       w99TCC        W98FCC
418600930420     C*
418700930420     C* RESA
418800090921     C***  W98RRI        SUB       D98TRI        W98FRI
418900090921     C     W98RRI        SUB       w99TRI        W98FRI
419000090921     c
419100090921     C***  W98RRC        SUB       D98TRC        W98FRC
419200090921     C     W98RRC        SUB       w99TRC        W98FRC
419300930420     C*
419400930420     C                   ENDSR
419500930420     C********************************************************************
419600930420     C*   CALCNF - CALCOLA LE CONFORMITA'
419700930420     C********************************************************************
419800930420     C     CALCNF        BEGSR
419900941228     C*
420000941228     C*-------------------------------------------------------------------
420100941228     C* CONFORMITA' PARTENZA  INTERNA
420200941228     C*-------------------------------------------------------------------
420300941228     C                   Z-ADD     W98FPI        W98NPI
420400941228     C     W98FPI        IFLE      *ZEROS
420500941228     C                   MOVEL     'S'           D98CPI
420600941228     C                   ELSE
420700941228     C                   MOVEL     'N'           D98CPI
420800941228     C                   END
420900941228     C*
421000941228     C*-------------------------------------------------------------------
421100941228     C* CONFORMITA' PARTENZA  CLIENTE
421200941228     C*-------------------------------------------------------------------
421300941228     C                   Z-ADD     W98FPC        W98NPC
421400941228     C     W98FPC        IFLE      *ZEROS
421500941228     C                   MOVEL     'S'           D98CPC
421600941228     C                   ELSE
421700941228     C                   MOVEL     'N'           D98CPC
421800941228     C                   END
421900930420     C*
422000930420     C*-------------------------------------------------------------------
422100930420     C* CONFORMITA' TRAZIONE  INTERNA
422200930420     C*-------------------------------------------------------------------
422300930510     C                   Z-ADD     W98FTI        W98NTI
422400930423     C     W98FTI        IFLE      *ZEROS
422500930420     C                   MOVEL     'S'           D98CTI
422600930420     C                   ELSE
422700930420     C                   MOVEL     'N'           D98CTI
422800930420     C                   END
422900930420     C*
423000930420     C*-------------------------------------------------------------------
423100930420     C* CONFORMITA' TRAZIONE  CLIENTE
423200930420     C*-------------------------------------------------------------------
423300930510     C                   Z-ADD     W98FTC        W98NTC
423400930423     C     W98FTC        IFLE      *ZEROS
423500930420     C                   MOVEL     'S'           D98CTC
423600930420     C                   ELSE
423700930420     C                   MOVEL     'N'           D98CTC
423800930420     C                   END
423900930420     C*
424000930420     C*-------------------------------------------------------------------
424100930420     C* CONFORMITA' CONSEGNA  INTERNA
424200930420     C*-------------------------------------------------------------------
424300150226     c                   if        wmancatraz='S'
424400150226     c                   clear                   d98dti
424500150226     c                   clear                   d98hti
424600150226     c                   endif
424700150226     c
424800930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
424900930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
425000930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
425100050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
425200050914     c                   if        consideraora='S'
425300050914     c     w98fci        sub       24            w98nci
425400050914     c                   add       24            d98hrc
425500050914     c                   else
425600930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
425700050914     c                   endif
425800080116     c
425900110923     c                   select
426000080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
426100080116     c*  --> trasformo il ritardo in 0
426200090827     c                   when      d98cca='2'
426300080116     c                   z-add     0             w98nci
426400090827
426500080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
426600080116     c*  e sto leggendo la bolla in arrivo
426700080116     c*  --> trasformo il ritardo = 0
426800090827     c                   when      d98cca='1' and d98tbo='A'
426900080116     c                   z-add     0             w98nci
427000090902     c
427100090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
427200090902     c                   when      wmancaTRAZ='S'
427300090902     c                   z-add     0             w98nci
427400090902     c                   clear                   d98dti
427500090902     c                   clear                   d98hti
427600110923     c
427700110923     c* Ignoro ritardo se pefect order
427800130221     c                   when      d98xco >*zeros
427900110923     c                   z-add     0             w98nci
428000090827     c
428100090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
428200091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
428300091014     c*  controllo ora soglia  consegna da tabella OSC)
428400091009     c                   when      d98tci=10   and
428500091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
428600091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
428700091014     c*
428800091014     c                             d98hci=1030
428900091014     C                   MOVEL     'OSC  '       WR3TAB
429000091014     C                   eval      WR3HHI=d98hmc
429100091014     C                   EXSR      RO3ORE
429200131218
429300091014     c                   if        wr3matpom='M' and d98hmc>1030
429400131218     c                   exsr      ctrLAVH1030
429500131218
429600131218     c                   if        wlav1030<>'S'
429700091014     c                   add       12            w98nci
429800091014     c                   endif
429900131218     c                   endif
430000090928     c
430100090827     c                   endsl
430200050914     c
430300930524     C                   ELSE                                                   <-- 1 -->
430400930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
430500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
430600930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
430700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
430800930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
430900930608     C                   ELSE
431000930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
431100930608     C                   END
431200930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
431300930525     C                   EXSR      RO2COV
431400930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
431500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
431600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
431700930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
431800931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
431900930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
432000930608     C                   ELSE
432100930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
432200930608     C                   END
432300931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
432400930525     C                   EXSR      RO2COV
432500930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
432600930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
432700930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
432800930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
432900930525     C* E' IL RITARDO
433000930610     C     WNOCRI        SUB       W98RCI        W98NCI
433100050901     c
433200050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
433300050901     c*  --> trasformo il ritardo in anticipo
433400050901     c                   if        d98cca='2'
433500080116     c***                z-sub     w98nci        w98nci
433600080116     c                   z-add     0             w98nci
433700050901     c                   endif
433800111018     c* Ignoro ritardo se pefect order
433900130221     c                   if        d98xco >*zeros
434000111018     c                   z-add     0             w98nci
434100111018     c                   endif
434200061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
434300061121     c*  e sto leggendo la bolla in arrivo
434400061121     c*  --> trasformo il ritardo in anticipo
434500061121     c                   if        d98cca='1' and d98tbo='A'
434600080116     c***                z-sub     w98nci        w98nci
434700080116     c                   z-add     0             w98nci
434800061121     c                   endif
434900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
435000070827     c*  mittente, --> consegnato in anticipo
435100070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
435200140403     c                   if        Callar5=' '
435300070827     c                   exsr      LeggiAR5
435400140403     c                   endif
435500070827     c
435600070827     c                   if        dcrpart<>'S'
435700070824     c                   z-sub     w98nci        w98nci
435800070824     c                   endif
435900070827     c
436000070824     c                   endif
436100050907     c
436200050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
436300050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
436400050907     c*  lo trasformo in anticipo perchè non sono in ritardo
436500050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
436600050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
436700050916     c***                if        d98dfc>0
436800070828     c                   if        d98dcm=0  and w98nci>0
436900050907     c                   z-sub     w98nci        w98nci
437000050907     c                   sub       oreILoDOPO    w98nci
437100050907     c                   endif
437200050901     c
437300930525     C                   ELSE                                                   <-- 2 -->
437400070521     c* non do ritardo se la consegna reale cade nel giorno
437500070521     c*  della consegna richiesta se presente IL
437600100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
437700070521     c                             wpodcm<=wpocr1
437800070521     c                   clear                   w98nci
437900100224     c                   else                                                   ---2a -->
438000930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
438100090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
438200090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
438300930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
438400930525     C* E' IL RITARDO O L'ANTICIPO
438500930525     C     W98RCI        SUB       WNOCRF        W98NCI
438600070115     c
438700070115     c* non do ritardo se comunque la consegna reale cade nel giorno
438800070115     c*  della consegna richiesta se presente IL
438900070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
439000070521     c***                          wpodcm<=wpocr1  and w98nci>0
439100070521     c***                clear                   w98nci
439200070521     c***                endif
439300070115     c
439400930525     C                   ELSE                                                   <-- 3 -->
439500930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
439600930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
439700930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
439800930525     C                   END                                                    <-- 3 ---
439900100224     C                   END                                                    <-- 2a---
440000100224
440100100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
440200100224     c                   select
440300100224     c                   when      d98cca='2'
440400100224     c                   z-add     0             w98nci
440500100224
440600100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
440700100224     c*  e sto leggendo la bolla in arrivo
440800100224     c                   when      d98cca='1' and d98tbo='A'
440900100224     c                   z-add     0             w98nci
441000100224     c                   endsl
441100100224     c
441200100224     C                   END                                                    <-- 2 ---
441300070521     C                   END                                                    <-- 1 ---
441400070521     c
441500970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
441600970915     C*/         WAZNCI    IFEQ 'S'
441700970915     C*/         W98NCI    ANDLT*ZEROS
441800970915     C*/                   Z-ADD*ZEROS    W98NCI
441900970915     C*/                   ENDIF
442000970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
442100050922     c* lo faccio solo se non c'e' la data cons richiesta
442200970915     C     D98DTI        IFEQ      *ZEROS
442300050922     c     d98dcr        andeq     *zeros
442400970915     C                   Z-ADD     *ZEROS        W98NCI
442500970915     C                   ENDIF
442600930524     C*
442700930524     C* CONTROLLO CONFORMITA
442800930423     C     W98NCI        IFLE      *ZEROS
442900930524     C                   MOVEL     'S'           D98CCI
443000930420     C                   ELSE
443100930524     C                   MOVEL     'N'           D98CCI
443200930420     C                   END
443300930420     C*
443400930420     C*-------------------------------------------------------------------
443500930420     C* CONFORMITA' CONSEGNA  CLIENTE
443600930420     C*-------------------------------------------------------------------
443700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
443800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
443900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
444000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
444100050914     c                   if        consideraora='S'
444200050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
444300050914     c                   else
444400050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
444500050914     c                   endif
444600080116     c
444700080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
444800080116     c*  --> trasformo il ritardo =0
444900090827     c                   select
445000090827     c                   when      d98cca='2'
445100080116     c                   z-add     0             w98ncc
445200090827
445300080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
445400080116     c*  e sto leggendo la bolla in arrivo
445500080116     c*  --> trasformo il ritardo =0
445600090827     c                   when      d98cca='1' and d98tbo='A'
445700080116     c                   z-add     0             w98ncc
445800090902     c
445900090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
446000090902     c                   when      wmancaTRAZ='S'
446100090902     c                   z-add     0             w98ncc
446200110923     c* Ignoro ritardo se Perfect Order
446300130221     c                   when      d98xco >*zeros
446400110923     c                   z-add     0             w98ncc
446500090902     c
446600090827     c
446700090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
446800090827     c* sono  in ritardo di 12 ore
446900091009     c                   when      d98tcc=10  and
447000091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
447100091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
447200091014     c*
447300091014     c                             d98hce=1030
447400091014     C                   MOVEL     'OSC  '       WR3TAB
447500091014     C                   eval      WR3HHI=d98hmc
447600091014     C                   EXSR      RO3ORE
447700091014     c                   if        wr3matpom='M' and d98hmc>1030
447800131218     c                   exsr      ctrLAVH1030
447900131218
448000131218     c                   if        wlav1030<>'S'
448100131218     c                   add       12            w98ncc
448200131218     c                   endif
448300131218     c****               add       12            w98ncc
448400091014     c                   endif
448500131218     c
448600091014     c***                z-add     12            w98ncc
448700090827     c                   endsl
448800050914     c
448900930524     C                   ELSE                                                   <-- 1 -->
449000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
449100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
449200930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
449300930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
449400930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
449500930525     C                   EXSR      RO2COV
449600930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
449700930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
449800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
449900930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
450000930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
450100931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
450200930525     C                   EXSR      RO2COV
450300930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
450400930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
450500930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
450600930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
450700930525     C* E' IL RITARDO
450800930525     C     WNOCRI        SUB       W98RCC        W98NCC
450900050901     c
451000050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
451100050901     c*  --> trasformo il ritardo in anticipo
451200050901     c                   if        d98cca='2'
451300080116     c***                z-sub     w98ncc        w98ncc
451400080116     c                   z-add     0             w98ncc
451500050901     c                   endif
451600111018     c* Ignoro ritardo se pefect order
451700130221     c                   if        d98xco >*zeros
451800111018     c                   z-add     0             w98ncc
451900111018     c                   endif
452000061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
452100061121     c*  e sto leggendo la bolla in arrivo
452200061121     c*  --> trasformo il ritardo in anticipo
452300061121     c                   if        d98cca='1' and d98tbo='A'
452400080116     c***                z-sub     w98ncc        w98ncc
452500080116     c                   z-add     0             w98ncc
452600061121     c                   endif
452700070824     c
452800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
452900070827     c*  mittente, --> consegnato in anticipo
453000070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
453100070827     c* SBR per vedere se tassativa
453200070827     c                   if        Callar5=' '
453300070827     c                   EXSR      LeggiAR5
453400070827     c                   endif
453500070827     c                   if        dcrpart<>'S'
453600070824     c                   z-sub     w98ncc        w98ncc
453700070824     c                   endif
453800070824     c                   endif
453900050907     c
454000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
454100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
454200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
454300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
454400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
454500050916     c***                if        d98dfc>0
454600070828     c                   if        d98dcm=0 and w98ncc>0
454700050907     c                   z-sub     w98ncc        w98ncc
454800050907     c                   sub       oreILoDOPO    w98ncc
454900050907     c                   endif
455000050901     c
455100930525     C                   ELSE                                                   <-- 2 -->
455200070521     c* Non do ritardo se la consegna reale cade nel giorno
455300070521     c*  della consegna richiesta se presente IL
455400100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
455500070521     c                             wpodcm<=wpocr2
455600070521     c                   clear                   w98ncc
455700100224     c                   else                                                   <--2a -->
455800070521     c
455900930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
456000090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
456100090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
456200930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
456300930525     C* E' IL RITARDO O L'ANTICIPO
456400930525     C     W98RCC        SUB       WNOCRF        W98NCC
456500070115     c
456600070115     c* non do ritardo se comunque la consegna reale cade nel giorno
456700070115     c*  della consegna richiesta se presente IL
456800070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
456900070521     c***                          wpodcm<=wpocr2 and w98ncc>0
457000070521     c***                clear                   w98ncc
457100070521     c***                endif
457200070115     c
457300930525     C                   ELSE                                                   <-- 3 -->
457400930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
457500930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
457600930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
457700930525     C                   END                                                    <-- 3 ---
457800100224     C                   END                                                    <-- 2a---
457900100224
458000100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
458100100224     c                   select
458200100224     c                   when      d98cca='2'
458300100224     c                   z-add     0             w98nci
458400100224
458500100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
458600100224     c*  e sto leggendo la bolla in arrivo
458700100224     c                   when      d98cca='1' and d98tbo='A'
458800100224     c                   z-add     0             w98nci
458900100224     c                   endsl
459000100224     c
459100100224     C                   END                                                    <-- 2 ---
459200070521     C                   END                                                    <-- 1 ---
459300930524     C*
459400930524     C* CONTROLLO CONFORMITA
459500930423     C     W98NCC        IFLE      *ZEROS
459600930524     C                   MOVEL     'S'           D98CCC
459700930420     C                   ELSE
459800930524     C                   MOVEL     'N'           D98CCC
459900930420     C                   END
460000930420     C*
460100930420     C*-------------------------------------------------------------------
460200930420     C* CONFORMITA' RESA INTERNA
460300930420     C*-------------------------------------------------------------------
460400930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
460500930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
460600930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
460700050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
460800050914     c                   if        consideraora='S'
460900050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
461000050914     c                   else
461100050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
461200050914     c                   endif
461300080116     c
461400080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
461500090827     c                   select
461600090827     c                   when      d98cca='2' and w98nri>0
461700080116     c                   z-add     0             w98nri
461800090827     c
461900080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
462000080116     c*  e sto leggendo la bolla in arrivo
462100090827     c                   when      d98cca='1' and d98tbo='A' and
462200080116     c                                            w98nri>0
462300080116     c                   z-add     0             w98nri
462400110923     c
462500110923     c* Ignoro ritardo se Perfect Order
462600130221     c                   when      d98xco >*zeros
462700110923     c                   z-add     0             w98nri
462800110923     c
462900090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
463000090827     c* sono  in ritardo di 12 ore
463100091009     c                   when      d98tci=10  and
463200091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
463300091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
463400091014     c*
463500091014     c                             d98hci=1030
463600091014     C                   MOVEL     'OSC  '       WR3TAB
463700091014     C                   eval      WR3HHI=d98hmc
463800091014     C                   EXSR      RO3ORE
463900091014     c                   if        wr3matpom='M' and d98hmc>1030
464000131218     c                   exsr      ctrLAVH1030
464100131218
464200131218     c                   if        wlav1030<>'S'
464300091014     c                   add       12            w98nri
464400091014     c                   endif
464500131218     c                   endif
464600091014     c
464700091014     c***                z-add     12            w98nri
464800090827     c                   endsl
464900090827
465000930524     C                   ELSE                                                   <-- 1 -->
465100930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
465200930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
465300930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
465400931206     C     WPODCM        ANDLE     WPOCR1
465500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
465600930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
465700930525     C                   Z-ADD     WPODCM        WR2POI            9 0
465800931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
465900930525     C                   EXSR      RO2COV
466000930525     C     WR2NOV        IFNE      *ZEROS
466100930525     C     WR2NOV        MULT      -1            W98NRI
466200930525     C                   END
466300930524     C                   ELSE                                                   <-- 2 -->
466400930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
466500930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
466600930524     C     WPODCM        IFLT      WPOCRI
466700930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
466800930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
466900930524     C                   ELSE
467000931206     C                   Z-ADD     WPOCR1        WPORFI
467100930524     C                   Z-ADD     WPODCM        WPORFF
467200930524     C                   END
467300930524     C* CALCOLO ORE DI DIFFERENZA
467400930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
467500930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
467600930524     C                   Z-ADD     WPORFI        WR2POI            9 0
467700930524     C                   Z-ADD     WPORFF        WR2POF            9 0
467800930524     C                   EXSR      RO2COV
467900930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
468000050902     c
468100050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
468200100224     c                   if        d98cca='2' and w98nri>0
468300100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
468400080116     c***                z-sub     w98nri        w98nri
468500080116     c                   z-add     0             w98nri
468600050902     c                   endif
468700111018     c* Ignoro ritardo se pefect order
468800130221     c                   if        d98xco >*zeros
468900111018     c                   z-add     0             w98nri
469000111018     c                   endif
469100061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
469200061121     c*  e sto leggendo la bolla in arrivo
469300100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
469400100224     c***                if        d98cca='1' and d98tbo='A' and
469500100224     c***                                         w98nri>0 and wpodcm<wpocri
469600080116     c***                z-sub     w98nri        w98nri
469700080116     c                   z-add     0             w98nri
469800061121     c                   endif
469900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
470000070827     c*  mittente, --> consegnato in anticipo
470100070828     c                   if        d98tcr=' ' and d98nsp>0  and
470200070828     c                                            w98nri>0 and wpodcm<wpocri
470300070827     c                   if        CallAR5=' '
470400070827     c                   EXSR      LeggiAR5
470500070824     c                   endif
470600070827     c                   if        dcrpart<>'S'
470700070824     c                   z-sub     w98nri        w98nri
470800070824     c                   endif
470900070824     c                   endif
471000050907     c
471100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
471200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
471300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
471400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
471500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
471600050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
471700050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
471800050907     c                   z-sub     w98nri        w98nri
471900050907     c                   sub       oreILoDOPO    w98nri
472000050907     c                   endif
472100050907     c
472200050907     C                   END                                                    <-- 2 ---
472300930524     C                   END                                                    <-- 1 ---
472400930420     C* CALCOLO LE ORE DI RESA
472500930420     C* CONTROLLO CONFORMITA
472600930423     C     W98NRI        IFLE      *ZEROS
472700930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
472800930420     C                   ELSE
472900930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
473000930420     C                   END
473100930420     C*
473200930420     C*-------------------------------------------------------------------
473300930420     C* CONFORMITA' RESA CLIENTE
473400930420     C*-------------------------------------------------------------------
473500930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
473600930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
473700930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
473800050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
473900050914     c                   if        consideraora='S'
474000050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
474100050914     c                   else
474200050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
474300050914     c                   endif
474400090827     c
474500080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
474600090827     c                   select
474700090827     c                   when      d98cca='2' and w98nrc>0
474800080116     c                   z-add     0             w98nrc
474900090827
475000080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
475100080116     c*  e sto leggendo la bolla in arrivo
475200090827     c                   when      d98cca='1' and d98tbo='A' and
475300080116     c                                            w98nrc>0
475400080116     c                   z-add     0             w98nrc
475500110923     c
475600110923     c* Ignoro ritardo se Perfect Order
475700130221     c                   when      d98xco >*zeros
475800110923     c                   z-add     0             w98nrc
475900090827     c
476000090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
476100090827     c* sono  in ritardo di 12 ore
476200091009     c                   when      d98tcc=10  and
476300091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
476400091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
476500091014     c                             d98hce=1030
476600091014     C                   MOVEL     'OSC  '       WR3TAB
476700091014     C                   eval      WR3HHI=d98hmc
476800091014     C                   EXSR      RO3ORE
476900091014     c                   if        wr3matpom='M' and d98hmc>1030
477000131218     c                   exsr      ctrLAVH1030
477100131218
477200131218     c                   if        wlav1030<>'S'
477300091014     c                   add       12            w98nrc
477400091014     c                   endif
477500131218     c                   endif
477600091014     c
477700091014     c**                 z-add     12            w98nrc
477800090827     c                   endsl
477900090827     c
478000930524     C                   ELSE                                                   <-- 1 -->
478100930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
478200930524     C* DI DIFFERENZA SONO UGUALI A ZERO
478300930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
478400931206     C     WPODCM        ANDLE     WPOCR2
478500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
478600930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
478700930525     C                   Z-ADD     WPODCM        WR2POI            9 0
478800931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
478900930525     C                   EXSR      RO2COV
479000930525     C     WR2NOV        IFNE      *ZEROS
479100930525     C     WR2NOV        MULT      -1            W98NRC
479200930525     C                   END
479300930524     C                   ELSE                                                   <-- 2 -->
479400930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
479500930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
479600930524     C     WPODCM        IFLT      WPOCRI
479700930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
479800930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
479900930524     C                   ELSE
480000931206     C                   Z-ADD     WPOCR2        WPORFI
480100930524     C                   Z-ADD     WPODCM        WPORFF
480200930524     C                   END
480300930524     C* CALCOLO ORE DI DIFFERENZA
480400930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
480500930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
480600930524     C                   Z-ADD     WPORFI        WR2POI            9 0
480700930524     C                   Z-ADD     WPORFF        WR2POF            9 0
480800930524     C                   EXSR      RO2COV
480900930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
481000050902     c
481100050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
481200100224     c                   if        d98cca='2' and w98nrc>0
481300100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
481400080116     c***                z-sub     w98nrc        w98nrc
481500080116     c                   z-add     0             w98nrc
481600050902     c                   endif
481700111018     c* Ignoro ritardo se pefect order
481800130221     c                   if        d98xco >*zeros
481900111018     c                   z-add     0             w98nrc
482000111018     c                   endif
482100070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
482200061121     c*  e sto leggendo la bolla in arrivo
482300100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
482400100224     c***                if        d98cca='1' and d98tbo='A' and
482500100224     c***                                         w98nrc>0 and wpodcm<wpocri
482600080116     c***                z-sub     w98nrc        w98nrc
482700080116     c                   z-add     0             w98nrc
482800061121     c                   endif
482900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
483000070827     c*  mittente, --> consegnato in anticipo
483100070828     c                   if        d98tcr=' ' and d98nsp>0 and
483200070828     c                                            w98nrc>0 and wpodcm<wpocri
483300070827     c                   if        CallAR5=' '
483400070827     c                   EXSR      LeggiAR5
483500070827     c                   endif
483600070827     c                   if        dcrpart<>'S'
483700070824     c                   z-sub     w98nrc        w98nrc
483800070824     c                   endif
483900070824     c                   endif
484000050907     c
484100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
484200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
484300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
484400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
484500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
484600050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
484700050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
484800050907     c                   z-sub     w98nrc        w98nrc
484900050907     c                   sub       oreILoDOPO    w98nrc
485000050907     c                   endif
485100050907     c
485200050907     C                   END                                                    <-- 2 ---
485300930524     C                   END                                                    <-- 1 ---
485400930420     C* CALCOLO LE ORE DI RESA
485500930420     C* CONTROLLO CONFORMITA
485600930423     C     W98NRC        IFLE      *ZEROS
485700930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
485800930420     C                   ELSE
485900930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
486000930420     C                   END
486100930420     C*
486200930420     C                   ENDSR
486300070827     c
486400131218     c********************************************************************
486500131218     c     CtrlavH1030   BEGSR
486600131218     c********************************************************************
486700131218     c                   clear                   wlav1030          1
486800140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
486900131218     c                   z-add     1             xx                4 0
487000131218     c                   dow       evc(xx)<>*blanks
487100140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
487200140127     c                   if        evd(xx)=d98dcm
487300131218     c                   if        evo(xx)<=1030
487400131218     c                   eval      wlav1030='S'
487500131218     c                   leave
487600131218     c                   endif
487700131218     c                   endif
487800131218     c                   add       1             xx
487900131218     c                   enddo
488000131218     c                   ENDSR
488100070827     c********************************************************************
488200070827     c* Controllo record GEN di FIAR5
488300070827     c********************************************************************
488400070827     c     LeggiAR5      BEGSR
488500070827     c                   clear                   DCRPART           1
488600141104     c                   clear                   dar5gen
488700141104     c                   clear                   gc1PART           2
488800141104     c                   clear                   gc2PART           2
488900070827     c     kar5          chain     fiar501l
489000070827    1c                   if        not %found
489100070827     c                   eval      dcrpart='S'
489200141104     c                   eval      gc1part=d98gc1
489300141104     c                   eval      gc2part=d98gc2
489400070827   x1c                   else
489500070827     c* i dati devono essere quelli immessi dalla partenza
489600070827     c                   movel     ar5uni        dar5gen
489700070827     c
489800070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
489900070827     c*  ma il record GEN è stato scritto per altri motivi
490000070827    2c                   if        §ar5mdp='S'
490100070827     c                   movel     d98dcr        alfadcr           8
490200141104     c                   eval      gc1part=§ar5gc1
490300141104     c                   eval      gc2part=§ar5gc2
490400070827     c
490500070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
490600070827     c                   eval      dcrpart='S'
490700070827    3c                   endif
490800070827     c
490900070827   x2c                   else
491000070827     c                   eval      dcrpart='S'
491100141106     c                   eval      gc1part=d98gc1
491200141106     c                   eval      gc2part=d98gc2
491300070827    2c                   endif
491400070827    1c                   endif
491500070827     c
491600140402     c* Memorizzo il flag pickup 2
491700140402     c                   eval      wpick2=§AR5PK2
491800140402     c
491900150401     c*
492000150414     c**                 if        d98cas>0 or d98ffd<>' ' or
492100150414     c**                           d98tc1='A'  or d98tc2='A' or
492200150414     c**                           d98tc1='S'  or d98tc2='S'
492300150401     c     kar4          chain     fiar401l
492400150401     c                   if        %found(fiar401l)
492500150401     c                   eval      dsbl4L=ar4not
492600150401     c                   endif
492700150401     c* Solo per fermo deposito verifico se inviato alert
492800150401     c                   if        d98ffd<>' '
492900150401     c                   clear                   fnlv82ds
493000150401     c                   eval      ILV82AAS=d98aas
493100150401     c                   eval      ILV82LNP=d98lnp
493200150401     c                   eval      ILV82NRS=d98nrs
493300150401     c                   eval      ILV82NSP=d98nsp
493400150401     c                   eval      ILV82TIPA='FD'
493500150401     c                   call      'FNLV82R'
493600150401     c                   parm                    fnlv82ds
493700150401     c                   endif
493800150414     c**                 endif
493900150401
494000150401     c                   eval      callar5='S'
494100150401
494200070827     c                   ENDSR
494300930913     C********************************************************************
494400930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
494500930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
494600930913     C*            DATA/ORA (FINE RANGE)
494700930913     C********************************************************************
494800930913     C     CALCNS        BEGSR
494900930914     C*
495000930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
495100930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
495200930914     C     WPODTI        IFEQ      *ZEROS
495300960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
495400930914     C                   ELSE
495500930914     C                   Z-ADD     WPODTI        WPOIRC
495600930914     C                   END
495700930914     C*
495800930914     C* SE NON ANCORA CONSEGNATA
495900930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
496000050922     c* in realtà nella data consegna c'e' la data fine calcolo
496100050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
496200930914     C     WPODCM        IFEQ      *ZEROS
496300050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
496400930914     C                   ELSE
496500930916     C                   Z-ADD     WPODCM        WPOFRC
496600930914     C                   END
496700930914     C*
496800930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
496900930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
497000930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
497100930914     C     WPODFC        ANDLT     WPOIRC
497200930914     C                   ELSE
497300930914     C*
497400930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
497500930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
497600930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
497700930914     C     WPODFC        ANDGT     WPOFRC
497800930914     C                   ELSE
497900930916     C*
498000930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
498100930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
498200930916     C     WPODFC        ANDGT     WPOFRC
498300930916     C                   ELSE
498400930916     C*
498500930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
498600930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
498700930916     C     WPODFC        ANDLT     WPOIRC
498800930916     C                   ELSE
498900930914     C*
499000930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
499100930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
499200930914     C     WPODIC        IFNE      *ZEROS
499300930914     C     WPODIC        ANDGT     WPOFRC
499400930914     C                   Z-ADD     WPOFRC        WPODIC
499500930914     C                   ENDIF
499600930914     C*
499700930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
499800930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
499900930914     C     WPODFC        IFGT      WPOFRC
500000930914     C                   Z-ADD     WPOFRC        WPODFC
500100930914     C                   ENDIF
500200930914     C*
500300930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
500400930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
500500930914     C     WPODIC        IFNE      *ZEROS
500600930914     C     WPODIC        ANDLT     WPOIRC
500700930914     C                   Z-ADD     WPOIRC        WPODIC
500800930914     C                   ENDIF
500900930914     C*
501000930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
501100930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
501200930914     C     WPODFC        IFLT      WPOIRC
501300930914     C                   Z-ADD     WPOIRC        WPODFC
501400930914     C                   ENDIF
501500930914     C*
501600930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
501700931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
501800930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
501900930916     C*
502000931210     C***********WPODIC    IFNE *ZEROS
502100931210     C***********WPODFC    ANDNE*ZEROS
502200931210     C*********************ADD  12        WPODFC
502300931210     C*********************ENDIF
502400930914     C     WPODIC        IFEQ      *ZEROS
502500930914     C     WPODFC        ANDNE     *ZEROS
502600931130     C                   Z-ADD     WPODFC        WPODIC
502700931210     C*********************ADD  12        WPODFC
502800930916     C                   ENDIF
502900930913     C*
503000930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
503100930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
503200930914     C* INTERNA
503300050908     c                   eval      Usacons='S'
503400930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
503500930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
503600931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
503700931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
503800940324     C                   EXSR      RO9COV
503900931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
504000930914     C* CLIENTE
504100930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
504200931210     C                   EXSR      RO9COV
504300931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
504400050908     c                   eval      Usacons=' '
504500930914     C*
504600930916     C                   ENDIF                                                  <-- 5 ---
504700930916     C                   ENDIF                                                  <-- 4 ---
504800930916     C                   ENDIF                                                  <-- 3 ---
504900930914     C                   ENDIF                                                  <-- 2 ---
505000930914     C                   ENDIF                                                  <-- 1 ---
505100930913     C*
505200930913     C                   ENDSR
505300930325     C********************************************************************
505400930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
505500930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
505600930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
505700930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
505800930325     C********************************************************************
505900930325     C     RO1CDP        BEGSR
506000930414     C*
506100931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
506200931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
506300931126     C*
506400931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
506500931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
506600150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
506700150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
506800931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
506900060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
507000930326     C*
507100930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
507200931130     C                   Z-ADD     WR1POI        WR1POO            9 0
507300930325     C*
507400930325     C* CICLO FINO A CHE:
507500930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
507600930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
507700930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
507800930406     C     WR1POO        ANDLT     WPOLIM
507900930414     C*
508000930407     C                   Z-ADD     WR1POO        WPOROU            9 0
508100930325     C                   MOVEL     'N'           WSLITT            1
508200930414     C*
508300930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
508400060420     C*
508500931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
508600150605     c     wdee          andeq     ' '
508700930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
508800930414     C                   END
508900930511     C*
509000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
509100150608     c     wdee          andeq     ' '
509200930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
509300930511     C                   END
509400930414     C*
509500930325     C     WSLITT        IFEQ      'N'
509600930423     C                   SUB       12            WR1NOI            5 0
509700930325     C                   END
509800931130     C                   ADD       12            WR1POO            9 0
509900930414     C*
510000931223     C                   ENDDO                                                  <-- 1 ---
510100931126     C*
510200931126     C                   ELSE                                                   <-- 0 -->
510300931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
510400931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
510500931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
510600960129     C     WPOLAF        IFGT      WPODCG
510700960129     C                   Z-ADD     WPOLAF        WTM090            9 0
510800960129     C                   ELSE
510900960129     C                   Z-ADD     WPODCG        WTM090
511000960129     C                   ENDIF
511100960129     C     WPOGIF        IFGT      WTM090
511200960129     C                   Z-ADD     WPOGIF        WTM090
511300960129     C                   ENDIF
511400060420     C     WPOpaif       IFGT      WTM090
511500060420     C                   Z-ADD     WPOpaif       WTM090
511600060420     C                   ENDIF
511700960129     C*
511800960129     C     12            ADD       WTM090        WR1POO
511900960129     C                   ENDIF                                                  <-- 0 ---
512000930512     C*
512100931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
512200931202     C     WR1POO        SUB       12            WR7POI
512300931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
512400931202     C                   ADD       12            WR7POO
512500931202     C     WR7POO        IFGT      WR1POO
512600931202     C                   Z-ADD     WR7POO        WR1POO
512700931202     C                   END
512800930406     C*
512900930325     C                   ENDSR
513000930414     C********************************************************************
513100930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
513200930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
513300930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
513400930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
513500930414     C********************************************************************
513600930414     C     RO2COV        BEGSR
513700091014     c                   clear                   salvaEveL
513800091014     c                   clear                   salvaEveG
513900931130     C*
514000930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
514100930414     C                   Z-ADD     *ZEROS        WR2NOV
514200930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
514300931130     C                   Z-ADD     WR2POI        WR2POW
514400930414     C*
514500930414     C* CICLO FINO A CHE:
514600930414     C*   - DATA INIZIO SUPERATO DATA FINE
514700930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
514800931223     C     WR2POW        ANDLT     WPOLIM
514900930414     C*
515000930414     C                   Z-ADD     WR2POW        WPOROU
515100930414     C                   MOVEL     'N'           WSLITT            1
515200930414     C*
515300930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
515400930414     C*
515500930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
515600930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
515700930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
515800930414     C                   END
515900930414     C*
516000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
516100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
516200930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
516300930618     C                   END
516400060420     c
516500060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
516600060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
516700060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
516800060420     C                   END
516900060420     c
517000931201     C*
517100931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
517200931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
517300931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
517400931201     C                   END
517500060418     c
517600060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
517700060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
517800060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
517900060418     C                   END
518000930414     C*
518100930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
518200930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
518300930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
518400930414     C                   END
518500930511     C*
518600930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
518700930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
518800930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
518900930511     C                   END
519000930414     C*
519100930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
519200930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
519300930414     C     WSLITT        IFEQ      'N'
519400930423     C                   ADD       12            WR2NOV            5 0
519500930414     C                   END
519600931130     C                   ADD       12            WR2POW            9 0
519700930414     C*
519800931223     C                   ENDDO                                                  <-- 1 ---
519900930414     C*
520000930414     C                   ENDSR
520100941228     C********************************************************************
520200941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
520300941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
520400941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
520500941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
520600941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
520700941228     C********************************************************************
520800941228     C     RO2COP        BEGSR
520900941228     C*
521000941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
521100941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
521200941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
521300941228     C                   Z-ADD     *ZEROS        WR2NOV
521400941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
521500941228     C                   Z-ADD     WR2POI        WR2POW
521600941228     C*
521700941228     C* CICLO FINO A CHE:
521800941228     C*   - DATA INIZIO SUPERATO DATA FINE
521900941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
522000941228     C*
522100950215     C                   Z-ADD     WR2POW        WPOROU
522200941228     C                   MOVEL     'N'           WSLITT
522300941228     C* TRASFORMA IL PROGR. ORE IN DATA
522400950215     C*******              Z-ADDWR2POW    WORE
522500950215     C*******              EXSR OREDAT
522600950215     C*******              Z-ADDWDAT      W12DAT  80
522700950215     C*******              Z-ADDWRES      W12RES  30
522800941228     C*
522900941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
523000941228     C*
523100941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
523200941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
523300941228     C     WSLITT        IFEQ      'N'
523400941228     C                   ADD       12            WR2NOV            5 0
523500941228     C                   END
523600941228     C                   ADD       12            WR2POW            9 0
523700941228     C*
523800941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
523900941228     C* ENTRI IN LOOP
524000941228     C     WR2NOV        IFGT      999
524100941228     C                   LEAVE                                                  *VA A ENDDO
524200941228     C                   END
524300941228     C*
524400941228     C                   ENDDO                                                  <-- 1 ---
524500941228     C*
524600941228     C                   ENDSR
524700930510     C********************************************************************
524800930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
524900930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
525000930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
525100930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
525200050905     c*                     il num ore deve essere un multiplo di 12
525300050905     c*                     oppure 1 ora
525400930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
525500930510     C********************************************************************
525600930510     C     RO4CDP        BEGSR
525700930510     C*
525800930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
525900930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
526000050905     c                   movel     '1'           wgiro             1
526100930510     C*
526200930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
526300930510     C                   Z-ADD     WR4POI        WR4POO            9 0
526400930510     C*
526500930510     C* CICLO FINO A CHE:
526600930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
526700930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
526800930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
526900930510     C     WR4POO        ANDLT     WPOLIM
527000930510     C*
527100141106     c                   if        nosomma<>'N'
527200931130     C                   ADD       12            WR4POO            9 0
527300141106     c                   endif
527400930510     C                   Z-ADD     WR4POO        WPOROU            9 0
527500930510     C                   MOVEL     'N'           WSLITT            1
527600930510     C*
527700930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
527800050905     c
527900050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
528000930510     C     WSLITT        IFEQ      'N'
528100050905     c
528200050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
528300050905     c*  se cado in giorno non festivo
528400050905     c                   if        wr4noi=1 and wgiro='1'
528500050905     c                   sub       12            wr4poo
528600050905     c                   add       wr4noi        wr4poo
528700050905     c                   endif
528800050905     c*
528900050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
529000050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
529100050905     c*  calcolate dal pgm
529200050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
529300050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
529400050905     c*  del giorno sucessivo non festivo
529500050905     c                   if        wr4noi=1 and wgiro<>'1'
529600050905     c                   add       wr4noi        wr4poo
529700050905     c                   endif
529800050905     c
529900930510     C                   SUB       12            WR4NOI            5 0
530000930510     C                   END
530100141106     c
530200141106     c                   if        nosomma= 'N'
530300141106     C                   ADD       12            WR4POO            9 0
530400141106     c                   endif
530500930510     C*
530600050905     c                   eval      wgiro='2'
530700931223     C                   ENDDO                                                  <-- 1 ---
530800930510     C*
530900930510     C                   ENDSR
531000931130     C********************************************************************
531100931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
531200931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
531300931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
531400931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
531500931130     C********************************************************************
531600931130     C     RO5COV        BEGSR
531700931130     C*
531800931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
531900060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
532000060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
532100060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
532200931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
532300931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
532400931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
532500931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
532600931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
532700931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
532800931130     C                   Z-ADD     *ZEROS        WR5NOV
532900940714     C*
533000940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
533100940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
533200931207     C*
533300931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
533400940908     C     WIC           IFEQ      'I'
533500090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
533600090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
533700931207     C                   ELSE
533800090827     C*****              Z-ADD     D98TCC        WPOTCT
533900090827     C                   Z-ADD     w99TCC        WPOTCT
534000931207     C                   END
534100931130     C*
534200931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
534300060420     C                   clear                   COV
534400931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
534500931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
534600931207     C                   Z-ADD     2             CO                3 0
534700931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
534800931207     C                   EXSR      RO5SUB
534900981102     C                   DO        99            IC                3 0
535000931207     C     LAF(IC)       IFNE      *ZEROS
535100931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
535200931207     C                   EXSR      RO5SUB
535300931207     C                   ENDIF
535400931207     C                   ENDDO
535500060420     c
535600060420     C                   DO        99            IC                3 0
535700060420     C     paif(IC)      IFNE      *ZEROS
535800060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
535900060420     C                   EXSR      RO5SUB
536000060420     C                   ENDIF
536100060420     C                   ENDDO
536200931207     C* ORDINA LA SCHIERA COV
536300931207     C                   SORTA     COV
536400931130     C*
536500931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
536600931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
536700981102     C                   DO        98            IC                             --- 1 -->
536800931207     C     IC            ADD       1             I1C               3 0
536900931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
537000931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
537100931207     C     COV(I1C)      OREQ      *ZEROS
537200931207     C                   ELSE
537300931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
537400931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
537500931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
537600931130     C                   EXSR      RO2COV
537700931207     C                   ADD       WR2NOV        WR5NOV            9 0
537800931209     C* SE NON SONO IN DATA CONSEGNA
537900931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
538000931207     C     WR5NOV        IFLT      WPOTCT
538100931209     C     COV(I1C)      ANDNE     WPODCM
5382009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5383009709111009 C     WPOTCT        SUB       12            WR5NOV
538400931130     C                   END
538500931207     C                   ENDIF                                                  <-- 2 ---
538600931207     C                   ENDDO                                                  <-- 1 ---
538700940714     C                   ENDIF                                                  <-- 0 ---
538800931130     C*
538900931130     C                   ENDSR
539000931207     C********************************************************************
539100931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
539200931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
539300931207     C********************************************************************
539400931207     C     RO5SUB        BEGSR
539500931207     C*
539600931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
539700931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
539800931207     C     WR5COV        ANDLT     WR5POF
539900931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
540000931207     C                   MOVEL     'N'           WR5PRE            1
540100981102     C*---                 DO   30        I
540200981102     C                   DO        99            I
540300931207     C     WR5COV        IFEQ      COV(I)
540400931207     C                   MOVEL     'S'           WR5PRE
540500931207     C                   LEAVE
540600931207     C                   ENDIF
540700931207     C                   ENDDO
540800931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
540900931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
541000931207     C                   ADD       1             CO                3 0
541100931207     C                   Z-ADD     WR5COV        COV(CO)
541200931207     C                   ENDIF                                                  <-- 2 ---
541300931207     C                   ENDIF                                                  <-- 1 ---
541400931207     C*
541500931207     C                   ENDSR
541600931201     C********************************************************************
541700060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
541800931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
541900931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
542000931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
542100931201     C********************************************************************
542200931201     C     RO6CDT        BEGSR
542300931201     C*
542400931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
542500931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
542600931201     C*
542700931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
542800931201     C                   Z-ADD     WR6POI        WR6POO            9 0
542900931201     C*
543000931201     C* CICLO FINO A CHE:
543100931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
543200931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
543300931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
543400931201     C     WR6POO        ANDLT     WPOLIM
543500931201     C*
543600931201     C                   Z-ADD     WR6POO        WPOROU            9 0
543700931201     C                   MOVEL     'N'           WSLITT            1
543800931201     C*
543900931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
544000931201     C*
544100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
544200150605     c     wdee          andeq     ' '
544300931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
544400931201     C                   END
544500931201     C*
544600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
544700150608     c     wdee          andeq     ' '
544800931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
544900931201     C                   END
545000931201     C*
545100931201     C     WSLITT        IFEQ      'N'
545200931201     C                   SUB       12            WR6NOI            5 0
545300931201     C                   END
545400931201     C                   ADD       12            WR6POO            9 0
545500931201     C*
545600931223     C                   ENDDO                                                  <-- 1 ---
545700931201     C*
545800931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
545900931202     C     WR6POO        SUB       12            WR7POI
546000931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
546100931207     C                   ADD       12            WR7POO
546200931202     C     WR7POO        IFGT      WR6POO
546300931201     C                   Z-ADD     WR7POO        WR6POO
546400931202     C                   END
546500931201     C*
546600931201     C                   ENDSR
546700931201     C********************************************************************
546800060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
546900060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
547000060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
547100060407     C*            INPUT:  -WR7POI (PROGR.ORE)
547200931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
547300931201     C********************************************************************
547400931201     C     RO7TCF        BEGSR
547500060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
547600060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
547700060407     C                   MOVEL     'N'           WSLITT
547800931201     C*
547900931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
548000931201     C                   Z-ADD     WR7POI        WR7POO            9 0
548100931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
548200931201     C* COINCIDA CON UN TURNO DI CHIUSURA
548300060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
548400931201     C     D98GC2        ANDEQ     *BLANKS
548500060407   x1C                   ELSE                                                   <-- 1 -->
548600931214     C*
548700931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
548800931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
548900931214     C*
549000931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
549100931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
549200931223     C     WPOROU        ANDLT     WPOLIM
549300931214     C* MEMORIZZO INIZIO E FINE RANGE
549400931202     C     WPOTI         IFEQ      *ZEROS
549500931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
549600931202     C                   END
549700931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
549800931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
549900931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
550000931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
550100931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
550200931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
550300931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
550400931201     C                   MOVEL     'N'           WSLITT
550500931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
550600060407     c* Se non trovat, controllo se c'e' evento part che giustifica
550700060407     c                   if        wslitt='N'
550800060407     c                   exsr      datcepg
550900060407     c                   endif
551000060407     c
551100931201     C                   ENDDO                                                  <-- 2 ---
551200931214     C*
551300931201     C                   Z-ADD     WPOROU        WR7POO
551400931214     C*
551500931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
551600931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
551700931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
551800931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
551900931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
552000931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
552100931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
552200931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
552300931209     C                   ELSE                                                   <-- 3 -->
552400931209     C     WIC           IFEQ      'I'
552500931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
552600931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
552700931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
552800931201     C                   ELSE
552900931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
553000931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
553100931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
553200931201     C                   ENDIF
553300931209     C                   ENDIF                                                  <-- 3 ---
553400931209     C                   ENDIF                                                  <-- 2 ---
553500931214     C*
553600060407    1C                   ENDIF                                                  <-- 1 ---
553700060407     c
553800060407     c* Se non trovato turno di chiusura, verifico se c'e'
553900060407     c*  evento particolare che GIUSTIFICA
554000060407     c                   if        wpoti=0
554100060418     C                   EXSR      RO7CEPG
554200060407     c                   endif
554300931201     C*
554400931201     C                   ENDSR
554500060406     C********************************************************************
554600060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
554700060406     c*                        che giustifica
554800060407     C*            INPUT:-WR7POI (PROGR.ORE)
554900060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
555000060406     C********************************************************************
555100060407     C     RO7CEPG       BEGSR
555200060406     C*
555300060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
555400060407     C                   Z-ADD     WR7POI        WR7POO            9 0
555500060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
555600060406     C* coincide
555700060406    1C                   if        wevpag='S'                                   <-- 1 -->
555800060406     C*
555900060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
556000060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
556100060406     C                   MOVEL     'N'           WSLITT
556200060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
556300060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
556400060406     C*
556500060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
556600060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
556700060406     C     WPOROU        ANDLT     WPOLIM
556800060406     C* MEMORIZZO INIZIO E FINE RANGE
556900060406    3C     WPOTI         IFEQ      *ZEROS
557000060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
557100060406    3C                   END
557200060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
557300060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
557400060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
557500060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
557600060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
557700060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
557800060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
557900060406     C                   MOVEL     'N'           WSLITT
558000060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
558100060406     C* Se non trovato,
558200060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
558300060406    3c                   if        wslitt='N'
558400060406     c                   exsr      DATTUC
558500060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
558600060406    2C                   ENDDO                                                  <-- 2 ---
558700060406     C*
558800060407     C                   Z-ADD     WPOROU        WR7POO
558900060406     C*
559000060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
559100060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
559200060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
559300060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
559400060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
559500060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
559600060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
559700060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
559800060406   x3C                   ELSE                                                   <-- 3 -->
559900060406    4C     WIC           IFEQ      'I'
560000060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
560100060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
560200060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
560300060406   x4C                   ELSE
560400060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
560500060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
560600060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
560700060406    4C                   ENDIF
560800060406    3C                   ENDIF                                                  <-- 3 ---
560900060406    2C                   ENDIF                                                  <-- 2 ---
561000060406     C*
561100060406    1C                   ENDIF                                                  <-- 1 ---
561200060406     C*
561300060406     C                   ENDSR
561400931207     C********************************************************************
561500931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
561600931207     C*            INPUT:  -WR8POI (PROGR.ORE)
561700931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
561800931207     C********************************************************************
561900931207     C     RO8FES        BEGSR
562000130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
562100130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
562200931207     C*
562300931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
562400130411     C     WR8POI        SUB       12            WR8POO            9 0
562500931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
562600931207     C                   MOVEL     'N'           WSLITT
562700130411     c
562800130411     c* Non salto i festivi ???
562900130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
563000130411     c
563100931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
563200130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
563300130411     C     WPOROU        ANDGT     WPODSP
563400130411     C                   SUB       12            WPOROU
563500130411     C                   MOVEL     'N'           WSLITT
563600130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
563700130411     C                   ENDDO                                                  <-- 2 ---
563800130411     c
563900130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
564000130411     C     D98GC2        ANDEQ     *BLANKS
564100130411   x1C                   ELSE                                                   <-- 1 -->
564200130411     c* eseguo controllo per i turni di chiusura
564300130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
564400130411     C*
564500130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
564600130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
564700130411     C     WPOROU        ANDgT     WPOdsp
564800130411     C* MEMORIZZO INIZIO E FINE RANGE
564900130411     C     WPOTf         IFEQ      *ZEROS
565000130411     C                   z-add     wporou        WPOTf
565100130411     C                   END
565200140826     C     WPOROU        SUB       wnoadd        WPOROU
565300130411     c
565400130411     C                   MOVEL     'N'           WSLITT
565500130411     c                   exsr      dattuc
565600130411     c                   if        wslitt='N'
565700130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
565800140826     c* Se slitta per festivo --> tolgo sempre 12 ore
565900140826     c                   z-add     12            WNOADD
566000130411     c                   endif
566100130411     C* SLITTA
566200140826     C     wporou        add       12            WPOTi
566300130411    2C                   ENDDO                                                  <-- 2 ---
566400130411     c
566500130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
566600130411     c
566700130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
566800130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
566900130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
567000130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
567100130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
567200130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
567300130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
567400130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
567500130411   x3C                   ELSE                                                   <-- 3 -->
567600130411    4C     WIC           IFEQ      'I'
567700130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
567800130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
567900130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
568000130411   x4C                   ELSE
568100130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
568200130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
568300130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
568400130411    4C                   ENDIF
568500130411    3C                   ENDIF                                                  <-- 3 ---
568600130411    2C                   ENDIF                                                  <-- 3 ---
568700130411     C*
568800130411    1C                   ENDIF                                                  <-- 1 ---
568900130411
569000130411     C**   WPOROU        ADD       12            WR8POO
569100130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
569200130411     c                   z-add     wr8poi        wr8poo
569300931207     C*
569400931207     C                   ENDSR
569500931210     C********************************************************************
569600931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
569700940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
569800931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
569900931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
570000931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
570100931210     C********************************************************************
570200931210     C     RO9COV        BEGSR
570300931210     C*
570400931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
570500931210     C                   Z-ADD     *ZEROS        WR9NOV
570600931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
570700931210     C                   Z-ADD     WR9POI        WR9POW
570800931210     C* CICLO FINO A CHE:
570900931210     C*   - DATA INIZIO SUPERATO DATA FINE
571000931223     C*   - RAGGIUNTO LIMITE MASSIMO
571100931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
571200931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
571300931223     C     WR9POW        ANDLT     WPOLIM
571400931210     C*
571500931210     C                   Z-ADD     WR9POW        WPOROU
571600931210     C                   MOVEL     'N'           WSLITT            1
571700940324     C* FESTIVI
571800931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
571900940324     C* GIACENZA
572000931214     C     WSLITT        IFEQ      'N'
572100941229£$   C     WPOAGC        IFNE      *ZEROS
572200940324£$   C     WPOCGC        ORNE      *ZEROS
572300941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
572400940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
572500940324£$   C                   MOVEL     'S'           WSLITT
572600940324£$   C                   END
572700940324£$   C                   END
572800940324£$   C                   END
572900960129     C* EVENTI GIACENZA
573000960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
573100960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
573200960129     C                   Z-ADD     *ZEROS        G                 3 0
573300960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
573400960129     C     G             ANDLT     IG
573500960129     C                   ADD       1             G
573600960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
573700960209     C     WPOROU        ANDLE     GCF(G)
573800960129     C                   MOVEL     'S'           WSLITT
573900960129     C                   ENDIF                                                  <-- 3 ---
574000960129     C                   ENDDO                                                  <-- 2 ---
574100960129     C                   ENDIF                                                  <-- 1 ---
574200960129     C                   ENDIF                                                  <-- 0 ---
574300940324     C* LASCIATO AVVISO
574400931214     C     WSLITT        IFEQ      'N'
574500941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
574600940324£$   C                   Z-ADD     *ZEROS        L                 3 0
574700940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
574800940324£$   C     L             ANDLT     IL
574900940324£$   C                   ADD       1             L
575000941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
575100941229£$   C     WPOROU        ANDLE     LCF(L)
575200050520     c     LCon(L)       andeq     'S'
575300940324£$   C                   MOVEL     'S'           WSLITT
575400940324£$   C                   END                                                    <-- 3 ---
575500940324£$   C                   ENDDO                                                  <-- 2 ---
575600940324£$   C                   END                                                    <-- 1 ---
575700931210     C                   END
575800940324     C* TURNO CHIUSURA
575900140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
576000140826     c*  di sistemare le ore da sommare 12 o 24...
576100931214     C     WSLITT        IFEQ      'N'
576200931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
576300931210     C                   END
576400940324     C* DATA CONSEGNA RICHIESTA
576500050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
576600050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
576700050907     c
576800931214     C     WSLITT        IFEQ      'N'
576900050907     c                   add       1             wporou
577000940324£$   C     WPOCRI        IFNE      *ZEROS
577100940324£$   C     WPOROU        IFLT      WPOCRC
577200940324£$   C                   MOVEL     'S'           WSLITT
577300940324£$   C                   END
577400940324£4   C                   END
577500050907     c                   sub       1             wporou
577600931210     C                   END
577700931210     C*
577800931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
577900931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
578000931210     C     WSLITT        IFEQ      'N'
578100931210     C                   ADD       12            WR9NOV            5 0
578200931210     C                   END
578300931210     C                   ADD       12            WR9POW            9 0
578400931210     C*
578500931223     C                   ENDDO                                                  <-- 1 ---
578600931210     C*
578700931210     C                   ENDSR
578800940628%%%  C********************************************************************
578900940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
579000940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
579100940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
579200940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
579300940628%%%  C********************************************************************
579400940628%%%  C     RO0GES        BEGSR
579500940628%%%  C*
579600940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
579700940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
579800940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
579900940628%%%  C*
580000940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
580100940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
580200940628%%%  C*
580300940628%%%  C* CICLO FINO A CHE:
580400940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
580500940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
580600940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
580700050922%%%  C     WR0POO        ANDGe     WPODSP
580800940628%%%  C*
580900940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
581000940628%%%  C                   MOVEL     'N'           WSLITT            1
581100940628%%%  C*
581200940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
581300940628%%%  C*
581400940628%%%  C     WSLITT        IFEQ      'N'
581500940628%%%  C                   SUB       12            WR0NOI            5 0
581600940628%%%  C                   END
581700940628%%%  C                   SUB       12            WR0POO            9 0
581800940628%%%  C*
581900940628%%%  C                   ENDDO                                                  <-- 1 ---
582000940628%%%  C*
582100940628%%%  C                   ADD       12            WR0POO            9 0
582200940628%%%  C*
582300940628%%%  C                   ENDSR
582400960911     C********************************************************************
582500960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
582600960911     C*            INPUT:  -W10POI (PROGR.ORE)
582700960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
582800960911     C********************************************************************
582900960911     C     R10FES        BEGSR
583000960911     C*
583100960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
583200960911     C                   Z-ADD     W10POI        W10POO            9 0
583300960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
583400960911     C                   MOVEL     'N'           WSLITT
583500960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
583600960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
583700960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
583800960911     C                   ADD       12            WPOROU
583900960911     C                   MOVEL     'N'           WSLITT
584000960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
584100960911     C                   ENDDO                                                  <-- 2 ---
584200960911     C                   Z-ADD     WPOROU        W10POO
584300960911     C*
584400960911     C                   ENDSR
584500930325     C********************************************************************
584600930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
584700930325     C********************************************************************
584800930325     C     CHKGIA        BEGSR
584900930406     C*
585000960129     C* APERTURA/CHIUSURA GIACENZA
585100930326     C     WPODAG        IFNE      *ZEROS
585200930406     C     WPODCG        ORNE      *ZEROS
585300930407     C     WPOROU        IFGE      WPODAG
585400930407     C     WPOROU        ANDLE     WPODCG
585500930325     C                   MOVEL     'S'           WSLITT
585600930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
585700930325     C                   END
585800930326     C                   END
585900960129     C*
586000960129     C* EVENTI GIACENZA
586100960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
586200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
586300960129     C                   Z-ADD     *ZEROS        G                 3 0
586400960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
586500960129     C     G             ANDLT     IG
586600960129     C                   ADD       1             G
586700960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
586800960129     C     WPOROU        ANDLE     GIF(G)
586900091009     c
587000091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
587100091009     c*  l'orario dell'evento
587200091009     c                   select
587300091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
587400091009     c* Se orario entro le 10.30  va bene
587500091014     c*  solo la prima volta per evento cioè quando controllo
587600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
587700091014     c*  e non devo dare errore
587800091014     C                   MOVEL     'OSC  '       WR3TAB
587900091014     C                   eval      WR3HHI=evo(gip(G))
588000091014     C                   EXSR      RO3ORE
588100091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
588200091014     c*  in ritardo come per "E")
588300091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
588400091014     c                              wr3matpom='P'
588500091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
588600091009     C                   MOVEL     'S'           WSLITT
588700091009     c                   endif
588800091014     c                   eval      SalvaEveG=gip(G)
588900091009     c
589000091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
589100091009     c* Se orario entro le 10.30  va bene
589200091014     c*  solo la prima volta per evento cioè quando controllo
589300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
589400091014     c*  e non devo dare errore
589500091014     C                   MOVEL     'OSC  '       WR3TAB
589600091014     C                   eval      WR3HHI=evo(gip(G))
589700091014     C                   EXSR      RO3ORE
589800091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
589900091014     c*  in ritardo come per "E")
590000091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
590100091014     c                              wr3matpom='P'
590200091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
590300091009     C                   MOVEL     'S'           WSLITT
590400091009     c                   endif
590500091014     c                   eval      SalvaEveG=gip(G)
590600091009     c
590700091009     c                   other
590800960129     C                   MOVEL     'S'           WSLITT
590900960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
591000091009     c                   endsl
591100091009
591200960129     C                   ENDIF                                                  <-- 3 ---
591300960129     C                   ENDDO                                                  <-- 2 ---
591400960129     C                   ENDIF                                                  <-- 1 ---
591500960129     C                   ENDIF                                                  <-- 0 ---
591600930406     C*
591700930325     C                   ENDSR
591800060420     C********************************************************************
591900930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
592000930325     C********************************************************************
592100930325     C     CHKLAV        BEGSR
592200930325     C*
592300941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
592400930421     C                   Z-ADD     *ZEROS        L                 3 0
592500930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
592600930421     C     L             ANDLT     IL
592700930421     C                   ADD       1             L
592800930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
592900930421     C     WPOROU        ANDLE     LAF(L)
593000090928     c
593100090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
593200091009     c*  l'orario dell'evento
593300091009     c                   select
593400091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
593500091009     c* Se orario entro le 10.30  va bene
593600091014     c*  solo la prima volta per evento cioè quando controllo
593700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
593800091014     c*  e non devo dare errore
593900091014     C                   MOVEL     'OSC  '       WR3TAB
594000091014     C                   eval      WR3HHI=evo(lap(L))
594100091014     C                   EXSR      RO3ORE
594200091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
594300091014     c*  in ritardo come per "E")
594400091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
594500091014     c                              wr3matpom='P'
594600091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
594700091009     C                   MOVEL     'S'           WSLITT
594800091009     c                   endif
594900091014     c                   eval      SalvaEveL=lap(L)
595000091009     c
595100091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
595200091009     c* Se orario entro le 10.30  va bene
595300091014     c*  solo la prima volta per evento cioè quando controllo
595400091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
595500091014     c*  e non devo dare errore
595600091014     C                   MOVEL     'OSC  '       WR3TAB
595700091014     C                   eval      WR3HHI=evo(lap(L))
595800091014     C                   EXSR      RO3ORE
595900091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
596000091014     c*  in ritardo come per "E")
596100091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
596200091014     c                              wr3matpom='P'
596300091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
596400091009     C                   MOVEL     'S'           WSLITT
596500091009     c                   endif
596600091014     c                   eval      SalvaEveL=lap(L)
596700090928     c
596800091009     c                   other
596900930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
597000091009     C                   MOVEL     'S'           WSLITT
597100091009     c                   endsl
597200090928     c
597300930421     C                   END                                                    <-- 3 ---
597400931223     C                   ENDDO                                                  <-- 2 ---
597500930421     C                   END                                                    <-- 1 ---
597600930325     C*
597700930325     C                   ENDSR
597800060420     C********************************************************************
597900060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
598000060420     c*                   che Allungano
598100060420     C********************************************************************
598200060420     C     CHKCEPA       BEGSR
598300060420     C*
598400060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
598500060420     C                   Z-ADD     *ZEROS        p                 3 0
598600060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
598700060420     C     p             ANDLT     IpA
598800060420     C                   ADD       1             P
598900060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
599000060420     C     WPOROU        ANDLE     paif(P)
599100060420     C                   MOVEL     'S'           WSLITT
599200060420     C                   ADD       12            W98EA
599300060420     C                   END                                                    <-- 3 ---
599400060420     C                   ENDDO                                                  <-- 2 ---
599500060420     C                   END                                                    <-- 1 ---
599600060420     C*
599700060420     C                   ENDSR
599800930414     C********************************************************************
599900930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
600000930414     C********************************************************************
600100930414     C     CHKCNT        BEGSR
600200930511     C*
600300930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
600400930611     C     WIC           IFEQ      'I'                                          *INTERNO
600500930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
600600930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
600700930611     C                   ELSE
600800930611     C     WPOROU        IFGE      WPOAII
600900930611     C     WPOROU        ANDLE     WPOAFI
601000930611     C                   MOVEL     'S'           WSLITT
601100930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
601200930611     C                   END
601300930611     C                   END
601400930611     C                   ELSE                                                   *CLIENTE
601500930611     C     WPOAIC        IFEQ      *ZEROS
601600930611     C     WPOAFC        ANDEQ     *ZEROS
601700930611     C                   ELSE
601800930611     C     WPOROU        IFGE      WPOAIC
601900930611     C     WPOROU        ANDLE     WPOAFC
602000930611     C                   MOVEL     'S'           WSLITT
602100930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
602200930611     C                   END
602300930611     C                   END
602400930611     C                   END
602500930414     C*
602600930414     C                   ENDSR
602700930511     C********************************************************************
602800930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
602900930511     C********************************************************************
603000930511     C     CHKFED        BEGSR
603100930511     C*
603200930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
603300930611     C     WIC           IFEQ      'I'                                          *INTERNO
603400930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
603500930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
603600930611     C                   ELSE
603700930611     C     WPOROU        IFGE      WPOFII
603800930611     C     WPOROU        ANDLE     WPOFFI
603900930611     C                   MOVEL     'S'           WSLITT
604000930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
604100930611     C                   END
604200930611     C                   END
604300930611     C                   ELSE                                                   *CLIENTE
604400930611     C     WPOFIC        IFEQ      *ZEROS
604500930611     C     WPOFFC        ANDEQ     *ZEROS
604600930611     C                   ELSE
604700930611     C     WPOROU        IFGE      WPOFIC
604800930611     C     WPOROU        ANDLE     WPOFFC
604900930611     C                   MOVEL     'S'           WSLITT
605000930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
605100930611     C                   END
605200930611     C                   END
605300930611     C                   END
605400930511     C*
605500930511     C                   ENDSR
605600930325     C********************************************************************
605700930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
605800930325     C********************************************************************
605900930325     C     CHKFES        BEGSR
606000060526     c* se già passata la data, non controllo wporou
606100060526     c                   if        wpodata>0
606200060526     c                   z-add     wpodata       w12dat
606300060526     c                   clear                   w12res
606400060526     c                   else
606500931130     C*
606600931130     C* TRASFORMA IL  PROGR.ORE IN DATA
606700931130     C                   Z-ADD     WPOROU        WORE
606800931130     C                   EXSR      OREDAT
606900941227     C                   Z-ADD     WDAT          W12DAT            8 0
607000931130     C                   Z-ADD     WRES          W12RES            3 0
607100060526     c                   endif
607200930325     C*
607300941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
607400930325     C* E' FESTIVO
607500090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
607600090108     C**                 Z-ADD     D98LNP        CLNTFP
607700090108     C**                 Z-ADD     *ZEROS        CLNTFA
607800090108     C**                 ENDIF
607900090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
608000090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
608100090108     c**   è possibile rendere festivo in terminal per un altro terminal
608200090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
608300930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
608400090108     C     WTC           oreq      'P'                                          *TRAZIONE
608500941227     C                   Z-ADD     D98TFP        CLNTFP
608600941227     C                   Z-ADD     D98TFA        CLNTFA
608700941228     C                   ENDIF
608800941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
608900941227     C                   Z-ADD     *ZEROS        CLNTFP
609000941227     C                   Z-ADD     D98LNA        CLNTFA
609100941228     C                   ENDIF
609200941227     C                   Z-ADD     W12DAT        DSDAT8
609300941227     C                   Z-ADD     DSAAS         CLNANN
609400941227     C                   Z-ADD     DSMES         CLNMES
609500930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
609600930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
609700930421     C                   Z-ADD     DSGIO         G
609800930415     C     W12RES        IFEQ      *ZEROS
609900941227     C                   MOVEL     CLNMAT        GFEDS
610000930415     C                   ELSE
610100941227     C                   MOVEL     CLNPOM        GFEDS
610200930415     C                   END
610300930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
610400930405     C                   MOVEL     *BLANKS       WC1Y
610500930421     C                   MOVEL     GFE(G)        WC1Y              8
610600930421     C                   Z-ADD     1             I
610700930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
610800930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
610900930405     C     WIC           IFEQ      'I'
611000930421     C     I1Y(I)        ANDEQ     'S'
611100930405     C                   MOVEL     'S'           WSLITT
611200930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
611300930405     C                   END
611400930405     C     WIC           IFEQ      'C'
611500930421     C     E1Y(I)        ANDEQ     'S'
611600930405     C                   MOVEL     'S'           WSLITT
611700930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
611800930405     C                   END
611900970204     C     WIC           IFEQ      'X'
612000970204     C     E1Y(I)        ANDEQ     'S'
612100970204     C                   MOVEL     'S'           WSLITT
612200970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
612300970204     C                   END
612400930405     C                   ELSE                                                   <-- 3 -->
612500930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
612600930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
612700930405     C                   END                                                    <-- 3 ---
612800930405     C                   END                                                    <-- 2 ---
612900930405     C                   END                                                    <-- 1 ---
613000930325     C*
613100060526     c                   clear                   wpodata
613200930325     C                   ENDSR
613300931201     C********************************************************************
613400931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
613500931201     C********************************************************************
613600931201     C     CHKTUC        BEGSR
613700931201     C*
613800931201     C     WIC           IFEQ      'I'                                          --- 1 -->
613900931201     C                   DO        TI            T                 3 0          --- 2 -->
614000931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
614100931201     C     WPOROU        ANDLE     TIF(T)
614200931201     C                   MOVEL     'S'           WSLITT
614300931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
614400931201     C                   LEAVE                                                  *VA A FINE DO
614500931201     C                   ENDIF                                                  <-- 3 ---
614600931201     C                   ENDDO                                                  <-- 2 ---
614700931201     C                   ELSE                                                   <-- 1 -->
614800931201     C                   DO        TC            T                 3 0          --- 2 -->
614900931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
615000931201     C     WPOROU        ANDLE     TCF(T)
615100931201     C                   MOVEL     'S'           WSLITT
615200931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
615300931201     C                   LEAVE                                                  *VA A FINE DO
615400931201     C                   ENDIF                                                  <-- 3 ---
615500931201     C                   ENDDO                                                  <-- 2 ---
615600931201     C                   ENDIF                                                  <-- 1 ---
615700931201     C*
615800931201     C                   ENDSR
615900060418     C********************************************************************
616000060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
616100060418     c*          - eventi particolari che giustificano
616200060418     C********************************************************************
616300060418     C     CHKCEPG       BEGSR
616400060418     C*
616500060418     C     WIC           IFEQ      'I'                                          --- 1 -->
616600060420     C                   DO        PGI           p                 3 0          --- 2 -->
616700060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
616800060420     C     WPOROU        ANDLE     PGIF(p)
616900060418     C                   MOVEL     'S'           WSLITT
617000060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
617100060418     C                   LEAVE                                                  *VA A FINE DO
617200060418     C                   ENDIF                                                  <-- 3 ---
617300060418     C                   ENDDO                                                  <-- 2 ---
617400060418     C                   ELSE                                                   <-- 1 -->
617500060420     C                   DO        PGC           p                 3 0          --- 2 -->
617600060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
617700060420     C     WPOROU        ANDLE     PGCF(p)
617800060418     C                   MOVEL     'S'           WSLITT
617900060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
618000060418     C                   LEAVE                                                  *VA A FINE DO
618100060418     C                   ENDIF                                                  <-- 3 ---
618200060418     C                   ENDDO                                                  <-- 2 ---
618300060418     C                   ENDIF                                                  <-- 1 ---
618400060418     C*
618500060418     C                   ENDSR
618600930512     C********************************************************************
618700931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
618800930512     C********************************************************************
618900931130     C     DATTUC        BEGSR
619000931214     C*
619100931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
619200931214     C                   Z-ADD     12            WNOADD            9 0
619300930325     C*
619400931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
619500931125     C     D98GC2        ORNE      *BLANKS
619600931130     C*
619700931213     C                   Z-ADD     WPOROU        WORE
619800931130     C                   EXSR      OREDAT                                       *ORE->DATA
619900931130     C                   Z-ADD     WDAT          W12DAT
620000931130     C                   Z-ADD     WRES          W12RES
620100930405     C*
620200941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
620300930405     C* DELLA SETTIMANA
620400941227     C                   Z-ADD     W12DAT        DSDAT8
620500941227     C                   Z-ADD     DSAAS         CLNANN
620600941227     C                   Z-ADD     DSMES         CLNMES
620700941227     C                   Z-ADD     *ZEROS        CLNTFP
620800941227     C                   Z-ADD     *ZEROS        CLNTFA
620900930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
621000930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
621100941227     C                   MOVEL     CLNMAT        GSEDS
621200960129     C                   Z-ADD     DSGIO         GI                6 0
621300960129     C                   MOVEL     GSE(GI)       WGSE              1
621400140911     c
621500140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
621600140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
621700140911     c                   if        wtuctut='S'
621800140911     c                   eval      wtuctut='P'
621900140911     c                   endif
622000930326     C*
622100931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
622200931213     C                   DO        4             IT                3 0          --- 3 -->
622300931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
622400931213     C                   MOVEL     TUC(IT)       W93GSE            1
622500931213     C                   MOVE      TUC(IT)       W93GMP            1
622600930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
622700930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
622800931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
622900930512     C     W93GSE        OREQ      ' '
623000140911     c     WTUCTUT       andeq     ' '
623100140911     c
623200930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
623300931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
623400930401     C     W93GMP        OREQ      ' '
623500930325     C                   MOVEL     'S'           WSLITT
623600931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
623700931213     C                   ELSE                                                   <-- 6 -->
623800001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
623900001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
624000001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
624100050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
624200050908     C     Usacons       IFNE      'S'                                          --- 7 -->
624300141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
624400141107     c*    orari giorno per giorno
624500141107     c                   if        OOR2FCSGL<>' '
624600141108     c                             and w12dat<>sav_12dat
624700141107     c                   eval      or_datarif=w12dat
624800141107     c                   eval      or_DDC    =w12dat
624900141107     c                   exsr      Reper_tempi
625000141108     c                   eval      sav_12dat=w12dat
625100141107     c                   endif
625200141107     c
625300930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
625400930325     C     W93GMP        IFEQ      'M'
625500930326     C     W12RES        ANDEQ     *ZEROS
625600141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
625700141111     c                             or OOR2STfS =0
625800141105     c                             or (tuc(IT)<>gc1part and
625900141105     c                              tuc(IT)<>gc2part)
626000140825     C                   MOVEL     'S'           WSLITT
626100140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
626200140911     c* slitto solo una volta  se chiuso tutti " M"
626300140911     c                   if        W93GSE=' '
626400140911     c                   eval      wTUCTUT='S'
626500140911     c                   endif
626600140911     c
626700140825     c                   endif
626800930325     C                   END
626900930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
627000930325     C     W93GMP        IFEQ      'P'
627100930326     C     W12RES        ANDNE     *ZEROS
627200140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
627300140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
627400141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
627500141111     c                             or OOR2STiS =0
627600141105     c                             or (tuc(IT)<>gc1part and
627700141105     c                              tuc(IT)<>gc2part)
627800930325     C                   MOVEL     'S'           WSLITT
627900140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
628000140911     c* slitto solo una volta  se chiuso tutti " P"
628100140911     c                   if        W93GSE=' '
628200140911     c                   eval      wTUCTUT='S'
628300140911     c                   endif
628400140825     c                   endif
628500140825     c
628600931213     C                   ENDIF
628700001128     C                   ENDIF                                                  <-- 7 ---
628800931213     C                   ENDIF                                                  <-- 6 ---
628900931213     C                   ENDIF                                                  <-- 5 ---
629000931213     C                   ENDIF                                                  <-- 4 ---
629100931213     C                   ENDDO                                                  <-- 3 ---
629200930405     C*
629300931213     C                   ENDIF                                                  <-- 2 ---
629400931213     C                   ENDIF                                                  <-- 1 ---
629500140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
629600140911     c                   if        wtuctut='P'  and wslitt='N'
629700140911     c                   clear                   wtuctut
629800140911     c                   endif
629900140911     c
630000930325     C                   ENDSR
630100060406     C********************************************************************
630200060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
630300060406     C********************************************************************
630400060406     C     DATCEPG       BEGSR
630500060406     C*
630600060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
630700060406     C                   Z-ADD     12            WNOADD            9 0
630800060406     C*
630900060406    1C                   if        wevpag='S'
631000060406     C*
631100060406     C                   Z-ADD     WPOROU        WORE
631200060406     C                   EXSR      OREDAT                                       *ORE->DATA
631300060406     C                   Z-ADD     WDAT          W12DAT
631400060406     C                   Z-ADD     WRES          W12RES
631500060418     c                   z-add     1             P
631600060406     c* Verifico se nella data esiste un evento Part che giustifica
631700060418     c     w12dat        lookup    pvd(P)                                 30
631800060418    2c                   dow       *in30 and pvf(P)<>'G'
631900060418     c                   add       1             P
632000060420     c     w12dat        lookup    pvd(P)                                 30
632100060406    2c                   enddo
632200060406     c
632300060406     c* 30 on --> TROVATO !!
632400060406    2c                   if        *in30
632500060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
632600060406     C                   MOVEL     'S'           WSLITT
632700060406     c                   z-add     24            wnoadd
632800060406    3C                   ENDIF
632900060406     C*
633000060406    2C                   ENDIF                                                  <-- 2 ---
633100060406    1C                   ENDIF                                                  <-- 1 ---
633200060406     C*
633300060406     C                   ENDSR
633400931210     C********************************************************************
633500931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
633600931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
633700931210     C********************************************************************
633800050805     C***  CHKDCR        BEGSR
633900931210     C*
634000050805     C***  WPOCRI        IFNE      *ZEROS
634100050805     C***  WPOROU        IFGE      WPOCRI
634200050805     C***  WPOROU        ANDLT     WPOCRF
634300050805     C***                ELSE
634400050805     C***                MOVEL     'S'           WSLITT
634500050805     C***                END
634600050805     C***                END
634700931210     C*
634800050805     C***                ENDSR
634900140822     C********************************************************************
635000140822     C*  Reperimento tempi di consegna per decidere se slittare
635100140822     C********************************************************************
635200140822     C     Reper_tempi   BEGSR
635300140822
635400140822     c                   clear                   trulorSds
635500140825     c                   clear                   trulor2ds
635600140822     c                   eval       IOREtfp = d98tfp
635700140822     c                   eval       IOREtfa = d98tfa
635800140825     c                   eval       IOREdti = d98dti
635900140825     c                   eval       IOREhti = d98hti
636000140825     c                   eval       IOREdcr = s98dcr
636100140825     c                   eval       IOREhcr = s98hcr
636200140825     c                   eval       IOREtcr = s98tcr
636300140825     c                   eval       IOREDEI = d98dei
636400140825     c                   eval       IOREOEI = d98oei
636500140825     c                   eval       IOREfil = d98lna
636600140825     c                   eval       IOREcap = d98cad
636700140825     c                   eval       IOREloc = d98lod
636800140825     c                   eval       IOREnar = d98nzd
636900140825     c
637000140825     c* Data di Riferimento:
637100140825     c* Se impostata la consegnabilità --> prendo questa
637200140822     c                   select
637300140825     c                   when      d98dfc>0
637400141107     c                   if        or_datarif<d98dfc and or_datarif>0
637500141107     c                   eval      ioredta=or_datarif
637600141107     c                   else
637700141107     c                   eval      ioredta=d98dfc
637800141107     c                   endif
637900140825     c* altrimenti data di consegna
638000140825     c                   when      d98dcm>0
638100141107     c                   if        or_datarif<d98dcm and or_datarif>0
638200141107     c                   eval      ioredta=or_datarif
638300141107     c                   else
638400140825     c                   eval      ioredta=d98dcm
638500141107     c                   endif
638600140825     c
638700140822     c                   other
638800140825     c* altrimenti data teorica consegna se > = oggi
638900140825     C                   time                    wrkoggiiso
639000140825     C     *iso          move      wrkoggiiso    ioredta
639100140822     c* se < di oggi, imposto oggi
639200140825     c                   if        d98dei>ioredta
639300140825     c                   eval      ioredta=d98dei
639400140822     c                   endif
639500141107     c
639600141107     c                   if        or_datarif<ioredta and or_datarif>0
639700141107     c                   eval      ioredta=or_datarif
639800141107     c                   endif
639900141107     c
640000140822     c                   endsl
640100140822     c
640200140825     c                   eval       IOREtser = 'C'
640300141107     c                   if         or_ddc>0 and or_ddc<d98ddc
640400141107     c                   eval       IOREDDC=or_ddc
640500141107     c                   eval       IOREnDC=8888888
640600141107     c                   eval       IOREcons = 'S'
640700141107     c                   else
640800140825     c                   eval       IOREDDC=d98ddc
640900140825     c                   eval       IOREnDC=wd98ndc
641000140825     c                   eval       IOREcons = d98cons
641100141107     c                   endif
641200141017     c                   eval       IOREtsp = d98tsp
641300140822     c
641400140822     c                   call      'TRULORSR'
641500140822     c                   parm                    kpjba
641600140822     c                   parm                    trulorSds
641700140825     c                   parm                    trulor2ds
641800140825     c
641900140825     c                   eval      wtrulor= 'S'
642000141107     c                   clear                   or_datarif        8 0
642100141107     c                   clear                   or_ddc            8 0
642200140822     C                   ENDSR
642300950406     C********************************************************************
642400950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
642500950406     C********************************************************************
642600950406     C     DATORE        BEGSR
642700950406     C*
642800950406     C                   Z-ADD     *ZEROS        WORE
642900950406     C     WDAT          IFNE      0
643000950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
643100950406     C     WAA           DIV       4             WQUA              4 0
643200950406     C                   MVR                     WRES
643300950406     C     WAA           MULT      365           WGIO              9 0
643400950406     C                   ADD       WQUA          WGIO
643500950406     C                   ADD       WGG           WGIO
643600950406     C     WMM           SUB       1             MC                3 0
643700950406     C                   DO        MC            WX                3 0
643800950406     C                   ADD       MES(WX)       WGIO
643900950406     C                   END
644000950406     C     WRES          IFEQ      0
644100950406     C     WMM           ANDLE     2
644200950406     C                   SUB       1             WGIO
644300950406     C                   END
644400950406     C     WGIO          MULT      24            WORE              9 0
644500950406     C                   ENDIF
644600950406     C*
644700950406     C                   ENDSR
644800930323     C********************************************************************
644900930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
645000930323     C********************************************************************
645100930323     C     OREDAT        BEGSR
645200930323     C*
645300950407     C                   SETOFF                                       717273
645400950407     C                   SETOFF                                       7475
645500930323     C     WORE          DIV       24            WGIO
645600930323     C                   MVR                     WRES              3 0
645700950406     C*
645800950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
645900950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
646000950406     C*  TROVO ANNO
646100950406     C     WGIO          DIV       365           WAA
646200950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
646300950406     C                   MVR                     WREST                71
646400950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
646500950406     C     WGIO          DIV       365           WAA
646600950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
646700950406     C*                                                    0 = 31/12/PR
646800950406     C   75
646900950406     CAN 71              DO                                                     BIS. E NON ERA
647000950406     C     WAA           SUB       1             WAA
647100950406     C                   Z-ADD     365           WREST
647200950406     C                   END
647300950406     C*
647400950406     C     WAA           DIV       4             WNBIS                                      T.
647500950406     C                   MVR                     WRBIS             3 073
647600950406     C   73
647700950406     CANN71                                                                     NON PIU' BISEST
647800950406     CORN73              ADD       1             WREST                          E'BISESTILE
647900950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
648000950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
648100950406     C  N73              SUB       1             $PM(1)
648200950406     C                   Z-ADD     1             WI                2 0
648300950406     C* TROVO IL MESE E GIORNI
648400950406     C     WREST         LOOKUP    $PM(WI)                            74  73
648500950406     C                   MOVE      WI            WMM
648600950406     C     WI            COMP      1                                      72
648700950406     C  N72
648800950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
648900950406     C   74              SUB       1             WI
649000950406     C  N72
649100950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
649200950406     C   72              MOVE      WREST         WGG
649300950406     C                   ADD       1900          WAA
649400950406     C*
649500930323     C*
649600930323     C                   ENDSR
6497009407151507 C********************************************************************
6498009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6499009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6500009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6501009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6502009407151507 C********************************************************************
6503009407151507 C     FINGIO        BEGSR
6504009407151507 C*
6505009407151507 C     WPORE         DIV       24            WGIO
6506009407151507 C                   MVR                     WRES
6507009407151507 C     WRES          IFEQ      *ZEROS
6508009407151507 C                   Z-ADD     12            WNORE             3 0
6509009407151507 C                   ELSE
6510009407151507 C                   Z-ADD     *ZEROS        WNORE
6511009407151507 C                   ENDIF
6512009407151507 C*
6513009407151507 C                   ENDSR
651400930517     C********************************************************************
651500971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
651600930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
651700930517     C********************************************************************
651800930517     C     CERCAL        BEGSR
651900930517     C*
652000930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
652100941227     C                   MOVE      CLNTFP        DCATFP
652200941227     C                   MOVE      CLNTFA        DCATFA
652300941227     C                   MOVE      CLNANN        DCAANN
652400941227     C                   MOVE      CLNMES        DCAMES
652500930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
652600930517     C                   Z-ADD     1             I
652700930517     C     DCALEN        LOOKUP    CAK(I)                                 99
652800930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
652900930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
653000941227     C     KEYCLN        CHAIN     AZCLN01L                           99
653100930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
653200930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
653300930517     C                   MOVEL     '1'           D98ERR
653400930517     C                   MOVEL     MSG(3)        D98MSG
653500930517     C                   ELSE                                                   <-- 2 -->
653600970401     C* CONTROLLA SE ANNULLATO
653700970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
653800970401     C                   MOVEL     '1'           D98ERR
653900970401     C                   MOVEL     MSG(3)        D98MSG
654000970401     C                   ELSE                                                   <-- 3 -->
654100930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
654200930517     C                   ADD       1             WINDCA            6 0
654300970401     C     WINDCA        IFLE      200                                          --- 4 -->
654400970401     C     WINDCA        ANDGT     *ZEROS
654500930517     C                   Z-ADD     WINDCA        I
654600930517     C                   MOVEL     DCALEN        CAK(I)
654700941227     C                   MOVEL     CLNMAT        CAM(I)
654800941227     C                   MOVEL     CLNPOM        CAP(I)
654900970401     C                   END                                                    <-- 4 ---
655000970401     C                   ENDIF                                                  <-- 3 ---
655100930517     C                   END                                                    <-- 2 ---
655200930517     C                   ELSE                                                   <-- 1 -->
655300930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
655400941227     C                   MOVEL     CAM(I)        CLNMAT
655500941227     C                   MOVEL     CAP(I)        CLNPOM
655600930517     C                   END                                                    <-- 1 ---
655700930517     C*
655800930517     C                   ENDSR
655900010821     C********************************************************************
656000010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
656100010821     C********************************************************************
656200010821     C     CHKVCA        BEGSR
656300010821     C*
656400010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
656500010821     C     D98CAS        IFGT      *ZEROS
656600010821     C     D98VCA        ANDEQ     *BLANKS
656700010821     C                   MOVEL     'ITL'         D98VCA
656800010821     C                   ENDIF
656900010822     C*
657000010822     C     D98VCA        IFNE      'ITL'
657100010822     C     D98VCA        ANDNE     'EUR'
657200010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
657300010822     C                   Z-ADD     D98CAS        INPIMP
657400010822     C                   MOVEL     D98VCA        INPDIV
657500010822     C                   EXSR      EXECNV
657600010822     C                   Z-ADD     OUTIMP        D98CAS
657700010822     C                   MOVEL     §GEDCN        D98VCA
657800010822     C                   ENDIF
657900010821     C*
658000010821     C                   ENDSR
658100010821     C********************************************************************
658200010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
658300010821     C********************************************************************
658400010821     C     EXECNV        BEGSR
658500010821     C*
658600010821     C                   CLEAR                   YEURDS
658700010821     C*
658800010821     C                   MOVEL     INPDIV        YECDVI
658900010821     C                   Z-ADD     INPIMP        YECIMI
659000010821     C                   MOVEL     §GEDCN        YECDVO
659100010821     C                   MOVEL     '3'           YECDCO
659200010821     C                   MOVEL     'H'           YECTAR
659300010821     C                   CALL      'YEURCO'
659400010821     C                   PARM                    YEURDS
659500010821     C*
659600010821     C                   Z-ADD     YECIMO        OUTIMP
659700010821     C*
659800010821     C                   ENDSR
659900011211     C*------------------------------------------------------------------------*
660000011211     C* Controllo importo massimo contrassegno.
660100011211     C*------------------------------------------------------------------------*
660200011211     C     REPMAXCSB     BEGSR
660300011211     C*
660400011211     C                   CLEAR                   Trul21DS
660500011211     C*
660600011211     C                   EVAL      I21TSp = d98TSP
660700011211     C                   EVAL      I21NzD = d98NZD
660800011211     C                   EVAL      I21LnP = d98LNP
660900011211     C                   EVAL      I21LnA = d98LNA
661000011211     C                   EVAL      I21Ksc = d98KSC
661100011211     C                   EVAL      I21TiC = d98TIC
661200011211     C                   EVAL      I21Imp = d98CAS
661300011211     C                   EVAL      I21Div = d98VCA
661400011211     C*
661500011211     C                   CALL      'TRUL21R'
661600011211     C                   PARM                    Trul21DS
661700011211     C*
661800011211     C                   ENDSR
661900011211     C*------------------------------------------------------------------------*
662000011211
662100930414     C********************************************************************
662200930414     C*   RUTINZ - ROUTINE INIZIALE
662300930414     C********************************************************************
662400930414     C     RUTINZ        BEGSR
662500930414     C*
662600930414     C* PARAMETRI IN INPUT ALLA ROUTINE
662700930414     C     *ENTRY        PLIST
662800970731     C                   PARM                    DS99
662900941227     C*
663000941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
663100941227     C     KEYBLP        KLIST
663200941227     C                   KFLD                    BLPAAS
663300941227     C                   KFLD                    BLPLNP
663400941227     C                   KFLD                    BLPNRS
663500941227     C                   KFLD                    BLPNSP
663600050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
663700941227     C     KEYGCP        KLIST
663800050405     C                   KFLD                    KWGCPAAS
663900050405     C                   KFLD                    KWGCPLNP
664000050405     C                   KFLD                    KWGCPNRS
664100050405     C                   KFLD                    KWGCPNSP
664200050405     C                   KFLD                    KWGCPFRG
664300050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
664400031125     C     KEYGC_P       KLIST
664500050405     C                   KFLD                    KWGCPAAS
664600050405     C                   KFLD                    KWGCPLNP
664700050405     C                   KFLD                    KWGCPNRS
664800050405     C                   KFLD                    KWGCPNSP
664900970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
665000150120     C**   KEYBLT        KLIST
665100150120     C**                 KFLD                    BLTAAS
665200150120     C**                 KFLD                    BLTLNP
665300150120     C**                 KFLD                    BLTNRS
665400150120     C**                 KFLD                    BLTNSP
665500941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
665600941227     C     KEYARB        KLIST
665700941227     C                   KFLD                    ARBAAS
665800941227     C                   KFLD                    ARBLNP
665900941227     C                   KFLD                    ARBNRS
666000941227     C                   KFLD                    ARBNSP
666100051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
666200941228     C     KEYAR9        KLIST
666300941228     C                   KFLD                    AR9AAS
666400941228     C                   KFLD                    AR9LNP
666500941228     C                   KFLD                    AR9NRS
666600941228     C                   KFLD                    AR9NSP
666700941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
666800050405     C     KEYGCP21      KLIST
666900050405     C                   KFLD                    KWGCPAAS
667000050405     C                   KFLD                    KWGCPLNP
667100050405     C                   KFLD                    KWGCPNRS
667200050405     C                   KFLD                    KWGCPNSP
667300050405     C                   KFLD                    KWGCPFRG
667400970912     C* CHIAVE LETTURA FNART01L - PARZIALE
667500150120     C***  KEYART        KLIST
667600150120     C***                KFLD                    ARTAAS
667700150120     C***                KFLD                    ARTLNP
667800150120     C***                KFLD                    ARTNRS
667900150120     C***                KFLD                    ARTNSP
668000950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
668100950102     C     KEYLBL        KLIST
668200950102     C                   KFLD                    LBLAAN
668300950102     C                   KFLD                    LBLLPN
668400950102     C                   KFLD                    LBLNRN
668500950102     C                   KFLD                    LBLNSN
668600930414     C*
668700070827     c     kAR5          Klist
668800070827     c                   Kfld                    d98Aas
668900070827     c                   Kfld                    d98Lnp
669000070827     c                   Kfld                    d98nrs
669100070827     c                   Kfld                    d98nsp
669200070827     c                   Kfld                    kAr5Trd
669300150325     c     kAR4          Klist
669400150325     c                   Kfld                    d98Aas
669500150325     c                   Kfld                    d98Lnp
669600150325     c                   Kfld                    d98nrs
669700150325     c                   Kfld                    d98nsp
669800150325     c                   Kfld                    kAr4Trc
669900150218     c*
670000150218     c     kart          Klist
670100150218     c                   Kfld                    d98Aas
670200150218     c                   Kfld                    d98Lnp
670300150218     c                   Kfld                    d98nrs
670400150218     c                   Kfld                    d98nsp
670500150218     c     kbrv          Klist
670600150218     c                   Kfld                    artfls
670700150218     c                   Kfld                    artlna
670800150218     c                   Kfld                    artnrs
670900150218     c                   Kfld                    artnsc
671000150325     C     Kfapd         klist
671100150325     C                   kfld                    kpdtip
671200150325     C                   kfld                    kpdpdr
671300930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
671400930414     C     KEYTAB        KLIST
671500930414     C                   KFLD                    KTBUTE
671600930414     C                   KFLD                    KTBCOD
671700930414     C                   KFLD                    KTBKEY
671800060406     C     Ktab          KLIST
671900060406     C                   KFLD                    KTBUTE
672000060406     C                   KFLD                    KTBCOD
672100930414     C*
672200941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
672300941227     C     KEYCLN        KLIST
672400941227     C                   KFLD                    CLNTFP
672500941227     C                   KFLD                    CLNTFA
672600941227     C                   KFLD                    CLNANN
672700941227     C                   KFLD                    CLNMES
672800941227     C*
672900941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
673000941227     C     KEYCAS        KLIST
673100941227     C                   KFLD                    CASPTA
673200941227     C                   KFLD                    CASLNP
673300941227     C                   KFLD                    CASLNA
673400941227     C                   KFLD                    CASTSP
673500941227     C                   KFLD                    CASDDE
673600930420     C*
673700990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
673800930420     C     KEYEVB        KLIST
673900930420     C                   KFLD                    KEBAAS
674000930420     C                   KFLD                    KEBLNP
674100930420     C                   KFLD                    KEBNRS
674200930420     C                   KFLD                    KEBNSP
674300010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
674400010821     C     KTBE          KLIST
674500010821     C                   KFLD                    TBECOD
674600010821     C                   KFLD                    TBEKE1
674700010821     C                   KFLD                    TBEKE2
674800930414     C*
674900010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
675000930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
675100930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
675200930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
675300930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
675400930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
675500930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
675600930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
675700930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
675800930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
675900010821     C     *LIKE         DEFINE    D98CAS        INPIMP
676000010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
676100010821     C     *LIKE         DEFINE    D98VCA        INPDIV
676200930414     C*
676300930414     C* RECUPERO IL CODICE DELL'AZIENDA
676400930414     C                   Z-ADD     1             CODUT             1 0
676500970701     C                   Z-ADD     *ZEROS        I                 6 0
676600930414     C*
676700930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
676800930414     C                   EXSR      AZZERA
676900930414     C*
677000930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
677100941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
677200941227     C                   MOVEL     'S'           CTABSN            1
677300930414     C                   EXSR      CARTAB
677400941227     C                   ENDIF
677500041116     c* Se passato umero di spedizioni apro i files , altrimenti no
677600041116     c                   if        d98nsp>0 and wapri=' '
677700041116     c                   open      fnblp01l
677800050405     c                   open      tigcp01l
677900150120     c***                open      fnblt01l
678000041116     c                   open      fnarb01l
678100051115     c                   open      fiar901l
678200150325     c                   open      fiar401l
678300050405     c                   open      tigcp21l
678400150218     c                   open      fnart01l
678500150218     c                   open      fnbrv07l
678600041116     c                   open      fnlbl01l
678700041116     c                   open      fnevb01l
678800070827     c                   open      fiar501l
678900041116     c                   eval      wapri='S'
679000041116     c                   endif
679100930414     C*
679200930414     C                   ENDSR
679300941227     C*------------------------------------------------------------------------*
679400941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
679500941227     C*------------------------------------------------------------------------*
679600941227     C     SUBDAT        BEGSR
679700941227     C*
679800941227     C                   TIME                    DTTIME
679900941227     C                   Z-ADD     DTANN         DSANN
680000941227     C                   Z-ADD     DTMES         DSMES
680100941227     C                   Z-ADD     DTGIO         DSGIO
680200941227     C                   Z-ADD     DTSEC         DSSEC
680300941227     C*
680400941227     C                   ENDSR
680500930414     C********************************************************************
680600930414     C*   CARTAB - CARICA LE TABELLE
680700930414     C********************************************************************
680800930414     C     CARTAB        BEGSR
680900930414     C*
681000930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
681100930414     C     D98ERR        IFEQ      '0'
681200930421     C                   Z-ADD     0             I
681300930414     C                   Z-ADD     CODUT         KTBUTE
681400930414     C                   MOVEL     '1Y'          KTBCOD
681500930414     C                   MOVEL     *BLANKS       KTBKEY
681600930414     C     KEYTAB        SETLL     TABEL00F                           99
681700930414     C  N99              READ      TABEL00F                               99
681800930414     C     *IN99         IFEQ      '1'
681900930414     C                   MOVEL     '1'           D98ERR
682000930414     C                   MOVEL     MSG(5)        D98MSG
682100930414     C                   END
682200930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
682300930414     C     TBLCOD        ANDEQ     '1Y'
682400930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
682500930421     C                   ADD       1             I
682600930421     C                   MOVEL     TBLKEY        C1Y(I)
682700930414     C                   MOVEL     TBLUNI        DS1Y
682800930421     C                   MOVEL     §1YFEI        I1Y(I)
682900930421     C                   MOVEL     §1YFEC        E1Y(I)
683000930414     C                   END                                                    <-- 3 ---
683100930414     C                   READ      TABEL00F                               99
683200930414     C                   END                                                    <-- 2 ---
683300930414     C                   END
683400930414     C*
683500930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
683600930414     C     D98ERR        IFEQ      '0'
683700930421     C                   Z-ADD     0             I
683800930414     C                   Z-ADD     CODUT         KTBUTE
683900930414     C                   MOVEL     '1X'          KTBCOD
684000930414     C                   MOVEL     *BLANKS       KTBKEY
684100930414     C     KEYTAB        SETLL     TABEL00F                           99
684200930414     C  N99              READ      TABEL00F                               99
684300930414     C     *IN99         IFEQ      '1'
684400930414     C                   MOVEL     '1'           D98ERR
684500930414     C                   MOVEL     MSG(1)        D98MSG
684600930414     C                   END
684700930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
684800930414     C     TBLCOD        ANDEQ     '1X'
684900930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
685000930421     C                   ADD       1             I
685100930421     C                   MOVEL     TBLKEY        C1X(I)
685200930414     C                   MOVEL     TBLUNI        DS1X
685300930421     C                   Z-ADD     §1XHHG        G1X(I)
685400930421     C                   Z-ADD     §1XHHO        O1X(I)
685500930421     C                   Z-ADD     §1XHHM        M1X(I)
685600930421     C                   Z-ADD     §1XHHP        P1X(I)
685700930421     C                   Z-ADD     §1XHHI        I1X(I)
685800930421     C                   Z-ADD     §1XHHS        S1X(I)
685900930414     C                   END                                                    <-- 3 ---
686000930414     C                   READ      TABEL00F                               99
686100931223     C                   ENDDO                                                  <-- 2 ---
686200050523     c
686300050523     c* controllo riempimento skiere
686400050523     c                   clear                   trul0sds
686500050523     c                   eval      i0sski='C1X/G1X/O1X/...'
686600050523     c                   eval      i0sele=%elem(c1x)
686700050523     c                   eval      i0spie=i
686800050523     c                   eval      i0sfile='TABEL00F  '
686900050523     c                   eval      i0scod=' 1X  '
687000050523     c                   eval      i0spgm='TNSD99R'
687100050523     c                   movel     trul0sds      kpjbu
687200050523     c                   call      'TRUL0SR'
687300050523     c                   parm                    kpjba
687400050523     C                   END
687500930421     C*
687600930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
687700930421     C     D98ERR        IFEQ      '0'
687800930421     C                   Z-ADD     0             I
687900930421     C                   Z-ADD     CODUT         KTBUTE
688000930421     C                   MOVEL     '2A'          KTBCOD
688100060406     C     Ktab          SETLL     TABEL00F                           99
688200060406     C  N99ktab          READe     TABEL00F                               99
688300930421     C     *IN99         IFEQ      '1'
688400930421     C                   MOVEL     '1'           D98ERR
688500930421     C                   MOVEL     MSG(8)        D98MSG
688600930421     C                   END
688700930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
688800930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
688900930421     C                   ADD       1             I
689000930421     C                   MOVEL     TBLKEY        C2A(I)
689100060406     C                   MOVEL     TBLUNI        ds2a
689200060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
689300060406     C                   MOVEL     w003a         D2A(I)
689400930421     C                   END                                                    <-- 3 ---
689500060406     C     ktab          READe     TABEL00F                               99
689600931223     C                   ENDDO                                                  <-- 2 ---
689700050523     c* controllo riempimento skiere
689800050523     c                   clear                   trul0sds
689900050523     c                   eval      i0sski='C2A/D2A'
690000050523     c                   eval      i0sele=%elem(c2a)
690100050523     c                   eval      i0spie=i
690200050523     c                   eval      i0sfile='TABEL00F  '
690300060406     c                   eval      i0scod='2A/2Z'
690400050523     c                   eval      i0spgm='TNSD99R'
690500050523     c                   movel     trul0sds      kpjbu
690600050523     c                   call      'TRUL0SR'
690700050523     c                   parm                    kpjba
690800930421     C                   END
690900060406     c* Tabella  2Z
691000060406     C     D98ERR        IFEQ      '0'
691100060406     C                   Z-ADD     CODUT         KTBUTE
691200060406     C                   MOVEL     '2Z'          KTBCOD
691300060406     C     Ktab          SETLL     TABEL00F                           99
691400060406     C  N99ktab          READe     TABEL00F                               99
691500060406     C     *IN99         IFEQ      '1'
691600060406     C                   MOVEL     '1'           D98ERR
691700060406     C                   MOVEL     MSG(33)       D98MSG
691800060406     C                   END
691900060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
692000060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
692100060406     C                   Z-ADD     1             I
692200060406     C                   MOVEL     TBLKEY        w003a
692300060406     c     w003a         lookup    c2a(i)                                 99
692400060406     C     *IN99         IFEQ      *off
692500060406     C                   MOVEL     '1'           D98ERR
692600060406     C                   MOVEL     MSG(34)       D98MSG
692700060406     C                   Else
692800060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
692900060406     C                   END                                                    <-- 3 ---
693000060406     C                   END                                                    <-- 3 ---
693100060406     C     ktab          READe     TABEL00F                               99
693200060406     C                   ENDDO                                                  <-- 2 ---
693300060406     C                   END
693400941227     C*
693500941227     C* CARICA TABELLA CODICI BOLLA -3A-
693600941227     C     D98ERR        IFEQ      '0'
693700941228     C                   Z-ADD     0             I
693800941227     C                   Z-ADD     CODUT         KTBUTE
693900941227     C                   MOVEL     '3A'          KTBCOD
694000941227     C                   MOVEL     *BLANKS       KTBKEY
694100941227     C     KEYTAB        SETLL     TABEL00F                           99
694200941227     C  N99              READ      TABEL00F                               99
694300941227     C     *IN99         IFEQ      '1'
694400941227     C                   MOVEL     '1'           D98ERR
694500941227     C                   MOVEL     MSG(22)       D98MSG
694600941227     C                   END
694700941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
694800941227     C     TBLCOD        ANDEQ     '3A'
694900941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
695000941227     C                   ADD       1             I
695100941227     C                   MOVEL     TBLKEY        C3A(I)
695200941227     C                   MOVEL     TBLUNI        DS3A
695300941227     C                   MOVEL     §3ATB1        T3A(I)
695400941228     C                   Z-ADD     §3AFCA        F3A(I)
695500990308     C                   MOVEL     §3ARBL        R3A(I)
695600941227     C                   END                                                    <-- 3 ---
695700941227     C                   READ      TABEL00F                               99
695800941227     C                   END                                                    <-- 2 ---
695900050523     c* controllo riempimento skiere
696000050523     c                   clear                   trul0sds
696100050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
696200050523     c                   eval      i0sele=%elem(c3a)
696300050523     c                   eval      i0spie=i
696400050523     c                   eval      i0sfile='TABEL00F  '
696500050523     c                   eval      i0scod=' 3A  '
696600050523     c                   eval      i0spgm='TNSD99R'
696700050523     c                   movel     trul0sds      kpjbu
696800050523     c                   call      'TRUL0SR'
696900050523     c                   parm                    kpjba
697000941227     C                   END
697100971210     C*
697200020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
697300020226     C                   Z-ADD     *ZEROS        J                 3 0
697400971210IF  0C     D98ERR        IFEQ      '0'
697500971210     C     *LOVAL        SETLL     AZORG01L
697600971210     C                   READ      AZORG01L                               98
697700971210DO  1C     *IN98         DOWEQ     *OFF
697800971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
697900971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
698000971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
698100020627     C                   MOVEL     ORGDE3        OG143
698200020702     C*
698300020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
698400020702     C                   CLEAR                   DNTW
698500020702     C*
698600020702     C                   MOVEL(P)  'NTW'         TBECOD
698700020702     C                   MOVEL(P)  §OGNTW        TBEKE1
698800020702     C     KTBE          CHAIN     TNTBE01L
698900020702     C                   IF        %found(TNTBE01L)
699000020702     C                   MOVEL     TBEUNI        DNTW
699100020702     C                   ENDIF
699200020702     C*
699300020702     C                   MOVEL     §NTWFIE       FL1(F)
699400020627     C*
699500020226     C* VERIFICO SE FILIALE FEDEX
699600020226     C                   IF        §OGNTW = 'FED'
699700020226     C                   ADD       1             J
699800020226     C                   Z-ADD     ORGFIL        FILFED(J)
699900020226     C                   ENDIF
700000020226     C*
700100971210E   2C                   ENDIF
700200971210     C                   READ      AZORG01L                               98
700300971210E   1C                   ENDDO
700400971210E   0C                   ENDIF
700500010821     C*
700600010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
700700010821     C                   MOVEL     'GED'         TBECOD
700800070514     C                   MOVEL     *BLANKS       TBEKE1
700900070514     C                   MOVEL     '1'           TBEKE1
701000010821     C                   MOVEL     *BLANKS       TBEKE2
701100010821     C     KTBE          CHAIN     TNTBE01L                           21
701200070514     C  N21              MOVEL     TBEUNI        DGED
701300150326     c
701400150326     C* REPERISCO LA tabella flag operativi per tabella OLP
701500150326     c                   clear                   dvpo
701600150326     C                   MOVEL     'VPO'         TBECOD
701700150326     C                   MOVEL     *BLANKS       TBEKE2
701800150326     C                   MOVEL(P)  'VPO'         TBEKE1
701900150326     C     KTBE          CHAIN     TNTBE01L                           21
702000150326     C  N21              MOVEL     TBEUNI        DVPO
702100150326     C*
702200031201     C*
702300031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
702400031201     C     D98ERR        IFEQ      '0'
702500060403     C                   Z-ADD     0             I7QW              3 0
702600031201     C                   Z-ADD     CODUT         KTBUTE
702700031201     C                   MOVEL     '7Q'          KTBCOD
702800031201     C                   MOVEL     *BLANKS       KTBKEY
702900031201     C     KEYTAB        SETLL     TABEL00F                           99
703000031201     C  N99              READ      TABEL00F                               99
703100031201     C     *IN99         IFEQ      '1'
703200031201     C                   MOVEL     '1'           D98ERR
703300031201     C                   MOVEL     MSG(8)        D98MSG
703400031201     C                   ENDIF
703500031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
703600031201     C     TBLCOD        ANDEQ     '7Q'
703700031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
703800031201     C                   MOVEL     TBLUNI        DS7Q
703900031201     C                   IF        §7Q4D1 = 'R' OR
704000031201     C                             §7Q4D2 = 'R' OR
704100031201     C                             §7Q4D3 = 'R' OR
704200031201     C                             §7Q4D4 = 'R' OR
704300031201     C                             §7Q4D5 = 'R'
704400031201     C                   ADD       1             I7QW
704500031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
704600031201     C                   ENDIF                                                  <-- 3 ---
704700031201     C                   ENDIF                                                  <-- 3 ---
704800031201     C                   READ      TABEL00F                               99
704900031201     C                   ENDDO                                                  <-- 2 ---
705000031201     C                   ENDIF
705100031201     C*
705200031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
705300031201     C     D98ERR        IFEQ      '0'
705400031201     C                   Z-ADD     CODUT         KTBUTE
705500031201     C                   MOVEL     '7W'          KTBCOD
705600031201     C                   MOVEL     *BLANKS       KTBKEY
705700031201     C     KEYTAB        SETLL     TABEL00F                           99
705800031201     C  N99              READ      TABEL00F                               99
705900031201     C     *IN99         IFEQ      '1'
706000031201     C                   MOVEL     '1'           D98ERR
706100031201     C                   MOVEL     MSG(8)        D98MSG
706200031201     C                   ENDIF
706300031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
706400031201     C     TBLCOD        ANDEQ     '7W'
706500031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
706600031201     C                   MOVEL     TBLUNI        DS7W
706700031201     C                   IF        §7W6D1 = 'C' OR
706800031201     C                             §7W6D2 = 'C' OR
706900031201     C                             §7W6D3 = 'C' OR
707000031201     C                             §7W6D4 = 'C' OR
707100031201     C                             §7W6D5 = 'C'
707200031201     C                   ADD       1             I7QW
707300031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
707400031201     C                   ENDIF                                                  <-- 3 ---
707500031201     C                   ENDIF                                                  <-- 3 ---
707600031201     C                   READ      TABEL00F                               99
707700031201     C                   ENDDO                                                  <-- 2 ---
707800050523     c* controllo riempimento skiere
707900050523     c                   clear                   trul0sds
708000050523     c                   eval      i0sski='SK7QW'
708100050523     c                   eval      i0sele=%elem(sk7qw)
708200050523     c                   eval      i0spie=i7qw
708300050523     c                   eval      i0sfile='TABEL00F  '
708400050523     c                   eval      i0scod='7Q-7W'
708500050523     c                   eval      i0spgm='TNSD99R'
708600050523     c                   movel     trul0sds      kpjbu
708700050523     c                   call      'TRUL0SR'
708800050523     c                   parm                    kpjba
708900031201     C                   ENDIF
709000930414     C*
709100930414     C                   ENDSR
709200930414     C********************************************************************
709300930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
709400930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
709500930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
709600930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
709700930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
709800930414     C********************************************************************
709900930414     C     RO3ORE        BEGSR
710000930414     C*
710100930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
710200930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
710300930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
710400050902     c                   movel     ' '           WR3MATPOM         1
710500931209     C                   Z-ADD     *ZEROS        WR3HHO
710600931209     C                   Z-ADD     *ZEROS        WR3HHA
710700930414     C                   MOVE      WR3TAB        WR3COD
710800930604     C                   MOVEL     D98LNA        WR3COD            8
710900930421     C                   Z-ADD     1             I
711000930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
711100930414     C     *IN99         IFEQ      '0'
711200930414     C                   MOVE      WR3TAB        WR3COD
711300930414     C                   MOVEL     '   '         WR3COD            8
711400930421     C                   Z-ADD     1             I
711500930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
711600930414     C                   END
711700930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
711800930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
711900930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
712000930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
712100930414     C                   ELSE                                                   <-- 2 -->
712200930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
712300930421     C                   Z-ADD     M1X(I)        WR3HHO
712400930510     C                   Z-ADD     I1X(I)        WR3HHA
712500050902     c                   movel     'M'           WR3MATPOM         1
712600930414     C                   END
712700930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
712800930421     C                   Z-ADD     P1X(I)        WR3HHO
712900930510     C                   Z-ADD     S1X(I)        WR3HHA
713000050902     c                   movel     'P'           WR3MATPOM
713100930414     C                   END
713200930414     C                   END                                                    <-- 2 ---
713300930414     C                   END                                                    <-- 1 ---
713400930414     C*
713500930414     C                   ENDSR
713600930423     C********************************************************************
713700930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
713800930423     C********************************************************************
713900930423     C     W98D98        BEGSR
714000930423     C*
714100941228     C*
714200941228     C* ORE REALI PARTENZA INTERNE
714300941228     C                   Z-ADD     W98RPI        W98               3 0
714400941228     C                   EXSR      W98999
714500941228     C                   Z-ADD     D98           D98RPI
714600941228     C* ORE REALI PARTENZA CLIENTE
714700941228     C                   Z-ADD     W98RPC        W98
714800941228     C                   EXSR      W98999
714900941228     C                   Z-ADD     D98           D98RPC
715000930423     C* ORE REALI TRAZIONE INTERNE
715100941228     C                   Z-ADD     W98RTI        W98
715200941228     C                   EXSR      W98999
715300941228     C                   Z-ADD     D98           D98RTI
715400930423     C* ORE REALI TRAZIONE CLIENTE
715500941228     C                   Z-ADD     W98RTC        W98
715600941228     C                   EXSR      W98999
715700941228     C                   Z-ADD     D98           D98RTC
715800930423     C* ORE REALI CONSEGNA INTERNE
715900941228     C                   Z-ADD     W98RCI        W98
716000941228     C                   EXSR      W98999
716100941228     C                   Z-ADD     D98           D98RCI
716200930423     C* ORE REALI CONSEGNA CLIENTE
716300941228     C                   Z-ADD     W98RCC        W98
716400941228     C                   EXSR      W98999
716500941228     C                   Z-ADD     D98           D98RCC
716600930423     C* ORE REALI RESA INTERNE
716700941228     C                   Z-ADD     W98RRI        W98
716800941228     C                   EXSR      W98999
716900941228     C                   Z-ADD     D98           D98RRI
717000930423     C* ORE REALI RESA CLIENTE
717100941228     C                   Z-ADD     W98RRC        W98
717200941228     C                   EXSR      W98999
717300941228     C                   Z-ADD     D98           D98RRC
717400941228     C* ORE DIFF. PARTENZA INTERNE
717500941228     C                   Z-ADD     W98FPI        W98
717600941228     C                   EXSR      W98999
717700941228     C                   Z-ADD     D98           D98FPI
717800941228     C* ORE DIFF. PARTENZA CLIENTE
717900941228     C                   Z-ADD     W98FPC        W98
718000941228     C                   EXSR      W98999
718100941228     C                   Z-ADD     D98           D98FPC
718200930423     C* ORE DIFF. TRAZIONE INTERNE
718300941228     C                   Z-ADD     W98FTI        W98
718400941228     C                   EXSR      W98999
718500941228     C                   Z-ADD     D98           D98FTI
718600930423     C* ORE DIFF. TRAZIONE CLIENTE
718700941228     C                   Z-ADD     W98FTC        W98
718800941228     C                   EXSR      W98999
718900941228     C                   Z-ADD     D98           D98FTC
719000930423     C* ORE DIFF. CONSEGNA INTERNE
719100941228     C                   Z-ADD     W98FCI        W98
719200941228     C                   EXSR      W98999
719300941228     C                   Z-ADD     D98           D98FCI
719400930423     C* ORE DIFF. CONSEGNA CLIENTE
719500941228     C                   Z-ADD     W98FCC        W98
719600941228     C                   EXSR      W98999
719700941228     C                   Z-ADD     D98           D98FCC
719800930423     C* ORE DIFF. RESA INTERNE
719900941228     C                   Z-ADD     W98FRI        W98
720000941228     C                   EXSR      W98999
720100941228     C                   Z-ADD     D98           D98FRI
720200930423     C* ORE DIFF. RESA CLIENTE
720300941228     C                   Z-ADD     W98FRC        W98
720400941228     C                   EXSR      W98999
720500941228     C                   Z-ADD     D98           D98FRC
720600941228     C* ORE NON CONF. PARTENZA INTERNE
720700941228     C     W98NPI        IFGT      999
720800941228     C                   Z-ADD     999           D98NPI
720900941228     C                   ELSE
721000941228     C                   Z-ADD     W98NPI        D98NPI
721100941228     C                   END
721200941228     C* ORE NON CONF. PARTENZA CLIENTE
721300941228     C     W98NPC        IFGT      999
721400941228     C                   Z-ADD     999           D98NPC
721500941228     C                   ELSE
721600941228     C                   Z-ADD     W98NPC        D98NPC
721700941228     C                   END
721800930423     C* ORE NON CONF. TRAZIONE INTERNE
721900930423     C     W98NTI        IFGT      999
722000930423     C                   Z-ADD     999           D98NTI
722100930423     C                   ELSE
722200930423     C                   Z-ADD     W98NTI        D98NTI
722300930423     C                   END
722400930423     C* ORE NON CONF. TRAZIONE CLIENTE
722500930423     C     W98NTC        IFGT      999
722600930423     C                   Z-ADD     999           D98NTC
722700930423     C                   ELSE
722800930423     C                   Z-ADD     W98NTC        D98NTC
722900930423     C                   END
723000930423     C* ORE NON CONF. CONSEGNA INTERNE
723100930423     C     W98NCI        IFGT      999
723200930423     C                   Z-ADD     999           D98NCI
723300930423     C                   ELSE
723400930423     C                   Z-ADD     W98NCI        D98NCI
723500930423     C                   END
723600930423     C* ORE NON CONF. CONSEGNA CLIENTE
723700930423     C     W98NCC        IFGT      999
723800930423     C                   Z-ADD     999           D98NCC
723900930423     C                   ELSE
724000930423     C                   Z-ADD     W98NCC        D98NCC
724100930423     C                   END
724200930423     C* ORE NON CONF. RESA INTERNE
724300930423     C     W98NRI        IFGT      999
724400930423     C                   Z-ADD     999           D98NRI
724500930423     C                   ELSE
724600930423     C                   Z-ADD     W98NRI        D98NRI
724700930423     C                   END
724800930423     C* ORE NON CONF. RESA CLIENTE
724900930423     C     W98NRC        IFGT      999
725000930423     C                   Z-ADD     999           D98NRC
725100930423     C                   ELSE
725200930423     C                   Z-ADD     W98NRC        D98NRC
725300930423     C                   END
725400930423     C* ORE X TURNO CHIUSURA INTERNE
725500941228     C                   Z-ADD     W98HHI        W98
725600941228     C                   EXSR      W98999
725700941228     C                   Z-ADD     D98           D98HHI
725800930423     C* ORE X TURNO CHIUSURA CLIENTE
725900941228     C                   Z-ADD     W98HHC        W98
726000941228     C                   EXSR      W98999
726100941228     C                   Z-ADD     D98           D98HHC
726200060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
726300060418     C                   Z-ADD     W98EGI        W98
726400060418     C                   EXSR      W98999
726500060418     C                   Z-ADD     D98           D98hEGI
726600060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
726700060418     C                   Z-ADD     W98EGC        W98
726800060418     C                   EXSR      W98999
726900060418     C                   Z-ADD     D98           D98hEGC
727000060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
727100060420     C                   Z-ADD     W98EAI        W98
727200060420     C                   EXSR      W98999
727300060420     C                   Z-ADD     D98           D98hEAI
727400060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
727500060420     C                   Z-ADD     W98EAC        W98
727600060420     C                   EXSR      W98999
727700060420     C                   Z-ADD     D98           D98hEAC
727800930423     C* ORE X LASC. AVVISO INTERNE
727900941228     C                   Z-ADD     W98HLI        W98
728000941228     C                   EXSR      W98999
728100941228     C                   Z-ADD     D98           D98HLI
728200930423     C* ORE X LASC. AVVISO CLIENTE
728300941228     C                   Z-ADD     W98HLC        W98
728400941228     C                   EXSR      W98999
728500941228     C                   Z-ADD     D98           D98HLC
728600930423     C* ORE X GIACENZA INTERNE
728700941228     C                   Z-ADD     W98HGI        W98
728800941228     C                   EXSR      W98999
728900941228     C                   Z-ADD     D98           D98HGI
729000930423     C* ORE X GIACENZA CLIENTE
729100941228     C                   Z-ADD     W98HGC        W98
729200941228     C                   EXSR      W98999
729300941228     C                   Z-ADD     D98           D98HGC
729400941228     C* ORE X FEST. PART. INTERNE
729500941228     C                   Z-ADD     W98HPI        W98
729600941228     C                   EXSR      W98999
729700941228     C                   Z-ADD     D98           D98HPI
729800941228     C* ORE X FEST. PART. CLIENTE
729900941228     C                   Z-ADD     W98HPC        W98
730000941228     C                   EXSR      W98999
730100941228     C                   Z-ADD     D98           D98HPC
730200930423     C* ORE X FEST. TRAZ. INTERNE
730300941228     C                   Z-ADD     W98HFI        W98
730400941228     C                   EXSR      W98999
730500941228     C                   Z-ADD     D98           D98HFI
730600930423     C* ORE X FEST. TRAZ. CLIENTE
730700941228     C                   Z-ADD     W98HFC        W98
730800941228     C                   EXSR      W98999
730900941228     C                   Z-ADD     D98           D98HFC
731000930423     C* ORE X FEST. CONS. INTERNE
731100941228     C                   Z-ADD     W98HEI        W98
731200941228     C                   EXSR      W98999
731300941228     C                   Z-ADD     D98           D98HEI
731400930423     C* ORE X FEST. CONS. CLIENTE
731500941228     C                   Z-ADD     W98HEC        W98
731600941228     C                   EXSR      W98999
731700941228     C                   Z-ADD     D98           D98HEC
731800930511     C* ORE X F.DEPOSITO INTERNE
731900941228     C                   Z-ADD     W98HDI        W98
732000941228     C                   EXSR      W98999
732100941228     C                   Z-ADD     D98           D98HDI
732200930511     C* ORE X F.DEPOSITO CLIENTE
732300941228     C                   Z-ADD     W98HDC        W98
732400941228     C                   EXSR      W98999
732500941228     C                   Z-ADD     D98           D98HDC
732600930511     C* ORE X CONTRASSEGNO INTERNE
732700941228     C                   Z-ADD     W98HOI        W98
732800941228     C                   EXSR      W98999
732900941228     C                   Z-ADD     D98           D98HOI
733000930511     C* ORE X CONTRASSEGNO CLIENTE
733100941228     C                   Z-ADD     W98HOC        W98
733200941228     C                   EXSR      W98999
733300941228     C                   Z-ADD     D98           D98HOC
733400930914     C* ORE DI CONSEGNABILITA' INTERNE
733500941228     C                   Z-ADD     W98OCI        W98
733600941228     C                   EXSR      W98000
733700941228     C                   Z-ADD     D98           D98OCI
733800930914     C* ORE DI CONSEGNABILITA' CLIENTE
733900941228     C                   Z-ADD     W98OCC        W98
734000941228     C                   EXSR      W98000
734100941228     C                   Z-ADD     D98           D98OCC
734200071122     C
734300071122     c* Per i codici bolla M prepagati annullati, azzero tutto
734400071122     c                   if        d98cbo='M '
734500071122     c                   clear                   d98rpi
734600071122     c                   clear                   d98rpc
734700071122     c                   clear                   d98rti
734800071122     c                   clear                   d98rtc
734900071122     c                   clear                   d98rci
735000071122     c                   clear                   d98rcc
735100071122     c                   clear                   d98rri
735200071122     c                   clear                   d98rrc
735300071122     c                   clear                   d98fpi
735400071122     c                   clear                   d98fpc
735500071122     c                   clear                   d98fti
735600071122     c                   clear                   d98ftc
735700071122     c                   clear                   d98fci
735800071122     c                   clear                   d98fcc
735900071122     c                   clear                   d98fri
736000071122     c                   clear                   d98frc
736100071122     c                   clear                   d98npi
736200071122     c                   clear                   d98npc
736300071122     c                   clear                   d98nti
736400071122     c                   clear                   d98ntc
736500071122     c                   clear                   d98nci
736600071122     c                   clear                   d98ncc
736700071122     c                   clear                   d98nri
736800071122     c                   clear                   d98nrc
736900071122     c                   clear                   d98OCi
737000071122     c                   clear                   d98OCC
737100071122     c                   endif
737200930423     C*
737300100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
737400100308     c*  calcolo
737500100308     c                   eval      d98dcr=s98dcr
737600100308     c                   eval      d98hcr=s98hcr
737700100308     c                   eval      d98tcr=s98tcr
737800100308     c
737900930423     C                   ENDSR
738000941228     C***********************************************************************
738100941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
738200941228     C***********************************************************************
738300941228     C     W98999        BEGSR
738400941228     C     W98           IFLT      -999
738500941228     C                   Z-ADD     -999          D98               3 0
738600941228     C                   ELSE
738700941228     C     W98           IFGT      999
738800941228     C                   Z-ADD     999           D98
738900941228     C                   ELSE
739000941228     C                   Z-ADD     W98           D98
739100941228     C                   END
739200941228     C                   END
739300941228     C                   ENDSR
739400941228     C***********************************************************************
739500941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
739600941228     C***********************************************************************
739700941228     C     W98000        BEGSR
739800941228     C     W98           IFLE      *ZEROS
739900941228     C                   Z-ADD     *ZEROS        D98
740000941228     C                   ELSE
740100941228     C     W98           IFGT      999
740200941228     C                   Z-ADD     999           D98
740300941228     C                   ELSE
740400941228     C                   Z-ADD     W98           D98
740500941228     C                   END
740600941228     C                   END
740700941228     C                   ENDSR
740800930305     C***********************************************************************
740900930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
741000930305     C***********************************************************************
741100930305     C     AZZERA        BEGSR
741200150219     c* data e ora del momento
741300150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
741400150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
741500150219     C                   Z-ADD     DTORA4        WHHODI            4 0
741600930305     C*
741700141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
741800150603     C                   MOVEL     'N'           opendan
741900141107     c                   clear                   or_ddc
742000141108     c                   clear                   sav_12dat         8 0
742100141107     c                   clear                   or_datarif
742200140402     c                   clear                   wpick2            1
742300140402     c                   clear                   Primotipo
742400090928     c                   clear                   Primodev
742500090928     c                   clear                   Primohev
742600070827     c                   clear                   CallAR5
742700071122     c                   clear                   d98cbo
742800100308     c                   clear                   s98dcr
742900100308     c                   clear                   s98hcr
743000100308     c                   clear                   s98tcr
743100141106     c                   clear                   nosomma           1
743200141229     c                   clear                   wtrasm
743300150324     c                   clear                   avvcmc
743400150325     c                   clear                   DSbl4L
743500150401     c                   clear                   fnlv82ds
743600150331     c                   clear                   DS7R
743700150605     c                   clear                   wDEE              1
743800150629     c                   clear                   wtsogliaE
743900930323     C                   MOVEL     '0'           D98ERR
744000930323     C                   MOVEL     *BLANKS       D98MSG
744100931124     C                   Z-ADD     *ZEROS        D98TPI
744200931124     C                   Z-ADD     *ZEROS        D98TPC
744300930325     C                   Z-ADD     *ZEROS        D98TTI
744400930325     C                   Z-ADD     *ZEROS        D98TTC
744500930325     C                   Z-ADD     *ZEROS        D98TCI
744600930325     C                   Z-ADD     *ZEROS        D98TCC
744700090827     C                   Z-ADD     *ZEROS        w99TCI
744800090827     C                   Z-ADD     *ZEROS        w99TCC
744900930325     C                   Z-ADD     *ZEROS        D98TRI
745000930325     C                   Z-ADD     *ZEROS        D98TRC
745100090921     C                   Z-ADD     *ZEROS        w99TRI
745200090921     C                   Z-ADD     *ZEROS        w99TRC
745300931124     C                   Z-ADD     *ZEROS        D98RPI
745400931124     C                   Z-ADD     *ZEROS        D98RPC
745500930405     C                   Z-ADD     *ZEROS        D98RTI
745600930405     C                   Z-ADD     *ZEROS        D98RTC
745700930405     C                   Z-ADD     *ZEROS        D98RCI
745800930405     C                   Z-ADD     *ZEROS        D98RCC
745900930405     C                   Z-ADD     *ZEROS        D98RRI
746000930405     C                   Z-ADD     *ZEROS        D98RRC
746100931124     C                   Z-ADD     *ZEROS        D98FPI
746200931124     C                   Z-ADD     *ZEROS        D98FPC
746300930325     C                   Z-ADD     *ZEROS        D98FTI
746400930325     C                   Z-ADD     *ZEROS        D98FTC
746500930325     C                   Z-ADD     *ZEROS        D98FCI
746600930325     C                   Z-ADD     *ZEROS        D98FCC
746700930325     C                   Z-ADD     *ZEROS        D98FRI
746800930325     C                   Z-ADD     *ZEROS        D98FRC
746900931124     C                   MOVEL     *BLANKS       D98CPI
747000931124     C                   MOVEL     *BLANKS       D98CPC
747100930325     C                   MOVEL     *BLANKS       D98CTI
747200930325     C                   MOVEL     *BLANKS       D98CTC
747300930325     C                   MOVEL     *BLANKS       D98CCI
747400930325     C                   MOVEL     *BLANKS       D98CCC
747500930325     C                   MOVEL     *BLANKS       D98CRI
747600930325     C                   MOVEL     *BLANKS       D98CRC
747700931124     C                   Z-ADD     *ZEROS        D98NPI
747800931124     C                   Z-ADD     *ZEROS        D98NPC
747900930325     C                   Z-ADD     *ZEROS        D98NTI
748000930325     C                   Z-ADD     *ZEROS        D98NTC
748100930325     C                   Z-ADD     *ZEROS        D98NCI
748200930325     C                   Z-ADD     *ZEROS        D98NCC
748300930325     C                   Z-ADD     *ZEROS        D98NRI
748400930325     C                   Z-ADD     *ZEROS        D98NRC
748500930414     C                   Z-ADD     *ZEROS        D98HHI
748600930414     C                   Z-ADD     *ZEROS        D98HHC
748700930414     C                   Z-ADD     *ZEROS        D98HOI
748800930414     C                   Z-ADD     *ZEROS        D98HOC
748900930414     C                   Z-ADD     *ZEROS        D98HLI
749000930414     C                   Z-ADD     *ZEROS        D98HLC
749100930414     C                   Z-ADD     *ZEROS        D98HGI
749200930414     C                   Z-ADD     *ZEROS        D98HGC
749300931124     C                   Z-ADD     *ZEROS        D98HPI
749400931124     C                   Z-ADD     *ZEROS        D98HPC
749500930414     C                   Z-ADD     *ZEROS        D98HFI
749600930414     C                   Z-ADD     *ZEROS        D98HFC
749700930416     C                   Z-ADD     *ZEROS        D98HEI
749800930416     C                   Z-ADD     *ZEROS        D98HEC
749900930414     C                   Z-ADD     *ZEROS        D98HRI
750000930414     C                   Z-ADD     *ZEROS        D98HRC
750100930511     C                   Z-ADD     *ZEROS        D98HDI
750200930511     C                   Z-ADD     *ZEROS        D98HDC
750300930325     C                   Z-ADD     *ZEROS        D98DCI
750400930325     C                   Z-ADD     *ZEROS        D98HCI
750500930325     C                   Z-ADD     *ZEROS        D98DCE
750600930325     C                   Z-ADD     *ZEROS        D98HCE
750700931125     C                   Z-ADD     *ZEROS        D98OCI
750800931125     C                   Z-ADD     *ZEROS        D98OCC
750900931202     C                   Z-ADD     *ZEROS        D98DEI
751000931202     C                   Z-ADD     *ZEROS        D98OEI
751100931202     C                   Z-ADD     *ZEROS        D98DEE
751200931202     C                   Z-ADD     *ZEROS        D98OEE
751300150325     C                   clear                   D98dtctd
751400150326     C                   clear                   D98picklim
751500150325     C                   clear                   D98spcdee
751600150331     C                   clear                   fddata
751700140825     c
751800141017     c                   clear                   d98ddc
751900141017     c                   clear                   d98cons
752000141017     c                   clear                   wd98ndc
752100140910     c                   clear                   wd98fbc
752200150119     c                   clear                   wd98acc
752300140825     c                   clear                   wtrulor
752400150219     c
752500150219     c                   clear                   IMPdata
752600150219     c                   clear                   IMPfgs
752700150219     c                   clear                   DISdata
752800150219     c                   clear                   DISfgs
752900150219     c                   clear                   DISora
753000150219     c                   clear                   DISnpg
753100150219     c                   clear                   IDnpg
753200150219     c                   clear                   IDdata
753300150219     c                   clear                   IDora
753400150219     c                   clear                   IDfgs
753500931125     C* CAMPI VARI
753600931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
753700140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
753800140825      *   DALLA LETTURA DI QUESTI FILE                               *
753900931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
754000931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
754100931125     C     D98NSP        IFNE      *ZEROS
754200930525     C                   MOVEL     *BLANKS       D98TSP
754300941228     C                   MOVEL     *BLANKS       D98CAD
754400970728     C                   MOVEL     *BLANKS       D98LOD
754500930525     C                   Z-ADD     *ZEROS        D98TFP
754600930525     C                   Z-ADD     *ZEROS        D98TFA
754700930525     C                   Z-ADD     *ZEROS        D98LNA
754800930525     C                   Z-ADD     *ZEROS        D98DSP
754900941228     C                   Z-ADD     *ZEROS        D98MGS
755000930525     C                   Z-ADD     *ZEROS        D98DTI
755100930525     C                   Z-ADD     *ZEROS        D98HTI
755200930525     C                   Z-ADD     *ZEROS        D98DCM
755300930525     C                   Z-ADD     *ZEROS        D98HMC
755400930525     C                   MOVEL     *BLANKS       D98GC1
755500930525     C                   MOVEL     *BLANKS       D98GC2
755600010820     C                   Z-ADD     *ZEROS        D98CAS
755700010821     C                   MOVEL     *BLANKS       D98VCA
755800930525     C                   MOVEL     *BLANKS       D98TIC
755900930525     C                   Z-ADD     *ZEROS        D98DCR
756000930525     C                   Z-ADD     *ZEROS        D98HCR
756100930525     C                   MOVEL     *BLANKS       D98TCR
756200050914     C                   MOVEL     *BLANKS       Consideraora      1
756300930525     C                   MOVEL     *BLANKS       D98CMC
756400930525     C                   Z-ADD     *ZEROS        D98DAG
756500930525     C                   Z-ADD     *ZEROS        D98DCG
756600930525     C                   MOVEL     *BLANKS       D98FFD
756700930525     C                   Z-ADD     *ZEROS        D98NCD
756800930525     C                   MOVEL     *BLANKS       D98FNS
756900931124     C                   Z-ADD     *ZEROS        D98DAM
757000940628     C                   MOVEL     *BLANKS       D98FIN
757100940628     C                   Z-ADD     *ZEROS        D98KSC
757200940628     C                   MOVEL     *BLANKS       D98TBL
757300931125     C                   END
757400960229     C     D98TLA        IFEQ      'T'
757500960229     C                   Z-ADD     *ZEROS        D98DTI
757600960229     C                   Z-ADD     *ZEROS        D98HTI
757700960229     C                   ENDIF
757800931125     C* CAMPI WORK
757900930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
758000930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
758100150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
758200930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
758300941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
758400960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
758500930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
758600930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
758700940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
758800930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
758900150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
759000931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
759100931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
759200150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
759300930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
759400930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
759500930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
759600930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
759700930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
759800940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
759900941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
760000930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
760100050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
760200050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
760300140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
760400930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
760500930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
760600930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
760700930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
760800930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
760900930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
761000930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
761100930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
761200930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
761300930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
761400930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
761500930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
761600930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
761700930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
761800140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
761900931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
762000060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
762100960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
762200931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
762300140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
762400140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
762500150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
762600140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
762700941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
762800970304     C******************** Z-ADD*ZEROS    W98DBR  80
762900970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
763000150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
763100050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
763200930414     C*
763300941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
763400941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
763500930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
763600930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
763700930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
763800930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
763900930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
764000930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
764100941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
764200941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
764300930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
764400930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
764500930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
764600930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
764700930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
764800930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
764900941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
765000941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
765100930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
765200930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
765300930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
765400930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
765500930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
765600930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
765700930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
765800930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
765900060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
766000060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
766100060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
766200060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
766300930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
766400930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
766500930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
766600930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
766700930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
766800930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
766900930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
767000930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
767100930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
767200930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
767300930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
767400930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
767500930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
767600930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
767700941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
767800941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
767900930423     C*
768000930511     C                   Z-ADD     *ZEROS        W98HF             5 0
768100930423     C                   Z-ADD     *ZEROS        W98HH             5 0
768200060418     C                   Z-ADD     *ZEROS        W98EG             5 0
768300060420     C                   Z-ADD     *ZEROS        W98EA             5 0
768400930423     C                   Z-ADD     *ZEROS        W98HG             5 0
768500930423     C                   Z-ADD     *ZEROS        W98HL             5 0
768600930423     C                   Z-ADD     *ZEROS        W98HO             5 0
768700930511     C                   Z-ADD     *ZEROS        W98HD             5 0
768800930611     C*
768900930611     C                   Z-ADD     *ZEROS        WORELA            3 0
769000960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
769100930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
769200930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
769300931207     C                   Z-ADD     *ZEROS        WORECR            3 0
769400090902     c
769500090902     c                   clear                   wmancaTRAZ
769600150113     c                   clear                   wcaldpcee         1
769700150220     c                   clear                   evemic            2
769800930305     C*
769900060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
770000060406     C                   clear                   LAF                            *LASC.AVVISO FINE
770100091014     C                   clear                   LAp                            *posizione l.avviso
770200060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
770300060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
770400060406c    c                   clear                   LCon
770500060406     C                   clear                   EVD                            *DATA EVENTO
770600060406     C                   clear                   EVO                            *ORA EVENTO
770700060406     C                   clear                   EVC                            *CAUSALE EVENTO
770800131218     C                   clear                   EVT
770900060406     C                   clear                   pVD                            *DATA EVENTO
771000060406     C                   clear                   pVO                            *ORA EVENTO
771100060406     C                   clear                   pVC                            *CAUSALE EVENTO
771200060406     C                   clear                   pVF                            *CAUSALE EVENTO
771300060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
771400060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
771500060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
771600060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
771700060406     C                   clear                   COV                            *DATE FINE L.A. E GI
771800060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
771900060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
772000091014     C                   clear                   GIp                            *posizione vento gia
772100060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
772200060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
772300060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
772400931213     C*
772500060406     C                   clear                   TUC                            *TURNI CHIUSURA
772600931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
772700931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
772800060406     c
772900060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
773000060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
773100060406     c                   clear                   pgii
773200060406     c                   clear                   pgif
773300060406     c                   clear                   pgci
773400060406     c                   clear                   pgcf
773500060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
773600060420     c                   clear                   paii
773700060420     c                   clear                   paif
773800930420     C*
773900930305     C                   ENDSR
774000930512     C***********************************************************************
774100930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
774200930512     C***********************************************************************
774300930512     C     FORCON        BEGSR
774400930512     C*
774500931124     C                   Z-ADD     *ZEROS        D98RPI
774600931124     C                   Z-ADD     *ZEROS        D98RPC
774700930512     C                   Z-ADD     *ZEROS        D98RTI
774800930512     C                   Z-ADD     *ZEROS        D98RTC
774900930512     C                   Z-ADD     *ZEROS        D98RCI
775000930512     C                   Z-ADD     *ZEROS        D98RCC
775100930512     C                   Z-ADD     *ZEROS        D98RRI
775200930512     C                   Z-ADD     *ZEROS        D98RRC
775300931124     C                   Z-ADD     *ZEROS        D98FPI
775400931124     C                   Z-ADD     *ZEROS        D98FPC
775500930512     C                   Z-ADD     *ZEROS        D98FTI
775600930512     C                   Z-ADD     *ZEROS        D98FTC
775700930512     C                   Z-ADD     *ZEROS        D98FCI
775800930512     C                   Z-ADD     *ZEROS        D98FCC
775900930512     C                   Z-ADD     *ZEROS        D98FRI
776000930512     C                   Z-ADD     *ZEROS        D98FRC
776100931124     C                   MOVEL     'S'           D98CPI
776200931124     C                   MOVEL     'S'           D98CPC
776300930512     C                   MOVEL     'S'           D98CTI
776400930512     C                   MOVEL     'S'           D98CTC
776500930512     C                   MOVEL     'S'           D98CCI
776600930512     C                   MOVEL     'S'           D98CCC
776700930512     C                   MOVEL     'S'           D98CRI
776800930512     C                   MOVEL     'S'           D98CRC
776900931124     C                   Z-ADD     *ZEROS        D98NPI
777000931124     C                   Z-ADD     *ZEROS        D98NPC
777100930512     C                   Z-ADD     *ZEROS        D98NTI
777200930512     C                   Z-ADD     *ZEROS        D98NTC
777300930512     C                   Z-ADD     *ZEROS        D98NCI
777400930512     C                   Z-ADD     *ZEROS        D98NCC
777500930512     C                   Z-ADD     *ZEROS        D98NRI
777600930512     C                   Z-ADD     *ZEROS        D98NRC
777700930512     C                   Z-ADD     *ZEROS        D98HHI
777800930512     C                   Z-ADD     *ZEROS        D98HHC
777900930512     C                   Z-ADD     *ZEROS        D98HOI
778000930512     C                   Z-ADD     *ZEROS        D98HOC
778100930512     C                   Z-ADD     *ZEROS        D98HLI
778200930512     C                   Z-ADD     *ZEROS        D98HLC
778300930512     C                   Z-ADD     *ZEROS        D98HGI
778400930512     C                   Z-ADD     *ZEROS        D98HGC
778500931124     C                   Z-ADD     *ZEROS        D98HPI
778600931124     C                   Z-ADD     *ZEROS        D98HPC
778700930512     C                   Z-ADD     *ZEROS        D98HFI
778800930512     C                   Z-ADD     *ZEROS        D98HFC
778900930512     C                   Z-ADD     *ZEROS        D98HEI
779000930512     C                   Z-ADD     *ZEROS        D98HEC
779100930512     C                   Z-ADD     *ZEROS        D98HRI
779200930512     C                   Z-ADD     *ZEROS        D98HRC
779300930512     C                   Z-ADD     *ZEROS        D98HDI
779400930512     C                   Z-ADD     *ZEROS        D98HDC
779500930520     C*
779600930512     C                   ENDSR
779700930305     C********************************************************************
779800971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
779900950406031059090120151181212243273304334365
780000950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
780100950406312831303130313130313031
780200930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
780300971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
780400971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
780500971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
780600971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
780700971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
780800971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
780900971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
781000971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
781100971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
781200971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
781300971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
781400971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
781500971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
781600971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
781700971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
781800971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
781900971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
782000971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
782100971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
782200971127TNSD99R- Bolla inesistente.                                               BOIN20
782300971127TNSD99R-     Cap inesistente.                                             CAPI21
782400971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
782500971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
782600971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
782700971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
782800971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
782900971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
783000971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
783100971127TNSD99R- Contrassegno non trovato.                                        CONT29
783200971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
783300990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
783400990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
783500060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
783600060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
783700971127TNSD99R- 35                                                                   35
783800971127TNSD99R- 36                                                                   36
783900971127TNSD99R- 37                                                                   37
784000971127TNSD99R- 38                                                                   38
784100971127TNSD99R- 39                                                                   39
784200971127TNSD99R- 40                                                                   40
