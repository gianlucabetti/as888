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
321900151007     c                   clear                   wtempi12C         1
322000150722     c                   clear                   woreOLP
322100151009     c                   clear                   wdiff
322200150219     c
322300150219     c* 1) Se presente l'arrivo della trazione lo uso
322400150219    1c                   select
322500150219     c                   when      wmancatraz=' '  and d98dti>0
322600150630     c                   Z-ADD     WPODTI        WR1POI
322700150219     c
322800150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
322900150219    1c                   when      IDdata>0
323000150325     c                   exsr      CalcoloIMP
323100151008     c* Passo in output il terminal di disguido
323200151009    2c                   if        IDfgs<>D98lnp and idfgs<>D98TFP
323300151008     c                   eval      d98flp=%editc(LTP(2):'X')
323400151008     c                   endif
323500151008
323600150528     c* stato IMP/disguido solo se >  alla data di oggi
323700151006     c                   if        wr1poi > wpoattdata00 and
323800151006     c                             (wr1poi-wpoattdata00)>12
323900150427     c                   eval      d98spcdee='IMP'
324000151008     c                   endif
324100151006     c                   if        wr1poi > wpoattdata00 and
324200151006     c                             (wr1poi-wpoattdata00)=12
324300151006     c                             and w991In>12
324400151006     c                   eval      d98spcdee='IMP'
324500151008     c                   endif
324600150219     c
324700150219     c                   other
324800150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
324900150225     c* devo far finta che sia arrivato.
325000150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
325100150508     c*  che tiene conto della data partenza PRIMO  collo
325200150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
325300150508     c*
325400150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
325500150508     c                   if        wr1poi >=wpoattdata00
325600150508     c                              and WPOPTC_DEE > WPOPTC
325700150508     c                   eval      d98spcdee='IMP'
325800150508     c                   endif
325900150219     c                   endsl
326000150113     c
326100150219     c* se il calcolo è < di oggi --> imposto oggi
326200150113     c                   if        wr1poi < wpoattdata00
326300150113     c                   eval      wr1poi= wpoattdata00
326400150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
326500150508     c                   eval      wTempi00C='S'
326600151009     c                   clear                   wdiff
326700150113     c                   endif
326800150217     c
326900150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
327000150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
327100150630     c                   if        wr1poi>wpoattdata00
327200150630     c                   eval      wdiff=wr1poi-wpoattdata00
327300150630     c                   endif
327400151006     c* se la differenza è 12 ore significa che devo usare i tempi oltre soglia
327500151006     c                   if        wr1poi>wpoattdata00  and  wdiff=12
327600151006     c                   eval      wTempi12C='S'
327700151006     c                   endif
327800151009     c* Se la differenza è > 24 ma oltre soglia devo usare i tempi giusti
327900151009     c* lo faccio solo per disguido. Per IMP i tempi determinati sono
328000151009     c                   if        wdiff>12 and idfgs<>d98lnp and idfgs<>D98TFP
328100151009     C     wdiff         DIV       24            WDIV
328200151009     C                   MVR                     WRES
328300151009     C                   IF        wres>0
328400151009     c                   eval      wTempi12C='S'
328500151009     c                   else
328600151009     c                   eval      wTempi00C='S'
328700151009     c                   endif
328800151009     c                   endif
328900151009     c
329000151006     c
329100151006     c                   select
329200151006    1c                   when      wr1poi>wpoattdata00  and  wdiff=12 and
329300151006     c                             w991in>12 and
329400151006     c                             (deeindist<>'S' or d98cons<>'S'
329500151006     c                             or d98ddc>wdaodi)
329600151006     c
329700151006     c* Se stato IMP lo mantengo per avere messaggio diverso
329800151007     c* ( in questo punto dovrebbe essere SOLO stato IMP/disg)
329900151006     c                   if        d98spcdee<>'IMP'
330000151006     c                   eval      d98spcdee='PRV'
330100151006     c                   endif
330200151006    1c                   when      wr1poi>wpoattdata00  and  wdiff>12 and
330300150330     c                             (deeindist<>'S' or d98cons<>'S'
330400150330     c                             or d98ddc>wdaodi)
330500150427     c
330600150427     c* Se stato IMP lo mantengo per avere messaggio diverso
330700150427     c                   if        d98spcdee<>'IMP'
330800150304     c                   eval      d98spcdee='PRV'
330900150427     c                   endif
331000150427     c
331100151006   x1c                   other
331200150330     c                   exsr      CAL_OLP
331300150217     c
331400150528     c* Se passato l'orario oppure data distinta < data di oggi
331500150508     c* 1) spedizione in distinta          --> in consegna
331600150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
331700150528     c                             d98ddc<wdaodi)
331800150508    3c                   IF        deeindist='S' and d98cons='S'
331900150508     c                   eval      d98spcdee= 'DDC'
332000150508
332100150508   x3c                   else
332200150507     c
332300150722     c* memorizzo le ore che mi servono da sommare, se la  consegna era prevista
332400150722     c*  entro sera
332500150630     c                   if        wr1poi>wpoattdata00
332600150722     c                   z-add     12            woreOLP           3 0
332700150722     c***                add       12            wr1poi
332800150630    4c                   else
332900150507    4c                   if        wr1poi<=wpoattdata00
333000150722     c                   add       24            woreOLP
333100150722     c***                add       24            wr1poi
333200150507    4c                   endif
333300150630    4c                   endif
333400150507     c
333500150722     c****               eval      d98spcdee='OLP'
333600150507     c* memorizzo orario limite picking
333700150507     c                   eval      d98picklim=%editc(picklim:'X')
333800150507    3c                   endif
333900150507    2c                   endif
334000150507     c
334100151006    1c                   endsl
334200150217     c
334300150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
334400150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
334500150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
334600150113     C                   Z-ADD     Wr1poi        W10POI            9 0
334700150113     C                   EXSR      R10FES
334800150113     C                   Z-ADD     W10POO        wr1poi
334900150113     C
335000151006     c                   select
335100151006    1c                   when      wtempi12C='S'
335200151006     c                   z-add     W991IN        wr1noi
335300151006     c                   if        woreOLP>0
335400151006     c                   add       woreOLP       wr1noi
335500151006     c                   eval      d98spcdee='OLP'
335600151006     c                   endif
335700151006    1c                   when      wtempi00C='S'
335800150630     c***                z-add     W990CN        wr1noi
335900150630     c                   z-add     W990IN        wr1noi
336000150722     c
336100150722     c* se i tempi di consegna sono > 24 , la consegna deve essere in giornata
336200150722     c*  quindi sottraggo 24
336300151009     c                   if        wr1noi>24  and wdiff=0
336400150722     c                   sub       24            wr1noi
336500150722     c                   endif
336600150722     c
336700150722     c* se passato orario limite picking, slitto in avanti
336800150722     c                   if        woreOLP>0
336900150722     c                   add       woreOLP       wr1noi
337000150722     c                   eval      d98spcdee='OLP'
337100150722     c                   endif
337200150722     c
337300151006   x1c                   other
337400150722     c
337500150714     c                   if        wmancatraz=' '  and d98dti>0
337600150630     c                   z-add     w99tci        wr1noi
337700150714     c                   else
337800150714     c                   z-add     w99tcc        wr1noi
337900150714     c                   endif
338000150722     c* se i tempi di consegna sono > 24 per arrivo entro soglia
338100150722     c*  o >12 per arrivo oltre soglia --> non sommo le ore "OLP"
338200150722     c                   if        woreOLP=24 and wr1noi<=24
338300150722     c                   add       woreOLP       wr1noi
338400150722     c                   eval      d98spcdee='OLP'
338500150722     c                   endif
338600150722     c                   if        woreOLP=12 and wr1noi<24
338700150722     c                   add       woreOLP       wr1noi
338800150722     c                   eval      d98spcdee='OLP'
338900150722     c                   endif
339000150722     c
339100151006    1c                   endsl
339200150629     c
339300150722    1c**                 if        WR1NOI=10
339400150722     c**                 z-add     12            wr1noi
339500150722     c**                 endif
339600150722     c
339700150722     c     wr1noi        div       12            wdiv
339800150722     c                   mvr                     wres
339900150722     c                   if        wres >0
340000150722     c                   add       2             wr1noi
340100150722     c                   endif
340200150722     c
340300150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
340400150629    1c******             endif
340500150508     c
340600150605     c                   eval      wdee='S'
340700150113     C                   EXSR      RO1CDP
340800150605     c                   eval      wdee=' '
340900150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
341000150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
341100150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
341200150113     C* A QUELLA DI RICHIESTA CONSEGNA,
341300150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
341400150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
341500150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
341600150331     c*  2) IL        -> è il giorno indicato
341700150331     C     D98DCR        IFNE      *ZEROS
341800150331     C     D98tcr        andne     'P'
341900150331     C***  WPOdpcee      IFLE      WPOCR2
342000150331     C***  WPOCR2        SUB       12            WPOdpcee
342100150330     c
342200150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
342300150330     c*   dare la prima data utile di consegna
342400150330     C     WPOdpcee      IFLE      WPOCRF_dee
342500150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
342600150330     C                   z-add     WPOCRF_dee    WPOdpcee
342700150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
342800150409     c                   if        wpodpcee>wpoattdata00
342900150409     c                   eval      d98spcdee='PRV'
343000150409     c                   endif
343100150113     C                   END
343200150331     c
343300150220     c
343400150113     C                   END
343500150220     c
343600150113     c                   ENDSR
343700150330     C********************************************************************
343800150330     C*   Calcolo orario da tabella OLP
343900150330     C********************************************************************
344000150330     c     Cal_OLP       BEGSR
344100150330     c                   eval      picklim=0900
344200150330     c* Dopo l'ora limite picking, passo al giorno dopo
344300150330     C                   MOVEL(P)  'OLP'         TBECOD
344400150330     C                   MOVEL(P)  d98lna        TBEKE1
344500150330     C                   MOVEL(P)  wdaodi        TBEKE2
344600150330     C     KTBE          CHAIN     TNTBE01L
344700150330     C                   IF        not %found(TNTBE01L)
344800150330     c                   clear                   tbeke2
344900150330     C     KTBE          CHAIN     TNTBE01L
345000150330     c                   endif
345100150330     C                   IF        not %found(TNTBE01L)
345200150330     C                   MOVEL(P)  '000'         TBEKE1
345300150330     C     KTBE          CHAIN     TNTBE01L
345400150330     c                   endif
345500150330     c
345600150330     C                   IF        %found(TNTBE01L) and
345700150330     c                             %subst(tbeuni:1:4)>*zeros
345800150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
345900150330     c                   endif
346000150330     c                   ENDSR
346100150325     C********************************************************************
346200150325     C*   Riclcolo data se colli in IMP/disguido
346300150325     C********************************************************************
346400150325     c     CalcoloIMP    BEGSR
346500150325     C                   Z-ADD     IDdata        WDAT
346600150325     C                   EXSR      DATORE
346700150325     C                   Z-ADD     WORE          WR4POI
346800150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
346900151009    2c                   if        IDfgs<>D98lnp and IDfgs<>d98tfp
347000150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
347100150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
347200150325     c* calcolo il terminal di partenza
347300150325     c                   clear                   fnlv55ds
347400150325     c                   eval      d55tla='L'
347500150325     c                   eval      d55tpt='P'
347600150325     c                   eval      d55lin=IDfgs
347700150325     c                   eval      d55drf=IDdata
347800150325     c                   call      'FNLV55R'
347900150325     c                   parm                    fnlv55ds
348000150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
348100150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
348200150325     c                   EXSR      RTTRAZ
348300150325    3c                   if        d98err<>'1'
348400150325     C                   Z-ADD     CASTcn        wr4noi
348500150325     c                   else
348600150325     C                   Z-ADD     d98ttc        wr4noi
348700150325    3C                   endif
348800150325
348900150325   x2c                   else
349000150325     C                   Z-ADD     d98ttc        wr4noi
349100150325    2C                   endif
349200150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
349300150325     c*  perchè partirà un giorno dopo
349400150325    3c                   if        IDnpg='P'
349500150325     c                   add       24            WR4noi
349600150325    3c                   endif
349700150325     c
349800150325     c                   exsr      ro4cdp
349900150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
350000150325     c                   ENDSR
350100930604     C********************************************************************
350200930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
350300930604     C********************************************************************
350400930604     C     TPODAT        BEGSR
350500960229     C*
350600960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
350700960229     C*
350800960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
350900960229     C     WPOPTI        IFEQ      *ZEROS
351000960229     C                   Z-ADD     *ZEROS        D98DTI
351100960229     C                   Z-ADD     *ZEROS        D98HTI
351200960229     C                   ELSE
351300960229     C                   Z-ADD     WPOPTI        WORE
351400960229     C                   EXSR      OREDAT
351500960229     C                   Z-ADD     WDAT          D98DTI
351600960229     C     WRES          MULT      100           D98HTI
351700960229     C                   END
351800960229     C*
351900960229     C                   ELSE                                                   <-- 1 -->
352000930604     C*
352100931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
352200140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
352300140825     c*******            exsr      Tras_DEI
352400140825     c
352500930604     C*
352600150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
352700140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
352800140820     c* è data da :
352900140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
353000140820     c*  Tenere  conto, ovvero slittare in avanti, per :
353100140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
353200140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
353300140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
353400140911     c*
353500140820     c*  Da azzerare nei seguenti casi:
353600150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
353700150324     c* 4) bolla export    7) Cod manca consegna
353800140911     c* 8) bolla con DCR DOPOil o PRIMAdel
353900150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
354000150119     c*10) se completamente non arrivata e collo in IMP
354100150119     c
354200140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
354300150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
354400140925     C*  - FD   - export -
354500150217     c                   movel     'S'           DEEinDIST         1
354600150113     c                   eval      wcaldpcee='S'
354700150304     c                   eval      D98SPCDEE='PRV'
354800150324     c                   clear                   avvcmc            1
354900150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
355000150323     c                   if        d98cmc<>*blanks
355100150324     C                   Z-ADD     1             II
355200150326     C     d98cmc        LOOKUP    C2A(II)                                99
355300150324     C                   MOVEL     D2A(II)       usa2A
355400150324    4C     §2AFTC        IFEQ      'A'
355500150326     c                   eval      avvcmc='A'
355600150323     c                   endif
355700150324    4C     §2AFTC        IFEQ      'G'
355800150326     c                   eval      avvcmc='G'
355900150324     c                   endif
356000150324     c                   endif
356100140820     c
356200140911     c                   z-add     d98lna        EX
356300150113     c*  la azzero nei casi previsti                 0
356400150601    1c                   select
356500150119     c* 1) Se la bolla e consegnata
356600140925     c                   when      d98dcm>0
356700150113     c                   clear                   wcaldpcee
356800150216     c                   eval      d98spcdee='CON'
356900150217     c                   movel     ' '           DEEinDIST         1
357000150323     c* 2) Se pratica di danno
357100150603     c                   when      flgdan='S'
357200150323     c                   clear                   wcaldpcee
357300150323     c                   eval      d98spcdee='CA '
357400150323     c                   eval      deeindist='S'
357500150324     c* 3) Se bloccata/rimpallo
357600150114     c                   when      wd98fbc='B'
357700150113     c                   clear                   wcaldpcee
357800150216     c                   eval      d98spcdee='BLO'
357900150217     c                   movel     ' '           DEEinDIST         1
358000150324     c* 4) Se bolla export
358100150324     c                   when      fl1(EX)='E'
358200150324     c                   clear                   wcaldpcee
358300150324     c                   movel     ' '           DEEinDIST         1
358400150324     c                   eval      d98spcdee='EXP'
358500150324     c* 5) Se giacente  o in attesa di apertura giacenza
358600150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
358700150324     c                   clear                   wcaldpcee
358800150324     c                   eval      d98spcdee='GIA'
358900150324     c                   eval      deeindist='S'
359000150119     c* 7) Se codice manca consegna
359100150326     c                   when      d98cmc<>*blanks and avvcmc='A'
359200150324     c                   clear                   wpodpcee
359300150324     c                   clear                   wcaldpcee
359400150324     c                   movel     ' '           DEEinDIST         1
359500150324     c                   eval      d98spcdee='ATT'
359600150410     c* 7) Bolla in sospensione consegna
359700150410     c                   when      §B4LBGE='S'
359800150410     c                   clear                   wpodpcee
359900150410     c                   clear                   wcaldpcee
360000150410     c                   movel     ' '           DEEinDIST         1
360100150410     c                   eval      d98spcdee='SOS'
360200150616     c
360300150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
360400150616     c                   when      wrbl='R'
360500150616     c                   clear                   wcaldpcee
360600150616     c                   eval      d98spcdee='   '
360700150616     c                   eval      deeindist='S'
360800150601   x1c                   other
360900150325     c*
361000150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
361100150331     c                   if        d98ffd='S'
361200150601    2c                   select
361300150403     c* FD escluso dai contatti
361400150601    2c                   when      §B4LTFD= 'N'
361500150403     c                   eval      d98spcdee='FD3'
361600150403     c                   clear                   wcaldpcee
361700150403     c                   movel     ' '           DEEinDIST         1
361800150403
361900150403     c* FD non contattato ( o da ricontattare)
362000150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
362100150401     c                             olv82sts<>'1'
362200150323     c                   clear                   wcaldpcee
362300150323     c                   movel     ' '           DEEinDIST         1
362400150323     c                   eval      d98spcdee='FFD'
362500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
362600150424     c                   eval      wpodpcee=wpoffi
362700150403
362800150403     c* FD contattato e     IN distinta
362900150601    2c                   when      d98cons='S'
363000150401     c                   eval      d98spcdee='FD2'
363100150401     c                   clear                   wcaldpcee
363200150401     c                   movel     ' '           DEEinDIST         1
363300150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
363400150424     c                   eval      wpodpcee=wpoffi
363500150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
363600150427     c*  ritirare la merce
363700150427     C                   MOVEL     'C'           WTC                            *PARTENZA
363800150427     C                   MOVEL     'C'           WIC
363900150427     C                   Z-ADD     D98ddc        WDAT
364000150427     C                   EXSR      DATORE
364100150427     c                   z-add     wore          wr4poi
364200150427     c                   z-add     24            wr4noi
364300150427     C                   EXSR      RO4CDP
364400150427     c                   z-add     wr4poo        wore
364500150427     c                   exsr      OREDAT
364600150427     c                   movel     wdat          D98DTCTD
364700150601   x2c                   other
364800150403     c* FD contattato e NON in distinta
364900150331     c                   eval      d98spcdee='FD1'
365000150408     c                   eval      wpodpcee=wpoffi
365100150331     c                   clear                   wcaldpcee
365200150331     c                   movel     ' '           DEEinDIST         1
365300150601    2c                   endsl
365400150331     c
365500150331     c                   endif
365600150403     c
365700150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
365800150601    2c                   if        (d98tc1='A' or d98tc2='A' or
365900150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
366000150601     c                             and d98ffd= ' '
366100150325     c                   clear                   wcaldpcee
366200150325     c                   movel     ' '           DEEinDIST         1
366300150601    3c                   if        §B4LBAS= 'N'
366400150427     c                   eval      d98spcdee='AP3'
366500150403     c                   else
366600150403     c                   eval      d98spcdee='APP'
366700150601    3c                   endif
366800150601    2c                   endif
366900150325     c*   se la bolla ha il C/Assegno verifico se da contattare
367000150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
367100150325     c                   clear                   wcaldpcee
367200150325     c                   movel     ' '           DEEinDIST         1
367300150601    3c                   if        §B4LBAS= 'N'
367400150427     c                   eval      d98spcdee='CA3'
367500150403     c                   else
367600150403     c                   eval      d98spcdee='CAS'
367700150601    3c                   endif
367800150601    2c                   endif
367900150325     c* Per questi 3 casi devo impostare la data contatto destinatario
368000150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
368100150325     c                             d98spcdee='CAS'
368200150601    2c                   select
368300150601    2c                   when      d98dti>0 and wmancatraz= ' '
368400150325     c                   movel     d98dti        d98dtctd
368500150403     c* se piene le ore già portate ad un giorno lavorativo
368600150403     c                   if        wpodti>0
368700150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
368800150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
368900150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
369000150403     C                   Z-ADD     WPODTI        W10POI            9 0
369100150403     C                   EXSR      R10FES
369200150403     c                   z-add     w10poo        wore
369300150403     C                   EXSR      OREDAT
369400150403     C                   movel     WDAT          D98dtctd
369500150403     c                   endif
369600150403     c
369700150601    2c                   when      d98dam>0
369800150421     C                   Z-ADD     D98dam        WDAT
369900150421     C                   EXSR      DATORE
370000150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
370100150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
370200150421     C                   Z-ADD     Wore          W10POI            9 0
370300150421     C                   EXSR      R10FES
370400150421     c                   z-add     w10poo        wore
370500150421     C                   EXSR      OREDAT
370600150421     C                   movel     WDAT          D98dtctd
370700150601   x2c                   other
370800150325     c* data teorica di arrivo tenendo conto dell'IMP
370900150325     c                   exsr      letbrv
371000150325     c                   if        idDATA>0
371100150325     c                   exsr      CalcoloIMP
371200150325     c                   z-add     wr1poi        WORE
371300150325     c                   else
371400150325     c                   z-add     wpoptc        WORE
371500150325     c                   endif
371600150325     C                   EXSR      OREDAT
371700150325     c                   movel     wdat          d98dtctd
371800150601    2c                   endsl
371900150325     c
372000150325     c                   endif
372100150325     c
372200150601    1c                   endsl
372300140820     c
372400150113     c* Se la bolla non è in distinta calcolo la data se previsto
372500150220     c                   if        wcaldpcee='S' and
372600150220     c                             (d98cons<>'S'  or (d98cons='S' and
372700150220     c                             evemic<>'SI'))
372800150219     c
372900150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
373000150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
373100150219     c                   if        d98nsp>0 and wtrasm<>'D' and
373200150219     c                             d98dam=0
373300150219     c                   exsr      letBRV
373400150219     c                   endif
373500150219     c
373600150217     c                   eval      d98spcdee='PRG'
373700150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
373800150320     c                             d98tc1='S' or d98tc2='S')
373900150320     c                   eval      d98spcdee='APP'
374000150320     c                   endif
374100150320     c
374200150113     c                   exsr      cal_DEE
374300150113     c                   endif
374400150113     c
374500140820     C     WPOdpcee      IFEQ      *ZEROS
374600931202     C                   Z-ADD     *ZEROS        D98DEE
374700931202     C                   Z-ADD     *ZEROS        D98OEE
374800930604     C                   ELSE
374900140820     C                   Z-ADD     WPOdpcee      WORE
375000141021     C******             SUB       12            WORE
375100930604     C                   EXSR      OREDAT
375200931202     C                   Z-ADD     WDAT          D98DEE
375300090903     c                   if        d98tcc<>10
375400090903     C                   ADD       12            WRES
375500090903     c                   else
375600090903     C                   ADD       10            WRES
375700090903     c                   endif
375800090903
375900931202     C     WRES          MULT      100           D98OEE
376000090903     c
376100090903     c                   if        d98oee=1000
376200090903     c                   eval      d98oee=1030
376300090903     c                   endif
376400140925     C                   END
376500150311     c
376600150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
376700150311     c                   select
376800150311     c                   when      d98spcdee='OLP'
376900140820     c
377000150220     c* Se bolla in distinta --> data prevista = data distinta
377100150311     c                   when      deeindist='S'  and
377200150113     c                             d98cons='S'
377300150220
377400150225     c*  non la propongo senza MIC e se presente disguido o IMP
377500150220     c                   if        d98dee>d98ddc and evemic<>'SI'
377600150220     c                             and IDdata>0
377700150220     c                   else
377800150220
377900140820     c                   eval      d98dee=d98ddc
378000140925     c                   clear                   d98oee
378100150320     c* se la data è > di oggi stato PRV se non è in IMP
378200150320     c                   if        d98dee>wdaodi
378300150320     c                   if        d98spcdee<>'IMP'
378400150311     c                   eval      d98spcdee='PRV'
378500150320     c                   endif
378600150320     c
378700150225     c                   else
378800150217     c* se c'è il MIC
378900150217     c                   if        evemic='SI'
379000150217     c                   eval      d98spcdee='DDC'
379100150225     c                   endif
379200150217     c                   endif
379300150220     c                   endif
379400150217     c
379500150722     c                   when      d98spcdee='PRG' and d98dee>wdaodi
379600150722     c                   eval      d98spcdee='PRV'
379700150311     c                   endsl
379800931202     C*
379900931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
380000931202     C     WPODCI        IFEQ      *ZEROS
380100931202     C                   Z-ADD     *ZEROS        D98DCI
380200931202     C                   Z-ADD     *ZEROS        D98HCI
380300931202     C                   ELSE
380400931202     C                   Z-ADD     WPODCI        WORE
380500931202     C                   EXSR      OREDAT
380600931202     C                   Z-ADD     WDAT          D98DCI
380700090902     c                   if        d98tci<>10
380800931202     C                   ADD       12            WRES
380900090902     c                   else
381000090902     C                   ADD       10            WRES
381100090902     c                   endif
381200090902
381300931202     C     WRES          MULT      100           D98HCI
381400090901     c
381500090903     c                   if        d98hci=1000
381600090901     c                   eval      d98hci=1030
381700090901     c                   endif
381800090901     c
381900931202     C                   END
382000931202     C*
382100931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
382200931202     C     WPODCE        IFEQ      *ZEROS
382300931202     C                   Z-ADD     *ZEROS        D98DCE
382400931202     C                   Z-ADD     *ZEROS        D98HCE
382500931202     C                   ELSE
382600931202     C                   Z-ADD     WPODCE        WORE
382700931202//// C**************       SUB  12        WORE
382800931202     C                   EXSR      OREDAT
382900931202     C                   Z-ADD     WDAT          D98DCE
383000090902     c                   if        d98tcc >10
383100931202     C                   ADD       12            WRES
383200090902     c                   else
383300090902     C                   ADD       10            WRES
383400090902     c                   endif
383500090902     c
383600931202     C     WRES          MULT      100           D98HCE
383700090902     c
383800090903     c                   if        d98hce=1000
383900090902     c                   eval      d98hce=1030
384000090902     c                   endif
384100090902     c
384200931202     C                   END
384300960229     C*
384400960229     C                   ENDIF                                                  <-- 1 ---
384500930604     C*
384600930604     C                   ENDSR
384700150218     C********************************************************************
384800150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
384900150218     C********************************************************************
385000150218     c     LetBRV        BEGSR
385100150219     c                   clear                   bollaOK           1
385200150219     c                   clear                   IDnpg
385300150219     c                   clear                   IDdata
385400150219     c                   clear                   IDora
385500150219     c                   clear                   IDfgs
385600150219     c
385700150218     C     Kart          SETLL     FNART01L
385800150218     C     Kart          READE     FNART01L
385900151002    1c                   dow       not %eof(fnart01l) and bollaok=' '
386000150219     c                   clear                   DISfgs
386100150219     c                   clear                   DISdata
386200150219     c                   clear                   DISnpg
386300150219     c                   clear                   DISora
386400150219     c                   clear                   IMPdata
386500150219     c                   clear                   IMPfgs
386600150508     c* per ogni collo verifico se in IMP o disguidato
386700150218     c* al primo che NON trovo, esco come --> OK
386800150218     c     kbrv          setll     fnbrv07l
386900150218     c     kbrv          reade     fnbrv07l
387000150219    2c                   dow       not %eof(fnbrv07l)
387100150219    3c                   if        brvatr='D'  or brvnpg=3
387200150218     c* prendo la data della spunta
387300150218     c                   clear                   fnlv53ds
387400150219     c                   eval      d53tla='L'
387500150218     C                   MOVEL     BRvNPG        D53NPG
387600150218     C                   Z-ADD     BRvNFV        D53NFV
387700150218     C                   MOVEL     BRvFGS        D53FGS
387800150218     C                   CALL      'FNLV53R'
387900150218     C                   PARM                    fnlv53ds
388000150218    4c                   if        d53err<>*blanks
388100150219    5c                   if        brvdcs>0
388200150218     c                   eval      d53dfv=brvdcs
388300150218     c                   else
388400150219    6c                   if        brvdfs>0
388500150218     c                   eval      d53dfv=brvdfs
388600150219    6c                   endif
388700150219    5c                   endif
388800150219    4c                   endif
388900150218     c
389000150219    4c                   select
389100150219     c* se si tratta di DISGUIDO
389200150218     c                   when      brvatr='D'
389300150219     c
389400150219    5c                   if        d53dfv=DISdata
389500150219     c* Verifico l'ora
389600150219    6c                   if        brvhcs>0 and brvhcs>DISora
389700150219     c                   eval      DISdata=d53dfv
389800150219     c                   eval      DISfgs =brvfgs
389900150219     c                   eval      DISnpg =brvnpg
390000150219     c                   eval      DISora =brvhcs
390100150219   x6c                   else
390200150219    7c                   if        brvhms>0 and brvhms>DISora
390300150219     c                   eval      DISdata=d53dfv
390400150219     c                   eval      DISfgs =brvfgs
390500150219     c                   eval      DISnpg =brvnpg
390600150219     c                   eval      DISora =brvhms
390700150219    7c                   endif
390800150219    6c                   endif
390900150219    5c                   endif
391000150219     c
391100150219    5c                   if        d53dfv>DISdata
391200150219     c                   eval      DISdata=d53dfv
391300150219     c                   eval      DISfgs =brvfgs
391400150219     c                   eval      DISnpg =brvnpg
391500150219     c                   eval      DISora =brvhcs
391600150219     c                   if        disora=0
391700150219     c                   eval      DISora =brvhms
391800150219     c                   endif
391900150219    5c                   endif
392000150219     c
392100151009     c* se si tratta di IMP
392200151009    4c                   when      d53spg='P' and d53dfv>impdata
392300151009     c                   eval      IMPdata=d53dfv
392400151009     c                   eval      IMPfgs =brvfgs
392500150219    4c                   endsl
392600150219    3c                   endif
392700151009
392800150219     c     kbrv          reade     fnbrv07l
392900150219    2c                   enddo
393000150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
393100150219     c*  esco dal ciclo: la bolla è OK
393200150219    2c                   if        impdata=0 and disdata=0
393300150219     c                   eval      bollaOK='S'
393400150219     c                   clear                   IDdata
393500150219     c                   clear                   IDfgs
393600150219     c                   clear                   IDora
393700150219     c                   clear                   IDnpg
393800150219   x2c                   else
393900150219     c* memorizzo la data più alta
394000150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
394100150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
394200150219     c                   eval      IDdata=IMPdata
394300150219     c                   eval      IDfgs=IMPfgs
394400150219     c                   eval      IDora=0
394500150219     c                   eval      IDnpg='P'
394600150219    3c                   endif
394700150219    3c                   if        (DISdata>0 and DISdata=IDdata
394800150219     c                             and IDnpg<>'P' and DISora>IDora) or
394900150219     c                             (DISdata>0 and DISdata>IDdata)
395000150219     c                   eval      IDdata=DISdata
395100150219     c                   eval      IDfgs=DISfgs
395200150219     c                   eval      IDora=DISora
395300150219     c                   eval      IDnpg=%char(DISnpg)
395400150219    3c                   endif
395500150219    2c                   endif
395600150218     c
395700150218     C     Kart          READE     FNART01L
395800150219    1c                   enddo
395900150219     c
396000150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
396100150508     c**                 if        d98duc> 0 and d98duc>IDdata
396200150508     c**                 eval      bollaOK='S'
396300150508     c**                 clear                   IDdata
396400150508     c**                 clear                   IDfgs
396500150508     c**                 clear                   IDora
396600150508     c**                 clear                   IDnpg
396700150508     c**                 endif
396800150219     c
396900150218     c
397000150218     C                   ENDSR
397100140825     C********************************************************************
397200140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
397300140825     C********************************************************************
397400140825     c     TRAS_DEI      BEGSR
397500140825     C     WPODEI        IFEQ      *ZEROS
397600140825     C                   Z-ADD     *ZEROS        D98DEI
397700140825     C                   Z-ADD     *ZEROS        D98OEI
397800140825     C                   ELSE
397900140825     C                   Z-ADD     WPODEI        WORE
398000140825     C                   SUB       12            WORE
398100140825     C                   EXSR      OREDAT
398200140825     c
398300140825     C                   Z-ADD     WDAT          D98DEI
398400140825     c                   if        d98tci<>10
398500140825     C                   ADD       12            WRES
398600140825     c                   else
398700140825     C                   ADD       10            WRES
398800140825     c                   endif
398900140825     c
399000140825     C     WRES          MULT      100           D98OEI
399100140825     c
399200140825     c                   if        d98oei=1000
399300140825     c                   eval      d98oei=1030
399400140825     c                   endif
399500140825     c
399600140825     C                   END
399700140825     c                   ENDSR
399800930604     C********************************************************************
399900930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
400000930604     C********************************************************************
400100930604     C     CHKFOR        BEGSR
400200930604     C*
400300930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
400400930604     C* E QUINDI FORZO LA CONFORMITA' A SI
400500941227     C     D98ERR        IFEQ      '0'
400600930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
400700930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
400800930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
400900930604     C                   MOVEL     '1'           D98ERR
401000930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
401100930604     C                   END
401200930604     C                   END
401300990212     C*
401400990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
401500990212     C* E QUINDI FORZO LA CONFORMITA' A SI
401600990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
401700990212     C     D98ERR        IFEQ      '0'
401800990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
401900141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
402000990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
402100990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
402200990212     C                   MOVEL     '1'           D98ERR
402300990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
402400990212     C                   END
402500990212     C                   END
402600990308     C*
402700990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
402800990308     C* E QUINDI FORZO LA CONFORMITA' A SI
402900100614     C**   D98ERR        IFEQ      '0'
403000100614     C**   WFBR          IFEQ      'S'                                          *RESO
403100100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
403200100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
403300100614     C**                 MOVEL     '1'           D98ERR
403400100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
403500100614     C**                 END
403600100614     C**                 END
403700930604     C*
403800930604     C                   ENDSR
403900990212     C********************************************************************
404000990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
404100990212     C********************************************************************
404200990212     C     CHKDAN        BEGSR
404300990212     C*
404400990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
404500150603     c                   movel     'N'           opendan           1
404600040809      *
404700040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
404800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
404900040809      * legate alla spedizione che passo
405000040809     c                   clear                   fidn12ds
405100040809     c                   eval      i12aas = D98AAS
405200040809     c                   eval      i12lnp = D98LNP
405300040809     c                   eval      i12nrs = D98NRS
405400040809     c                   eval      i12nsp = D98NSP
405500040809     c                   eval      i12fel = 'E'
405600040809     c                   eval      i12fan = 'E'
405700040809     c*
405800040809     c                   call      'FIDN12R'
405900040809     c                   parm                    fidn12ds
406000040809     c*
406100040809     c* se non ci sono errori
406200040809     c                   if        o12err = ' '
406300040809     c* se numero di CA trovate maggiore di zero
406400040809     c                   if        o12nca > 0
406500040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
406600150603     c
406700150603     c***                z-add     1             aa                2 0
406800150603     c***                dow       aa <= o12nca
406900150603     C* APERTA
407000150603     C***  skdch(aa)     IFEQ      0
407100150603     c***                movel     'S'           opendan
407200150603     c***                endif
407300150603     c*
407400150603     c***                add       1             aa
407500150603     c***                enddo
407600150603     c
407700040809     c                   endif
407800040809     c                   endif
407900990212     C*
408000990212     C                   ENDSR
408100941228     C********************************************************************
408200941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
408300941228     C********************************************************************
408400941228     C     CALORP        BEGSR
408500941228     C*
408600941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
408700941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
408800941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
408900960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
409000950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
409100950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
409200950110     C                   ELSE
409300950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
409400950110     C                   ENDIF
409500941228     C                   EXSR      RO2COP
409600941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
409700941228     C* SPED. IL GIORNO DOPO
409800941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
409900941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
410000941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
410100941228     C*
410200941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
410300941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
410400941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
410500941228     C                   Z-ADD     WPODSP        WR2POI
410600950110     C                   Z-ADD     WPOPPC        WR2POF
410700941228     C                   EXSR      RO2COP
410800941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
410900941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
411000941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
411100941228     C*
411200941228     C                   ENDSR
411300930414     C********************************************************************
411400930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
411500930414     C********************************************************************
411600930414     C     CALORT        BEGSR
411700930414     C*
411800930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
411900931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
412000930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
412100931126     C                   Z-ADD     *ZEROS        W98HF
412200950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
412300950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
412400950110     C                   ELSE
412500950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
412600950110     C                   ENDIF
412700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
412800931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
412900930608     C                   ELSE
413000931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
413100930608     C                   END
413200931126     C                   EXSR      RO2COV
413300931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
413400930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
413500930414     C*
413600930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
413700930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
413800930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
413900931126     C                   Z-ADD     *ZEROS        W98HF
414000950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
414100931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
414200931126     C                   EXSR      RO2COV
414300931126     C                   Z-ADD     WR2NOV        W98RTC
414400930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
414500930414     C*
414600930414     C                   ENDSR
414700930414     C********************************************************************
414800930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
414900930414     C********************************************************************
415000930414     C     CALORC        BEGSR
415100930511     C*
415200930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
415300930511     C* I TEMPI DI RITARDO AD OGGI
415400930511     C     WPODCM        IFEQ      *ZEROS
415500960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
415600930511     C                   END
415700940117     C*
415800940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
415900940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
416000940117     C* ALLA DATA DI FINE CONSEGNABILITA'
416100940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
416200940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
416300940118££££ C     WPODFC        IFNE      *ZEROS
416400940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
416500940118££££ C                   END
416600930414     C*
416700930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
416800930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
416900930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
417000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
417100931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
417200930608     C                   ELSE
417300931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
417400930608     C                   END
417500931126     C                   Z-ADD     WPODCM        WR5POF            9 0
417600931126     C                   EXSR      RO5COV
417700931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
417800930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
417900930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
418000060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
418100060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
418200930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
418300930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
418400930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
418500930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
418600930414     C*
418700930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
418800930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
418900930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
419000931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
419100931126     C                   Z-ADD     WPODCM        WR5POF            9 0
419200931126     C                   EXSR      RO5COV
419300931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
419400930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
419500930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
419600060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
419700060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
419800930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
419900930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
420000930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
420100930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
420200930414     C*
420300930414     C                   ENDSR
420400930414     C********************************************************************
420500930414     C*   CALORR - CALCOLA ORE REALI DI RESA
420600930414     C********************************************************************
420700930414     C     CALORR        BEGSR
420800930414     C*
420900950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
421000950110     C                   ADD       W98RTI        W98RRI
421100941228     C                   ADD       W98RCI        W98RRI
421200950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
421300950110     C                   ADD       W98RTC        W98RRC
421400941228     C                   ADD       W98RCC        W98RRC
421500930414     C*
421600930414     C                   ENDSR
421700930420     C********************************************************************
421800930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
421900930420     C********************************************************************
422000930420     C     CALDIF        BEGSR
422100930420     C*
422200941228     C* PARTENZA
422300941228     C     W98RPI        SUB       D98TPI        W98FPI
422400941228     C     W98RPC        SUB       D98TPC        W98FPC
422500941228     C*
422600930420     C* TRAZIONE
422700930423     C     W98RTI        SUB       D98TTI        W98FTI
422800930423     C     W98RTC        SUB       D98TTC        W98FTC
422900930420     C*
423000930420     C* CONSEGNA
423100090827     C***  W98RCI        SUB       D98TCI        W98FCI
423200090827     C     W98RCI        SUB       w99TCI        W98FCI
423300090827     C***  W98RCC        SUB       D98TCC        W98FCC
423400090827     C     W98RCC        SUB       w99TCC        W98FCC
423500930420     C*
423600930420     C* RESA
423700090921     C***  W98RRI        SUB       D98TRI        W98FRI
423800090921     C     W98RRI        SUB       w99TRI        W98FRI
423900090921     c
424000090921     C***  W98RRC        SUB       D98TRC        W98FRC
424100090921     C     W98RRC        SUB       w99TRC        W98FRC
424200930420     C*
424300930420     C                   ENDSR
424400930420     C********************************************************************
424500930420     C*   CALCNF - CALCOLA LE CONFORMITA'
424600930420     C********************************************************************
424700930420     C     CALCNF        BEGSR
424800941228     C*
424900941228     C*-------------------------------------------------------------------
425000941228     C* CONFORMITA' PARTENZA  INTERNA
425100941228     C*-------------------------------------------------------------------
425200941228     C                   Z-ADD     W98FPI        W98NPI
425300941228     C     W98FPI        IFLE      *ZEROS
425400941228     C                   MOVEL     'S'           D98CPI
425500941228     C                   ELSE
425600941228     C                   MOVEL     'N'           D98CPI
425700941228     C                   END
425800941228     C*
425900941228     C*-------------------------------------------------------------------
426000941228     C* CONFORMITA' PARTENZA  CLIENTE
426100941228     C*-------------------------------------------------------------------
426200941228     C                   Z-ADD     W98FPC        W98NPC
426300941228     C     W98FPC        IFLE      *ZEROS
426400941228     C                   MOVEL     'S'           D98CPC
426500941228     C                   ELSE
426600941228     C                   MOVEL     'N'           D98CPC
426700941228     C                   END
426800930420     C*
426900930420     C*-------------------------------------------------------------------
427000930420     C* CONFORMITA' TRAZIONE  INTERNA
427100930420     C*-------------------------------------------------------------------
427200930510     C                   Z-ADD     W98FTI        W98NTI
427300930423     C     W98FTI        IFLE      *ZEROS
427400930420     C                   MOVEL     'S'           D98CTI
427500930420     C                   ELSE
427600930420     C                   MOVEL     'N'           D98CTI
427700930420     C                   END
427800930420     C*
427900930420     C*-------------------------------------------------------------------
428000930420     C* CONFORMITA' TRAZIONE  CLIENTE
428100930420     C*-------------------------------------------------------------------
428200930510     C                   Z-ADD     W98FTC        W98NTC
428300930423     C     W98FTC        IFLE      *ZEROS
428400930420     C                   MOVEL     'S'           D98CTC
428500930420     C                   ELSE
428600930420     C                   MOVEL     'N'           D98CTC
428700930420     C                   END
428800930420     C*
428900930420     C*-------------------------------------------------------------------
429000930420     C* CONFORMITA' CONSEGNA  INTERNA
429100930420     C*-------------------------------------------------------------------
429200150226     c                   if        wmancatraz='S'
429300150226     c                   clear                   d98dti
429400150226     c                   clear                   d98hti
429500150226     c                   endif
429600150226     c
429700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
429800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
429900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
430000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
430100050914     c                   if        consideraora='S'
430200050914     c     w98fci        sub       24            w98nci
430300050914     c                   add       24            d98hrc
430400050914     c                   else
430500930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
430600050914     c                   endif
430700080116     c
430800110923     c                   select
430900080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
431000080116     c*  --> trasformo il ritardo in 0
431100090827     c                   when      d98cca='2'
431200080116     c                   z-add     0             w98nci
431300090827
431400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
431500080116     c*  e sto leggendo la bolla in arrivo
431600080116     c*  --> trasformo il ritardo = 0
431700090827     c                   when      d98cca='1' and d98tbo='A'
431800080116     c                   z-add     0             w98nci
431900090902     c
432000090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
432100090902     c                   when      wmancaTRAZ='S'
432200090902     c                   z-add     0             w98nci
432300090902     c                   clear                   d98dti
432400090902     c                   clear                   d98hti
432500110923     c
432600110923     c* Ignoro ritardo se pefect order
432700130221     c                   when      d98xco >*zeros
432800110923     c                   z-add     0             w98nci
432900090827     c
433000090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
433100091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
433200091014     c*  controllo ora soglia  consegna da tabella OSC)
433300091009     c                   when      d98tci=10   and
433400091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
433500091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
433600091014     c*
433700091014     c                             d98hci=1030
433800091014     C                   MOVEL     'OSC  '       WR3TAB
433900091014     C                   eval      WR3HHI=d98hmc
434000091014     C                   EXSR      RO3ORE
434100131218
434200091014     c                   if        wr3matpom='M' and d98hmc>1030
434300131218     c                   exsr      ctrLAVH1030
434400131218
434500131218     c                   if        wlav1030<>'S'
434600091014     c                   add       12            w98nci
434700091014     c                   endif
434800131218     c                   endif
434900090928     c
435000090827     c                   endsl
435100050914     c
435200930524     C                   ELSE                                                   <-- 1 -->
435300930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
435400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
435500930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
435600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
435700930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
435800930608     C                   ELSE
435900930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
436000930608     C                   END
436100930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
436200930525     C                   EXSR      RO2COV
436300930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
436400930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
436500930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
436600930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
436700931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
436800930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
436900930608     C                   ELSE
437000930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
437100930608     C                   END
437200931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
437300930525     C                   EXSR      RO2COV
437400930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
437500930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
437600930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
437700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
437800930525     C* E' IL RITARDO
437900930610     C     WNOCRI        SUB       W98RCI        W98NCI
438000050901     c
438100050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
438200050901     c*  --> trasformo il ritardo in anticipo
438300050901     c                   if        d98cca='2'
438400080116     c***                z-sub     w98nci        w98nci
438500080116     c                   z-add     0             w98nci
438600050901     c                   endif
438700111018     c* Ignoro ritardo se pefect order
438800130221     c                   if        d98xco >*zeros
438900111018     c                   z-add     0             w98nci
439000111018     c                   endif
439100061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
439200061121     c*  e sto leggendo la bolla in arrivo
439300061121     c*  --> trasformo il ritardo in anticipo
439400061121     c                   if        d98cca='1' and d98tbo='A'
439500080116     c***                z-sub     w98nci        w98nci
439600080116     c                   z-add     0             w98nci
439700061121     c                   endif
439800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
439900070827     c*  mittente, --> consegnato in anticipo
440000070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
440100140403     c                   if        Callar5=' '
440200070827     c                   exsr      LeggiAR5
440300140403     c                   endif
440400070827     c
440500070827     c                   if        dcrpart<>'S'
440600070824     c                   z-sub     w98nci        w98nci
440700070824     c                   endif
440800070827     c
440900070824     c                   endif
441000050907     c
441100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
441200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
441300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
441400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
441500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
441600050916     c***                if        d98dfc>0
441700070828     c                   if        d98dcm=0  and w98nci>0
441800050907     c                   z-sub     w98nci        w98nci
441900050907     c                   sub       oreILoDOPO    w98nci
442000050907     c                   endif
442100050901     c
442200930525     C                   ELSE                                                   <-- 2 -->
442300070521     c* non do ritardo se la consegna reale cade nel giorno
442400070521     c*  della consegna richiesta se presente IL
442500100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
442600070521     c                             wpodcm<=wpocr1
442700070521     c                   clear                   w98nci
442800100224     c                   else                                                   ---2a -->
442900930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
443000090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
443100090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
443200930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
443300930525     C* E' IL RITARDO O L'ANTICIPO
443400930525     C     W98RCI        SUB       WNOCRF        W98NCI
443500070115     c
443600070115     c* non do ritardo se comunque la consegna reale cade nel giorno
443700070115     c*  della consegna richiesta se presente IL
443800070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
443900070521     c***                          wpodcm<=wpocr1  and w98nci>0
444000070521     c***                clear                   w98nci
444100070521     c***                endif
444200070115     c
444300930525     C                   ELSE                                                   <-- 3 -->
444400930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
444500930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
444600930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
444700930525     C                   END                                                    <-- 3 ---
444800100224     C                   END                                                    <-- 2a---
444900100224
445000100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
445100100224     c                   select
445200100224     c                   when      d98cca='2'
445300100224     c                   z-add     0             w98nci
445400100224
445500100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
445600100224     c*  e sto leggendo la bolla in arrivo
445700100224     c                   when      d98cca='1' and d98tbo='A'
445800100224     c                   z-add     0             w98nci
445900100224     c                   endsl
446000100224     c
446100100224     C                   END                                                    <-- 2 ---
446200070521     C                   END                                                    <-- 1 ---
446300070521     c
446400970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
446500970915     C*/         WAZNCI    IFEQ 'S'
446600970915     C*/         W98NCI    ANDLT*ZEROS
446700970915     C*/                   Z-ADD*ZEROS    W98NCI
446800970915     C*/                   ENDIF
446900970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
447000050922     c* lo faccio solo se non c'e' la data cons richiesta
447100970915     C     D98DTI        IFEQ      *ZEROS
447200050922     c     d98dcr        andeq     *zeros
447300970915     C                   Z-ADD     *ZEROS        W98NCI
447400970915     C                   ENDIF
447500930524     C*
447600930524     C* CONTROLLO CONFORMITA
447700930423     C     W98NCI        IFLE      *ZEROS
447800930524     C                   MOVEL     'S'           D98CCI
447900930420     C                   ELSE
448000930524     C                   MOVEL     'N'           D98CCI
448100930420     C                   END
448200930420     C*
448300930420     C*-------------------------------------------------------------------
448400930420     C* CONFORMITA' CONSEGNA  CLIENTE
448500930420     C*-------------------------------------------------------------------
448600930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
448700930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
448800930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
448900050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
449000050914     c                   if        consideraora='S'
449100050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
449200050914     c                   else
449300050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
449400050914     c                   endif
449500080116     c
449600080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
449700080116     c*  --> trasformo il ritardo =0
449800090827     c                   select
449900090827     c                   when      d98cca='2'
450000080116     c                   z-add     0             w98ncc
450100090827
450200080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
450300080116     c*  e sto leggendo la bolla in arrivo
450400080116     c*  --> trasformo il ritardo =0
450500090827     c                   when      d98cca='1' and d98tbo='A'
450600080116     c                   z-add     0             w98ncc
450700090902     c
450800090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
450900090902     c                   when      wmancaTRAZ='S'
451000090902     c                   z-add     0             w98ncc
451100110923     c* Ignoro ritardo se Perfect Order
451200130221     c                   when      d98xco >*zeros
451300110923     c                   z-add     0             w98ncc
451400090902     c
451500090827     c
451600090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
451700090827     c* sono  in ritardo di 12 ore
451800091009     c                   when      d98tcc=10  and
451900091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
452000091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
452100091014     c*
452200091014     c                             d98hce=1030
452300091014     C                   MOVEL     'OSC  '       WR3TAB
452400091014     C                   eval      WR3HHI=d98hmc
452500091014     C                   EXSR      RO3ORE
452600091014     c                   if        wr3matpom='M' and d98hmc>1030
452700131218     c                   exsr      ctrLAVH1030
452800131218
452900131218     c                   if        wlav1030<>'S'
453000131218     c                   add       12            w98ncc
453100131218     c                   endif
453200131218     c****               add       12            w98ncc
453300091014     c                   endif
453400131218     c
453500091014     c***                z-add     12            w98ncc
453600090827     c                   endsl
453700050914     c
453800930524     C                   ELSE                                                   <-- 1 -->
453900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
454000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
454100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
454200930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
454300930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
454400930525     C                   EXSR      RO2COV
454500930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
454600930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
454700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
454800930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
454900930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
455000931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
455100930525     C                   EXSR      RO2COV
455200930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
455300930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
455400930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
455500930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
455600930525     C* E' IL RITARDO
455700930525     C     WNOCRI        SUB       W98RCC        W98NCC
455800050901     c
455900050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
456000050901     c*  --> trasformo il ritardo in anticipo
456100050901     c                   if        d98cca='2'
456200080116     c***                z-sub     w98ncc        w98ncc
456300080116     c                   z-add     0             w98ncc
456400050901     c                   endif
456500111018     c* Ignoro ritardo se pefect order
456600130221     c                   if        d98xco >*zeros
456700111018     c                   z-add     0             w98ncc
456800111018     c                   endif
456900061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
457000061121     c*  e sto leggendo la bolla in arrivo
457100061121     c*  --> trasformo il ritardo in anticipo
457200061121     c                   if        d98cca='1' and d98tbo='A'
457300080116     c***                z-sub     w98ncc        w98ncc
457400080116     c                   z-add     0             w98ncc
457500061121     c                   endif
457600070824     c
457700070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
457800070827     c*  mittente, --> consegnato in anticipo
457900070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
458000070827     c* SBR per vedere se tassativa
458100070827     c                   if        Callar5=' '
458200070827     c                   EXSR      LeggiAR5
458300070827     c                   endif
458400070827     c                   if        dcrpart<>'S'
458500070824     c                   z-sub     w98ncc        w98ncc
458600070824     c                   endif
458700070824     c                   endif
458800050907     c
458900050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
459000050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
459100050907     c*  lo trasformo in anticipo perchè non sono in ritardo
459200050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
459300050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
459400050916     c***                if        d98dfc>0
459500070828     c                   if        d98dcm=0 and w98ncc>0
459600050907     c                   z-sub     w98ncc        w98ncc
459700050907     c                   sub       oreILoDOPO    w98ncc
459800050907     c                   endif
459900050901     c
460000930525     C                   ELSE                                                   <-- 2 -->
460100070521     c* Non do ritardo se la consegna reale cade nel giorno
460200070521     c*  della consegna richiesta se presente IL
460300100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
460400070521     c                             wpodcm<=wpocr2
460500070521     c                   clear                   w98ncc
460600100224     c                   else                                                   <--2a -->
460700070521     c
460800930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
460900090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
461000090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
461100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
461200930525     C* E' IL RITARDO O L'ANTICIPO
461300930525     C     W98RCC        SUB       WNOCRF        W98NCC
461400070115     c
461500070115     c* non do ritardo se comunque la consegna reale cade nel giorno
461600070115     c*  della consegna richiesta se presente IL
461700070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
461800070521     c***                          wpodcm<=wpocr2 and w98ncc>0
461900070521     c***                clear                   w98ncc
462000070521     c***                endif
462100070115     c
462200930525     C                   ELSE                                                   <-- 3 -->
462300930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
462400930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
462500930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
462600930525     C                   END                                                    <-- 3 ---
462700100224     C                   END                                                    <-- 2a---
462800100224
462900100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
463000100224     c                   select
463100100224     c                   when      d98cca='2'
463200100224     c                   z-add     0             w98nci
463300100224
463400100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
463500100224     c*  e sto leggendo la bolla in arrivo
463600100224     c                   when      d98cca='1' and d98tbo='A'
463700100224     c                   z-add     0             w98nci
463800100224     c                   endsl
463900100224     c
464000100224     C                   END                                                    <-- 2 ---
464100070521     C                   END                                                    <-- 1 ---
464200930524     C*
464300930524     C* CONTROLLO CONFORMITA
464400930423     C     W98NCC        IFLE      *ZEROS
464500930524     C                   MOVEL     'S'           D98CCC
464600930420     C                   ELSE
464700930524     C                   MOVEL     'N'           D98CCC
464800930420     C                   END
464900930420     C*
465000930420     C*-------------------------------------------------------------------
465100930420     C* CONFORMITA' RESA INTERNA
465200930420     C*-------------------------------------------------------------------
465300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
465400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
465500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
465600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
465700050914     c                   if        consideraora='S'
465800050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
465900050914     c                   else
466000050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
466100050914     c                   endif
466200080116     c
466300080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
466400090827     c                   select
466500090827     c                   when      d98cca='2' and w98nri>0
466600080116     c                   z-add     0             w98nri
466700090827     c
466800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
466900080116     c*  e sto leggendo la bolla in arrivo
467000090827     c                   when      d98cca='1' and d98tbo='A' and
467100080116     c                                            w98nri>0
467200080116     c                   z-add     0             w98nri
467300110923     c
467400110923     c* Ignoro ritardo se Perfect Order
467500130221     c                   when      d98xco >*zeros
467600110923     c                   z-add     0             w98nri
467700110923     c
467800090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
467900090827     c* sono  in ritardo di 12 ore
468000091009     c                   when      d98tci=10  and
468100091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
468200091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
468300091014     c*
468400091014     c                             d98hci=1030
468500091014     C                   MOVEL     'OSC  '       WR3TAB
468600091014     C                   eval      WR3HHI=d98hmc
468700091014     C                   EXSR      RO3ORE
468800091014     c                   if        wr3matpom='M' and d98hmc>1030
468900131218     c                   exsr      ctrLAVH1030
469000131218
469100131218     c                   if        wlav1030<>'S'
469200091014     c                   add       12            w98nri
469300091014     c                   endif
469400131218     c                   endif
469500091014     c
469600091014     c***                z-add     12            w98nri
469700090827     c                   endsl
469800090827
469900930524     C                   ELSE                                                   <-- 1 -->
470000930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
470100930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
470200930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
470300931206     C     WPODCM        ANDLE     WPOCR1
470400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
470500930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
470600930525     C                   Z-ADD     WPODCM        WR2POI            9 0
470700931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
470800930525     C                   EXSR      RO2COV
470900930525     C     WR2NOV        IFNE      *ZEROS
471000930525     C     WR2NOV        MULT      -1            W98NRI
471100930525     C                   END
471200930524     C                   ELSE                                                   <-- 2 -->
471300930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
471400930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
471500930524     C     WPODCM        IFLT      WPOCRI
471600930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
471700930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
471800930524     C                   ELSE
471900931206     C                   Z-ADD     WPOCR1        WPORFI
472000930524     C                   Z-ADD     WPODCM        WPORFF
472100930524     C                   END
472200930524     C* CALCOLO ORE DI DIFFERENZA
472300930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
472400930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
472500930524     C                   Z-ADD     WPORFI        WR2POI            9 0
472600930524     C                   Z-ADD     WPORFF        WR2POF            9 0
472700930524     C                   EXSR      RO2COV
472800930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
472900050902     c
473000050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
473100100224     c                   if        d98cca='2' and w98nri>0
473200100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
473300080116     c***                z-sub     w98nri        w98nri
473400080116     c                   z-add     0             w98nri
473500050902     c                   endif
473600111018     c* Ignoro ritardo se pefect order
473700130221     c                   if        d98xco >*zeros
473800111018     c                   z-add     0             w98nri
473900111018     c                   endif
474000061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
474100061121     c*  e sto leggendo la bolla in arrivo
474200100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
474300100224     c***                if        d98cca='1' and d98tbo='A' and
474400100224     c***                                         w98nri>0 and wpodcm<wpocri
474500080116     c***                z-sub     w98nri        w98nri
474600080116     c                   z-add     0             w98nri
474700061121     c                   endif
474800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
474900070827     c*  mittente, --> consegnato in anticipo
475000070828     c                   if        d98tcr=' ' and d98nsp>0  and
475100070828     c                                            w98nri>0 and wpodcm<wpocri
475200070827     c                   if        CallAR5=' '
475300070827     c                   EXSR      LeggiAR5
475400070824     c                   endif
475500070827     c                   if        dcrpart<>'S'
475600070824     c                   z-sub     w98nri        w98nri
475700070824     c                   endif
475800070824     c                   endif
475900050907     c
476000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
476100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
476200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
476300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
476400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
476500050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
476600050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
476700050907     c                   z-sub     w98nri        w98nri
476800050907     c                   sub       oreILoDOPO    w98nri
476900050907     c                   endif
477000050907     c
477100050907     C                   END                                                    <-- 2 ---
477200930524     C                   END                                                    <-- 1 ---
477300930420     C* CALCOLO LE ORE DI RESA
477400930420     C* CONTROLLO CONFORMITA
477500930423     C     W98NRI        IFLE      *ZEROS
477600930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
477700930420     C                   ELSE
477800930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
477900930420     C                   END
478000930420     C*
478100930420     C*-------------------------------------------------------------------
478200930420     C* CONFORMITA' RESA CLIENTE
478300930420     C*-------------------------------------------------------------------
478400930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
478500930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
478600930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
478700050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
478800050914     c                   if        consideraora='S'
478900050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
479000050914     c                   else
479100050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
479200050914     c                   endif
479300090827     c
479400080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
479500090827     c                   select
479600090827     c                   when      d98cca='2' and w98nrc>0
479700080116     c                   z-add     0             w98nrc
479800090827
479900080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
480000080116     c*  e sto leggendo la bolla in arrivo
480100090827     c                   when      d98cca='1' and d98tbo='A' and
480200080116     c                                            w98nrc>0
480300080116     c                   z-add     0             w98nrc
480400110923     c
480500110923     c* Ignoro ritardo se Perfect Order
480600130221     c                   when      d98xco >*zeros
480700110923     c                   z-add     0             w98nrc
480800090827     c
480900090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
481000090827     c* sono  in ritardo di 12 ore
481100091009     c                   when      d98tcc=10  and
481200091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
481300091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
481400091014     c                             d98hce=1030
481500091014     C                   MOVEL     'OSC  '       WR3TAB
481600091014     C                   eval      WR3HHI=d98hmc
481700091014     C                   EXSR      RO3ORE
481800091014     c                   if        wr3matpom='M' and d98hmc>1030
481900131218     c                   exsr      ctrLAVH1030
482000131218
482100131218     c                   if        wlav1030<>'S'
482200091014     c                   add       12            w98nrc
482300091014     c                   endif
482400131218     c                   endif
482500091014     c
482600091014     c**                 z-add     12            w98nrc
482700090827     c                   endsl
482800090827     c
482900930524     C                   ELSE                                                   <-- 1 -->
483000930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
483100930524     C* DI DIFFERENZA SONO UGUALI A ZERO
483200930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
483300931206     C     WPODCM        ANDLE     WPOCR2
483400930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
483500930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
483600930525     C                   Z-ADD     WPODCM        WR2POI            9 0
483700931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
483800930525     C                   EXSR      RO2COV
483900930525     C     WR2NOV        IFNE      *ZEROS
484000930525     C     WR2NOV        MULT      -1            W98NRC
484100930525     C                   END
484200930524     C                   ELSE                                                   <-- 2 -->
484300930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
484400930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
484500930524     C     WPODCM        IFLT      WPOCRI
484600930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
484700930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
484800930524     C                   ELSE
484900931206     C                   Z-ADD     WPOCR2        WPORFI
485000930524     C                   Z-ADD     WPODCM        WPORFF
485100930524     C                   END
485200930524     C* CALCOLO ORE DI DIFFERENZA
485300930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
485400930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
485500930524     C                   Z-ADD     WPORFI        WR2POI            9 0
485600930524     C                   Z-ADD     WPORFF        WR2POF            9 0
485700930524     C                   EXSR      RO2COV
485800930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
485900050902     c
486000050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
486100100224     c                   if        d98cca='2' and w98nrc>0
486200100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
486300080116     c***                z-sub     w98nrc        w98nrc
486400080116     c                   z-add     0             w98nrc
486500050902     c                   endif
486600111018     c* Ignoro ritardo se pefect order
486700130221     c                   if        d98xco >*zeros
486800111018     c                   z-add     0             w98nrc
486900111018     c                   endif
487000070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
487100061121     c*  e sto leggendo la bolla in arrivo
487200100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
487300100224     c***                if        d98cca='1' and d98tbo='A' and
487400100224     c***                                         w98nrc>0 and wpodcm<wpocri
487500080116     c***                z-sub     w98nrc        w98nrc
487600080116     c                   z-add     0             w98nrc
487700061121     c                   endif
487800070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
487900070827     c*  mittente, --> consegnato in anticipo
488000070828     c                   if        d98tcr=' ' and d98nsp>0 and
488100070828     c                                            w98nrc>0 and wpodcm<wpocri
488200070827     c                   if        CallAR5=' '
488300070827     c                   EXSR      LeggiAR5
488400070827     c                   endif
488500070827     c                   if        dcrpart<>'S'
488600070824     c                   z-sub     w98nrc        w98nrc
488700070824     c                   endif
488800070824     c                   endif
488900050907     c
489000050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
489100050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
489200050907     c*  lo trasformo in anticipo perchè non sono in ritardo
489300050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
489400050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
489500050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
489600050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
489700050907     c                   z-sub     w98nrc        w98nrc
489800050907     c                   sub       oreILoDOPO    w98nrc
489900050907     c                   endif
490000050907     c
490100050907     C                   END                                                    <-- 2 ---
490200930524     C                   END                                                    <-- 1 ---
490300930420     C* CALCOLO LE ORE DI RESA
490400930420     C* CONTROLLO CONFORMITA
490500930423     C     W98NRC        IFLE      *ZEROS
490600930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
490700930420     C                   ELSE
490800930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
490900930420     C                   END
491000930420     C*
491100930420     C                   ENDSR
491200070827     c
491300131218     c********************************************************************
491400131218     c     CtrlavH1030   BEGSR
491500131218     c********************************************************************
491600131218     c                   clear                   wlav1030          1
491700140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
491800131218     c                   z-add     1             xx                4 0
491900131218     c                   dow       evc(xx)<>*blanks
492000140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
492100140127     c                   if        evd(xx)=d98dcm
492200131218     c                   if        evo(xx)<=1030
492300131218     c                   eval      wlav1030='S'
492400131218     c                   leave
492500131218     c                   endif
492600131218     c                   endif
492700131218     c                   add       1             xx
492800131218     c                   enddo
492900131218     c                   ENDSR
493000070827     c********************************************************************
493100070827     c* Controllo record GEN di FIAR5
493200070827     c********************************************************************
493300070827     c     LeggiAR5      BEGSR
493400070827     c                   clear                   DCRPART           1
493500141104     c                   clear                   dar5gen
493600141104     c                   clear                   gc1PART           2
493700141104     c                   clear                   gc2PART           2
493800070827     c     kar5          chain     fiar501l
493900070827    1c                   if        not %found
494000070827     c                   eval      dcrpart='S'
494100141104     c                   eval      gc1part=d98gc1
494200141104     c                   eval      gc2part=d98gc2
494300070827   x1c                   else
494400070827     c* i dati devono essere quelli immessi dalla partenza
494500070827     c                   movel     ar5uni        dar5gen
494600070827     c
494700070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
494800070827     c*  ma il record GEN è stato scritto per altri motivi
494900070827    2c                   if        §ar5mdp='S'
495000070827     c                   movel     d98dcr        alfadcr           8
495100141104     c                   eval      gc1part=§ar5gc1
495200141104     c                   eval      gc2part=§ar5gc2
495300070827     c
495400070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
495500070827     c                   eval      dcrpart='S'
495600070827    3c                   endif
495700070827     c
495800070827   x2c                   else
495900070827     c                   eval      dcrpart='S'
496000141106     c                   eval      gc1part=d98gc1
496100141106     c                   eval      gc2part=d98gc2
496200070827    2c                   endif
496300070827    1c                   endif
496400070827     c
496500140402     c* Memorizzo il flag pickup 2
496600140402     c                   eval      wpick2=§AR5PK2
496700140402     c
496800150401     c*
496900150414     c**                 if        d98cas>0 or d98ffd<>' ' or
497000150414     c**                           d98tc1='A'  or d98tc2='A' or
497100150414     c**                           d98tc1='S'  or d98tc2='S'
497200150401     c     kar4          chain     fiar401l
497300150401     c                   if        %found(fiar401l)
497400150401     c                   eval      dsbl4L=ar4not
497500150401     c                   endif
497600150401     c* Solo per fermo deposito verifico se inviato alert
497700150401     c                   if        d98ffd<>' '
497800150401     c                   clear                   fnlv82ds
497900150401     c                   eval      ILV82AAS=d98aas
498000150401     c                   eval      ILV82LNP=d98lnp
498100150401     c                   eval      ILV82NRS=d98nrs
498200150401     c                   eval      ILV82NSP=d98nsp
498300150401     c                   eval      ILV82TIPA='FD'
498400150401     c                   call      'FNLV82R'
498500150401     c                   parm                    fnlv82ds
498600150401     c                   endif
498700150414     c**                 endif
498800150401
498900150401     c                   eval      callar5='S'
499000150401
499100070827     c                   ENDSR
499200930913     C********************************************************************
499300930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
499400930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
499500930913     C*            DATA/ORA (FINE RANGE)
499600930913     C********************************************************************
499700930913     C     CALCNS        BEGSR
499800930914     C*
499900930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
500000930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
500100930914     C     WPODTI        IFEQ      *ZEROS
500200960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
500300930914     C                   ELSE
500400930914     C                   Z-ADD     WPODTI        WPOIRC
500500930914     C                   END
500600930914     C*
500700930914     C* SE NON ANCORA CONSEGNATA
500800930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
500900050922     c* in realtà nella data consegna c'e' la data fine calcolo
501000050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
501100930914     C     WPODCM        IFEQ      *ZEROS
501200050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
501300930914     C                   ELSE
501400930916     C                   Z-ADD     WPODCM        WPOFRC
501500930914     C                   END
501600930914     C*
501700930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
501800930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
501900930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
502000930914     C     WPODFC        ANDLT     WPOIRC
502100930914     C                   ELSE
502200930914     C*
502300930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
502400930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
502500930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
502600930914     C     WPODFC        ANDGT     WPOFRC
502700930914     C                   ELSE
502800930916     C*
502900930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
503000930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
503100930916     C     WPODFC        ANDGT     WPOFRC
503200930916     C                   ELSE
503300930916     C*
503400930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
503500930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
503600930916     C     WPODFC        ANDLT     WPOIRC
503700930916     C                   ELSE
503800930914     C*
503900930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
504000930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
504100930914     C     WPODIC        IFNE      *ZEROS
504200930914     C     WPODIC        ANDGT     WPOFRC
504300930914     C                   Z-ADD     WPOFRC        WPODIC
504400930914     C                   ENDIF
504500930914     C*
504600930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
504700930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
504800930914     C     WPODFC        IFGT      WPOFRC
504900930914     C                   Z-ADD     WPOFRC        WPODFC
505000930914     C                   ENDIF
505100930914     C*
505200930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
505300930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
505400930914     C     WPODIC        IFNE      *ZEROS
505500930914     C     WPODIC        ANDLT     WPOIRC
505600930914     C                   Z-ADD     WPOIRC        WPODIC
505700930914     C                   ENDIF
505800930914     C*
505900930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
506000930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
506100930914     C     WPODFC        IFLT      WPOIRC
506200930914     C                   Z-ADD     WPOIRC        WPODFC
506300930914     C                   ENDIF
506400930914     C*
506500930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
506600931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
506700930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
506800930916     C*
506900931210     C***********WPODIC    IFNE *ZEROS
507000931210     C***********WPODFC    ANDNE*ZEROS
507100931210     C*********************ADD  12        WPODFC
507200931210     C*********************ENDIF
507300930914     C     WPODIC        IFEQ      *ZEROS
507400930914     C     WPODFC        ANDNE     *ZEROS
507500931130     C                   Z-ADD     WPODFC        WPODIC
507600931210     C*********************ADD  12        WPODFC
507700930916     C                   ENDIF
507800930913     C*
507900930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
508000930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
508100930914     C* INTERNA
508200050908     c                   eval      Usacons='S'
508300930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
508400930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
508500931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
508600931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
508700940324     C                   EXSR      RO9COV
508800931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
508900930914     C* CLIENTE
509000930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
509100931210     C                   EXSR      RO9COV
509200931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
509300050908     c                   eval      Usacons=' '
509400930914     C*
509500930916     C                   ENDIF                                                  <-- 5 ---
509600930916     C                   ENDIF                                                  <-- 4 ---
509700930916     C                   ENDIF                                                  <-- 3 ---
509800930914     C                   ENDIF                                                  <-- 2 ---
509900930914     C                   ENDIF                                                  <-- 1 ---
510000930913     C*
510100930913     C                   ENDSR
510200930325     C********************************************************************
510300930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
510400930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
510500930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
510600930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
510700930325     C********************************************************************
510800930325     C     RO1CDP        BEGSR
510900930414     C*
511000931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
511100931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
511200931126     C*
511300931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
511400931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
511500150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
511600150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
511700931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
511800060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
511900930326     C*
512000930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
512100931130     C                   Z-ADD     WR1POI        WR1POO            9 0
512200930325     C*
512300930325     C* CICLO FINO A CHE:
512400930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
512500930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
512600930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
512700930406     C     WR1POO        ANDLT     WPOLIM
512800930414     C*
512900930407     C                   Z-ADD     WR1POO        WPOROU            9 0
513000930325     C                   MOVEL     'N'           WSLITT            1
513100930414     C*
513200930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
513300060420     C*
513400931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
513500150605     c     wdee          andeq     ' '
513600930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
513700930414     C                   END
513800930511     C*
513900931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
514000150608     c     wdee          andeq     ' '
514100930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
514200930511     C                   END
514300930414     C*
514400930325     C     WSLITT        IFEQ      'N'
514500930423     C                   SUB       12            WR1NOI            5 0
514600930325     C                   END
514700931130     C                   ADD       12            WR1POO            9 0
514800930414     C*
514900931223     C                   ENDDO                                                  <-- 1 ---
515000931126     C*
515100931126     C                   ELSE                                                   <-- 0 -->
515200931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
515300931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
515400931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
515500960129     C     WPOLAF        IFGT      WPODCG
515600960129     C                   Z-ADD     WPOLAF        WTM090            9 0
515700960129     C                   ELSE
515800960129     C                   Z-ADD     WPODCG        WTM090
515900960129     C                   ENDIF
516000960129     C     WPOGIF        IFGT      WTM090
516100960129     C                   Z-ADD     WPOGIF        WTM090
516200960129     C                   ENDIF
516300060420     C     WPOpaif       IFGT      WTM090
516400060420     C                   Z-ADD     WPOpaif       WTM090
516500060420     C                   ENDIF
516600960129     C*
516700960129     C     12            ADD       WTM090        WR1POO
516800960129     C                   ENDIF                                                  <-- 0 ---
516900930512     C*
517000931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
517100931202     C     WR1POO        SUB       12            WR7POI
517200931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
517300931202     C                   ADD       12            WR7POO
517400931202     C     WR7POO        IFGT      WR1POO
517500931202     C                   Z-ADD     WR7POO        WR1POO
517600931202     C                   END
517700930406     C*
517800930325     C                   ENDSR
517900930414     C********************************************************************
518000930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
518100930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
518200930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
518300930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
518400930414     C********************************************************************
518500930414     C     RO2COV        BEGSR
518600091014     c                   clear                   salvaEveL
518700091014     c                   clear                   salvaEveG
518800931130     C*
518900930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
519000930414     C                   Z-ADD     *ZEROS        WR2NOV
519100930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
519200931130     C                   Z-ADD     WR2POI        WR2POW
519300930414     C*
519400930414     C* CICLO FINO A CHE:
519500930414     C*   - DATA INIZIO SUPERATO DATA FINE
519600930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
519700931223     C     WR2POW        ANDLT     WPOLIM
519800930414     C*
519900930414     C                   Z-ADD     WR2POW        WPOROU
520000930414     C                   MOVEL     'N'           WSLITT            1
520100930414     C*
520200930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
520300930414     C*
520400930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
520500930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
520600930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
520700930414     C                   END
520800930414     C*
520900930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
521000930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
521100930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
521200930618     C                   END
521300060420     c
521400060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
521500060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
521600060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
521700060420     C                   END
521800060420     c
521900931201     C*
522000931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
522100931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
522200931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
522300931201     C                   END
522400060418     c
522500060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
522600060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
522700060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
522800060418     C                   END
522900930414     C*
523000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
523100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
523200930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
523300930414     C                   END
523400930511     C*
523500930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
523600930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
523700930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
523800930511     C                   END
523900930414     C*
524000930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
524100930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
524200930414     C     WSLITT        IFEQ      'N'
524300930423     C                   ADD       12            WR2NOV            5 0
524400930414     C                   END
524500931130     C                   ADD       12            WR2POW            9 0
524600930414     C*
524700931223     C                   ENDDO                                                  <-- 1 ---
524800930414     C*
524900930414     C                   ENDSR
525000941228     C********************************************************************
525100941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
525200941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
525300941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
525400941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
525500941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
525600941228     C********************************************************************
525700941228     C     RO2COP        BEGSR
525800941228     C*
525900941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
526000941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
526100941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
526200941228     C                   Z-ADD     *ZEROS        WR2NOV
526300941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
526400941228     C                   Z-ADD     WR2POI        WR2POW
526500941228     C*
526600941228     C* CICLO FINO A CHE:
526700941228     C*   - DATA INIZIO SUPERATO DATA FINE
526800941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
526900941228     C*
527000950215     C                   Z-ADD     WR2POW        WPOROU
527100941228     C                   MOVEL     'N'           WSLITT
527200941228     C* TRASFORMA IL PROGR. ORE IN DATA
527300950215     C*******              Z-ADDWR2POW    WORE
527400950215     C*******              EXSR OREDAT
527500950215     C*******              Z-ADDWDAT      W12DAT  80
527600950215     C*******              Z-ADDWRES      W12RES  30
527700941228     C*
527800941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
527900941228     C*
528000941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
528100941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
528200941228     C     WSLITT        IFEQ      'N'
528300941228     C                   ADD       12            WR2NOV            5 0
528400941228     C                   END
528500941228     C                   ADD       12            WR2POW            9 0
528600941228     C*
528700941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
528800941228     C* ENTRI IN LOOP
528900941228     C     WR2NOV        IFGT      999
529000941228     C                   LEAVE                                                  *VA A ENDDO
529100941228     C                   END
529200941228     C*
529300941228     C                   ENDDO                                                  <-- 1 ---
529400941228     C*
529500941228     C                   ENDSR
529600930510     C********************************************************************
529700930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
529800930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
529900930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
530000930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
530100050905     c*                     il num ore deve essere un multiplo di 12
530200050905     c*                     oppure 1 ora
530300930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
530400930510     C********************************************************************
530500930510     C     RO4CDP        BEGSR
530600930510     C*
530700930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
530800930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
530900050905     c                   movel     '1'           wgiro             1
531000930510     C*
531100930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
531200930510     C                   Z-ADD     WR4POI        WR4POO            9 0
531300930510     C*
531400930510     C* CICLO FINO A CHE:
531500930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
531600930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
531700930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
531800930510     C     WR4POO        ANDLT     WPOLIM
531900930510     C*
532000141106     c                   if        nosomma<>'N'
532100931130     C                   ADD       12            WR4POO            9 0
532200141106     c                   endif
532300930510     C                   Z-ADD     WR4POO        WPOROU            9 0
532400930510     C                   MOVEL     'N'           WSLITT            1
532500930510     C*
532600930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
532700050905     c
532800050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
532900930510     C     WSLITT        IFEQ      'N'
533000050905     c
533100050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
533200050905     c*  se cado in giorno non festivo
533300050905     c                   if        wr4noi=1 and wgiro='1'
533400050905     c                   sub       12            wr4poo
533500050905     c                   add       wr4noi        wr4poo
533600050905     c                   endif
533700050905     c*
533800050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
533900050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
534000050905     c*  calcolate dal pgm
534100050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
534200050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
534300050905     c*  del giorno sucessivo non festivo
534400050905     c                   if        wr4noi=1 and wgiro<>'1'
534500050905     c                   add       wr4noi        wr4poo
534600050905     c                   endif
534700050905     c
534800930510     C                   SUB       12            WR4NOI            5 0
534900930510     C                   END
535000141106     c
535100141106     c                   if        nosomma= 'N'
535200141106     C                   ADD       12            WR4POO            9 0
535300141106     c                   endif
535400930510     C*
535500050905     c                   eval      wgiro='2'
535600931223     C                   ENDDO                                                  <-- 1 ---
535700930510     C*
535800930510     C                   ENDSR
535900931130     C********************************************************************
536000931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
536100931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
536200931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
536300931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
536400931130     C********************************************************************
536500931130     C     RO5COV        BEGSR
536600931130     C*
536700931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
536800060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
536900060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
537000060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
537100931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
537200931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
537300931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
537400931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
537500931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
537600931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
537700931130     C                   Z-ADD     *ZEROS        WR5NOV
537800940714     C*
537900940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
538000940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
538100931207     C*
538200931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
538300940908     C     WIC           IFEQ      'I'
538400090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
538500090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
538600931207     C                   ELSE
538700090827     C*****              Z-ADD     D98TCC        WPOTCT
538800090827     C                   Z-ADD     w99TCC        WPOTCT
538900931207     C                   END
539000931130     C*
539100931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
539200060420     C                   clear                   COV
539300931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
539400931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
539500931207     C                   Z-ADD     2             CO                3 0
539600931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
539700931207     C                   EXSR      RO5SUB
539800981102     C                   DO        99            IC                3 0
539900931207     C     LAF(IC)       IFNE      *ZEROS
540000931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
540100931207     C                   EXSR      RO5SUB
540200931207     C                   ENDIF
540300931207     C                   ENDDO
540400060420     c
540500060420     C                   DO        99            IC                3 0
540600060420     C     paif(IC)      IFNE      *ZEROS
540700060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
540800060420     C                   EXSR      RO5SUB
540900060420     C                   ENDIF
541000060420     C                   ENDDO
541100931207     C* ORDINA LA SCHIERA COV
541200931207     C                   SORTA     COV
541300931130     C*
541400931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
541500931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
541600981102     C                   DO        98            IC                             --- 1 -->
541700931207     C     IC            ADD       1             I1C               3 0
541800931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
541900931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
542000931207     C     COV(I1C)      OREQ      *ZEROS
542100931207     C                   ELSE
542200931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
542300931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
542400931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
542500931130     C                   EXSR      RO2COV
542600931207     C                   ADD       WR2NOV        WR5NOV            9 0
542700931209     C* SE NON SONO IN DATA CONSEGNA
542800931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
542900931207     C     WR5NOV        IFLT      WPOTCT
543000931209     C     COV(I1C)      ANDNE     WPODCM
5431009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5432009709111009 C     WPOTCT        SUB       12            WR5NOV
543300931130     C                   END
543400931207     C                   ENDIF                                                  <-- 2 ---
543500931207     C                   ENDDO                                                  <-- 1 ---
543600940714     C                   ENDIF                                                  <-- 0 ---
543700931130     C*
543800931130     C                   ENDSR
543900931207     C********************************************************************
544000931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
544100931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
544200931207     C********************************************************************
544300931207     C     RO5SUB        BEGSR
544400931207     C*
544500931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
544600931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
544700931207     C     WR5COV        ANDLT     WR5POF
544800931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
544900931207     C                   MOVEL     'N'           WR5PRE            1
545000981102     C*---                 DO   30        I
545100981102     C                   DO        99            I
545200931207     C     WR5COV        IFEQ      COV(I)
545300931207     C                   MOVEL     'S'           WR5PRE
545400931207     C                   LEAVE
545500931207     C                   ENDIF
545600931207     C                   ENDDO
545700931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
545800931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
545900931207     C                   ADD       1             CO                3 0
546000931207     C                   Z-ADD     WR5COV        COV(CO)
546100931207     C                   ENDIF                                                  <-- 2 ---
546200931207     C                   ENDIF                                                  <-- 1 ---
546300931207     C*
546400931207     C                   ENDSR
546500931201     C********************************************************************
546600060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
546700931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
546800931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
546900931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
547000931201     C********************************************************************
547100931201     C     RO6CDT        BEGSR
547200931201     C*
547300931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
547400931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
547500931201     C*
547600931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
547700931201     C                   Z-ADD     WR6POI        WR6POO            9 0
547800931201     C*
547900931201     C* CICLO FINO A CHE:
548000931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
548100931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
548200931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
548300931201     C     WR6POO        ANDLT     WPOLIM
548400931201     C*
548500931201     C                   Z-ADD     WR6POO        WPOROU            9 0
548600931201     C                   MOVEL     'N'           WSLITT            1
548700931201     C*
548800931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
548900931201     C*
549000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
549100150605     c     wdee          andeq     ' '
549200931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
549300931201     C                   END
549400931201     C*
549500931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
549600150608     c     wdee          andeq     ' '
549700931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
549800931201     C                   END
549900931201     C*
550000931201     C     WSLITT        IFEQ      'N'
550100931201     C                   SUB       12            WR6NOI            5 0
550200931201     C                   END
550300931201     C                   ADD       12            WR6POO            9 0
550400931201     C*
550500931223     C                   ENDDO                                                  <-- 1 ---
550600931201     C*
550700931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
550800931202     C     WR6POO        SUB       12            WR7POI
550900931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
551000931207     C                   ADD       12            WR7POO
551100931202     C     WR7POO        IFGT      WR6POO
551200931201     C                   Z-ADD     WR7POO        WR6POO
551300931202     C                   END
551400931201     C*
551500931201     C                   ENDSR
551600931201     C********************************************************************
551700060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
551800060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
551900060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
552000060407     C*            INPUT:  -WR7POI (PROGR.ORE)
552100931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
552200931201     C********************************************************************
552300931201     C     RO7TCF        BEGSR
552400060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
552500060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
552600060407     C                   MOVEL     'N'           WSLITT
552700931201     C*
552800931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
552900931201     C                   Z-ADD     WR7POI        WR7POO            9 0
553000931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
553100931201     C* COINCIDA CON UN TURNO DI CHIUSURA
553200060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
553300931201     C     D98GC2        ANDEQ     *BLANKS
553400060407   x1C                   ELSE                                                   <-- 1 -->
553500931214     C*
553600931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
553700931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
553800931214     C*
553900931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
554000931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
554100931223     C     WPOROU        ANDLT     WPOLIM
554200931214     C* MEMORIZZO INIZIO E FINE RANGE
554300931202     C     WPOTI         IFEQ      *ZEROS
554400931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
554500931202     C                   END
554600931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
554700931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
554800931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
554900931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
555000931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
555100931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
555200931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
555300931201     C                   MOVEL     'N'           WSLITT
555400931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
555500060407     c* Se non trovat, controllo se c'e' evento part che giustifica
555600060407     c                   if        wslitt='N'
555700060407     c                   exsr      datcepg
555800060407     c                   endif
555900060407     c
556000931201     C                   ENDDO                                                  <-- 2 ---
556100931214     C*
556200931201     C                   Z-ADD     WPOROU        WR7POO
556300931214     C*
556400931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
556500931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
556600931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
556700931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
556800931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
556900931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
557000931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
557100931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
557200931209     C                   ELSE                                                   <-- 3 -->
557300931209     C     WIC           IFEQ      'I'
557400931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
557500931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
557600931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
557700931201     C                   ELSE
557800931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
557900931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
558000931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
558100931201     C                   ENDIF
558200931209     C                   ENDIF                                                  <-- 3 ---
558300931209     C                   ENDIF                                                  <-- 2 ---
558400931214     C*
558500060407    1C                   ENDIF                                                  <-- 1 ---
558600060407     c
558700060407     c* Se non trovato turno di chiusura, verifico se c'e'
558800060407     c*  evento particolare che GIUSTIFICA
558900060407     c                   if        wpoti=0
559000060418     C                   EXSR      RO7CEPG
559100060407     c                   endif
559200931201     C*
559300931201     C                   ENDSR
559400060406     C********************************************************************
559500060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
559600060406     c*                        che giustifica
559700060407     C*            INPUT:-WR7POI (PROGR.ORE)
559800060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
559900060406     C********************************************************************
560000060407     C     RO7CEPG       BEGSR
560100060406     C*
560200060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
560300060407     C                   Z-ADD     WR7POI        WR7POO            9 0
560400060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
560500060406     C* coincide
560600060406    1C                   if        wevpag='S'                                   <-- 1 -->
560700060406     C*
560800060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
560900060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
561000060406     C                   MOVEL     'N'           WSLITT
561100060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
561200060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
561300060406     C*
561400060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
561500060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
561600060406     C     WPOROU        ANDLT     WPOLIM
561700060406     C* MEMORIZZO INIZIO E FINE RANGE
561800060406    3C     WPOTI         IFEQ      *ZEROS
561900060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
562000060406    3C                   END
562100060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
562200060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
562300060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
562400060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
562500060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
562600060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
562700060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
562800060406     C                   MOVEL     'N'           WSLITT
562900060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
563000060406     C* Se non trovato,
563100060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
563200060406    3c                   if        wslitt='N'
563300060406     c                   exsr      DATTUC
563400060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
563500060406    2C                   ENDDO                                                  <-- 2 ---
563600060406     C*
563700060407     C                   Z-ADD     WPOROU        WR7POO
563800060406     C*
563900060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
564000060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
564100060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
564200060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
564300060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
564400060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
564500060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
564600060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
564700060406   x3C                   ELSE                                                   <-- 3 -->
564800060406    4C     WIC           IFEQ      'I'
564900060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
565000060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
565100060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
565200060406   x4C                   ELSE
565300060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
565400060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
565500060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
565600060406    4C                   ENDIF
565700060406    3C                   ENDIF                                                  <-- 3 ---
565800060406    2C                   ENDIF                                                  <-- 2 ---
565900060406     C*
566000060406    1C                   ENDIF                                                  <-- 1 ---
566100060406     C*
566200060406     C                   ENDSR
566300931207     C********************************************************************
566400931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
566500931207     C*            INPUT:  -WR8POI (PROGR.ORE)
566600931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
566700931207     C********************************************************************
566800931207     C     RO8FES        BEGSR
566900130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
567000130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
567100931207     C*
567200931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
567300130411     C     WR8POI        SUB       12            WR8POO            9 0
567400931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
567500931207     C                   MOVEL     'N'           WSLITT
567600130411     c
567700130411     c* Non salto i festivi ???
567800130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
567900130411     c
568000931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
568100130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
568200130411     C     WPOROU        ANDGT     WPODSP
568300130411     C                   SUB       12            WPOROU
568400130411     C                   MOVEL     'N'           WSLITT
568500130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
568600130411     C                   ENDDO                                                  <-- 2 ---
568700130411     c
568800130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
568900130411     C     D98GC2        ANDEQ     *BLANKS
569000130411   x1C                   ELSE                                                   <-- 1 -->
569100130411     c* eseguo controllo per i turni di chiusura
569200130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
569300130411     C*
569400130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
569500130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
569600130411     C     WPOROU        ANDgT     WPOdsp
569700130411     C* MEMORIZZO INIZIO E FINE RANGE
569800130411     C     WPOTf         IFEQ      *ZEROS
569900130411     C                   z-add     wporou        WPOTf
570000130411     C                   END
570100140826     C     WPOROU        SUB       wnoadd        WPOROU
570200130411     c
570300130411     C                   MOVEL     'N'           WSLITT
570400130411     c                   exsr      dattuc
570500130411     c                   if        wslitt='N'
570600130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
570700140826     c* Se slitta per festivo --> tolgo sempre 12 ore
570800140826     c                   z-add     12            WNOADD
570900130411     c                   endif
571000130411     C* SLITTA
571100140826     C     wporou        add       12            WPOTi
571200130411    2C                   ENDDO                                                  <-- 2 ---
571300130411     c
571400130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
571500130411     c
571600130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
571700130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
571800130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
571900130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
572000130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
572100130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
572200130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
572300130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
572400130411   x3C                   ELSE                                                   <-- 3 -->
572500130411    4C     WIC           IFEQ      'I'
572600130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
572700130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
572800130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
572900130411   x4C                   ELSE
573000130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
573100130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
573200130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
573300130411    4C                   ENDIF
573400130411    3C                   ENDIF                                                  <-- 3 ---
573500130411    2C                   ENDIF                                                  <-- 3 ---
573600130411     C*
573700130411    1C                   ENDIF                                                  <-- 1 ---
573800130411
573900130411     C**   WPOROU        ADD       12            WR8POO
574000130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
574100130411     c                   z-add     wr8poi        wr8poo
574200931207     C*
574300931207     C                   ENDSR
574400931210     C********************************************************************
574500931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
574600940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
574700931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
574800931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
574900931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
575000931210     C********************************************************************
575100931210     C     RO9COV        BEGSR
575200931210     C*
575300931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
575400931210     C                   Z-ADD     *ZEROS        WR9NOV
575500931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
575600931210     C                   Z-ADD     WR9POI        WR9POW
575700931210     C* CICLO FINO A CHE:
575800931210     C*   - DATA INIZIO SUPERATO DATA FINE
575900931223     C*   - RAGGIUNTO LIMITE MASSIMO
576000931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
576100931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
576200931223     C     WR9POW        ANDLT     WPOLIM
576300931210     C*
576400931210     C                   Z-ADD     WR9POW        WPOROU
576500931210     C                   MOVEL     'N'           WSLITT            1
576600940324     C* FESTIVI
576700931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
576800940324     C* GIACENZA
576900931214     C     WSLITT        IFEQ      'N'
577000941229£$   C     WPOAGC        IFNE      *ZEROS
577100940324£$   C     WPOCGC        ORNE      *ZEROS
577200941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
577300940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
577400940324£$   C                   MOVEL     'S'           WSLITT
577500940324£$   C                   END
577600940324£$   C                   END
577700940324£$   C                   END
577800960129     C* EVENTI GIACENZA
577900960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
578000960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
578100960129     C                   Z-ADD     *ZEROS        G                 3 0
578200960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
578300960129     C     G             ANDLT     IG
578400960129     C                   ADD       1             G
578500960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
578600960209     C     WPOROU        ANDLE     GCF(G)
578700960129     C                   MOVEL     'S'           WSLITT
578800960129     C                   ENDIF                                                  <-- 3 ---
578900960129     C                   ENDDO                                                  <-- 2 ---
579000960129     C                   ENDIF                                                  <-- 1 ---
579100960129     C                   ENDIF                                                  <-- 0 ---
579200940324     C* LASCIATO AVVISO
579300931214     C     WSLITT        IFEQ      'N'
579400941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
579500940324£$   C                   Z-ADD     *ZEROS        L                 3 0
579600940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
579700940324£$   C     L             ANDLT     IL
579800940324£$   C                   ADD       1             L
579900941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
580000941229£$   C     WPOROU        ANDLE     LCF(L)
580100050520     c     LCon(L)       andeq     'S'
580200940324£$   C                   MOVEL     'S'           WSLITT
580300940324£$   C                   END                                                    <-- 3 ---
580400940324£$   C                   ENDDO                                                  <-- 2 ---
580500940324£$   C                   END                                                    <-- 1 ---
580600931210     C                   END
580700940324     C* TURNO CHIUSURA
580800140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
580900140826     c*  di sistemare le ore da sommare 12 o 24...
581000931214     C     WSLITT        IFEQ      'N'
581100931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
581200931210     C                   END
581300940324     C* DATA CONSEGNA RICHIESTA
581400050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
581500050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
581600050907     c
581700931214     C     WSLITT        IFEQ      'N'
581800050907     c                   add       1             wporou
581900940324£$   C     WPOCRI        IFNE      *ZEROS
582000940324£$   C     WPOROU        IFLT      WPOCRC
582100940324£$   C                   MOVEL     'S'           WSLITT
582200940324£$   C                   END
582300940324£4   C                   END
582400050907     c                   sub       1             wporou
582500931210     C                   END
582600931210     C*
582700931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
582800931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
582900931210     C     WSLITT        IFEQ      'N'
583000931210     C                   ADD       12            WR9NOV            5 0
583100931210     C                   END
583200931210     C                   ADD       12            WR9POW            9 0
583300931210     C*
583400931223     C                   ENDDO                                                  <-- 1 ---
583500931210     C*
583600931210     C                   ENDSR
583700940628%%%  C********************************************************************
583800940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
583900940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
584000940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
584100940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
584200940628%%%  C********************************************************************
584300940628%%%  C     RO0GES        BEGSR
584400940628%%%  C*
584500940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
584600940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
584700940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
584800940628%%%  C*
584900940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
585000940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
585100940628%%%  C*
585200940628%%%  C* CICLO FINO A CHE:
585300940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
585400940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
585500940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
585600050922%%%  C     WR0POO        ANDGe     WPODSP
585700940628%%%  C*
585800940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
585900940628%%%  C                   MOVEL     'N'           WSLITT            1
586000940628%%%  C*
586100940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
586200940628%%%  C*
586300940628%%%  C     WSLITT        IFEQ      'N'
586400940628%%%  C                   SUB       12            WR0NOI            5 0
586500940628%%%  C                   END
586600940628%%%  C                   SUB       12            WR0POO            9 0
586700940628%%%  C*
586800940628%%%  C                   ENDDO                                                  <-- 1 ---
586900940628%%%  C*
587000940628%%%  C                   ADD       12            WR0POO            9 0
587100940628%%%  C*
587200940628%%%  C                   ENDSR
587300960911     C********************************************************************
587400960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
587500960911     C*            INPUT:  -W10POI (PROGR.ORE)
587600960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
587700960911     C********************************************************************
587800960911     C     R10FES        BEGSR
587900960911     C*
588000960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
588100960911     C                   Z-ADD     W10POI        W10POO            9 0
588200960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
588300960911     C                   MOVEL     'N'           WSLITT
588400960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
588500960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
588600960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
588700960911     C                   ADD       12            WPOROU
588800960911     C                   MOVEL     'N'           WSLITT
588900960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
589000960911     C                   ENDDO                                                  <-- 2 ---
589100960911     C                   Z-ADD     WPOROU        W10POO
589200960911     C*
589300960911     C                   ENDSR
589400930325     C********************************************************************
589500930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
589600930325     C********************************************************************
589700930325     C     CHKGIA        BEGSR
589800930406     C*
589900960129     C* APERTURA/CHIUSURA GIACENZA
590000930326     C     WPODAG        IFNE      *ZEROS
590100930406     C     WPODCG        ORNE      *ZEROS
590200930407     C     WPOROU        IFGE      WPODAG
590300930407     C     WPOROU        ANDLE     WPODCG
590400930325     C                   MOVEL     'S'           WSLITT
590500930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
590600930325     C                   END
590700930326     C                   END
590800960129     C*
590900960129     C* EVENTI GIACENZA
591000960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
591100960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
591200960129     C                   Z-ADD     *ZEROS        G                 3 0
591300960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
591400960129     C     G             ANDLT     IG
591500960129     C                   ADD       1             G
591600960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
591700960129     C     WPOROU        ANDLE     GIF(G)
591800091009     c
591900091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
592000091009     c*  l'orario dell'evento
592100091009     c                   select
592200091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
592300091009     c* Se orario entro le 10.30  va bene
592400091014     c*  solo la prima volta per evento cioè quando controllo
592500091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
592600091014     c*  e non devo dare errore
592700091014     C                   MOVEL     'OSC  '       WR3TAB
592800091014     C                   eval      WR3HHI=evo(gip(G))
592900091014     C                   EXSR      RO3ORE
593000091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
593100091014     c*  in ritardo come per "E")
593200091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
593300091014     c                              wr3matpom='P'
593400091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
593500091009     C                   MOVEL     'S'           WSLITT
593600091009     c                   endif
593700091014     c                   eval      SalvaEveG=gip(G)
593800091009     c
593900091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
594000091009     c* Se orario entro le 10.30  va bene
594100091014     c*  solo la prima volta per evento cioè quando controllo
594200091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
594300091014     c*  e non devo dare errore
594400091014     C                   MOVEL     'OSC  '       WR3TAB
594500091014     C                   eval      WR3HHI=evo(gip(G))
594600091014     C                   EXSR      RO3ORE
594700091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
594800091014     c*  in ritardo come per "E")
594900091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
595000091014     c                              wr3matpom='P'
595100091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
595200091009     C                   MOVEL     'S'           WSLITT
595300091009     c                   endif
595400091014     c                   eval      SalvaEveG=gip(G)
595500091009     c
595600091009     c                   other
595700960129     C                   MOVEL     'S'           WSLITT
595800960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
595900091009     c                   endsl
596000091009
596100960129     C                   ENDIF                                                  <-- 3 ---
596200960129     C                   ENDDO                                                  <-- 2 ---
596300960129     C                   ENDIF                                                  <-- 1 ---
596400960129     C                   ENDIF                                                  <-- 0 ---
596500930406     C*
596600930325     C                   ENDSR
596700060420     C********************************************************************
596800930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
596900930325     C********************************************************************
597000930325     C     CHKLAV        BEGSR
597100930325     C*
597200941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
597300930421     C                   Z-ADD     *ZEROS        L                 3 0
597400930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
597500930421     C     L             ANDLT     IL
597600930421     C                   ADD       1             L
597700930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
597800930421     C     WPOROU        ANDLE     LAF(L)
597900090928     c
598000090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
598100091009     c*  l'orario dell'evento
598200091009     c                   select
598300091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
598400091009     c* Se orario entro le 10.30  va bene
598500091014     c*  solo la prima volta per evento cioè quando controllo
598600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
598700091014     c*  e non devo dare errore
598800091014     C                   MOVEL     'OSC  '       WR3TAB
598900091014     C                   eval      WR3HHI=evo(lap(L))
599000091014     C                   EXSR      RO3ORE
599100091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
599200091014     c*  in ritardo come per "E")
599300091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
599400091014     c                              wr3matpom='P'
599500091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
599600091009     C                   MOVEL     'S'           WSLITT
599700091009     c                   endif
599800091014     c                   eval      SalvaEveL=lap(L)
599900091009     c
600000091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
600100091009     c* Se orario entro le 10.30  va bene
600200091014     c*  solo la prima volta per evento cioè quando controllo
600300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
600400091014     c*  e non devo dare errore
600500091014     C                   MOVEL     'OSC  '       WR3TAB
600600091014     C                   eval      WR3HHI=evo(lap(L))
600700091014     C                   EXSR      RO3ORE
600800091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
600900091014     c*  in ritardo come per "E")
601000091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
601100091014     c                              wr3matpom='P'
601200091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
601300091009     C                   MOVEL     'S'           WSLITT
601400091009     c                   endif
601500091014     c                   eval      SalvaEveL=lap(L)
601600090928     c
601700091009     c                   other
601800930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
601900091009     C                   MOVEL     'S'           WSLITT
602000091009     c                   endsl
602100090928     c
602200930421     C                   END                                                    <-- 3 ---
602300931223     C                   ENDDO                                                  <-- 2 ---
602400930421     C                   END                                                    <-- 1 ---
602500930325     C*
602600930325     C                   ENDSR
602700060420     C********************************************************************
602800060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
602900060420     c*                   che Allungano
603000060420     C********************************************************************
603100060420     C     CHKCEPA       BEGSR
603200060420     C*
603300060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
603400060420     C                   Z-ADD     *ZEROS        p                 3 0
603500060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
603600060420     C     p             ANDLT     IpA
603700060420     C                   ADD       1             P
603800060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
603900060420     C     WPOROU        ANDLE     paif(P)
604000060420     C                   MOVEL     'S'           WSLITT
604100060420     C                   ADD       12            W98EA
604200060420     C                   END                                                    <-- 3 ---
604300060420     C                   ENDDO                                                  <-- 2 ---
604400060420     C                   END                                                    <-- 1 ---
604500060420     C*
604600060420     C                   ENDSR
604700930414     C********************************************************************
604800930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
604900930414     C********************************************************************
605000930414     C     CHKCNT        BEGSR
605100930511     C*
605200930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
605300930611     C     WIC           IFEQ      'I'                                          *INTERNO
605400930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
605500930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
605600930611     C                   ELSE
605700930611     C     WPOROU        IFGE      WPOAII
605800930611     C     WPOROU        ANDLE     WPOAFI
605900930611     C                   MOVEL     'S'           WSLITT
606000930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
606100930611     C                   END
606200930611     C                   END
606300930611     C                   ELSE                                                   *CLIENTE
606400930611     C     WPOAIC        IFEQ      *ZEROS
606500930611     C     WPOAFC        ANDEQ     *ZEROS
606600930611     C                   ELSE
606700930611     C     WPOROU        IFGE      WPOAIC
606800930611     C     WPOROU        ANDLE     WPOAFC
606900930611     C                   MOVEL     'S'           WSLITT
607000930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
607100930611     C                   END
607200930611     C                   END
607300930611     C                   END
607400930414     C*
607500930414     C                   ENDSR
607600930511     C********************************************************************
607700930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
607800930511     C********************************************************************
607900930511     C     CHKFED        BEGSR
608000930511     C*
608100930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
608200930611     C     WIC           IFEQ      'I'                                          *INTERNO
608300930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
608400930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
608500930611     C                   ELSE
608600930611     C     WPOROU        IFGE      WPOFII
608700930611     C     WPOROU        ANDLE     WPOFFI
608800930611     C                   MOVEL     'S'           WSLITT
608900930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
609000930611     C                   END
609100930611     C                   END
609200930611     C                   ELSE                                                   *CLIENTE
609300930611     C     WPOFIC        IFEQ      *ZEROS
609400930611     C     WPOFFC        ANDEQ     *ZEROS
609500930611     C                   ELSE
609600930611     C     WPOROU        IFGE      WPOFIC
609700930611     C     WPOROU        ANDLE     WPOFFC
609800930611     C                   MOVEL     'S'           WSLITT
609900930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
610000930611     C                   END
610100930611     C                   END
610200930611     C                   END
610300930511     C*
610400930511     C                   ENDSR
610500930325     C********************************************************************
610600930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
610700930325     C********************************************************************
610800930325     C     CHKFES        BEGSR
610900060526     c* se già passata la data, non controllo wporou
611000060526     c                   if        wpodata>0
611100060526     c                   z-add     wpodata       w12dat
611200060526     c                   clear                   w12res
611300060526     c                   else
611400931130     C*
611500931130     C* TRASFORMA IL  PROGR.ORE IN DATA
611600931130     C                   Z-ADD     WPOROU        WORE
611700931130     C                   EXSR      OREDAT
611800941227     C                   Z-ADD     WDAT          W12DAT            8 0
611900931130     C                   Z-ADD     WRES          W12RES            3 0
612000060526     c                   endif
612100930325     C*
612200941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
612300930325     C* E' FESTIVO
612400090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
612500090108     C**                 Z-ADD     D98LNP        CLNTFP
612600090108     C**                 Z-ADD     *ZEROS        CLNTFA
612700090108     C**                 ENDIF
612800090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
612900090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
613000090108     c**   è possibile rendere festivo in terminal per un altro terminal
613100090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
613200930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
613300090108     C     WTC           oreq      'P'                                          *TRAZIONE
613400941227     C                   Z-ADD     D98TFP        CLNTFP
613500941227     C                   Z-ADD     D98TFA        CLNTFA
613600941228     C                   ENDIF
613700941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
613800941227     C                   Z-ADD     *ZEROS        CLNTFP
613900941227     C                   Z-ADD     D98LNA        CLNTFA
614000941228     C                   ENDIF
614100941227     C                   Z-ADD     W12DAT        DSDAT8
614200941227     C                   Z-ADD     DSAAS         CLNANN
614300941227     C                   Z-ADD     DSMES         CLNMES
614400930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
614500930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
614600930421     C                   Z-ADD     DSGIO         G
614700930415     C     W12RES        IFEQ      *ZEROS
614800941227     C                   MOVEL     CLNMAT        GFEDS
614900930415     C                   ELSE
615000941227     C                   MOVEL     CLNPOM        GFEDS
615100930415     C                   END
615200930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
615300930405     C                   MOVEL     *BLANKS       WC1Y
615400930421     C                   MOVEL     GFE(G)        WC1Y              8
615500930421     C                   Z-ADD     1             I
615600930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
615700930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
615800930405     C     WIC           IFEQ      'I'
615900930421     C     I1Y(I)        ANDEQ     'S'
616000930405     C                   MOVEL     'S'           WSLITT
616100930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
616200930405     C                   END
616300930405     C     WIC           IFEQ      'C'
616400930421     C     E1Y(I)        ANDEQ     'S'
616500930405     C                   MOVEL     'S'           WSLITT
616600930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
616700930405     C                   END
616800970204     C     WIC           IFEQ      'X'
616900970204     C     E1Y(I)        ANDEQ     'S'
617000970204     C                   MOVEL     'S'           WSLITT
617100970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
617200970204     C                   END
617300930405     C                   ELSE                                                   <-- 3 -->
617400930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
617500930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
617600930405     C                   END                                                    <-- 3 ---
617700930405     C                   END                                                    <-- 2 ---
617800930405     C                   END                                                    <-- 1 ---
617900930325     C*
618000060526     c                   clear                   wpodata
618100930325     C                   ENDSR
618200931201     C********************************************************************
618300931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
618400931201     C********************************************************************
618500931201     C     CHKTUC        BEGSR
618600931201     C*
618700931201     C     WIC           IFEQ      'I'                                          --- 1 -->
618800931201     C                   DO        TI            T                 3 0          --- 2 -->
618900931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
619000931201     C     WPOROU        ANDLE     TIF(T)
619100931201     C                   MOVEL     'S'           WSLITT
619200931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
619300931201     C                   LEAVE                                                  *VA A FINE DO
619400931201     C                   ENDIF                                                  <-- 3 ---
619500931201     C                   ENDDO                                                  <-- 2 ---
619600931201     C                   ELSE                                                   <-- 1 -->
619700931201     C                   DO        TC            T                 3 0          --- 2 -->
619800931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
619900931201     C     WPOROU        ANDLE     TCF(T)
620000931201     C                   MOVEL     'S'           WSLITT
620100931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
620200931201     C                   LEAVE                                                  *VA A FINE DO
620300931201     C                   ENDIF                                                  <-- 3 ---
620400931201     C                   ENDDO                                                  <-- 2 ---
620500931201     C                   ENDIF                                                  <-- 1 ---
620600931201     C*
620700931201     C                   ENDSR
620800060418     C********************************************************************
620900060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
621000060418     c*          - eventi particolari che giustificano
621100060418     C********************************************************************
621200060418     C     CHKCEPG       BEGSR
621300060418     C*
621400060418     C     WIC           IFEQ      'I'                                          --- 1 -->
621500060420     C                   DO        PGI           p                 3 0          --- 2 -->
621600060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
621700060420     C     WPOROU        ANDLE     PGIF(p)
621800060418     C                   MOVEL     'S'           WSLITT
621900060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
622000060418     C                   LEAVE                                                  *VA A FINE DO
622100060418     C                   ENDIF                                                  <-- 3 ---
622200060418     C                   ENDDO                                                  <-- 2 ---
622300060418     C                   ELSE                                                   <-- 1 -->
622400060420     C                   DO        PGC           p                 3 0          --- 2 -->
622500060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
622600060420     C     WPOROU        ANDLE     PGCF(p)
622700060418     C                   MOVEL     'S'           WSLITT
622800060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
622900060418     C                   LEAVE                                                  *VA A FINE DO
623000060418     C                   ENDIF                                                  <-- 3 ---
623100060418     C                   ENDDO                                                  <-- 2 ---
623200060418     C                   ENDIF                                                  <-- 1 ---
623300060418     C*
623400060418     C                   ENDSR
623500930512     C********************************************************************
623600931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
623700930512     C********************************************************************
623800931130     C     DATTUC        BEGSR
623900931214     C*
624000931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
624100931214     C                   Z-ADD     12            WNOADD            9 0
624200930325     C*
624300931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
624400931125     C     D98GC2        ORNE      *BLANKS
624500931130     C*
624600931213     C                   Z-ADD     WPOROU        WORE
624700931130     C                   EXSR      OREDAT                                       *ORE->DATA
624800931130     C                   Z-ADD     WDAT          W12DAT
624900931130     C                   Z-ADD     WRES          W12RES
625000930405     C*
625100941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
625200930405     C* DELLA SETTIMANA
625300941227     C                   Z-ADD     W12DAT        DSDAT8
625400941227     C                   Z-ADD     DSAAS         CLNANN
625500941227     C                   Z-ADD     DSMES         CLNMES
625600941227     C                   Z-ADD     *ZEROS        CLNTFP
625700941227     C                   Z-ADD     *ZEROS        CLNTFA
625800930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
625900930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
626000941227     C                   MOVEL     CLNMAT        GSEDS
626100960129     C                   Z-ADD     DSGIO         GI                6 0
626200960129     C                   MOVEL     GSE(GI)       WGSE              1
626300140911     c
626400140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
626500140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
626600140911     c                   if        wtuctut='S'
626700140911     c                   eval      wtuctut='P'
626800140911     c                   endif
626900930326     C*
627000931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
627100931213     C                   DO        4             IT                3 0          --- 3 -->
627200931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
627300931213     C                   MOVEL     TUC(IT)       W93GSE            1
627400931213     C                   MOVE      TUC(IT)       W93GMP            1
627500930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
627600930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
627700931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
627800930512     C     W93GSE        OREQ      ' '
627900140911     c     WTUCTUT       andeq     ' '
628000140911     c
628100930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
628200931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
628300930401     C     W93GMP        OREQ      ' '
628400930325     C                   MOVEL     'S'           WSLITT
628500931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
628600931213     C                   ELSE                                                   <-- 6 -->
628700001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
628800001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
628900001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
629000050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
629100050908     C     Usacons       IFNE      'S'                                          --- 7 -->
629200141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
629300141107     c*    orari giorno per giorno
629400141107     c                   if        OOR2FCSGL<>' '
629500141108     c                             and w12dat<>sav_12dat
629600141107     c                   eval      or_datarif=w12dat
629700141107     c                   eval      or_DDC    =w12dat
629800141107     c                   exsr      Reper_tempi
629900141108     c                   eval      sav_12dat=w12dat
630000141107     c                   endif
630100141107     c
630200930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
630300930325     C     W93GMP        IFEQ      'M'
630400930326     C     W12RES        ANDEQ     *ZEROS
630500141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
630600141111     c                             or OOR2STfS =0
630700141105     c                             or (tuc(IT)<>gc1part and
630800141105     c                              tuc(IT)<>gc2part)
630900140825     C                   MOVEL     'S'           WSLITT
631000140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
631100140911     c* slitto solo una volta  se chiuso tutti " M"
631200140911     c                   if        W93GSE=' '
631300140911     c                   eval      wTUCTUT='S'
631400140911     c                   endif
631500140911     c
631600140825     c                   endif
631700930325     C                   END
631800930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
631900930325     C     W93GMP        IFEQ      'P'
632000930326     C     W12RES        ANDNE     *ZEROS
632100140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
632200140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
632300141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
632400141111     c                             or OOR2STiS =0
632500141105     c                             or (tuc(IT)<>gc1part and
632600141105     c                              tuc(IT)<>gc2part)
632700930325     C                   MOVEL     'S'           WSLITT
632800140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
632900140911     c* slitto solo una volta  se chiuso tutti " P"
633000140911     c                   if        W93GSE=' '
633100140911     c                   eval      wTUCTUT='S'
633200140911     c                   endif
633300140825     c                   endif
633400140825     c
633500931213     C                   ENDIF
633600001128     C                   ENDIF                                                  <-- 7 ---
633700931213     C                   ENDIF                                                  <-- 6 ---
633800931213     C                   ENDIF                                                  <-- 5 ---
633900931213     C                   ENDIF                                                  <-- 4 ---
634000931213     C                   ENDDO                                                  <-- 3 ---
634100930405     C*
634200931213     C                   ENDIF                                                  <-- 2 ---
634300931213     C                   ENDIF                                                  <-- 1 ---
634400140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
634500140911     c                   if        wtuctut='P'  and wslitt='N'
634600140911     c                   clear                   wtuctut
634700140911     c                   endif
634800140911     c
634900930325     C                   ENDSR
635000060406     C********************************************************************
635100060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
635200060406     C********************************************************************
635300060406     C     DATCEPG       BEGSR
635400060406     C*
635500060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
635600060406     C                   Z-ADD     12            WNOADD            9 0
635700060406     C*
635800060406    1C                   if        wevpag='S'
635900060406     C*
636000060406     C                   Z-ADD     WPOROU        WORE
636100060406     C                   EXSR      OREDAT                                       *ORE->DATA
636200060406     C                   Z-ADD     WDAT          W12DAT
636300060406     C                   Z-ADD     WRES          W12RES
636400060418     c                   z-add     1             P
636500060406     c* Verifico se nella data esiste un evento Part che giustifica
636600060418     c     w12dat        lookup    pvd(P)                                 30
636700060418    2c                   dow       *in30 and pvf(P)<>'G'
636800060418     c                   add       1             P
636900060420     c     w12dat        lookup    pvd(P)                                 30
637000060406    2c                   enddo
637100060406     c
637200060406     c* 30 on --> TROVATO !!
637300060406    2c                   if        *in30
637400060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
637500060406     C                   MOVEL     'S'           WSLITT
637600060406     c                   z-add     24            wnoadd
637700060406    3C                   ENDIF
637800060406     C*
637900060406    2C                   ENDIF                                                  <-- 2 ---
638000060406    1C                   ENDIF                                                  <-- 1 ---
638100060406     C*
638200060406     C                   ENDSR
638300931210     C********************************************************************
638400931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
638500931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
638600931210     C********************************************************************
638700050805     C***  CHKDCR        BEGSR
638800931210     C*
638900050805     C***  WPOCRI        IFNE      *ZEROS
639000050805     C***  WPOROU        IFGE      WPOCRI
639100050805     C***  WPOROU        ANDLT     WPOCRF
639200050805     C***                ELSE
639300050805     C***                MOVEL     'S'           WSLITT
639400050805     C***                END
639500050805     C***                END
639600931210     C*
639700050805     C***                ENDSR
639800140822     C********************************************************************
639900140822     C*  Reperimento tempi di consegna per decidere se slittare
640000140822     C********************************************************************
640100140822     C     Reper_tempi   BEGSR
640200140822
640300140822     c                   clear                   trulorSds
640400140825     c                   clear                   trulor2ds
640500140822     c                   eval       IOREtfp = d98tfp
640600140822     c                   eval       IOREtfa = d98tfa
640700140825     c                   eval       IOREdti = d98dti
640800140825     c                   eval       IOREhti = d98hti
640900140825     c                   eval       IOREdcr = s98dcr
641000140825     c                   eval       IOREhcr = s98hcr
641100140825     c                   eval       IOREtcr = s98tcr
641200140825     c                   eval       IOREDEI = d98dei
641300140825     c                   eval       IOREOEI = d98oei
641400140825     c                   eval       IOREfil = d98lna
641500140825     c                   eval       IOREcap = d98cad
641600140825     c                   eval       IOREloc = d98lod
641700140825     c                   eval       IOREnar = d98nzd
641800140825     c
641900140825     c* Data di Riferimento:
642000140825     c* Se impostata la consegnabilità --> prendo questa
642100140822     c                   select
642200140825     c                   when      d98dfc>0
642300141107     c                   if        or_datarif<d98dfc and or_datarif>0
642400141107     c                   eval      ioredta=or_datarif
642500141107     c                   else
642600141107     c                   eval      ioredta=d98dfc
642700141107     c                   endif
642800140825     c* altrimenti data di consegna
642900140825     c                   when      d98dcm>0
643000141107     c                   if        or_datarif<d98dcm and or_datarif>0
643100141107     c                   eval      ioredta=or_datarif
643200141107     c                   else
643300140825     c                   eval      ioredta=d98dcm
643400141107     c                   endif
643500140825     c
643600140822     c                   other
643700140825     c* altrimenti data teorica consegna se > = oggi
643800140825     C                   time                    wrkoggiiso
643900140825     C     *iso          move      wrkoggiiso    ioredta
644000140822     c* se < di oggi, imposto oggi
644100140825     c                   if        d98dei>ioredta
644200140825     c                   eval      ioredta=d98dei
644300140822     c                   endif
644400141107     c
644500141107     c                   if        or_datarif<ioredta and or_datarif>0
644600141107     c                   eval      ioredta=or_datarif
644700141107     c                   endif
644800141107     c
644900140822     c                   endsl
645000140822     c
645100140825     c                   eval       IOREtser = 'C'
645200141107     c                   if         or_ddc>0 and or_ddc<d98ddc
645300141107     c                   eval       IOREDDC=or_ddc
645400141107     c                   eval       IOREnDC=8888888
645500141107     c                   eval       IOREcons = 'S'
645600141107     c                   else
645700140825     c                   eval       IOREDDC=d98ddc
645800140825     c                   eval       IOREnDC=wd98ndc
645900140825     c                   eval       IOREcons = d98cons
646000141107     c                   endif
646100141017     c                   eval       IOREtsp = d98tsp
646200140822     c
646300140822     c                   call      'TRULORSR'
646400140822     c                   parm                    kpjba
646500140822     c                   parm                    trulorSds
646600140825     c                   parm                    trulor2ds
646700140825     c
646800140825     c                   eval      wtrulor= 'S'
646900141107     c                   clear                   or_datarif        8 0
647000141107     c                   clear                   or_ddc            8 0
647100140822     C                   ENDSR
647200950406     C********************************************************************
647300950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
647400950406     C********************************************************************
647500950406     C     DATORE        BEGSR
647600950406     C*
647700950406     C                   Z-ADD     *ZEROS        WORE
647800950406     C     WDAT          IFNE      0
647900950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
648000950406     C     WAA           DIV       4             WQUA              4 0
648100950406     C                   MVR                     WRES
648200950406     C     WAA           MULT      365           WGIO              9 0
648300950406     C                   ADD       WQUA          WGIO
648400950406     C                   ADD       WGG           WGIO
648500950406     C     WMM           SUB       1             MC                3 0
648600950406     C                   DO        MC            WX                3 0
648700950406     C                   ADD       MES(WX)       WGIO
648800950406     C                   END
648900950406     C     WRES          IFEQ      0
649000950406     C     WMM           ANDLE     2
649100950406     C                   SUB       1             WGIO
649200950406     C                   END
649300950406     C     WGIO          MULT      24            WORE              9 0
649400950406     C                   ENDIF
649500950406     C*
649600950406     C                   ENDSR
649700930323     C********************************************************************
649800930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
649900930323     C********************************************************************
650000930323     C     OREDAT        BEGSR
650100930323     C*
650200950407     C                   SETOFF                                       717273
650300950407     C                   SETOFF                                       7475
650400930323     C     WORE          DIV       24            WGIO
650500930323     C                   MVR                     WRES              3 0
650600950406     C*
650700950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
650800950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
650900950406     C*  TROVO ANNO
651000950406     C     WGIO          DIV       365           WAA
651100950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
651200950406     C                   MVR                     WREST                71
651300950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
651400950406     C     WGIO          DIV       365           WAA
651500950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
651600950406     C*                                                    0 = 31/12/PR
651700950406     C   75
651800950406     CAN 71              DO                                                     BIS. E NON ERA
651900950406     C     WAA           SUB       1             WAA
652000950406     C                   Z-ADD     365           WREST
652100950406     C                   END
652200950406     C*
652300950406     C     WAA           DIV       4             WNBIS                                      T.
652400950406     C                   MVR                     WRBIS             3 073
652500950406     C   73
652600950406     CANN71                                                                     NON PIU' BISEST
652700950406     CORN73              ADD       1             WREST                          E'BISESTILE
652800950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
652900950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
653000950406     C  N73              SUB       1             $PM(1)
653100950406     C                   Z-ADD     1             WI                2 0
653200950406     C* TROVO IL MESE E GIORNI
653300950406     C     WREST         LOOKUP    $PM(WI)                            74  73
653400950406     C                   MOVE      WI            WMM
653500950406     C     WI            COMP      1                                      72
653600950406     C  N72
653700950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
653800950406     C   74              SUB       1             WI
653900950406     C  N72
654000950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
654100950406     C   72              MOVE      WREST         WGG
654200950406     C                   ADD       1900          WAA
654300950406     C*
654400930323     C*
654500930323     C                   ENDSR
6546009407151507 C********************************************************************
6547009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6548009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6549009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6550009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6551009407151507 C********************************************************************
6552009407151507 C     FINGIO        BEGSR
6553009407151507 C*
6554009407151507 C     WPORE         DIV       24            WGIO
6555009407151507 C                   MVR                     WRES
6556009407151507 C     WRES          IFEQ      *ZEROS
6557009407151507 C                   Z-ADD     12            WNORE             3 0
6558009407151507 C                   ELSE
6559009407151507 C                   Z-ADD     *ZEROS        WNORE
6560009407151507 C                   ENDIF
6561009407151507 C*
6562009407151507 C                   ENDSR
656300930517     C********************************************************************
656400971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
656500930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
656600930517     C********************************************************************
656700930517     C     CERCAL        BEGSR
656800930517     C*
656900930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
657000941227     C                   MOVE      CLNTFP        DCATFP
657100941227     C                   MOVE      CLNTFA        DCATFA
657200941227     C                   MOVE      CLNANN        DCAANN
657300941227     C                   MOVE      CLNMES        DCAMES
657400930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
657500930517     C                   Z-ADD     1             I
657600930517     C     DCALEN        LOOKUP    CAK(I)                                 99
657700930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
657800930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
657900941227     C     KEYCLN        CHAIN     AZCLN01L                           99
658000930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
658100930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
658200930517     C                   MOVEL     '1'           D98ERR
658300930517     C                   MOVEL     MSG(3)        D98MSG
658400930517     C                   ELSE                                                   <-- 2 -->
658500970401     C* CONTROLLA SE ANNULLATO
658600970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
658700970401     C                   MOVEL     '1'           D98ERR
658800970401     C                   MOVEL     MSG(3)        D98MSG
658900970401     C                   ELSE                                                   <-- 3 -->
659000930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
659100930517     C                   ADD       1             WINDCA            6 0
659200970401     C     WINDCA        IFLE      200                                          --- 4 -->
659300970401     C     WINDCA        ANDGT     *ZEROS
659400930517     C                   Z-ADD     WINDCA        I
659500930517     C                   MOVEL     DCALEN        CAK(I)
659600941227     C                   MOVEL     CLNMAT        CAM(I)
659700941227     C                   MOVEL     CLNPOM        CAP(I)
659800970401     C                   END                                                    <-- 4 ---
659900970401     C                   ENDIF                                                  <-- 3 ---
660000930517     C                   END                                                    <-- 2 ---
660100930517     C                   ELSE                                                   <-- 1 -->
660200930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
660300941227     C                   MOVEL     CAM(I)        CLNMAT
660400941227     C                   MOVEL     CAP(I)        CLNPOM
660500930517     C                   END                                                    <-- 1 ---
660600930517     C*
660700930517     C                   ENDSR
660800010821     C********************************************************************
660900010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
661000010821     C********************************************************************
661100010821     C     CHKVCA        BEGSR
661200010821     C*
661300010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
661400010821     C     D98CAS        IFGT      *ZEROS
661500010821     C     D98VCA        ANDEQ     *BLANKS
661600010821     C                   MOVEL     'ITL'         D98VCA
661700010821     C                   ENDIF
661800010822     C*
661900010822     C     D98VCA        IFNE      'ITL'
662000010822     C     D98VCA        ANDNE     'EUR'
662100010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
662200010822     C                   Z-ADD     D98CAS        INPIMP
662300010822     C                   MOVEL     D98VCA        INPDIV
662400010822     C                   EXSR      EXECNV
662500010822     C                   Z-ADD     OUTIMP        D98CAS
662600010822     C                   MOVEL     §GEDCN        D98VCA
662700010822     C                   ENDIF
662800010821     C*
662900010821     C                   ENDSR
663000010821     C********************************************************************
663100010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
663200010821     C********************************************************************
663300010821     C     EXECNV        BEGSR
663400010821     C*
663500010821     C                   CLEAR                   YEURDS
663600010821     C*
663700010821     C                   MOVEL     INPDIV        YECDVI
663800010821     C                   Z-ADD     INPIMP        YECIMI
663900010821     C                   MOVEL     §GEDCN        YECDVO
664000010821     C                   MOVEL     '3'           YECDCO
664100010821     C                   MOVEL     'H'           YECTAR
664200010821     C                   CALL      'YEURCO'
664300010821     C                   PARM                    YEURDS
664400010821     C*
664500010821     C                   Z-ADD     YECIMO        OUTIMP
664600010821     C*
664700010821     C                   ENDSR
664800011211     C*------------------------------------------------------------------------*
664900011211     C* Controllo importo massimo contrassegno.
665000011211     C*------------------------------------------------------------------------*
665100011211     C     REPMAXCSB     BEGSR
665200011211     C*
665300011211     C                   CLEAR                   Trul21DS
665400011211     C*
665500011211     C                   EVAL      I21TSp = d98TSP
665600011211     C                   EVAL      I21NzD = d98NZD
665700011211     C                   EVAL      I21LnP = d98LNP
665800011211     C                   EVAL      I21LnA = d98LNA
665900011211     C                   EVAL      I21Ksc = d98KSC
666000011211     C                   EVAL      I21TiC = d98TIC
666100011211     C                   EVAL      I21Imp = d98CAS
666200011211     C                   EVAL      I21Div = d98VCA
666300011211     C*
666400011211     C                   CALL      'TRUL21R'
666500011211     C                   PARM                    Trul21DS
666600011211     C*
666700011211     C                   ENDSR
666800011211     C*------------------------------------------------------------------------*
666900011211
667000930414     C********************************************************************
667100930414     C*   RUTINZ - ROUTINE INIZIALE
667200930414     C********************************************************************
667300930414     C     RUTINZ        BEGSR
667400930414     C*
667500930414     C* PARAMETRI IN INPUT ALLA ROUTINE
667600930414     C     *ENTRY        PLIST
667700970731     C                   PARM                    DS99
667800941227     C*
667900941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
668000941227     C     KEYBLP        KLIST
668100941227     C                   KFLD                    BLPAAS
668200941227     C                   KFLD                    BLPLNP
668300941227     C                   KFLD                    BLPNRS
668400941227     C                   KFLD                    BLPNSP
668500050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
668600941227     C     KEYGCP        KLIST
668700050405     C                   KFLD                    KWGCPAAS
668800050405     C                   KFLD                    KWGCPLNP
668900050405     C                   KFLD                    KWGCPNRS
669000050405     C                   KFLD                    KWGCPNSP
669100050405     C                   KFLD                    KWGCPFRG
669200050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
669300031125     C     KEYGC_P       KLIST
669400050405     C                   KFLD                    KWGCPAAS
669500050405     C                   KFLD                    KWGCPLNP
669600050405     C                   KFLD                    KWGCPNRS
669700050405     C                   KFLD                    KWGCPNSP
669800970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
669900150120     C**   KEYBLT        KLIST
670000150120     C**                 KFLD                    BLTAAS
670100150120     C**                 KFLD                    BLTLNP
670200150120     C**                 KFLD                    BLTNRS
670300150120     C**                 KFLD                    BLTNSP
670400941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
670500941227     C     KEYARB        KLIST
670600941227     C                   KFLD                    ARBAAS
670700941227     C                   KFLD                    ARBLNP
670800941227     C                   KFLD                    ARBNRS
670900941227     C                   KFLD                    ARBNSP
671000051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
671100941228     C     KEYAR9        KLIST
671200941228     C                   KFLD                    AR9AAS
671300941228     C                   KFLD                    AR9LNP
671400941228     C                   KFLD                    AR9NRS
671500941228     C                   KFLD                    AR9NSP
671600941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
671700050405     C     KEYGCP21      KLIST
671800050405     C                   KFLD                    KWGCPAAS
671900050405     C                   KFLD                    KWGCPLNP
672000050405     C                   KFLD                    KWGCPNRS
672100050405     C                   KFLD                    KWGCPNSP
672200050405     C                   KFLD                    KWGCPFRG
672300970912     C* CHIAVE LETTURA FNART01L - PARZIALE
672400150120     C***  KEYART        KLIST
672500150120     C***                KFLD                    ARTAAS
672600150120     C***                KFLD                    ARTLNP
672700150120     C***                KFLD                    ARTNRS
672800150120     C***                KFLD                    ARTNSP
672900950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
673000950102     C     KEYLBL        KLIST
673100950102     C                   KFLD                    LBLAAN
673200950102     C                   KFLD                    LBLLPN
673300950102     C                   KFLD                    LBLNRN
673400950102     C                   KFLD                    LBLNSN
673500930414     C*
673600070827     c     kAR5          Klist
673700070827     c                   Kfld                    d98Aas
673800070827     c                   Kfld                    d98Lnp
673900070827     c                   Kfld                    d98nrs
674000070827     c                   Kfld                    d98nsp
674100070827     c                   Kfld                    kAr5Trd
674200150325     c     kAR4          Klist
674300150325     c                   Kfld                    d98Aas
674400150325     c                   Kfld                    d98Lnp
674500150325     c                   Kfld                    d98nrs
674600150325     c                   Kfld                    d98nsp
674700150325     c                   Kfld                    kAr4Trc
674800150218     c*
674900150218     c     kart          Klist
675000150218     c                   Kfld                    d98Aas
675100150218     c                   Kfld                    d98Lnp
675200150218     c                   Kfld                    d98nrs
675300150218     c                   Kfld                    d98nsp
675400150218     c     kbrv          Klist
675500150218     c                   Kfld                    artfls
675600150218     c                   Kfld                    artlna
675700150218     c                   Kfld                    artnrs
675800150218     c                   Kfld                    artnsc
675900150325     C     Kfapd         klist
676000150325     C                   kfld                    kpdtip
676100150325     C                   kfld                    kpdpdr
676200930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
676300930414     C     KEYTAB        KLIST
676400930414     C                   KFLD                    KTBUTE
676500930414     C                   KFLD                    KTBCOD
676600930414     C                   KFLD                    KTBKEY
676700060406     C     Ktab          KLIST
676800060406     C                   KFLD                    KTBUTE
676900060406     C                   KFLD                    KTBCOD
677000930414     C*
677100941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
677200941227     C     KEYCLN        KLIST
677300941227     C                   KFLD                    CLNTFP
677400941227     C                   KFLD                    CLNTFA
677500941227     C                   KFLD                    CLNANN
677600941227     C                   KFLD                    CLNMES
677700941227     C*
677800941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
677900941227     C     KEYCAS        KLIST
678000941227     C                   KFLD                    CASPTA
678100941227     C                   KFLD                    CASLNP
678200941227     C                   KFLD                    CASLNA
678300941227     C                   KFLD                    CASTSP
678400941227     C                   KFLD                    CASDDE
678500930420     C*
678600990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
678700930420     C     KEYEVB        KLIST
678800930420     C                   KFLD                    KEBAAS
678900930420     C                   KFLD                    KEBLNP
679000930420     C                   KFLD                    KEBNRS
679100930420     C                   KFLD                    KEBNSP
679200010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
679300010821     C     KTBE          KLIST
679400010821     C                   KFLD                    TBECOD
679500010821     C                   KFLD                    TBEKE1
679600010821     C                   KFLD                    TBEKE2
679700930414     C*
679800010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
679900930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
680000930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
680100930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
680200930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
680300930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
680400930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
680500930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
680600930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
680700930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
680800010821     C     *LIKE         DEFINE    D98CAS        INPIMP
680900010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
681000010821     C     *LIKE         DEFINE    D98VCA        INPDIV
681100930414     C*
681200930414     C* RECUPERO IL CODICE DELL'AZIENDA
681300930414     C                   Z-ADD     1             CODUT             1 0
681400970701     C                   Z-ADD     *ZEROS        I                 6 0
681500930414     C*
681600930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
681700930414     C                   EXSR      AZZERA
681800930414     C*
681900930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
682000941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
682100941227     C                   MOVEL     'S'           CTABSN            1
682200930414     C                   EXSR      CARTAB
682300941227     C                   ENDIF
682400041116     c* Se passato umero di spedizioni apro i files , altrimenti no
682500041116     c                   if        d98nsp>0 and wapri=' '
682600041116     c                   open      fnblp01l
682700050405     c                   open      tigcp01l
682800150120     c***                open      fnblt01l
682900041116     c                   open      fnarb01l
683000051115     c                   open      fiar901l
683100150325     c                   open      fiar401l
683200050405     c                   open      tigcp21l
683300150218     c                   open      fnart01l
683400150218     c                   open      fnbrv07l
683500041116     c                   open      fnlbl01l
683600041116     c                   open      fnevb01l
683700070827     c                   open      fiar501l
683800041116     c                   eval      wapri='S'
683900041116     c                   endif
684000930414     C*
684100930414     C                   ENDSR
684200941227     C*------------------------------------------------------------------------*
684300941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
684400941227     C*------------------------------------------------------------------------*
684500941227     C     SUBDAT        BEGSR
684600941227     C*
684700941227     C                   TIME                    DTTIME
684800941227     C                   Z-ADD     DTANN         DSANN
684900941227     C                   Z-ADD     DTMES         DSMES
685000941227     C                   Z-ADD     DTGIO         DSGIO
685100941227     C                   Z-ADD     DTSEC         DSSEC
685200941227     C*
685300941227     C                   ENDSR
685400930414     C********************************************************************
685500930414     C*   CARTAB - CARICA LE TABELLE
685600930414     C********************************************************************
685700930414     C     CARTAB        BEGSR
685800930414     C*
685900930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
686000930414     C     D98ERR        IFEQ      '0'
686100930421     C                   Z-ADD     0             I
686200930414     C                   Z-ADD     CODUT         KTBUTE
686300930414     C                   MOVEL     '1Y'          KTBCOD
686400930414     C                   MOVEL     *BLANKS       KTBKEY
686500930414     C     KEYTAB        SETLL     TABEL00F                           99
686600930414     C  N99              READ      TABEL00F                               99
686700930414     C     *IN99         IFEQ      '1'
686800930414     C                   MOVEL     '1'           D98ERR
686900930414     C                   MOVEL     MSG(5)        D98MSG
687000930414     C                   END
687100930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
687200930414     C     TBLCOD        ANDEQ     '1Y'
687300930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
687400930421     C                   ADD       1             I
687500930421     C                   MOVEL     TBLKEY        C1Y(I)
687600930414     C                   MOVEL     TBLUNI        DS1Y
687700930421     C                   MOVEL     §1YFEI        I1Y(I)
687800930421     C                   MOVEL     §1YFEC        E1Y(I)
687900930414     C                   END                                                    <-- 3 ---
688000930414     C                   READ      TABEL00F                               99
688100930414     C                   END                                                    <-- 2 ---
688200930414     C                   END
688300930414     C*
688400930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
688500930414     C     D98ERR        IFEQ      '0'
688600930421     C                   Z-ADD     0             I
688700930414     C                   Z-ADD     CODUT         KTBUTE
688800930414     C                   MOVEL     '1X'          KTBCOD
688900930414     C                   MOVEL     *BLANKS       KTBKEY
689000930414     C     KEYTAB        SETLL     TABEL00F                           99
689100930414     C  N99              READ      TABEL00F                               99
689200930414     C     *IN99         IFEQ      '1'
689300930414     C                   MOVEL     '1'           D98ERR
689400930414     C                   MOVEL     MSG(1)        D98MSG
689500930414     C                   END
689600930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
689700930414     C     TBLCOD        ANDEQ     '1X'
689800930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
689900930421     C                   ADD       1             I
690000930421     C                   MOVEL     TBLKEY        C1X(I)
690100930414     C                   MOVEL     TBLUNI        DS1X
690200930421     C                   Z-ADD     §1XHHG        G1X(I)
690300930421     C                   Z-ADD     §1XHHO        O1X(I)
690400930421     C                   Z-ADD     §1XHHM        M1X(I)
690500930421     C                   Z-ADD     §1XHHP        P1X(I)
690600930421     C                   Z-ADD     §1XHHI        I1X(I)
690700930421     C                   Z-ADD     §1XHHS        S1X(I)
690800930414     C                   END                                                    <-- 3 ---
690900930414     C                   READ      TABEL00F                               99
691000931223     C                   ENDDO                                                  <-- 2 ---
691100050523     c
691200050523     c* controllo riempimento skiere
691300050523     c                   clear                   trul0sds
691400050523     c                   eval      i0sski='C1X/G1X/O1X/...'
691500050523     c                   eval      i0sele=%elem(c1x)
691600050523     c                   eval      i0spie=i
691700050523     c                   eval      i0sfile='TABEL00F  '
691800050523     c                   eval      i0scod=' 1X  '
691900050523     c                   eval      i0spgm='TNSD99R'
692000050523     c                   movel     trul0sds      kpjbu
692100050523     c                   call      'TRUL0SR'
692200050523     c                   parm                    kpjba
692300050523     C                   END
692400930421     C*
692500930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
692600930421     C     D98ERR        IFEQ      '0'
692700930421     C                   Z-ADD     0             I
692800930421     C                   Z-ADD     CODUT         KTBUTE
692900930421     C                   MOVEL     '2A'          KTBCOD
693000060406     C     Ktab          SETLL     TABEL00F                           99
693100060406     C  N99ktab          READe     TABEL00F                               99
693200930421     C     *IN99         IFEQ      '1'
693300930421     C                   MOVEL     '1'           D98ERR
693400930421     C                   MOVEL     MSG(8)        D98MSG
693500930421     C                   END
693600930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
693700930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
693800930421     C                   ADD       1             I
693900930421     C                   MOVEL     TBLKEY        C2A(I)
694000060406     C                   MOVEL     TBLUNI        ds2a
694100060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
694200060406     C                   MOVEL     w003a         D2A(I)
694300930421     C                   END                                                    <-- 3 ---
694400060406     C     ktab          READe     TABEL00F                               99
694500931223     C                   ENDDO                                                  <-- 2 ---
694600050523     c* controllo riempimento skiere
694700050523     c                   clear                   trul0sds
694800050523     c                   eval      i0sski='C2A/D2A'
694900050523     c                   eval      i0sele=%elem(c2a)
695000050523     c                   eval      i0spie=i
695100050523     c                   eval      i0sfile='TABEL00F  '
695200060406     c                   eval      i0scod='2A/2Z'
695300050523     c                   eval      i0spgm='TNSD99R'
695400050523     c                   movel     trul0sds      kpjbu
695500050523     c                   call      'TRUL0SR'
695600050523     c                   parm                    kpjba
695700930421     C                   END
695800060406     c* Tabella  2Z
695900060406     C     D98ERR        IFEQ      '0'
696000060406     C                   Z-ADD     CODUT         KTBUTE
696100060406     C                   MOVEL     '2Z'          KTBCOD
696200060406     C     Ktab          SETLL     TABEL00F                           99
696300060406     C  N99ktab          READe     TABEL00F                               99
696400060406     C     *IN99         IFEQ      '1'
696500060406     C                   MOVEL     '1'           D98ERR
696600060406     C                   MOVEL     MSG(33)       D98MSG
696700060406     C                   END
696800060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
696900060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
697000060406     C                   Z-ADD     1             I
697100060406     C                   MOVEL     TBLKEY        w003a
697200060406     c     w003a         lookup    c2a(i)                                 99
697300060406     C     *IN99         IFEQ      *off
697400060406     C                   MOVEL     '1'           D98ERR
697500060406     C                   MOVEL     MSG(34)       D98MSG
697600060406     C                   Else
697700060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
697800060406     C                   END                                                    <-- 3 ---
697900060406     C                   END                                                    <-- 3 ---
698000060406     C     ktab          READe     TABEL00F                               99
698100060406     C                   ENDDO                                                  <-- 2 ---
698200060406     C                   END
698300941227     C*
698400941227     C* CARICA TABELLA CODICI BOLLA -3A-
698500941227     C     D98ERR        IFEQ      '0'
698600941228     C                   Z-ADD     0             I
698700941227     C                   Z-ADD     CODUT         KTBUTE
698800941227     C                   MOVEL     '3A'          KTBCOD
698900941227     C                   MOVEL     *BLANKS       KTBKEY
699000941227     C     KEYTAB        SETLL     TABEL00F                           99
699100941227     C  N99              READ      TABEL00F                               99
699200941227     C     *IN99         IFEQ      '1'
699300941227     C                   MOVEL     '1'           D98ERR
699400941227     C                   MOVEL     MSG(22)       D98MSG
699500941227     C                   END
699600941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
699700941227     C     TBLCOD        ANDEQ     '3A'
699800941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
699900941227     C                   ADD       1             I
700000941227     C                   MOVEL     TBLKEY        C3A(I)
700100941227     C                   MOVEL     TBLUNI        DS3A
700200941227     C                   MOVEL     §3ATB1        T3A(I)
700300941228     C                   Z-ADD     §3AFCA        F3A(I)
700400990308     C                   MOVEL     §3ARBL        R3A(I)
700500941227     C                   END                                                    <-- 3 ---
700600941227     C                   READ      TABEL00F                               99
700700941227     C                   END                                                    <-- 2 ---
700800050523     c* controllo riempimento skiere
700900050523     c                   clear                   trul0sds
701000050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
701100050523     c                   eval      i0sele=%elem(c3a)
701200050523     c                   eval      i0spie=i
701300050523     c                   eval      i0sfile='TABEL00F  '
701400050523     c                   eval      i0scod=' 3A  '
701500050523     c                   eval      i0spgm='TNSD99R'
701600050523     c                   movel     trul0sds      kpjbu
701700050523     c                   call      'TRUL0SR'
701800050523     c                   parm                    kpjba
701900941227     C                   END
702000971210     C*
702100020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
702200020226     C                   Z-ADD     *ZEROS        J                 3 0
702300971210IF  0C     D98ERR        IFEQ      '0'
702400971210     C     *LOVAL        SETLL     AZORG01L
702500971210     C                   READ      AZORG01L                               98
702600971210DO  1C     *IN98         DOWEQ     *OFF
702700971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
702800971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
702900971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
703000020627     C                   MOVEL     ORGDE3        OG143
703100020702     C*
703200020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
703300020702     C                   CLEAR                   DNTW
703400020702     C*
703500020702     C                   MOVEL(P)  'NTW'         TBECOD
703600020702     C                   MOVEL(P)  §OGNTW        TBEKE1
703700020702     C     KTBE          CHAIN     TNTBE01L
703800020702     C                   IF        %found(TNTBE01L)
703900020702     C                   MOVEL     TBEUNI        DNTW
704000020702     C                   ENDIF
704100020702     C*
704200020702     C                   MOVEL     §NTWFIE       FL1(F)
704300020627     C*
704400020226     C* VERIFICO SE FILIALE FEDEX
704500020226     C                   IF        §OGNTW = 'FED'
704600020226     C                   ADD       1             J
704700020226     C                   Z-ADD     ORGFIL        FILFED(J)
704800020226     C                   ENDIF
704900020226     C*
705000971210E   2C                   ENDIF
705100971210     C                   READ      AZORG01L                               98
705200971210E   1C                   ENDDO
705300971210E   0C                   ENDIF
705400010821     C*
705500010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
705600010821     C                   MOVEL     'GED'         TBECOD
705700070514     C                   MOVEL     *BLANKS       TBEKE1
705800070514     C                   MOVEL     '1'           TBEKE1
705900010821     C                   MOVEL     *BLANKS       TBEKE2
706000010821     C     KTBE          CHAIN     TNTBE01L                           21
706100070514     C  N21              MOVEL     TBEUNI        DGED
706200150326     c
706300150326     C* REPERISCO LA tabella flag operativi per tabella OLP
706400150326     c                   clear                   dvpo
706500150326     C                   MOVEL     'VPO'         TBECOD
706600150326     C                   MOVEL     *BLANKS       TBEKE2
706700150326     C                   MOVEL(P)  'VPO'         TBEKE1
706800150326     C     KTBE          CHAIN     TNTBE01L                           21
706900150326     C  N21              MOVEL     TBEUNI        DVPO
707000150326     C*
707100031201     C*
707200031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
707300031201     C     D98ERR        IFEQ      '0'
707400060403     C                   Z-ADD     0             I7QW              3 0
707500031201     C                   Z-ADD     CODUT         KTBUTE
707600031201     C                   MOVEL     '7Q'          KTBCOD
707700031201     C                   MOVEL     *BLANKS       KTBKEY
707800031201     C     KEYTAB        SETLL     TABEL00F                           99
707900031201     C  N99              READ      TABEL00F                               99
708000031201     C     *IN99         IFEQ      '1'
708100031201     C                   MOVEL     '1'           D98ERR
708200031201     C                   MOVEL     MSG(8)        D98MSG
708300031201     C                   ENDIF
708400031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
708500031201     C     TBLCOD        ANDEQ     '7Q'
708600031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
708700031201     C                   MOVEL     TBLUNI        DS7Q
708800031201     C                   IF        §7Q4D1 = 'R' OR
708900031201     C                             §7Q4D2 = 'R' OR
709000031201     C                             §7Q4D3 = 'R' OR
709100031201     C                             §7Q4D4 = 'R' OR
709200031201     C                             §7Q4D5 = 'R'
709300031201     C                   ADD       1             I7QW
709400031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
709500031201     C                   ENDIF                                                  <-- 3 ---
709600031201     C                   ENDIF                                                  <-- 3 ---
709700031201     C                   READ      TABEL00F                               99
709800031201     C                   ENDDO                                                  <-- 2 ---
709900031201     C                   ENDIF
710000031201     C*
710100031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
710200031201     C     D98ERR        IFEQ      '0'
710300031201     C                   Z-ADD     CODUT         KTBUTE
710400031201     C                   MOVEL     '7W'          KTBCOD
710500031201     C                   MOVEL     *BLANKS       KTBKEY
710600031201     C     KEYTAB        SETLL     TABEL00F                           99
710700031201     C  N99              READ      TABEL00F                               99
710800031201     C     *IN99         IFEQ      '1'
710900031201     C                   MOVEL     '1'           D98ERR
711000031201     C                   MOVEL     MSG(8)        D98MSG
711100031201     C                   ENDIF
711200031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
711300031201     C     TBLCOD        ANDEQ     '7W'
711400031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
711500031201     C                   MOVEL     TBLUNI        DS7W
711600031201     C                   IF        §7W6D1 = 'C' OR
711700031201     C                             §7W6D2 = 'C' OR
711800031201     C                             §7W6D3 = 'C' OR
711900031201     C                             §7W6D4 = 'C' OR
712000031201     C                             §7W6D5 = 'C'
712100031201     C                   ADD       1             I7QW
712200031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
712300031201     C                   ENDIF                                                  <-- 3 ---
712400031201     C                   ENDIF                                                  <-- 3 ---
712500031201     C                   READ      TABEL00F                               99
712600031201     C                   ENDDO                                                  <-- 2 ---
712700050523     c* controllo riempimento skiere
712800050523     c                   clear                   trul0sds
712900050523     c                   eval      i0sski='SK7QW'
713000050523     c                   eval      i0sele=%elem(sk7qw)
713100050523     c                   eval      i0spie=i7qw
713200050523     c                   eval      i0sfile='TABEL00F  '
713300050523     c                   eval      i0scod='7Q-7W'
713400050523     c                   eval      i0spgm='TNSD99R'
713500050523     c                   movel     trul0sds      kpjbu
713600050523     c                   call      'TRUL0SR'
713700050523     c                   parm                    kpjba
713800031201     C                   ENDIF
713900930414     C*
714000930414     C                   ENDSR
714100930414     C********************************************************************
714200930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
714300930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
714400930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
714500930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
714600930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
714700930414     C********************************************************************
714800930414     C     RO3ORE        BEGSR
714900930414     C*
715000930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
715100930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
715200930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
715300050902     c                   movel     ' '           WR3MATPOM         1
715400931209     C                   Z-ADD     *ZEROS        WR3HHO
715500931209     C                   Z-ADD     *ZEROS        WR3HHA
715600930414     C                   MOVE      WR3TAB        WR3COD
715700930604     C                   MOVEL     D98LNA        WR3COD            8
715800930421     C                   Z-ADD     1             I
715900930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
716000930414     C     *IN99         IFEQ      '0'
716100930414     C                   MOVE      WR3TAB        WR3COD
716200930414     C                   MOVEL     '   '         WR3COD            8
716300930421     C                   Z-ADD     1             I
716400930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
716500930414     C                   END
716600930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
716700930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
716800930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
716900930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
717000930414     C                   ELSE                                                   <-- 2 -->
717100930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
717200930421     C                   Z-ADD     M1X(I)        WR3HHO
717300930510     C                   Z-ADD     I1X(I)        WR3HHA
717400050902     c                   movel     'M'           WR3MATPOM         1
717500930414     C                   END
717600930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
717700930421     C                   Z-ADD     P1X(I)        WR3HHO
717800930510     C                   Z-ADD     S1X(I)        WR3HHA
717900050902     c                   movel     'P'           WR3MATPOM
718000930414     C                   END
718100930414     C                   END                                                    <-- 2 ---
718200930414     C                   END                                                    <-- 1 ---
718300930414     C*
718400930414     C                   ENDSR
718500930423     C********************************************************************
718600930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
718700930423     C********************************************************************
718800930423     C     W98D98        BEGSR
718900930423     C*
719000941228     C*
719100941228     C* ORE REALI PARTENZA INTERNE
719200941228     C                   Z-ADD     W98RPI        W98               3 0
719300941228     C                   EXSR      W98999
719400941228     C                   Z-ADD     D98           D98RPI
719500941228     C* ORE REALI PARTENZA CLIENTE
719600941228     C                   Z-ADD     W98RPC        W98
719700941228     C                   EXSR      W98999
719800941228     C                   Z-ADD     D98           D98RPC
719900930423     C* ORE REALI TRAZIONE INTERNE
720000941228     C                   Z-ADD     W98RTI        W98
720100941228     C                   EXSR      W98999
720200941228     C                   Z-ADD     D98           D98RTI
720300930423     C* ORE REALI TRAZIONE CLIENTE
720400941228     C                   Z-ADD     W98RTC        W98
720500941228     C                   EXSR      W98999
720600941228     C                   Z-ADD     D98           D98RTC
720700930423     C* ORE REALI CONSEGNA INTERNE
720800941228     C                   Z-ADD     W98RCI        W98
720900941228     C                   EXSR      W98999
721000941228     C                   Z-ADD     D98           D98RCI
721100930423     C* ORE REALI CONSEGNA CLIENTE
721200941228     C                   Z-ADD     W98RCC        W98
721300941228     C                   EXSR      W98999
721400941228     C                   Z-ADD     D98           D98RCC
721500930423     C* ORE REALI RESA INTERNE
721600941228     C                   Z-ADD     W98RRI        W98
721700941228     C                   EXSR      W98999
721800941228     C                   Z-ADD     D98           D98RRI
721900930423     C* ORE REALI RESA CLIENTE
722000941228     C                   Z-ADD     W98RRC        W98
722100941228     C                   EXSR      W98999
722200941228     C                   Z-ADD     D98           D98RRC
722300941228     C* ORE DIFF. PARTENZA INTERNE
722400941228     C                   Z-ADD     W98FPI        W98
722500941228     C                   EXSR      W98999
722600941228     C                   Z-ADD     D98           D98FPI
722700941228     C* ORE DIFF. PARTENZA CLIENTE
722800941228     C                   Z-ADD     W98FPC        W98
722900941228     C                   EXSR      W98999
723000941228     C                   Z-ADD     D98           D98FPC
723100930423     C* ORE DIFF. TRAZIONE INTERNE
723200941228     C                   Z-ADD     W98FTI        W98
723300941228     C                   EXSR      W98999
723400941228     C                   Z-ADD     D98           D98FTI
723500930423     C* ORE DIFF. TRAZIONE CLIENTE
723600941228     C                   Z-ADD     W98FTC        W98
723700941228     C                   EXSR      W98999
723800941228     C                   Z-ADD     D98           D98FTC
723900930423     C* ORE DIFF. CONSEGNA INTERNE
724000941228     C                   Z-ADD     W98FCI        W98
724100941228     C                   EXSR      W98999
724200941228     C                   Z-ADD     D98           D98FCI
724300930423     C* ORE DIFF. CONSEGNA CLIENTE
724400941228     C                   Z-ADD     W98FCC        W98
724500941228     C                   EXSR      W98999
724600941228     C                   Z-ADD     D98           D98FCC
724700930423     C* ORE DIFF. RESA INTERNE
724800941228     C                   Z-ADD     W98FRI        W98
724900941228     C                   EXSR      W98999
725000941228     C                   Z-ADD     D98           D98FRI
725100930423     C* ORE DIFF. RESA CLIENTE
725200941228     C                   Z-ADD     W98FRC        W98
725300941228     C                   EXSR      W98999
725400941228     C                   Z-ADD     D98           D98FRC
725500941228     C* ORE NON CONF. PARTENZA INTERNE
725600941228     C     W98NPI        IFGT      999
725700941228     C                   Z-ADD     999           D98NPI
725800941228     C                   ELSE
725900941228     C                   Z-ADD     W98NPI        D98NPI
726000941228     C                   END
726100941228     C* ORE NON CONF. PARTENZA CLIENTE
726200941228     C     W98NPC        IFGT      999
726300941228     C                   Z-ADD     999           D98NPC
726400941228     C                   ELSE
726500941228     C                   Z-ADD     W98NPC        D98NPC
726600941228     C                   END
726700930423     C* ORE NON CONF. TRAZIONE INTERNE
726800930423     C     W98NTI        IFGT      999
726900930423     C                   Z-ADD     999           D98NTI
727000930423     C                   ELSE
727100930423     C                   Z-ADD     W98NTI        D98NTI
727200930423     C                   END
727300930423     C* ORE NON CONF. TRAZIONE CLIENTE
727400930423     C     W98NTC        IFGT      999
727500930423     C                   Z-ADD     999           D98NTC
727600930423     C                   ELSE
727700930423     C                   Z-ADD     W98NTC        D98NTC
727800930423     C                   END
727900930423     C* ORE NON CONF. CONSEGNA INTERNE
728000930423     C     W98NCI        IFGT      999
728100930423     C                   Z-ADD     999           D98NCI
728200930423     C                   ELSE
728300930423     C                   Z-ADD     W98NCI        D98NCI
728400930423     C                   END
728500930423     C* ORE NON CONF. CONSEGNA CLIENTE
728600930423     C     W98NCC        IFGT      999
728700930423     C                   Z-ADD     999           D98NCC
728800930423     C                   ELSE
728900930423     C                   Z-ADD     W98NCC        D98NCC
729000930423     C                   END
729100930423     C* ORE NON CONF. RESA INTERNE
729200930423     C     W98NRI        IFGT      999
729300930423     C                   Z-ADD     999           D98NRI
729400930423     C                   ELSE
729500930423     C                   Z-ADD     W98NRI        D98NRI
729600930423     C                   END
729700930423     C* ORE NON CONF. RESA CLIENTE
729800930423     C     W98NRC        IFGT      999
729900930423     C                   Z-ADD     999           D98NRC
730000930423     C                   ELSE
730100930423     C                   Z-ADD     W98NRC        D98NRC
730200930423     C                   END
730300930423     C* ORE X TURNO CHIUSURA INTERNE
730400941228     C                   Z-ADD     W98HHI        W98
730500941228     C                   EXSR      W98999
730600941228     C                   Z-ADD     D98           D98HHI
730700930423     C* ORE X TURNO CHIUSURA CLIENTE
730800941228     C                   Z-ADD     W98HHC        W98
730900941228     C                   EXSR      W98999
731000941228     C                   Z-ADD     D98           D98HHC
731100060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
731200060418     C                   Z-ADD     W98EGI        W98
731300060418     C                   EXSR      W98999
731400060418     C                   Z-ADD     D98           D98hEGI
731500060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
731600060418     C                   Z-ADD     W98EGC        W98
731700060418     C                   EXSR      W98999
731800060418     C                   Z-ADD     D98           D98hEGC
731900060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
732000060420     C                   Z-ADD     W98EAI        W98
732100060420     C                   EXSR      W98999
732200060420     C                   Z-ADD     D98           D98hEAI
732300060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
732400060420     C                   Z-ADD     W98EAC        W98
732500060420     C                   EXSR      W98999
732600060420     C                   Z-ADD     D98           D98hEAC
732700930423     C* ORE X LASC. AVVISO INTERNE
732800941228     C                   Z-ADD     W98HLI        W98
732900941228     C                   EXSR      W98999
733000941228     C                   Z-ADD     D98           D98HLI
733100930423     C* ORE X LASC. AVVISO CLIENTE
733200941228     C                   Z-ADD     W98HLC        W98
733300941228     C                   EXSR      W98999
733400941228     C                   Z-ADD     D98           D98HLC
733500930423     C* ORE X GIACENZA INTERNE
733600941228     C                   Z-ADD     W98HGI        W98
733700941228     C                   EXSR      W98999
733800941228     C                   Z-ADD     D98           D98HGI
733900930423     C* ORE X GIACENZA CLIENTE
734000941228     C                   Z-ADD     W98HGC        W98
734100941228     C                   EXSR      W98999
734200941228     C                   Z-ADD     D98           D98HGC
734300941228     C* ORE X FEST. PART. INTERNE
734400941228     C                   Z-ADD     W98HPI        W98
734500941228     C                   EXSR      W98999
734600941228     C                   Z-ADD     D98           D98HPI
734700941228     C* ORE X FEST. PART. CLIENTE
734800941228     C                   Z-ADD     W98HPC        W98
734900941228     C                   EXSR      W98999
735000941228     C                   Z-ADD     D98           D98HPC
735100930423     C* ORE X FEST. TRAZ. INTERNE
735200941228     C                   Z-ADD     W98HFI        W98
735300941228     C                   EXSR      W98999
735400941228     C                   Z-ADD     D98           D98HFI
735500930423     C* ORE X FEST. TRAZ. CLIENTE
735600941228     C                   Z-ADD     W98HFC        W98
735700941228     C                   EXSR      W98999
735800941228     C                   Z-ADD     D98           D98HFC
735900930423     C* ORE X FEST. CONS. INTERNE
736000941228     C                   Z-ADD     W98HEI        W98
736100941228     C                   EXSR      W98999
736200941228     C                   Z-ADD     D98           D98HEI
736300930423     C* ORE X FEST. CONS. CLIENTE
736400941228     C                   Z-ADD     W98HEC        W98
736500941228     C                   EXSR      W98999
736600941228     C                   Z-ADD     D98           D98HEC
736700930511     C* ORE X F.DEPOSITO INTERNE
736800941228     C                   Z-ADD     W98HDI        W98
736900941228     C                   EXSR      W98999
737000941228     C                   Z-ADD     D98           D98HDI
737100930511     C* ORE X F.DEPOSITO CLIENTE
737200941228     C                   Z-ADD     W98HDC        W98
737300941228     C                   EXSR      W98999
737400941228     C                   Z-ADD     D98           D98HDC
737500930511     C* ORE X CONTRASSEGNO INTERNE
737600941228     C                   Z-ADD     W98HOI        W98
737700941228     C                   EXSR      W98999
737800941228     C                   Z-ADD     D98           D98HOI
737900930511     C* ORE X CONTRASSEGNO CLIENTE
738000941228     C                   Z-ADD     W98HOC        W98
738100941228     C                   EXSR      W98999
738200941228     C                   Z-ADD     D98           D98HOC
738300930914     C* ORE DI CONSEGNABILITA' INTERNE
738400941228     C                   Z-ADD     W98OCI        W98
738500941228     C                   EXSR      W98000
738600941228     C                   Z-ADD     D98           D98OCI
738700930914     C* ORE DI CONSEGNABILITA' CLIENTE
738800941228     C                   Z-ADD     W98OCC        W98
738900941228     C                   EXSR      W98000
739000941228     C                   Z-ADD     D98           D98OCC
739100071122     C
739200071122     c* Per i codici bolla M prepagati annullati, azzero tutto
739300071122     c                   if        d98cbo='M '
739400071122     c                   clear                   d98rpi
739500071122     c                   clear                   d98rpc
739600071122     c                   clear                   d98rti
739700071122     c                   clear                   d98rtc
739800071122     c                   clear                   d98rci
739900071122     c                   clear                   d98rcc
740000071122     c                   clear                   d98rri
740100071122     c                   clear                   d98rrc
740200071122     c                   clear                   d98fpi
740300071122     c                   clear                   d98fpc
740400071122     c                   clear                   d98fti
740500071122     c                   clear                   d98ftc
740600071122     c                   clear                   d98fci
740700071122     c                   clear                   d98fcc
740800071122     c                   clear                   d98fri
740900071122     c                   clear                   d98frc
741000071122     c                   clear                   d98npi
741100071122     c                   clear                   d98npc
741200071122     c                   clear                   d98nti
741300071122     c                   clear                   d98ntc
741400071122     c                   clear                   d98nci
741500071122     c                   clear                   d98ncc
741600071122     c                   clear                   d98nri
741700071122     c                   clear                   d98nrc
741800071122     c                   clear                   d98OCi
741900071122     c                   clear                   d98OCC
742000071122     c                   endif
742100930423     C*
742200100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
742300100308     c*  calcolo
742400100308     c                   eval      d98dcr=s98dcr
742500100308     c                   eval      d98hcr=s98hcr
742600100308     c                   eval      d98tcr=s98tcr
742700100308     c
742800930423     C                   ENDSR
742900941228     C***********************************************************************
743000941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
743100941228     C***********************************************************************
743200941228     C     W98999        BEGSR
743300941228     C     W98           IFLT      -999
743400941228     C                   Z-ADD     -999          D98               3 0
743500941228     C                   ELSE
743600941228     C     W98           IFGT      999
743700941228     C                   Z-ADD     999           D98
743800941228     C                   ELSE
743900941228     C                   Z-ADD     W98           D98
744000941228     C                   END
744100941228     C                   END
744200941228     C                   ENDSR
744300941228     C***********************************************************************
744400941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
744500941228     C***********************************************************************
744600941228     C     W98000        BEGSR
744700941228     C     W98           IFLE      *ZEROS
744800941228     C                   Z-ADD     *ZEROS        D98
744900941228     C                   ELSE
745000941228     C     W98           IFGT      999
745100941228     C                   Z-ADD     999           D98
745200941228     C                   ELSE
745300941228     C                   Z-ADD     W98           D98
745400941228     C                   END
745500941228     C                   END
745600941228     C                   ENDSR
745700930305     C***********************************************************************
745800930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
745900930305     C***********************************************************************
746000930305     C     AZZERA        BEGSR
746100150219     c* data e ora del momento
746200150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
746300150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
746400150219     C                   Z-ADD     DTORA4        WHHODI            4 0
746500930305     C*
746600141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
746700150603     C                   MOVEL     'N'           opendan
746800141107     c                   clear                   or_ddc
746900141108     c                   clear                   sav_12dat         8 0
747000141107     c                   clear                   or_datarif
747100140402     c                   clear                   wpick2            1
747200140402     c                   clear                   Primotipo
747300090928     c                   clear                   Primodev
747400090928     c                   clear                   Primohev
747500070827     c                   clear                   CallAR5
747600071122     c                   clear                   d98cbo
747700100308     c                   clear                   s98dcr
747800100308     c                   clear                   s98hcr
747900100308     c                   clear                   s98tcr
748000141106     c                   clear                   nosomma           1
748100141229     c                   clear                   wtrasm
748200150324     c                   clear                   avvcmc
748300150325     c                   clear                   DSbl4L
748400150401     c                   clear                   fnlv82ds
748500150331     c                   clear                   DS7R
748600150605     c                   clear                   wDEE              1
748700150629     c                   clear                   wtsogliaE
748800930323     C                   MOVEL     '0'           D98ERR
748900930323     C                   MOVEL     *BLANKS       D98MSG
749000931124     C                   Z-ADD     *ZEROS        D98TPI
749100931124     C                   Z-ADD     *ZEROS        D98TPC
749200930325     C                   Z-ADD     *ZEROS        D98TTI
749300930325     C                   Z-ADD     *ZEROS        D98TTC
749400930325     C                   Z-ADD     *ZEROS        D98TCI
749500930325     C                   Z-ADD     *ZEROS        D98TCC
749600090827     C                   Z-ADD     *ZEROS        w99TCI
749700090827     C                   Z-ADD     *ZEROS        w99TCC
749800930325     C                   Z-ADD     *ZEROS        D98TRI
749900930325     C                   Z-ADD     *ZEROS        D98TRC
750000090921     C                   Z-ADD     *ZEROS        w99TRI
750100090921     C                   Z-ADD     *ZEROS        w99TRC
750200931124     C                   Z-ADD     *ZEROS        D98RPI
750300931124     C                   Z-ADD     *ZEROS        D98RPC
750400930405     C                   Z-ADD     *ZEROS        D98RTI
750500930405     C                   Z-ADD     *ZEROS        D98RTC
750600930405     C                   Z-ADD     *ZEROS        D98RCI
750700930405     C                   Z-ADD     *ZEROS        D98RCC
750800930405     C                   Z-ADD     *ZEROS        D98RRI
750900930405     C                   Z-ADD     *ZEROS        D98RRC
751000931124     C                   Z-ADD     *ZEROS        D98FPI
751100931124     C                   Z-ADD     *ZEROS        D98FPC
751200930325     C                   Z-ADD     *ZEROS        D98FTI
751300930325     C                   Z-ADD     *ZEROS        D98FTC
751400930325     C                   Z-ADD     *ZEROS        D98FCI
751500930325     C                   Z-ADD     *ZEROS        D98FCC
751600930325     C                   Z-ADD     *ZEROS        D98FRI
751700930325     C                   Z-ADD     *ZEROS        D98FRC
751800931124     C                   MOVEL     *BLANKS       D98CPI
751900931124     C                   MOVEL     *BLANKS       D98CPC
752000930325     C                   MOVEL     *BLANKS       D98CTI
752100930325     C                   MOVEL     *BLANKS       D98CTC
752200930325     C                   MOVEL     *BLANKS       D98CCI
752300930325     C                   MOVEL     *BLANKS       D98CCC
752400930325     C                   MOVEL     *BLANKS       D98CRI
752500930325     C                   MOVEL     *BLANKS       D98CRC
752600931124     C                   Z-ADD     *ZEROS        D98NPI
752700931124     C                   Z-ADD     *ZEROS        D98NPC
752800930325     C                   Z-ADD     *ZEROS        D98NTI
752900930325     C                   Z-ADD     *ZEROS        D98NTC
753000930325     C                   Z-ADD     *ZEROS        D98NCI
753100930325     C                   Z-ADD     *ZEROS        D98NCC
753200930325     C                   Z-ADD     *ZEROS        D98NRI
753300930325     C                   Z-ADD     *ZEROS        D98NRC
753400930414     C                   Z-ADD     *ZEROS        D98HHI
753500930414     C                   Z-ADD     *ZEROS        D98HHC
753600930414     C                   Z-ADD     *ZEROS        D98HOI
753700930414     C                   Z-ADD     *ZEROS        D98HOC
753800930414     C                   Z-ADD     *ZEROS        D98HLI
753900930414     C                   Z-ADD     *ZEROS        D98HLC
754000930414     C                   Z-ADD     *ZEROS        D98HGI
754100930414     C                   Z-ADD     *ZEROS        D98HGC
754200931124     C                   Z-ADD     *ZEROS        D98HPI
754300931124     C                   Z-ADD     *ZEROS        D98HPC
754400930414     C                   Z-ADD     *ZEROS        D98HFI
754500930414     C                   Z-ADD     *ZEROS        D98HFC
754600930416     C                   Z-ADD     *ZEROS        D98HEI
754700930416     C                   Z-ADD     *ZEROS        D98HEC
754800930414     C                   Z-ADD     *ZEROS        D98HRI
754900930414     C                   Z-ADD     *ZEROS        D98HRC
755000930511     C                   Z-ADD     *ZEROS        D98HDI
755100930511     C                   Z-ADD     *ZEROS        D98HDC
755200930325     C                   Z-ADD     *ZEROS        D98DCI
755300930325     C                   Z-ADD     *ZEROS        D98HCI
755400930325     C                   Z-ADD     *ZEROS        D98DCE
755500930325     C                   Z-ADD     *ZEROS        D98HCE
755600931125     C                   Z-ADD     *ZEROS        D98OCI
755700931125     C                   Z-ADD     *ZEROS        D98OCC
755800931202     C                   Z-ADD     *ZEROS        D98DEI
755900931202     C                   Z-ADD     *ZEROS        D98OEI
756000931202     C                   Z-ADD     *ZEROS        D98DEE
756100931202     C                   Z-ADD     *ZEROS        D98OEE
756200151019     c                   CLEAR                   D98FLP
756300150325     C                   clear                   D98dtctd
756400150326     C                   clear                   D98picklim
756500150325     C                   clear                   D98spcdee
756600150331     C                   clear                   fddata
756700140825     c
756800141017     c                   clear                   d98ddc
756900141017     c                   clear                   d98cons
757000141017     c                   clear                   wd98ndc
757100140910     c                   clear                   wd98fbc
757200150119     c                   clear                   wd98acc
757300140825     c                   clear                   wtrulor
757400150219     c
757500150219     c                   clear                   IMPdata
757600150219     c                   clear                   IMPfgs
757700150219     c                   clear                   DISdata
757800150219     c                   clear                   DISfgs
757900150219     c                   clear                   DISora
758000150219     c                   clear                   DISnpg
758100150219     c                   clear                   IDnpg
758200150219     c                   clear                   IDdata
758300150219     c                   clear                   IDora
758400150219     c                   clear                   IDfgs
758500931125     C* CAMPI VARI
758600931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
758700140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
758800140825      *   DALLA LETTURA DI QUESTI FILE                               *
758900931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
759000931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
759100931125     C     D98NSP        IFNE      *ZEROS
759200930525     C                   MOVEL     *BLANKS       D98TSP
759300941228     C                   MOVEL     *BLANKS       D98CAD
759400970728     C                   MOVEL     *BLANKS       D98LOD
759500930525     C                   Z-ADD     *ZEROS        D98TFP
759600930525     C                   Z-ADD     *ZEROS        D98TFA
759700930525     C                   Z-ADD     *ZEROS        D98LNA
759800930525     C                   Z-ADD     *ZEROS        D98DSP
759900941228     C                   Z-ADD     *ZEROS        D98MGS
760000930525     C                   Z-ADD     *ZEROS        D98DTI
760100930525     C                   Z-ADD     *ZEROS        D98HTI
760200930525     C                   Z-ADD     *ZEROS        D98DCM
760300930525     C                   Z-ADD     *ZEROS        D98HMC
760400930525     C                   MOVEL     *BLANKS       D98GC1
760500930525     C                   MOVEL     *BLANKS       D98GC2
760600010820     C                   Z-ADD     *ZEROS        D98CAS
760700010821     C                   MOVEL     *BLANKS       D98VCA
760800930525     C                   MOVEL     *BLANKS       D98TIC
760900930525     C                   Z-ADD     *ZEROS        D98DCR
761000930525     C                   Z-ADD     *ZEROS        D98HCR
761100930525     C                   MOVEL     *BLANKS       D98TCR
761200050914     C                   MOVEL     *BLANKS       Consideraora      1
761300930525     C                   MOVEL     *BLANKS       D98CMC
761400930525     C                   Z-ADD     *ZEROS        D98DAG
761500930525     C                   Z-ADD     *ZEROS        D98DCG
761600930525     C                   MOVEL     *BLANKS       D98FFD
761700930525     C                   Z-ADD     *ZEROS        D98NCD
761800930525     C                   MOVEL     *BLANKS       D98FNS
761900931124     C                   Z-ADD     *ZEROS        D98DAM
762000940628     C                   MOVEL     *BLANKS       D98FIN
762100940628     C                   Z-ADD     *ZEROS        D98KSC
762200940628     C                   MOVEL     *BLANKS       D98TBL
762300931125     C                   END
762400960229     C     D98TLA        IFEQ      'T'
762500960229     C                   Z-ADD     *ZEROS        D98DTI
762600960229     C                   Z-ADD     *ZEROS        D98HTI
762700960229     C                   ENDIF
762800931125     C* CAMPI WORK
762900930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
763000930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
763100150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
763200930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
763300941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
763400960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
763500930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
763600930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
763700940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
763800930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
763900150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
764000931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
764100931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
764200150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
764300930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
764400930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
764500930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
764600930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
764700930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
764800940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
764900941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
765000930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
765100050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
765200050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
765300140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
765400930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
765500930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
765600930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
765700930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
765800930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
765900930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
766000930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
766100930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
766200930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
766300930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
766400930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
766500930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
766600930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
766700930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
766800140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
766900931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
767000060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
767100960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
767200931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
767300140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
767400140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
767500150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
767600140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
767700941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
767800970304     C******************** Z-ADD*ZEROS    W98DBR  80
767900970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
768000150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
768100050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
768200930414     C*
768300941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
768400941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
768500930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
768600930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
768700930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
768800930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
768900930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
769000930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
769100941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
769200941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
769300930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
769400930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
769500930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
769600930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
769700930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
769800930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
769900941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
770000941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
770100930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
770200930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
770300930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
770400930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
770500930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
770600930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
770700930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
770800930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
770900060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
771000060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
771100060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
771200060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
771300930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
771400930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
771500930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
771600930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
771700930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
771800930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
771900930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
772000930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
772100930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
772200930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
772300930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
772400930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
772500930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
772600930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
772700941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
772800941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
772900930423     C*
773000930511     C                   Z-ADD     *ZEROS        W98HF             5 0
773100930423     C                   Z-ADD     *ZEROS        W98HH             5 0
773200060418     C                   Z-ADD     *ZEROS        W98EG             5 0
773300060420     C                   Z-ADD     *ZEROS        W98EA             5 0
773400930423     C                   Z-ADD     *ZEROS        W98HG             5 0
773500930423     C                   Z-ADD     *ZEROS        W98HL             5 0
773600930423     C                   Z-ADD     *ZEROS        W98HO             5 0
773700930511     C                   Z-ADD     *ZEROS        W98HD             5 0
773800930611     C*
773900930611     C                   Z-ADD     *ZEROS        WORELA            3 0
774000960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
774100930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
774200930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
774300931207     C                   Z-ADD     *ZEROS        WORECR            3 0
774400090902     c
774500090902     c                   clear                   wmancaTRAZ
774600150113     c                   clear                   wcaldpcee         1
774700150220     c                   clear                   evemic            2
774800930305     C*
774900060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
775000060406     C                   clear                   LAF                            *LASC.AVVISO FINE
775100091014     C                   clear                   LAp                            *posizione l.avviso
775200060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
775300060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
775400060406c    c                   clear                   LCon
775500060406     C                   clear                   EVD                            *DATA EVENTO
775600060406     C                   clear                   EVO                            *ORA EVENTO
775700060406     C                   clear                   EVC                            *CAUSALE EVENTO
775800131218     C                   clear                   EVT
775900060406     C                   clear                   pVD                            *DATA EVENTO
776000060406     C                   clear                   pVO                            *ORA EVENTO
776100060406     C                   clear                   pVC                            *CAUSALE EVENTO
776200060406     C                   clear                   pVF                            *CAUSALE EVENTO
776300060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
776400060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
776500060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
776600060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
776700060406     C                   clear                   COV                            *DATE FINE L.A. E GI
776800060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
776900060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
777000091014     C                   clear                   GIp                            *posizione vento gia
777100060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
777200060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
777300060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
777400931213     C*
777500060406     C                   clear                   TUC                            *TURNI CHIUSURA
777600931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
777700931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
777800060406     c
777900060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
778000060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
778100060406     c                   clear                   pgii
778200060406     c                   clear                   pgif
778300060406     c                   clear                   pgci
778400060406     c                   clear                   pgcf
778500060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
778600060420     c                   clear                   paii
778700060420     c                   clear                   paif
778800930420     C*
778900930305     C                   ENDSR
779000930512     C***********************************************************************
779100930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
779200930512     C***********************************************************************
779300930512     C     FORCON        BEGSR
779400930512     C*
779500931124     C                   Z-ADD     *ZEROS        D98RPI
779600931124     C                   Z-ADD     *ZEROS        D98RPC
779700930512     C                   Z-ADD     *ZEROS        D98RTI
779800930512     C                   Z-ADD     *ZEROS        D98RTC
779900930512     C                   Z-ADD     *ZEROS        D98RCI
780000930512     C                   Z-ADD     *ZEROS        D98RCC
780100930512     C                   Z-ADD     *ZEROS        D98RRI
780200930512     C                   Z-ADD     *ZEROS        D98RRC
780300931124     C                   Z-ADD     *ZEROS        D98FPI
780400931124     C                   Z-ADD     *ZEROS        D98FPC
780500930512     C                   Z-ADD     *ZEROS        D98FTI
780600930512     C                   Z-ADD     *ZEROS        D98FTC
780700930512     C                   Z-ADD     *ZEROS        D98FCI
780800930512     C                   Z-ADD     *ZEROS        D98FCC
780900930512     C                   Z-ADD     *ZEROS        D98FRI
781000930512     C                   Z-ADD     *ZEROS        D98FRC
781100931124     C                   MOVEL     'S'           D98CPI
781200931124     C                   MOVEL     'S'           D98CPC
781300930512     C                   MOVEL     'S'           D98CTI
781400930512     C                   MOVEL     'S'           D98CTC
781500930512     C                   MOVEL     'S'           D98CCI
781600930512     C                   MOVEL     'S'           D98CCC
781700930512     C                   MOVEL     'S'           D98CRI
781800930512     C                   MOVEL     'S'           D98CRC
781900931124     C                   Z-ADD     *ZEROS        D98NPI
782000931124     C                   Z-ADD     *ZEROS        D98NPC
782100930512     C                   Z-ADD     *ZEROS        D98NTI
782200930512     C                   Z-ADD     *ZEROS        D98NTC
782300930512     C                   Z-ADD     *ZEROS        D98NCI
782400930512     C                   Z-ADD     *ZEROS        D98NCC
782500930512     C                   Z-ADD     *ZEROS        D98NRI
782600930512     C                   Z-ADD     *ZEROS        D98NRC
782700930512     C                   Z-ADD     *ZEROS        D98HHI
782800930512     C                   Z-ADD     *ZEROS        D98HHC
782900930512     C                   Z-ADD     *ZEROS        D98HOI
783000930512     C                   Z-ADD     *ZEROS        D98HOC
783100930512     C                   Z-ADD     *ZEROS        D98HLI
783200930512     C                   Z-ADD     *ZEROS        D98HLC
783300930512     C                   Z-ADD     *ZEROS        D98HGI
783400930512     C                   Z-ADD     *ZEROS        D98HGC
783500931124     C                   Z-ADD     *ZEROS        D98HPI
783600931124     C                   Z-ADD     *ZEROS        D98HPC
783700930512     C                   Z-ADD     *ZEROS        D98HFI
783800930512     C                   Z-ADD     *ZEROS        D98HFC
783900930512     C                   Z-ADD     *ZEROS        D98HEI
784000930512     C                   Z-ADD     *ZEROS        D98HEC
784100930512     C                   Z-ADD     *ZEROS        D98HRI
784200930512     C                   Z-ADD     *ZEROS        D98HRC
784300930512     C                   Z-ADD     *ZEROS        D98HDI
784400930512     C                   Z-ADD     *ZEROS        D98HDC
784500930520     C*
784600930512     C                   ENDSR
784700930305     C********************************************************************
784800971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
784900950406031059090120151181212243273304334365
785000950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
785100950406312831303130313130313031
785200930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
785300971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
785400971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
785500971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
785600971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
785700971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
785800971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
785900971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
786000971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
786100971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
786200971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
786300971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
786400971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
786500971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
786600971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
786700971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
786800971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
786900971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
787000971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
787100971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
787200971127TNSD99R- Bolla inesistente.                                               BOIN20
787300971127TNSD99R-     Cap inesistente.                                             CAPI21
787400971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
787500971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
787600971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
787700971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
787800971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
787900971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
788000971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
788100971127TNSD99R- Contrassegno non trovato.                                        CONT29
788200971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
788300990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
788400990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
788500060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
788600060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
788700971127TNSD99R- 35                                                                   35
788800971127TNSD99R- 36                                                                   36
788900971127TNSD99R- 37                                                                   37
789000971127TNSD99R- 38                                                                   38
789100971127TNSD99R- 39                                                                   39
789200971127TNSD99R- 40                                                                   40
