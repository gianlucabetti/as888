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
035200150330     d wdiff           s              3  0
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
070200970310     C                   Z-ADD     *ZEROS        W98HUC
070300071122     c
070400071122     c                   movel     arbcbo        d98cbo
070500140820     c                   z-add     arbddc        d98ddc
070600140825     c                   z-add     arbndc        wd98ndc
070700140911     c                   movel     arbfbc        wd98fbc
070800150119     c                   movel     arbacc        wd98acc
070900150325     c                   exsr      IN_dist
071000150331     c* Se in fermo deposito vedo se presente particlarità consegna
071100150331     c                   if        arbffd='S' and arbgma<>*blanks
071200150331     C                   Z-ADD     CODUT         KTBUTE
071300150331     C                   MOVEL     '7R'          KTBCOD
071400150331     C                   MOVEL(P)  arbgma        KTBKEY
071500150331     C     KEYTAB        chain     TABEL00F
071600150331     c                   if        %found(tabel00f)
071700150331     c                   movel     tbluni        ds7r
071800150331     c                   endif
071900150331     c                   endif
072000150325     c
072100070511     c
072200141215     C                   Z-ADD     ARBDBR        D98DBR
072300141215     c                   eval      d98fdrt=' '
072400070511     c
072500141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
072600070511     c* La data ritiro è presente solo su fnblp
072700070511     C     KEYARB        CHAIN     FNblp01L                           99
072800141215     c                   if        %found(fnblp01l)
072900070511     C                   Z-ADD     blpDrt        D98DBR
073000070511     c                   eval      d98fdrt='S'
073100070511     c                   endif
073200141215     c******             endif
073300070511     c
073400110923     C                   movel     ARBxco        D98xco
073500110923     C                   Z-ADD     ARBDAM        D98DAM
073600941227     C                   MOVEL     ARBFIN        D98FIN
073700990308     C                   MOVEL     ARBFBR        WFBR
073800031201     C                   MOVEL     ARBGGA        WGGA
073900050901     C                   MOVEL     ARBcca        d98cca
074000051124     C                   MOVEL     ARBTC1        d98TC1
074100051124     C                   MOVEL     ARBTC2        d98TC2
074200941227     C                   Z-ADD     1             I
074300941227     C     ARBCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
074400941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
074500941227     C                   MOVEL     '1'           D98ERR
074600941227     C                   MOVEL     MSG(23)       D98MSG                         *COD.BOLLA NON TROV.
074700941227     C                   ELSE                                                   <-- 2 -->
074800941227     C                   MOVEL     T3A(I)        D98TBL
074900941228     C                   Z-ADD     F3A(I)        WFCA              1 0          *FLAG CONTRASSEGNO
075000990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
075100941227     C                   END                                                    <-- 2 ---
075200130826
075300130826     c* in d98ksc passo il MITTENTE ovvero CCM se assegnato, il ksc se franco
075400130826     c                   if        %subst(d98tbl:1:1) ='F'
075500130826     C                   Z-ADD     ARBKSC        D98KSC
075600130826     c                   else
075700130826     c
075800130826     c                   if        arbccm>0
075900130826     C                   Z-ADD     ARBccm        D98KSC
076000130826     c                   endif
076100130826     c                   if        d98ksc=0
076200130826     c                   z-add     8888          d98ksc
076300130826     c                   movel     d98lnp        d98ksc
076400130826     c                   endif
076500130826     c                   endif
076600130826     c
076700941227     C                   END                                                    <-- 1 ---
076800941228     C*
076900051115     C* LEGGO IL FILE FiAR901L (CONTRASSEGNI)
077000950102     C                   MOVEL     'N'           WFLASS            1            *FLAG CONTRASS.TROVA
077100941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
077200941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
077300941228     C                   Z-ADD     D98AAS        AR9AAS
077400941228     C                   Z-ADD     D98LNP        AR9LNP
077500941228     C                   Z-ADD     D98NRS        AR9NRS
077600941228     C                   Z-ADD     D98NSP        AR9NSP
077700051115     C     KEYAR9        CHAIN     FiAR901L                           99
077800950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
077900950102     C                   MOVEL     'S'           WFLASS
078000950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
078100950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
078200950102     C                   Z-ADD     D98AAS        LBLAAN
078300950102     C                   Z-ADD     D98LNP        LBLLPN
078400950102     C                   Z-ADD     D98NRS        LBLNRN
078500950102     C                   Z-ADD     D98NSP        LBLNSN
078600950102     C     KEYLBL        CHAIN     FNLBL01L                           99
078700950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
078800950102     C                   Z-ADD     LBLAAP        AR9AAS
078900950102     C                   Z-ADD     LBLLPP        AR9LNP
079000950102     C                   Z-ADD     LBLNRP        AR9NRS
079100950102     C                   Z-ADD     LBLNSP        AR9NSP
079200051115     C     KEYAR9        CHAIN     FiAR901L                           99
079300950102     C     *IN99         IFEQ      '0'
079400950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
079500950102     C                   ENDIF
079600950102     C                   ENDIF                                                  <-- 3 ---
079700950102     C                   ENDIF                                                  <-- 2 ---
079800950102     C*
079900950102     C* CONTRASSEGNO NON TROVATO
080000950102     C     WFLASS        IFEQ      'N'                                          --- 2 -->
080100941228     C                   ELSE                                                   <-- 2 -->
080200950102     C* CONTRASSEGNO TROVATO
080300941228     C                   MOVEL     AR9TIC        D98TIC                         *TIPO INCASSO CONTR.
080400010822     C                   Z-ADD     AR9CAS        D98CAS                         *IMPORTO CONTRASSEGN
080500010822     C                   MOVEL     AR9VCA        D98VCA                         *DIVISA CONTRASEGNO
080600941228     C                   ENDIF                                                  <-- 2 ---
080700941228     C                   ENDIF                                                  <-- 1 ---
080800150325     c
080900941227     C*
081000050405     C* LEGGO IL FILE TIGCP21L (GIACENZA)
081100941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
081200050405     C                   Z-ADD     D98AAS        KWGCPAAS
081300050405     C                   Z-ADD     D98LNP        KWGCPLNP
081400050405     C                   Z-ADD     D98NRS        KWGCPNRS
081500050405     C                   Z-ADD     D98NSP        KWGCPNSP
081600050405     C                   Z-ADD     *ZEROS        KWGCPFRG
081700050405     C     KEYGCP21      CHAIN     TIGCP21L                           99
081800941228     C     *IN99         IFEQ      '0'                                          *BOLLA TROV.
081900941227     C     GCPATB        ANDNE     'A'
082000960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
082100960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
082200960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
082300050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
082400960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
082500960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
082600050405     C* O UGUALE A 35 INVECE DI 40)
082700960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
082800050405     C     GCPFAS        ANDGE     40
082900941227     C                   Z-ADD     GCPDCG        D98DCG
083000950112     C                   Z-ADD     GCPDCG        W98DCG
083100960123     C                   ENDIF
083200960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
083300960729     C     GCPFAS        ANDGE     35
083400070202     C     GCPFAS        ANDne     36
083500960729     C                   Z-ADD     GCPDCG        D98DCG
083600960729     C                   Z-ADD     GCPDCG        W98DCG
083700960729     C                   ENDIF
083800941228     C                   ENDIF
083900941228     C                   ENDIF                                                  <-- 1 ---
084000970911     C*
084100970911     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
084200970911     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
084300970911     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
084400050922     C*****              Z-ADD     D98DTI        W98DTI
084500971210     C                   Z-ADD     D98LNP        F
084600971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
084700970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
084800970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
084900150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
085000150120     c                   if        arbdcp=0 and d98dfc=0
085100150120     C                   Z-ADD     d98dam        D98DTI
085200150120     c                   endif
085300150120     c                   if        arbdcp=0 and d98dfc>0  and d98dam<=d98dfc
085400150120     C                   Z-ADD     d98dam        D98DTI
085500150120     c                   endif
085600150120     c                   ENDIF
085700150120     c***
085800150120     C***                Z-ADD     D98AAS        ARTAAS
085900150120     C***                Z-ADD     D98LNP        ARTLNP
086000150120     C***                Z-ADD     D98NRS        ARTNRS
086100150120     C***                Z-ADD     D98NSP        ARTNSP
086200150120     C***  KEYART        SETLL     FNART01L                           99
086300150120     C**N99KEYART        READE     FNART01L                               99
086400150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
086500150120     C***  ARTATB        IFEQ      *BLANKS                                      --- 3 -->
086600970911     C* NON C'E' DATA CONSEGNAB. IN INPUT
086700150120     C***  D98DFC        IFEQ      *ZEROS                                       --- 4 -->
086800970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
086900970911     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
087000150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
087100150120     C***  ARTDCM        ANDEQ     *ZEROS
087200970912     C* SE PRIMO REC. TROVATO, MEM. DATA
087300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 6 -->
087400150120     C***                Z-ADD     ARTDAM        D98DTI
087500150120     C***                ELSE                                                   <-- 6 -->
087600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
087700150120     C***  ARTDAM        IFLT      D98DTI                                       --- 7 -->
087800150120     C***                Z-ADD     ARTDAM        D98DTI
087900150120     C***                ENDIF                                                  <-- 7 ---
088000150120     C***                ENDIF                                                  <-- 6 ---
088100150120     C***                ENDIF                                                  <-- 5 ---
088200970911     C* C'E' DATA CONSEGNAB.
088300150120     C***                ELSE                                                   <-- 4 -->
088400970911     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
088500970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
088600150120     C***  ARTDAM        IFGT      *ZEROS                                       --- 5 -->
088700150120     C***  ARTDAM        ANDLE     D98DFC
088800970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
088900970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
089000150120     C***  ARTDCM        IFEQ      *ZEROS                                       --- 6 -->
089100150120     C***  ARTDCM        ORGT      D98DFC
089200970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
089300150120     C***  D98DTI        IFEQ      *ZEROS                                       --- 7 -->
089400150120     C***                Z-ADD     ARTDAM        D98DTI
089500150120     C***                ELSE                                                   <-- 7 -->
089600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
089700150120     C***  ARTDAM        IFLT      D98DTI
089800150120     C***                Z-ADD     ARTDAM        D98DTI
089900150120     C***                ENDIF
090000150120     C***                ENDIF                                                  <-- 7 ---
090100150120     C***                ENDIF                                                  <-- 6 ---
090200150120     C***                ENDIF                                                  <-- 5 ---
090300150120     C***                ENDIF                                                  <-- 4 ---
090400150120     C***                ENDIF                                                  <-- 3 ---
090500150120     C***  KEYART        READE     FNART01L                               99
090600150120     C***                ENDDO                                                  <-- 2 ---
090700150120     C***                ENDIF                                                  <-- 1 ---
090800941227     C*
090900941227     C                   ENDSR
091000150325     C********************************************************************
091100150325     C*   Verifico se bolla in distinta
091200150325     C********************************************************************
091300150325     c     IN_dist       BEGSR
091400150325     c* Determino se " in DISTINTA"  ( ignoro i soli incasso)
091500150325     c                   if         ARBfbc = *blanks and
091600150325     c                               ARBddc > 0 and
091700150325     c                               ARBndc > 0 and ARBdcm=0
091800150325     C* PER ESSERE DAVVERO IN DISTINTA aut no fittizio
091900150325     c                   if        not %open(fiapd01l)
092000150325     c                   open      fiapd01l
092100150325     c                   endif
092200150325     c
092300150325     C                   move      'A'           kpdtip
092400150325     c                   eval      kpdpdr=arbpdc
092500150325     C     kfapd         chain     fiapd01l
092600150325     c
092700150325     c                   if        %found(fiapd01l)  and apdfcm= ' '
092800150325     c                   eval       d98cons = 'S'
092900150325     c                   endif
093000150325     c                   endif
093100150325     c                   ENDSR
093200941227     C********************************************************************
093300941227     C*   LETBLP - LEGGE IL FILE FNBLP (BOLLE PARTENZA)
093400941227     C********************************************************************
093500941227     C     LETBLP        BEGSR
093600150331     c                   clear                   dsbl4l
093700150331     c                   clear                   ds7R
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
102000970310     C                   Z-ADD     *ZEROS        W98HUC
102100071122     c                   movel     blpcbo        d98cbo
102200150331     c*
102300150331     c* Se in fermo deposito vedo se presente particlarità consegna
102400150331     c                   if        blpffd='S' and blpgma<>*blanks
102500150331     C                   Z-ADD     CODUT         KTBUTE
102600150331     C                   MOVEL     '7R'          KTBCOD
102700150331     C                   MOVEL(P)  blpgma        KTBKEY
102800150331     C     KEYTAB        chain     TABEL00F
102900150331     c                   if        %found(tabel00f)
103000150331     c                   movel     tbluni        ds7r
103100150331     c                   endif
103200150331     c                   endif
103300150331     c
103400970304     C                   Z-ADD     BLPDBR        D98DBR
103500070514     c                   eval      d98fdrt=' '
103600070514     c
103700141215     c*******            if        wdata8>=§vponpi  and §vponpi>0
103800070514     C                   Z-ADD     blpDrt        D98DBR
103900070514     c                   eval      d98fdrt='S'
104000141215     c********           endif
104100070514     c*
104200941227     C                   MOVEL     BLPFIN        D98FIN
104300130826     C****               Z-ADD     BLPKSC        D98KSC
104400990308     C                   MOVEL     BLPFBR        WFBR
104500031201     C                   MOVEL     BLPGGA        WGGA
104600941227     C                   Z-ADD     1             I
104700941228     C     BLPCBO        LOOKUP    C3A(I)                                 99    *CERCA IL TIPO BOLLA
104800941227     C     *IN99         IFEQ      '0'                                          --- 2 -->
104900941227     C                   MOVEL     '1'           D98ERR
105000941227     C                   MOVEL     MSG(23)       D98MSG                         *NON TROVATO
105100941227     C                   ELSE                                                   <-- 2 -->
105200941227     C                   MOVEL     T3A(I)        D98TBL
105300941228     C                   Z-ADD     F3A(I)        WFCA                           *FLAG CONTRASSEGNO
105400990308     C                   MOVEL     R3A(I)        WRBL                           *RAGGRUPP.BOLLE
105500941227     C                   END                                                    <-- 2 ---
105600130826     c                   if        %subst(d98tbl:1:1) ='F'
105700130826     C                   Z-ADD     blpKSC        D98KSC
105800130826     c                   else
105900130826     c
106000130826     c                   if        blpccm>0
106100130826     C                   Z-ADD     blpccm        D98KSC
106200130826     c                   endif
106300130826     c                   if        d98ksc=0
106400130826     c                   z-add     8888          d98ksc
106500130826     c                   movel     d98lnp        d98ksc
106600130826     c                   endif
106700130826     c                   endif
106800130826     c
106900941227     C                   END
107000941228     C*
107100051115     C* LEGGO IL FILE Fiar901L (CONTRASSEGNI)
107200950102     C                   MOVEL     'N'           WFLASS                         *FLAG CONTRASS.TROVA
107300941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
107400941228     C     WFCA          ANDNE     *ZEROS                                       *HA CONTRASSEGNO
107500051115     C                   Z-ADD     D98AAS        ar9AAS
107600051115     C                   Z-ADD     D98LNP        ar9LNP
107700051115     C                   Z-ADD     D98NRS        ar9NRS
107800051115     C                   Z-ADD     D98NSP        ar9NSP
107900051115     C     KEYar9        CHAIN     Fiar901L                           99
108000950102     C     *IN99         IFEQ      '0'                                          --- 2 --> *TROVATO
108100950102     C                   MOVEL     'S'           WFLASS
108200950102     C                   ELSE                                                   <-- 2 --> *NON TROV.
108300950102     C* FACCIO TENTATIVO CON LEGAMI PER TROVARE CONTRASSEGNO DELLA MAMMA
108400950102     C                   Z-ADD     D98AAS        LBLAAN
108500950102     C                   Z-ADD     D98LNP        LBLLPN
108600950102     C                   Z-ADD     D98NRS        LBLNRN
108700950102     C                   Z-ADD     D98NSP        LBLNSN
108800950102     C     KEYLBL        CHAIN     FNLBL01L                           99
108900950102     C     *IN99         IFEQ      '0'                                          --- 3 --> *C'E' LEGA
109000051115     C                   Z-ADD     LBLAAP        ar9AAS
109100051115     C                   Z-ADD     LBLLPP        ar9LNP
109200051115     C                   Z-ADD     LBLNRP        ar9NRS
109300051115     C                   Z-ADD     LBLNSP        ar9NSP
109400051115     C     KEYar9        CHAIN     Fiar901L                           99
109500950102     C     *IN99         IFEQ      '0'
109600950102     C                   MOVEL     'S'           WFLASS                         *CONTRASS.TROVATO
109700950102     C                   ENDIF
109800950102     C                   ENDIF                                                  <-- 3 ---
109900950102     C                   ENDIF                                                  <-- 2 ---
110000950102     C*
110100950102     C* CONTRASSEGNO TROVATO
110200150219     C     WFLASS        IFEQ      'S'                                          --- 2 -->
110300051115     C                   MOVEL     ar9TIC        D98TIC                         *TIPO INCASSO CONTR.
110400051115     C                   Z-ADD     ar9CAS        D98CAS                         *IMPORTO CONTRASSEGN
110500051115     C                   MOVEL     ar9VCA        D98VCA                         *DIVISA CONTRASEGNO
110600941228     C                   ENDIF                                                  <-- 2 ---
110700941228     C                   ENDIF                                                  <-- 1 ---
110800941227     C*
110900050405     C* LEGGO IL FILE TIGCP01L (GIACENZA)
111000941228     C     D98ERR        IFEQ      '0'                                          --- 1 -->
111100050405     C                   Z-ADD     D98AAS        KWGCPAAS
111200050405     C                   Z-ADD     D98LNP        KWGCPLNP
111300050405     C                   Z-ADD     D98NRS        KWGCPNRS
111400050405     C                   Z-ADD     D98NSP        KWGCPNSP
111500050405     C                   Z-ADD     *ZEROS        KWGCPFRG
111600050405     C     KEYGCP        CHAIN     TIGCP01L                           99
111700941228     C     *IN99         IFEQ      '0'                                          *TROV.
111800941227     C     GCPATB        ANDNE     'A'
111900960228$$$$ C******************** Z-ADDGCPDXD    D98DAG
112000960228$$$$ C                   Z-ADD     GCPAGC        DSAAS
112100960228$$$$ C                   Z-ADD     GCPMGC        DSMGS
112200960228$$$$ C                   Z-ADD     DSDAT8        D98DAG                         *DATA APERT.GIAC.
112300050405     C* SE LA FASE DI GIACENZA E' INFERIORE A 40 (EX FASE 38) SIGNIFICA CHE LA GIACENZA
112400960123     C* NON E' ANCORA CHIUSA ANCHE SE C'E' LA DATA DI CHIUSURA GIACENZA
112500960729     C* (SE C'E' CONSEGNABILITA' CONTROLLO SE LA FASE GIACENZA E' MAGGIORE
112600050405     C* O UGUALE A 35 INVECE DI 40)
112700960729     C     FLCONS        IFNE      'S'                                          *NO CONSEGNAB.
112800050405     C     GCPFAS        ANDGE     40
112900941227     C                   Z-ADD     GCPDCG        D98DCG
113000950112     C                   Z-ADD     GCPDCG        W98DCG
113100960123     C                   ENDIF
113200960729     C     FLCONS        IFEQ      'S'                                          *SI CONSEGNAB.
113300960729     C     GCPFAS        ANDGE     35
113400070202     C     GCPFAS        ANDne     36
113500960729     C                   Z-ADD     GCPDCG        D98DCG
113600960729     C                   Z-ADD     GCPDCG        W98DCG
113700960729     C                   ENDIF
113800941228     C                   ENDIF
113900941228     C                   ENDIF                                                  <-- 1 ---
114000970912     C*
114100970912     C* SE BOLLA IN IMPORT ED E LA DATA DI ARRIVO DEL PRIMO COLLO > 0
114200970912     C* E LA DATA DI ARRIVO DELL'ULTIMO COLLO = 0
114300970912     C* CONTROLLO FILE SEGNACOLLI PER CONTROLLARE SE CONSEGNA PARZIALE
114400050922     C******             Z-ADD     D98DTI        W98DTI
114500971210     C                   Z-ADD     D98LNP        F
114600971210     C     FL1(F)        IFEQ      'E'                                          --- 1 -->
114700970912     C     D98DAM        ANDGT     *ZEROS                                       *DATA ARR.MERCE
114800970912     C     D98DTI        ANDEQ     *ZEROS                                       *DATA ARR.TRAZ.
114900150120     c* non vado più sul dettaglio colli ma sulla data prima cons. parziale
115000150120     c                   if        blpdcp=0 and d98dfc=0
115100150120     C                   Z-ADD     d98dam        D98DTI
115200150120     c                   endif
115300150120     c                   if        blpdcp=0 and d98dfc>0  and d98dam<=d98dfc
115400150120     C                   Z-ADD     d98dam        D98DTI
115500150120     c                   endif
115600150120     c                   ENDIF
115700150120     c*
115800150120     C***                Z-ADD     D98AAS        BLTAAS
115900150120     C****               Z-ADD     D98LNP        BLTLNP
116000150120     C***                Z-ADD     D98NRS        BLTNRS
116100150120     C***                Z-ADD     D98NSP        BLTNSP
116200150120     C***  KEYBLT        SETLL     FNBLT01L                           99
116300150120     C**N99KEYBLT        READE     FNBLT01L                               99
116400150120     C***  *IN99         DOWEQ     *OFF                                         --- 2 -->
116500150120     C***  BLTATB        IFEQ      *BLANKS                                      --- 3 -->
116600970912     C* NON C'E' DATA CONSEGNAB. IN INPUT
116700150120     C**   D98DFC        IFEQ      *ZEROS                                       --- 4 -->
116800970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
116900970912     C* E NON CONSEGNATO (DATA CONS.MERCE = 0)
117000150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
117100150120     C**   BLTDCM        ANDEQ     *ZEROS
117200970912     C* SE PRIMO REC. TROVATO, MEM. DATA
117300150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 6 -->
117400150120     C**                 Z-ADD     BLTDAM        D98DTI
117500150120     C**                 ELSE                                                   <-- 6 -->
117600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
117700150120     C**   BLTDAM        IFLT      D98DTI                                       --- 7 -->
117800150120     C**                 Z-ADD     BLTDAM        D98DTI
117900150120     C**                 ENDIF                                                  <-- 7 ---
118000150120     C**                 ENDIF                                                  <-- 6 ---
118100150120     C**                 ENDIF                                                  <-- 5 ---
118200970912     C* C'E' DATA CONSEGNAB.
118300150120     **                  ELSE                                                   <-- 4 -->
118400970912     C* SE COLLO ARRIVATO (DATA ARR.MERCE > 0)
118500970912     C* ED E' ARRIVATO PRIMA DELLA DATA DI CONSEGNAB.
118600150120     C**   BLTDAM        IFGT      *ZEROS                                       --- 5 -->
118700150120     C**   BLTDAM        ANDLT     D98DFC
118800970912     C* SE COLLO NON CONSEGNATO (DATA CONS.MERCE = 0)
118900970912     C* OPPURE E' STATO CONSEGNATO DOPO LA DATA DI CONSEGNAB.
119000150120     C**   BLTDCM        IFEQ      *ZEROS                                       --- 6 -->
119100150120     C**   BLTDCM        ORGT      D98DFC
119200970912     C* SE PRIMO REC. TROVATO, MEM. LA DATA
119300150120     C**   D98DTI        IFEQ      *ZEROS                                       --- 7 -->
119400150120     C**                 Z-ADD     BLTDAM        D98DTI
119500150120     C**                 ELSE                                                   <-- 7 -->
119600970912     C* SE NON E' PRIMO REC. TROVATO, TIENE LA DATA PIU' BASSA
119700150120     C**   BLTDAM        IFLT      D98DTI
119800150120     C**                 Z-ADD     BLTDAM        D98DTI
119900150120     C**                 ENDIF
120000150120     C**                 ENDIF                                                  <-- 7 ---
120100150120     C**                 ENDIF                                                  <-- 6 ---
120200150120     C**                 ENDIF                                                  <-- 5 ---
120300150120     C**                 ENDIF                                                  <-- 4 ---
120400150120     C**                 ENDIF                                                  <-- 3 ---
120500150120     C**   KEYBLT        READE     FNBLT01L                               99
120600150120     C**                 ENDDO                                                  <-- 2 ---
120700150120     C**                 ENDIF                                                  <-- 1 ---
120800941227     C*
120900941227     C                   ENDSR
121000930323     C********************************************************************
121100941227     C*   RTTEOR - RECUPERA I TEMPI TEORICI
121200930323     C********************************************************************
121300930323     C     RTTEOR        BEGSR
121400941227     C*
121500941227     C* RECUPERO IL TEMPO TEORICO DI PARTENZA
121600941228     C     D98ERR        IFEQ      '0'
121700941227     C                   EXSR      RTPART
121800941227     C                   ENDIF
121900941227     C*
122000941227     C* RECUPERO IL TEMPO TEORICO DI TRAZIONE
122100941228     C     D98ERR        IFEQ      '0'
122200150219     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
122300150219     C                   Z-ADD     D98LNP        LTP(1)                         LINEA   /LINEA
122400150219     C                   Z-ADD     D98TFP        LTP(2)                         TERMINAL/LINEA
122500150219     C                   Z-ADD     D98LNP        LTP(3)                         LINEA   /TERMINAL
122600150219     C                   Z-ADD     D98TFP        LTP(4)                         TERMINAL/TERMINAL
122700941227     C                   EXSR      RTTRAZ
122800150219     c                   if        d98err<>'1'
122900150219     C                   Z-ADD     CASTIN        D98TTI
123000150219     C                   Z-ADD     CASTCN        D98TTC
123100150219     C                   endif
123200150219     C                   ENDIF
123300941227     C*
123400941227     C* RECUPERO IL TEMPO TEORICO DI CONSEGNA
123500941228     C     D98ERR        IFEQ      '0'
123600941227     C                   EXSR      RTCONS
123700941227     C                   ENDIF
123800941227     C*
123900941228     C     D98ERR        IFEQ      '0'
124000941227     C*
124100941227     C* CALCOLO ORE DI RESA TEORICHE INTERNE
124200941227     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
124300941227     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
124400090921     c                   z-add     d98tri        w99tri
124500090921     c* per la DS
124600090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
124700090921     c* per i calcoli interi
124800090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
124900090921     c
125000941227     C* CALCOLO ORE DI RESA TEORICHE CLIENTE
125100941227     C                   Z-ADD     D98TPC        D98TRC                         *PARTENZA INTERNO ->
125200941227     C                   ADD       D98TTC        D98TRC                         *TRAZIONE INTERNO ->
125300090921     c                   z-add     d98trc        w99trc
125400090921     c* per la DS
125500090921     C                   ADD       D98TCC        D98TRC                         *CONSEGNA INTERNO ->
125600090921     c* per i calcoli interi
125700090921     C                   ADD       w99TCC        w99TRC                         *CONSEGNA INTERNO ->
125800941227     C*
125900941227     C                   ENDIF
126000930323     C*
126100930323     C                   ENDSR
126200941227     C********************************************************************
126300941227     C*   RTPART - RECUPERA IL TEMPO DI PARTENZA
126400941227     C********************************************************************
126500941227     C     RTPART        BEGSR
126600941228     C*
126700941228     C* IMPOSTO FLAG TIPO SPEDIZIONE (NORMALE,ESPRESSO)
126800941228     C* SE LA BOLLA E' IN PORTO ASSEGNATO O DI TIPO 88 RECUPERO I TEMPI ESPRESSO
126900941228     C                   MOVEL     D98TSP        WTSP              1
127000130826     C*******            MOVEL     D98TBL        W001              1            *PRIMO BYTE TIPO BOL
127100130826     C*******            Z-ADD     D98KSC        W0040             4 0          *COD. CLIENTE
127200050301     C*******
127300050301     C* EDPFG - 01/03/2005 - Eliminata forzatura sul tipo servizio bolla
127400050301     C*
127500050301     C*    W001          IFEQ      'A'
127600050301     C*    W0040         OREQ      8888
127700050301     C*                  MOVEL     'E'           WTSP
127800050301     C*                  ENDIF
127900050301     C*******
128000941227     C*
128100941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
128200941227     C                   MOVEL     'N'           WTRAOK            1
128300941227     C                   MOVEL     'P'           CASPTA
128400941227     C                   Z-ADD     D98LNP        CASLNP
128500941227     C                   Z-ADD     *ZEROS        CASLNA
128600941228     C                   MOVEL     WTSP          CASTSP
128700941228     C                   Z-ADD     99999999      CASDDE
128800941227     C     KEYCAS        SETGT     AZCAS01L                             99
128900941227     C  N99              READP     AZCAS01L                               99
129000941227     C     *IN99         DOWEQ     '0'                                          --- 1 -->
129100941227     C     WTRAOK        ANDEQ     'N'
129200941227     C     CASLNP        ANDEQ     D98LNP
129300950102     C     CASTSP        ANDEQ     WTSP
129400941227     C     CASDDE        IFLT      D98DSP                                       --- 2 -->
129500941227     C     CASATB        ANDEQ     *BLANKS
129600941227     C                   MOVEL     'S'           WTRAOK
129700941227     C                   ELSE                                                   <-- 2 -->
129800941227     C                   READP     AZCAS01L                               99
129900941227     C                   ENDIF                                                  <-- 2 ---
130000941227     C                   ENDDO                                                  <-- 1 ---
130100941227     C*
130200941227     C* SE NON HO TROVATO TEMPI DI PARTENZA VALIDI, ERRORE
130300941227     C     WTRAOK        IFEQ      'N'
130400941227     C                   MOVEL     '1'           D98ERR
130500941227     C                   MOVEL     MSG(26)       D98MSG                         *NON TROV.TEMPI PART
130600941227     C                   ELSE
130700941227     C                   Z-ADD     CASTIN        D98TPI                         *PART.INTERNO
130800941227     C                   Z-ADD     CASTCN        D98TPC                         *PART.CLIENTE
130900941227     C                   END
131000970214     C*
131100140402     C* SE DISTRIBUZIONE E LINEA ARRIVO EXPORT
131200140403     C**   TEMPI DI PARTENZA = ZERO (SI LASCIANO 24 ORE A MI-SUD)
131300140403     c** es 03/04/14: asterscate. come fanno a torvare i colli se tutti il cliente parte
131400140403     c*  il giorno dopo? è un calcolo anacronistico e non corrispondere a realtà per cui tolto
131500140403     C**                 Z-ADD     D98LNA        F
131600140403     C**   D98ERR        IFEQ      '0'
131700140403     C**   D98TSP        IFEQ      'D'                                          *DISTRIBUZIONE
131800140403     C**   FL1(F)        ANDEQ     'E'                                          *EXPORT
131900140403     C**                 Z-ADD     *ZEROS        D98TPI                         *PART.INTERNO
132000140403     C**                 Z-ADD     *ZEROS        D98TPC                         *PART.CLIENTE
132100140403     C**                 ENDIF
132200140403     C**                 ENDIF
132300140402     c
132400140402     c* Se si tratta di ritiro in pickup 2 - giorno dopo - aggiungo 24 ore
132500140402     c*  se tempi  = 0
132600140402     c                   if        wpick2='S' and d98tpi<=0
132700140402     c                   add       24            D98TPI
132800140402     c                   endif
132900941227     C*
133000941227     C                   ENDSR
133100941227     C********************************************************************
133200941227     C*   RTTRAZ - RECUPERA IL TEMPO DI TRAZIONE
133300941227     C********************************************************************
133400941227     C     RTTRAZ        BEGSR
133500941227     C*
133600941227     C* CARICO LA SCHIERA COMBINAZIONI LINEE/TERMINAL
133700150219     c
133800150219     C                   Z-ADD     D98LNA        LTA(1)
133900150219     C                   Z-ADD     D98LNA        LTA(2)
134000150219     C                   Z-ADD     D98TFA        LTA(3)
134100941227     C                   Z-ADD     D98TFA        LTA(4)
134200941227     C*
134300941227     C* IMPOSTO FLAG TROVATA/NON TROVATA COMBINAZIONE TEMPI VALIDI A NO
134400941227     C                   MOVEL     'N'           WTRAOK            1
134500941227     C                   Z-ADD     1             K                 3 0
134600941227     C*
134700941227     C* CICLO FINO A:
134800941227     C*    - FINE SCHIERA
134900941227     C*    - TROVATO TEMPI VALIDI
135000941227     C     K             DOUGT     4                                            --- 1 -->
135100941227     C     WTRAOK        OREQ      'S'
135200941227     C     LTP(K)        IFNE      *ZEROS                                       --- 2 -->
135300941227     C     LTA(K)        ORNE      *ZEROS
135400941227     C                   MOVEL     'T'           CASPTA
135500941227     C                   Z-ADD     LTP(K)        CASLNP
135600941227     C                   Z-ADD     LTA(K)        CASLNA
135700941227     C                   MOVEL     D98TSP        CASTSP
135800941228     C                   Z-ADD     99999999      CASDDE
135900941227     C     KEYCAS        SETGT     AZCAS01L                             99
136000941227     C  N99              READP     AZCAS01L                               99
136100941227     C     *IN99         DOWEQ     '0'                                          --- 3 -->
136200941227     C     WTRAOK        ANDEQ     'N'
136300941227     C     LTP(K)        ANDEQ     CASLNP
136400941227     C     LTA(K)        ANDEQ     CASLNA
136500941227     C     D98TSP        ANDEQ     CASTSP
136600960531     C     CASDDE        IFLE      D98DSP                                       --- 4 -->
136700941227     C     CASATB        ANDEQ     *BLANKS
136800941227     C                   MOVEL     'S'           WTRAOK
136900941227     C                   ELSE                                                   <-- 4 -->
137000941227     C                   READP     AZCAS01L                               99
137100941227     C                   ENDIF                                                  <-- 4 ---
137200941227     C                   ENDDO                                                  <-- 3 ---
137300941227     C                   ENDIF                                                  <-- 2 ---
137400941227     C                   ADD       1             K
137500941227     C                   ENDDO                                                  <-- 1 ---
137600941227     C*
137700941227     C* SE NON HO TROVATO TEMPI DI TRAZIONE VALIDI, ERRORE
137800941227     C     WTRAOK        IFEQ      'N'
137900941227     C                   MOVEL     '1'           D98ERR
138000941227     C                   MOVEL     MSG(27)       D98MSG                         *NON TROV.TEMPI TRAZ
138100150219     C***                ELSE
138200150219     C***                Z-ADD     CASTIN        D98TTI
138300150219     C***                Z-ADD     CASTCN        D98TTC
138400941227     C                   END
138500941227     C*
138600941227     C                   ENDSR
138700941227     C********************************************************************
138800941227     C*   RTCONS - RECUPERA IL TEMPO DI CONSEGNA
138900941227     C********************************************************************
139000941227     C     RTCONS        BEGSR
139100970701     C*
139200970701     C* LANCIO PGM CONTR.CAPPARIO PER AVERE I TEMPI CONS.
139300970701     C                   CLEAR                   SI95
139400970701     C                   MOVEL     '3'           I95TCN
139500970701     C                   MOVEL     D98NZD        I95NAR
139600970701     C                   MOVEL     D98CAD        I95CAP
139700970701     C                   MOVEL     D98LOD        I95LOC
139800970701     C                   MOVEL     D98DSP        I95DAT
139900970701     C                   MOVEL     D98TSP        I95TSP
140000980602     C                   MOVEL     'S'           I95FRT
140100020226     C*
140200020226     C* VERIFICO SE LNA E' FILIALE FEDEX
140300020226     C                   IF        D98LNA > *ZEROS
140400020226     C     D98LNA        LOOKUP    FILFED                                 55
140500020226     C                   IF        *IN55 = *ON
140600020226     C                   EVAL      I95FI1 = 'S'
140700020226     C                   ENDIF
140800020226     C                   ENDIF
140900020226     C*
141000020226     C                   CALL      'TISI95R'
141100970701     C                   PARM                    SI95
141200941227     C*
141300941228     C* SE NON TROVATO, ERRORE
141400970701     C     O95ERR        IFNE      *BLANKS                                      --- 1 -->
141500941227     C                   MOVEL     '1'           D98ERR
141600970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
141700941227     C                   ELSE                                                   <-- 1 -->
141800941227     C*
141900941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
142000941227     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
142100941227     C     CASTIN        DIV       24            WDIV              3 0
142200941227     C                   MVR                     WRES              3 0
142300941227     C     WRES          IFEQ      *ZEROS
142400970701     C                   Z-ADD     O950IN        D98TCI
142500941227     C                   ELSE
142600970701     C                   Z-ADD     O951IN        D98TCI
142700941227     C                   END
142800941227     C*
142900941227     C     CASTCN        DIV       24            WDIV                     99
143000941227     C                   MVR                     WRES
143100941227     C     WRES          IFEQ      *ZEROS
143200970701     C                   Z-ADD     O950CN        D98TCC
143300941227     C                   ELSE
143400970701     C                   Z-ADD     O951CN        D98TCC
143500941227     C                   END
143600941227     C*
143700941227     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
143800941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
143900970701     C                   Z-ADD     O950IN        W990IN            3 0          H.00
144000970701     C                   Z-ADD     O951IN        W991IN            3 0          H.12
144100941227     C*
144200941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
144300941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
144400970701     C     O95ISO        IFEQ      'I'
144500941227     C     D98FIN        OREQ      'I'
144600970701     C                   ADD       O95TIS        D98TCI
144700970701     C                   ADD       O95TIS        D98TCC
144800941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
144900970701     C                   ADD       O95TIS        W990IN
145000970701     C                   ADD       O95TIS        W991IN
145100941227     C                   END
145200090827     c
145300090827     c* Sistemo i tempi di consegna se non sono multipli di 12
145400090827     c                   exsr      SisCONS
145500941228     C*
145600941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
145700941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
145800941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
145900941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
146000941228     C                   Z-ADD     D98TCC        W99CCE            3 0
146100941228     C                   ELSE                                                   <-- 3 -->
146200970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
146300970701     C                   CLEAR                   SI95
146400970701     C                   MOVEL     '3'           I95TCN
146500970701     C                   MOVEL     D98NZD        I95NAR
146600970701     C                   MOVEL     D98CAD        I95CAP
146700970701     C                   MOVEL     D98LOD        I95LOC
146800970701     C                   MOVEL     D98DSP        I95DAT
146900970701     C                   MOVEL     'E'           I95TSP
147000980602     C                   MOVEL     'S'           I95FRT
147100020226     C*
147200020226     C* VERIFICO SE LNA E' FILIALE FEDEX
147300020226     C                   IF        D98LNA > *ZEROS
147400020226     C     D98LNA        LOOKUP    FILFED                                 55
147500020226     C                   IF        *IN55 = *ON
147600020226     C                   EVAL      I95FI1 = 'S'
147700020226     C                   ENDIF
147800020226     C                   ENDIF
147900020226     C*
148000020226     C                   CALL      'TISI95R'
148100970701     C                   PARM                    SI95
148200970701     C*
148300941228     C* SE NON TROVATO, ERRORE
148400970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
148500941228     C                   MOVEL     '1'           D98ERR
148600970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
148700941228     C                   ELSE                                                   <-- 4 -->
148800941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
148900941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
149000941228     C     CASTCN        DIV       24            WDIV                     99
149100941228     C                   MVR                     WRES
149200941228     C     WRES          IFEQ      *ZEROS
149300970701     C                   Z-ADD     O950CN        W99CCE
149400941228     C                   ELSE
149500970701     C                   Z-ADD     O951CN        W99CCE
149600941228     C                   END
149700941228     C                   ENDIF                                                  <-- 4 ---
149800941228     C                   ENDIF                                                  <-- 3 ---
149900941228     C                   ENDIF                                                  <-- 2 ---
150000941227     C*
150100941227     C                   END                                                    <-- 1 ---
150200941227     C*
150300941227     C                   ENDSR
150400090827     C********************************************************************
150500090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
150600090827     C********************************************************************
150700090827     C     SisCONS       BEGSR
150800090827     c                   eval      w99tci=d98tci
150900090827     c                   if        d98tci=10
151000090827     c                   z-add     12            w99tci
151100090827     c                   endif
151200090827     c
151300090827     c                   eval      w99tcc=d98tcc
151400090827     c                   if        d98tcc=10
151500090827     c                   z-add     12            w99tcc
151600090827     c                   endif
151700090827     c
151800090827     C                   ENDSR
151900930420     C********************************************************************
152000930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
152100930420     C********************************************************************
152200930420     C     CAREVB        BEGSR
152300930420     C*
152400930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
152500930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
152600060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
152700930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
152800930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
152900060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
153000060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
153100930420     C                   Z-ADD     D98AAS        KEBAAS
153200930420     C                   Z-ADD     D98LNP        KEBLNP
153300930420     C                   Z-ADD     D98NRS        KEBNRS
153400930420     C                   Z-ADD     D98NSP        KEBNSP
153500990810     C     KEYEVB        SETLL     FNEVB01L                           99
153600060406     C  N99keyevb        READe     FNEVB01L                               99
153700060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
153800930421     C     D98ERR        ANDEQ     '0'
153900060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
154000930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
154100930421     C                   Z-ADD     1             I
154200930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
154300060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
154400930421     C                   MOVEL     '1'           D98ERR
154500930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
154600060406   x3C                   ELSE                                                   <-- 3 -->
154700060406     C                   MOVEL     D2A(I)        usa2A
154800060406     C                   MOVEL     D2Z(I)        usa2Z
154900930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
155000060406    4C     §2AFTC        IFEQ      'A'
155100930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
155200060406    4C                   END
155300060406    4C     §2AFTC        IFEQ      'G'
155400930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
155500060406    4C                   END
155600150216     c* Se si tratta di MIC verifico se uguale alla sped.
155700150303    4c                   if        evbcev='MIC' or evbcev='NIC'
155800150303    5c                   if        %subst(evbnot:1:7) >*zeros
155900150303     c
156000150303    6c                   if        evbcev='MIC' and
156100150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
156200150216     c                   eval      evemic='SI'
156300150303    6c                   endif
156400150303     c
156500150303    6c                   if        evbcev='NIC' and
156600150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
156700150216     c                   eval      evemic='  '
156800150303    6c                   endif
156900150303     c
157000150303   x5c                   else
157100150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
157200150303     c                   eval      evemic='SI'
157300150303    6c                   endif
157400150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
157500150303     c                   eval      evemic='  '
157600150303    6c                   endif
157700150303
157800150303    5c                   endif
157900150303    4c                   endif
158000150216     c
158100060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
158200930421     C     §2AFTC        OREQ      'G'
158300930421     C                   ADD       1             IE
158400990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
158500930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
158600930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
158700131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
158800090928     c
158900090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
159000091014     c**                 if        ie=1
159100091014     c**                 eval      Primotipo=§2aftc
159200091014     c**                 eval      Primodev=evbdev
159300091014     c**                 eval      Primohev=evbhev
159400091014     c**                 endif
159500090928     c
159600060406    4C                   END                                                    <-- 4 ---
159700060406     c* Verifico se si tratta di evento particolare
159800060406    4c                   if        §2zCEP<>' '
159900060526     c* non carico l'evento se cade in giorno festivo
160000060526     c                   z-add     evbdev        wpodata
160100060526     C                   EVAL      WTC='C'
160200060526     C                   EVAL      WIC='C'
160300060526     c                   eval      wslitt='N'
160400060526     c                   exsr      chkfes
160500060526     c
160600060526     c* Wslitt='N' non festivo
160700060526   4ac                   if        wslitt='N'
160800060526     c
160900060406    5c                   if        §2zfep='A'
161000060406     c                   eval      wevpaA='S'
161100060406    5C                   END                                                    <-- 3 ---
161200060406    5c                   if        §2zfep='G'
161300060406     c                   eval      wevpaG='S'
161400060406    5C                   END                                                    <-- 3 ---
161500060406     c                   add       1             ip
161600060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
161700060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
161800060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
161900060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
162000060526   4aC                   ENDif                                                  <-- 3 ---
162100060526    4C                   END                                                    <-- 3 ---
162200060406    3C                   END                                                    <-- 3 ---
162300060406    2C                   END                                                    <-- 2 ---
162400060406     C     keyevb        READe     FNEVB01L                               99
162500060406    1C                   ENDDO                                                  <-- 1 ---
162600050523     c* controllo riempimento skiere
162700050523     c                   clear                   trul0sds
162800050523     c                   eval      i0sski='EVD/EVO/EVC'
162900050523     c                   eval      i0sele=%elem(EVD)
163000050523     c                   eval      i0spie=ie
163100050523     c                   eval      i0sfile='FNEVB01L  '
163200050523     c                   eval      i0scod='     '
163300050523     c                   eval      i0spgm='TNSD99R'
163400050523     c                   movel     trul0sds      kpjbu
163500050523     c                   call      'TRUL0SR'
163600050523     c                   parm                    kpjba
163700930420     C*
163800060406     c* controllo riempimento skiere
163900060406     c                   clear                   trul0sds
164000060406     c                   eval      i0sski='PVD/PVO/PVC'
164100060406     c                   eval      i0sele=%elem(PVD)
164200060406     c                   eval      i0spie=ie
164300060406     c                   eval      i0sfile='FNEVB01L  '
164400060406     c                   eval      i0scod='     '
164500060406     c                   eval      i0spgm='TNSD99R'
164600060406     c                   movel     trul0sds      kpjbu
164700060406     c                   call      'TRUL0SR'
164800060406     c                   parm                    kpjba
164900060406     C*
165000930420     C                   ENDSR
165100931213     C********************************************************************
165200931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
165300931213     C********************************************************************
165400931213     C     CARTUC        BEGSR
165500931213     C*
165600931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
165700931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
165800931214     C     D98GC2        ANDEQ     *BLANKS
165900931213     C                   ELSE                                                   <-- 1 -->
166000931213     C*
166100931213     C                   MOVE      D98GC1        TUC(1)
166200931214     C                   MOVE      D98GC2        TUC(2)
166300931213     C*
166400931213     C                   END                                                    <-- 1 ---
166500931213     C*
166600931213     C                   ENDSR
166700970304     C********************************************************************
166800970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
166900970304     C********************************************************************
167000970304     C     DATRIF        BEGSR
167100970304     C*
167200050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
167300050922     c*  partenza
167400970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
167500970304     C*
167600970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
167700070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
167800070605     C***                MOVEL     '1'           D98ERR
167900070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
168000070605     C***                ELSE                                                   <-- 2 -->
168100970304     C*
168200970304     C                   Z-ADD     D98DRF        WD1
168300970304     C                   Z-ADD     D98ORF        WO1
168400970310     C* DATA PARTENZA ULTIMO COLLO
168500970310     C     D98DRF        IFLE      D98DUC
168600050928     c     d98duc        oreq      0
168700970310     C                   Z-ADD     D98DRF        D98DUC
168800970310     C                   Z-ADD     D98ORF        W98HUC
168900050928     c*
169000070605     C                   ENDIF
169100970304     C* DATA ARRIVO TRAZIONE
169200050922     C***                Z-ADD     D98DTI        WD2
169300050922     C***                Z-ADD     D98HTI        WO2
169400050922     C***  WDO1          IFLE      WDO2
169500050922     C***                Z-ADD     D98DRF        D98DTI
169600050922     C***                Z-ADD     D98ORF        D98HTI
169700050922     C***                ENDIF
169800970304     C* DATA CONSEGNA
169900050922     C***                Z-ADD     D98DCM        WD2
170000050922     C***                Z-ADD     D98HMC        WO2
170100050922     C***  WDO1          IFLE      WDO2
170200050922     C***                Z-ADD     D98DRF        D98DCM
170300050922     C***                Z-ADD     D98ORF        D98HMC
170400050922     C***                ENDIF
170500970304     C*
170600070605     C***                ENDIF                                                  <-- 2 ---
170700970304     C                   ENDIF                                                  <-- 1 ---
170800970304     C*
170900970304     C                   ENDSR
171000940901     C********************************************************************
171100940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
171200940901     C********************************************************************
171300940901     C     TDTIPO        BEGSR
171400940901     C*
171500940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
171600940902     C                   Z-ADD     D98TTI        W98TTI            3 0
171700941011     C*
171800941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
171900941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
172000941011     C     W98TTI        DIV       24            WDIV              3 0
172100941011     C                   MVR                     WRES              3 0
172200941011     C     WRES          IFEQ      *ZEROS
172300941011     C                   Z-ADD     *ZEROS        WASTA             9 0
172400941011     C                   ELSE
172500941011     C                   Z-ADD     12            WASTA
172600941011     C                   END
172700940901     C*
172800940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
172900940901     C     D98DTI        IFNE      *ZEROS
173000940901     C                   Z-ADD     D98DTI        WDAT
173100940901     C                   EXSR      DATORE
173200960911     C                   Z-ADD     WORE          WPODTI
173300960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
173400960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
173500960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
173600960911     C                   Z-ADD     WPODTI        W10POI            9 0
173700960911     C                   EXSR      R10FES
173800960911     C                   Z-ADD     W10POO        WPODTI
173900960911     C* AGGIUNGO LE ORA D'ASTA
174000960911     C                   ADD       WASTA         WPODTI
174100960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
174200960911     C     W10POI        IFEQ      W10POO
174300940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
174400940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
174500940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
174600940901     C                   EXSR      RO3ORE
174700940901     C                   ADD       WR3HHO        WPODTI
174800940901     C                   ADD       WR3HHO        W98TTI
174900940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
175000940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
175100940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
175200940901     C                   EXSR      RO3ORE
175300940901     C                   ADD       WR3HHO        WPODTI
175400940901     C                   ADD       WR3HHO        W98TTI
175500960911     C                   ENDIF                                                  *SE NON FESTIVO
175600940901%%%  C                   ELSE                                                   *SE D98DTI = 0
175700950327%%%  C***********D98DAM    IFNE 0
175800950327%%%  C*********************Z-ADDD98DAM    WDAT             *DATA ARR.FIL.ARR.
175900950327%%%  C*********************EXSR DATORE
176000950327%%%  C***********WASTA     ADD  WORE      WPODTI
176100950327%%%  C*********************ENDIF
176200940901     C                   END
176300940901     C*
176400940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
176500940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
176600940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
176700940901     C     W98TTI        DIV       24            WDIV              3 0
176800940901     C                   MVR                     WRES              3 0
176900940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
177000941228     C                   Z-ADD     W990IN        D98TCI
177100940901     C                   ELSE                                                   <-- 2 -->
177200941228     C                   Z-ADD     W991IN        D98TCI
177300940908     C                   ENDIF                                                  <-- 2 ---
177400090827     c* Sistemo i tempi di consegna se non sono multipli di 12
177500090827     c                   exsr      SisCONS
177600090827     c*
177700940908     C* RICALCOLO TEMPO DI RESA INTERNO
177800950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
177900950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
178000090921     c                   z-add     d98tri        w99tri
178100090921     c* per la DS
178200090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
178300090921     c* per i calcoli interni
178400090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
178500940908     C                   ENDIF                                                  <-- 1 ---
178600940901     C*
178700940901     C                   ENDSR
178800930325     C********************************************************************
178900930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
179000930325     C********************************************************************
179100930325     C     TDATPO        BEGSR
179200930406     C*
179300930420     C* TRASFORMO LA DATA ODIERNA
179400930416     C                   Z-ADD     WDAODI        WDAT
179500930416     C                   EXSR      DATORE
179600050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
179700960911     C                   MOVEL     'OSC  '       WR3TAB
179800960911     C                   Z-ADD     WHHODI        WR3HHI
179900960911     C                   EXSR      RO3ORE
180000050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
180100050922     c                   if        wr3hho=12
180200050922     c     wpoatt        add       12            wpoattdata24
180300050922     c                   else
180400050922     c                   z-add     wpoatt        wpoattdata24
180500050922     c                   endif
180600930325     C*
180700930420     C* TRASFORMO LA DATA DI SPEDIZIONE
180800931125     C     D98DSP        IFEQ      *ZEROS
180900930326     C                   MOVEL     '1'           D98ERR
181000930326     C                   MOVEL     MSG(2)        D98MSG
181100930326     C                   ELSE
181200931125     C                   Z-ADD     D98DSP        WDAT
181300930325     C                   EXSR      DATORE
181400930406     C                   Z-ADD     WORE          WPODSP
181500930326     C                   END
181600930414     C*
181700930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
181800930414     C                   MOVEL     'LIM  '       WR3TAB
181900930414     C                   Z-ADD     *ZEROS        WR3HHI
182000930414     C                   EXSR      RO3ORE
182100931222     C     WR3HHO        MULT      10            WTM060            6 0
182200931222     C     WPODSP        ADD       WTM060        WPOLIM
182300140822     c
182400930325     C*
182500930420     C* TRASFORMO LA DATA DI CONSEGNA
182600931125     C     D98DCM        IFNE      *ZEROS
182700931125     C                   Z-ADD     D98DCM        WDAT
182800930325     C                   EXSR      DATORE
182900930326     C                   Z-ADD     WORE          WPODCM
183000930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
183100930326     C                   MOVEL     'OSC  '       WR3TAB
183200931125     C                   Z-ADD     D98HMC        WR3HHI
183300930326     C                   EXSR      RO3ORE
183400930326     C                   ADD       WR3HHO        WPODCM
183500930326     C                   END
183600941228     C*
183700941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
183800941228     C     D98DUC        IFNE      *ZEROS
183900941228     C                   Z-ADD     D98DUC        WDAT
184000941228     C                   EXSR      DATORE
184100941228     C                   Z-ADD     WORE          WPODUC
184200970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
184300970310     C                   MOVEL     'OSP  '       WR3TAB
184400970310     C                   Z-ADD     W98HUC        WR3HHI
184500970310     C                   EXSR      RO3ORE
184600970310     C     D98DRF        IFNE      *ZEROS
184700050928     c     w98huc        andgt     1200
184800970310     C                   SUB       12            WR3HHO
184900970310     C                   ENDIF
185000970310     C                   ADD       WR3HHO        WPODUC
185100941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
185200070605     C***  WPODSP        IFGT      WPODUC
185300070605     C***                Z-ADD     WPODSP        WPODUC
185400070605     C***                ENDIF
185500941228     C                   ENDIF
185600960402     C*
185700141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
185800970304     C     D98DBR        IFNE      *ZEROS
185900970304     C                   Z-ADD     D98DBR        WDAT
186000960402     C                   EXSR      DATORE
186100960402     C                   Z-ADD     WORE          WPODBR
186200070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
186300960402     C                   ELSE
186400960402     C                   Z-ADD     WPODSP        WPODBR
186500960402     C                   ENDIF
186600930913     C*
186700960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
186800930913     C* CONSEGNABILITA'
186900960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
187000950407     C*
187100930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
187200931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
187300931125     C                   Z-ADD     D98DIC        WDAT
187400930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
187500930913     C                   Z-ADD     WORE          WPODIC
187600930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
187700931125     C                   Z-ADD     D98OIC        WR3HHI
187800930913     C                   EXSR      RO3ORE
187900930913     C                   ADD       WR3HHO        WPODIC
188000931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
188100950407     C*
188200930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
188300931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
188400931125     C                   Z-ADD     D98DFC        WDAT
188500930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
188600930913     C                   Z-ADD     WORE          WPODFC
188700140822     C                   Z-ADD     WORE          WPODFC_00
188800930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
188900931125     C                   Z-ADD     D98OFC        WR3HHI
189000930913     C                   EXSR      RO3ORE
189100930913     C                   ADD       WR3HHO        WPODFC
189200931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
189300930913     C*
189400930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
189500930913     C     D98ERR        IFEQ      '0'
189600930913     C     WPODFC        IFEQ      *ZEROS
189700930913     C     WPODIC        ANDNE     *ZEROS
189800930913     C                   MOVEL     '1'           D98ERR
189900930913     C                   MOVEL     MSG(17)       D98MSG
190000930913     C                   ENDIF
190100930913     C                   ENDIF
190200930907     C*
190300930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
190400930913     C     D98ERR        IFEQ      '0'
190500930913     C     WPODIC        IFGT      WPODFC
190600930907     C                   MOVEL     '1'           D98ERR
190700930913     C                   MOVEL     MSG(18)       D98MSG
190800930913     C                   ENDIF
190900930913     C                   ENDIF
191000950407     C*
191100950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
191200930511     C*
191300930511     C                   ENDSR
191400950110     C********************************************************************
191500950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
191600950110     C********************************************************************
191700950110     C     CALDPP        BEGSR
191800950110     C*
191900950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
192000950110     C                   MOVEL     'P'           WTC                            *PARTENZA
192100950110     C                   MOVEL     'I'           WIC                            *INTERNA
192200960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
192300950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
192400950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
192500950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
192600950110     C*
192700950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
192800950110     C                   MOVEL     'P'           WTC                            *PARTENZA
192900950110     C                   MOVEL     'C'           WIC                            *CLIENTE
193000950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
193100950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
193200950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
193300950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
193400960229     C*
193500960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
193600050922     c
193700050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
193800050922     c
193900050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
194000090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
194100050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
194200090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
194300050922%%%  C*  ritardo alla partenza.
194400050922     c* Il tutto solo se la data di oggi > della
194500050922     c*  data teorica di partenza determinata
194600050922     c*
194700950110%%%  C     D98ERR        IFEQ      '0'
194800950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
194900950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
195000950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
195100050922     c     wpoattData24  andgt     wpoppi
195200960911%%%  C********             MOVEL'OSC  '   WR3TAB
195300960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
195400960911%%%  C********             EXSR RO3ORE
195500960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
195600050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
195700090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
195800090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
195900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
196000050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
196100050922     c* La nuova data calcolata non può essere < della minima prevista
196200050922     c                   if        wr0poo>wpoppi
196300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
196400050922     c                   endif
196500050922     c
196600950110%%%  C                   ENDIF
196700950110%%%  C                   ENDIF
196800950110%%%  C*
196900950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
197000050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
197100050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
197200050915%%%  C*  alla prtenza.
197300950110%%%  C     D98ERR        IFEQ      '0'
197400950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
197500950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
197600950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
197700950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
197800950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
197900950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
198000050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
198100950110%%%  C                   ENDIF
198200950110%%%  C                   ENDIF
198300950110%%%  C*
198400950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
198500950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
198600950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
198700950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
198800950110%%%  C     D98ERR        IFEQ      '0'
198900950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
199000950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
199100950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
199200950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
199300090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
199400090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
199500950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
199600050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
199700950110%%%  C                   ENDIF
199800950110%%%  C                   ENDIF
199900960229     C*
200000960229     C                   ENDIF                                                  <-- 1 ---
200100950110     C*
200200950110     C                   ENDSR
200300930604     C********************************************************************
200400930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
200500930604     C********************************************************************
200600930604     C     CALDPT        BEGSR
200700090902     c                   clear                   RicalcoloTRAZ     1
200800930604     C*
200900930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
201000931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
201100931129     C                   MOVEL     'I'           WIC                            *INTERNA
201200050915     c* se non presente la data partenza ultimo collo, uso la teorica
201300950110     C     D98DUC        IFEQ      *ZEROS
201400950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
201500950110     C                   ELSE
201600950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
201700950110     C                   ENDIF
201800930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
201900930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
202000930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
202100930604     C*
202200930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
202300931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
202400931129     C                   MOVEL     'C'           WIC                            *CLIENTE
202500950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
202600930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
202700930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
202800930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
202900960229     C*
203000960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
203100970310     C***********D98DRF    ANDEQ*ZEROS
203200940628%%%  C*
203300940628%%%  C* SE LA BOLLA NON HA DATA ARR.TRAZIONE MA SOLO DATA ARR. IN FIL. ARR.
203400940628%%%  C* IMPOSTO LA DATA DI PREVISTA CONSEGNA COME LA DATA DI ARR. IN FIL. ARR.
203500950327%%%  C***********D98ERR    IFEQ '0'
203600950327%%%  C***********D98DTI    IFEQ 0                          *DATA ARR. TRAZIONE
203700950327%%%  C***********D98DAM    ANDNE0                          *DATA ARR. IN FIL.ARR
203800950327%%%  C*********************Z-ADDWPODTI    WPOPTI           *ORA PREV.ARR.TRAZ.
203900950327%%%  C*********************ENDIF
204000950327%%%  C*********************ENDIF
204100940624%%%  C*
204200050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
204300050915
204400940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
204500050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
204600050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
204700050915     c*   attribuito alla trazione
204800940624%%%  C     D98ERR        IFEQ      '0'
204900940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
205000940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
205100090903     c     wpoatt        andge     wpopti
205200050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
205300090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
205400090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
205500940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
205600050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
205700050922     c                   if        wr0poo>wpopti
205800050922     c                   eval      wpopti=wr0poo
205900050922%%%  C                   endif
206000090902     c*
206100090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
206200090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
206300090902     c*  della trazione udate e utime
206400090902     c                   eval      RicalcoloTRAZ='S'
206500090902     c
206600940624%%%  C                   ENDIF
206700940624%%%  C                   ENDIF
206800940628%%%  C*
206900050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
207000050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
207100050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
207200940628%%%  C     D98ERR        IFEQ      '0'
207300940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
207400940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
207500940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
207600090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
207700090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
207800940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
207900050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
208000940628%%%  C                   ENDIF
208100940628%%%  C                   ENDIF
208200960229     C*
208300960229     C                   ENDIF                                                  <-- 1 ---
208400930604     C*
208500930604     C                   ENDSR
208600930511     C********************************************************************
208700930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
208800930511     C********************************************************************
208900930511     C     CALDCR        BEGSR
209000050914     c                   clear                   consideraora
209100100308     c
209200100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
209300100308     c*
209400100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
209500100308     c* 1) se presente solo ORA --> ignoro sempre
209600100308     c* 2) se presente data e ora IL --> ignoro l'ora
209700100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
209800100309     c***                if        d98tsp='E'
209900100309     c***                clear                   d98dcr
210000100309     c***                clear                   d98hcr
210100100309     c**+                clear                   d98tcr
210200100309     c***                endif
210300100308     c
210400100308     c                   if        d98hcr>0
210500100308     c
210600100308     c                   if        d98dcr=0
210700100308     c                   clear                   d98hcr
210800100308     c                   endif
210900100308
211000100308     c                   if        d98dcr>0 and d98tcr=' '
211100100308     c                   clear                   d98hcr
211200100308     c                   endif
211300100308     c                   endif
211400941228     C*
211500941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
211600941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
211700050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
211800050901     c*  (per uguale si intende entrambe nel mattino o entrambe
211900050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
212000050901     c*  (EDPES 1/9/05)
212100050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
212200050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
212300050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
212400050831     C***                Z-ADD     D98DCM        D98DCR
212500050831     C***                ENDIF
212600050901     c*
212700070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
212800050914     c
212900100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
213000100308     c***                clear                   d98hcr
213100100308     c***                clear                   d98tcr
213200100308     c***                endif
213300050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
213400050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
213500070521     c*  richiesta dal cliente
213600100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
213700100308     C***                MOVEL     'OSC  '       WR3TAB
213800100308     C***                Z-ADD     D98HMC        WR3HHI
213900100308     C***                EXSR      RO3ORE
214000100308     c***                z-add     wr3hho        salvaoreHMC
214100050901     c
214200100308     C***                Z-ADD     D98Hcr        WR3HHI
214300100308     C***                EXSR      RO3ORE
214400100308     c***                z-add     wr3hho        salvaoreHCR
214500100308     c***
214600050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
214700100308    2c***                if        salvaoreHMC=salvaoreHCR
214800100308     C***                eval      Consideraora='S'
214900100308     c***                else
215000100308     C***                eval      Consideraora=' '
215100100308     c***                clear                   d98hcr
215200100308    2c***                endif
215300100308    1c***                endif
215400930511     C*
215500930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
215600931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
215700931125     C                   Z-ADD     D98DCR        WDAT
215800930325     C                   EXSR      DATORE
215900930326     C                   Z-ADD     WORE          WPODCR
216000930420     C*
216100930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
216200930420     C*
216300931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
216400930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
216500930326     C                   MOVEL     'CRD  '       WR3TAB
216600931125     C                   Z-ADD     D98HCR        WR3HHI
216700930419     C                   EXSR      RO3ORE
216800930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
216900940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
217000050902     c
217100931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
217200930420     C                   MOVEL     'CRD1 '       WR3TAB
217300930420     C                   Z-ADD     *ZEROS        WR3HHI
217400930420     C                   EXSR      RO3ORE
217500931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
217600090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
217700090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
217800050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
217900060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
218000050907     c                   z-add     worecr        oreILoDOPO
218100931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
218200931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
218300931207     C                   MOVEL     'C'           WIC                            *CLIENTE
218400931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
218500150330     C                   Z-ADD     WORECR        WR4NOI
218600931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
218700931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
218800931209     C     WPOCRF        SUB       12            WR7POI            9 0
218900931209     C                   MOVEL     'X'           WIC
219000931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
219100931209     C                   ADD       12            WR7POO
219200931207     C     WR7POO        IFGT      WPOCRF
219300931207     C                   Z-ADD     WR7POO        WPOCRF
219400931207     C                   END
219500150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
219600150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
219700150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
219800150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
219900150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
220000150330     C                   MOVEL     'C'           WIC                            *CLIENTE
220100150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
220200150330     C                   Z-ADD     w99TCC        WR4NOI
220300150330     c                   sub       12            wr4noi
220400150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
220500150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
220600150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
220700150330     c
220800150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
220900150330     C                   MOVEL     'X'           WIC
221000150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
221100150330     C******             ADD       12            WR7POO
221200150330     C     WR7POO        IFGT      WPOCRF_dee
221300150330     C                   Z-ADD     WR7POO        WPOCRF_dee
221400150330     C                   END
221500050902     c
221600050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
221700050902     c*  sono le ore di fine della non consegnabilità
221800050905     c* Se aggiungendo 1 finisco in giorno festivo
221900050905     c*  cerco primo giorno non festivo
222000050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
222100050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
222200050907     C****               MOVEL     'C'           WIC                            *CLIENTE
222300050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
222400050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
222500050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
222600050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
222700050907     c
222800050907     c                   add       1             wpocri
222900050902     c
223000050902   FFC                   Z-ADD     WPOCRI        WPOCRC
223100050902     c
223200930325     C                   END
223300150330     C*
223400931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
223500930420     C* DATA SPED. = DATA INIZIO RANGE
223600930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
223700940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
223800940324£$   C                   Z-ADD     WPOCRI        WPOCRC
223900930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
224000930420     C                   MOVEL     'CRP  '       WR3TAB
224100931125     C                   Z-ADD     D98HCR        WR3HHI
224200930420     C                   EXSR      RO3ORE
224300931207     C     WPODCR        ADD       WR3HHO        WPOCRF
224400931207     C                   SUB       24            WPOCRF                         *FINE RANGE
224500931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
224600931207     C* ANDANDO INDIETRO NEL TEMPO
224700050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
224800050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
224900050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
225000050907     c* data utile venedì sera
225100130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
225200130411     C                   MOVEL     'X'           WIC                            *CLIENTE
225300931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
225400931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
225500931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
225600930326     C                   END
225700930526     C*
225800931125     C     D98TCR        IFEQ      'P'
225900931125     C     D98TCR        OREQ      'D'
226000930526     C                   ELSE                                                   *CONS.RICH.IL ..
226100940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
226200940324£$   C                   Z-ADD     WPODCR        WPOCRC
226300930526     C* DATA SPED. = DATA INIZIO RANGE
226400050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
226500050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
226600050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
226700930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
226800930526     C                   MOVEL     'CRI  '       WR3TAB
226900931125     C                   Z-ADD     D98HCR        WR3HHI
227000930526     C                   EXSR      RO3ORE
227100050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
227200050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
227300050907     c                   z-add     wr3hho        oreILoDOPO
227400930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
227500050902     c
227600050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
227700050902     c                   if        wr3MATPOM='P' and d98hcr>0
227800050902     c                   add       12            wpocri
227900100308     c* tolgo 12 ore dalle ore conseganbili
228000050907     c                   sub       12            oreILoDOPO
228100050902     c                   endif
228200050902     c
228300931209     C     WPOCRF        SUB       12            WR7POI            9 0
228400931209     C                   MOVEL     'X'           WIC
228500931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
228600931209     C                   ADD       12            WR7POO
228700931209     C     WR7POO        IFGT      WPOCRF
228800931209     C                   Z-ADD     WR7POO        WPOCRF
228900931209     C                   END
229000050905     c
229100050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
229200050905     c*  sono le ore di fine della non consegnabilità
229300050905     c* Se aggiungendo 1 finisco in giorno festivo
229400050905     c*  cerco primo giorno non festivo
229500050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
229600050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
229700050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
229800050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
229900050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
230000050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
230100050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
230200050907     c
230300050907     c                   add       1             wpocri
230400050905     c
230500050905   FFC                   Z-ADD     WPOCRI        WPOCRC
230600050905     c
230700930526     C                   END
230800931206     C*
230900931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
231000931206     C* LA IMPOSTO COME LA CONS.TEORICA
231100140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
231200140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
231300931206     C                   ELSE
231400931206     C                   Z-ADD     WPOCRF        WPOCR1
231500931206     C                   END
231600140825     C     WPOCRF        IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
231700140825     C                   Z-ADD     WPODEE_dcr    WPOCR2                         *->FINE CONS.RICH.CL
231800931206     C                   ELSE
231900931206     C                   Z-ADD     WPOCRF        WPOCR2
232000931206     C                   END
232100150331     c
232200150331     c                   if        wpocrf_dee>0
232300150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
232400150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
232500150330     C                   END
232600150331     C                   END
232700150331     C*
232800150331     c                   if        wpocrf_dee=0
232900150331     C     wpocr2        sub       12            WPOCRF_dee
233000150331     c                   endif
233100150331     c
233200930526     C                   END                                                    <-- 1 ---
233300930511     C*
233400930511     C                   ENDSR
233500930511     C********************************************************************
233600930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
233700930511     C********************************************************************
233800930511     C     CALLAV        BEGSR
233900930511     C*
234000941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
234100930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
234200930421     C*
234300930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
234400930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
234500930421     C     1             DO        IE            E                 3 0          --- 2 -->
234600930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
234700930421     C                   Z-ADD     1             I
234800930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
234900060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
235000930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
235100930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
235200930421     C                   ADD       1             IL
235300930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
235400930421     C                   EXSR      DATORE
235500930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
235600091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
235700091014     c* Mi salvo la posizione dell'evento
235800091014     C                   Z-ADD     E             LAp(IL)
235900930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
236000930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
236100930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
236200930421     C                   EXSR      RO3ORE
236300930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
236400930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
236500930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
236600930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
236700091015     c**                 if        §2agla>1
236800091015     c**                 sub       1             §2agla
236900930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
237000091015     c**                 else
237100091015     c**                 clear                   whh
237200091015     c**                 endif
237300930510     C                   ADD       WHH           WORELA
237400940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
237500930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
237600090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
237700150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
237800930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
237900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
238000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
238100930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
238200930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
238300930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
238400930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
238500931209     C                   Z-ADD     LAF(IL)       WR7POI
238600931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
238700931202     C     WR7POO        IFGT      LAF(IL)
238800931201     C                   Z-ADD     WR7POO        LAF(IL)
238900931202     C                   END
239000930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
239100930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
239200091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
239300930510     C                   END                                                    <-- 3 ---
239400931223     C                   ENDDO                                                  <-- 2 ---
239500930421     C*
239600930421     C                   END                                                    <-- 1 ---
239700930618££$$ C*
239800930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
239900931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
240000941228££$$ C     1             DO        IL            I
240100930618££$$ C     LAF(I)        IFGT      WPOLAF
240200930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
240300930618££$$ C                   END
240400931223££$$ C                   ENDDO
240500930511     C*
240600930511     C                   ENDSR
240700940324£$   C********************************************************************
240800940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
240900940324£$   C********************************************************************
241000940324£$   C     CALLAC        BEGSR
241100941229     C*
241200950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
241300950407     C*
241400950210     C                   Z-ADD     WORELA        WTM060
241500941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
241600941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
241700941229     C     D98DAG        IFEQ      *ZEROS
241800050520     c     §2aftg        andeq     'S'
241900090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
242000090827     C     WORELA        ADD       w99TCC        WTM060            6 0
242100941229     C                   ENDIF
242200941229     C*
242300940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
242400940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
242500941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
242600941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
242700941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
242800940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
242900941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
243000050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
243100050520     c                   movel     §2aftg        LCON(IL)
243200950407     C*
243300950407     C                   ENDIF
243400960129     C*
243500940324£$   C                   ENDSR
243600930511     C********************************************************************
243700930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
243800930511     C********************************************************************
243900930511     C     CALGIA        BEGSR
244000960129     C*
244100960129     C*-------------------------------------------------------------------
244200960129     C* EVENTI GIACENZA
244300960129     C*-------------------------------------------------------------------
244400960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
244500960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
244600960129     C*
244700050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
244800041116     c     d98nsp        ifgt      0
244900031125     C                   EXSR      CARGIAC
245000041116     c                   endif
245100031125     C*
245200960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
245300960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
245400960129     C     1             DO        IE            E                 3 0          --- 2 -->
245500960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
245600960129     C                   Z-ADD     1             I
245700960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
245800060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
245900960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
246000960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
246100960129     C                   ADD       1             IG
246200960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
246300960129     C                   EXSR      DATORE
246400960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
246500140606     c* Mi salvo la posizione dell'evento
246600140606     C                   Z-ADD     E             gip(IG)
246700960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
246800960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
246900960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
247000960129     C                   EXSR      RO3ORE
247100960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
247200960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
247300960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
247400960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
247500960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
247600960129     C                   ADD       WHH           WOREGI
247700960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
247800960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
247900090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
248000090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
248100960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
248200960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
248300960129     C                   MOVEL     'C'           WIC                            *CLIENTE
248400960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
248500960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
248600960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
248700960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
248800960129     C                   Z-ADD     GIF(IG)       WR7POI
248900960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
249000031125     C     WR7POO        IFGT      GIF(IG)
249100031125     C                   Z-ADD     WR7POO        GIF(IG)
249200031125     C                   ENDIF
249300031201     C*
249400031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
249500031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
249600031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
249700031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
249800031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
249900031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
250000031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
250100031201     C                   IF        WPOATT > GIF(IG)
250200031201     C*
250300031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
250400031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
250500031201     C                   IF        WGGA = *BLANKS
250600031201     C                   SETOFF                                       54
250700031201     C                   ELSE
250800031201     C     WGGA          LOOKUP    SK7QW                                  54
250900031201     C                   ENDIF
251000031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
251100031201     C                   MOVEL     '1'           FLGGIACOK
251200031201     C                   ELSE
251300140127     c
251400031125     C*
251500031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
251600031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
251700031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
251800031125     C                   MOVEL     '0'           FLGGIACOK         1
251900031125     C*
252000031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
252100031125     C     1             DO        IAG           EG                3 0
252200031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
252300031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
252400031125     C                   EXSR      DATORE
252500031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
252600031125     C*
252700031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
252800031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
252900031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
253000031125     C                   EXSR      DATORE
253100031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
253200031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
253300031229     C* => NN LO CONSIDERO
253400031229     C                   IF        EVC(E+1) = 'G' AND
253500031229     C                             WPROXGII = WGCPDORE
253600031229     C                   MOVEL     '1'           FLGGIACOK
253700031229     C                   ENDIF
253800031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
253900031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
254000031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
254100031125     C                   EXSR      RO3ORE
254200031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
254300031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
254400031125     C* DELL'EVENTO GIACENZA CORRENTE
254500031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
254600060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
254700031125     C                   MOVEL     '1'           FLGGIACOK
254800031125     C                   LEAVE
254900031125     C                   ENDIF
255000031125     C                   ELSE
255100031229     C                   IF        EVC(E)  = 'G' AND
255200031229     C                             GII(IG) = WGCPDORE
255300031229     C                   MOVEL     '1'           FLGGIACOK
255400031229     C                   LEAVE
255500031229     C                   ENDIF
255600031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
255700031125     C                   MOVEL     '1'           FLGGIACOK
255800031125     C                   LEAVE
255900031125     C                   ENDIF
256000031125     C                   ENDIF
256100031125     C                   ENDDO
256200031201     C                   ENDIF
256300140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
256400140127     C                   IF        FLGGIACOK = '0'  and
256500140127     c                             d98dcm=evd(E)
256600140127     C                   MOVEL     '1'           FLGGIACOK
256700140127     c                   endif
256800031125     C*
256900031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
257000031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
257100031125     C* DELL'EVENTO GIACENZA CORRENTE
257200031125     C                   IF        FLGGIACOK = '0'
257300031125     C                   Z-ADD     *ZEROS        GIF(IG)
257400031125     C                   ELSE
257500031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
257600031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
257700031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
257800031125     C                   ENDIF
257900031125     C                   ELSE
258000031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
258100031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
258200031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
258300031125     C                   ENDIF
258400031125     C*
258500960129     C                   ENDIF                                                  <-- 3 ---
258600960129     C                   ENDDO                                                  <-- 2 ---
258700960129     C*
258800960129     C                   ENDIF                                                  <-- 1 ---
258900960129££$$ C*
259000960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
259100960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
259200960129££$$ C     1             DO        IG            I
259300960129££$$ C     GIF(I)        IFGT      WPOGIF
259400960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
259500960129££$$ C                   ENDIF
259600960129££$$ C                   ENDDO
259700941229     C*
259800941229     C*-------------------------------------------------------------------
259900941229     C* APERTURA GIACENZA
260000941229     C*-------------------------------------------------------------------
260100960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
260200960129     C*
260300960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
260400931125     C                   Z-ADD     D98DAG        WDAT
260500930325     C                   EXSR      DATORE
260600950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
260700960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
260800950413£££  C                   Z-ADD     WORE          WPODAG
260900960129$$£  C*********************ENDIF
261000930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
261100930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
261200930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
261300930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
261400930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
261500941229     C*
261600941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
261700941229     C                   Z-ADD     WPODAG        WPOAGC
261800960129     C*
261900960129     C                   ENDIF                                                  <-- 1 ---
262000941229     C*
262100941229     C*-------------------------------------------------------------------
262200930511     C* CHIUSURA GIACENZA
262300941229     C*-------------------------------------------------------------------
262400930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
262500930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
262600931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
262700931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
262800931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
262900931201     C                   Z-ADD     D98DCM        W98DCG
263000930416     C                   ELSE
263100930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
263200930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
263300931201     C                   Z-ADD     WDAODI        W98DCG
263400061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
263500061123     c*  chiusura giacenza, come già fa in altri posti
263600061123     c                   if        d98dfc>0
263700061123     C                   Z-ADD     d98dfc        W98DCG
263800061123     c                   endif
263900061123     c
264000930409     C                   END
264100930409     C                   END
264200930409     C                   END
264300930414     C* TRASF.DATA CHIUSURA
264400931201     C     W98DCG        IFNE      *ZEROS
264500931201     C                   Z-ADD     W98DCG        WDAT
264600930325     C                   EXSR      DATORE
264700930326     C                   Z-ADD     WORE          WPODCG
264800930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
264900930407     C* GIORNO DA ESCLUDERE
265000150216     C                   ADD       12            WPODCG
265100150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
265200150123     c*  il giorno di chiusura dovrebe già essere protetto
265300941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
265400941229     C     WPOCGC        IFEQ      *ZEROS
265500941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
265600941229     C                   ENDIF
265700930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
265800930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
265900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
266000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
266100150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
266200150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
266300930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
266400930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
266500930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
266600931209     C                   Z-ADD     WPODCG        WR7POI
266700931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
266800931202     C     WR7POO        IFGT      WPODCG
266900931201     C                   Z-ADD     WR7POO        WPODCG
267000930326     C                   END
267100931202     C                   END
267200930511     C*
267300930511     C                   ENDSR
267400031125     C********************************************************************
267500031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
267600031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
267700031125     C*             DEGLI EVENTI DI TIPO GIACENZA
267800031125     C********************************************************************
267900031125     C     CARGIAC       BEGSR
268000031125     C*
268100031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
268200031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
268300031125     C*
268400050405     C                   Z-ADD     D98AAS        KWGCPAAS
268500050405     C                   Z-ADD     D98LNP        KWGCPLNP
268600050405     C                   Z-ADD     D98NRS        KWGCPNRS
268700050405     C                   Z-ADD     D98NSP        KWGCPNSP
268800031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
268900050405     C     KEYGC_P       SETLL     TIGCP01L
269000050405     C                   IF        %equal(TIGCP01L)
269100050405     C     KEYGC_P       READE     TIGCP01L
269200050405     C                   DOW       not %eof(TIGCP01L)
269300031125     C                   ADD       1             IAG
269400031125     C                   EVAL      GCPD(IAG) = GCPDUR
269500050405     C     KEYGC_P       READE     TIGCP01L
269600031125     C                   ENDDO
269700031125     C                   ENDIF
269800031125     C                   ELSE
269900050405     C     KEYGC_P       SETLL     TIGCP21L
270000050405     C                   IF        %equal(TIGCP21L)
270100050405     C     KEYGC_P       READE     TIGCP21L
270200050405     C                   DOW       not %eof(TIGCP21L)
270300031125     C                   ADD       1             IAG
270400031125     C                   EVAL      GCPD(IAG) = GCPDUR
270500050405     C     KEYGC_P       READE     TIGCP21L
270600031125     C                   ENDDO
270700031125     C                   ENDIF
270800031125     C                   ENDIF
270900031125     C*
271000031125     C                   ENDSR
271100941229     C********************************************************************
271200960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
271300941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
271400941229     C********************************************************************
271500960129     C     CALGIC        BEGSR
271600941229     C*
271700960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
271800960129     C*
271900960129     C                   Z-ADD     WOREGI        WTM060
272000960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
272100960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
272200960129     C     D98DAG        IFEQ      *ZEROS
272300960129$$£  C     WOREGI        ADD       12            WTM060            6 0
272400960129     C                   ENDIF
272500960129     C*
272600960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
272700960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
272800960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
272900960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
273000960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
273100960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
273200960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
273300960129     C*
273400960129     C                   ENDIF
273500941229     C*
273600941229     C                   ENDSR
273700060418     C********************************************************************
273800060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
273900060418     c*            l'affidabilità
274000060418     C********************************************************************
274100060418     C     CALCEPA       BEGSR
274200060418     C*
274300060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
274400060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
274500060420     c                   clear                   numggcepa         3 0
274600060418     C*
274700060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
274800060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
274900060420     c                   eval      wgiroepa='0'
275000060418     c                   eval      wfine=' '
275100060418     c
275200060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
275300060420    2C     W             DOwle     IP                                           --- 2 -->
275400060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
275500060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
275600060418     c
275700060420    4c                   if        wgiroepa='0'
275800060420     c                   exsr      ElaInizCEPA
275900060418   x4c                   else
276000060420    5c                   if        pvd(W)>wdat
276100060418     c                   eval      wfine='S'
276200060418    5c                   endif
276300060418    4c                   endif
276400060418     c
276500060420    4c                   if        wfine=' ' and pvd(W)=wdat
276600060420     C*
276700060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
276800060420     c*  finale
276900060420     c                   add       1             numggcepa
277000060420     c
277100060420     C                   z-add     24            WORELA
277200060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
277300060418     c
277400060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
277500060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
277600060418     C                   MOVEL     'C'           WIC                            *CLIENTE
277700060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
277800060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
277900060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
278000060418     c
278100060418     c* Trasformo le ore in data e verifico se c'e' altro evento
278200060418     c*  in sta data che allunga ancora
278300060420     c                   z-add     wr4poo        wore
278400060418     c                   EXSR      OREDAT
278500060418    4c                   endif
278600060418     c
278700060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
278800060418    4c                   if        wfine='S'
278900060418     c                   EXSR      ELAFINECEPA
279000060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
279100060420     c*  inizio range
279200060420     c                   else
279300060420     c                   add       1             w
279400060418    4C                   ENDif                                                  <-- 3 ---
279500060420     c
279600060420     c                   else
279700060420     c                   add       1             w
279800060418    3C                   ENDif                                                  <-- 3 ---
279900060418    2C                   ENDDO                                                  <-- 2 ---
280000060418     c
280100060420     c                   if        wgiroepa='1'
280200060418     c                   EXSR      ELAFINECEPA
280300060418     c                   endif
280400060418     C*
280500060418    1C                   END                                                    <-- 1 ---
280600060418££$$ C*
280700060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
280800060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
280900060420££$$ C     1             DO        Ipa           w
281000060420££$$ C     paif(w)       IFGT      WPOpaif
281100060420££$$ C                   Z-ADD     paif(w)       WPOpaif
281200060418££$$ C                   END
281300060418££$$ C                   ENDDO
281400060418     C*
281500060418     C                   ENDSR
281600060420     C********************************************************************
281700060420     c* Elabora Inizio range eventi particolari che allungano
281800060420     C********************************************************************
281900060420     c     ElaInizCEPA   BEGSR
282000060420     C                   ADD       1             IPA
282100060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
282200060420     C                   EXSR      DATORE
282300060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
282400061017     c
282500060420     c                   eval      wgiroepa='1'
282600060420     c                   eval      wdat=pvd(w)
282700060420     C                   ENDSR
282800060418     C********************************************************************
282900060418     c* Elabora fine range eventi particolari che allungano
283000060418     C********************************************************************
283100060418     c     ElafineCEPA   BEGSR
283200060420     c     numggcepa     mult      24            worela
283300060420     c                   sub       12            worela
283400090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
283500090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
283600060418     c
283700060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
283800060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
283900060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
284000060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
284100060418     C                   Z-ADD     paif(IPA)     WR7POI
284200060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
284300060418    5C     WR7POO        IFGT      paif(IPA)
284400060418     C                   Z-ADD     WR7POO        paif(IPA)
284500060418    5C                   END
284600061017     c*Sommo 24 ore di inizio giornata
284700061017     c*            : non ho capito perchè ma ci vogliono
284800061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
284900090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
285000090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
285100061017     c* Se tempi di consegna inferiori alle 24 ore , devo
285200061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
285300061017     c*  partire prima
285400061017     c****               if        d98tcc<24
285500061017     c****               sub       12            paii(ipa)
285600061017     c****               endif
285700060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
285800060418     c* Imposto nuovo giro
285900060420     c                   eval      wgiroepa='0'
286000060418     c                   eval      wfine=' '
286100060420     c                   clear                   numggcepa         3 0
286200060418     c                   ENDSR
286300930511     C********************************************************************
286400930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
286500930511     C********************************************************************
286600010822     C     CALASS        BEGSR
286700010821     C*
286800011211     C                   EXSR      REPMAXCSB
286900930511     C*
287000930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
287100010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
287200011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
287300930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
287400930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
287500930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
287600930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
287700930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
287800930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
287900930511     C                   ELSE                                                   <-- 4 -->
2880009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
288100930512     C                   END                                                    <-- 4 ---
288200930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
288300930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
288400930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
288500930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
288600930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
288700930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
288800930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
288900930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
289000930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
289100930413     C                   END                                                    <-- 2 ---
289200011211     C                   END                                                    <-- 1 ---
289300930406     C*
289400930325     C                   ENDSR
289500930608     C********************************************************************
289600930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
289700930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2898009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
289900930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
290000930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
290100930608     C********************************************************************
290200930608     C     RDRASS        BEGSR
290300930608     C*
290400930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2905009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2906009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2907009407151507 C*********************EXSR RO3ORE
2908009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
290900930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2910009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2911009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2912009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2913009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2914009407191907 C                   Z-ADD     WPOASI        WPOROU
2915009407191907 C                   EXSR      CHKFES
2916009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2917009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2918009407191907 C                   ELSE                                                   *E' FERIALE
2919009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2920009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
2921009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
2922009407191907 C                   ENDIF
292300930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
292400930608     C                   MOVEL     'ASS1 '       WR3TAB
292500930608     C                   Z-ADD     *ZEROS        WR3HHI
292600930608     C                   EXSR      RO3ORE
292700930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
292800930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
292900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
293000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
293100930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
293200930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
293300930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
293400930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
293500930608     C*
293600930608     C                   ENDSR
293700930511     C********************************************************************
293800930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
293900930511     C********************************************************************
294000930511     C     CALFED        BEGSR
294100150331     c                   clear                   fddata
294200150331     c                   clear                   WPOFDdata
294300930511     C*
294400051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
294500931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
294600051108     C     D98FFD        oreq      'M'                                          --- 1 -->
294700930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
294800930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
294900930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
295000930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
295100150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
295200150331     c                   if        §ar5dfd>*zeros
295300150331     c                   movel     §ar5dfd       fddata            8 0
295400150331     c                   endif
295500150331     c                   if        fddata>0 and ((d98dti>0 and
295600150331     c                             fddata>d98dti) or d98dti=0)
295700150331     C                   Z-ADD     fddata        WDAT
295800150331     C                   EXSR      DATORE
295900150331     C                   Z-ADD     WORE          WPOfddata
296000150331     c                   endif
296100150331     c
296200930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
296300930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
296400930511     C                   ELSE                                                   <-- 2 -->
2965009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
296600930512     C                   END                                                    <-- 2 ---
296700150331     c                   if        wpofddata>0 and wpofddata>wpofdi
2968001504081507 C                   Z-ADD     WPOfddata     WPOFDI
296900150331     c                   endif
297000150331
297100930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
297200930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
297300930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
297400930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
297500930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
297600930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
297700930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
297800930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
297900930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
298000930511     C                   END                                                    <-- 1 ---
298100930511     C*
298200930511     C                   ENDSR
298300930608     C********************************************************************
298400930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
298500930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
2986009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
298700930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
298800930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
298900930608     C********************************************************************
299000930608     C     RDRFED        BEGSR
299100930608     C*
299200930608     C* CALCOLO ORE DEL FERMO DEPOSITO
299300930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
2994009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2995001503311907 C                   Z-ADD     WPOFDI        WPOROU
2996001503311907 C                   EXSR      CHKFES
2997001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2998001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
2999001503311907 C                   ELSE                                                   *E' FERIALE
3000001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3001001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3002001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3003001503311907 C                   ENDIF
300400150331     c* Se presente la particolarità consegna con più giorni di fermo depo
300500150331     c*  uso questi
300600150331     c                   if        §7rggfd>*zeros
300700150424     c                   movel     §7rggfd       wggfd             2 0
300800150331     c     wggfd         mult      24            Wnore
300900150331     c                   add       wnore         worefd
301000150331     c                   else
301100930608     C                   MOVEL     'FED1 '       WR3TAB
301200930608     C                   Z-ADD     *ZEROS        WR3HHI
301300930608     C                   EXSR      RO3ORE
301400940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
301500150331     c                   endif
301600930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
301700931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
301800931129     C                   MOVEL     'C'           WIC                            *CLIENTE
301900930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
302000930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
302100930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
302200930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
302300930608     C*
302400930608     C                   ENDSR
302500931201     C********************************************************************
302600931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
302700931201     C********************************************************************
302800931201     C     CALDTE        BEGSR
302900140825     c
303000140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
303100140825     c*  che tiene conto solo dei festivi per restituire in output
303200140825     c*  la data teorica di consegna
303300140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
303400140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
303500140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
303600140825     C                   ELSE
303700140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
303800140825     C                   END
303900140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
304000141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
304100141106     c*  quindi passo un flag
304200141106     c                   eval      nosomma='N'
304300140825     C                   EXSR      RO4CDP
304400141106     c                   eval      nosomma=' '
304500140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
304600140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
304700140825     c*  di consegna
304800140825     c                   EXSR      TRAS_DEI
304900140825     c
305000140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
305100140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
305200140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
305300140825     c                             ='M' or %subst(d98gc2:2:1)='P')
305400140825     c                   exsr      Reper_tempi
305500140825     c                   endif
305600931201     C*
305700140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
305800140825     c*  che tiene conto di:
305900140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
306000140825     c*  per determinare la data FINE consegna richeista che non deve essere <
306100931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
306200931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
306300931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
306400931201     C                   ELSE
306500931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
306600931201     C                   END
306700090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
306800090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
306900931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
307000140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
307100931201     C*
307200931201     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA *******************
307300931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
307400931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
307500090827     C*****              Z-ADD     D98TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
307600090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
307700931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
307800140825     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
307900141017
308000141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
308100150219     C**                 EVAL      WTC='C'
308200150219     C**                 EVAL      WIC='C'
308300150219     c**                 z-add     wpoattdata00  WPOROU
308400150219     c**                 z-add     wdaodi        wpodata
308500150219     c**                 exsr      chkfes
308600150219     c**                 if        wslitt='S'
308700150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
308800150219     c**                 z-add     24            wr6noi
308900150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
309000150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
309100150219     c**                 else
309200150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
309300150219     c**                 endif
309400141204     c* Ricalcolo WPOrifDatLav
309500141229     C**                 MOVEL     'OSC  '       WR3TAB
309600141229     C**                 Z-ADD     WHHODI        WR3HHI
309700141229     C**                 EXSR      RO3ORE
309800141229     c**
309900141229     C**                 EVAL      WTC='C'
310000141229     C**                 EVAL      WIC='C'
310100141229     c**                 z-add     wpoattdata00  WPOROU
310200141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
310300141229     c**                 z-add     wr3hho        wr6noi
310400141229     c**                 add       12            wr6noi
310500141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
310600141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
310700141229     c
310800141017     c
310900141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
311000141017     c*  ma sempre devo slittare i festivi
311100141017     C* CONSEGNABILITA'
311200150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
311300150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
311400150219     C**                 EVAL      WTC='C'
311500150219     C**                 EVAL      WIC='C'
311600150219     c**                 z-add     d98dfc        WPOdata
311700150219     c**                 exsr      chkfes
311800150219     c**                 if        wslitt='S'
311900150219     c**                 z-add     wpodfc_00     Wr6poi
312000150219     c**                 z-add     24            wr6noi
312100150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
312200150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
312300150219     c**                 else
312400150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
312500150219     c**                 endif
312600150219     c**                 endif
312700931201     C*
312800931201     C                   ENDSR
312900930511     C********************************************************************
313000930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
313100930511     C********************************************************************
313200930511     C     CALDPC        BEGSR
313300930325     C*
313400930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
313500930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
313600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
313700930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
313800930325     C                   ELSE
313900930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
314000930407     C                   END
314100090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
314200090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
314300930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
314400090902     c
314500931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
314600090901
314700930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
314800930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
314900930423     C* A QUELLA DI RICHIESTA CONSEGNA,
315000930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
315100931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
315200931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
315300931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
315400931206     C                   END
315500930423     C                   END
315600930325     C*
315700930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
315800930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
315900930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
316000090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
316100090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
316200930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
316300931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
316400930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
316500930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
316600930423     C* A QUELLA DI RICHIESTA CONSEGNA,
316700930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
316800931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
316900931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
317000931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
317100931206     C                   END
317200930423     C                   END
317300140822     C*
317400930324     C                   ENDSR
317500150113     C********************************************************************
317600150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
317700150113     C********************************************************************
317800150113     c     Cal_DEE       BEGSR
317900150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
318000150219     c
318100150219     c* 1) Se presente l'arrivo della trazione lo uso
318200150219    1c                   select
318300150219     c                   when      wmancatraz=' '  and d98dti>0
318400150113     c                   Z-ADD     WPODTI        WR1POI
318500150219     c
318600150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
318700150219    1c                   when      IDdata>0
318800150325     c                   exsr      CalcoloIMP
318900150427     c* stato IMP/disguido solo se >= alla data di oggi
319000150427     c                   if        wr1poi >=wpoattdata00
319100150427     c                   eval      d98spcdee='IMP'
319200150427     c                   endif
319300150219     c
319400150219     c                   other
319500150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
319600150225     c* devo far finta che sia arrivato.
319700150219     c* uso i tempi teorici patenza + trazione cliente
319800150219     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
319900150219     c                   endsl
320000150113     c
320100150219     c* se il calcolo è < di oggi --> imposto oggi
320200150113     c                   if        wr1poi < wpoattdata00
320300150113     c                   eval      wr1poi= wpoattdata00
320400150113     c                   endif
320500150217     c
320600150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
320700150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
320800150427    1c                   if        wr1poi>wpoattdata00  and
320900150330     c                             (deeindist<>'S' or d98cons<>'S'
321000150330     c                             or d98ddc>wdaodi)
321100150427     c
321200150427     c* Se stato IMP lo mantengo per avere messaggio diverso
321300150427     c                   if        d98spcdee<>'IMP'
321400150304     c                   eval      d98spcdee='PRV'
321500150427     c                   endif
321600150427     c
321700150326   x1c                   else
321800150330     c                   exsr      CAL_OLP
321900150217     c
322000150326     c* Se passato l'orario e :
322100150326    2c                   if        whhodi>picklim
322200150326     c*    1) ancora in distinta non quadrata --> posticipo l'orario
322300150326    3c                   if        deeindist='S' and d98cons='S' and
322400150326     c                             evemic<>'SI'
322500150326    4c                   if        §vpoolpnu>0  and §VPOOLPMN>0
322600150326    5c                   do        §vpoolpnu
322700150326     c                   add       §VPOOLPMN     picklim
322800150330     c                   eval      wora=picklim
322900150330     c                   eval      wdiff=70
323000150330     c
323100150330     c                   dow       wdiff>=0
323200150330     c                   eval      wdiff=wmn-60
323300150330     c                   if        wdiff>=0
323400150330     c                   eval      wmn=wmn-60
323500150330     c                   eval      who=who+1
323600150330     c                   endif
323700150330     c                   enddo
323800150330     c                   eval      picklim=wora
323900150326    6c                   if        picklim>whhodi
324000150326     c                   leave
324100150326    6c                   endif
324200150326    5c                   enddo
324300150326    4c                   endif
324400150326    3c                   endif
324500150326
324600150427     c*    2) non più in distinta o passati i tentativi di massimo avanzamento automatico:
324700150326     c*                           imposto stato OLP prevista cons NON modificabile
324800150326    3c                   if        whhodi>picklim
324900150330    1c                   if        wr1poi<=wpoattdata00
325000150217     c                   add       24            wr1poi
325100150330     c                   endif
325200150330     c
325300150311     c                   eval      d98spcdee='OLP'
325400150326    3c                   endif
325500150326    2c                   endif
325600150326     c* memorizzo orario limite picking
325700150326     c                   eval      d98picklim=%editc(picklim:'X')
325800150326    1c                   endif
325900150217     c
326000150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
326100150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
326200150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
326300150113     C                   Z-ADD     Wr1poi        W10POI            9 0
326400150113     C                   EXSR      R10FES
326500150113     C                   Z-ADD     W10POO        wr1poi
326600150113     C
326700150113     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
326800150113     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
326900150113     C                   EXSR      RO1CDP
327000150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
327100150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
327200150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
327300150113     C* A QUELLA DI RICHIESTA CONSEGNA,
327400150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
327500150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
327600150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
327700150331     c*  2) IL        -> è il giorno indicato
327800150331     C     D98DCR        IFNE      *ZEROS
327900150331     C     D98tcr        andne     'P'
328000150331     C***  WPOdpcee      IFLE      WPOCR2
328100150331     C***  WPOCR2        SUB       12            WPOdpcee
328200150330     c
328300150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
328400150330     c*   dare la prima data utile di consegna
328500150330     C     WPOdpcee      IFLE      WPOCRF_dee
328600150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
328700150330     C                   z-add     WPOCRF_dee    WPOdpcee
328800150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
328900150409     c                   if        wpodpcee>wpoattdata00
329000150409     c                   eval      d98spcdee='PRV'
329100150409     c                   endif
329200150113     C                   END
329300150331     c
329400150220     c
329500150113     C                   END
329600150220     c
329700150113     c                   ENDSR
329800150330     C********************************************************************
329900150330     C*   Calcolo orario da tabella OLP
330000150330     C********************************************************************
330100150330     c     Cal_OLP       BEGSR
330200150330     c                   eval      picklim=0900
330300150330     c* Dopo l'ora limite picking, passo al giorno dopo
330400150330     C                   MOVEL(P)  'OLP'         TBECOD
330500150330     C                   MOVEL(P)  d98lna        TBEKE1
330600150330     C                   MOVEL(P)  wdaodi        TBEKE2
330700150330     C     KTBE          CHAIN     TNTBE01L
330800150330     C                   IF        not %found(TNTBE01L)
330900150330     c                   clear                   tbeke2
331000150330     C     KTBE          CHAIN     TNTBE01L
331100150330     c                   endif
331200150330     C                   IF        not %found(TNTBE01L)
331300150330     C                   MOVEL(P)  '000'         TBEKE1
331400150330     C     KTBE          CHAIN     TNTBE01L
331500150330     c                   endif
331600150330     c
331700150330     C                   IF        %found(TNTBE01L) and
331800150330     c                             %subst(tbeuni:1:4)>*zeros
331900150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
332000150330     c                   endif
332100150330     c                   ENDSR
332200150325     C********************************************************************
332300150325     C*   Riclcolo data se colli in IMP/disguido
332400150325     C********************************************************************
332500150325     c     CalcoloIMP    BEGSR
332600150325     C                   Z-ADD     IDdata        WDAT
332700150325     C                   EXSR      DATORE
332800150325     C                   Z-ADD     WORE          WR4POI
332900150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
333000150325    2c                   if        IDfgs<>D98lnp
333100150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
333200150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
333300150325     c* calcolo il terminal di partenza
333400150325     c                   clear                   fnlv55ds
333500150325     c                   eval      d55tla='L'
333600150325     c                   eval      d55tpt='P'
333700150325     c                   eval      d55lin=IDfgs
333800150325     c                   eval      d55drf=IDdata
333900150325     c                   call      'FNLV55R'
334000150325     c                   parm                    fnlv55ds
334100150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
334200150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
334300150325     c                   EXSR      RTTRAZ
334400150325    3c                   if        d98err<>'1'
334500150325     C                   Z-ADD     CASTcn        wr4noi
334600150325     c                   else
334700150325     C                   Z-ADD     d98ttc        wr4noi
334800150325    3C                   endif
334900150325
335000150325   x2c                   else
335100150325     C                   Z-ADD     d98ttc        wr4noi
335200150325    2C                   endif
335300150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
335400150325     c*  perchè partirà un giorno dopo
335500150325    3c                   if        IDnpg='P'
335600150325     c                   add       24            WR4noi
335700150325    3c                   endif
335800150325     c
335900150325     c                   exsr      ro4cdp
336000150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
336100150325     c                   ENDSR
336200930604     C********************************************************************
336300930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
336400930604     C********************************************************************
336500930604     C     TPODAT        BEGSR
336600960229     C*
336700960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
336800960229     C*
336900960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
337000960229     C     WPOPTI        IFEQ      *ZEROS
337100960229     C                   Z-ADD     *ZEROS        D98DTI
337200960229     C                   Z-ADD     *ZEROS        D98HTI
337300960229     C                   ELSE
337400960229     C                   Z-ADD     WPOPTI        WORE
337500960229     C                   EXSR      OREDAT
337600960229     C                   Z-ADD     WDAT          D98DTI
337700960229     C     WRES          MULT      100           D98HTI
337800960229     C                   END
337900960229     C*
338000960229     C                   ELSE                                                   <-- 1 -->
338100930604     C*
338200931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
338300140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
338400140825     c*******            exsr      Tras_DEI
338500140825     c
338600930604     C*
338700150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
338800140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
338900140820     c* è data da :
339000140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
339100140820     c*  Tenere  conto, ovvero slittare in avanti, per :
339200140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
339300140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
339400140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
339500140911     c*
339600140820     c*  Da azzerare nei seguenti casi:
339700150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
339800150324     c* 4) bolla export    7) Cod manca consegna
339900140911     c* 8) bolla con DCR DOPOil o PRIMAdel
340000150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
340100150119     c*10) se completamente non arrivata e collo in IMP
340200150119     c
340300140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
340400150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
340500140925     C*  - FD   - export -
340600150217     c                   movel     'S'           DEEinDIST         1
340700150113     c                   eval      wcaldpcee='S'
340800150304     c                   eval      D98SPCDEE='PRV'
340900150324     c                   clear                   avvcmc            1
341000150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
341100150323     c                   if        d98cmc<>*blanks
341200150324     C                   Z-ADD     1             II
341300150326     C     d98cmc        LOOKUP    C2A(II)                                99
341400150324     C                   MOVEL     D2A(II)       usa2A
341500150324    4C     §2AFTC        IFEQ      'A'
341600150326     c                   eval      avvcmc='A'
341700150323     c                   endif
341800150324    4C     §2AFTC        IFEQ      'G'
341900150326     c                   eval      avvcmc='G'
342000150324     c                   endif
342100150324     c                   endif
342200140820     c
342300140911     c                   z-add     d98lna        EX
342400150113     c*  la azzero nei casi previsti                 0
342500140820     c                   select
342600150119     c* 1) Se la bolla e consegnata
342700140925     c                   when      d98dcm>0
342800150113     c                   clear                   wcaldpcee
342900150216     c                   eval      d98spcdee='CON'
343000150217     c                   movel     ' '           DEEinDIST         1
343100150323     c* 2) Se pratica di danno
343200150323     c                   when      flgdan='S'
343300150323     c                   clear                   wcaldpcee
343400150323     c                   eval      d98spcdee='CA '
343500150323     c                   eval      deeindist='S'
343600150324     c* 3) Se bloccata/rimpallo
343700150114     c                   when      wd98fbc='B'
343800150113     c                   clear                   wcaldpcee
343900150216     c                   eval      d98spcdee='BLO'
344000150217     c                   movel     ' '           DEEinDIST         1
344100150324     c* 4) Se bolla export
344200150324     c                   when      fl1(EX)='E'
344300150324     c                   clear                   wcaldpcee
344400150324     c                   movel     ' '           DEEinDIST         1
344500150324     c                   eval      d98spcdee='EXP'
344600150324     c* 5) Se giacente  o in attesa di apertura giacenza
344700150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
344800150324     c                   clear                   wcaldpcee
344900150324     c                   eval      d98spcdee='GIA'
345000150324     c                   eval      deeindist='S'
345100150119     c* 7) Se codice manca consegna
345200150326     c                   when      d98cmc<>*blanks and avvcmc='A'
345300150324     c                   clear                   wpodpcee
345400150324     c                   clear                   wcaldpcee
345500150324     c                   movel     ' '           DEEinDIST         1
345600150324     c                   eval      d98spcdee='ATT'
345700150410     c* 7) Bolla in sospensione consegna
345800150410     c                   when      §B4LBGE='S'
345900150410     c                   clear                   wpodpcee
346000150410     c                   clear                   wcaldpcee
346100150410     c                   movel     ' '           DEEinDIST         1
346200150410     c                   eval      d98spcdee='SOS'
346300150325     c                   other
346400150325     c*
346500150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
346600150331     c                   if        d98ffd='S'
346700150403     c                   select
346800150403     c* FD escluso dai contatti
346900150403     c                   when      §B4LTFD= 'N'
347000150403     c                   eval      d98spcdee='FD3'
347100150403     c                   clear                   wcaldpcee
347200150403     c                   movel     ' '           DEEinDIST         1
347300150403
347400150403     c* FD non contattato ( o da ricontattare)
347500150403     c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
347600150401     c                             olv82sts<>'1'
347700150323     c                   clear                   wcaldpcee
347800150323     c                   movel     ' '           DEEinDIST         1
347900150323     c                   eval      d98spcdee='FFD'
348000150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
348100150424     c                   eval      wpodpcee=wpoffi
348200150403
348300150403     c* FD contattato e     IN distinta
348400150403     c                   when      d98cons='S'
348500150401     c                   eval      d98spcdee='FD2'
348600150401     c                   clear                   wcaldpcee
348700150401     c                   movel     ' '           DEEinDIST         1
348800150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
348900150424     c                   eval      wpodpcee=wpoffi
349000150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
349100150427     c*  ritirare la merce
349200150427     C                   MOVEL     'C'           WTC                            *PARTENZA
349300150427     C                   MOVEL     'C'           WIC
349400150427     C                   Z-ADD     D98ddc        WDAT
349500150427     C                   EXSR      DATORE
349600150427     c                   z-add     wore          wr4poi
349700150427     c                   z-add     24            wr4noi
349800150427     C                   EXSR      RO4CDP
349900150427     c                   z-add     wr4poo        wore
350000150427     c                   exsr      OREDAT
350100150427     c                   movel     wdat          D98DTCTD
350200150403     c                   other
350300150403     c* FD contattato e NON in distinta
350400150331     c                   eval      d98spcdee='FD1'
350500150408     c                   eval      wpodpcee=wpoffi
350600150331     c                   clear                   wcaldpcee
350700150331     c                   movel     ' '           DEEinDIST         1
350800150403     c                   endsl
350900150331     c
351000150331     c                   endif
351100150403     c
351200150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
351300150325     c                   if        (d98tc1='A' or d98tc2='A' or
351400150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
351500150325     c                   clear                   wcaldpcee
351600150325     c                   movel     ' '           DEEinDIST         1
351700150403     c                   if        §B4LBAS= 'N'
351800150427     c                   eval      d98spcdee='AP3'
351900150403     c                   else
352000150403     c                   eval      d98spcdee='APP'
352100150325     c                   endif
352200150403     c                   endif
352300150325     c*   se la bolla ha il C/Assegno verifico se da contattare
352400150325     c                   if        d98cas>0 and §B4LTCA<>' '
352500150325     c                   clear                   wcaldpcee
352600150325     c                   movel     ' '           DEEinDIST         1
352700150403     c                   if        §B4LBAS= 'N'
352800150427     c                   eval      d98spcdee='CA3'
352900150403     c                   else
353000150403     c                   eval      d98spcdee='CAS'
353100150325     c                   endif
353200150403     c                   endif
353300150325     c* Per questi 3 casi devo impostare la data contatto destinatario
353400150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
353500150325     c                             d98spcdee='CAS'
353600150325     c                   select
353700150403     c                   when      d98dti>0 and wmancatraz= ' '
353800150325     c                   movel     d98dti        d98dtctd
353900150403     c* se piene le ore già portate ad un giorno lavorativo
354000150403     c                   if        wpodti>0
354100150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
354200150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
354300150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
354400150403     C                   Z-ADD     WPODTI        W10POI            9 0
354500150403     C                   EXSR      R10FES
354600150403     c                   z-add     w10poo        wore
354700150403     C                   EXSR      OREDAT
354800150403     C                   movel     WDAT          D98dtctd
354900150403     c                   endif
355000150403     c
355100150325     c                   when      d98dam>0
355200150421     C                   Z-ADD     D98dam        WDAT
355300150421     C                   EXSR      DATORE
355400150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
355500150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
355600150421     C                   Z-ADD     Wore          W10POI            9 0
355700150421     C                   EXSR      R10FES
355800150421     c                   z-add     w10poo        wore
355900150421     C                   EXSR      OREDAT
356000150421     C                   movel     WDAT          D98dtctd
356100150325     c                   other
356200150325     c* data teorica di arrivo tenendo conto dell'IMP
356300150325     c                   exsr      letbrv
356400150325     c                   if        idDATA>0
356500150325     c                   exsr      CalcoloIMP
356600150325     c                   z-add     wr1poi        WORE
356700150325     c                   else
356800150325     c                   z-add     wpoptc        WORE
356900150325     c                   endif
357000150325     C                   EXSR      OREDAT
357100150325     c                   movel     wdat          d98dtctd
357200150325     c                   endsl
357300150325     c
357400150325     c                   endif
357500150325     c
357600150119     c* 8) cons richiesta DOPOil o PRIMAdel
357700150320     c*****              when      d98dcr>0 and (d98TCR='D' or d98TCR='P')
357800150113     c*****              clear                   wpodpcee
357900150320     c*****              clear                   wcaldpcee
358000150320     c*****              clear                   d98spcdee
358100150320     c*****              eval      deeindist='S'
358200150119     c* 9) Se Appuntamento o supermercato senza data consegna richiesta
358300150320     c****               when      d98dcr=0 and (d98tc1='A' or d98tc2='A' or
358400150320     c****                         d98tc1='S' or d98tc2='S')
358500150113     c*******            clear                   wpodpcee
358600150320     c****               clear                   wcaldpcee
358700150320     c****               clear                   d98spcdee
358800150320     c****               eval      deeindist='S'
358900150218     c*
359000150218     c* 10) Se il C/Ass non è abilitato
359100150325     c**                 when      wd98acc=*blanks  and wtrasm= ' '
359200150325     c**                 clear                   wcaldpcee
359300150325     c**                 clear                   d98spcdee
359400150325     c**                 movel     ' '           DEEinDIST         1
359500150325     c**
359600140820     c                   endsl
359700140820     c
359800150113     c* Se la bolla non è in distinta calcolo la data se previsto
359900150220     c                   if        wcaldpcee='S' and
360000150220     c                             (d98cons<>'S'  or (d98cons='S' and
360100150220     c                             evemic<>'SI'))
360200150219     c
360300150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
360400150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
360500150219     c                   if        d98nsp>0 and wtrasm<>'D' and
360600150219     c                             d98dam=0
360700150219     c                   exsr      letBRV
360800150219     c                   endif
360900150219     c
361000150217     c                   eval      d98spcdee='PRG'
361100150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
361200150320     c                             d98tc1='S' or d98tc2='S')
361300150320     c                   eval      d98spcdee='APP'
361400150320     c                   endif
361500150320     c
361600150113     c                   exsr      cal_DEE
361700150113     c                   endif
361800150113     c
361900140820     C     WPOdpcee      IFEQ      *ZEROS
362000931202     C                   Z-ADD     *ZEROS        D98DEE
362100931202     C                   Z-ADD     *ZEROS        D98OEE
362200930604     C                   ELSE
362300140820     C                   Z-ADD     WPOdpcee      WORE
362400141021     C******             SUB       12            WORE
362500930604     C                   EXSR      OREDAT
362600931202     C                   Z-ADD     WDAT          D98DEE
362700090903     c                   if        d98tcc<>10
362800090903     C                   ADD       12            WRES
362900090903     c                   else
363000090903     C                   ADD       10            WRES
363100090903     c                   endif
363200090903
363300931202     C     WRES          MULT      100           D98OEE
363400090903     c
363500090903     c                   if        d98oee=1000
363600090903     c                   eval      d98oee=1030
363700090903     c                   endif
363800140925     C                   END
363900150311     c
364000150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
364100150311     c                   select
364200150311     c                   when      d98spcdee='OLP'
364300140820     c
364400150220     c* Se bolla in distinta --> data prevista = data distinta
364500150311     c                   when      deeindist='S'  and
364600150113     c                             d98cons='S'
364700150220
364800150225     c*  non la propongo senza MIC e se presente disguido o IMP
364900150220     c                   if        d98dee>d98ddc and evemic<>'SI'
365000150220     c                             and IDdata>0
365100150220     c                   else
365200150220
365300140820     c                   eval      d98dee=d98ddc
365400140925     c                   clear                   d98oee
365500150320     c* se la data è > di oggi stato PRV se non è in IMP
365600150320     c                   if        d98dee>wdaodi
365700150320     c                   if        d98spcdee<>'IMP'
365800150311     c                   eval      d98spcdee='PRV'
365900150320     c                   endif
366000150320     c
366100150225     c                   else
366200150217     c* se c'è il MIC
366300150217     c                   if        evemic='SI'
366400150217     c                   eval      d98spcdee='DDC'
366500150330     C***                else
366600150304     c* solo se in PRV, metto lo stato
366700150330     c* questo caso lo asterisco perchè dovrebbe essere già stato gestito
366800150330     c*  prima in quanto devo tenere conto di due aggiunte all'orario limite tab.OLP
366900150330     c***                if        d98spcdee='PRV'
367000150330     c***                eval      d98spcdee='PRG'
367100150330     c***                endif
367200150225     c                   endif
367300150217     c                   endif
367400150220     c                   endif
367500150217     c
367600150311     c                   endsl
367700931202     C*
367800931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
367900931202     C     WPODCI        IFEQ      *ZEROS
368000931202     C                   Z-ADD     *ZEROS        D98DCI
368100931202     C                   Z-ADD     *ZEROS        D98HCI
368200931202     C                   ELSE
368300931202     C                   Z-ADD     WPODCI        WORE
368400931202     C                   EXSR      OREDAT
368500931202     C                   Z-ADD     WDAT          D98DCI
368600090902     c                   if        d98tci<>10
368700931202     C                   ADD       12            WRES
368800090902     c                   else
368900090902     C                   ADD       10            WRES
369000090902     c                   endif
369100090902
369200931202     C     WRES          MULT      100           D98HCI
369300090901     c
369400090903     c                   if        d98hci=1000
369500090901     c                   eval      d98hci=1030
369600090901     c                   endif
369700090901     c
369800931202     C                   END
369900931202     C*
370000931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
370100931202     C     WPODCE        IFEQ      *ZEROS
370200931202     C                   Z-ADD     *ZEROS        D98DCE
370300931202     C                   Z-ADD     *ZEROS        D98HCE
370400931202     C                   ELSE
370500931202     C                   Z-ADD     WPODCE        WORE
370600931202//// C**************       SUB  12        WORE
370700931202     C                   EXSR      OREDAT
370800931202     C                   Z-ADD     WDAT          D98DCE
370900090902     c                   if        d98tcc >10
371000931202     C                   ADD       12            WRES
371100090902     c                   else
371200090902     C                   ADD       10            WRES
371300090902     c                   endif
371400090902     c
371500931202     C     WRES          MULT      100           D98HCE
371600090902     c
371700090903     c                   if        d98hce=1000
371800090902     c                   eval      d98hce=1030
371900090902     c                   endif
372000090902     c
372100931202     C                   END
372200960229     C*
372300960229     C                   ENDIF                                                  <-- 1 ---
372400930604     C*
372500930604     C                   ENDSR
372600150218     C********************************************************************
372700150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
372800150218     C********************************************************************
372900150218     c     LetBRV        BEGSR
373000150219     c                   clear                   bollaOK           1
373100150219     c                   clear                   IDnpg
373200150219     c                   clear                   IDdata
373300150219     c                   clear                   IDora
373400150219     c                   clear                   IDfgs
373500150219     c
373600150218     C     Kart          SETLL     FNART01L
373700150218     C     Kart          READE     FNART01L
373800150219    1c                   dow       not %eof(fnart01l)
373900150219     c                   clear                   DISfgs
374000150219     c                   clear                   DISdata
374100150219     c                   clear                   DISnpg
374200150219     c                   clear                   DISora
374300150219     c                   clear                   IMPdata
374400150219     c                   clear                   IMPfgs
374500150218     c* per ogno collo verifico se in IMP o disguidato
374600150218     c* al primo che NON trovo, esco come --> OK
374700150218     c     kbrv          setll     fnbrv07l
374800150218     c     kbrv          reade     fnbrv07l
374900150219    2c                   dow       not %eof(fnbrv07l)
375000150219    3c                   if        brvatr='D'  or brvnpg=3
375100150218     c* prendo la data della spunta
375200150218     c                   clear                   fnlv53ds
375300150219     c                   eval      d53tla='L'
375400150218     C                   MOVEL     BRvNPG        D53NPG
375500150218     C                   Z-ADD     BRvNFV        D53NFV
375600150218     C                   MOVEL     BRvFGS        D53FGS
375700150218     C                   CALL      'FNLV53R'
375800150218     C                   PARM                    fnlv53ds
375900150218    4c                   if        d53err<>*blanks
376000150219    5c                   if        brvdcs>0
376100150218     c                   eval      d53dfv=brvdcs
376200150218     c                   else
376300150219    6c                   if        brvdfs>0
376400150218     c                   eval      d53dfv=brvdfs
376500150219    6c                   endif
376600150219    5c                   endif
376700150219    4c                   endif
376800150218     c
376900150219    4c                   select
377000150219     c* se si tratta di IMP --> memorizzo
377100150219    4c                   when      d53spg='P' and d53dfv>impdata
377200150218     c                   eval      IMPdata=d53dfv
377300150219     c                   eval      IMPfgs =brvfgs
377400150219     c* se si tratta di DISGUIDO
377500150218     c                   when      brvatr='D'
377600150219     c
377700150219    5c                   if        d53dfv=DISdata
377800150219     c* Verifico l'ora
377900150219    6c                   if        brvhcs>0 and brvhcs>DISora
378000150219     c                   eval      DISdata=d53dfv
378100150219     c                   eval      DISfgs =brvfgs
378200150219     c                   eval      DISnpg =brvnpg
378300150219     c                   eval      DISora =brvhcs
378400150219   x6c                   else
378500150219    7c                   if        brvhms>0 and brvhms>DISora
378600150219     c                   eval      DISdata=d53dfv
378700150219     c                   eval      DISfgs =brvfgs
378800150219     c                   eval      DISnpg =brvnpg
378900150219     c                   eval      DISora =brvhms
379000150219    7c                   endif
379100150219    6c                   endif
379200150219    5c                   endif
379300150219     c
379400150219    5c                   if        d53dfv>DISdata
379500150219     c                   eval      DISdata=d53dfv
379600150219     c                   eval      DISfgs =brvfgs
379700150219     c                   eval      DISnpg =brvnpg
379800150219     c                   eval      DISora =brvhcs
379900150219     c                   if        disora=0
380000150219     c                   eval      DISora =brvhms
380100150219     c                   endif
380200150219    5c                   endif
380300150219     c
380400150219    4c                   endsl
380500150219    3c                   endif
380600150219     c     kbrv          reade     fnbrv07l
380700150219    2c                   enddo
380800150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
380900150219     c*  esco dal ciclo: la bolla è OK
381000150219    2c                   if        impdata=0 and disdata=0
381100150219     c                   eval      bollaOK='S'
381200150219     c                   clear                   IDdata
381300150219     c                   clear                   IDfgs
381400150219     c                   clear                   IDora
381500150219     c                   clear                   IDnpg
381600150219   x2c                   else
381700150219     c* memorizzo la data più alta
381800150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
381900150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
382000150219     c                   eval      IDdata=IMPdata
382100150219     c                   eval      IDfgs=IMPfgs
382200150219     c                   eval      IDora=0
382300150219     c                   eval      IDnpg='P'
382400150219    3c                   endif
382500150219    3c                   if        (DISdata>0 and DISdata=IDdata
382600150219     c                             and IDnpg<>'P' and DISora>IDora) or
382700150219     c                             (DISdata>0 and DISdata>IDdata)
382800150219     c                   eval      IDdata=DISdata
382900150219     c                   eval      IDfgs=DISfgs
383000150219     c                   eval      IDora=DISora
383100150219     c                   eval      IDnpg=%char(DISnpg)
383200150219    3c                   endif
383300150219    2c                   endif
383400150218     c
383500150218     C     Kart          READE     FNART01L
383600150219    1c                   enddo
383700150219     c
383800150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
383900150219     c                   if        d98duc> 0 and d98duc>IDdata
384000150219     c                   eval      bollaOK='S'
384100150219     c                   clear                   IDdata
384200150219     c                   clear                   IDfgs
384300150219     c                   clear                   IDora
384400150219     c                   clear                   IDnpg
384500150219     c                   endif
384600150219     c
384700150218     c
384800150218     C                   ENDSR
384900140825     C********************************************************************
385000140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
385100140825     C********************************************************************
385200140825     c     TRAS_DEI      BEGSR
385300140825     C     WPODEI        IFEQ      *ZEROS
385400140825     C                   Z-ADD     *ZEROS        D98DEI
385500140825     C                   Z-ADD     *ZEROS        D98OEI
385600140825     C                   ELSE
385700140825     C                   Z-ADD     WPODEI        WORE
385800140825     C                   SUB       12            WORE
385900140825     C                   EXSR      OREDAT
386000140825     c
386100140825     C                   Z-ADD     WDAT          D98DEI
386200140825     c                   if        d98tci<>10
386300140825     C                   ADD       12            WRES
386400140825     c                   else
386500140825     C                   ADD       10            WRES
386600140825     c                   endif
386700140825     c
386800140825     C     WRES          MULT      100           D98OEI
386900140825     c
387000140825     c                   if        d98oei=1000
387100140825     c                   eval      d98oei=1030
387200140825     c                   endif
387300140825     c
387400140825     C                   END
387500140825     c                   ENDSR
387600930604     C********************************************************************
387700930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
387800930604     C********************************************************************
387900930604     C     CHKFOR        BEGSR
388000930604     C*
388100930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
388200930604     C* E QUINDI FORZO LA CONFORMITA' A SI
388300941227     C     D98ERR        IFEQ      '0'
388400930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
388500930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
388600930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
388700930604     C                   MOVEL     '1'           D98ERR
388800930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
388900930604     C                   END
389000930604     C                   END
389100990212     C*
389200990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
389300990212     C* E QUINDI FORZO LA CONFORMITA' A SI
389400990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
389500990212     C     D98ERR        IFEQ      '0'
389600990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
389700141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
389800990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
389900990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
390000990212     C                   MOVEL     '1'           D98ERR
390100990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
390200990212     C                   END
390300990212     C                   END
390400990308     C*
390500990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
390600990308     C* E QUINDI FORZO LA CONFORMITA' A SI
390700100614     C**   D98ERR        IFEQ      '0'
390800100614     C**   WFBR          IFEQ      'S'                                          *RESO
390900100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
391000100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
391100100614     C**                 MOVEL     '1'           D98ERR
391200100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
391300100614     C**                 END
391400100614     C**                 END
391500930604     C*
391600930604     C                   ENDSR
391700990212     C********************************************************************
391800990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
391900990212     C********************************************************************
392000990212     C     CHKDAN        BEGSR
392100990212     C*
392200990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
392300040809      *
392400040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
392500040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
392600040809      * legate alla spedizione che passo
392700040809     c                   clear                   fidn12ds
392800040809     c                   eval      i12aas = D98AAS
392900040809     c                   eval      i12lnp = D98LNP
393000040809     c                   eval      i12nrs = D98NRS
393100040809     c                   eval      i12nsp = D98NSP
393200040809     c                   eval      i12fel = 'E'
393300040809     c                   eval      i12fan = 'E'
393400040809     c*
393500040809     c                   call      'FIDN12R'
393600040809     c                   parm                    fidn12ds
393700040809     c*
393800040809     c* se non ci sono errori
393900040809     c                   if        o12err = ' '
394000040809     c* se numero di CA trovate maggiore di zero
394100040809     c                   if        o12nca > 0
394200040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
394300040809     c                   endif
394400040809     c                   endif
394500990212     C*
394600990212     C                   ENDSR
394700941228     C********************************************************************
394800941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
394900941228     C********************************************************************
395000941228     C     CALORP        BEGSR
395100941228     C*
395200941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
395300941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
395400941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
395500960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
395600950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
395700950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
395800950110     C                   ELSE
395900950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
396000950110     C                   ENDIF
396100941228     C                   EXSR      RO2COP
396200941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
396300941228     C* SPED. IL GIORNO DOPO
396400941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
396500941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
396600941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
396700941228     C*
396800941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
396900941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
397000941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
397100941228     C                   Z-ADD     WPODSP        WR2POI
397200950110     C                   Z-ADD     WPOPPC        WR2POF
397300941228     C                   EXSR      RO2COP
397400941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
397500941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
397600941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
397700941228     C*
397800941228     C                   ENDSR
397900930414     C********************************************************************
398000930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
398100930414     C********************************************************************
398200930414     C     CALORT        BEGSR
398300930414     C*
398400930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
398500931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
398600930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
398700931126     C                   Z-ADD     *ZEROS        W98HF
398800950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
398900950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
399000950110     C                   ELSE
399100950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
399200950110     C                   ENDIF
399300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
399400931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
399500930608     C                   ELSE
399600931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
399700930608     C                   END
399800931126     C                   EXSR      RO2COV
399900931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
400000930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
400100930414     C*
400200930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
400300930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
400400930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
400500931126     C                   Z-ADD     *ZEROS        W98HF
400600950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
400700931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
400800931126     C                   EXSR      RO2COV
400900931126     C                   Z-ADD     WR2NOV        W98RTC
401000930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
401100930414     C*
401200930414     C                   ENDSR
401300930414     C********************************************************************
401400930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
401500930414     C********************************************************************
401600930414     C     CALORC        BEGSR
401700930511     C*
401800930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
401900930511     C* I TEMPI DI RITARDO AD OGGI
402000930511     C     WPODCM        IFEQ      *ZEROS
402100960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
402200930511     C                   END
402300940117     C*
402400940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
402500940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
402600940117     C* ALLA DATA DI FINE CONSEGNABILITA'
402700940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
402800940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
402900940118££££ C     WPODFC        IFNE      *ZEROS
403000940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
403100940118££££ C                   END
403200930414     C*
403300930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
403400930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
403500930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
403600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
403700931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
403800930608     C                   ELSE
403900931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
404000930608     C                   END
404100931126     C                   Z-ADD     WPODCM        WR5POF            9 0
404200931126     C                   EXSR      RO5COV
404300931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
404400930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
404500930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
404600060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
404700060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
404800930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
404900930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
405000930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
405100930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
405200930414     C*
405300930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
405400930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
405500930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
405600931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
405700931126     C                   Z-ADD     WPODCM        WR5POF            9 0
405800931126     C                   EXSR      RO5COV
405900931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
406000930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
406100930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
406200060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
406300060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
406400930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
406500930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
406600930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
406700930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
406800930414     C*
406900930414     C                   ENDSR
407000930414     C********************************************************************
407100930414     C*   CALORR - CALCOLA ORE REALI DI RESA
407200930414     C********************************************************************
407300930414     C     CALORR        BEGSR
407400930414     C*
407500950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
407600950110     C                   ADD       W98RTI        W98RRI
407700941228     C                   ADD       W98RCI        W98RRI
407800950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
407900950110     C                   ADD       W98RTC        W98RRC
408000941228     C                   ADD       W98RCC        W98RRC
408100930414     C*
408200930414     C                   ENDSR
408300930420     C********************************************************************
408400930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
408500930420     C********************************************************************
408600930420     C     CALDIF        BEGSR
408700930420     C*
408800941228     C* PARTENZA
408900941228     C     W98RPI        SUB       D98TPI        W98FPI
409000941228     C     W98RPC        SUB       D98TPC        W98FPC
409100941228     C*
409200930420     C* TRAZIONE
409300930423     C     W98RTI        SUB       D98TTI        W98FTI
409400930423     C     W98RTC        SUB       D98TTC        W98FTC
409500930420     C*
409600930420     C* CONSEGNA
409700090827     C***  W98RCI        SUB       D98TCI        W98FCI
409800090827     C     W98RCI        SUB       w99TCI        W98FCI
409900090827     C***  W98RCC        SUB       D98TCC        W98FCC
410000090827     C     W98RCC        SUB       w99TCC        W98FCC
410100930420     C*
410200930420     C* RESA
410300090921     C***  W98RRI        SUB       D98TRI        W98FRI
410400090921     C     W98RRI        SUB       w99TRI        W98FRI
410500090921     c
410600090921     C***  W98RRC        SUB       D98TRC        W98FRC
410700090921     C     W98RRC        SUB       w99TRC        W98FRC
410800930420     C*
410900930420     C                   ENDSR
411000930420     C********************************************************************
411100930420     C*   CALCNF - CALCOLA LE CONFORMITA'
411200930420     C********************************************************************
411300930420     C     CALCNF        BEGSR
411400941228     C*
411500941228     C*-------------------------------------------------------------------
411600941228     C* CONFORMITA' PARTENZA  INTERNA
411700941228     C*-------------------------------------------------------------------
411800941228     C                   Z-ADD     W98FPI        W98NPI
411900941228     C     W98FPI        IFLE      *ZEROS
412000941228     C                   MOVEL     'S'           D98CPI
412100941228     C                   ELSE
412200941228     C                   MOVEL     'N'           D98CPI
412300941228     C                   END
412400941228     C*
412500941228     C*-------------------------------------------------------------------
412600941228     C* CONFORMITA' PARTENZA  CLIENTE
412700941228     C*-------------------------------------------------------------------
412800941228     C                   Z-ADD     W98FPC        W98NPC
412900941228     C     W98FPC        IFLE      *ZEROS
413000941228     C                   MOVEL     'S'           D98CPC
413100941228     C                   ELSE
413200941228     C                   MOVEL     'N'           D98CPC
413300941228     C                   END
413400930420     C*
413500930420     C*-------------------------------------------------------------------
413600930420     C* CONFORMITA' TRAZIONE  INTERNA
413700930420     C*-------------------------------------------------------------------
413800930510     C                   Z-ADD     W98FTI        W98NTI
413900930423     C     W98FTI        IFLE      *ZEROS
414000930420     C                   MOVEL     'S'           D98CTI
414100930420     C                   ELSE
414200930420     C                   MOVEL     'N'           D98CTI
414300930420     C                   END
414400930420     C*
414500930420     C*-------------------------------------------------------------------
414600930420     C* CONFORMITA' TRAZIONE  CLIENTE
414700930420     C*-------------------------------------------------------------------
414800930510     C                   Z-ADD     W98FTC        W98NTC
414900930423     C     W98FTC        IFLE      *ZEROS
415000930420     C                   MOVEL     'S'           D98CTC
415100930420     C                   ELSE
415200930420     C                   MOVEL     'N'           D98CTC
415300930420     C                   END
415400930420     C*
415500930420     C*-------------------------------------------------------------------
415600930420     C* CONFORMITA' CONSEGNA  INTERNA
415700930420     C*-------------------------------------------------------------------
415800150226     c                   if        wmancatraz='S'
415900150226     c                   clear                   d98dti
416000150226     c                   clear                   d98hti
416100150226     c                   endif
416200150226     c
416300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
416400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
416500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
416600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
416700050914     c                   if        consideraora='S'
416800050914     c     w98fci        sub       24            w98nci
416900050914     c                   add       24            d98hrc
417000050914     c                   else
417100930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
417200050914     c                   endif
417300080116     c
417400110923     c                   select
417500080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
417600080116     c*  --> trasformo il ritardo in 0
417700090827     c                   when      d98cca='2'
417800080116     c                   z-add     0             w98nci
417900090827
418000080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
418100080116     c*  e sto leggendo la bolla in arrivo
418200080116     c*  --> trasformo il ritardo = 0
418300090827     c                   when      d98cca='1' and d98tbo='A'
418400080116     c                   z-add     0             w98nci
418500090902     c
418600090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
418700090902     c                   when      wmancaTRAZ='S'
418800090902     c                   z-add     0             w98nci
418900090902     c                   clear                   d98dti
419000090902     c                   clear                   d98hti
419100110923     c
419200110923     c* Ignoro ritardo se pefect order
419300130221     c                   when      d98xco >*zeros
419400110923     c                   z-add     0             w98nci
419500090827     c
419600090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
419700091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
419800091014     c*  controllo ora soglia  consegna da tabella OSC)
419900091009     c                   when      d98tci=10   and
420000091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
420100091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
420200091014     c*
420300091014     c                             d98hci=1030
420400091014     C                   MOVEL     'OSC  '       WR3TAB
420500091014     C                   eval      WR3HHI=d98hmc
420600091014     C                   EXSR      RO3ORE
420700131218
420800091014     c                   if        wr3matpom='M' and d98hmc>1030
420900131218     c                   exsr      ctrLAVH1030
421000131218
421100131218     c                   if        wlav1030<>'S'
421200091014     c                   add       12            w98nci
421300091014     c                   endif
421400131218     c                   endif
421500090928     c
421600090827     c                   endsl
421700050914     c
421800930524     C                   ELSE                                                   <-- 1 -->
421900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
422000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
422100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
422200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
422300930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
422400930608     C                   ELSE
422500930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
422600930608     C                   END
422700930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
422800930525     C                   EXSR      RO2COV
422900930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
423000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
423100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
423200930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
423300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
423400930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
423500930608     C                   ELSE
423600930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
423700930608     C                   END
423800931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
423900930525     C                   EXSR      RO2COV
424000930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
424100930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
424200930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
424300930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
424400930525     C* E' IL RITARDO
424500930610     C     WNOCRI        SUB       W98RCI        W98NCI
424600050901     c
424700050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
424800050901     c*  --> trasformo il ritardo in anticipo
424900050901     c                   if        d98cca='2'
425000080116     c***                z-sub     w98nci        w98nci
425100080116     c                   z-add     0             w98nci
425200050901     c                   endif
425300111018     c* Ignoro ritardo se pefect order
425400130221     c                   if        d98xco >*zeros
425500111018     c                   z-add     0             w98nci
425600111018     c                   endif
425700061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
425800061121     c*  e sto leggendo la bolla in arrivo
425900061121     c*  --> trasformo il ritardo in anticipo
426000061121     c                   if        d98cca='1' and d98tbo='A'
426100080116     c***                z-sub     w98nci        w98nci
426200080116     c                   z-add     0             w98nci
426300061121     c                   endif
426400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
426500070827     c*  mittente, --> consegnato in anticipo
426600070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
426700140403     c                   if        Callar5=' '
426800070827     c                   exsr      LeggiAR5
426900140403     c                   endif
427000070827     c
427100070827     c                   if        dcrpart<>'S'
427200070824     c                   z-sub     w98nci        w98nci
427300070824     c                   endif
427400070827     c
427500070824     c                   endif
427600050907     c
427700050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
427800050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
427900050907     c*  lo trasformo in anticipo perchè non sono in ritardo
428000050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
428100050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
428200050916     c***                if        d98dfc>0
428300070828     c                   if        d98dcm=0  and w98nci>0
428400050907     c                   z-sub     w98nci        w98nci
428500050907     c                   sub       oreILoDOPO    w98nci
428600050907     c                   endif
428700050901     c
428800930525     C                   ELSE                                                   <-- 2 -->
428900070521     c* non do ritardo se la consegna reale cade nel giorno
429000070521     c*  della consegna richiesta se presente IL
429100100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
429200070521     c                             wpodcm<=wpocr1
429300070521     c                   clear                   w98nci
429400100224     c                   else                                                   ---2a -->
429500930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
429600090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
429700090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
429800930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
429900930525     C* E' IL RITARDO O L'ANTICIPO
430000930525     C     W98RCI        SUB       WNOCRF        W98NCI
430100070115     c
430200070115     c* non do ritardo se comunque la consegna reale cade nel giorno
430300070115     c*  della consegna richiesta se presente IL
430400070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
430500070521     c***                          wpodcm<=wpocr1  and w98nci>0
430600070521     c***                clear                   w98nci
430700070521     c***                endif
430800070115     c
430900930525     C                   ELSE                                                   <-- 3 -->
431000930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
431100930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
431200930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
431300930525     C                   END                                                    <-- 3 ---
431400100224     C                   END                                                    <-- 2a---
431500100224
431600100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
431700100224     c                   select
431800100224     c                   when      d98cca='2'
431900100224     c                   z-add     0             w98nci
432000100224
432100100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
432200100224     c*  e sto leggendo la bolla in arrivo
432300100224     c                   when      d98cca='1' and d98tbo='A'
432400100224     c                   z-add     0             w98nci
432500100224     c                   endsl
432600100224     c
432700100224     C                   END                                                    <-- 2 ---
432800070521     C                   END                                                    <-- 1 ---
432900070521     c
433000970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
433100970915     C*/         WAZNCI    IFEQ 'S'
433200970915     C*/         W98NCI    ANDLT*ZEROS
433300970915     C*/                   Z-ADD*ZEROS    W98NCI
433400970915     C*/                   ENDIF
433500970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
433600050922     c* lo faccio solo se non c'e' la data cons richiesta
433700970915     C     D98DTI        IFEQ      *ZEROS
433800050922     c     d98dcr        andeq     *zeros
433900970915     C                   Z-ADD     *ZEROS        W98NCI
434000970915     C                   ENDIF
434100930524     C*
434200930524     C* CONTROLLO CONFORMITA
434300930423     C     W98NCI        IFLE      *ZEROS
434400930524     C                   MOVEL     'S'           D98CCI
434500930420     C                   ELSE
434600930524     C                   MOVEL     'N'           D98CCI
434700930420     C                   END
434800930420     C*
434900930420     C*-------------------------------------------------------------------
435000930420     C* CONFORMITA' CONSEGNA  CLIENTE
435100930420     C*-------------------------------------------------------------------
435200930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
435300930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
435400930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
435500050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
435600050914     c                   if        consideraora='S'
435700050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
435800050914     c                   else
435900050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
436000050914     c                   endif
436100080116     c
436200080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
436300080116     c*  --> trasformo il ritardo =0
436400090827     c                   select
436500090827     c                   when      d98cca='2'
436600080116     c                   z-add     0             w98ncc
436700090827
436800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
436900080116     c*  e sto leggendo la bolla in arrivo
437000080116     c*  --> trasformo il ritardo =0
437100090827     c                   when      d98cca='1' and d98tbo='A'
437200080116     c                   z-add     0             w98ncc
437300090902     c
437400090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
437500090902     c                   when      wmancaTRAZ='S'
437600090902     c                   z-add     0             w98ncc
437700110923     c* Ignoro ritardo se Perfect Order
437800130221     c                   when      d98xco >*zeros
437900110923     c                   z-add     0             w98ncc
438000090902     c
438100090827     c
438200090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
438300090827     c* sono  in ritardo di 12 ore
438400091009     c                   when      d98tcc=10  and
438500091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
438600091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
438700091014     c*
438800091014     c                             d98hce=1030
438900091014     C                   MOVEL     'OSC  '       WR3TAB
439000091014     C                   eval      WR3HHI=d98hmc
439100091014     C                   EXSR      RO3ORE
439200091014     c                   if        wr3matpom='M' and d98hmc>1030
439300131218     c                   exsr      ctrLAVH1030
439400131218
439500131218     c                   if        wlav1030<>'S'
439600131218     c                   add       12            w98ncc
439700131218     c                   endif
439800131218     c****               add       12            w98ncc
439900091014     c                   endif
440000131218     c
440100091014     c***                z-add     12            w98ncc
440200090827     c                   endsl
440300050914     c
440400930524     C                   ELSE                                                   <-- 1 -->
440500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
440600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
440700930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
440800930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
440900930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
441000930525     C                   EXSR      RO2COV
441100930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
441200930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
441300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
441400930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
441500930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
441600931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
441700930525     C                   EXSR      RO2COV
441800930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
441900930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
442000930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
442100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
442200930525     C* E' IL RITARDO
442300930525     C     WNOCRI        SUB       W98RCC        W98NCC
442400050901     c
442500050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
442600050901     c*  --> trasformo il ritardo in anticipo
442700050901     c                   if        d98cca='2'
442800080116     c***                z-sub     w98ncc        w98ncc
442900080116     c                   z-add     0             w98ncc
443000050901     c                   endif
443100111018     c* Ignoro ritardo se pefect order
443200130221     c                   if        d98xco >*zeros
443300111018     c                   z-add     0             w98ncc
443400111018     c                   endif
443500061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
443600061121     c*  e sto leggendo la bolla in arrivo
443700061121     c*  --> trasformo il ritardo in anticipo
443800061121     c                   if        d98cca='1' and d98tbo='A'
443900080116     c***                z-sub     w98ncc        w98ncc
444000080116     c                   z-add     0             w98ncc
444100061121     c                   endif
444200070824     c
444300070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
444400070827     c*  mittente, --> consegnato in anticipo
444500070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
444600070827     c* SBR per vedere se tassativa
444700070827     c                   if        Callar5=' '
444800070827     c                   EXSR      LeggiAR5
444900070827     c                   endif
445000070827     c                   if        dcrpart<>'S'
445100070824     c                   z-sub     w98ncc        w98ncc
445200070824     c                   endif
445300070824     c                   endif
445400050907     c
445500050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
445600050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
445700050907     c*  lo trasformo in anticipo perchè non sono in ritardo
445800050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
445900050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
446000050916     c***                if        d98dfc>0
446100070828     c                   if        d98dcm=0 and w98ncc>0
446200050907     c                   z-sub     w98ncc        w98ncc
446300050907     c                   sub       oreILoDOPO    w98ncc
446400050907     c                   endif
446500050901     c
446600930525     C                   ELSE                                                   <-- 2 -->
446700070521     c* Non do ritardo se la consegna reale cade nel giorno
446800070521     c*  della consegna richiesta se presente IL
446900100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
447000070521     c                             wpodcm<=wpocr2
447100070521     c                   clear                   w98ncc
447200100224     c                   else                                                   <--2a -->
447300070521     c
447400930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
447500090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
447600090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
447700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
447800930525     C* E' IL RITARDO O L'ANTICIPO
447900930525     C     W98RCC        SUB       WNOCRF        W98NCC
448000070115     c
448100070115     c* non do ritardo se comunque la consegna reale cade nel giorno
448200070115     c*  della consegna richiesta se presente IL
448300070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
448400070521     c***                          wpodcm<=wpocr2 and w98ncc>0
448500070521     c***                clear                   w98ncc
448600070521     c***                endif
448700070115     c
448800930525     C                   ELSE                                                   <-- 3 -->
448900930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
449000930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
449100930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
449200930525     C                   END                                                    <-- 3 ---
449300100224     C                   END                                                    <-- 2a---
449400100224
449500100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
449600100224     c                   select
449700100224     c                   when      d98cca='2'
449800100224     c                   z-add     0             w98nci
449900100224
450000100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
450100100224     c*  e sto leggendo la bolla in arrivo
450200100224     c                   when      d98cca='1' and d98tbo='A'
450300100224     c                   z-add     0             w98nci
450400100224     c                   endsl
450500100224     c
450600100224     C                   END                                                    <-- 2 ---
450700070521     C                   END                                                    <-- 1 ---
450800930524     C*
450900930524     C* CONTROLLO CONFORMITA
451000930423     C     W98NCC        IFLE      *ZEROS
451100930524     C                   MOVEL     'S'           D98CCC
451200930420     C                   ELSE
451300930524     C                   MOVEL     'N'           D98CCC
451400930420     C                   END
451500930420     C*
451600930420     C*-------------------------------------------------------------------
451700930420     C* CONFORMITA' RESA INTERNA
451800930420     C*-------------------------------------------------------------------
451900930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
452000930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
452100930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
452200050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
452300050914     c                   if        consideraora='S'
452400050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
452500050914     c                   else
452600050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
452700050914     c                   endif
452800080116     c
452900080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
453000090827     c                   select
453100090827     c                   when      d98cca='2' and w98nri>0
453200080116     c                   z-add     0             w98nri
453300090827     c
453400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
453500080116     c*  e sto leggendo la bolla in arrivo
453600090827     c                   when      d98cca='1' and d98tbo='A' and
453700080116     c                                            w98nri>0
453800080116     c                   z-add     0             w98nri
453900110923     c
454000110923     c* Ignoro ritardo se Perfect Order
454100130221     c                   when      d98xco >*zeros
454200110923     c                   z-add     0             w98nri
454300110923     c
454400090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
454500090827     c* sono  in ritardo di 12 ore
454600091009     c                   when      d98tci=10  and
454700091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
454800091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
454900091014     c*
455000091014     c                             d98hci=1030
455100091014     C                   MOVEL     'OSC  '       WR3TAB
455200091014     C                   eval      WR3HHI=d98hmc
455300091014     C                   EXSR      RO3ORE
455400091014     c                   if        wr3matpom='M' and d98hmc>1030
455500131218     c                   exsr      ctrLAVH1030
455600131218
455700131218     c                   if        wlav1030<>'S'
455800091014     c                   add       12            w98nri
455900091014     c                   endif
456000131218     c                   endif
456100091014     c
456200091014     c***                z-add     12            w98nri
456300090827     c                   endsl
456400090827
456500930524     C                   ELSE                                                   <-- 1 -->
456600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
456700930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
456800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
456900931206     C     WPODCM        ANDLE     WPOCR1
457000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
457100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
457200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
457300931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
457400930525     C                   EXSR      RO2COV
457500930525     C     WR2NOV        IFNE      *ZEROS
457600930525     C     WR2NOV        MULT      -1            W98NRI
457700930525     C                   END
457800930524     C                   ELSE                                                   <-- 2 -->
457900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
458000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
458100930524     C     WPODCM        IFLT      WPOCRI
458200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
458300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
458400930524     C                   ELSE
458500931206     C                   Z-ADD     WPOCR1        WPORFI
458600930524     C                   Z-ADD     WPODCM        WPORFF
458700930524     C                   END
458800930524     C* CALCOLO ORE DI DIFFERENZA
458900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
459000930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
459100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
459200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
459300930524     C                   EXSR      RO2COV
459400930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
459500050902     c
459600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
459700100224     c                   if        d98cca='2' and w98nri>0
459800100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
459900080116     c***                z-sub     w98nri        w98nri
460000080116     c                   z-add     0             w98nri
460100050902     c                   endif
460200111018     c* Ignoro ritardo se pefect order
460300130221     c                   if        d98xco >*zeros
460400111018     c                   z-add     0             w98nri
460500111018     c                   endif
460600061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
460700061121     c*  e sto leggendo la bolla in arrivo
460800100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
460900100224     c***                if        d98cca='1' and d98tbo='A' and
461000100224     c***                                         w98nri>0 and wpodcm<wpocri
461100080116     c***                z-sub     w98nri        w98nri
461200080116     c                   z-add     0             w98nri
461300061121     c                   endif
461400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
461500070827     c*  mittente, --> consegnato in anticipo
461600070828     c                   if        d98tcr=' ' and d98nsp>0  and
461700070828     c                                            w98nri>0 and wpodcm<wpocri
461800070827     c                   if        CallAR5=' '
461900070827     c                   EXSR      LeggiAR5
462000070824     c                   endif
462100070827     c                   if        dcrpart<>'S'
462200070824     c                   z-sub     w98nri        w98nri
462300070824     c                   endif
462400070824     c                   endif
462500050907     c
462600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
462700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
462800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
462900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
463000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
463100050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
463200050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
463300050907     c                   z-sub     w98nri        w98nri
463400050907     c                   sub       oreILoDOPO    w98nri
463500050907     c                   endif
463600050907     c
463700050907     C                   END                                                    <-- 2 ---
463800930524     C                   END                                                    <-- 1 ---
463900930420     C* CALCOLO LE ORE DI RESA
464000930420     C* CONTROLLO CONFORMITA
464100930423     C     W98NRI        IFLE      *ZEROS
464200930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
464300930420     C                   ELSE
464400930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
464500930420     C                   END
464600930420     C*
464700930420     C*-------------------------------------------------------------------
464800930420     C* CONFORMITA' RESA CLIENTE
464900930420     C*-------------------------------------------------------------------
465000930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
465100930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
465200930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
465300050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
465400050914     c                   if        consideraora='S'
465500050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
465600050914     c                   else
465700050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
465800050914     c                   endif
465900090827     c
466000080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
466100090827     c                   select
466200090827     c                   when      d98cca='2' and w98nrc>0
466300080116     c                   z-add     0             w98nrc
466400090827
466500080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
466600080116     c*  e sto leggendo la bolla in arrivo
466700090827     c                   when      d98cca='1' and d98tbo='A' and
466800080116     c                                            w98nrc>0
466900080116     c                   z-add     0             w98nrc
467000110923     c
467100110923     c* Ignoro ritardo se Perfect Order
467200130221     c                   when      d98xco >*zeros
467300110923     c                   z-add     0             w98nrc
467400090827     c
467500090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
467600090827     c* sono  in ritardo di 12 ore
467700091009     c                   when      d98tcc=10  and
467800091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
467900091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
468000091014     c                             d98hce=1030
468100091014     C                   MOVEL     'OSC  '       WR3TAB
468200091014     C                   eval      WR3HHI=d98hmc
468300091014     C                   EXSR      RO3ORE
468400091014     c                   if        wr3matpom='M' and d98hmc>1030
468500131218     c                   exsr      ctrLAVH1030
468600131218
468700131218     c                   if        wlav1030<>'S'
468800091014     c                   add       12            w98nrc
468900091014     c                   endif
469000131218     c                   endif
469100091014     c
469200091014     c**                 z-add     12            w98nrc
469300090827     c                   endsl
469400090827     c
469500930524     C                   ELSE                                                   <-- 1 -->
469600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
469700930524     C* DI DIFFERENZA SONO UGUALI A ZERO
469800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
469900931206     C     WPODCM        ANDLE     WPOCR2
470000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
470100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
470200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
470300931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
470400930525     C                   EXSR      RO2COV
470500930525     C     WR2NOV        IFNE      *ZEROS
470600930525     C     WR2NOV        MULT      -1            W98NRC
470700930525     C                   END
470800930524     C                   ELSE                                                   <-- 2 -->
470900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
471000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
471100930524     C     WPODCM        IFLT      WPOCRI
471200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
471300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
471400930524     C                   ELSE
471500931206     C                   Z-ADD     WPOCR2        WPORFI
471600930524     C                   Z-ADD     WPODCM        WPORFF
471700930524     C                   END
471800930524     C* CALCOLO ORE DI DIFFERENZA
471900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
472000930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
472100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
472200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
472300930524     C                   EXSR      RO2COV
472400930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
472500050902     c
472600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
472700100224     c                   if        d98cca='2' and w98nrc>0
472800100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
472900080116     c***                z-sub     w98nrc        w98nrc
473000080116     c                   z-add     0             w98nrc
473100050902     c                   endif
473200111018     c* Ignoro ritardo se pefect order
473300130221     c                   if        d98xco >*zeros
473400111018     c                   z-add     0             w98nrc
473500111018     c                   endif
473600070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
473700061121     c*  e sto leggendo la bolla in arrivo
473800100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
473900100224     c***                if        d98cca='1' and d98tbo='A' and
474000100224     c***                                         w98nrc>0 and wpodcm<wpocri
474100080116     c***                z-sub     w98nrc        w98nrc
474200080116     c                   z-add     0             w98nrc
474300061121     c                   endif
474400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
474500070827     c*  mittente, --> consegnato in anticipo
474600070828     c                   if        d98tcr=' ' and d98nsp>0 and
474700070828     c                                            w98nrc>0 and wpodcm<wpocri
474800070827     c                   if        CallAR5=' '
474900070827     c                   EXSR      LeggiAR5
475000070827     c                   endif
475100070827     c                   if        dcrpart<>'S'
475200070824     c                   z-sub     w98nrc        w98nrc
475300070824     c                   endif
475400070824     c                   endif
475500050907     c
475600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
475700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
475800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
475900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
476000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
476100050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
476200050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
476300050907     c                   z-sub     w98nrc        w98nrc
476400050907     c                   sub       oreILoDOPO    w98nrc
476500050907     c                   endif
476600050907     c
476700050907     C                   END                                                    <-- 2 ---
476800930524     C                   END                                                    <-- 1 ---
476900930420     C* CALCOLO LE ORE DI RESA
477000930420     C* CONTROLLO CONFORMITA
477100930423     C     W98NRC        IFLE      *ZEROS
477200930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
477300930420     C                   ELSE
477400930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
477500930420     C                   END
477600930420     C*
477700930420     C                   ENDSR
477800070827     c
477900131218     c********************************************************************
478000131218     c     CtrlavH1030   BEGSR
478100131218     c********************************************************************
478200131218     c                   clear                   wlav1030          1
478300140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
478400131218     c                   z-add     1             xx                4 0
478500131218     c                   dow       evc(xx)<>*blanks
478600140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
478700140127     c                   if        evd(xx)=d98dcm
478800131218     c                   if        evo(xx)<=1030
478900131218     c                   eval      wlav1030='S'
479000131218     c                   leave
479100131218     c                   endif
479200131218     c                   endif
479300131218     c                   add       1             xx
479400131218     c                   enddo
479500131218     c                   ENDSR
479600070827     c********************************************************************
479700070827     c* Controllo record GEN di FIAR5
479800070827     c********************************************************************
479900070827     c     LeggiAR5      BEGSR
480000070827     c                   clear                   DCRPART           1
480100141104     c                   clear                   dar5gen
480200141104     c                   clear                   gc1PART           2
480300141104     c                   clear                   gc2PART           2
480400070827     c     kar5          chain     fiar501l
480500070827    1c                   if        not %found
480600070827     c                   eval      dcrpart='S'
480700141104     c                   eval      gc1part=d98gc1
480800141104     c                   eval      gc2part=d98gc2
480900070827   x1c                   else
481000070827     c* i dati devono essere quelli immessi dalla partenza
481100070827     c                   movel     ar5uni        dar5gen
481200070827     c
481300070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
481400070827     c*  ma il record GEN è stato scritto per altri motivi
481500070827    2c                   if        §ar5mdp='S'
481600070827     c                   movel     d98dcr        alfadcr           8
481700141104     c                   eval      gc1part=§ar5gc1
481800141104     c                   eval      gc2part=§ar5gc2
481900070827     c
482000070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
482100070827     c                   eval      dcrpart='S'
482200070827    3c                   endif
482300070827     c
482400070827   x2c                   else
482500070827     c                   eval      dcrpart='S'
482600141106     c                   eval      gc1part=d98gc1
482700141106     c                   eval      gc2part=d98gc2
482800070827    2c                   endif
482900070827    1c                   endif
483000070827     c
483100140402     c* Memorizzo il flag pickup 2
483200140402     c                   eval      wpick2=§AR5PK2
483300140402     c
483400150401     c*
483500150414     c**                 if        d98cas>0 or d98ffd<>' ' or
483600150414     c**                           d98tc1='A'  or d98tc2='A' or
483700150414     c**                           d98tc1='S'  or d98tc2='S'
483800150401     c     kar4          chain     fiar401l
483900150401     c                   if        %found(fiar401l)
484000150401     c                   eval      dsbl4L=ar4not
484100150401     c                   endif
484200150401     c* Solo per fermo deposito verifico se inviato alert
484300150401     c                   if        d98ffd<>' '
484400150401     c                   clear                   fnlv82ds
484500150401     c                   eval      ILV82AAS=d98aas
484600150401     c                   eval      ILV82LNP=d98lnp
484700150401     c                   eval      ILV82NRS=d98nrs
484800150401     c                   eval      ILV82NSP=d98nsp
484900150401     c                   eval      ILV82TIPA='FD'
485000150401     c                   call      'FNLV82R'
485100150401     c                   parm                    fnlv82ds
485200150401     c                   endif
485300150414     c**                 endif
485400150401
485500150401     c                   eval      callar5='S'
485600150401
485700070827     c                   ENDSR
485800930913     C********************************************************************
485900930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
486000930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
486100930913     C*            DATA/ORA (FINE RANGE)
486200930913     C********************************************************************
486300930913     C     CALCNS        BEGSR
486400930914     C*
486500930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
486600930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
486700930914     C     WPODTI        IFEQ      *ZEROS
486800960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
486900930914     C                   ELSE
487000930914     C                   Z-ADD     WPODTI        WPOIRC
487100930914     C                   END
487200930914     C*
487300930914     C* SE NON ANCORA CONSEGNATA
487400930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
487500050922     c* in realtà nella data consegna c'e' la data fine calcolo
487600050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
487700930914     C     WPODCM        IFEQ      *ZEROS
487800050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
487900930914     C                   ELSE
488000930916     C                   Z-ADD     WPODCM        WPOFRC
488100930914     C                   END
488200930914     C*
488300930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
488400930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
488500930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
488600930914     C     WPODFC        ANDLT     WPOIRC
488700930914     C                   ELSE
488800930914     C*
488900930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
489000930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
489100930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
489200930914     C     WPODFC        ANDGT     WPOFRC
489300930914     C                   ELSE
489400930916     C*
489500930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
489600930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
489700930916     C     WPODFC        ANDGT     WPOFRC
489800930916     C                   ELSE
489900930916     C*
490000930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
490100930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
490200930916     C     WPODFC        ANDLT     WPOIRC
490300930916     C                   ELSE
490400930914     C*
490500930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
490600930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
490700930914     C     WPODIC        IFNE      *ZEROS
490800930914     C     WPODIC        ANDGT     WPOFRC
490900930914     C                   Z-ADD     WPOFRC        WPODIC
491000930914     C                   ENDIF
491100930914     C*
491200930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
491300930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
491400930914     C     WPODFC        IFGT      WPOFRC
491500930914     C                   Z-ADD     WPOFRC        WPODFC
491600930914     C                   ENDIF
491700930914     C*
491800930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
491900930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
492000930914     C     WPODIC        IFNE      *ZEROS
492100930914     C     WPODIC        ANDLT     WPOIRC
492200930914     C                   Z-ADD     WPOIRC        WPODIC
492300930914     C                   ENDIF
492400930914     C*
492500930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
492600930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
492700930914     C     WPODFC        IFLT      WPOIRC
492800930914     C                   Z-ADD     WPOIRC        WPODFC
492900930914     C                   ENDIF
493000930914     C*
493100930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
493200931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
493300930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
493400930916     C*
493500931210     C***********WPODIC    IFNE *ZEROS
493600931210     C***********WPODFC    ANDNE*ZEROS
493700931210     C*********************ADD  12        WPODFC
493800931210     C*********************ENDIF
493900930914     C     WPODIC        IFEQ      *ZEROS
494000930914     C     WPODFC        ANDNE     *ZEROS
494100931130     C                   Z-ADD     WPODFC        WPODIC
494200931210     C*********************ADD  12        WPODFC
494300930916     C                   ENDIF
494400930913     C*
494500930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
494600930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
494700930914     C* INTERNA
494800050908     c                   eval      Usacons='S'
494900930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
495000930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
495100931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
495200931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
495300940324     C                   EXSR      RO9COV
495400931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
495500930914     C* CLIENTE
495600930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
495700931210     C                   EXSR      RO9COV
495800931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
495900050908     c                   eval      Usacons=' '
496000930914     C*
496100930916     C                   ENDIF                                                  <-- 5 ---
496200930916     C                   ENDIF                                                  <-- 4 ---
496300930916     C                   ENDIF                                                  <-- 3 ---
496400930914     C                   ENDIF                                                  <-- 2 ---
496500930914     C                   ENDIF                                                  <-- 1 ---
496600930913     C*
496700930913     C                   ENDSR
496800930325     C********************************************************************
496900930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
497000930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
497100930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
497200930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
497300930325     C********************************************************************
497400930325     C     RO1CDP        BEGSR
497500930414     C*
497600931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
497700931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
497800931126     C*
497900931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
498000931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
498100150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
498200150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
498300931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
498400060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
498500930326     C*
498600930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
498700931130     C                   Z-ADD     WR1POI        WR1POO            9 0
498800930325     C*
498900930325     C* CICLO FINO A CHE:
499000930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
499100930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
499200930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
499300930406     C     WR1POO        ANDLT     WPOLIM
499400930414     C*
499500930407     C                   Z-ADD     WR1POO        WPOROU            9 0
499600930325     C                   MOVEL     'N'           WSLITT            1
499700930414     C*
499800930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
499900930414     C*
500000060420     C**   WSLITT        IFEQ      'N'                                          *ORA VALIDA
500100060420     C**                 EXSR      CHKGIA                                       *CONTR.GIACENZA
500200060420     C*+                 END
500300930414     C*
500400060420     C**   WSLITT        IFEQ      'N'                                          *ORA VALIDA
500500060420     C**                 EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
500600060420     C**                 END
500700930414     C*
500800060420     C**   WSLITT        IFEQ      'N'                                          *ORA VALIDA
500900060420     C**                 EXSR      CHKCEPA                                      *CONTR.LASC.AVVISO
501000060420     C**                 END
501100060420     C*
501200931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
501300930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
501400930414     C                   END
501500930511     C*
501600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
501700930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
501800930511     C                   END
501900930414     C*
502000930325     C     WSLITT        IFEQ      'N'
502100930423     C                   SUB       12            WR1NOI            5 0
502200930325     C                   END
502300931130     C                   ADD       12            WR1POO            9 0
502400930414     C*
502500931223     C                   ENDDO                                                  <-- 1 ---
502600931126     C*
502700931126     C                   ELSE                                                   <-- 0 -->
502800931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
502900931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
503000931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
503100960129     C     WPOLAF        IFGT      WPODCG
503200960129     C                   Z-ADD     WPOLAF        WTM090            9 0
503300960129     C                   ELSE
503400960129     C                   Z-ADD     WPODCG        WTM090
503500960129     C                   ENDIF
503600960129     C     WPOGIF        IFGT      WTM090
503700960129     C                   Z-ADD     WPOGIF        WTM090
503800960129     C                   ENDIF
503900060420     C     WPOpaif       IFGT      WTM090
504000060420     C                   Z-ADD     WPOpaif       WTM090
504100060420     C                   ENDIF
504200960129     C*
504300960129     C     12            ADD       WTM090        WR1POO
504400960129     C                   ENDIF                                                  <-- 0 ---
504500930512     C*
504600931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
504700931202     C     WR1POO        SUB       12            WR7POI
504800931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
504900931202     C                   ADD       12            WR7POO
505000931202     C     WR7POO        IFGT      WR1POO
505100931202     C                   Z-ADD     WR7POO        WR1POO
505200931202     C                   END
505300930406     C*
505400930325     C                   ENDSR
505500930414     C********************************************************************
505600930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
505700930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
505800930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
505900930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
506000930414     C********************************************************************
506100930414     C     RO2COV        BEGSR
506200091014     c                   clear                   salvaEveL
506300091014     c                   clear                   salvaEveG
506400931130     C*
506500930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
506600930414     C                   Z-ADD     *ZEROS        WR2NOV
506700930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
506800931130     C                   Z-ADD     WR2POI        WR2POW
506900930414     C*
507000930414     C* CICLO FINO A CHE:
507100930414     C*   - DATA INIZIO SUPERATO DATA FINE
507200930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
507300931223     C     WR2POW        ANDLT     WPOLIM
507400930414     C*
507500930414     C                   Z-ADD     WR2POW        WPOROU
507600930414     C                   MOVEL     'N'           WSLITT            1
507700930414     C*
507800930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
507900930414     C*
508000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
508100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
508200930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
508300930414     C                   END
508400930414     C*
508500930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
508600930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
508700930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
508800930618     C                   END
508900060420     c
509000060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
509100060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
509200060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
509300060420     C                   END
509400060420     c
509500931201     C*
509600931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
509700931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
509800931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
509900931201     C                   END
510000060418     c
510100060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
510200060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
510300060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
510400060418     C                   END
510500930414     C*
510600930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
510700930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
510800930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
510900930414     C                   END
511000930511     C*
511100930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
511200930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
511300930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
511400930511     C                   END
511500930414     C*
511600930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
511700930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
511800930414     C     WSLITT        IFEQ      'N'
511900930423     C                   ADD       12            WR2NOV            5 0
512000930414     C                   END
512100931130     C                   ADD       12            WR2POW            9 0
512200930414     C*
512300931223     C                   ENDDO                                                  <-- 1 ---
512400930414     C*
512500930414     C                   ENDSR
512600941228     C********************************************************************
512700941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
512800941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
512900941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
513000941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
513100941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
513200941228     C********************************************************************
513300941228     C     RO2COP        BEGSR
513400941228     C*
513500941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
513600941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
513700941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
513800941228     C                   Z-ADD     *ZEROS        WR2NOV
513900941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
514000941228     C                   Z-ADD     WR2POI        WR2POW
514100941228     C*
514200941228     C* CICLO FINO A CHE:
514300941228     C*   - DATA INIZIO SUPERATO DATA FINE
514400941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
514500941228     C*
514600950215     C                   Z-ADD     WR2POW        WPOROU
514700941228     C                   MOVEL     'N'           WSLITT
514800941228     C* TRASFORMA IL PROGR. ORE IN DATA
514900950215     C*******              Z-ADDWR2POW    WORE
515000950215     C*******              EXSR OREDAT
515100950215     C*******              Z-ADDWDAT      W12DAT  80
515200950215     C*******              Z-ADDWRES      W12RES  30
515300941228     C*
515400941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
515500941228     C*
515600941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
515700941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
515800941228     C     WSLITT        IFEQ      'N'
515900941228     C                   ADD       12            WR2NOV            5 0
516000941228     C                   END
516100941228     C                   ADD       12            WR2POW            9 0
516200941228     C*
516300941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
516400941228     C* ENTRI IN LOOP
516500941228     C     WR2NOV        IFGT      999
516600941228     C                   LEAVE                                                  *VA A ENDDO
516700941228     C                   END
516800941228     C*
516900941228     C                   ENDDO                                                  <-- 1 ---
517000941228     C*
517100941228     C                   ENDSR
517200930510     C********************************************************************
517300930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
517400930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
517500930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
517600930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
517700050905     c*                     il num ore deve essere un multiplo di 12
517800050905     c*                     oppure 1 ora
517900930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
518000930510     C********************************************************************
518100930510     C     RO4CDP        BEGSR
518200930510     C*
518300930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
518400930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
518500050905     c                   movel     '1'           wgiro             1
518600930510     C*
518700930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
518800930510     C                   Z-ADD     WR4POI        WR4POO            9 0
518900930510     C*
519000930510     C* CICLO FINO A CHE:
519100930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
519200930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
519300930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
519400930510     C     WR4POO        ANDLT     WPOLIM
519500930510     C*
519600141106     c                   if        nosomma<>'N'
519700931130     C                   ADD       12            WR4POO            9 0
519800141106     c                   endif
519900930510     C                   Z-ADD     WR4POO        WPOROU            9 0
520000930510     C                   MOVEL     'N'           WSLITT            1
520100930510     C*
520200930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
520300050905     c
520400050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
520500930510     C     WSLITT        IFEQ      'N'
520600050905     c
520700050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
520800050905     c*  se cado in giorno non festivo
520900050905     c                   if        wr4noi=1 and wgiro='1'
521000050905     c                   sub       12            wr4poo
521100050905     c                   add       wr4noi        wr4poo
521200050905     c                   endif
521300050905     c*
521400050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
521500050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
521600050905     c*  calcolate dal pgm
521700050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
521800050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
521900050905     c*  del giorno sucessivo non festivo
522000050905     c                   if        wr4noi=1 and wgiro<>'1'
522100050905     c                   add       wr4noi        wr4poo
522200050905     c                   endif
522300050905     c
522400930510     C                   SUB       12            WR4NOI            5 0
522500930510     C                   END
522600141106     c
522700141106     c                   if        nosomma= 'N'
522800141106     C                   ADD       12            WR4POO            9 0
522900141106     c                   endif
523000930510     C*
523100050905     c                   eval      wgiro='2'
523200931223     C                   ENDDO                                                  <-- 1 ---
523300930510     C*
523400930510     C                   ENDSR
523500931130     C********************************************************************
523600931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
523700931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
523800931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
523900931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
524000931130     C********************************************************************
524100931130     C     RO5COV        BEGSR
524200931130     C*
524300931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
524400060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
524500060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
524600060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
524700931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
524800931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
524900931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
525000931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
525100931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
525200931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
525300931130     C                   Z-ADD     *ZEROS        WR5NOV
525400940714     C*
525500940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
525600940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
525700931207     C*
525800931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
525900940908     C     WIC           IFEQ      'I'
526000090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
526100090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
526200931207     C                   ELSE
526300090827     C*****              Z-ADD     D98TCC        WPOTCT
526400090827     C                   Z-ADD     w99TCC        WPOTCT
526500931207     C                   END
526600931130     C*
526700931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
526800060420     C                   clear                   COV
526900931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
527000931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
527100931207     C                   Z-ADD     2             CO                3 0
527200931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
527300931207     C                   EXSR      RO5SUB
527400981102     C                   DO        99            IC                3 0
527500931207     C     LAF(IC)       IFNE      *ZEROS
527600931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
527700931207     C                   EXSR      RO5SUB
527800931207     C                   ENDIF
527900931207     C                   ENDDO
528000060420     c
528100060420     C                   DO        99            IC                3 0
528200060420     C     paif(IC)      IFNE      *ZEROS
528300060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
528400060420     C                   EXSR      RO5SUB
528500060420     C                   ENDIF
528600060420     C                   ENDDO
528700931207     C* ORDINA LA SCHIERA COV
528800931207     C                   SORTA     COV
528900931130     C*
529000931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
529100931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
529200981102     C                   DO        98            IC                             --- 1 -->
529300931207     C     IC            ADD       1             I1C               3 0
529400931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
529500931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
529600931207     C     COV(I1C)      OREQ      *ZEROS
529700931207     C                   ELSE
529800931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
529900931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
530000931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
530100931130     C                   EXSR      RO2COV
530200931207     C                   ADD       WR2NOV        WR5NOV            9 0
530300931209     C* SE NON SONO IN DATA CONSEGNA
530400931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
530500931207     C     WR5NOV        IFLT      WPOTCT
530600931209     C     COV(I1C)      ANDNE     WPODCM
5307009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5308009709111009 C     WPOTCT        SUB       12            WR5NOV
530900931130     C                   END
531000931207     C                   ENDIF                                                  <-- 2 ---
531100931207     C                   ENDDO                                                  <-- 1 ---
531200940714     C                   ENDIF                                                  <-- 0 ---
531300931130     C*
531400931130     C                   ENDSR
531500931207     C********************************************************************
531600931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
531700931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
531800931207     C********************************************************************
531900931207     C     RO5SUB        BEGSR
532000931207     C*
532100931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
532200931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
532300931207     C     WR5COV        ANDLT     WR5POF
532400931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
532500931207     C                   MOVEL     'N'           WR5PRE            1
532600981102     C*---                 DO   30        I
532700981102     C                   DO        99            I
532800931207     C     WR5COV        IFEQ      COV(I)
532900931207     C                   MOVEL     'S'           WR5PRE
533000931207     C                   LEAVE
533100931207     C                   ENDIF
533200931207     C                   ENDDO
533300931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
533400931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
533500931207     C                   ADD       1             CO                3 0
533600931207     C                   Z-ADD     WR5COV        COV(CO)
533700931207     C                   ENDIF                                                  <-- 2 ---
533800931207     C                   ENDIF                                                  <-- 1 ---
533900931207     C*
534000931207     C                   ENDSR
534100931201     C********************************************************************
534200060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
534300931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
534400931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
534500931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
534600931201     C********************************************************************
534700931201     C     RO6CDT        BEGSR
534800931201     C*
534900931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
535000931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
535100931201     C*
535200931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
535300931201     C                   Z-ADD     WR6POI        WR6POO            9 0
535400931201     C*
535500931201     C* CICLO FINO A CHE:
535600931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
535700931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
535800931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
535900931201     C     WR6POO        ANDLT     WPOLIM
536000931201     C*
536100931201     C                   Z-ADD     WR6POO        WPOROU            9 0
536200931201     C                   MOVEL     'N'           WSLITT            1
536300931201     C*
536400931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
536500931201     C*
536600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
536700931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
536800931201     C                   END
536900931201     C*
537000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
537100931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
537200931201     C                   END
537300931201     C*
537400931201     C     WSLITT        IFEQ      'N'
537500931201     C                   SUB       12            WR6NOI            5 0
537600931201     C                   END
537700931201     C                   ADD       12            WR6POO            9 0
537800931201     C*
537900931223     C                   ENDDO                                                  <-- 1 ---
538000931201     C*
538100931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
538200931202     C     WR6POO        SUB       12            WR7POI
538300931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
538400931207     C                   ADD       12            WR7POO
538500931202     C     WR7POO        IFGT      WR6POO
538600931201     C                   Z-ADD     WR7POO        WR6POO
538700931202     C                   END
538800931201     C*
538900931201     C                   ENDSR
539000931201     C********************************************************************
539100060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
539200060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
539300060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
539400060407     C*            INPUT:  -WR7POI (PROGR.ORE)
539500931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
539600931201     C********************************************************************
539700931201     C     RO7TCF        BEGSR
539800060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
539900060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
540000060407     C                   MOVEL     'N'           WSLITT
540100931201     C*
540200931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
540300931201     C                   Z-ADD     WR7POI        WR7POO            9 0
540400931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
540500931201     C* COINCIDA CON UN TURNO DI CHIUSURA
540600060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
540700931201     C     D98GC2        ANDEQ     *BLANKS
540800060407   x1C                   ELSE                                                   <-- 1 -->
540900931214     C*
541000931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
541100931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
541200931214     C*
541300931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
541400931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
541500931223     C     WPOROU        ANDLT     WPOLIM
541600931214     C* MEMORIZZO INIZIO E FINE RANGE
541700931202     C     WPOTI         IFEQ      *ZEROS
541800931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
541900931202     C                   END
542000931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
542100931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
542200931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
542300931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
542400931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
542500931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
542600931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
542700931201     C                   MOVEL     'N'           WSLITT
542800931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
542900060407     c* Se non trovat, controllo se c'e' evento part che giustifica
543000060407     c                   if        wslitt='N'
543100060407     c                   exsr      datcepg
543200060407     c                   endif
543300060407     c
543400931201     C                   ENDDO                                                  <-- 2 ---
543500931214     C*
543600931201     C                   Z-ADD     WPOROU        WR7POO
543700931214     C*
543800931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
543900931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
544000931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
544100931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
544200931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
544300931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
544400931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
544500931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
544600931209     C                   ELSE                                                   <-- 3 -->
544700931209     C     WIC           IFEQ      'I'
544800931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
544900931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
545000931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
545100931201     C                   ELSE
545200931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
545300931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
545400931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
545500931201     C                   ENDIF
545600931209     C                   ENDIF                                                  <-- 3 ---
545700931209     C                   ENDIF                                                  <-- 2 ---
545800931214     C*
545900060407    1C                   ENDIF                                                  <-- 1 ---
546000060407     c
546100060407     c* Se non trovato turno di chiusura, verifico se c'e'
546200060407     c*  evento particolare che GIUSTIFICA
546300060407     c                   if        wpoti=0
546400060418     C                   EXSR      RO7CEPG
546500060407     c                   endif
546600931201     C*
546700931201     C                   ENDSR
546800060406     C********************************************************************
546900060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
547000060406     c*                        che giustifica
547100060407     C*            INPUT:-WR7POI (PROGR.ORE)
547200060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
547300060406     C********************************************************************
547400060407     C     RO7CEPG       BEGSR
547500060406     C*
547600060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
547700060407     C                   Z-ADD     WR7POI        WR7POO            9 0
547800060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
547900060406     C* coincide
548000060406    1C                   if        wevpag='S'                                   <-- 1 -->
548100060406     C*
548200060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
548300060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
548400060406     C                   MOVEL     'N'           WSLITT
548500060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
548600060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
548700060406     C*
548800060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
548900060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
549000060406     C     WPOROU        ANDLT     WPOLIM
549100060406     C* MEMORIZZO INIZIO E FINE RANGE
549200060406    3C     WPOTI         IFEQ      *ZEROS
549300060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
549400060406    3C                   END
549500060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
549600060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
549700060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
549800060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
549900060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
550000060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
550100060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
550200060406     C                   MOVEL     'N'           WSLITT
550300060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
550400060406     C* Se non trovato,
550500060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
550600060406    3c                   if        wslitt='N'
550700060406     c                   exsr      DATTUC
550800060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
550900060406    2C                   ENDDO                                                  <-- 2 ---
551000060406     C*
551100060407     C                   Z-ADD     WPOROU        WR7POO
551200060406     C*
551300060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
551400060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
551500060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
551600060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
551700060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
551800060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
551900060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
552000060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
552100060406   x3C                   ELSE                                                   <-- 3 -->
552200060406    4C     WIC           IFEQ      'I'
552300060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
552400060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
552500060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
552600060406   x4C                   ELSE
552700060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
552800060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
552900060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
553000060406    4C                   ENDIF
553100060406    3C                   ENDIF                                                  <-- 3 ---
553200060406    2C                   ENDIF                                                  <-- 2 ---
553300060406     C*
553400060406    1C                   ENDIF                                                  <-- 1 ---
553500060406     C*
553600060406     C                   ENDSR
553700931207     C********************************************************************
553800931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
553900931207     C*            INPUT:  -WR8POI (PROGR.ORE)
554000931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
554100931207     C********************************************************************
554200931207     C     RO8FES        BEGSR
554300130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
554400130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
554500931207     C*
554600931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
554700130411     C     WR8POI        SUB       12            WR8POO            9 0
554800931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
554900931207     C                   MOVEL     'N'           WSLITT
555000130411     c
555100130411     c* Non salto i festivi ???
555200130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
555300130411     c
555400931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
555500130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
555600130411     C     WPOROU        ANDGT     WPODSP
555700130411     C                   SUB       12            WPOROU
555800130411     C                   MOVEL     'N'           WSLITT
555900130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
556000130411     C                   ENDDO                                                  <-- 2 ---
556100130411     c
556200130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
556300130411     C     D98GC2        ANDEQ     *BLANKS
556400130411   x1C                   ELSE                                                   <-- 1 -->
556500130411     c* eseguo controllo per i turni di chiusura
556600130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
556700130411     C*
556800130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
556900130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
557000130411     C     WPOROU        ANDgT     WPOdsp
557100130411     C* MEMORIZZO INIZIO E FINE RANGE
557200130411     C     WPOTf         IFEQ      *ZEROS
557300130411     C                   z-add     wporou        WPOTf
557400130411     C                   END
557500140826     C     WPOROU        SUB       wnoadd        WPOROU
557600130411     c
557700130411     C                   MOVEL     'N'           WSLITT
557800130411     c                   exsr      dattuc
557900130411     c                   if        wslitt='N'
558000130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
558100140826     c* Se slitta per festivo --> tolgo sempre 12 ore
558200140826     c                   z-add     12            WNOADD
558300130411     c                   endif
558400130411     C* SLITTA
558500140826     C     wporou        add       12            WPOTi
558600130411    2C                   ENDDO                                                  <-- 2 ---
558700130411     c
558800130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
558900130411     c
559000130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
559100130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
559200130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
559300130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
559400130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
559500130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
559600130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
559700130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
559800130411   x3C                   ELSE                                                   <-- 3 -->
559900130411    4C     WIC           IFEQ      'I'
560000130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
560100130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
560200130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
560300130411   x4C                   ELSE
560400130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
560500130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
560600130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
560700130411    4C                   ENDIF
560800130411    3C                   ENDIF                                                  <-- 3 ---
560900130411    2C                   ENDIF                                                  <-- 3 ---
561000130411     C*
561100130411    1C                   ENDIF                                                  <-- 1 ---
561200130411
561300130411     C**   WPOROU        ADD       12            WR8POO
561400130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
561500130411     c                   z-add     wr8poi        wr8poo
561600931207     C*
561700931207     C                   ENDSR
561800931210     C********************************************************************
561900931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
562000940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
562100931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
562200931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
562300931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
562400931210     C********************************************************************
562500931210     C     RO9COV        BEGSR
562600931210     C*
562700931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
562800931210     C                   Z-ADD     *ZEROS        WR9NOV
562900931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
563000931210     C                   Z-ADD     WR9POI        WR9POW
563100931210     C* CICLO FINO A CHE:
563200931210     C*   - DATA INIZIO SUPERATO DATA FINE
563300931223     C*   - RAGGIUNTO LIMITE MASSIMO
563400931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
563500931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
563600931223     C     WR9POW        ANDLT     WPOLIM
563700931210     C*
563800931210     C                   Z-ADD     WR9POW        WPOROU
563900931210     C                   MOVEL     'N'           WSLITT            1
564000940324     C* FESTIVI
564100931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
564200940324     C* GIACENZA
564300931214     C     WSLITT        IFEQ      'N'
564400941229£$   C     WPOAGC        IFNE      *ZEROS
564500940324£$   C     WPOCGC        ORNE      *ZEROS
564600941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
564700940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
564800940324£$   C                   MOVEL     'S'           WSLITT
564900940324£$   C                   END
565000940324£$   C                   END
565100940324£$   C                   END
565200960129     C* EVENTI GIACENZA
565300960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
565400960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
565500960129     C                   Z-ADD     *ZEROS        G                 3 0
565600960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
565700960129     C     G             ANDLT     IG
565800960129     C                   ADD       1             G
565900960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
566000960209     C     WPOROU        ANDLE     GCF(G)
566100960129     C                   MOVEL     'S'           WSLITT
566200960129     C                   ENDIF                                                  <-- 3 ---
566300960129     C                   ENDDO                                                  <-- 2 ---
566400960129     C                   ENDIF                                                  <-- 1 ---
566500960129     C                   ENDIF                                                  <-- 0 ---
566600940324     C* LASCIATO AVVISO
566700931214     C     WSLITT        IFEQ      'N'
566800941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
566900940324£$   C                   Z-ADD     *ZEROS        L                 3 0
567000940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
567100940324£$   C     L             ANDLT     IL
567200940324£$   C                   ADD       1             L
567300941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
567400941229£$   C     WPOROU        ANDLE     LCF(L)
567500050520     c     LCon(L)       andeq     'S'
567600940324£$   C                   MOVEL     'S'           WSLITT
567700940324£$   C                   END                                                    <-- 3 ---
567800940324£$   C                   ENDDO                                                  <-- 2 ---
567900940324£$   C                   END                                                    <-- 1 ---
568000931210     C                   END
568100940324     C* TURNO CHIUSURA
568200140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
568300140826     c*  di sistemare le ore da sommare 12 o 24...
568400931214     C     WSLITT        IFEQ      'N'
568500931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
568600931210     C                   END
568700940324     C* DATA CONSEGNA RICHIESTA
568800050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
568900050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
569000050907     c
569100931214     C     WSLITT        IFEQ      'N'
569200050907     c                   add       1             wporou
569300940324£$   C     WPOCRI        IFNE      *ZEROS
569400940324£$   C     WPOROU        IFLT      WPOCRC
569500940324£$   C                   MOVEL     'S'           WSLITT
569600940324£$   C                   END
569700940324£4   C                   END
569800050907     c                   sub       1             wporou
569900931210     C                   END
570000931210     C*
570100931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
570200931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
570300931210     C     WSLITT        IFEQ      'N'
570400931210     C                   ADD       12            WR9NOV            5 0
570500931210     C                   END
570600931210     C                   ADD       12            WR9POW            9 0
570700931210     C*
570800931223     C                   ENDDO                                                  <-- 1 ---
570900931210     C*
571000931210     C                   ENDSR
571100940628%%%  C********************************************************************
571200940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
571300940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
571400940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
571500940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
571600940628%%%  C********************************************************************
571700940628%%%  C     RO0GES        BEGSR
571800940628%%%  C*
571900940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
572000940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
572100940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
572200940628%%%  C*
572300940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
572400940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
572500940628%%%  C*
572600940628%%%  C* CICLO FINO A CHE:
572700940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
572800940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
572900940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
573000050922%%%  C     WR0POO        ANDGe     WPODSP
573100940628%%%  C*
573200940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
573300940628%%%  C                   MOVEL     'N'           WSLITT            1
573400940628%%%  C*
573500940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
573600940628%%%  C*
573700940628%%%  C     WSLITT        IFEQ      'N'
573800940628%%%  C                   SUB       12            WR0NOI            5 0
573900940628%%%  C                   END
574000940628%%%  C                   SUB       12            WR0POO            9 0
574100940628%%%  C*
574200940628%%%  C                   ENDDO                                                  <-- 1 ---
574300940628%%%  C*
574400940628%%%  C                   ADD       12            WR0POO            9 0
574500940628%%%  C*
574600940628%%%  C                   ENDSR
574700960911     C********************************************************************
574800960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
574900960911     C*            INPUT:  -W10POI (PROGR.ORE)
575000960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
575100960911     C********************************************************************
575200960911     C     R10FES        BEGSR
575300960911     C*
575400960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
575500960911     C                   Z-ADD     W10POI        W10POO            9 0
575600960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
575700960911     C                   MOVEL     'N'           WSLITT
575800960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
575900960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
576000960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
576100960911     C                   ADD       12            WPOROU
576200960911     C                   MOVEL     'N'           WSLITT
576300960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
576400960911     C                   ENDDO                                                  <-- 2 ---
576500960911     C                   Z-ADD     WPOROU        W10POO
576600960911     C*
576700960911     C                   ENDSR
576800930325     C********************************************************************
576900930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
577000930325     C********************************************************************
577100930325     C     CHKGIA        BEGSR
577200930406     C*
577300960129     C* APERTURA/CHIUSURA GIACENZA
577400930326     C     WPODAG        IFNE      *ZEROS
577500930406     C     WPODCG        ORNE      *ZEROS
577600930407     C     WPOROU        IFGE      WPODAG
577700930407     C     WPOROU        ANDLE     WPODCG
577800930325     C                   MOVEL     'S'           WSLITT
577900930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
578000930325     C                   END
578100930326     C                   END
578200960129     C*
578300960129     C* EVENTI GIACENZA
578400960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
578500960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
578600960129     C                   Z-ADD     *ZEROS        G                 3 0
578700960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
578800960129     C     G             ANDLT     IG
578900960129     C                   ADD       1             G
579000960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
579100960129     C     WPOROU        ANDLE     GIF(G)
579200091009     c
579300091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
579400091009     c*  l'orario dell'evento
579500091009     c                   select
579600091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
579700091009     c* Se orario entro le 10.30  va bene
579800091014     c*  solo la prima volta per evento cioè quando controllo
579900091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
580000091014     c*  e non devo dare errore
580100091014     C                   MOVEL     'OSC  '       WR3TAB
580200091014     C                   eval      WR3HHI=evo(gip(G))
580300091014     C                   EXSR      RO3ORE
580400091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
580500091014     c*  in ritardo come per "E")
580600091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
580700091014     c                              wr3matpom='P'
580800091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
580900091009     C                   MOVEL     'S'           WSLITT
581000091009     c                   endif
581100091014     c                   eval      SalvaEveG=gip(G)
581200091009     c
581300091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
581400091009     c* Se orario entro le 10.30  va bene
581500091014     c*  solo la prima volta per evento cioè quando controllo
581600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
581700091014     c*  e non devo dare errore
581800091014     C                   MOVEL     'OSC  '       WR3TAB
581900091014     C                   eval      WR3HHI=evo(gip(G))
582000091014     C                   EXSR      RO3ORE
582100091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
582200091014     c*  in ritardo come per "E")
582300091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
582400091014     c                              wr3matpom='P'
582500091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
582600091009     C                   MOVEL     'S'           WSLITT
582700091009     c                   endif
582800091014     c                   eval      SalvaEveG=gip(G)
582900091009     c
583000091009     c                   other
583100960129     C                   MOVEL     'S'           WSLITT
583200960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
583300091009     c                   endsl
583400091009
583500960129     C                   ENDIF                                                  <-- 3 ---
583600960129     C                   ENDDO                                                  <-- 2 ---
583700960129     C                   ENDIF                                                  <-- 1 ---
583800960129     C                   ENDIF                                                  <-- 0 ---
583900930406     C*
584000930325     C                   ENDSR
584100060420     C********************************************************************
584200930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
584300930325     C********************************************************************
584400930325     C     CHKLAV        BEGSR
584500930325     C*
584600941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
584700930421     C                   Z-ADD     *ZEROS        L                 3 0
584800930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
584900930421     C     L             ANDLT     IL
585000930421     C                   ADD       1             L
585100930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
585200930421     C     WPOROU        ANDLE     LAF(L)
585300090928     c
585400090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
585500091009     c*  l'orario dell'evento
585600091009     c                   select
585700091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
585800091009     c* Se orario entro le 10.30  va bene
585900091014     c*  solo la prima volta per evento cioè quando controllo
586000091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
586100091014     c*  e non devo dare errore
586200091014     C                   MOVEL     'OSC  '       WR3TAB
586300091014     C                   eval      WR3HHI=evo(lap(L))
586400091014     C                   EXSR      RO3ORE
586500091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
586600091014     c*  in ritardo come per "E")
586700091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
586800091014     c                              wr3matpom='P'
586900091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
587000091009     C                   MOVEL     'S'           WSLITT
587100091009     c                   endif
587200091014     c                   eval      SalvaEveL=lap(L)
587300091009     c
587400091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
587500091009     c* Se orario entro le 10.30  va bene
587600091014     c*  solo la prima volta per evento cioè quando controllo
587700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
587800091014     c*  e non devo dare errore
587900091014     C                   MOVEL     'OSC  '       WR3TAB
588000091014     C                   eval      WR3HHI=evo(lap(L))
588100091014     C                   EXSR      RO3ORE
588200091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
588300091014     c*  in ritardo come per "E")
588400091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
588500091014     c                              wr3matpom='P'
588600091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
588700091009     C                   MOVEL     'S'           WSLITT
588800091009     c                   endif
588900091014     c                   eval      SalvaEveL=lap(L)
589000090928     c
589100091009     c                   other
589200930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
589300091009     C                   MOVEL     'S'           WSLITT
589400091009     c                   endsl
589500090928     c
589600930421     C                   END                                                    <-- 3 ---
589700931223     C                   ENDDO                                                  <-- 2 ---
589800930421     C                   END                                                    <-- 1 ---
589900930325     C*
590000930325     C                   ENDSR
590100060420     C********************************************************************
590200060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
590300060420     c*                   che Allungano
590400060420     C********************************************************************
590500060420     C     CHKCEPA       BEGSR
590600060420     C*
590700060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
590800060420     C                   Z-ADD     *ZEROS        p                 3 0
590900060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
591000060420     C     p             ANDLT     IpA
591100060420     C                   ADD       1             P
591200060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
591300060420     C     WPOROU        ANDLE     paif(P)
591400060420     C                   MOVEL     'S'           WSLITT
591500060420     C                   ADD       12            W98EA
591600060420     C                   END                                                    <-- 3 ---
591700060420     C                   ENDDO                                                  <-- 2 ---
591800060420     C                   END                                                    <-- 1 ---
591900060420     C*
592000060420     C                   ENDSR
592100930414     C********************************************************************
592200930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
592300930414     C********************************************************************
592400930414     C     CHKCNT        BEGSR
592500930511     C*
592600930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
592700930611     C     WIC           IFEQ      'I'                                          *INTERNO
592800930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
592900930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
593000930611     C                   ELSE
593100930611     C     WPOROU        IFGE      WPOAII
593200930611     C     WPOROU        ANDLE     WPOAFI
593300930611     C                   MOVEL     'S'           WSLITT
593400930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
593500930611     C                   END
593600930611     C                   END
593700930611     C                   ELSE                                                   *CLIENTE
593800930611     C     WPOAIC        IFEQ      *ZEROS
593900930611     C     WPOAFC        ANDEQ     *ZEROS
594000930611     C                   ELSE
594100930611     C     WPOROU        IFGE      WPOAIC
594200930611     C     WPOROU        ANDLE     WPOAFC
594300930611     C                   MOVEL     'S'           WSLITT
594400930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
594500930611     C                   END
594600930611     C                   END
594700930611     C                   END
594800930414     C*
594900930414     C                   ENDSR
595000930511     C********************************************************************
595100930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
595200930511     C********************************************************************
595300930511     C     CHKFED        BEGSR
595400930511     C*
595500930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
595600930611     C     WIC           IFEQ      'I'                                          *INTERNO
595700930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
595800930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
595900930611     C                   ELSE
596000930611     C     WPOROU        IFGE      WPOFII
596100930611     C     WPOROU        ANDLE     WPOFFI
596200930611     C                   MOVEL     'S'           WSLITT
596300930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
596400930611     C                   END
596500930611     C                   END
596600930611     C                   ELSE                                                   *CLIENTE
596700930611     C     WPOFIC        IFEQ      *ZEROS
596800930611     C     WPOFFC        ANDEQ     *ZEROS
596900930611     C                   ELSE
597000930611     C     WPOROU        IFGE      WPOFIC
597100930611     C     WPOROU        ANDLE     WPOFFC
597200930611     C                   MOVEL     'S'           WSLITT
597300930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
597400930611     C                   END
597500930611     C                   END
597600930611     C                   END
597700930511     C*
597800930511     C                   ENDSR
597900930325     C********************************************************************
598000930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
598100930325     C********************************************************************
598200930325     C     CHKFES        BEGSR
598300060526     c* se già passata la data, non controllo wporou
598400060526     c                   if        wpodata>0
598500060526     c                   z-add     wpodata       w12dat
598600060526     c                   clear                   w12res
598700060526     c                   else
598800931130     C*
598900931130     C* TRASFORMA IL  PROGR.ORE IN DATA
599000931130     C                   Z-ADD     WPOROU        WORE
599100931130     C                   EXSR      OREDAT
599200941227     C                   Z-ADD     WDAT          W12DAT            8 0
599300931130     C                   Z-ADD     WRES          W12RES            3 0
599400060526     c                   endif
599500930325     C*
599600941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
599700930325     C* E' FESTIVO
599800090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
599900090108     C**                 Z-ADD     D98LNP        CLNTFP
600000090108     C**                 Z-ADD     *ZEROS        CLNTFA
600100090108     C**                 ENDIF
600200090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
600300090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
600400090108     c**   è possibile rendere festivo in terminal per un altro terminal
600500090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
600600930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
600700090108     C     WTC           oreq      'P'                                          *TRAZIONE
600800941227     C                   Z-ADD     D98TFP        CLNTFP
600900941227     C                   Z-ADD     D98TFA        CLNTFA
601000941228     C                   ENDIF
601100941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
601200941227     C                   Z-ADD     *ZEROS        CLNTFP
601300941227     C                   Z-ADD     D98LNA        CLNTFA
601400941228     C                   ENDIF
601500941227     C                   Z-ADD     W12DAT        DSDAT8
601600941227     C                   Z-ADD     DSAAS         CLNANN
601700941227     C                   Z-ADD     DSMES         CLNMES
601800930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
601900930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
602000930421     C                   Z-ADD     DSGIO         G
602100930415     C     W12RES        IFEQ      *ZEROS
602200941227     C                   MOVEL     CLNMAT        GFEDS
602300930415     C                   ELSE
602400941227     C                   MOVEL     CLNPOM        GFEDS
602500930415     C                   END
602600930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
602700930405     C                   MOVEL     *BLANKS       WC1Y
602800930421     C                   MOVEL     GFE(G)        WC1Y              8
602900930421     C                   Z-ADD     1             I
603000930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
603100930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
603200930405     C     WIC           IFEQ      'I'
603300930421     C     I1Y(I)        ANDEQ     'S'
603400930405     C                   MOVEL     'S'           WSLITT
603500930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
603600930405     C                   END
603700930405     C     WIC           IFEQ      'C'
603800930421     C     E1Y(I)        ANDEQ     'S'
603900930405     C                   MOVEL     'S'           WSLITT
604000930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
604100930405     C                   END
604200970204     C     WIC           IFEQ      'X'
604300970204     C     E1Y(I)        ANDEQ     'S'
604400970204     C                   MOVEL     'S'           WSLITT
604500970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
604600970204     C                   END
604700930405     C                   ELSE                                                   <-- 3 -->
604800930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
604900930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
605000930405     C                   END                                                    <-- 3 ---
605100930405     C                   END                                                    <-- 2 ---
605200930405     C                   END                                                    <-- 1 ---
605300930325     C*
605400060526     c                   clear                   wpodata
605500930325     C                   ENDSR
605600931201     C********************************************************************
605700931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
605800931201     C********************************************************************
605900931201     C     CHKTUC        BEGSR
606000931201     C*
606100931201     C     WIC           IFEQ      'I'                                          --- 1 -->
606200931201     C                   DO        TI            T                 3 0          --- 2 -->
606300931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
606400931201     C     WPOROU        ANDLE     TIF(T)
606500931201     C                   MOVEL     'S'           WSLITT
606600931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
606700931201     C                   LEAVE                                                  *VA A FINE DO
606800931201     C                   ENDIF                                                  <-- 3 ---
606900931201     C                   ENDDO                                                  <-- 2 ---
607000931201     C                   ELSE                                                   <-- 1 -->
607100931201     C                   DO        TC            T                 3 0          --- 2 -->
607200931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
607300931201     C     WPOROU        ANDLE     TCF(T)
607400931201     C                   MOVEL     'S'           WSLITT
607500931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
607600931201     C                   LEAVE                                                  *VA A FINE DO
607700931201     C                   ENDIF                                                  <-- 3 ---
607800931201     C                   ENDDO                                                  <-- 2 ---
607900931201     C                   ENDIF                                                  <-- 1 ---
608000931201     C*
608100931201     C                   ENDSR
608200060418     C********************************************************************
608300060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
608400060418     c*          - eventi particolari che giustificano
608500060418     C********************************************************************
608600060418     C     CHKCEPG       BEGSR
608700060418     C*
608800060418     C     WIC           IFEQ      'I'                                          --- 1 -->
608900060420     C                   DO        PGI           p                 3 0          --- 2 -->
609000060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
609100060420     C     WPOROU        ANDLE     PGIF(p)
609200060418     C                   MOVEL     'S'           WSLITT
609300060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
609400060418     C                   LEAVE                                                  *VA A FINE DO
609500060418     C                   ENDIF                                                  <-- 3 ---
609600060418     C                   ENDDO                                                  <-- 2 ---
609700060418     C                   ELSE                                                   <-- 1 -->
609800060420     C                   DO        PGC           p                 3 0          --- 2 -->
609900060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
610000060420     C     WPOROU        ANDLE     PGCF(p)
610100060418     C                   MOVEL     'S'           WSLITT
610200060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
610300060418     C                   LEAVE                                                  *VA A FINE DO
610400060418     C                   ENDIF                                                  <-- 3 ---
610500060418     C                   ENDDO                                                  <-- 2 ---
610600060418     C                   ENDIF                                                  <-- 1 ---
610700060418     C*
610800060418     C                   ENDSR
610900930512     C********************************************************************
611000931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
611100930512     C********************************************************************
611200931130     C     DATTUC        BEGSR
611300931214     C*
611400931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
611500931214     C                   Z-ADD     12            WNOADD            9 0
611600930325     C*
611700931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
611800931125     C     D98GC2        ORNE      *BLANKS
611900931130     C*
612000931213     C                   Z-ADD     WPOROU        WORE
612100931130     C                   EXSR      OREDAT                                       *ORE->DATA
612200931130     C                   Z-ADD     WDAT          W12DAT
612300931130     C                   Z-ADD     WRES          W12RES
612400930405     C*
612500941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
612600930405     C* DELLA SETTIMANA
612700941227     C                   Z-ADD     W12DAT        DSDAT8
612800941227     C                   Z-ADD     DSAAS         CLNANN
612900941227     C                   Z-ADD     DSMES         CLNMES
613000941227     C                   Z-ADD     *ZEROS        CLNTFP
613100941227     C                   Z-ADD     *ZEROS        CLNTFA
613200930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
613300930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
613400941227     C                   MOVEL     CLNMAT        GSEDS
613500960129     C                   Z-ADD     DSGIO         GI                6 0
613600960129     C                   MOVEL     GSE(GI)       WGSE              1
613700140911     c
613800140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
613900140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
614000140911     c                   if        wtuctut='S'
614100140911     c                   eval      wtuctut='P'
614200140911     c                   endif
614300930326     C*
614400931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
614500931213     C                   DO        4             IT                3 0          --- 3 -->
614600931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
614700931213     C                   MOVEL     TUC(IT)       W93GSE            1
614800931213     C                   MOVE      TUC(IT)       W93GMP            1
614900930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
615000930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
615100931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
615200930512     C     W93GSE        OREQ      ' '
615300140911     c     WTUCTUT       andeq     ' '
615400140911     c
615500930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
615600931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
615700930401     C     W93GMP        OREQ      ' '
615800930325     C                   MOVEL     'S'           WSLITT
615900931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
616000931213     C                   ELSE                                                   <-- 6 -->
616100001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
616200001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
616300001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
616400050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
616500050908     C     Usacons       IFNE      'S'                                          --- 7 -->
616600141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
616700141107     c*    orari giorno per giorno
616800141107     c                   if        OOR2FCSGL<>' '
616900141108     c                             and w12dat<>sav_12dat
617000141107     c                   eval      or_datarif=w12dat
617100141107     c                   eval      or_DDC    =w12dat
617200141107     c                   exsr      Reper_tempi
617300141108     c                   eval      sav_12dat=w12dat
617400141107     c                   endif
617500141107     c
617600930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
617700930325     C     W93GMP        IFEQ      'M'
617800930326     C     W12RES        ANDEQ     *ZEROS
617900141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
618000141111     c                             or OOR2STfS =0
618100141105     c                             or (tuc(IT)<>gc1part and
618200141105     c                              tuc(IT)<>gc2part)
618300140825     C                   MOVEL     'S'           WSLITT
618400140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
618500140911     c* slitto solo una volta  se chiuso tutti " M"
618600140911     c                   if        W93GSE=' '
618700140911     c                   eval      wTUCTUT='S'
618800140911     c                   endif
618900140911     c
619000140825     c                   endif
619100930325     C                   END
619200930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
619300930325     C     W93GMP        IFEQ      'P'
619400930326     C     W12RES        ANDNE     *ZEROS
619500140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
619600140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
619700141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
619800141111     c                             or OOR2STiS =0
619900141105     c                             or (tuc(IT)<>gc1part and
620000141105     c                              tuc(IT)<>gc2part)
620100930325     C                   MOVEL     'S'           WSLITT
620200140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
620300140911     c* slitto solo una volta  se chiuso tutti " P"
620400140911     c                   if        W93GSE=' '
620500140911     c                   eval      wTUCTUT='S'
620600140911     c                   endif
620700140825     c                   endif
620800140825     c
620900931213     C                   ENDIF
621000001128     C                   ENDIF                                                  <-- 7 ---
621100931213     C                   ENDIF                                                  <-- 6 ---
621200931213     C                   ENDIF                                                  <-- 5 ---
621300931213     C                   ENDIF                                                  <-- 4 ---
621400931213     C                   ENDDO                                                  <-- 3 ---
621500930405     C*
621600931213     C                   ENDIF                                                  <-- 2 ---
621700931213     C                   ENDIF                                                  <-- 1 ---
621800140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
621900140911     c                   if        wtuctut='P'  and wslitt='N'
622000140911     c                   clear                   wtuctut
622100140911     c                   endif
622200140911     c
622300930325     C                   ENDSR
622400060406     C********************************************************************
622500060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
622600060406     C********************************************************************
622700060406     C     DATCEPG       BEGSR
622800060406     C*
622900060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
623000060406     C                   Z-ADD     12            WNOADD            9 0
623100060406     C*
623200060406    1C                   if        wevpag='S'
623300060406     C*
623400060406     C                   Z-ADD     WPOROU        WORE
623500060406     C                   EXSR      OREDAT                                       *ORE->DATA
623600060406     C                   Z-ADD     WDAT          W12DAT
623700060406     C                   Z-ADD     WRES          W12RES
623800060418     c                   z-add     1             P
623900060406     c* Verifico se nella data esiste un evento Part che giustifica
624000060418     c     w12dat        lookup    pvd(P)                                 30
624100060418    2c                   dow       *in30 and pvf(P)<>'G'
624200060418     c                   add       1             P
624300060420     c     w12dat        lookup    pvd(P)                                 30
624400060406    2c                   enddo
624500060406     c
624600060406     c* 30 on --> TROVATO !!
624700060406    2c                   if        *in30
624800060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
624900060406     C                   MOVEL     'S'           WSLITT
625000060406     c                   z-add     24            wnoadd
625100060406    3C                   ENDIF
625200060406     C*
625300060406    2C                   ENDIF                                                  <-- 2 ---
625400060406    1C                   ENDIF                                                  <-- 1 ---
625500060406     C*
625600060406     C                   ENDSR
625700931210     C********************************************************************
625800931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
625900931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
626000931210     C********************************************************************
626100050805     C***  CHKDCR        BEGSR
626200931210     C*
626300050805     C***  WPOCRI        IFNE      *ZEROS
626400050805     C***  WPOROU        IFGE      WPOCRI
626500050805     C***  WPOROU        ANDLT     WPOCRF
626600050805     C***                ELSE
626700050805     C***                MOVEL     'S'           WSLITT
626800050805     C***                END
626900050805     C***                END
627000931210     C*
627100050805     C***                ENDSR
627200140822     C********************************************************************
627300140822     C*  Reperimento tempi di consegna per decidere se slittare
627400140822     C********************************************************************
627500140822     C     Reper_tempi   BEGSR
627600140822
627700140822     c                   clear                   trulorSds
627800140825     c                   clear                   trulor2ds
627900140822     c                   eval       IOREtfp = d98tfp
628000140822     c                   eval       IOREtfa = d98tfa
628100140825     c                   eval       IOREdti = d98dti
628200140825     c                   eval       IOREhti = d98hti
628300140825     c                   eval       IOREdcr = s98dcr
628400140825     c                   eval       IOREhcr = s98hcr
628500140825     c                   eval       IOREtcr = s98tcr
628600140825     c                   eval       IOREDEI = d98dei
628700140825     c                   eval       IOREOEI = d98oei
628800140825     c                   eval       IOREfil = d98lna
628900140825     c                   eval       IOREcap = d98cad
629000140825     c                   eval       IOREloc = d98lod
629100140825     c                   eval       IOREnar = d98nzd
629200140825     c
629300140825     c* Data di Riferimento:
629400140825     c* Se impostata la consegnabilità --> prendo questa
629500140822     c                   select
629600140825     c                   when      d98dfc>0
629700141107     c                   if        or_datarif<d98dfc and or_datarif>0
629800141107     c                   eval      ioredta=or_datarif
629900141107     c                   else
630000141107     c                   eval      ioredta=d98dfc
630100141107     c                   endif
630200140825     c* altrimenti data di consegna
630300140825     c                   when      d98dcm>0
630400141107     c                   if        or_datarif<d98dcm and or_datarif>0
630500141107     c                   eval      ioredta=or_datarif
630600141107     c                   else
630700140825     c                   eval      ioredta=d98dcm
630800141107     c                   endif
630900140825     c
631000140822     c                   other
631100140825     c* altrimenti data teorica consegna se > = oggi
631200140825     C                   time                    wrkoggiiso
631300140825     C     *iso          move      wrkoggiiso    ioredta
631400140822     c* se < di oggi, imposto oggi
631500140825     c                   if        d98dei>ioredta
631600140825     c                   eval      ioredta=d98dei
631700140822     c                   endif
631800141107     c
631900141107     c                   if        or_datarif<ioredta and or_datarif>0
632000141107     c                   eval      ioredta=or_datarif
632100141107     c                   endif
632200141107     c
632300140822     c                   endsl
632400140822     c
632500140825     c                   eval       IOREtser = 'C'
632600141107     c                   if         or_ddc>0 and or_ddc<d98ddc
632700141107     c                   eval       IOREDDC=or_ddc
632800141107     c                   eval       IOREnDC=8888888
632900141107     c                   eval       IOREcons = 'S'
633000141107     c                   else
633100140825     c                   eval       IOREDDC=d98ddc
633200140825     c                   eval       IOREnDC=wd98ndc
633300140825     c                   eval       IOREcons = d98cons
633400141107     c                   endif
633500141017     c                   eval       IOREtsp = d98tsp
633600140822     c
633700140822     c                   call      'TRULORSR'
633800140822     c                   parm                    kpjba
633900140822     c                   parm                    trulorSds
634000140825     c                   parm                    trulor2ds
634100140825     c
634200140825     c                   eval      wtrulor= 'S'
634300141107     c                   clear                   or_datarif        8 0
634400141107     c                   clear                   or_ddc            8 0
634500140822     C                   ENDSR
634600950406     C********************************************************************
634700950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
634800950406     C********************************************************************
634900950406     C     DATORE        BEGSR
635000950406     C*
635100950406     C                   Z-ADD     *ZEROS        WORE
635200950406     C     WDAT          IFNE      0
635300950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
635400950406     C     WAA           DIV       4             WQUA              4 0
635500950406     C                   MVR                     WRES
635600950406     C     WAA           MULT      365           WGIO              9 0
635700950406     C                   ADD       WQUA          WGIO
635800950406     C                   ADD       WGG           WGIO
635900950406     C     WMM           SUB       1             MC                3 0
636000950406     C                   DO        MC            WX                3 0
636100950406     C                   ADD       MES(WX)       WGIO
636200950406     C                   END
636300950406     C     WRES          IFEQ      0
636400950406     C     WMM           ANDLE     2
636500950406     C                   SUB       1             WGIO
636600950406     C                   END
636700950406     C     WGIO          MULT      24            WORE              9 0
636800950406     C                   ENDIF
636900950406     C*
637000950406     C                   ENDSR
637100930323     C********************************************************************
637200930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
637300930323     C********************************************************************
637400930323     C     OREDAT        BEGSR
637500930323     C*
637600950407     C                   SETOFF                                       717273
637700950407     C                   SETOFF                                       7475
637800930323     C     WORE          DIV       24            WGIO
637900930323     C                   MVR                     WRES              3 0
638000950406     C*
638100950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
638200950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
638300950406     C*  TROVO ANNO
638400950406     C     WGIO          DIV       365           WAA
638500950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
638600950406     C                   MVR                     WREST                71
638700950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
638800950406     C     WGIO          DIV       365           WAA
638900950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
639000950406     C*                                                    0 = 31/12/PR
639100950406     C   75
639200950406     CAN 71              DO                                                     BIS. E NON ERA
639300950406     C     WAA           SUB       1             WAA
639400950406     C                   Z-ADD     365           WREST
639500950406     C                   END
639600950406     C*
639700950406     C     WAA           DIV       4             WNBIS                                      T.
639800950406     C                   MVR                     WRBIS             3 073
639900950406     C   73
640000950406     CANN71                                                                     NON PIU' BISEST
640100950406     CORN73              ADD       1             WREST                          E'BISESTILE
640200950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
640300950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
640400950406     C  N73              SUB       1             $PM(1)
640500950406     C                   Z-ADD     1             WI                2 0
640600950406     C* TROVO IL MESE E GIORNI
640700950406     C     WREST         LOOKUP    $PM(WI)                            74  73
640800950406     C                   MOVE      WI            WMM
640900950406     C     WI            COMP      1                                      72
641000950406     C  N72
641100950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
641200950406     C   74              SUB       1             WI
641300950406     C  N72
641400950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
641500950406     C   72              MOVE      WREST         WGG
641600950406     C                   ADD       1900          WAA
641700950406     C*
641800930323     C*
641900930323     C                   ENDSR
6420009407151507 C********************************************************************
6421009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6422009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6423009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6424009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6425009407151507 C********************************************************************
6426009407151507 C     FINGIO        BEGSR
6427009407151507 C*
6428009407151507 C     WPORE         DIV       24            WGIO
6429009407151507 C                   MVR                     WRES
6430009407151507 C     WRES          IFEQ      *ZEROS
6431009407151507 C                   Z-ADD     12            WNORE             3 0
6432009407151507 C                   ELSE
6433009407151507 C                   Z-ADD     *ZEROS        WNORE
6434009407151507 C                   ENDIF
6435009407151507 C*
6436009407151507 C                   ENDSR
643700930517     C********************************************************************
643800971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
643900930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
644000930517     C********************************************************************
644100930517     C     CERCAL        BEGSR
644200930517     C*
644300930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
644400941227     C                   MOVE      CLNTFP        DCATFP
644500941227     C                   MOVE      CLNTFA        DCATFA
644600941227     C                   MOVE      CLNANN        DCAANN
644700941227     C                   MOVE      CLNMES        DCAMES
644800930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
644900930517     C                   Z-ADD     1             I
645000930517     C     DCALEN        LOOKUP    CAK(I)                                 99
645100930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
645200930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
645300941227     C     KEYCLN        CHAIN     AZCLN01L                           99
645400930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
645500930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
645600930517     C                   MOVEL     '1'           D98ERR
645700930517     C                   MOVEL     MSG(3)        D98MSG
645800930517     C                   ELSE                                                   <-- 2 -->
645900970401     C* CONTROLLA SE ANNULLATO
646000970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
646100970401     C                   MOVEL     '1'           D98ERR
646200970401     C                   MOVEL     MSG(3)        D98MSG
646300970401     C                   ELSE                                                   <-- 3 -->
646400930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
646500930517     C                   ADD       1             WINDCA            6 0
646600970401     C     WINDCA        IFLE      200                                          --- 4 -->
646700970401     C     WINDCA        ANDGT     *ZEROS
646800930517     C                   Z-ADD     WINDCA        I
646900930517     C                   MOVEL     DCALEN        CAK(I)
647000941227     C                   MOVEL     CLNMAT        CAM(I)
647100941227     C                   MOVEL     CLNPOM        CAP(I)
647200970401     C                   END                                                    <-- 4 ---
647300970401     C                   ENDIF                                                  <-- 3 ---
647400930517     C                   END                                                    <-- 2 ---
647500930517     C                   ELSE                                                   <-- 1 -->
647600930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
647700941227     C                   MOVEL     CAM(I)        CLNMAT
647800941227     C                   MOVEL     CAP(I)        CLNPOM
647900930517     C                   END                                                    <-- 1 ---
648000930517     C*
648100930517     C                   ENDSR
648200010821     C********************************************************************
648300010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
648400010821     C********************************************************************
648500010821     C     CHKVCA        BEGSR
648600010821     C*
648700010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
648800010821     C     D98CAS        IFGT      *ZEROS
648900010821     C     D98VCA        ANDEQ     *BLANKS
649000010821     C                   MOVEL     'ITL'         D98VCA
649100010821     C                   ENDIF
649200010822     C*
649300010822     C     D98VCA        IFNE      'ITL'
649400010822     C     D98VCA        ANDNE     'EUR'
649500010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
649600010822     C                   Z-ADD     D98CAS        INPIMP
649700010822     C                   MOVEL     D98VCA        INPDIV
649800010822     C                   EXSR      EXECNV
649900010822     C                   Z-ADD     OUTIMP        D98CAS
650000010822     C                   MOVEL     §GEDCN        D98VCA
650100010822     C                   ENDIF
650200010821     C*
650300010821     C                   ENDSR
650400010821     C********************************************************************
650500010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
650600010821     C********************************************************************
650700010821     C     EXECNV        BEGSR
650800010821     C*
650900010821     C                   CLEAR                   YEURDS
651000010821     C*
651100010821     C                   MOVEL     INPDIV        YECDVI
651200010821     C                   Z-ADD     INPIMP        YECIMI
651300010821     C                   MOVEL     §GEDCN        YECDVO
651400010821     C                   MOVEL     '3'           YECDCO
651500010821     C                   MOVEL     'H'           YECTAR
651600010821     C                   CALL      'YEURCO'
651700010821     C                   PARM                    YEURDS
651800010821     C*
651900010821     C                   Z-ADD     YECIMO        OUTIMP
652000010821     C*
652100010821     C                   ENDSR
652200011211     C*------------------------------------------------------------------------*
652300011211     C* Controllo importo massimo contrassegno.
652400011211     C*------------------------------------------------------------------------*
652500011211     C     REPMAXCSB     BEGSR
652600011211     C*
652700011211     C                   CLEAR                   Trul21DS
652800011211     C*
652900011211     C                   EVAL      I21TSp = d98TSP
653000011211     C                   EVAL      I21NzD = d98NZD
653100011211     C                   EVAL      I21LnP = d98LNP
653200011211     C                   EVAL      I21LnA = d98LNA
653300011211     C                   EVAL      I21Ksc = d98KSC
653400011211     C                   EVAL      I21TiC = d98TIC
653500011211     C                   EVAL      I21Imp = d98CAS
653600011211     C                   EVAL      I21Div = d98VCA
653700011211     C*
653800011211     C                   CALL      'TRUL21R'
653900011211     C                   PARM                    Trul21DS
654000011211     C*
654100011211     C                   ENDSR
654200011211     C*------------------------------------------------------------------------*
654300011211
654400930414     C********************************************************************
654500930414     C*   RUTINZ - ROUTINE INIZIALE
654600930414     C********************************************************************
654700930414     C     RUTINZ        BEGSR
654800930414     C*
654900930414     C* PARAMETRI IN INPUT ALLA ROUTINE
655000930414     C     *ENTRY        PLIST
655100970731     C                   PARM                    DS99
655200941227     C*
655300941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
655400941227     C     KEYBLP        KLIST
655500941227     C                   KFLD                    BLPAAS
655600941227     C                   KFLD                    BLPLNP
655700941227     C                   KFLD                    BLPNRS
655800941227     C                   KFLD                    BLPNSP
655900050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
656000941227     C     KEYGCP        KLIST
656100050405     C                   KFLD                    KWGCPAAS
656200050405     C                   KFLD                    KWGCPLNP
656300050405     C                   KFLD                    KWGCPNRS
656400050405     C                   KFLD                    KWGCPNSP
656500050405     C                   KFLD                    KWGCPFRG
656600050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
656700031125     C     KEYGC_P       KLIST
656800050405     C                   KFLD                    KWGCPAAS
656900050405     C                   KFLD                    KWGCPLNP
657000050405     C                   KFLD                    KWGCPNRS
657100050405     C                   KFLD                    KWGCPNSP
657200970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
657300150120     C**   KEYBLT        KLIST
657400150120     C**                 KFLD                    BLTAAS
657500150120     C**                 KFLD                    BLTLNP
657600150120     C**                 KFLD                    BLTNRS
657700150120     C**                 KFLD                    BLTNSP
657800941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
657900941227     C     KEYARB        KLIST
658000941227     C                   KFLD                    ARBAAS
658100941227     C                   KFLD                    ARBLNP
658200941227     C                   KFLD                    ARBNRS
658300941227     C                   KFLD                    ARBNSP
658400051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
658500941228     C     KEYAR9        KLIST
658600941228     C                   KFLD                    AR9AAS
658700941228     C                   KFLD                    AR9LNP
658800941228     C                   KFLD                    AR9NRS
658900941228     C                   KFLD                    AR9NSP
659000941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
659100050405     C     KEYGCP21      KLIST
659200050405     C                   KFLD                    KWGCPAAS
659300050405     C                   KFLD                    KWGCPLNP
659400050405     C                   KFLD                    KWGCPNRS
659500050405     C                   KFLD                    KWGCPNSP
659600050405     C                   KFLD                    KWGCPFRG
659700970912     C* CHIAVE LETTURA FNART01L - PARZIALE
659800150120     C***  KEYART        KLIST
659900150120     C***                KFLD                    ARTAAS
660000150120     C***                KFLD                    ARTLNP
660100150120     C***                KFLD                    ARTNRS
660200150120     C***                KFLD                    ARTNSP
660300950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
660400950102     C     KEYLBL        KLIST
660500950102     C                   KFLD                    LBLAAN
660600950102     C                   KFLD                    LBLLPN
660700950102     C                   KFLD                    LBLNRN
660800950102     C                   KFLD                    LBLNSN
660900930414     C*
661000070827     c     kAR5          Klist
661100070827     c                   Kfld                    d98Aas
661200070827     c                   Kfld                    d98Lnp
661300070827     c                   Kfld                    d98nrs
661400070827     c                   Kfld                    d98nsp
661500070827     c                   Kfld                    kAr5Trd
661600150325     c     kAR4          Klist
661700150325     c                   Kfld                    d98Aas
661800150325     c                   Kfld                    d98Lnp
661900150325     c                   Kfld                    d98nrs
662000150325     c                   Kfld                    d98nsp
662100150325     c                   Kfld                    kAr4Trc
662200150218     c*
662300150218     c     kart          Klist
662400150218     c                   Kfld                    d98Aas
662500150218     c                   Kfld                    d98Lnp
662600150218     c                   Kfld                    d98nrs
662700150218     c                   Kfld                    d98nsp
662800150218     c     kbrv          Klist
662900150218     c                   Kfld                    artfls
663000150218     c                   Kfld                    artlna
663100150218     c                   Kfld                    artnrs
663200150218     c                   Kfld                    artnsc
663300150325     C     Kfapd         klist
663400150325     C                   kfld                    kpdtip
663500150325     C                   kfld                    kpdpdr
663600930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
663700930414     C     KEYTAB        KLIST
663800930414     C                   KFLD                    KTBUTE
663900930414     C                   KFLD                    KTBCOD
664000930414     C                   KFLD                    KTBKEY
664100060406     C     Ktab          KLIST
664200060406     C                   KFLD                    KTBUTE
664300060406     C                   KFLD                    KTBCOD
664400930414     C*
664500941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
664600941227     C     KEYCLN        KLIST
664700941227     C                   KFLD                    CLNTFP
664800941227     C                   KFLD                    CLNTFA
664900941227     C                   KFLD                    CLNANN
665000941227     C                   KFLD                    CLNMES
665100941227     C*
665200941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
665300941227     C     KEYCAS        KLIST
665400941227     C                   KFLD                    CASPTA
665500941227     C                   KFLD                    CASLNP
665600941227     C                   KFLD                    CASLNA
665700941227     C                   KFLD                    CASTSP
665800941227     C                   KFLD                    CASDDE
665900930420     C*
666000990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
666100930420     C     KEYEVB        KLIST
666200930420     C                   KFLD                    KEBAAS
666300930420     C                   KFLD                    KEBLNP
666400930420     C                   KFLD                    KEBNRS
666500930420     C                   KFLD                    KEBNSP
666600010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
666700010821     C     KTBE          KLIST
666800010821     C                   KFLD                    TBECOD
666900010821     C                   KFLD                    TBEKE1
667000010821     C                   KFLD                    TBEKE2
667100930414     C*
667200010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
667300930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
667400930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
667500930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
667600930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
667700930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
667800930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
667900930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
668000930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
668100930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
668200010821     C     *LIKE         DEFINE    D98CAS        INPIMP
668300010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
668400010821     C     *LIKE         DEFINE    D98VCA        INPDIV
668500930414     C*
668600930414     C* RECUPERO IL CODICE DELL'AZIENDA
668700930414     C                   Z-ADD     1             CODUT             1 0
668800970701     C                   Z-ADD     *ZEROS        I                 6 0
668900930414     C*
669000930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
669100930414     C                   EXSR      AZZERA
669200930414     C*
669300930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
669400941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
669500941227     C                   MOVEL     'S'           CTABSN            1
669600930414     C                   EXSR      CARTAB
669700941227     C                   ENDIF
669800041116     c* Se passato umero di spedizioni apro i files , altrimenti no
669900041116     c                   if        d98nsp>0 and wapri=' '
670000041116     c                   open      fnblp01l
670100050405     c                   open      tigcp01l
670200150120     c***                open      fnblt01l
670300041116     c                   open      fnarb01l
670400051115     c                   open      fiar901l
670500150325     c                   open      fiar401l
670600050405     c                   open      tigcp21l
670700150218     c                   open      fnart01l
670800150218     c                   open      fnbrv07l
670900041116     c                   open      fnlbl01l
671000041116     c                   open      fnevb01l
671100070827     c                   open      fiar501l
671200041116     c                   eval      wapri='S'
671300041116     c                   endif
671400930414     C*
671500930414     C                   ENDSR
671600941227     C*------------------------------------------------------------------------*
671700941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
671800941227     C*------------------------------------------------------------------------*
671900941227     C     SUBDAT        BEGSR
672000941227     C*
672100941227     C                   TIME                    DTTIME
672200941227     C                   Z-ADD     DTANN         DSANN
672300941227     C                   Z-ADD     DTMES         DSMES
672400941227     C                   Z-ADD     DTGIO         DSGIO
672500941227     C                   Z-ADD     DTSEC         DSSEC
672600941227     C*
672700941227     C                   ENDSR
672800930414     C********************************************************************
672900930414     C*   CARTAB - CARICA LE TABELLE
673000930414     C********************************************************************
673100930414     C     CARTAB        BEGSR
673200930414     C*
673300930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
673400930414     C     D98ERR        IFEQ      '0'
673500930421     C                   Z-ADD     0             I
673600930414     C                   Z-ADD     CODUT         KTBUTE
673700930414     C                   MOVEL     '1Y'          KTBCOD
673800930414     C                   MOVEL     *BLANKS       KTBKEY
673900930414     C     KEYTAB        SETLL     TABEL00F                           99
674000930414     C  N99              READ      TABEL00F                               99
674100930414     C     *IN99         IFEQ      '1'
674200930414     C                   MOVEL     '1'           D98ERR
674300930414     C                   MOVEL     MSG(5)        D98MSG
674400930414     C                   END
674500930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
674600930414     C     TBLCOD        ANDEQ     '1Y'
674700930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
674800930421     C                   ADD       1             I
674900930421     C                   MOVEL     TBLKEY        C1Y(I)
675000930414     C                   MOVEL     TBLUNI        DS1Y
675100930421     C                   MOVEL     §1YFEI        I1Y(I)
675200930421     C                   MOVEL     §1YFEC        E1Y(I)
675300930414     C                   END                                                    <-- 3 ---
675400930414     C                   READ      TABEL00F                               99
675500930414     C                   END                                                    <-- 2 ---
675600930414     C                   END
675700930414     C*
675800930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
675900930414     C     D98ERR        IFEQ      '0'
676000930421     C                   Z-ADD     0             I
676100930414     C                   Z-ADD     CODUT         KTBUTE
676200930414     C                   MOVEL     '1X'          KTBCOD
676300930414     C                   MOVEL     *BLANKS       KTBKEY
676400930414     C     KEYTAB        SETLL     TABEL00F                           99
676500930414     C  N99              READ      TABEL00F                               99
676600930414     C     *IN99         IFEQ      '1'
676700930414     C                   MOVEL     '1'           D98ERR
676800930414     C                   MOVEL     MSG(1)        D98MSG
676900930414     C                   END
677000930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
677100930414     C     TBLCOD        ANDEQ     '1X'
677200930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
677300930421     C                   ADD       1             I
677400930421     C                   MOVEL     TBLKEY        C1X(I)
677500930414     C                   MOVEL     TBLUNI        DS1X
677600930421     C                   Z-ADD     §1XHHG        G1X(I)
677700930421     C                   Z-ADD     §1XHHO        O1X(I)
677800930421     C                   Z-ADD     §1XHHM        M1X(I)
677900930421     C                   Z-ADD     §1XHHP        P1X(I)
678000930421     C                   Z-ADD     §1XHHI        I1X(I)
678100930421     C                   Z-ADD     §1XHHS        S1X(I)
678200930414     C                   END                                                    <-- 3 ---
678300930414     C                   READ      TABEL00F                               99
678400931223     C                   ENDDO                                                  <-- 2 ---
678500050523     c
678600050523     c* controllo riempimento skiere
678700050523     c                   clear                   trul0sds
678800050523     c                   eval      i0sski='C1X/G1X/O1X/...'
678900050523     c                   eval      i0sele=%elem(c1x)
679000050523     c                   eval      i0spie=i
679100050523     c                   eval      i0sfile='TABEL00F  '
679200050523     c                   eval      i0scod=' 1X  '
679300050523     c                   eval      i0spgm='TNSD99R'
679400050523     c                   movel     trul0sds      kpjbu
679500050523     c                   call      'TRUL0SR'
679600050523     c                   parm                    kpjba
679700050523     C                   END
679800930421     C*
679900930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
680000930421     C     D98ERR        IFEQ      '0'
680100930421     C                   Z-ADD     0             I
680200930421     C                   Z-ADD     CODUT         KTBUTE
680300930421     C                   MOVEL     '2A'          KTBCOD
680400060406     C     Ktab          SETLL     TABEL00F                           99
680500060406     C  N99ktab          READe     TABEL00F                               99
680600930421     C     *IN99         IFEQ      '1'
680700930421     C                   MOVEL     '1'           D98ERR
680800930421     C                   MOVEL     MSG(8)        D98MSG
680900930421     C                   END
681000930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
681100930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
681200930421     C                   ADD       1             I
681300930421     C                   MOVEL     TBLKEY        C2A(I)
681400060406     C                   MOVEL     TBLUNI        ds2a
681500060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
681600060406     C                   MOVEL     w003a         D2A(I)
681700930421     C                   END                                                    <-- 3 ---
681800060406     C     ktab          READe     TABEL00F                               99
681900931223     C                   ENDDO                                                  <-- 2 ---
682000050523     c* controllo riempimento skiere
682100050523     c                   clear                   trul0sds
682200050523     c                   eval      i0sski='C2A/D2A'
682300050523     c                   eval      i0sele=%elem(c2a)
682400050523     c                   eval      i0spie=i
682500050523     c                   eval      i0sfile='TABEL00F  '
682600060406     c                   eval      i0scod='2A/2Z'
682700050523     c                   eval      i0spgm='TNSD99R'
682800050523     c                   movel     trul0sds      kpjbu
682900050523     c                   call      'TRUL0SR'
683000050523     c                   parm                    kpjba
683100930421     C                   END
683200060406     c* Tabella  2Z
683300060406     C     D98ERR        IFEQ      '0'
683400060406     C                   Z-ADD     CODUT         KTBUTE
683500060406     C                   MOVEL     '2Z'          KTBCOD
683600060406     C     Ktab          SETLL     TABEL00F                           99
683700060406     C  N99ktab          READe     TABEL00F                               99
683800060406     C     *IN99         IFEQ      '1'
683900060406     C                   MOVEL     '1'           D98ERR
684000060406     C                   MOVEL     MSG(33)       D98MSG
684100060406     C                   END
684200060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
684300060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
684400060406     C                   Z-ADD     1             I
684500060406     C                   MOVEL     TBLKEY        w003a
684600060406     c     w003a         lookup    c2a(i)                                 99
684700060406     C     *IN99         IFEQ      *off
684800060406     C                   MOVEL     '1'           D98ERR
684900060406     C                   MOVEL     MSG(34)       D98MSG
685000060406     C                   Else
685100060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
685200060406     C                   END                                                    <-- 3 ---
685300060406     C                   END                                                    <-- 3 ---
685400060406     C     ktab          READe     TABEL00F                               99
685500060406     C                   ENDDO                                                  <-- 2 ---
685600060406     C                   END
685700941227     C*
685800941227     C* CARICA TABELLA CODICI BOLLA -3A-
685900941227     C     D98ERR        IFEQ      '0'
686000941228     C                   Z-ADD     0             I
686100941227     C                   Z-ADD     CODUT         KTBUTE
686200941227     C                   MOVEL     '3A'          KTBCOD
686300941227     C                   MOVEL     *BLANKS       KTBKEY
686400941227     C     KEYTAB        SETLL     TABEL00F                           99
686500941227     C  N99              READ      TABEL00F                               99
686600941227     C     *IN99         IFEQ      '1'
686700941227     C                   MOVEL     '1'           D98ERR
686800941227     C                   MOVEL     MSG(22)       D98MSG
686900941227     C                   END
687000941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
687100941227     C     TBLCOD        ANDEQ     '3A'
687200941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
687300941227     C                   ADD       1             I
687400941227     C                   MOVEL     TBLKEY        C3A(I)
687500941227     C                   MOVEL     TBLUNI        DS3A
687600941227     C                   MOVEL     §3ATB1        T3A(I)
687700941228     C                   Z-ADD     §3AFCA        F3A(I)
687800990308     C                   MOVEL     §3ARBL        R3A(I)
687900941227     C                   END                                                    <-- 3 ---
688000941227     C                   READ      TABEL00F                               99
688100941227     C                   END                                                    <-- 2 ---
688200050523     c* controllo riempimento skiere
688300050523     c                   clear                   trul0sds
688400050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
688500050523     c                   eval      i0sele=%elem(c3a)
688600050523     c                   eval      i0spie=i
688700050523     c                   eval      i0sfile='TABEL00F  '
688800050523     c                   eval      i0scod=' 3A  '
688900050523     c                   eval      i0spgm='TNSD99R'
689000050523     c                   movel     trul0sds      kpjbu
689100050523     c                   call      'TRUL0SR'
689200050523     c                   parm                    kpjba
689300941227     C                   END
689400971210     C*
689500020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
689600020226     C                   Z-ADD     *ZEROS        J                 3 0
689700971210IF  0C     D98ERR        IFEQ      '0'
689800971210     C     *LOVAL        SETLL     AZORG01L
689900971210     C                   READ      AZORG01L                               98
690000971210DO  1C     *IN98         DOWEQ     *OFF
690100971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
690200971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
690300971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
690400020627     C                   MOVEL     ORGDE3        OG143
690500020702     C*
690600020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
690700020702     C                   CLEAR                   DNTW
690800020702     C*
690900020702     C                   MOVEL(P)  'NTW'         TBECOD
691000020702     C                   MOVEL(P)  §OGNTW        TBEKE1
691100020702     C     KTBE          CHAIN     TNTBE01L
691200020702     C                   IF        %found(TNTBE01L)
691300020702     C                   MOVEL     TBEUNI        DNTW
691400020702     C                   ENDIF
691500020702     C*
691600020702     C                   MOVEL     §NTWFIE       FL1(F)
691700020627     C*
691800020226     C* VERIFICO SE FILIALE FEDEX
691900020226     C                   IF        §OGNTW = 'FED'
692000020226     C                   ADD       1             J
692100020226     C                   Z-ADD     ORGFIL        FILFED(J)
692200020226     C                   ENDIF
692300020226     C*
692400971210E   2C                   ENDIF
692500971210     C                   READ      AZORG01L                               98
692600971210E   1C                   ENDDO
692700971210E   0C                   ENDIF
692800010821     C*
692900010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
693000010821     C                   MOVEL     'GED'         TBECOD
693100070514     C                   MOVEL     *BLANKS       TBEKE1
693200070514     C                   MOVEL     '1'           TBEKE1
693300010821     C                   MOVEL     *BLANKS       TBEKE2
693400010821     C     KTBE          CHAIN     TNTBE01L                           21
693500070514     C  N21              MOVEL     TBEUNI        DGED
693600150326     c
693700150326     C* REPERISCO LA tabella flag operativi per tabella OLP
693800150326     c                   clear                   dvpo
693900150326     C                   MOVEL     'VPO'         TBECOD
694000150326     C                   MOVEL     *BLANKS       TBEKE2
694100150326     C                   MOVEL(P)  'VPO'         TBEKE1
694200150326     C     KTBE          CHAIN     TNTBE01L                           21
694300150326     C  N21              MOVEL     TBEUNI        DVPO
694400150326     C*
694500031201     C*
694600031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
694700031201     C     D98ERR        IFEQ      '0'
694800060403     C                   Z-ADD     0             I7QW              3 0
694900031201     C                   Z-ADD     CODUT         KTBUTE
695000031201     C                   MOVEL     '7Q'          KTBCOD
695100031201     C                   MOVEL     *BLANKS       KTBKEY
695200031201     C     KEYTAB        SETLL     TABEL00F                           99
695300031201     C  N99              READ      TABEL00F                               99
695400031201     C     *IN99         IFEQ      '1'
695500031201     C                   MOVEL     '1'           D98ERR
695600031201     C                   MOVEL     MSG(8)        D98MSG
695700031201     C                   ENDIF
695800031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
695900031201     C     TBLCOD        ANDEQ     '7Q'
696000031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
696100031201     C                   MOVEL     TBLUNI        DS7Q
696200031201     C                   IF        §7Q4D1 = 'R' OR
696300031201     C                             §7Q4D2 = 'R' OR
696400031201     C                             §7Q4D3 = 'R' OR
696500031201     C                             §7Q4D4 = 'R' OR
696600031201     C                             §7Q4D5 = 'R'
696700031201     C                   ADD       1             I7QW
696800031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
696900031201     C                   ENDIF                                                  <-- 3 ---
697000031201     C                   ENDIF                                                  <-- 3 ---
697100031201     C                   READ      TABEL00F                               99
697200031201     C                   ENDDO                                                  <-- 2 ---
697300031201     C                   ENDIF
697400031201     C*
697500031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
697600031201     C     D98ERR        IFEQ      '0'
697700031201     C                   Z-ADD     CODUT         KTBUTE
697800031201     C                   MOVEL     '7W'          KTBCOD
697900031201     C                   MOVEL     *BLANKS       KTBKEY
698000031201     C     KEYTAB        SETLL     TABEL00F                           99
698100031201     C  N99              READ      TABEL00F                               99
698200031201     C     *IN99         IFEQ      '1'
698300031201     C                   MOVEL     '1'           D98ERR
698400031201     C                   MOVEL     MSG(8)        D98MSG
698500031201     C                   ENDIF
698600031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
698700031201     C     TBLCOD        ANDEQ     '7W'
698800031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
698900031201     C                   MOVEL     TBLUNI        DS7W
699000031201     C                   IF        §7W6D1 = 'C' OR
699100031201     C                             §7W6D2 = 'C' OR
699200031201     C                             §7W6D3 = 'C' OR
699300031201     C                             §7W6D4 = 'C' OR
699400031201     C                             §7W6D5 = 'C'
699500031201     C                   ADD       1             I7QW
699600031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
699700031201     C                   ENDIF                                                  <-- 3 ---
699800031201     C                   ENDIF                                                  <-- 3 ---
699900031201     C                   READ      TABEL00F                               99
700000031201     C                   ENDDO                                                  <-- 2 ---
700100050523     c* controllo riempimento skiere
700200050523     c                   clear                   trul0sds
700300050523     c                   eval      i0sski='SK7QW'
700400050523     c                   eval      i0sele=%elem(sk7qw)
700500050523     c                   eval      i0spie=i7qw
700600050523     c                   eval      i0sfile='TABEL00F  '
700700050523     c                   eval      i0scod='7Q-7W'
700800050523     c                   eval      i0spgm='TNSD99R'
700900050523     c                   movel     trul0sds      kpjbu
701000050523     c                   call      'TRUL0SR'
701100050523     c                   parm                    kpjba
701200031201     C                   ENDIF
701300930414     C*
701400930414     C                   ENDSR
701500930414     C********************************************************************
701600930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
701700930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
701800930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
701900930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
702000930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
702100930414     C********************************************************************
702200930414     C     RO3ORE        BEGSR
702300930414     C*
702400930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
702500930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
702600930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
702700050902     c                   movel     ' '           WR3MATPOM         1
702800931209     C                   Z-ADD     *ZEROS        WR3HHO
702900931209     C                   Z-ADD     *ZEROS        WR3HHA
703000930414     C                   MOVE      WR3TAB        WR3COD
703100930604     C                   MOVEL     D98LNA        WR3COD            8
703200930421     C                   Z-ADD     1             I
703300930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
703400930414     C     *IN99         IFEQ      '0'
703500930414     C                   MOVE      WR3TAB        WR3COD
703600930414     C                   MOVEL     '   '         WR3COD            8
703700930421     C                   Z-ADD     1             I
703800930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
703900930414     C                   END
704000930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
704100930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
704200930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
704300930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
704400930414     C                   ELSE                                                   <-- 2 -->
704500930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
704600930421     C                   Z-ADD     M1X(I)        WR3HHO
704700930510     C                   Z-ADD     I1X(I)        WR3HHA
704800050902     c                   movel     'M'           WR3MATPOM         1
704900930414     C                   END
705000930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
705100930421     C                   Z-ADD     P1X(I)        WR3HHO
705200930510     C                   Z-ADD     S1X(I)        WR3HHA
705300050902     c                   movel     'P'           WR3MATPOM
705400930414     C                   END
705500930414     C                   END                                                    <-- 2 ---
705600930414     C                   END                                                    <-- 1 ---
705700930414     C*
705800930414     C                   ENDSR
705900930423     C********************************************************************
706000930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
706100930423     C********************************************************************
706200930423     C     W98D98        BEGSR
706300930423     C*
706400941228     C*
706500941228     C* ORE REALI PARTENZA INTERNE
706600941228     C                   Z-ADD     W98RPI        W98               3 0
706700941228     C                   EXSR      W98999
706800941228     C                   Z-ADD     D98           D98RPI
706900941228     C* ORE REALI PARTENZA CLIENTE
707000941228     C                   Z-ADD     W98RPC        W98
707100941228     C                   EXSR      W98999
707200941228     C                   Z-ADD     D98           D98RPC
707300930423     C* ORE REALI TRAZIONE INTERNE
707400941228     C                   Z-ADD     W98RTI        W98
707500941228     C                   EXSR      W98999
707600941228     C                   Z-ADD     D98           D98RTI
707700930423     C* ORE REALI TRAZIONE CLIENTE
707800941228     C                   Z-ADD     W98RTC        W98
707900941228     C                   EXSR      W98999
708000941228     C                   Z-ADD     D98           D98RTC
708100930423     C* ORE REALI CONSEGNA INTERNE
708200941228     C                   Z-ADD     W98RCI        W98
708300941228     C                   EXSR      W98999
708400941228     C                   Z-ADD     D98           D98RCI
708500930423     C* ORE REALI CONSEGNA CLIENTE
708600941228     C                   Z-ADD     W98RCC        W98
708700941228     C                   EXSR      W98999
708800941228     C                   Z-ADD     D98           D98RCC
708900930423     C* ORE REALI RESA INTERNE
709000941228     C                   Z-ADD     W98RRI        W98
709100941228     C                   EXSR      W98999
709200941228     C                   Z-ADD     D98           D98RRI
709300930423     C* ORE REALI RESA CLIENTE
709400941228     C                   Z-ADD     W98RRC        W98
709500941228     C                   EXSR      W98999
709600941228     C                   Z-ADD     D98           D98RRC
709700941228     C* ORE DIFF. PARTENZA INTERNE
709800941228     C                   Z-ADD     W98FPI        W98
709900941228     C                   EXSR      W98999
710000941228     C                   Z-ADD     D98           D98FPI
710100941228     C* ORE DIFF. PARTENZA CLIENTE
710200941228     C                   Z-ADD     W98FPC        W98
710300941228     C                   EXSR      W98999
710400941228     C                   Z-ADD     D98           D98FPC
710500930423     C* ORE DIFF. TRAZIONE INTERNE
710600941228     C                   Z-ADD     W98FTI        W98
710700941228     C                   EXSR      W98999
710800941228     C                   Z-ADD     D98           D98FTI
710900930423     C* ORE DIFF. TRAZIONE CLIENTE
711000941228     C                   Z-ADD     W98FTC        W98
711100941228     C                   EXSR      W98999
711200941228     C                   Z-ADD     D98           D98FTC
711300930423     C* ORE DIFF. CONSEGNA INTERNE
711400941228     C                   Z-ADD     W98FCI        W98
711500941228     C                   EXSR      W98999
711600941228     C                   Z-ADD     D98           D98FCI
711700930423     C* ORE DIFF. CONSEGNA CLIENTE
711800941228     C                   Z-ADD     W98FCC        W98
711900941228     C                   EXSR      W98999
712000941228     C                   Z-ADD     D98           D98FCC
712100930423     C* ORE DIFF. RESA INTERNE
712200941228     C                   Z-ADD     W98FRI        W98
712300941228     C                   EXSR      W98999
712400941228     C                   Z-ADD     D98           D98FRI
712500930423     C* ORE DIFF. RESA CLIENTE
712600941228     C                   Z-ADD     W98FRC        W98
712700941228     C                   EXSR      W98999
712800941228     C                   Z-ADD     D98           D98FRC
712900941228     C* ORE NON CONF. PARTENZA INTERNE
713000941228     C     W98NPI        IFGT      999
713100941228     C                   Z-ADD     999           D98NPI
713200941228     C                   ELSE
713300941228     C                   Z-ADD     W98NPI        D98NPI
713400941228     C                   END
713500941228     C* ORE NON CONF. PARTENZA CLIENTE
713600941228     C     W98NPC        IFGT      999
713700941228     C                   Z-ADD     999           D98NPC
713800941228     C                   ELSE
713900941228     C                   Z-ADD     W98NPC        D98NPC
714000941228     C                   END
714100930423     C* ORE NON CONF. TRAZIONE INTERNE
714200930423     C     W98NTI        IFGT      999
714300930423     C                   Z-ADD     999           D98NTI
714400930423     C                   ELSE
714500930423     C                   Z-ADD     W98NTI        D98NTI
714600930423     C                   END
714700930423     C* ORE NON CONF. TRAZIONE CLIENTE
714800930423     C     W98NTC        IFGT      999
714900930423     C                   Z-ADD     999           D98NTC
715000930423     C                   ELSE
715100930423     C                   Z-ADD     W98NTC        D98NTC
715200930423     C                   END
715300930423     C* ORE NON CONF. CONSEGNA INTERNE
715400930423     C     W98NCI        IFGT      999
715500930423     C                   Z-ADD     999           D98NCI
715600930423     C                   ELSE
715700930423     C                   Z-ADD     W98NCI        D98NCI
715800930423     C                   END
715900930423     C* ORE NON CONF. CONSEGNA CLIENTE
716000930423     C     W98NCC        IFGT      999
716100930423     C                   Z-ADD     999           D98NCC
716200930423     C                   ELSE
716300930423     C                   Z-ADD     W98NCC        D98NCC
716400930423     C                   END
716500930423     C* ORE NON CONF. RESA INTERNE
716600930423     C     W98NRI        IFGT      999
716700930423     C                   Z-ADD     999           D98NRI
716800930423     C                   ELSE
716900930423     C                   Z-ADD     W98NRI        D98NRI
717000930423     C                   END
717100930423     C* ORE NON CONF. RESA CLIENTE
717200930423     C     W98NRC        IFGT      999
717300930423     C                   Z-ADD     999           D98NRC
717400930423     C                   ELSE
717500930423     C                   Z-ADD     W98NRC        D98NRC
717600930423     C                   END
717700930423     C* ORE X TURNO CHIUSURA INTERNE
717800941228     C                   Z-ADD     W98HHI        W98
717900941228     C                   EXSR      W98999
718000941228     C                   Z-ADD     D98           D98HHI
718100930423     C* ORE X TURNO CHIUSURA CLIENTE
718200941228     C                   Z-ADD     W98HHC        W98
718300941228     C                   EXSR      W98999
718400941228     C                   Z-ADD     D98           D98HHC
718500060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
718600060418     C                   Z-ADD     W98EGI        W98
718700060418     C                   EXSR      W98999
718800060418     C                   Z-ADD     D98           D98hEGI
718900060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
719000060418     C                   Z-ADD     W98EGC        W98
719100060418     C                   EXSR      W98999
719200060418     C                   Z-ADD     D98           D98hEGC
719300060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
719400060420     C                   Z-ADD     W98EAI        W98
719500060420     C                   EXSR      W98999
719600060420     C                   Z-ADD     D98           D98hEAI
719700060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
719800060420     C                   Z-ADD     W98EAC        W98
719900060420     C                   EXSR      W98999
720000060420     C                   Z-ADD     D98           D98hEAC
720100930423     C* ORE X LASC. AVVISO INTERNE
720200941228     C                   Z-ADD     W98HLI        W98
720300941228     C                   EXSR      W98999
720400941228     C                   Z-ADD     D98           D98HLI
720500930423     C* ORE X LASC. AVVISO CLIENTE
720600941228     C                   Z-ADD     W98HLC        W98
720700941228     C                   EXSR      W98999
720800941228     C                   Z-ADD     D98           D98HLC
720900930423     C* ORE X GIACENZA INTERNE
721000941228     C                   Z-ADD     W98HGI        W98
721100941228     C                   EXSR      W98999
721200941228     C                   Z-ADD     D98           D98HGI
721300930423     C* ORE X GIACENZA CLIENTE
721400941228     C                   Z-ADD     W98HGC        W98
721500941228     C                   EXSR      W98999
721600941228     C                   Z-ADD     D98           D98HGC
721700941228     C* ORE X FEST. PART. INTERNE
721800941228     C                   Z-ADD     W98HPI        W98
721900941228     C                   EXSR      W98999
722000941228     C                   Z-ADD     D98           D98HPI
722100941228     C* ORE X FEST. PART. CLIENTE
722200941228     C                   Z-ADD     W98HPC        W98
722300941228     C                   EXSR      W98999
722400941228     C                   Z-ADD     D98           D98HPC
722500930423     C* ORE X FEST. TRAZ. INTERNE
722600941228     C                   Z-ADD     W98HFI        W98
722700941228     C                   EXSR      W98999
722800941228     C                   Z-ADD     D98           D98HFI
722900930423     C* ORE X FEST. TRAZ. CLIENTE
723000941228     C                   Z-ADD     W98HFC        W98
723100941228     C                   EXSR      W98999
723200941228     C                   Z-ADD     D98           D98HFC
723300930423     C* ORE X FEST. CONS. INTERNE
723400941228     C                   Z-ADD     W98HEI        W98
723500941228     C                   EXSR      W98999
723600941228     C                   Z-ADD     D98           D98HEI
723700930423     C* ORE X FEST. CONS. CLIENTE
723800941228     C                   Z-ADD     W98HEC        W98
723900941228     C                   EXSR      W98999
724000941228     C                   Z-ADD     D98           D98HEC
724100930511     C* ORE X F.DEPOSITO INTERNE
724200941228     C                   Z-ADD     W98HDI        W98
724300941228     C                   EXSR      W98999
724400941228     C                   Z-ADD     D98           D98HDI
724500930511     C* ORE X F.DEPOSITO CLIENTE
724600941228     C                   Z-ADD     W98HDC        W98
724700941228     C                   EXSR      W98999
724800941228     C                   Z-ADD     D98           D98HDC
724900930511     C* ORE X CONTRASSEGNO INTERNE
725000941228     C                   Z-ADD     W98HOI        W98
725100941228     C                   EXSR      W98999
725200941228     C                   Z-ADD     D98           D98HOI
725300930511     C* ORE X CONTRASSEGNO CLIENTE
725400941228     C                   Z-ADD     W98HOC        W98
725500941228     C                   EXSR      W98999
725600941228     C                   Z-ADD     D98           D98HOC
725700930914     C* ORE DI CONSEGNABILITA' INTERNE
725800941228     C                   Z-ADD     W98OCI        W98
725900941228     C                   EXSR      W98000
726000941228     C                   Z-ADD     D98           D98OCI
726100930914     C* ORE DI CONSEGNABILITA' CLIENTE
726200941228     C                   Z-ADD     W98OCC        W98
726300941228     C                   EXSR      W98000
726400941228     C                   Z-ADD     D98           D98OCC
726500071122     C
726600071122     c* Per i codici bolla M prepagati annullati, azzero tutto
726700071122     c                   if        d98cbo='M '
726800071122     c                   clear                   d98rpi
726900071122     c                   clear                   d98rpc
727000071122     c                   clear                   d98rti
727100071122     c                   clear                   d98rtc
727200071122     c                   clear                   d98rci
727300071122     c                   clear                   d98rcc
727400071122     c                   clear                   d98rri
727500071122     c                   clear                   d98rrc
727600071122     c                   clear                   d98fpi
727700071122     c                   clear                   d98fpc
727800071122     c                   clear                   d98fti
727900071122     c                   clear                   d98ftc
728000071122     c                   clear                   d98fci
728100071122     c                   clear                   d98fcc
728200071122     c                   clear                   d98fri
728300071122     c                   clear                   d98frc
728400071122     c                   clear                   d98npi
728500071122     c                   clear                   d98npc
728600071122     c                   clear                   d98nti
728700071122     c                   clear                   d98ntc
728800071122     c                   clear                   d98nci
728900071122     c                   clear                   d98ncc
729000071122     c                   clear                   d98nri
729100071122     c                   clear                   d98nrc
729200071122     c                   clear                   d98OCi
729300071122     c                   clear                   d98OCC
729400071122     c                   endif
729500930423     C*
729600100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
729700100308     c*  calcolo
729800100308     c                   eval      d98dcr=s98dcr
729900100308     c                   eval      d98hcr=s98hcr
730000100308     c                   eval      d98tcr=s98tcr
730100100308     c
730200930423     C                   ENDSR
730300941228     C***********************************************************************
730400941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
730500941228     C***********************************************************************
730600941228     C     W98999        BEGSR
730700941228     C     W98           IFLT      -999
730800941228     C                   Z-ADD     -999          D98               3 0
730900941228     C                   ELSE
731000941228     C     W98           IFGT      999
731100941228     C                   Z-ADD     999           D98
731200941228     C                   ELSE
731300941228     C                   Z-ADD     W98           D98
731400941228     C                   END
731500941228     C                   END
731600941228     C                   ENDSR
731700941228     C***********************************************************************
731800941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
731900941228     C***********************************************************************
732000941228     C     W98000        BEGSR
732100941228     C     W98           IFLE      *ZEROS
732200941228     C                   Z-ADD     *ZEROS        D98
732300941228     C                   ELSE
732400941228     C     W98           IFGT      999
732500941228     C                   Z-ADD     999           D98
732600941228     C                   ELSE
732700941228     C                   Z-ADD     W98           D98
732800941228     C                   END
732900941228     C                   END
733000941228     C                   ENDSR
733100930305     C***********************************************************************
733200930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
733300930305     C***********************************************************************
733400930305     C     AZZERA        BEGSR
733500150219     c* data e ora del momento
733600150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
733700150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
733800150219     C                   Z-ADD     DTORA4        WHHODI            4 0
733900930305     C*
734000141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
734100141107     c                   clear                   or_ddc
734200141108     c                   clear                   sav_12dat         8 0
734300141107     c                   clear                   or_datarif
734400140402     c                   clear                   wpick2            1
734500140402     c                   clear                   Primotipo
734600090928     c                   clear                   Primodev
734700090928     c                   clear                   Primohev
734800070827     c                   clear                   CallAR5
734900071122     c                   clear                   d98cbo
735000100308     c                   clear                   s98dcr
735100100308     c                   clear                   s98hcr
735200100308     c                   clear                   s98tcr
735300141106     c                   clear                   nosomma           1
735400141229     c                   clear                   wtrasm
735500150324     c                   clear                   avvcmc
735600150325     c                   clear                   DSbl4L
735700150401     c                   clear                   fnlv82ds
735800150331     c                   clear                   DS7R
735900930323     C                   MOVEL     '0'           D98ERR
736000930323     C                   MOVEL     *BLANKS       D98MSG
736100931124     C                   Z-ADD     *ZEROS        D98TPI
736200931124     C                   Z-ADD     *ZEROS        D98TPC
736300930325     C                   Z-ADD     *ZEROS        D98TTI
736400930325     C                   Z-ADD     *ZEROS        D98TTC
736500930325     C                   Z-ADD     *ZEROS        D98TCI
736600930325     C                   Z-ADD     *ZEROS        D98TCC
736700090827     C                   Z-ADD     *ZEROS        w99TCI
736800090827     C                   Z-ADD     *ZEROS        w99TCC
736900930325     C                   Z-ADD     *ZEROS        D98TRI
737000930325     C                   Z-ADD     *ZEROS        D98TRC
737100090921     C                   Z-ADD     *ZEROS        w99TRI
737200090921     C                   Z-ADD     *ZEROS        w99TRC
737300931124     C                   Z-ADD     *ZEROS        D98RPI
737400931124     C                   Z-ADD     *ZEROS        D98RPC
737500930405     C                   Z-ADD     *ZEROS        D98RTI
737600930405     C                   Z-ADD     *ZEROS        D98RTC
737700930405     C                   Z-ADD     *ZEROS        D98RCI
737800930405     C                   Z-ADD     *ZEROS        D98RCC
737900930405     C                   Z-ADD     *ZEROS        D98RRI
738000930405     C                   Z-ADD     *ZEROS        D98RRC
738100931124     C                   Z-ADD     *ZEROS        D98FPI
738200931124     C                   Z-ADD     *ZEROS        D98FPC
738300930325     C                   Z-ADD     *ZEROS        D98FTI
738400930325     C                   Z-ADD     *ZEROS        D98FTC
738500930325     C                   Z-ADD     *ZEROS        D98FCI
738600930325     C                   Z-ADD     *ZEROS        D98FCC
738700930325     C                   Z-ADD     *ZEROS        D98FRI
738800930325     C                   Z-ADD     *ZEROS        D98FRC
738900931124     C                   MOVEL     *BLANKS       D98CPI
739000931124     C                   MOVEL     *BLANKS       D98CPC
739100930325     C                   MOVEL     *BLANKS       D98CTI
739200930325     C                   MOVEL     *BLANKS       D98CTC
739300930325     C                   MOVEL     *BLANKS       D98CCI
739400930325     C                   MOVEL     *BLANKS       D98CCC
739500930325     C                   MOVEL     *BLANKS       D98CRI
739600930325     C                   MOVEL     *BLANKS       D98CRC
739700931124     C                   Z-ADD     *ZEROS        D98NPI
739800931124     C                   Z-ADD     *ZEROS        D98NPC
739900930325     C                   Z-ADD     *ZEROS        D98NTI
740000930325     C                   Z-ADD     *ZEROS        D98NTC
740100930325     C                   Z-ADD     *ZEROS        D98NCI
740200930325     C                   Z-ADD     *ZEROS        D98NCC
740300930325     C                   Z-ADD     *ZEROS        D98NRI
740400930325     C                   Z-ADD     *ZEROS        D98NRC
740500930414     C                   Z-ADD     *ZEROS        D98HHI
740600930414     C                   Z-ADD     *ZEROS        D98HHC
740700930414     C                   Z-ADD     *ZEROS        D98HOI
740800930414     C                   Z-ADD     *ZEROS        D98HOC
740900930414     C                   Z-ADD     *ZEROS        D98HLI
741000930414     C                   Z-ADD     *ZEROS        D98HLC
741100930414     C                   Z-ADD     *ZEROS        D98HGI
741200930414     C                   Z-ADD     *ZEROS        D98HGC
741300931124     C                   Z-ADD     *ZEROS        D98HPI
741400931124     C                   Z-ADD     *ZEROS        D98HPC
741500930414     C                   Z-ADD     *ZEROS        D98HFI
741600930414     C                   Z-ADD     *ZEROS        D98HFC
741700930416     C                   Z-ADD     *ZEROS        D98HEI
741800930416     C                   Z-ADD     *ZEROS        D98HEC
741900930414     C                   Z-ADD     *ZEROS        D98HRI
742000930414     C                   Z-ADD     *ZEROS        D98HRC
742100930511     C                   Z-ADD     *ZEROS        D98HDI
742200930511     C                   Z-ADD     *ZEROS        D98HDC
742300930325     C                   Z-ADD     *ZEROS        D98DCI
742400930325     C                   Z-ADD     *ZEROS        D98HCI
742500930325     C                   Z-ADD     *ZEROS        D98DCE
742600930325     C                   Z-ADD     *ZEROS        D98HCE
742700931125     C                   Z-ADD     *ZEROS        D98OCI
742800931125     C                   Z-ADD     *ZEROS        D98OCC
742900931202     C                   Z-ADD     *ZEROS        D98DEI
743000931202     C                   Z-ADD     *ZEROS        D98OEI
743100931202     C                   Z-ADD     *ZEROS        D98DEE
743200931202     C                   Z-ADD     *ZEROS        D98OEE
743300150325     C                   clear                   D98dtctd
743400150326     C                   clear                   D98picklim
743500150325     C                   clear                   D98spcdee
743600150331     C                   clear                   fddata
743700140825     c
743800141017     c                   clear                   d98ddc
743900141017     c                   clear                   d98cons
744000141017     c                   clear                   wd98ndc
744100140910     c                   clear                   wd98fbc
744200150119     c                   clear                   wd98acc
744300140825     c                   clear                   wtrulor
744400150219     c
744500150219     c                   clear                   IMPdata
744600150219     c                   clear                   IMPfgs
744700150219     c                   clear                   DISdata
744800150219     c                   clear                   DISfgs
744900150219     c                   clear                   DISora
745000150219     c                   clear                   DISnpg
745100150219     c                   clear                   IDnpg
745200150219     c                   clear                   IDdata
745300150219     c                   clear                   IDora
745400150219     c                   clear                   IDfgs
745500931125     C* CAMPI VARI
745600931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
745700140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
745800140825      *   DALLA LETTURA DI QUESTI FILE                               *
745900931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
746000931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
746100931125     C     D98NSP        IFNE      *ZEROS
746200930525     C                   MOVEL     *BLANKS       D98TSP
746300941228     C                   MOVEL     *BLANKS       D98CAD
746400970728     C                   MOVEL     *BLANKS       D98LOD
746500930525     C                   Z-ADD     *ZEROS        D98TFP
746600930525     C                   Z-ADD     *ZEROS        D98TFA
746700930525     C                   Z-ADD     *ZEROS        D98LNA
746800930525     C                   Z-ADD     *ZEROS        D98DSP
746900941228     C                   Z-ADD     *ZEROS        D98MGS
747000930525     C                   Z-ADD     *ZEROS        D98DTI
747100930525     C                   Z-ADD     *ZEROS        D98HTI
747200930525     C                   Z-ADD     *ZEROS        D98DCM
747300930525     C                   Z-ADD     *ZEROS        D98HMC
747400930525     C                   MOVEL     *BLANKS       D98GC1
747500930525     C                   MOVEL     *BLANKS       D98GC2
747600010820     C                   Z-ADD     *ZEROS        D98CAS
747700010821     C                   MOVEL     *BLANKS       D98VCA
747800930525     C                   MOVEL     *BLANKS       D98TIC
747900930525     C                   Z-ADD     *ZEROS        D98DCR
748000930525     C                   Z-ADD     *ZEROS        D98HCR
748100930525     C                   MOVEL     *BLANKS       D98TCR
748200050914     C                   MOVEL     *BLANKS       Consideraora      1
748300930525     C                   MOVEL     *BLANKS       D98CMC
748400930525     C                   Z-ADD     *ZEROS        D98DAG
748500930525     C                   Z-ADD     *ZEROS        D98DCG
748600930525     C                   MOVEL     *BLANKS       D98FFD
748700930525     C                   Z-ADD     *ZEROS        D98NCD
748800930525     C                   MOVEL     *BLANKS       D98FNS
748900931124     C                   Z-ADD     *ZEROS        D98DAM
749000940628     C                   MOVEL     *BLANKS       D98FIN
749100940628     C                   Z-ADD     *ZEROS        D98KSC
749200940628     C                   MOVEL     *BLANKS       D98TBL
749300931125     C                   END
749400960229     C     D98TLA        IFEQ      'T'
749500960229     C                   Z-ADD     *ZEROS        D98DTI
749600960229     C                   Z-ADD     *ZEROS        D98HTI
749700960229     C                   ENDIF
749800931125     C* CAMPI WORK
749900930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
750000930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
750100150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
750200930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
750300941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
750400960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
750500930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
750600930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
750700940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
750800930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
750900150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
751000931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
751100931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
751200930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
751300930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
751400930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
751500930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
751600930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
751700940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
751800941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
751900930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
752000050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
752100050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
752200140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
752300930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
752400930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
752500930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
752600930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
752700930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
752800930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
752900930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
753000930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
753100930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
753200930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
753300930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
753400930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
753500930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
753600930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
753700140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
753800931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
753900060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
754000960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
754100931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
754200140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
754300140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
754400140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
754500941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
754600970304     C******************** Z-ADD*ZEROS    W98DBR  80
754700970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
754800050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
754900930414     C*
755000941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
755100941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
755200930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
755300930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
755400930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
755500930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
755600930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
755700930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
755800941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
755900941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
756000930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
756100930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
756200930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
756300930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
756400930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
756500930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
756600941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
756700941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
756800930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
756900930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
757000930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
757100930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
757200930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
757300930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
757400930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
757500930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
757600060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
757700060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
757800060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
757900060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
758000930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
758100930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
758200930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
758300930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
758400930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
758500930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
758600930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
758700930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
758800930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
758900930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
759000930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
759100930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
759200930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
759300930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
759400941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
759500941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
759600930423     C*
759700930511     C                   Z-ADD     *ZEROS        W98HF             5 0
759800930423     C                   Z-ADD     *ZEROS        W98HH             5 0
759900060418     C                   Z-ADD     *ZEROS        W98EG             5 0
760000060420     C                   Z-ADD     *ZEROS        W98EA             5 0
760100930423     C                   Z-ADD     *ZEROS        W98HG             5 0
760200930423     C                   Z-ADD     *ZEROS        W98HL             5 0
760300930423     C                   Z-ADD     *ZEROS        W98HO             5 0
760400930511     C                   Z-ADD     *ZEROS        W98HD             5 0
760500930611     C*
760600930611     C                   Z-ADD     *ZEROS        WORELA            3 0
760700960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
760800930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
760900930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
761000931207     C                   Z-ADD     *ZEROS        WORECR            3 0
761100090902     c
761200090902     c                   clear                   wmancaTRAZ
761300150113     c                   clear                   wcaldpcee         1
761400150220     c                   clear                   evemic            2
761500930305     C*
761600060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
761700060406     C                   clear                   LAF                            *LASC.AVVISO FINE
761800091014     C                   clear                   LAp                            *posizione l.avviso
761900060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
762000060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
762100060406c    c                   clear                   LCon
762200060406     C                   clear                   EVD                            *DATA EVENTO
762300060406     C                   clear                   EVO                            *ORA EVENTO
762400060406     C                   clear                   EVC                            *CAUSALE EVENTO
762500131218     C                   clear                   EVT
762600060406     C                   clear                   pVD                            *DATA EVENTO
762700060406     C                   clear                   pVO                            *ORA EVENTO
762800060406     C                   clear                   pVC                            *CAUSALE EVENTO
762900060406     C                   clear                   pVF                            *CAUSALE EVENTO
763000060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
763100060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
763200060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
763300060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
763400060406     C                   clear                   COV                            *DATE FINE L.A. E GI
763500060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
763600060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
763700091014     C                   clear                   GIp                            *posizione vento gia
763800060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
763900060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
764000060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
764100931213     C*
764200060406     C                   clear                   TUC                            *TURNI CHIUSURA
764300931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
764400931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
764500060406     c
764600060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
764700060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
764800060406     c                   clear                   pgii
764900060406     c                   clear                   pgif
765000060406     c                   clear                   pgci
765100060406     c                   clear                   pgcf
765200060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
765300060420     c                   clear                   paii
765400060420     c                   clear                   paif
765500930420     C*
765600930305     C                   ENDSR
765700930512     C***********************************************************************
765800930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
765900930512     C***********************************************************************
766000930512     C     FORCON        BEGSR
766100930512     C*
766200931124     C                   Z-ADD     *ZEROS        D98RPI
766300931124     C                   Z-ADD     *ZEROS        D98RPC
766400930512     C                   Z-ADD     *ZEROS        D98RTI
766500930512     C                   Z-ADD     *ZEROS        D98RTC
766600930512     C                   Z-ADD     *ZEROS        D98RCI
766700930512     C                   Z-ADD     *ZEROS        D98RCC
766800930512     C                   Z-ADD     *ZEROS        D98RRI
766900930512     C                   Z-ADD     *ZEROS        D98RRC
767000931124     C                   Z-ADD     *ZEROS        D98FPI
767100931124     C                   Z-ADD     *ZEROS        D98FPC
767200930512     C                   Z-ADD     *ZEROS        D98FTI
767300930512     C                   Z-ADD     *ZEROS        D98FTC
767400930512     C                   Z-ADD     *ZEROS        D98FCI
767500930512     C                   Z-ADD     *ZEROS        D98FCC
767600930512     C                   Z-ADD     *ZEROS        D98FRI
767700930512     C                   Z-ADD     *ZEROS        D98FRC
767800931124     C                   MOVEL     'S'           D98CPI
767900931124     C                   MOVEL     'S'           D98CPC
768000930512     C                   MOVEL     'S'           D98CTI
768100930512     C                   MOVEL     'S'           D98CTC
768200930512     C                   MOVEL     'S'           D98CCI
768300930512     C                   MOVEL     'S'           D98CCC
768400930512     C                   MOVEL     'S'           D98CRI
768500930512     C                   MOVEL     'S'           D98CRC
768600931124     C                   Z-ADD     *ZEROS        D98NPI
768700931124     C                   Z-ADD     *ZEROS        D98NPC
768800930512     C                   Z-ADD     *ZEROS        D98NTI
768900930512     C                   Z-ADD     *ZEROS        D98NTC
769000930512     C                   Z-ADD     *ZEROS        D98NCI
769100930512     C                   Z-ADD     *ZEROS        D98NCC
769200930512     C                   Z-ADD     *ZEROS        D98NRI
769300930512     C                   Z-ADD     *ZEROS        D98NRC
769400930512     C                   Z-ADD     *ZEROS        D98HHI
769500930512     C                   Z-ADD     *ZEROS        D98HHC
769600930512     C                   Z-ADD     *ZEROS        D98HOI
769700930512     C                   Z-ADD     *ZEROS        D98HOC
769800930512     C                   Z-ADD     *ZEROS        D98HLI
769900930512     C                   Z-ADD     *ZEROS        D98HLC
770000930512     C                   Z-ADD     *ZEROS        D98HGI
770100930512     C                   Z-ADD     *ZEROS        D98HGC
770200931124     C                   Z-ADD     *ZEROS        D98HPI
770300931124     C                   Z-ADD     *ZEROS        D98HPC
770400930512     C                   Z-ADD     *ZEROS        D98HFI
770500930512     C                   Z-ADD     *ZEROS        D98HFC
770600930512     C                   Z-ADD     *ZEROS        D98HEI
770700930512     C                   Z-ADD     *ZEROS        D98HEC
770800930512     C                   Z-ADD     *ZEROS        D98HRI
770900930512     C                   Z-ADD     *ZEROS        D98HRC
771000930512     C                   Z-ADD     *ZEROS        D98HDI
771100930512     C                   Z-ADD     *ZEROS        D98HDC
771200930520     C*
771300930512     C                   ENDSR
771400930305     C********************************************************************
771500971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
771600950406031059090120151181212243273304334365
771700950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
771800950406312831303130313130313031
771900930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
772000971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
772100971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
772200971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
772300971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
772400971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
772500971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
772600971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
772700971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
772800971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
772900971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
773000971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
773100971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
773200971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
773300971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
773400971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
773500971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
773600971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
773700971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
773800971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
773900971127TNSD99R- Bolla inesistente.                                               BOIN20
774000971127TNSD99R-     Cap inesistente.                                             CAPI21
774100971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
774200971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
774300971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
774400971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
774500971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
774600971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
774700971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
774800971127TNSD99R- Contrassegno non trovato.                                        CONT29
774900971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
775000990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
775100990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
775200060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
775300060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
775400971127TNSD99R- 35                                                                   35
775500971127TNSD99R- 36                                                                   36
775600971127TNSD99R- 37                                                                   37
775700971127TNSD99R- 38                                                                   38
775800971127TNSD99R- 39                                                                   39
775900971127TNSD99R- 40                                                                   40
