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
143900941227     C                   ELSE
144000970701     C                   Z-ADD     O951CN        D98TCC
144100941227     C                   END
144200941227     C*
144300941227     C* IMPOSTO ANCHE I TEMPI DI CONSEGNA INTERNI
144400941227     C* PER RICALCOLRE TEMPI DI CONSEGNA IN BASE ALL'ORA DI ARR. DELLA TRAZIONE
144500150508     C                   Z-ADD     O950IN        W990IN            3 0          H.00 INT
144600150508     C                   Z-ADD     O951IN        W991IN            3 0          H.12 INT
144700150508     C* PER RICALCOLRE TEMPI DI CONSEGNA cliente il secondo giorno ( sempre tempi 00)
144800150508     C                   Z-ADD     O950CN        W990CN            3 0          H.00 CLI
144900941227     C*
145000941227     C* SE E' IL CAP DI UN'ISOLA AGGIUNGO IL TEMPO ISOLA
145100941227     C* SE IL FLAG INOLTRO E' = 'I' (ISOLA) AGGIUNGO IL TEMPO ISOLA
145200970701     C     O95ISO        IFEQ      'I'
145300941227     C     D98FIN        OREQ      'I'
145400970701     C                   ADD       O95TIS        D98TCI
145500970701     C                   ADD       O95TIS        D98TCC
145600941228     C* AGGIUNGO L'EVENTUALE ISOLA ANCHE AI TEMPI DI CONSEGNA INTERNI H00 E H12
145700970701     C                   ADD       O95TIS        W990IN
145800970701     C                   ADD       O95TIS        W991IN
145900941227     C                   END
146000090827     c
146100090827     c* Sistemo i tempi di consegna se non sono multipli di 12
146200090827     c                   exsr      SisCONS
146300941228     C*
146400941228     C* SE LA BOLLA HA AVUTO GIACENZA MI SERVIRA' PIU' AVANTI IL TEMPO CONS.ESPR
146500941229     C     D98DAG        IFNE      *ZEROS                                       --- 2 -->
146600941228     C* SE LA SPED. E' GIA' ESPRESSO PRENDO QUELLO APPENA CALCOLATO
146700941228     C     D98TSP        IFEQ      'E'                                          --- 3 -->
146800941228     C                   Z-ADD     D98TCC        W99CCE            3 0
146900941228     C                   ELSE                                                   <-- 3 -->
147000970701     C* RILANCIO PGM CONTR.CAP PER AVERE I TEMPI CONS. ESPR.
147100970701     C                   CLEAR                   SI95
147200970701     C                   MOVEL     '3'           I95TCN
147300970701     C                   MOVEL     D98NZD        I95NAR
147400970701     C                   MOVEL     D98CAD        I95CAP
147500970701     C                   MOVEL     D98LOD        I95LOC
147600970701     C                   MOVEL     D98DSP        I95DAT
147700970701     C                   MOVEL     'E'           I95TSP
147800980602     C                   MOVEL     'S'           I95FRT
147900020226     C*
148000020226     C* VERIFICO SE LNA E' FILIALE FEDEX
148100020226     C                   IF        D98LNA > *ZEROS
148200020226     C     D98LNA        LOOKUP    FILFED                                 55
148300020226     C                   IF        *IN55 = *ON
148400020226     C                   EVAL      I95FI1 = 'S'
148500020226     C                   ENDIF
148600020226     C                   ENDIF
148700020226     C*
148800020226     C                   CALL      'TISI95R'
148900970701     C                   PARM                    SI95
149000970701     C*
149100941228     C* SE NON TROVATO, ERRORE
149200970701     C     O95ERR        IFNE      *BLANKS                                      --- 4 -->
149300941228     C                   MOVEL     '1'           D98ERR
149400970701     C                   MOVEL     O95MSG        D98MSG                         *NON TROV.TEMPI CONS
149500941228     C                   ELSE                                                   <-- 4 -->
149600941228     C* RECUPERO IL TEMPO DI CONSEGNA -> TEMPO TRAZIONE DIVISO 24
149700941228     C* SE IL RISULTATO NON HA RESTO E' UN ARRIVO ORE 00 ALTRIMENTI ORE 12
149800941228     C     CASTCN        DIV       24            WDIV                     99
149900941228     C                   MVR                     WRES
150000941228     C     WRES          IFEQ      *ZEROS
150100970701     C                   Z-ADD     O950CN        W99CCE
150200941228     C                   ELSE
150300970701     C                   Z-ADD     O951CN        W99CCE
150400941228     C                   END
150500941228     C                   ENDIF                                                  <-- 4 ---
150600941228     C                   ENDIF                                                  <-- 3 ---
150700941228     C                   ENDIF                                                  <-- 2 ---
150800941227     C*
150900941227     C                   END                                                    <-- 1 ---
151000941227     C*
151100941227     C                   ENDSR
151200090827     C********************************************************************
151300090827     C*   SisCONS - sistemi i tempi di consegna se non sono multipli di 12
151400090827     C********************************************************************
151500090827     C     SisCONS       BEGSR
151600090827     c                   eval      w99tci=d98tci
151700090827     c                   if        d98tci=10
151800090827     c                   z-add     12            w99tci
151900090827     c                   endif
152000090827     c
152100090827     c                   eval      w99tcc=d98tcc
152200090827     c                   if        d98tcc=10
152300090827     c                   z-add     12            w99tcc
152400090827     c                   endif
152500090827     c
152600090827     C                   ENDSR
152700930420     C********************************************************************
152800930421     C*   CAREVB - CARICA IN SCHIERA IL FILE DEGLI EVENTI BOLLA
152900930420     C********************************************************************
153000930420     C     CAREVB        BEGSR
153100930420     C*
153200930420     C* MI POSIZIONO E LEGGO TUTTI GLI EVENTI DI QUESTA BOLLA
153300930421     C                   Z-ADD     *ZEROS        IE                3 0          *INDICE EVENTI
153400060406     C                   Z-ADD     *ZEROS        IP                3 0          *INDICE EVENTI
153500930421     C                   MOVEL     'N'           WEELAV            1            *ESISTE EVENTO L.AVV
153600930421     C                   MOVEL     'N'           WEEGIA            1            *ESISTE EVENTO GIAC.
153700060406     C                   MOVEL     'N'           WEVPAG            1            *ESISTE EVENTO GIAC.
153800060406     C                   MOVEL     'N'           WEVPAA            1            *ESISTE EVENTO GIAC.
153900930420     C                   Z-ADD     D98AAS        KEBAAS
154000930420     C                   Z-ADD     D98LNP        KEBLNP
154100930420     C                   Z-ADD     D98NRS        KEBNRS
154200930420     C                   Z-ADD     D98NSP        KEBNSP
154300990810     C     KEYEVB        SETLL     FNEVB01L                           99
154400060406     C  N99keyevb        READe     FNEVB01L                               99
154500060406    1C     *IN99         DOWEQ     '0'                                          --- 1 -->
154600930421     C     D98ERR        ANDEQ     '0'
154700060406    2C     EVBATB        IFNE      'A'                                          --- 2 -->
154800930421     C* ATTRAVERSO LA TAB.2A VERIFICO IL TIPO DI EVENTO
154900930421     C                   Z-ADD     1             I
155000930421     C     EVBCEV        LOOKUP    C2A(I)                                 99
155100060406    3C     *IN99         IFEQ      '0'                                          --- 3 -->
155200930421     C                   MOVEL     '1'           D98ERR
155300930421     C                   MOVEL     MSG(9)        D98MSG                         *CAUS.NON TROV.
155400060406   x3C                   ELSE                                                   <-- 3 -->
155500060406     C                   MOVEL     D2A(I)        usa2A
155600060406     C                   MOVEL     D2Z(I)        usa2Z
155700930421     C* CARICO IN SCHIERA SOLO SE DI TIPO 'A' (LASC.AVV) O 'G' (GIAC.)
155800060406    4C     §2AFTC        IFEQ      'A'
155900930421     C                   MOVEL     'S'           WEELAV                         *ESISTE EVENTO L.AVV
156000060406    4C                   END
156100060406    4C     §2AFTC        IFEQ      'G'
156200930421     C                   MOVEL     'S'           WEEGIA                         *ESISTE EVENTO GIAC.
156300060406    4C                   END
156400150216     c* Se si tratta di MIC verifico se uguale alla sped.
156500150303    4c                   if        evbcev='MIC' or evbcev='NIC'
156600150303    5c                   if        %subst(evbnot:1:7) >*zeros
156700150303     c
156800150303    6c                   if        evbcev='MIC' and
156900150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
157000150216     c                   eval      evemic='SI'
157100150303    6c                   endif
157200150303     c
157300150303    6c                   if        evbcev='NIC' and
157400150303     c                             %int(%subst(evbnot:1:7))=wd98ndc
157500150216     c                   eval      evemic='  '
157600150303    6c                   endif
157700150303     c
157800150303   x5c                   else
157900150303    6c                   if        evbcev='MIC' and   evbdev=d98ddc
158000150303     c                   eval      evemic='SI'
158100150303    6c                   endif
158200150303    6c                   if        evbcev='NIC' and   evbdev=d98ddc
158300150303     c                   eval      evemic='  '
158400150303    6c                   endif
158500150303
158600150303    5c                   endif
158700150303    4c                   endif
158800150216     c
158900060406    4C     §2AFTC        IFEQ      'A'                                          --- 4 -->
159000930421     C     §2AFTC        OREQ      'G'
159100930421     C                   ADD       1             IE
159200990810     C                   Z-ADD     EVBDEV        EVD(IE)                        *DATA
159300930421     C                   Z-ADD     EVBHEV        EVO(IE)                        *ORA
159400930421     C                   MOVEL     EVBCEV        EVC(IE)                        *CAUSALE
159500131218     C                   MOVEL     §2aftc        EVT(IE)                        *tipoG/A
159600090928     c
159700090928     c* Memorizzo il primo evento di lasciato avviso o giacenza
159800091014     c**                 if        ie=1
159900091014     c**                 eval      Primotipo=§2aftc
160000091014     c**                 eval      Primodev=evbdev
160100091014     c**                 eval      Primohev=evbhev
160200091014     c**                 endif
160300090928     c
160400060406    4C                   END                                                    <-- 4 ---
160500060406     c* Verifico se si tratta di evento particolare
160600060406    4c                   if        §2zCEP<>' '
160700060526     c* non carico l'evento se cade in giorno festivo
160800060526     c                   z-add     evbdev        wpodata
160900060526     C                   EVAL      WTC='C'
161000060526     C                   EVAL      WIC='C'
161100060526     c                   eval      wslitt='N'
161200060526     c                   exsr      chkfes
161300060526     c
161400060526     c* Wslitt='N' non festivo
161500060526   4ac                   if        wslitt='N'
161600060526     c
161700060406    5c                   if        §2zfep='A'
161800060406     c                   eval      wevpaA='S'
161900060406    5C                   END                                                    <-- 3 ---
162000060406    5c                   if        §2zfep='G'
162100060406     c                   eval      wevpaG='S'
162200060406    5C                   END                                                    <-- 3 ---
162300060406     c                   add       1             ip
162400060406     C                   Z-ADD     eVBDEV        pVD(Ip)                        *DATA
162500060406     C                   Z-ADD     eVBHEV        pVO(Ip)                        *ORA
162600060406     C                   MOVEL     EVBCEV        pVC(Ip)                        *CAUSALE
162700060406     C                   MOVEL     §2zfep        pVF(Ip)                        *CAUSALE
162800060526   4aC                   ENDif                                                  <-- 3 ---
162900060526    4C                   END                                                    <-- 3 ---
163000060406    3C                   END                                                    <-- 3 ---
163100060406    2C                   END                                                    <-- 2 ---
163200060406     C     keyevb        READe     FNEVB01L                               99
163300060406    1C                   ENDDO                                                  <-- 1 ---
163400050523     c* controllo riempimento skiere
163500050523     c                   clear                   trul0sds
163600050523     c                   eval      i0sski='EVD/EVO/EVC'
163700050523     c                   eval      i0sele=%elem(EVD)
163800050523     c                   eval      i0spie=ie
163900050523     c                   eval      i0sfile='FNEVB01L  '
164000050523     c                   eval      i0scod='     '
164100050523     c                   eval      i0spgm='TNSD99R'
164200050523     c                   movel     trul0sds      kpjbu
164300050523     c                   call      'TRUL0SR'
164400050523     c                   parm                    kpjba
164500930420     C*
164600060406     c* controllo riempimento skiere
164700060406     c                   clear                   trul0sds
164800060406     c                   eval      i0sski='PVD/PVO/PVC'
164900060406     c                   eval      i0sele=%elem(PVD)
165000060406     c                   eval      i0spie=ie
165100060406     c                   eval      i0sfile='FNEVB01L  '
165200060406     c                   eval      i0scod='     '
165300060406     c                   eval      i0spgm='TNSD99R'
165400060406     c                   movel     trul0sds      kpjbu
165500060406     c                   call      'TRUL0SR'
165600060406     c                   parm                    kpjba
165700060406     C*
165800930420     C                   ENDSR
165900931213     C********************************************************************
166000931213     C*   CARTUC - CARICA IN SCHIERA I TURNI DI CHIUSURA
166100931213     C********************************************************************
166200931213     C     CARTUC        BEGSR
166300931213     C*
166400931214     C* SE NON VI E' NESSUN TURNO DI CHIUSURA NON CARICO LA SCHIERA
166500931213     C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
166600931214     C     D98GC2        ANDEQ     *BLANKS
166700931213     C                   ELSE                                                   <-- 1 -->
166800931213     C*
166900931213     C                   MOVE      D98GC1        TUC(1)
167000931214     C                   MOVE      D98GC2        TUC(2)
167100931213     C*
167200931213     C                   END                                                    <-- 1 ---
167300931213     C*
167400931213     C                   ENDSR
167500970304     C********************************************************************
167600970304     C*   DATRIF - CONTROLLO DATE PER LANCIO CON DATA RIFERIMENTO
167700970304     C********************************************************************
167800970304     C     DATRIF        BEGSR
167900970304     C*
168000050922     C* CONTROLLO SE LANCIO CON DATA RIFERIMENTO  per calcolo affidabilità
168100050922     c*  partenza
168200970304     C     D98DRF        IFGT      *ZEROS                                       --- 1 -->
168300970304     C*
168400970304     C* CONTROLLO SE LA DATA RIFERIMENTO E' PIU' BASSA DELLA DATA SPEDIZ.
168500070605     C***  D98DRF        IFLT      D98DSP                                       --- 2 -->
168600070605     C***                MOVEL     '1'           D98ERR
168700070605     C***                MOVEL     MSG(30)       D98MSG                         *DATA RIF. < DATA SP
168800070605     C***                ELSE                                                   <-- 2 -->
168900970304     C*
169000970304     C                   Z-ADD     D98DRF        WD1
169100970304     C                   Z-ADD     D98ORF        WO1
169200970310     C* DATA PARTENZA ULTIMO COLLO
169300970310     C     D98DRF        IFLE      D98DUC
169400050928     c     d98duc        oreq      0
169500970310     C                   Z-ADD     D98DRF        D98DUC
169600970310     C                   Z-ADD     D98ORF        W98HUC
169700070605     C                   ENDIF
169800970304     C* DATA ARRIVO TRAZIONE
169900050922     C***                Z-ADD     D98DTI        WD2
170000050922     C***                Z-ADD     D98HTI        WO2
170100050922     C***  WDO1          IFLE      WDO2
170200050922     C***                Z-ADD     D98DRF        D98DTI
170300050922     C***                Z-ADD     D98ORF        D98HTI
170400050922     C***                ENDIF
170500970304     C* DATA CONSEGNA
170600050922     C***                Z-ADD     D98DCM        WD2
170700050922     C***                Z-ADD     D98HMC        WO2
170800050922     C***  WDO1          IFLE      WDO2
170900050922     C***                Z-ADD     D98DRF        D98DCM
171000050922     C***                Z-ADD     D98ORF        D98HMC
171100050922     C***                ENDIF
171200970304     C*
171300070605     C***                ENDIF                                                  <-- 2 ---
171400970304     C                   ENDIF                                                  <-- 1 ---
171500970304     C*
171600970304     C                   ENDSR
171700940901     C********************************************************************
171800940901     C*   TDTIPO - TRASFORMA LA DATA ARRIVO DELLA TRAZIONE IN PROGRESS.ORE
171900940901     C********************************************************************
172000940901     C     TDTIPO        BEGSR
172100940901     C*
172200940901     C* PORTO TEMPO DI TRAZIONE TEORICO INTERNO IN UN CAMPO DI WORK
172300940902     C                   Z-ADD     D98TTI        W98TTI            3 0
172400941011     C*
172500941011     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
172600941011     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
172700941011     C     W98TTI        DIV       24            WDIV              3 0
172800941011     C                   MVR                     WRES              3 0
172900941011     C     WRES          IFEQ      *ZEROS
173000941011     C                   Z-ADD     *ZEROS        WASTA             9 0
173100941011     C                   ELSE
173200941011     C                   Z-ADD     12            WASTA
173300941011     C                   END
173400940901     C*
173500940901     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
173600940901     C     D98DTI        IFNE      *ZEROS
173700940901     C                   Z-ADD     D98DTI        WDAT
173800940901     C                   EXSR      DATORE
173900960911     C                   Z-ADD     WORE          WPODTI
174000960911     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
174100960911     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
174200960911     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
174300960911     C                   Z-ADD     WPODTI        W10POI            9 0
174400960911     C                   EXSR      R10FES
174500960911     C                   Z-ADD     W10POO        WPODTI
174600960911     C* AGGIUNGO LE ORA D'ASTA
174700960911     C                   ADD       WASTA         WPODTI
174800960911     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
174900960911     C     W10POI        IFEQ      W10POO
175000940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
175100940901     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
175200940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
175300940901     C                   EXSR      RO3ORE
175400940901     C                   ADD       WR3HHO        WPODTI
175500940901     C                   ADD       WR3HHO        W98TTI
175600940901     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
175700940901     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
175800940901     C                   Z-ADD     D98HTI        WR3HHI            4 0
175900940901     C                   EXSR      RO3ORE
176000940901     C                   ADD       WR3HHO        WPODTI
176100940901     C                   ADD       WR3HHO        W98TTI
176200960911     C                   ENDIF                                                  *SE NON FESTIVO
176300940901%%%  C                   ELSE                                                   *SE D98DTI = 0
176400950327%%%  C***********D98DAM    IFNE 0
176500950327%%%  C*********************Z-ADDD98DAM    WDAT             *DATA ARR.FIL.ARR.
176600950327%%%  C*********************EXSR DATORE
176700950327%%%  C***********WASTA     ADD  WORE      WPODTI
176800950327%%%  C*********************ENDIF
176900940901     C                   END
177000940901     C*
177100940901     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
177200940901     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
177300940901     C     D98TTI        IFNE      W98TTI                                       --- 1 -->
177400940901     C     W98TTI        DIV       24            WDIV              3 0
177500940901     C                   MVR                     WRES              3 0
177600940901     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
177700941228     C                   Z-ADD     W990IN        D98TCI
177800940901     C                   ELSE                                                   <-- 2 -->
177900941228     C                   Z-ADD     W991IN        D98TCI
178000940908     C                   ENDIF                                                  <-- 2 ---
178100090827     c* Sistemo i tempi di consegna se non sono multipli di 12
178200090827     c                   exsr      SisCONS
178300090827     c*
178400940908     C* RICALCOLO TEMPO DI RESA INTERNO
178500950112     C                   Z-ADD     D98TPI        D98TRI                         *PARTENZA INTERNO ->
178600950112     C                   ADD       D98TTI        D98TRI                         *TRAZIONE INTERNO ->
178700090921     c                   z-add     d98tri        w99tri
178800090921     c* per la DS
178900090921     C                   ADD       D98TCI        D98TRI                         *CONSEGNA INTERNO ->
179000090921     c* per i calcoli interni
179100090921     C                   ADD       w99TCI        w99TRI                         *CONSEGNA INTERNO ->
179200940908     C                   ENDIF                                                  <-- 1 ---
179300940901     C*
179400940901     C                   ENDSR
179500930325     C********************************************************************
179600930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
179700930325     C********************************************************************
179800930325     C     TDATPO        BEGSR
179900930406     C*
180000930420     C* TRASFORMO LA DATA ODIERNA
180100930416     C                   Z-ADD     WDAODI        WDAT
180200930416     C                   EXSR      DATORE
180300050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
180400960911     C                   MOVEL     'OSC  '       WR3TAB
180500960911     C                   Z-ADD     WHHODI        WR3HHI
180600960911     C                   EXSR      RO3ORE
180700050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
180800050922     c                   if        wr3hho=12
180900050922     c     wpoatt        add       12            wpoattdata24
181000050922     c                   else
181100050922     c                   z-add     wpoatt        wpoattdata24
181200050922     c                   endif
181300930325     C*
181400930420     C* TRASFORMO LA DATA DI SPEDIZIONE
181500931125     C     D98DSP        IFEQ      *ZEROS
181600930326     C                   MOVEL     '1'           D98ERR
181700930326     C                   MOVEL     MSG(2)        D98MSG
181800930326     C                   ELSE
181900931125     C                   Z-ADD     D98DSP        WDAT
182000930325     C                   EXSR      DATORE
182100930406     C                   Z-ADD     WORE          WPODSP
182200930326     C                   END
182300930414     C*
182400930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
182500930414     C                   MOVEL     'LIM  '       WR3TAB
182600930414     C                   Z-ADD     *ZEROS        WR3HHI
182700930414     C                   EXSR      RO3ORE
182800931222     C     WR3HHO        MULT      10            WTM060            6 0
182900931222     C     WPODSP        ADD       WTM060        WPOLIM
183000140822     c
183100930325     C*
183200930420     C* TRASFORMO LA DATA DI CONSEGNA
183300931125     C     D98DCM        IFNE      *ZEROS
183400931125     C                   Z-ADD     D98DCM        WDAT
183500930325     C                   EXSR      DATORE
183600930326     C                   Z-ADD     WORE          WPODCM
183700930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
183800930326     C                   MOVEL     'OSC  '       WR3TAB
183900931125     C                   Z-ADD     D98HMC        WR3HHI
184000930326     C                   EXSR      RO3ORE
184100930326     C                   ADD       WR3HHO        WPODCM
184200930326     C                   END
184300941228     C*
184400941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
184500941228     C     D98DUC        IFNE      *ZEROS
184600941228     C                   Z-ADD     D98DUC        WDAT
184700941228     C                   EXSR      DATORE
184800941228     C                   Z-ADD     WORE          WPODUC
184900970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
185000970310     C                   MOVEL     'OSP  '       WR3TAB
185100970310     C                   Z-ADD     W98HUC        WR3HHI
185200970310     C                   EXSR      RO3ORE
185300970310     C     D98DRF        IFNE      *ZEROS
185400050928     c     w98huc        andgt     1200
185500970310     C                   SUB       12            WR3HHO
185600970310     C                   ENDIF
185700970310     C                   ADD       WR3HHO        WPODUC
185800941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
185900070605     C***  WPODSP        IFGT      WPODUC
186000070605     C***                Z-ADD     WPODSP        WPODUC
186100070605     C***                ENDIF
186200941228     C                   ENDIF
186300960402     C*
186400141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
186500970304     C     D98DBR        IFNE      *ZEROS
186600970304     C                   Z-ADD     D98DBR        WDAT
186700960402     C                   EXSR      DATORE
186800960402     C                   Z-ADD     WORE          WPODBR
186900070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
187000960402     C                   ELSE
187100960402     C                   Z-ADD     WPODSP        WPODBR
187200960402     C                   ENDIF
187300930913     C*
187400960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
187500930913     C* CONSEGNABILITA'
187600960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
187700950407     C*
187800930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
187900931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
188000931125     C                   Z-ADD     D98DIC        WDAT
188100930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
188200930913     C                   Z-ADD     WORE          WPODIC
188300930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
188400931125     C                   Z-ADD     D98OIC        WR3HHI
188500930913     C                   EXSR      RO3ORE
188600930913     C                   ADD       WR3HHO        WPODIC
188700931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
188800950407     C*
188900930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
189000931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
189100931125     C                   Z-ADD     D98DFC        WDAT
189200930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
189300930913     C                   Z-ADD     WORE          WPODFC
189400140822     C                   Z-ADD     WORE          WPODFC_00
189500930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
189600931125     C                   Z-ADD     D98OFC        WR3HHI
189700930913     C                   EXSR      RO3ORE
189800930913     C                   ADD       WR3HHO        WPODFC
189900931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
190000930913     C*
190100930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
190200930913     C     D98ERR        IFEQ      '0'
190300930913     C     WPODFC        IFEQ      *ZEROS
190400930913     C     WPODIC        ANDNE     *ZEROS
190500930913     C                   MOVEL     '1'           D98ERR
190600930913     C                   MOVEL     MSG(17)       D98MSG
190700930913     C                   ENDIF
190800930913     C                   ENDIF
190900930907     C*
191000930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
191100930913     C     D98ERR        IFEQ      '0'
191200930913     C     WPODIC        IFGT      WPODFC
191300930907     C                   MOVEL     '1'           D98ERR
191400930913     C                   MOVEL     MSG(18)       D98MSG
191500930913     C                   ENDIF
191600930913     C                   ENDIF
191700950407     C*
191800950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
191900930511     C*
192000930511     C                   ENDSR
192100950110     C********************************************************************
192200950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
192300950110     C********************************************************************
192400950110     C     CALDPP        BEGSR
192500950110     C*
192600950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
192700950110     C                   MOVEL     'P'           WTC                            *PARTENZA
192800950110     C                   MOVEL     'I'           WIC                            *INTERNA
192900960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
193000950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
193100950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
193200950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
193300950110     C*
193400950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
193500950110     C                   MOVEL     'P'           WTC                            *PARTENZA
193600950110     C                   MOVEL     'C'           WIC                            *CLIENTE
193700950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
193800950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
193900950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
194000950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
194100960229     C*
194200960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
194300050922     c
194400050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
194500050922     c
194600050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
194700090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
194800050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
194900090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
195000050922%%%  C*  ritardo alla partenza.
195100050922     c* Il tutto solo se la data di oggi > della
195200050922     c*  data teorica di partenza determinata
195300050922     c*
195400950110%%%  C     D98ERR        IFEQ      '0'
195500950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
195600950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
195700950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
195800050922     c     wpoattData24  andgt     wpoppi
195900960911%%%  C********             MOVEL'OSC  '   WR3TAB
196000960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
196100960911%%%  C********             EXSR RO3ORE
196200960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
196300050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
196400090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
196500090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
196600950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
196700050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
196800050922     c* La nuova data calcolata non può essere < della minima prevista
196900050922     c                   if        wr0poo>wpoppi
197000050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
197100050922     c                   endif
197200050922     c
197300950110%%%  C                   ENDIF
197400950110%%%  C                   ENDIF
197500950110%%%  C*
197600950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
197700050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
197800050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
197900050915%%%  C*  alla prtenza.
198000950110%%%  C     D98ERR        IFEQ      '0'
198100950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
198200950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
198300950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
198400950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
198500950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
198600950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
198700050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
198800950110%%%  C                   ENDIF
198900950110%%%  C                   ENDIF
199000950110%%%  C*
199100950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
199200950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
199300950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
199400950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
199500950110%%%  C     D98ERR        IFEQ      '0'
199600950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
199700950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
199800950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
199900950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
200000090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
200100090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
200200950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
200300050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
200400950110%%%  C                   ENDIF
200500950110%%%  C                   ENDIF
200600960229     C*
200700960229     C                   ENDIF                                                  <-- 1 ---
200800950110     C*
200900950110     C                   ENDSR
201000930604     C********************************************************************
201100930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
201200930604     C********************************************************************
201300930604     C     CALDPT        BEGSR
201400090902     c                   clear                   RicalcoloTRAZ     1
201500930604     C*
201600930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
201700931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
201800931129     C                   MOVEL     'I'           WIC                            *INTERNA
201900050915     c* se non presente la data partenza ultimo collo, uso la teorica
202000950110     C     D98DUC        IFEQ      *ZEROS
202100950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
202200950110     C                   ELSE
202300950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
202400950110     C                   ENDIF
202500930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
202600930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
202700930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
202800930604     C*
202900930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
203000931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
203100931129     C                   MOVEL     'C'           WIC                            *CLIENTE
203200950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
203300930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
203400930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
203500930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
203600150508
203700150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
203800150508     c* se presente la data partenza PRIMO  collo --> la uso
203900150508     c                   if        w98dpc=0
204000150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
204100150508     c                   else
204200150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
204300150508     C                   MOVEL     'C'           WIC                            *INTERNA
204400150508     C                   Z-ADD     W98DPC        WDAT
204500150508     C                   EXSR      DATORE
204600150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
204700150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
204800150508     C                   EXSR      RO4CDP
204900150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
205000150508     c                   endif
205100960229     C*
205200960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
205300940624%%%  C*
205400050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
205500050915
205600940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
205700050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
205800050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
205900050915     c*   attribuito alla trazione
206000940624%%%  C     D98ERR        IFEQ      '0'
206100940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
206200940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
206300090903     c     wpoatt        andge     wpopti
206400050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
206500090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
206600090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
206700940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
206800050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
206900050922     c                   if        wr0poo>wpopti
207000050922     c                   eval      wpopti=wr0poo
207100050922%%%  C                   endif
207200090902     c*
207300090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
207400090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
207500090902     c*  della trazione udate e utime
207600090902     c                   eval      RicalcoloTRAZ='S'
207700090902     c
207800940624%%%  C                   ENDIF
207900940624%%%  C                   ENDIF
208000940628%%%  C*
208100050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
208200050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
208300050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
208400940628%%%  C     D98ERR        IFEQ      '0'
208500940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
208600940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
208700940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
208800090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
208900090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
209000940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
209100050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
209200940628%%%  C                   ENDIF
209300940628%%%  C                   ENDIF
209400960229     C*
209500960229     C                   ENDIF                                                  <-- 1 ---
209600930604     C*
209700930604     C                   ENDSR
209800930511     C********************************************************************
209900930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
210000930511     C********************************************************************
210100930511     C     CALDCR        BEGSR
210200050914     c                   clear                   consideraora
210300100308     c
210400100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
210500100308     c*
210600100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
210700100308     c* 1) se presente solo ORA --> ignoro sempre
210800100308     c* 2) se presente data e ora IL --> ignoro l'ora
210900100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
211000100309     c***                if        d98tsp='E'
211100100309     c***                clear                   d98dcr
211200100309     c***                clear                   d98hcr
211300100309     c**+                clear                   d98tcr
211400100309     c***                endif
211500100308     c
211600100308     c                   if        d98hcr>0
211700100308     c
211800100308     c                   if        d98dcr=0
211900100308     c                   clear                   d98hcr
212000100308     c                   endif
212100100308
212200100308     c                   if        d98dcr>0 and d98tcr=' '
212300100308     c                   clear                   d98hcr
212400100308     c                   endif
212500100308     c                   endif
212600941228     C*
212700941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
212800941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
212900050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
213000050901     c*  (per uguale si intende entrambe nel mattino o entrambe
213100050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
213200050901     c*  (EDPES 1/9/05)
213300050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
213400050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
213500050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
213600050831     C***                Z-ADD     D98DCM        D98DCR
213700050831     C***                ENDIF
213800050901     c*
213900070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
214000050914     c
214100100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
214200100308     c***                clear                   d98hcr
214300100308     c***                clear                   d98tcr
214400100308     c***                endif
214500050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
214600050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
214700070521     c*  richiesta dal cliente
214800100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
214900100308     C***                MOVEL     'OSC  '       WR3TAB
215000100308     C***                Z-ADD     D98HMC        WR3HHI
215100100308     C***                EXSR      RO3ORE
215200100308     c***                z-add     wr3hho        salvaoreHMC
215300050901     c
215400100308     C***                Z-ADD     D98Hcr        WR3HHI
215500100308     C***                EXSR      RO3ORE
215600100308     c***                z-add     wr3hho        salvaoreHCR
215700100308     c***
215800050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
215900100308    2c***                if        salvaoreHMC=salvaoreHCR
216000100308     C***                eval      Consideraora='S'
216100100308     c***                else
216200100308     C***                eval      Consideraora=' '
216300100308     c***                clear                   d98hcr
216400100308    2c***                endif
216500100308    1c***                endif
216600930511     C*
216700930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
216800931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
216900931125     C                   Z-ADD     D98DCR        WDAT
217000930325     C                   EXSR      DATORE
217100930326     C                   Z-ADD     WORE          WPODCR
217200930420     C*
217300930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
217400930420     C*
217500931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
217600930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
217700930326     C                   MOVEL     'CRD  '       WR3TAB
217800931125     C                   Z-ADD     D98HCR        WR3HHI
217900930419     C                   EXSR      RO3ORE
218000930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
218100940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
218200050902     c
218300931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
218400930420     C                   MOVEL     'CRD1 '       WR3TAB
218500930420     C                   Z-ADD     *ZEROS        WR3HHI
218600930420     C                   EXSR      RO3ORE
218700931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
218800090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
218900090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
219000050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
219100060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
219200050907     c                   z-add     worecr        oreILoDOPO
219300931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
219400931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
219500931207     C                   MOVEL     'C'           WIC                            *CLIENTE
219600931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
219700150330     C                   Z-ADD     WORECR        WR4NOI
219800931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
219900931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
220000931209     C     WPOCRF        SUB       12            WR7POI            9 0
220100931209     C                   MOVEL     'X'           WIC
220200931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
220300931209     C                   ADD       12            WR7POO
220400931207     C     WR7POO        IFGT      WPOCRF
220500931207     C                   Z-ADD     WR7POO        WPOCRF
220600931207     C                   END
220700150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
220800150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
220900150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
221000150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
221100150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
221200150330     C                   MOVEL     'C'           WIC                            *CLIENTE
221300150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
221400150330     C                   Z-ADD     w99TCC        WR4NOI
221500150330     c                   sub       12            wr4noi
221600150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
221700150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
221800150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
221900150330     c
222000150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
222100150330     C                   MOVEL     'X'           WIC
222200150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
222300150330     C******             ADD       12            WR7POO
222400150330     C     WR7POO        IFGT      WPOCRF_dee
222500150330     C                   Z-ADD     WR7POO        WPOCRF_dee
222600150330     C                   END
222700050902     c
222800050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
222900050902     c*  sono le ore di fine della non consegnabilità
223000050905     c* Se aggiungendo 1 finisco in giorno festivo
223100050905     c*  cerco primo giorno non festivo
223200050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
223300050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
223400050907     C****               MOVEL     'C'           WIC                            *CLIENTE
223500050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
223600050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
223700050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
223800050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
223900050907     c
224000050907     c                   add       1             wpocri
224100050902     c
224200050902   FFC                   Z-ADD     WPOCRI        WPOCRC
224300050902     c
224400930325     C                   END
224500150330     C*
224600931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
224700930420     C* DATA SPED. = DATA INIZIO RANGE
224800930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
224900940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
225000940324£$   C                   Z-ADD     WPOCRI        WPOCRC
225100930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
225200930420     C                   MOVEL     'CRP  '       WR3TAB
225300931125     C                   Z-ADD     D98HCR        WR3HHI
225400930420     C                   EXSR      RO3ORE
225500931207     C     WPODCR        ADD       WR3HHO        WPOCRF
225600931207     C                   SUB       24            WPOCRF                         *FINE RANGE
225700931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
225800931207     C* ANDANDO INDIETRO NEL TEMPO
225900050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
226000050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
226100050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
226200050907     c* data utile venedì sera
226300130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
226400130411     C                   MOVEL     'X'           WIC                            *CLIENTE
226500931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
226600931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
226700931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
226800930326     C                   END
226900930526     C*
227000931125     C     D98TCR        IFEQ      'P'
227100931125     C     D98TCR        OREQ      'D'
227200930526     C                   ELSE                                                   *CONS.RICH.IL ..
227300940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
227400940324£$   C                   Z-ADD     WPODCR        WPOCRC
227500930526     C* DATA SPED. = DATA INIZIO RANGE
227600050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
227700050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
227800050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
227900930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
228000930526     C                   MOVEL     'CRI  '       WR3TAB
228100931125     C                   Z-ADD     D98HCR        WR3HHI
228200930526     C                   EXSR      RO3ORE
228300050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
228400050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
228500050907     c                   z-add     wr3hho        oreILoDOPO
228600930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
228700050902     c
228800050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
228900050902     c                   if        wr3MATPOM='P' and d98hcr>0
229000050902     c                   add       12            wpocri
229100100308     c* tolgo 12 ore dalle ore conseganbili
229200050907     c                   sub       12            oreILoDOPO
229300050902     c                   endif
229400050902     c
229500931209     C     WPOCRF        SUB       12            WR7POI            9 0
229600931209     C                   MOVEL     'X'           WIC
229700931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
229800931209     C                   ADD       12            WR7POO
229900931209     C     WR7POO        IFGT      WPOCRF
230000931209     C                   Z-ADD     WR7POO        WPOCRF
230100931209     C                   END
230200050905     c
230300050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
230400050905     c*  sono le ore di fine della non consegnabilità
230500050905     c* Se aggiungendo 1 finisco in giorno festivo
230600050905     c*  cerco primo giorno non festivo
230700050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
230800050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
230900050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
231000050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
231100050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
231200050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
231300050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
231400050907     c
231500050907     c                   add       1             wpocri
231600050905     c
231700050905   FFC                   Z-ADD     WPOCRI        WPOCRC
231800050905     c
231900930526     C                   END
232000931206     C*
232100931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
232200931206     C* LA IMPOSTO COME LA CONS.TEORICA
232300140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
232400140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
232500931206     C                   ELSE
232600931206     C                   Z-ADD     WPOCRF        WPOCR1
232700931206     C                   END
232800140825     C     WPOCRF        IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
232900140825     C                   Z-ADD     WPODEE_dcr    WPOCR2                         *->FINE CONS.RICH.CL
233000931206     C                   ELSE
233100931206     C                   Z-ADD     WPOCRF        WPOCR2
233200931206     C                   END
233300150331     c
233400150331     c                   if        wpocrf_dee>0
233500150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
233600150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
233700150330     C                   END
233800150331     C                   END
233900150331     C*
234000150331     c                   if        wpocrf_dee=0
234100150508     C     wpocr2        sub       24            WPOCRF_dee
234200150331     c                   endif
234300150331     c
234400930526     C                   END                                                    <-- 1 ---
234500930511     C*
234600930511     C                   ENDSR
234700930511     C********************************************************************
234800930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
234900930511     C********************************************************************
235000930511     C     CALLAV        BEGSR
235100930511     C*
235200941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
235300930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
235400930421     C*
235500930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
235600930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
235700930421     C     1             DO        IE            E                 3 0          --- 2 -->
235800930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
235900930421     C                   Z-ADD     1             I
236000930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
236100060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
236200930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
236300930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
236400930421     C                   ADD       1             IL
236500930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
236600930421     C                   EXSR      DATORE
236700930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
236800091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
236900091014     c* Mi salvo la posizione dell'evento
237000091014     C                   Z-ADD     E             LAp(IL)
237100930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
237200930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
237300930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
237400930421     C                   EXSR      RO3ORE
237500930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
237600930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
237700930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
237800930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
237900091015     c**                 if        §2agla>1
238000091015     c**                 sub       1             §2agla
238100930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
238200091015     c**                 else
238300091015     c**                 clear                   whh
238400091015     c**                 endif
238500930510     C                   ADD       WHH           WORELA
238600940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
238700930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
238800090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
238900150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
239000930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
239100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
239200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
239300930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
239400930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
239500930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
239600930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
239700931209     C                   Z-ADD     LAF(IL)       WR7POI
239800931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
239900931202     C     WR7POO        IFGT      LAF(IL)
240000931201     C                   Z-ADD     WR7POO        LAF(IL)
240100931202     C                   END
240200930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
240300930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
240400091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
240500930510     C                   END                                                    <-- 3 ---
240600931223     C                   ENDDO                                                  <-- 2 ---
240700930421     C*
240800930421     C                   END                                                    <-- 1 ---
240900930618££$$ C*
241000930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
241100931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
241200941228££$$ C     1             DO        IL            I
241300930618££$$ C     LAF(I)        IFGT      WPOLAF
241400930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
241500930618££$$ C                   END
241600931223££$$ C                   ENDDO
241700930511     C*
241800930511     C                   ENDSR
241900940324£$   C********************************************************************
242000940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
242100940324£$   C********************************************************************
242200940324£$   C     CALLAC        BEGSR
242300941229     C*
242400950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
242500950407     C*
242600950210     C                   Z-ADD     WORELA        WTM060
242700941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
242800941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
242900941229     C     D98DAG        IFEQ      *ZEROS
243000050520     c     §2aftg        andeq     'S'
243100090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
243200090827     C     WORELA        ADD       w99TCC        WTM060            6 0
243300941229     C                   ENDIF
243400941229     C*
243500940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
243600940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
243700941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
243800941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
243900941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
244000940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
244100941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
244200050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
244300050520     c                   movel     §2aftg        LCON(IL)
244400950407     C*
244500950407     C                   ENDIF
244600960129     C*
244700940324£$   C                   ENDSR
244800930511     C********************************************************************
244900930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
245000930511     C********************************************************************
245100930511     C     CALGIA        BEGSR
245200960129     C*
245300960129     C*-------------------------------------------------------------------
245400960129     C* EVENTI GIACENZA
245500960129     C*-------------------------------------------------------------------
245600960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
245700960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
245800960129     C*
245900050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
246000041116     c     d98nsp        ifgt      0
246100031125     C                   EXSR      CARGIAC
246200041116     c                   endif
246300031125     C*
246400960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
246500960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
246600960129     C     1             DO        IE            E                 3 0          --- 2 -->
246700960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
246800960129     C                   Z-ADD     1             I
246900960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
247000060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
247100960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
247200960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
247300960129     C                   ADD       1             IG
247400960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
247500960129     C                   EXSR      DATORE
247600960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
247700140606     c* Mi salvo la posizione dell'evento
247800140606     C                   Z-ADD     E             gip(IG)
247900960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
248000960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
248100960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
248200960129     C                   EXSR      RO3ORE
248300960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
248400960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
248500960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
248600960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
248700960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
248800960129     C                   ADD       WHH           WOREGI
248900960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
249000960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
249100090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
249200090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
249300960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
249400960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
249500960129     C                   MOVEL     'C'           WIC                            *CLIENTE
249600960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
249700960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
249800960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
249900960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
250000960129     C                   Z-ADD     GIF(IG)       WR7POI
250100960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
250200031125     C     WR7POO        IFGT      GIF(IG)
250300031125     C                   Z-ADD     WR7POO        GIF(IG)
250400031125     C                   ENDIF
250500031201     C*
250600031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
250700031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
250800031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
250900031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
251000031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
251100031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
251200031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
251300031201     C                   IF        WPOATT > GIF(IG)
251400031201     C*
251500031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
251600031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
251700031201     C                   IF        WGGA = *BLANKS
251800031201     C                   SETOFF                                       54
251900031201     C                   ELSE
252000031201     C     WGGA          LOOKUP    SK7QW                                  54
252100031201     C                   ENDIF
252200031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
252300031201     C                   MOVEL     '1'           FLGGIACOK
252400031201     C                   ELSE
252500140127     c
252600031125     C*
252700031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
252800031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
252900031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
253000031125     C                   MOVEL     '0'           FLGGIACOK         1
253100031125     C*
253200031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
253300031125     C     1             DO        IAG           EG                3 0
253400031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
253500031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
253600031125     C                   EXSR      DATORE
253700031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
253800031125     C*
253900031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
254000031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
254100031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
254200031125     C                   EXSR      DATORE
254300031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
254400031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
254500031229     C* => NN LO CONSIDERO
254600031229     C                   IF        EVC(E+1) = 'G' AND
254700031229     C                             WPROXGII = WGCPDORE
254800031229     C                   MOVEL     '1'           FLGGIACOK
254900031229     C                   ENDIF
255000031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
255100031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
255200031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
255300031125     C                   EXSR      RO3ORE
255400031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
255500031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
255600031125     C* DELL'EVENTO GIACENZA CORRENTE
255700031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
255800060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
255900031125     C                   MOVEL     '1'           FLGGIACOK
256000031125     C                   LEAVE
256100031125     C                   ENDIF
256200031125     C                   ELSE
256300031229     C                   IF        EVC(E)  = 'G' AND
256400031229     C                             GII(IG) = WGCPDORE
256500031229     C                   MOVEL     '1'           FLGGIACOK
256600031229     C                   LEAVE
256700031229     C                   ENDIF
256800031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
256900031125     C                   MOVEL     '1'           FLGGIACOK
257000031125     C                   LEAVE
257100031125     C                   ENDIF
257200031125     C                   ENDIF
257300031125     C                   ENDDO
257400031201     C                   ENDIF
257500140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
257600140127     C                   IF        FLGGIACOK = '0'  and
257700140127     c                             d98dcm=evd(E)
257800140127     C                   MOVEL     '1'           FLGGIACOK
257900140127     c                   endif
258000031125     C*
258100031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
258200031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
258300031125     C* DELL'EVENTO GIACENZA CORRENTE
258400031125     C                   IF        FLGGIACOK = '0'
258500031125     C                   Z-ADD     *ZEROS        GIF(IG)
258600031125     C                   ELSE
258700031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
258800031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
258900031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
259000031125     C                   ENDIF
259100031125     C                   ELSE
259200031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
259300031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
259400031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
259500031125     C                   ENDIF
259600031125     C*
259700960129     C                   ENDIF                                                  <-- 3 ---
259800960129     C                   ENDDO                                                  <-- 2 ---
259900960129     C*
260000960129     C                   ENDIF                                                  <-- 1 ---
260100960129££$$ C*
260200960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
260300960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
260400960129££$$ C     1             DO        IG            I
260500960129££$$ C     GIF(I)        IFGT      WPOGIF
260600960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
260700960129££$$ C                   ENDIF
260800960129££$$ C                   ENDDO
260900941229     C*
261000941229     C*-------------------------------------------------------------------
261100941229     C* APERTURA GIACENZA
261200941229     C*-------------------------------------------------------------------
261300960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
261400960129     C*
261500960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
261600931125     C                   Z-ADD     D98DAG        WDAT
261700930325     C                   EXSR      DATORE
261800950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
261900960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
262000950413£££  C                   Z-ADD     WORE          WPODAG
262100960129$$£  C*********************ENDIF
262200930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
262300930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
262400930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
262500930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
262600930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
262700941229     C*
262800941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
262900941229     C                   Z-ADD     WPODAG        WPOAGC
263000960129     C*
263100960129     C                   ENDIF                                                  <-- 1 ---
263200941229     C*
263300941229     C*-------------------------------------------------------------------
263400930511     C* CHIUSURA GIACENZA
263500941229     C*-------------------------------------------------------------------
263600930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
263700930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
263800931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
263900931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
264000931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
264100931201     C                   Z-ADD     D98DCM        W98DCG
264200930416     C                   ELSE
264300930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
264400930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
264500931201     C                   Z-ADD     WDAODI        W98DCG
264600061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
264700061123     c*  chiusura giacenza, come già fa in altri posti
264800061123     c                   if        d98dfc>0
264900061123     C                   Z-ADD     d98dfc        W98DCG
265000061123     c                   endif
265100061123     c
265200930409     C                   END
265300930409     C                   END
265400930409     C                   END
265500930414     C* TRASF.DATA CHIUSURA
265600931201     C     W98DCG        IFNE      *ZEROS
265700931201     C                   Z-ADD     W98DCG        WDAT
265800930325     C                   EXSR      DATORE
265900930326     C                   Z-ADD     WORE          WPODCG
266000930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
266100930407     C* GIORNO DA ESCLUDERE
266200150216     C                   ADD       12            WPODCG
266300150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
266400150123     c*  il giorno di chiusura dovrebe già essere protetto
266500941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
266600941229     C     WPOCGC        IFEQ      *ZEROS
266700941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
266800941229     C                   ENDIF
266900930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
267000930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
267100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
267200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
267300150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
267400150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
267500930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
267600930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
267700930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
267800931209     C                   Z-ADD     WPODCG        WR7POI
267900931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
268000931202     C     WR7POO        IFGT      WPODCG
268100931201     C                   Z-ADD     WR7POO        WPODCG
268200930326     C                   END
268300931202     C                   END
268400930511     C*
268500930511     C                   ENDSR
268600031125     C********************************************************************
268700031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
268800031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
268900031125     C*             DEGLI EVENTI DI TIPO GIACENZA
269000031125     C********************************************************************
269100031125     C     CARGIAC       BEGSR
269200031125     C*
269300031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
269400031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
269500031125     C*
269600050405     C                   Z-ADD     D98AAS        KWGCPAAS
269700050405     C                   Z-ADD     D98LNP        KWGCPLNP
269800050405     C                   Z-ADD     D98NRS        KWGCPNRS
269900050405     C                   Z-ADD     D98NSP        KWGCPNSP
270000031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
270100050405     C     KEYGC_P       SETLL     TIGCP01L
270200050405     C                   IF        %equal(TIGCP01L)
270300050405     C     KEYGC_P       READE     TIGCP01L
270400050405     C                   DOW       not %eof(TIGCP01L)
270500031125     C                   ADD       1             IAG
270600031125     C                   EVAL      GCPD(IAG) = GCPDUR
270700050405     C     KEYGC_P       READE     TIGCP01L
270800031125     C                   ENDDO
270900031125     C                   ENDIF
271000031125     C                   ELSE
271100050405     C     KEYGC_P       SETLL     TIGCP21L
271200050405     C                   IF        %equal(TIGCP21L)
271300050405     C     KEYGC_P       READE     TIGCP21L
271400050405     C                   DOW       not %eof(TIGCP21L)
271500031125     C                   ADD       1             IAG
271600031125     C                   EVAL      GCPD(IAG) = GCPDUR
271700050405     C     KEYGC_P       READE     TIGCP21L
271800031125     C                   ENDDO
271900031125     C                   ENDIF
272000031125     C                   ENDIF
272100031125     C*
272200031125     C                   ENDSR
272300941229     C********************************************************************
272400960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
272500941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
272600941229     C********************************************************************
272700960129     C     CALGIC        BEGSR
272800941229     C*
272900960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
273000960129     C*
273100960129     C                   Z-ADD     WOREGI        WTM060
273200960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
273300960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
273400960129     C     D98DAG        IFEQ      *ZEROS
273500960129$$£  C     WOREGI        ADD       12            WTM060            6 0
273600960129     C                   ENDIF
273700960129     C*
273800960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
273900960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
274000960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
274100960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
274200960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
274300960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
274400960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
274500960129     C*
274600960129     C                   ENDIF
274700941229     C*
274800941229     C                   ENDSR
274900060418     C********************************************************************
275000060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
275100060418     c*            l'affidabilità
275200060418     C********************************************************************
275300060418     C     CALCEPA       BEGSR
275400060418     C*
275500060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
275600060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
275700060420     c                   clear                   numggcepa         3 0
275800060418     C*
275900060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
276000060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
276100060420     c                   eval      wgiroepa='0'
276200060418     c                   eval      wfine=' '
276300060418     c
276400060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
276500060420    2C     W             DOwle     IP                                           --- 2 -->
276600060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
276700060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
276800060418     c
276900060420    4c                   if        wgiroepa='0'
277000060420     c                   exsr      ElaInizCEPA
277100060418   x4c                   else
277200060420    5c                   if        pvd(W)>wdat
277300060418     c                   eval      wfine='S'
277400060418    5c                   endif
277500060418    4c                   endif
277600060418     c
277700060420    4c                   if        wfine=' ' and pvd(W)=wdat
277800060420     C*
277900060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
278000060420     c*  finale
278100060420     c                   add       1             numggcepa
278200060420     c
278300060420     C                   z-add     24            WORELA
278400060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
278500060418     c
278600060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
278700060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
278800060418     C                   MOVEL     'C'           WIC                            *CLIENTE
278900060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
279000060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
279100060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
279200060418     c
279300060418     c* Trasformo le ore in data e verifico se c'e' altro evento
279400060418     c*  in sta data che allunga ancora
279500060420     c                   z-add     wr4poo        wore
279600060418     c                   EXSR      OREDAT
279700060418    4c                   endif
279800060418     c
279900060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
280000060418    4c                   if        wfine='S'
280100060418     c                   EXSR      ELAFINECEPA
280200060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
280300060420     c*  inizio range
280400060420     c                   else
280500060420     c                   add       1             w
280600060418    4C                   ENDif                                                  <-- 3 ---
280700060420     c
280800060420     c                   else
280900060420     c                   add       1             w
281000060418    3C                   ENDif                                                  <-- 3 ---
281100060418    2C                   ENDDO                                                  <-- 2 ---
281200060418     c
281300060420     c                   if        wgiroepa='1'
281400060418     c                   EXSR      ELAFINECEPA
281500060418     c                   endif
281600060418     C*
281700060418    1C                   END                                                    <-- 1 ---
281800060418££$$ C*
281900060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
282000060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
282100060420££$$ C     1             DO        Ipa           w
282200060420££$$ C     paif(w)       IFGT      WPOpaif
282300060420££$$ C                   Z-ADD     paif(w)       WPOpaif
282400060418££$$ C                   END
282500060418££$$ C                   ENDDO
282600060418     C*
282700060418     C                   ENDSR
282800060420     C********************************************************************
282900060420     c* Elabora Inizio range eventi particolari che allungano
283000060420     C********************************************************************
283100060420     c     ElaInizCEPA   BEGSR
283200060420     C                   ADD       1             IPA
283300060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
283400060420     C                   EXSR      DATORE
283500060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
283600061017     c
283700060420     c                   eval      wgiroepa='1'
283800060420     c                   eval      wdat=pvd(w)
283900060420     C                   ENDSR
284000060418     C********************************************************************
284100060418     c* Elabora fine range eventi particolari che allungano
284200060418     C********************************************************************
284300060418     c     ElafineCEPA   BEGSR
284400060420     c     numggcepa     mult      24            worela
284500060420     c                   sub       12            worela
284600090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
284700090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
284800060418     c
284900060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
285000060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
285100060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
285200060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
285300060418     C                   Z-ADD     paif(IPA)     WR7POI
285400060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
285500060418    5C     WR7POO        IFGT      paif(IPA)
285600060418     C                   Z-ADD     WR7POO        paif(IPA)
285700060418    5C                   END
285800061017     c*Sommo 24 ore di inizio giornata
285900061017     c*            : non ho capito perchè ma ci vogliono
286000061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
286100090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
286200090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
286300061017     c* Se tempi di consegna inferiori alle 24 ore , devo
286400061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
286500061017     c*  partire prima
286600061017     c****               if        d98tcc<24
286700061017     c****               sub       12            paii(ipa)
286800061017     c****               endif
286900060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
287000060418     c* Imposto nuovo giro
287100060420     c                   eval      wgiroepa='0'
287200060418     c                   eval      wfine=' '
287300060420     c                   clear                   numggcepa         3 0
287400060418     c                   ENDSR
287500930511     C********************************************************************
287600930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
287700930511     C********************************************************************
287800010822     C     CALASS        BEGSR
287900010821     C*
288000011211     C                   EXSR      REPMAXCSB
288100930511     C*
288200930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
288300010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
288400011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
288500930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
288600930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
288700930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
288800930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
288900930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
289000930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
289100930511     C                   ELSE                                                   <-- 4 -->
2892009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
289300930512     C                   END                                                    <-- 4 ---
289400930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
289500930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
289600930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
289700930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
289800930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
289900930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
290000930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
290100930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
290200930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
290300930413     C                   END                                                    <-- 2 ---
290400011211     C                   END                                                    <-- 1 ---
290500930406     C*
290600930325     C                   ENDSR
290700930608     C********************************************************************
290800930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
290900930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2910009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
291100930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
291200930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
291300930608     C********************************************************************
291400930608     C     RDRASS        BEGSR
291500930608     C*
291600930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2917009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2918009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2919009407151507 C*********************EXSR RO3ORE
2920009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
292100930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2922009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2923009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2924009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2925009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2926009407191907 C                   Z-ADD     WPOASI        WPOROU
2927009407191907 C                   EXSR      CHKFES
2928009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2929009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2930009407191907 C                   ELSE                                                   *E' FERIALE
2931009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2932009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
2933009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
2934009407191907 C                   ENDIF
293500930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
293600930608     C                   MOVEL     'ASS1 '       WR3TAB
293700930608     C                   Z-ADD     *ZEROS        WR3HHI
293800930608     C                   EXSR      RO3ORE
293900930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
294000930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
294100931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
294200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
294300930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
294400930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
294500930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
294600930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
294700930608     C*
294800930608     C                   ENDSR
294900930511     C********************************************************************
295000930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
295100930511     C********************************************************************
295200930511     C     CALFED        BEGSR
295300150331     c                   clear                   fddata
295400150331     c                   clear                   WPOFDdata
295500930511     C*
295600051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
295700931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
295800051108     C     D98FFD        oreq      'M'                                          --- 1 -->
295900930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
296000930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
296100930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
296200930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
296300150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
296400150331     c                   if        §ar5dfd>*zeros
296500150331     c                   movel     §ar5dfd       fddata            8 0
296600150331     c                   endif
296700150331     c                   if        fddata>0 and ((d98dti>0 and
296800150331     c                             fddata>d98dti) or d98dti=0)
296900150331     C                   Z-ADD     fddata        WDAT
297000150331     C                   EXSR      DATORE
297100150331     C                   Z-ADD     WORE          WPOfddata
297200150331     c                   endif
297300150331     c
297400930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
297500930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
297600930511     C                   ELSE                                                   <-- 2 -->
2977009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
297800930512     C                   END                                                    <-- 2 ---
297900150331     c                   if        wpofddata>0 and wpofddata>wpofdi
2980001504081507 C                   Z-ADD     WPOfddata     WPOFDI
298100150331     c                   endif
298200150331
298300930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
298400930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
298500930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
298600930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
298700930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
298800930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
298900930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
299000930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
299100930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
299200930511     C                   END                                                    <-- 1 ---
299300930511     C*
299400930511     C                   ENDSR
299500930608     C********************************************************************
299600930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
299700930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
2998009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
299900930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
300000930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
300100930608     C********************************************************************
300200930608     C     RDRFED        BEGSR
300300930608     C*
300400930608     C* CALCOLO ORE DEL FERMO DEPOSITO
300500930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3006009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3007001503311907 C                   Z-ADD     WPOFDI        WPOROU
3008001503311907 C                   EXSR      CHKFES
3009001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3010001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3011001503311907 C                   ELSE                                                   *E' FERIALE
3012001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3013001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3014001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3015001503311907 C                   ENDIF
301600150331     c* Se presente la particolarità consegna con più giorni di fermo depo
301700150331     c*  uso questi
301800150331     c                   if        §7rggfd>*zeros
301900150424     c                   movel     §7rggfd       wggfd             2 0
302000150331     c     wggfd         mult      24            Wnore
302100150331     c                   add       wnore         worefd
302200150331     c                   else
302300930608     C                   MOVEL     'FED1 '       WR3TAB
302400930608     C                   Z-ADD     *ZEROS        WR3HHI
302500930608     C                   EXSR      RO3ORE
302600940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
302700150331     c                   endif
302800930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
302900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
303000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
303100930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
303200930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
303300930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
303400930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
303500930608     C*
303600930608     C                   ENDSR
303700931201     C********************************************************************
303800931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
303900931201     C********************************************************************
304000931201     C     CALDTE        BEGSR
304100140825     c
304200140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
304300140825     c*  che tiene conto solo dei festivi per restituire in output
304400140825     c*  la data teorica di consegna
304500140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
304600140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
304700140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
304800140825     C                   ELSE
304900140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
305000140825     C                   END
305100140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
305200141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
305300141106     c*  quindi passo un flag
305400141106     c                   eval      nosomma='N'
305500140825     C                   EXSR      RO4CDP
305600141106     c                   eval      nosomma=' '
305700140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
305800140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
305900140825     c*  di consegna
306000140825     c                   EXSR      TRAS_DEI
306100140825     c
306200140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
306300140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
306400140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
306500140825     c                             ='M' or %subst(d98gc2:2:1)='P')
306600140825     c                   exsr      Reper_tempi
306700140825     c                   endif
306800931201     C*
306900140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
307000140825     c*  che tiene conto di:
307100140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
307200140825     c*  per determinare la data FINE consegna richeista che non deve essere <
307300931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
307400931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
307500931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
307600931201     C                   ELSE
307700931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
307800931201     C                   END
307900090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
308000090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
308100931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
308200140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
308300931201     C*
308400931201     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA *******************
308500931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
308600931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
308700090827     C*****              Z-ADD     D98TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
308800090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
308900931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
309000140825     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
309100141017
309200141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
309300150219     C**                 EVAL      WTC='C'
309400150219     C**                 EVAL      WIC='C'
309500150219     c**                 z-add     wpoattdata00  WPOROU
309600150219     c**                 z-add     wdaodi        wpodata
309700150219     c**                 exsr      chkfes
309800150219     c**                 if        wslitt='S'
309900150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
310000150219     c**                 z-add     24            wr6noi
310100150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
310200150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
310300150219     c**                 else
310400150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
310500150219     c**                 endif
310600141204     c* Ricalcolo WPOrifDatLav
310700141229     C**                 MOVEL     'OSC  '       WR3TAB
310800141229     C**                 Z-ADD     WHHODI        WR3HHI
310900141229     C**                 EXSR      RO3ORE
311000141229     c**
311100141229     C**                 EVAL      WTC='C'
311200141229     C**                 EVAL      WIC='C'
311300141229     c**                 z-add     wpoattdata00  WPOROU
311400141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
311500141229     c**                 z-add     wr3hho        wr6noi
311600141229     c**                 add       12            wr6noi
311700141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
311800141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
311900141229     c
312000141017     c
312100141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
312200141017     c*  ma sempre devo slittare i festivi
312300141017     C* CONSEGNABILITA'
312400150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
312500150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
312600150219     C**                 EVAL      WTC='C'
312700150219     C**                 EVAL      WIC='C'
312800150219     c**                 z-add     d98dfc        WPOdata
312900150219     c**                 exsr      chkfes
313000150219     c**                 if        wslitt='S'
313100150219     c**                 z-add     wpodfc_00     Wr6poi
313200150219     c**                 z-add     24            wr6noi
313300150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
313400150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
313500150219     c**                 else
313600150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
313700150219     c**                 endif
313800150219     c**                 endif
313900931201     C*
314000931201     C                   ENDSR
314100930511     C********************************************************************
314200930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
314300930511     C********************************************************************
314400930511     C     CALDPC        BEGSR
314500930325     C*
314600930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
314700930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
314800931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
314900930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
315000930325     C                   ELSE
315100930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
315200930407     C                   END
315300090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
315400090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
315500930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
315600090902     c
315700931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
315800090901
315900930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
316000930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
316100930423     C* A QUELLA DI RICHIESTA CONSEGNA,
316200930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
316300931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
316400931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
316500931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
316600931206     C                   END
316700930423     C                   END
316800930325     C*
316900930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
317000930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
317100930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
317200090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
317300090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
317400930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
317500931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
317600930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
317700930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
317800930423     C* A QUELLA DI RICHIESTA CONSEGNA,
317900930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
318000931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
318100931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
318200931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
318300931206     C                   END
318400930423     C                   END
318500140822     C*
318600930324     C                   ENDSR
318700150113     C********************************************************************
318800150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
318900150113     C********************************************************************
319000150113     c     Cal_DEE       BEGSR
319100150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
319200150508     c                   clear                   wtempi00C         1
319300150219     c
319400150219     c* 1) Se presente l'arrivo della trazione lo uso
319500150219    1c                   select
319600150219     c                   when      wmancatraz=' '  and d98dti>0
319700150113     c                   Z-ADD     WPODTI        WR1POI
319800150219     c
319900150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
320000150219    1c                   when      IDdata>0
320100150325     c                   exsr      CalcoloIMP
320200150528     c* stato IMP/disguido solo se >  alla data di oggi
320300150528     c                   if        wr1poi > wpoattdata00
320400150427     c                   eval      d98spcdee='IMP'
320500150427     c                   endif
320600150219     c
320700150219     c                   other
320800150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
320900150225     c* devo far finta che sia arrivato.
321000150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
321100150508     c*  che tiene conto della data partenza PRIMO  collo
321200150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
321300150508     c*
321400150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
321500150508     c                   if        wr1poi >=wpoattdata00
321600150508     c                              and WPOPTC_DEE > WPOPTC
321700150508     c                   eval      d98spcdee='IMP'
321800150508     c                   endif
321900150219     c                   endsl
322000150113     c
322100150219     c* se il calcolo è < di oggi --> imposto oggi
322200150113     c                   if        wr1poi < wpoattdata00
322300150113     c                   eval      wr1poi= wpoattdata00
322400150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
322500150508     c                   eval      wTempi00C='S'
322600150113     c                   endif
322700150217     c
322800150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
322900150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
323000150427    1c                   if        wr1poi>wpoattdata00  and
323100150330     c                             (deeindist<>'S' or d98cons<>'S'
323200150330     c                             or d98ddc>wdaodi)
323300150427     c
323400150427     c* Se stato IMP lo mantengo per avere messaggio diverso
323500150427     c                   if        d98spcdee<>'IMP'
323600150304     c                   eval      d98spcdee='PRV'
323700150427     c                   endif
323800150427     c
323900150326   x1c                   else
324000150330     c                   exsr      CAL_OLP
324100150217     c
324200150528     c* Se passato l'orario oppure data distinta < data di oggi
324300150508     c* 1) spedizione in distinta          --> in consegna
324400150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
324500150528     c                             d98ddc<wdaodi)
324600150508    3c                   IF        deeindist='S' and d98cons='S'
324700150508     c                   eval      d98spcdee= 'DDC'
324800150508
324900150508   x3c                   else
325000150507     c
325100150507    4c                   if        wr1poi<=wpoattdata00
325200150507     c                   add       24            wr1poi
325300150507    4c                   endif
325400150507     c
325500150507     c                   eval      d98spcdee='OLP'
325600150507     c* memorizzo orario limite picking
325700150507     c                   eval      d98picklim=%editc(picklim:'X')
325800150507    3c                   endif
325900150507    2c                   endif
326000150507     c
326100150507    4c**                 if        §vpoolpnu>0  and §VPOOLPMN>0
326200150507    5c**                 do        §vpoolpnu
326300150507     c**                 add       §VPOOLPMN     picklim
326400150507     c**                 eval      wora=picklim
326500150507     c**                 eval      wdiff=70
326600150330     c
326700150507     c**                 dow       wdiff>=0
326800150507     c**                 eval      wdiff=wmn-60
326900150507     c**                 if        wdiff>=0
327000150507     c**                 eval      wmn=wmn-60
327100150507     c**                 eval      who=who+1
327200150507     c**                 endif
327300150507     c**                 enddo
327400150507     c**                 eval      picklim=wora
327500150507    6c**                 if        picklim>whhodi
327600150507     c**                 leave
327700150507    6c**                 endif
327800150507    5c**                 enddo
327900150507    4c**                 endif
328000150507    3c**                 endif
328100150326
328200150427     c*    2) non più in distinta o passati i tentativi di massimo avanzamento automatico:
328300150326     c*                           imposto stato OLP prevista cons NON modificabile
328400150507    3c**                 if        whhodi>picklim
328500150507    1c**                 if        wr1poi<=wpoattdata00
328600150507     c**                 add       24            wr1poi
328700150507     c**                 endif
328800150330     c
328900150507     c**                 eval      d98spcdee='OLP'
329000150507    3c**                 endif
329100150507    2c**                 endif
329200150507    c
329300150326     c* memorizzo orario limite picking
329400150507     c**                 eval      d98picklim=%editc(picklim:'X')
329500150326    1c                   endif
329600150217     c
329700150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
329800150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
329900150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
330000150113     C                   Z-ADD     Wr1poi        W10POI            9 0
330100150113     C                   EXSR      R10FES
330200150113     C                   Z-ADD     W10POO        wr1poi
330300150113     C
330400150508     c* Se devo sommare i tempi entro soglia --> lo faccio
330500150508     c                   if        wtempi00C='S'
330600150508     c                   if        w990CN=10
330700150508     c                   z-add     12            wr1noi
330800150508     c                   else
330900150508     c                   z-add     W990CN        wr1noi
331000150508     c                   endif
331100150508     c
331200150508     c                   else
331300150113     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
331400150508     c                   endif
331500150508     c
331600150113     C                   EXSR      RO1CDP
331700150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
331800150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
331900150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
332000150113     C* A QUELLA DI RICHIESTA CONSEGNA,
332100150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
332200150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
332300150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
332400150331     c*  2) IL        -> è il giorno indicato
332500150331     C     D98DCR        IFNE      *ZEROS
332600150331     C     D98tcr        andne     'P'
332700150331     C***  WPOdpcee      IFLE      WPOCR2
332800150331     C***  WPOCR2        SUB       12            WPOdpcee
332900150330     c
333000150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
333100150330     c*   dare la prima data utile di consegna
333200150330     C     WPOdpcee      IFLE      WPOCRF_dee
333300150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
333400150330     C                   z-add     WPOCRF_dee    WPOdpcee
333500150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
333600150409     c                   if        wpodpcee>wpoattdata00
333700150409     c                   eval      d98spcdee='PRV'
333800150409     c                   endif
333900150113     C                   END
334000150331     c
334100150220     c
334200150113     C                   END
334300150220     c
334400150113     c                   ENDSR
334500150330     C********************************************************************
334600150330     C*   Calcolo orario da tabella OLP
334700150330     C********************************************************************
334800150330     c     Cal_OLP       BEGSR
334900150330     c                   eval      picklim=0900
335000150330     c* Dopo l'ora limite picking, passo al giorno dopo
335100150330     C                   MOVEL(P)  'OLP'         TBECOD
335200150330     C                   MOVEL(P)  d98lna        TBEKE1
335300150330     C                   MOVEL(P)  wdaodi        TBEKE2
335400150330     C     KTBE          CHAIN     TNTBE01L
335500150330     C                   IF        not %found(TNTBE01L)
335600150330     c                   clear                   tbeke2
335700150330     C     KTBE          CHAIN     TNTBE01L
335800150330     c                   endif
335900150330     C                   IF        not %found(TNTBE01L)
336000150330     C                   MOVEL(P)  '000'         TBEKE1
336100150330     C     KTBE          CHAIN     TNTBE01L
336200150330     c                   endif
336300150330     c
336400150330     C                   IF        %found(TNTBE01L) and
336500150330     c                             %subst(tbeuni:1:4)>*zeros
336600150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
336700150330     c                   endif
336800150330     c                   ENDSR
336900150325     C********************************************************************
337000150325     C*   Riclcolo data se colli in IMP/disguido
337100150325     C********************************************************************
337200150325     c     CalcoloIMP    BEGSR
337300150325     C                   Z-ADD     IDdata        WDAT
337400150325     C                   EXSR      DATORE
337500150325     C                   Z-ADD     WORE          WR4POI
337600150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
337700150325    2c                   if        IDfgs<>D98lnp
337800150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
337900150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
338000150325     c* calcolo il terminal di partenza
338100150325     c                   clear                   fnlv55ds
338200150325     c                   eval      d55tla='L'
338300150325     c                   eval      d55tpt='P'
338400150325     c                   eval      d55lin=IDfgs
338500150325     c                   eval      d55drf=IDdata
338600150325     c                   call      'FNLV55R'
338700150325     c                   parm                    fnlv55ds
338800150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
338900150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
339000150325     c                   EXSR      RTTRAZ
339100150325    3c                   if        d98err<>'1'
339200150325     C                   Z-ADD     CASTcn        wr4noi
339300150325     c                   else
339400150325     C                   Z-ADD     d98ttc        wr4noi
339500150325    3C                   endif
339600150325
339700150325   x2c                   else
339800150325     C                   Z-ADD     d98ttc        wr4noi
339900150325    2C                   endif
340000150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
340100150325     c*  perchè partirà un giorno dopo
340200150325    3c                   if        IDnpg='P'
340300150325     c                   add       24            WR4noi
340400150325    3c                   endif
340500150325     c
340600150325     c                   exsr      ro4cdp
340700150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
340800150325     c                   ENDSR
340900930604     C********************************************************************
341000930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
341100930604     C********************************************************************
341200930604     C     TPODAT        BEGSR
341300960229     C*
341400960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
341500960229     C*
341600960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
341700960229     C     WPOPTI        IFEQ      *ZEROS
341800960229     C                   Z-ADD     *ZEROS        D98DTI
341900960229     C                   Z-ADD     *ZEROS        D98HTI
342000960229     C                   ELSE
342100960229     C                   Z-ADD     WPOPTI        WORE
342200960229     C                   EXSR      OREDAT
342300960229     C                   Z-ADD     WDAT          D98DTI
342400960229     C     WRES          MULT      100           D98HTI
342500960229     C                   END
342600960229     C*
342700960229     C                   ELSE                                                   <-- 1 -->
342800930604     C*
342900931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
343000140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
343100140825     c*******            exsr      Tras_DEI
343200140825     c
343300930604     C*
343400150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
343500140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
343600140820     c* è data da :
343700140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
343800140820     c*  Tenere  conto, ovvero slittare in avanti, per :
343900140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
344000140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
344100140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
344200140911     c*
344300140820     c*  Da azzerare nei seguenti casi:
344400150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
344500150324     c* 4) bolla export    7) Cod manca consegna
344600140911     c* 8) bolla con DCR DOPOil o PRIMAdel
344700150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
344800150119     c*10) se completamente non arrivata e collo in IMP
344900150119     c
345000140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
345100150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
345200140925     C*  - FD   - export -
345300150217     c                   movel     'S'           DEEinDIST         1
345400150113     c                   eval      wcaldpcee='S'
345500150304     c                   eval      D98SPCDEE='PRV'
345600150324     c                   clear                   avvcmc            1
345700150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
345800150323     c                   if        d98cmc<>*blanks
345900150324     C                   Z-ADD     1             II
346000150326     C     d98cmc        LOOKUP    C2A(II)                                99
346100150324     C                   MOVEL     D2A(II)       usa2A
346200150324    4C     §2AFTC        IFEQ      'A'
346300150326     c                   eval      avvcmc='A'
346400150323     c                   endif
346500150324    4C     §2AFTC        IFEQ      'G'
346600150326     c                   eval      avvcmc='G'
346700150324     c                   endif
346800150324     c                   endif
346900140820     c
347000140911     c                   z-add     d98lna        EX
347100150113     c*  la azzero nei casi previsti                 0
347200140820     c                   select
347300150119     c* 1) Se la bolla e consegnata
347400140925     c                   when      d98dcm>0
347500150113     c                   clear                   wcaldpcee
347600150216     c                   eval      d98spcdee='CON'
347700150217     c                   movel     ' '           DEEinDIST         1
347800150323     c* 2) Se pratica di danno
347900150323     c                   when      flgdan='S'
348000150323     c                   clear                   wcaldpcee
348100150323     c                   eval      d98spcdee='CA '
348200150323     c                   eval      deeindist='S'
348300150324     c* 3) Se bloccata/rimpallo
348400150114     c                   when      wd98fbc='B'
348500150113     c                   clear                   wcaldpcee
348600150216     c                   eval      d98spcdee='BLO'
348700150217     c                   movel     ' '           DEEinDIST         1
348800150324     c* 4) Se bolla export
348900150324     c                   when      fl1(EX)='E'
349000150324     c                   clear                   wcaldpcee
349100150324     c                   movel     ' '           DEEinDIST         1
349200150324     c                   eval      d98spcdee='EXP'
349300150324     c* 5) Se giacente  o in attesa di apertura giacenza
349400150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
349500150324     c                   clear                   wcaldpcee
349600150324     c                   eval      d98spcdee='GIA'
349700150324     c                   eval      deeindist='S'
349800150119     c* 7) Se codice manca consegna
349900150326     c                   when      d98cmc<>*blanks and avvcmc='A'
350000150324     c                   clear                   wpodpcee
350100150324     c                   clear                   wcaldpcee
350200150324     c                   movel     ' '           DEEinDIST         1
350300150324     c                   eval      d98spcdee='ATT'
350400150410     c* 7) Bolla in sospensione consegna
350500150410     c                   when      §B4LBGE='S'
350600150410     c                   clear                   wpodpcee
350700150410     c                   clear                   wcaldpcee
350800150410     c                   movel     ' '           DEEinDIST         1
350900150410     c                   eval      d98spcdee='SOS'
351000150325     c                   other
351100150325     c*
351200150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
351300150331     c                   if        d98ffd='S'
351400150403     c                   select
351500150403     c* FD escluso dai contatti
351600150403     c                   when      §B4LTFD= 'N'
351700150403     c                   eval      d98spcdee='FD3'
351800150403     c                   clear                   wcaldpcee
351900150403     c                   movel     ' '           DEEinDIST         1
352000150403
352100150403     c* FD non contattato ( o da ricontattare)
352200150403     c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
352300150401     c                             olv82sts<>'1'
352400150323     c                   clear                   wcaldpcee
352500150323     c                   movel     ' '           DEEinDIST         1
352600150323     c                   eval      d98spcdee='FFD'
352700150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
352800150424     c                   eval      wpodpcee=wpoffi
352900150403
353000150403     c* FD contattato e     IN distinta
353100150403     c                   when      d98cons='S'
353200150401     c                   eval      d98spcdee='FD2'
353300150401     c                   clear                   wcaldpcee
353400150401     c                   movel     ' '           DEEinDIST         1
353500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
353600150424     c                   eval      wpodpcee=wpoffi
353700150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
353800150427     c*  ritirare la merce
353900150427     C                   MOVEL     'C'           WTC                            *PARTENZA
354000150427     C                   MOVEL     'C'           WIC
354100150427     C                   Z-ADD     D98ddc        WDAT
354200150427     C                   EXSR      DATORE
354300150427     c                   z-add     wore          wr4poi
354400150427     c                   z-add     24            wr4noi
354500150427     C                   EXSR      RO4CDP
354600150427     c                   z-add     wr4poo        wore
354700150427     c                   exsr      OREDAT
354800150427     c                   movel     wdat          D98DTCTD
354900150403     c                   other
355000150403     c* FD contattato e NON in distinta
355100150331     c                   eval      d98spcdee='FD1'
355200150408     c                   eval      wpodpcee=wpoffi
355300150331     c                   clear                   wcaldpcee
355400150331     c                   movel     ' '           DEEinDIST         1
355500150403     c                   endsl
355600150331     c
355700150331     c                   endif
355800150403     c
355900150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
356000150325     c                   if        (d98tc1='A' or d98tc2='A' or
356100150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
356200150325     c                   clear                   wcaldpcee
356300150325     c                   movel     ' '           DEEinDIST         1
356400150403     c                   if        §B4LBAS= 'N'
356500150427     c                   eval      d98spcdee='AP3'
356600150403     c                   else
356700150403     c                   eval      d98spcdee='APP'
356800150325     c                   endif
356900150403     c                   endif
357000150325     c*   se la bolla ha il C/Assegno verifico se da contattare
357100150325     c                   if        d98cas>0 and §B4LTCA<>' '
357200150325     c                   clear                   wcaldpcee
357300150325     c                   movel     ' '           DEEinDIST         1
357400150403     c                   if        §B4LBAS= 'N'
357500150427     c                   eval      d98spcdee='CA3'
357600150403     c                   else
357700150403     c                   eval      d98spcdee='CAS'
357800150325     c                   endif
357900150403     c                   endif
358000150325     c* Per questi 3 casi devo impostare la data contatto destinatario
358100150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
358200150325     c                             d98spcdee='CAS'
358300150325     c                   select
358400150403     c                   when      d98dti>0 and wmancatraz= ' '
358500150325     c                   movel     d98dti        d98dtctd
358600150403     c* se piene le ore già portate ad un giorno lavorativo
358700150403     c                   if        wpodti>0
358800150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
358900150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
359000150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
359100150403     C                   Z-ADD     WPODTI        W10POI            9 0
359200150403     C                   EXSR      R10FES
359300150403     c                   z-add     w10poo        wore
359400150403     C                   EXSR      OREDAT
359500150403     C                   movel     WDAT          D98dtctd
359600150403     c                   endif
359700150403     c
359800150325     c                   when      d98dam>0
359900150421     C                   Z-ADD     D98dam        WDAT
360000150421     C                   EXSR      DATORE
360100150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
360200150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
360300150421     C                   Z-ADD     Wore          W10POI            9 0
360400150421     C                   EXSR      R10FES
360500150421     c                   z-add     w10poo        wore
360600150421     C                   EXSR      OREDAT
360700150421     C                   movel     WDAT          D98dtctd
360800150325     c                   other
360900150325     c* data teorica di arrivo tenendo conto dell'IMP
361000150325     c                   exsr      letbrv
361100150325     c                   if        idDATA>0
361200150325     c                   exsr      CalcoloIMP
361300150325     c                   z-add     wr1poi        WORE
361400150325     c                   else
361500150325     c                   z-add     wpoptc        WORE
361600150325     c                   endif
361700150325     C                   EXSR      OREDAT
361800150325     c                   movel     wdat          d98dtctd
361900150325     c                   endsl
362000150325     c
362100150325     c                   endif
362200150325     c
362300140820     c                   endsl
362400140820     c
362500150113     c* Se la bolla non è in distinta calcolo la data se previsto
362600150220     c                   if        wcaldpcee='S' and
362700150220     c                             (d98cons<>'S'  or (d98cons='S' and
362800150220     c                             evemic<>'SI'))
362900150219     c
363000150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
363100150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
363200150219     c                   if        d98nsp>0 and wtrasm<>'D' and
363300150219     c                             d98dam=0
363400150219     c                   exsr      letBRV
363500150219     c                   endif
363600150219     c
363700150217     c                   eval      d98spcdee='PRG'
363800150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
363900150320     c                             d98tc1='S' or d98tc2='S')
364000150320     c                   eval      d98spcdee='APP'
364100150320     c                   endif
364200150320     c
364300150113     c                   exsr      cal_DEE
364400150113     c                   endif
364500150113     c
364600140820     C     WPOdpcee      IFEQ      *ZEROS
364700931202     C                   Z-ADD     *ZEROS        D98DEE
364800931202     C                   Z-ADD     *ZEROS        D98OEE
364900930604     C                   ELSE
365000140820     C                   Z-ADD     WPOdpcee      WORE
365100141021     C******             SUB       12            WORE
365200930604     C                   EXSR      OREDAT
365300931202     C                   Z-ADD     WDAT          D98DEE
365400090903     c                   if        d98tcc<>10
365500090903     C                   ADD       12            WRES
365600090903     c                   else
365700090903     C                   ADD       10            WRES
365800090903     c                   endif
365900090903
366000931202     C     WRES          MULT      100           D98OEE
366100090903     c
366200090903     c                   if        d98oee=1000
366300090903     c                   eval      d98oee=1030
366400090903     c                   endif
366500140925     C                   END
366600150311     c
366700150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
366800150311     c                   select
366900150311     c                   when      d98spcdee='OLP'
367000140820     c
367100150220     c* Se bolla in distinta --> data prevista = data distinta
367200150311     c                   when      deeindist='S'  and
367300150113     c                             d98cons='S'
367400150220
367500150225     c*  non la propongo senza MIC e se presente disguido o IMP
367600150220     c                   if        d98dee>d98ddc and evemic<>'SI'
367700150220     c                             and IDdata>0
367800150220     c                   else
367900150220
368000140820     c                   eval      d98dee=d98ddc
368100140925     c                   clear                   d98oee
368200150320     c* se la data è > di oggi stato PRV se non è in IMP
368300150320     c                   if        d98dee>wdaodi
368400150320     c                   if        d98spcdee<>'IMP'
368500150311     c                   eval      d98spcdee='PRV'
368600150320     c                   endif
368700150320     c
368800150225     c                   else
368900150217     c* se c'è il MIC
369000150217     c                   if        evemic='SI'
369100150217     c                   eval      d98spcdee='DDC'
369200150330     C***                else
369300150304     c* solo se in PRV, metto lo stato
369400150330     c* questo caso lo asterisco perchè dovrebbe essere già stato gestito
369500150330     c*  prima in quanto devo tenere conto di due aggiunte all'orario limite tab.OLP
369600150330     c***                if        d98spcdee='PRV'
369700150330     c***                eval      d98spcdee='PRG'
369800150330     c***                endif
369900150225     c                   endif
370000150217     c                   endif
370100150220     c                   endif
370200150217     c
370300150311     c                   endsl
370400931202     C*
370500931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
370600931202     C     WPODCI        IFEQ      *ZEROS
370700931202     C                   Z-ADD     *ZEROS        D98DCI
370800931202     C                   Z-ADD     *ZEROS        D98HCI
370900931202     C                   ELSE
371000931202     C                   Z-ADD     WPODCI        WORE
371100931202     C                   EXSR      OREDAT
371200931202     C                   Z-ADD     WDAT          D98DCI
371300090902     c                   if        d98tci<>10
371400931202     C                   ADD       12            WRES
371500090902     c                   else
371600090902     C                   ADD       10            WRES
371700090902     c                   endif
371800090902
371900931202     C     WRES          MULT      100           D98HCI
372000090901     c
372100090903     c                   if        d98hci=1000
372200090901     c                   eval      d98hci=1030
372300090901     c                   endif
372400090901     c
372500931202     C                   END
372600931202     C*
372700931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
372800931202     C     WPODCE        IFEQ      *ZEROS
372900931202     C                   Z-ADD     *ZEROS        D98DCE
373000931202     C                   Z-ADD     *ZEROS        D98HCE
373100931202     C                   ELSE
373200931202     C                   Z-ADD     WPODCE        WORE
373300931202//// C**************       SUB  12        WORE
373400931202     C                   EXSR      OREDAT
373500931202     C                   Z-ADD     WDAT          D98DCE
373600090902     c                   if        d98tcc >10
373700931202     C                   ADD       12            WRES
373800090902     c                   else
373900090902     C                   ADD       10            WRES
374000090902     c                   endif
374100090902     c
374200931202     C     WRES          MULT      100           D98HCE
374300090902     c
374400090903     c                   if        d98hce=1000
374500090902     c                   eval      d98hce=1030
374600090902     c                   endif
374700090902     c
374800931202     C                   END
374900960229     C*
375000960229     C                   ENDIF                                                  <-- 1 ---
375100930604     C*
375200930604     C                   ENDSR
375300150218     C********************************************************************
375400150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
375500150218     C********************************************************************
375600150218     c     LetBRV        BEGSR
375700150219     c                   clear                   bollaOK           1
375800150219     c                   clear                   IDnpg
375900150219     c                   clear                   IDdata
376000150219     c                   clear                   IDora
376100150219     c                   clear                   IDfgs
376200150219     c
376300150218     C     Kart          SETLL     FNART01L
376400150218     C     Kart          READE     FNART01L
376500150219    1c                   dow       not %eof(fnart01l)
376600150219     c                   clear                   DISfgs
376700150219     c                   clear                   DISdata
376800150219     c                   clear                   DISnpg
376900150219     c                   clear                   DISora
377000150219     c                   clear                   IMPdata
377100150219     c                   clear                   IMPfgs
377200150508     c* per ogni collo verifico se in IMP o disguidato
377300150218     c* al primo che NON trovo, esco come --> OK
377400150218     c     kbrv          setll     fnbrv07l
377500150218     c     kbrv          reade     fnbrv07l
377600150219    2c                   dow       not %eof(fnbrv07l)
377700150219    3c                   if        brvatr='D'  or brvnpg=3
377800150218     c* prendo la data della spunta
377900150218     c                   clear                   fnlv53ds
378000150219     c                   eval      d53tla='L'
378100150218     C                   MOVEL     BRvNPG        D53NPG
378200150218     C                   Z-ADD     BRvNFV        D53NFV
378300150218     C                   MOVEL     BRvFGS        D53FGS
378400150218     C                   CALL      'FNLV53R'
378500150218     C                   PARM                    fnlv53ds
378600150218    4c                   if        d53err<>*blanks
378700150219    5c                   if        brvdcs>0
378800150218     c                   eval      d53dfv=brvdcs
378900150218     c                   else
379000150219    6c                   if        brvdfs>0
379100150218     c                   eval      d53dfv=brvdfs
379200150219    6c                   endif
379300150219    5c                   endif
379400150219    4c                   endif
379500150218     c
379600150219    4c                   select
379700150219     c* se si tratta di IMP --> memorizzo
379800150219    4c                   when      d53spg='P' and d53dfv>impdata
379900150218     c                   eval      IMPdata=d53dfv
380000150219     c                   eval      IMPfgs =brvfgs
380100150219     c* se si tratta di DISGUIDO
380200150218     c                   when      brvatr='D'
380300150219     c
380400150219    5c                   if        d53dfv=DISdata
380500150219     c* Verifico l'ora
380600150219    6c                   if        brvhcs>0 and brvhcs>DISora
380700150219     c                   eval      DISdata=d53dfv
380800150219     c                   eval      DISfgs =brvfgs
380900150219     c                   eval      DISnpg =brvnpg
381000150219     c                   eval      DISora =brvhcs
381100150219   x6c                   else
381200150219    7c                   if        brvhms>0 and brvhms>DISora
381300150219     c                   eval      DISdata=d53dfv
381400150219     c                   eval      DISfgs =brvfgs
381500150219     c                   eval      DISnpg =brvnpg
381600150219     c                   eval      DISora =brvhms
381700150219    7c                   endif
381800150219    6c                   endif
381900150219    5c                   endif
382000150219     c
382100150219    5c                   if        d53dfv>DISdata
382200150219     c                   eval      DISdata=d53dfv
382300150219     c                   eval      DISfgs =brvfgs
382400150219     c                   eval      DISnpg =brvnpg
382500150219     c                   eval      DISora =brvhcs
382600150219     c                   if        disora=0
382700150219     c                   eval      DISora =brvhms
382800150219     c                   endif
382900150219    5c                   endif
383000150219     c
383100150219    4c                   endsl
383200150219    3c                   endif
383300150219     c     kbrv          reade     fnbrv07l
383400150219    2c                   enddo
383500150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
383600150219     c*  esco dal ciclo: la bolla è OK
383700150219    2c                   if        impdata=0 and disdata=0
383800150219     c                   eval      bollaOK='S'
383900150219     c                   clear                   IDdata
384000150219     c                   clear                   IDfgs
384100150219     c                   clear                   IDora
384200150219     c                   clear                   IDnpg
384300150219   x2c                   else
384400150219     c* memorizzo la data più alta
384500150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
384600150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
384700150219     c                   eval      IDdata=IMPdata
384800150219     c                   eval      IDfgs=IMPfgs
384900150219     c                   eval      IDora=0
385000150219     c                   eval      IDnpg='P'
385100150219    3c                   endif
385200150219    3c                   if        (DISdata>0 and DISdata=IDdata
385300150219     c                             and IDnpg<>'P' and DISora>IDora) or
385400150219     c                             (DISdata>0 and DISdata>IDdata)
385500150219     c                   eval      IDdata=DISdata
385600150219     c                   eval      IDfgs=DISfgs
385700150219     c                   eval      IDora=DISora
385800150219     c                   eval      IDnpg=%char(DISnpg)
385900150219    3c                   endif
386000150219    2c                   endif
386100150218     c
386200150218     C     Kart          READE     FNART01L
386300150219    1c                   enddo
386400150219     c
386500150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
386600150508     c**                 if        d98duc> 0 and d98duc>IDdata
386700150508     c**                 eval      bollaOK='S'
386800150508     c**                 clear                   IDdata
386900150508     c**                 clear                   IDfgs
387000150508     c**                 clear                   IDora
387100150508     c**                 clear                   IDnpg
387200150508     c**                 endif
387300150219     c
387400150218     c
387500150218     C                   ENDSR
387600140825     C********************************************************************
387700140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
387800140825     C********************************************************************
387900140825     c     TRAS_DEI      BEGSR
388000140825     C     WPODEI        IFEQ      *ZEROS
388100140825     C                   Z-ADD     *ZEROS        D98DEI
388200140825     C                   Z-ADD     *ZEROS        D98OEI
388300140825     C                   ELSE
388400140825     C                   Z-ADD     WPODEI        WORE
388500140825     C                   SUB       12            WORE
388600140825     C                   EXSR      OREDAT
388700140825     c
388800140825     C                   Z-ADD     WDAT          D98DEI
388900140825     c                   if        d98tci<>10
389000140825     C                   ADD       12            WRES
389100140825     c                   else
389200140825     C                   ADD       10            WRES
389300140825     c                   endif
389400140825     c
389500140825     C     WRES          MULT      100           D98OEI
389600140825     c
389700140825     c                   if        d98oei=1000
389800140825     c                   eval      d98oei=1030
389900140825     c                   endif
390000140825     c
390100140825     C                   END
390200140825     c                   ENDSR
390300930604     C********************************************************************
390400930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
390500930604     C********************************************************************
390600930604     C     CHKFOR        BEGSR
390700930604     C*
390800930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
390900930604     C* E QUINDI FORZO LA CONFORMITA' A SI
391000941227     C     D98ERR        IFEQ      '0'
391100930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
391200930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
391300930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
391400930604     C                   MOVEL     '1'           D98ERR
391500930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
391600930604     C                   END
391700930604     C                   END
391800990212     C*
391900990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
392000990212     C* E QUINDI FORZO LA CONFORMITA' A SI
392100990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
392200990212     C     D98ERR        IFEQ      '0'
392300990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
392400141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
392500990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
392600990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
392700990212     C                   MOVEL     '1'           D98ERR
392800990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
392900990212     C                   END
393000990212     C                   END
393100990308     C*
393200990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
393300990308     C* E QUINDI FORZO LA CONFORMITA' A SI
393400100614     C**   D98ERR        IFEQ      '0'
393500100614     C**   WFBR          IFEQ      'S'                                          *RESO
393600100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
393700100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
393800100614     C**                 MOVEL     '1'           D98ERR
393900100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
394000100614     C**                 END
394100100614     C**                 END
394200930604     C*
394300930604     C                   ENDSR
394400990212     C********************************************************************
394500990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
394600990212     C********************************************************************
394700990212     C     CHKDAN        BEGSR
394800990212     C*
394900990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
395000040809      *
395100040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
395200040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
395300040809      * legate alla spedizione che passo
395400040809     c                   clear                   fidn12ds
395500040809     c                   eval      i12aas = D98AAS
395600040809     c                   eval      i12lnp = D98LNP
395700040809     c                   eval      i12nrs = D98NRS
395800040809     c                   eval      i12nsp = D98NSP
395900040809     c                   eval      i12fel = 'E'
396000040809     c                   eval      i12fan = 'E'
396100040809     c*
396200040809     c                   call      'FIDN12R'
396300040809     c                   parm                    fidn12ds
396400040809     c*
396500040809     c* se non ci sono errori
396600040809     c                   if        o12err = ' '
396700040809     c* se numero di CA trovate maggiore di zero
396800040809     c                   if        o12nca > 0
396900040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
397000040809     c                   endif
397100040809     c                   endif
397200990212     C*
397300990212     C                   ENDSR
397400941228     C********************************************************************
397500941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
397600941228     C********************************************************************
397700941228     C     CALORP        BEGSR
397800941228     C*
397900941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
398000941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
398100941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
398200960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
398300950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
398400950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
398500950110     C                   ELSE
398600950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
398700950110     C                   ENDIF
398800941228     C                   EXSR      RO2COP
398900941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
399000941228     C* SPED. IL GIORNO DOPO
399100941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
399200941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
399300941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
399400941228     C*
399500941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
399600941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
399700941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
399800941228     C                   Z-ADD     WPODSP        WR2POI
399900950110     C                   Z-ADD     WPOPPC        WR2POF
400000941228     C                   EXSR      RO2COP
400100941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
400200941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
400300941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
400400941228     C*
400500941228     C                   ENDSR
400600930414     C********************************************************************
400700930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
400800930414     C********************************************************************
400900930414     C     CALORT        BEGSR
401000930414     C*
401100930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
401200931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
401300930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
401400931126     C                   Z-ADD     *ZEROS        W98HF
401500950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
401600950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
401700950110     C                   ELSE
401800950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
401900950110     C                   ENDIF
402000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
402100931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
402200930608     C                   ELSE
402300931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
402400930608     C                   END
402500931126     C                   EXSR      RO2COV
402600931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
402700930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
402800930414     C*
402900930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
403000930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
403100930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
403200931126     C                   Z-ADD     *ZEROS        W98HF
403300950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
403400931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
403500931126     C                   EXSR      RO2COV
403600931126     C                   Z-ADD     WR2NOV        W98RTC
403700930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
403800930414     C*
403900930414     C                   ENDSR
404000930414     C********************************************************************
404100930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
404200930414     C********************************************************************
404300930414     C     CALORC        BEGSR
404400930511     C*
404500930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
404600930511     C* I TEMPI DI RITARDO AD OGGI
404700930511     C     WPODCM        IFEQ      *ZEROS
404800960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
404900930511     C                   END
405000940117     C*
405100940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
405200940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
405300940117     C* ALLA DATA DI FINE CONSEGNABILITA'
405400940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
405500940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
405600940118££££ C     WPODFC        IFNE      *ZEROS
405700940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
405800940118££££ C                   END
405900930414     C*
406000930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
406100930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
406200930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
406300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
406400931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
406500930608     C                   ELSE
406600931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
406700930608     C                   END
406800931126     C                   Z-ADD     WPODCM        WR5POF            9 0
406900931126     C                   EXSR      RO5COV
407000931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
407100930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
407200930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
407300060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
407400060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
407500930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
407600930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
407700930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
407800930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
407900930414     C*
408000930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
408100930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
408200930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
408300931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
408400931126     C                   Z-ADD     WPODCM        WR5POF            9 0
408500931126     C                   EXSR      RO5COV
408600931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
408700930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
408800930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
408900060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
409000060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
409100930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
409200930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
409300930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
409400930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
409500930414     C*
409600930414     C                   ENDSR
409700930414     C********************************************************************
409800930414     C*   CALORR - CALCOLA ORE REALI DI RESA
409900930414     C********************************************************************
410000930414     C     CALORR        BEGSR
410100930414     C*
410200950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
410300950110     C                   ADD       W98RTI        W98RRI
410400941228     C                   ADD       W98RCI        W98RRI
410500950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
410600950110     C                   ADD       W98RTC        W98RRC
410700941228     C                   ADD       W98RCC        W98RRC
410800930414     C*
410900930414     C                   ENDSR
411000930420     C********************************************************************
411100930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
411200930420     C********************************************************************
411300930420     C     CALDIF        BEGSR
411400930420     C*
411500941228     C* PARTENZA
411600941228     C     W98RPI        SUB       D98TPI        W98FPI
411700941228     C     W98RPC        SUB       D98TPC        W98FPC
411800941228     C*
411900930420     C* TRAZIONE
412000930423     C     W98RTI        SUB       D98TTI        W98FTI
412100930423     C     W98RTC        SUB       D98TTC        W98FTC
412200930420     C*
412300930420     C* CONSEGNA
412400090827     C***  W98RCI        SUB       D98TCI        W98FCI
412500090827     C     W98RCI        SUB       w99TCI        W98FCI
412600090827     C***  W98RCC        SUB       D98TCC        W98FCC
412700090827     C     W98RCC        SUB       w99TCC        W98FCC
412800930420     C*
412900930420     C* RESA
413000090921     C***  W98RRI        SUB       D98TRI        W98FRI
413100090921     C     W98RRI        SUB       w99TRI        W98FRI
413200090921     c
413300090921     C***  W98RRC        SUB       D98TRC        W98FRC
413400090921     C     W98RRC        SUB       w99TRC        W98FRC
413500930420     C*
413600930420     C                   ENDSR
413700930420     C********************************************************************
413800930420     C*   CALCNF - CALCOLA LE CONFORMITA'
413900930420     C********************************************************************
414000930420     C     CALCNF        BEGSR
414100941228     C*
414200941228     C*-------------------------------------------------------------------
414300941228     C* CONFORMITA' PARTENZA  INTERNA
414400941228     C*-------------------------------------------------------------------
414500941228     C                   Z-ADD     W98FPI        W98NPI
414600941228     C     W98FPI        IFLE      *ZEROS
414700941228     C                   MOVEL     'S'           D98CPI
414800941228     C                   ELSE
414900941228     C                   MOVEL     'N'           D98CPI
415000941228     C                   END
415100941228     C*
415200941228     C*-------------------------------------------------------------------
415300941228     C* CONFORMITA' PARTENZA  CLIENTE
415400941228     C*-------------------------------------------------------------------
415500941228     C                   Z-ADD     W98FPC        W98NPC
415600941228     C     W98FPC        IFLE      *ZEROS
415700941228     C                   MOVEL     'S'           D98CPC
415800941228     C                   ELSE
415900941228     C                   MOVEL     'N'           D98CPC
416000941228     C                   END
416100930420     C*
416200930420     C*-------------------------------------------------------------------
416300930420     C* CONFORMITA' TRAZIONE  INTERNA
416400930420     C*-------------------------------------------------------------------
416500930510     C                   Z-ADD     W98FTI        W98NTI
416600930423     C     W98FTI        IFLE      *ZEROS
416700930420     C                   MOVEL     'S'           D98CTI
416800930420     C                   ELSE
416900930420     C                   MOVEL     'N'           D98CTI
417000930420     C                   END
417100930420     C*
417200930420     C*-------------------------------------------------------------------
417300930420     C* CONFORMITA' TRAZIONE  CLIENTE
417400930420     C*-------------------------------------------------------------------
417500930510     C                   Z-ADD     W98FTC        W98NTC
417600930423     C     W98FTC        IFLE      *ZEROS
417700930420     C                   MOVEL     'S'           D98CTC
417800930420     C                   ELSE
417900930420     C                   MOVEL     'N'           D98CTC
418000930420     C                   END
418100930420     C*
418200930420     C*-------------------------------------------------------------------
418300930420     C* CONFORMITA' CONSEGNA  INTERNA
418400930420     C*-------------------------------------------------------------------
418500150226     c                   if        wmancatraz='S'
418600150226     c                   clear                   d98dti
418700150226     c                   clear                   d98hti
418800150226     c                   endif
418900150226     c
419000930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
419100930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
419200930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
419300050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
419400050914     c                   if        consideraora='S'
419500050914     c     w98fci        sub       24            w98nci
419600050914     c                   add       24            d98hrc
419700050914     c                   else
419800930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
419900050914     c                   endif
420000080116     c
420100110923     c                   select
420200080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
420300080116     c*  --> trasformo il ritardo in 0
420400090827     c                   when      d98cca='2'
420500080116     c                   z-add     0             w98nci
420600090827
420700080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
420800080116     c*  e sto leggendo la bolla in arrivo
420900080116     c*  --> trasformo il ritardo = 0
421000090827     c                   when      d98cca='1' and d98tbo='A'
421100080116     c                   z-add     0             w98nci
421200090902     c
421300090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
421400090902     c                   when      wmancaTRAZ='S'
421500090902     c                   z-add     0             w98nci
421600090902     c                   clear                   d98dti
421700090902     c                   clear                   d98hti
421800110923     c
421900110923     c* Ignoro ritardo se pefect order
422000130221     c                   when      d98xco >*zeros
422100110923     c                   z-add     0             w98nci
422200090827     c
422300090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
422400091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
422500091014     c*  controllo ora soglia  consegna da tabella OSC)
422600091009     c                   when      d98tci=10   and
422700091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
422800091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
422900091014     c*
423000091014     c                             d98hci=1030
423100091014     C                   MOVEL     'OSC  '       WR3TAB
423200091014     C                   eval      WR3HHI=d98hmc
423300091014     C                   EXSR      RO3ORE
423400131218
423500091014     c                   if        wr3matpom='M' and d98hmc>1030
423600131218     c                   exsr      ctrLAVH1030
423700131218
423800131218     c                   if        wlav1030<>'S'
423900091014     c                   add       12            w98nci
424000091014     c                   endif
424100131218     c                   endif
424200090928     c
424300090827     c                   endsl
424400050914     c
424500930524     C                   ELSE                                                   <-- 1 -->
424600930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
424700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
424800930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
424900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
425000930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
425100930608     C                   ELSE
425200930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
425300930608     C                   END
425400930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
425500930525     C                   EXSR      RO2COV
425600930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
425700930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
425800930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
425900930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
426000931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
426100930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
426200930608     C                   ELSE
426300930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
426400930608     C                   END
426500931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
426600930525     C                   EXSR      RO2COV
426700930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
426800930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
426900930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
427000930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
427100930525     C* E' IL RITARDO
427200930610     C     WNOCRI        SUB       W98RCI        W98NCI
427300050901     c
427400050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
427500050901     c*  --> trasformo il ritardo in anticipo
427600050901     c                   if        d98cca='2'
427700080116     c***                z-sub     w98nci        w98nci
427800080116     c                   z-add     0             w98nci
427900050901     c                   endif
428000111018     c* Ignoro ritardo se pefect order
428100130221     c                   if        d98xco >*zeros
428200111018     c                   z-add     0             w98nci
428300111018     c                   endif
428400061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
428500061121     c*  e sto leggendo la bolla in arrivo
428600061121     c*  --> trasformo il ritardo in anticipo
428700061121     c                   if        d98cca='1' and d98tbo='A'
428800080116     c***                z-sub     w98nci        w98nci
428900080116     c                   z-add     0             w98nci
429000061121     c                   endif
429100070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
429200070827     c*  mittente, --> consegnato in anticipo
429300070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
429400140403     c                   if        Callar5=' '
429500070827     c                   exsr      LeggiAR5
429600140403     c                   endif
429700070827     c
429800070827     c                   if        dcrpart<>'S'
429900070824     c                   z-sub     w98nci        w98nci
430000070824     c                   endif
430100070827     c
430200070824     c                   endif
430300050907     c
430400050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
430500050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
430600050907     c*  lo trasformo in anticipo perchè non sono in ritardo
430700050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
430800050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
430900050916     c***                if        d98dfc>0
431000070828     c                   if        d98dcm=0  and w98nci>0
431100050907     c                   z-sub     w98nci        w98nci
431200050907     c                   sub       oreILoDOPO    w98nci
431300050907     c                   endif
431400050901     c
431500930525     C                   ELSE                                                   <-- 2 -->
431600070521     c* non do ritardo se la consegna reale cade nel giorno
431700070521     c*  della consegna richiesta se presente IL
431800100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
431900070521     c                             wpodcm<=wpocr1
432000070521     c                   clear                   w98nci
432100100224     c                   else                                                   ---2a -->
432200930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
432300090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
432400090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
432500930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
432600930525     C* E' IL RITARDO O L'ANTICIPO
432700930525     C     W98RCI        SUB       WNOCRF        W98NCI
432800070115     c
432900070115     c* non do ritardo se comunque la consegna reale cade nel giorno
433000070115     c*  della consegna richiesta se presente IL
433100070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
433200070521     c***                          wpodcm<=wpocr1  and w98nci>0
433300070521     c***                clear                   w98nci
433400070521     c***                endif
433500070115     c
433600930525     C                   ELSE                                                   <-- 3 -->
433700930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
433800930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
433900930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
434000930525     C                   END                                                    <-- 3 ---
434100100224     C                   END                                                    <-- 2a---
434200100224
434300100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
434400100224     c                   select
434500100224     c                   when      d98cca='2'
434600100224     c                   z-add     0             w98nci
434700100224
434800100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
434900100224     c*  e sto leggendo la bolla in arrivo
435000100224     c                   when      d98cca='1' and d98tbo='A'
435100100224     c                   z-add     0             w98nci
435200100224     c                   endsl
435300100224     c
435400100224     C                   END                                                    <-- 2 ---
435500070521     C                   END                                                    <-- 1 ---
435600070521     c
435700970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
435800970915     C*/         WAZNCI    IFEQ 'S'
435900970915     C*/         W98NCI    ANDLT*ZEROS
436000970915     C*/                   Z-ADD*ZEROS    W98NCI
436100970915     C*/                   ENDIF
436200970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
436300050922     c* lo faccio solo se non c'e' la data cons richiesta
436400970915     C     D98DTI        IFEQ      *ZEROS
436500050922     c     d98dcr        andeq     *zeros
436600970915     C                   Z-ADD     *ZEROS        W98NCI
436700970915     C                   ENDIF
436800930524     C*
436900930524     C* CONTROLLO CONFORMITA
437000930423     C     W98NCI        IFLE      *ZEROS
437100930524     C                   MOVEL     'S'           D98CCI
437200930420     C                   ELSE
437300930524     C                   MOVEL     'N'           D98CCI
437400930420     C                   END
437500930420     C*
437600930420     C*-------------------------------------------------------------------
437700930420     C* CONFORMITA' CONSEGNA  CLIENTE
437800930420     C*-------------------------------------------------------------------
437900930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
438000930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
438100930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
438200050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
438300050914     c                   if        consideraora='S'
438400050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
438500050914     c                   else
438600050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
438700050914     c                   endif
438800080116     c
438900080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
439000080116     c*  --> trasformo il ritardo =0
439100090827     c                   select
439200090827     c                   when      d98cca='2'
439300080116     c                   z-add     0             w98ncc
439400090827
439500080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
439600080116     c*  e sto leggendo la bolla in arrivo
439700080116     c*  --> trasformo il ritardo =0
439800090827     c                   when      d98cca='1' and d98tbo='A'
439900080116     c                   z-add     0             w98ncc
440000090902     c
440100090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
440200090902     c                   when      wmancaTRAZ='S'
440300090902     c                   z-add     0             w98ncc
440400110923     c* Ignoro ritardo se Perfect Order
440500130221     c                   when      d98xco >*zeros
440600110923     c                   z-add     0             w98ncc
440700090902     c
440800090827     c
440900090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
441000090827     c* sono  in ritardo di 12 ore
441100091009     c                   when      d98tcc=10  and
441200091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
441300091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
441400091014     c*
441500091014     c                             d98hce=1030
441600091014     C                   MOVEL     'OSC  '       WR3TAB
441700091014     C                   eval      WR3HHI=d98hmc
441800091014     C                   EXSR      RO3ORE
441900091014     c                   if        wr3matpom='M' and d98hmc>1030
442000131218     c                   exsr      ctrLAVH1030
442100131218
442200131218     c                   if        wlav1030<>'S'
442300131218     c                   add       12            w98ncc
442400131218     c                   endif
442500131218     c****               add       12            w98ncc
442600091014     c                   endif
442700131218     c
442800091014     c***                z-add     12            w98ncc
442900090827     c                   endsl
443000050914     c
443100930524     C                   ELSE                                                   <-- 1 -->
443200930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
443300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
443400930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
443500930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
443600930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
443700930525     C                   EXSR      RO2COV
443800930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
443900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
444000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
444100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
444200930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
444300931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
444400930525     C                   EXSR      RO2COV
444500930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
444600930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
444700930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
444800930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
444900930525     C* E' IL RITARDO
445000930525     C     WNOCRI        SUB       W98RCC        W98NCC
445100050901     c
445200050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
445300050901     c*  --> trasformo il ritardo in anticipo
445400050901     c                   if        d98cca='2'
445500080116     c***                z-sub     w98ncc        w98ncc
445600080116     c                   z-add     0             w98ncc
445700050901     c                   endif
445800111018     c* Ignoro ritardo se pefect order
445900130221     c                   if        d98xco >*zeros
446000111018     c                   z-add     0             w98ncc
446100111018     c                   endif
446200061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
446300061121     c*  e sto leggendo la bolla in arrivo
446400061121     c*  --> trasformo il ritardo in anticipo
446500061121     c                   if        d98cca='1' and d98tbo='A'
446600080116     c***                z-sub     w98ncc        w98ncc
446700080116     c                   z-add     0             w98ncc
446800061121     c                   endif
446900070824     c
447000070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
447100070827     c*  mittente, --> consegnato in anticipo
447200070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
447300070827     c* SBR per vedere se tassativa
447400070827     c                   if        Callar5=' '
447500070827     c                   EXSR      LeggiAR5
447600070827     c                   endif
447700070827     c                   if        dcrpart<>'S'
447800070824     c                   z-sub     w98ncc        w98ncc
447900070824     c                   endif
448000070824     c                   endif
448100050907     c
448200050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
448300050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
448400050907     c*  lo trasformo in anticipo perchè non sono in ritardo
448500050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
448600050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
448700050916     c***                if        d98dfc>0
448800070828     c                   if        d98dcm=0 and w98ncc>0
448900050907     c                   z-sub     w98ncc        w98ncc
449000050907     c                   sub       oreILoDOPO    w98ncc
449100050907     c                   endif
449200050901     c
449300930525     C                   ELSE                                                   <-- 2 -->
449400070521     c* Non do ritardo se la consegna reale cade nel giorno
449500070521     c*  della consegna richiesta se presente IL
449600100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
449700070521     c                             wpodcm<=wpocr2
449800070521     c                   clear                   w98ncc
449900100224     c                   else                                                   <--2a -->
450000070521     c
450100930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
450200090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
450300090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
450400930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
450500930525     C* E' IL RITARDO O L'ANTICIPO
450600930525     C     W98RCC        SUB       WNOCRF        W98NCC
450700070115     c
450800070115     c* non do ritardo se comunque la consegna reale cade nel giorno
450900070115     c*  della consegna richiesta se presente IL
451000070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
451100070521     c***                          wpodcm<=wpocr2 and w98ncc>0
451200070521     c***                clear                   w98ncc
451300070521     c***                endif
451400070115     c
451500930525     C                   ELSE                                                   <-- 3 -->
451600930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
451700930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
451800930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
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
453500930524     C*
453600930524     C* CONTROLLO CONFORMITA
453700930423     C     W98NCC        IFLE      *ZEROS
453800930524     C                   MOVEL     'S'           D98CCC
453900930420     C                   ELSE
454000930524     C                   MOVEL     'N'           D98CCC
454100930420     C                   END
454200930420     C*
454300930420     C*-------------------------------------------------------------------
454400930420     C* CONFORMITA' RESA INTERNA
454500930420     C*-------------------------------------------------------------------
454600930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
454700930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
454800930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
454900050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
455000050914     c                   if        consideraora='S'
455100050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
455200050914     c                   else
455300050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
455400050914     c                   endif
455500080116     c
455600080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
455700090827     c                   select
455800090827     c                   when      d98cca='2' and w98nri>0
455900080116     c                   z-add     0             w98nri
456000090827     c
456100080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
456200080116     c*  e sto leggendo la bolla in arrivo
456300090827     c                   when      d98cca='1' and d98tbo='A' and
456400080116     c                                            w98nri>0
456500080116     c                   z-add     0             w98nri
456600110923     c
456700110923     c* Ignoro ritardo se Perfect Order
456800130221     c                   when      d98xco >*zeros
456900110923     c                   z-add     0             w98nri
457000110923     c
457100090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
457200090827     c* sono  in ritardo di 12 ore
457300091009     c                   when      d98tci=10  and
457400091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
457500091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
457600091014     c*
457700091014     c                             d98hci=1030
457800091014     C                   MOVEL     'OSC  '       WR3TAB
457900091014     C                   eval      WR3HHI=d98hmc
458000091014     C                   EXSR      RO3ORE
458100091014     c                   if        wr3matpom='M' and d98hmc>1030
458200131218     c                   exsr      ctrLAVH1030
458300131218
458400131218     c                   if        wlav1030<>'S'
458500091014     c                   add       12            w98nri
458600091014     c                   endif
458700131218     c                   endif
458800091014     c
458900091014     c***                z-add     12            w98nri
459000090827     c                   endsl
459100090827
459200930524     C                   ELSE                                                   <-- 1 -->
459300930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
459400930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
459500930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
459600931206     C     WPODCM        ANDLE     WPOCR1
459700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
459800930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
459900930525     C                   Z-ADD     WPODCM        WR2POI            9 0
460000931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
460100930525     C                   EXSR      RO2COV
460200930525     C     WR2NOV        IFNE      *ZEROS
460300930525     C     WR2NOV        MULT      -1            W98NRI
460400930525     C                   END
460500930524     C                   ELSE                                                   <-- 2 -->
460600930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
460700930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
460800930524     C     WPODCM        IFLT      WPOCRI
460900930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
461000930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
461100930524     C                   ELSE
461200931206     C                   Z-ADD     WPOCR1        WPORFI
461300930524     C                   Z-ADD     WPODCM        WPORFF
461400930524     C                   END
461500930524     C* CALCOLO ORE DI DIFFERENZA
461600930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
461700930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
461800930524     C                   Z-ADD     WPORFI        WR2POI            9 0
461900930524     C                   Z-ADD     WPORFF        WR2POF            9 0
462000930524     C                   EXSR      RO2COV
462100930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
462200050902     c
462300050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
462400100224     c                   if        d98cca='2' and w98nri>0
462500100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
462600080116     c***                z-sub     w98nri        w98nri
462700080116     c                   z-add     0             w98nri
462800050902     c                   endif
462900111018     c* Ignoro ritardo se pefect order
463000130221     c                   if        d98xco >*zeros
463100111018     c                   z-add     0             w98nri
463200111018     c                   endif
463300061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
463400061121     c*  e sto leggendo la bolla in arrivo
463500100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
463600100224     c***                if        d98cca='1' and d98tbo='A' and
463700100224     c***                                         w98nri>0 and wpodcm<wpocri
463800080116     c***                z-sub     w98nri        w98nri
463900080116     c                   z-add     0             w98nri
464000061121     c                   endif
464100070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
464200070827     c*  mittente, --> consegnato in anticipo
464300070828     c                   if        d98tcr=' ' and d98nsp>0  and
464400070828     c                                            w98nri>0 and wpodcm<wpocri
464500070827     c                   if        CallAR5=' '
464600070827     c                   EXSR      LeggiAR5
464700070824     c                   endif
464800070827     c                   if        dcrpart<>'S'
464900070824     c                   z-sub     w98nri        w98nri
465000070824     c                   endif
465100070824     c                   endif
465200050907     c
465300050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
465400050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
465500050907     c*  lo trasformo in anticipo perchè non sono in ritardo
465600050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
465700050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
465800050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
465900050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
466000050907     c                   z-sub     w98nri        w98nri
466100050907     c                   sub       oreILoDOPO    w98nri
466200050907     c                   endif
466300050907     c
466400050907     C                   END                                                    <-- 2 ---
466500930524     C                   END                                                    <-- 1 ---
466600930420     C* CALCOLO LE ORE DI RESA
466700930420     C* CONTROLLO CONFORMITA
466800930423     C     W98NRI        IFLE      *ZEROS
466900930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
467000930420     C                   ELSE
467100930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
467200930420     C                   END
467300930420     C*
467400930420     C*-------------------------------------------------------------------
467500930420     C* CONFORMITA' RESA CLIENTE
467600930420     C*-------------------------------------------------------------------
467700930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
467800930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
467900930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
468000050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
468100050914     c                   if        consideraora='S'
468200050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
468300050914     c                   else
468400050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
468500050914     c                   endif
468600090827     c
468700080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
468800090827     c                   select
468900090827     c                   when      d98cca='2' and w98nrc>0
469000080116     c                   z-add     0             w98nrc
469100090827
469200080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
469300080116     c*  e sto leggendo la bolla in arrivo
469400090827     c                   when      d98cca='1' and d98tbo='A' and
469500080116     c                                            w98nrc>0
469600080116     c                   z-add     0             w98nrc
469700110923     c
469800110923     c* Ignoro ritardo se Perfect Order
469900130221     c                   when      d98xco >*zeros
470000110923     c                   z-add     0             w98nrc
470100090827     c
470200090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
470300090827     c* sono  in ritardo di 12 ore
470400091009     c                   when      d98tcc=10  and
470500091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
470600091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
470700091014     c                             d98hce=1030
470800091014     C                   MOVEL     'OSC  '       WR3TAB
470900091014     C                   eval      WR3HHI=d98hmc
471000091014     C                   EXSR      RO3ORE
471100091014     c                   if        wr3matpom='M' and d98hmc>1030
471200131218     c                   exsr      ctrLAVH1030
471300131218
471400131218     c                   if        wlav1030<>'S'
471500091014     c                   add       12            w98nrc
471600091014     c                   endif
471700131218     c                   endif
471800091014     c
471900091014     c**                 z-add     12            w98nrc
472000090827     c                   endsl
472100090827     c
472200930524     C                   ELSE                                                   <-- 1 -->
472300930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
472400930524     C* DI DIFFERENZA SONO UGUALI A ZERO
472500930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
472600931206     C     WPODCM        ANDLE     WPOCR2
472700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
472800930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
472900930525     C                   Z-ADD     WPODCM        WR2POI            9 0
473000931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
473100930525     C                   EXSR      RO2COV
473200930525     C     WR2NOV        IFNE      *ZEROS
473300930525     C     WR2NOV        MULT      -1            W98NRC
473400930525     C                   END
473500930524     C                   ELSE                                                   <-- 2 -->
473600930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
473700930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
473800930524     C     WPODCM        IFLT      WPOCRI
473900930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
474000930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
474100930524     C                   ELSE
474200931206     C                   Z-ADD     WPOCR2        WPORFI
474300930524     C                   Z-ADD     WPODCM        WPORFF
474400930524     C                   END
474500930524     C* CALCOLO ORE DI DIFFERENZA
474600930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
474700930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
474800930524     C                   Z-ADD     WPORFI        WR2POI            9 0
474900930524     C                   Z-ADD     WPORFF        WR2POF            9 0
475000930524     C                   EXSR      RO2COV
475100930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
475200050902     c
475300050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
475400100224     c                   if        d98cca='2' and w98nrc>0
475500100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
475600080116     c***                z-sub     w98nrc        w98nrc
475700080116     c                   z-add     0             w98nrc
475800050902     c                   endif
475900111018     c* Ignoro ritardo se pefect order
476000130221     c                   if        d98xco >*zeros
476100111018     c                   z-add     0             w98nrc
476200111018     c                   endif
476300070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
476400061121     c*  e sto leggendo la bolla in arrivo
476500100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
476600100224     c***                if        d98cca='1' and d98tbo='A' and
476700100224     c***                                         w98nrc>0 and wpodcm<wpocri
476800080116     c***                z-sub     w98nrc        w98nrc
476900080116     c                   z-add     0             w98nrc
477000061121     c                   endif
477100070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
477200070827     c*  mittente, --> consegnato in anticipo
477300070828     c                   if        d98tcr=' ' and d98nsp>0 and
477400070828     c                                            w98nrc>0 and wpodcm<wpocri
477500070827     c                   if        CallAR5=' '
477600070827     c                   EXSR      LeggiAR5
477700070827     c                   endif
477800070827     c                   if        dcrpart<>'S'
477900070824     c                   z-sub     w98nrc        w98nrc
478000070824     c                   endif
478100070824     c                   endif
478200050907     c
478300050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
478400050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
478500050907     c*  lo trasformo in anticipo perchè non sono in ritardo
478600050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
478700050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
478800050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
478900050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
479000050907     c                   z-sub     w98nrc        w98nrc
479100050907     c                   sub       oreILoDOPO    w98nrc
479200050907     c                   endif
479300050907     c
479400050907     C                   END                                                    <-- 2 ---
479500930524     C                   END                                                    <-- 1 ---
479600930420     C* CALCOLO LE ORE DI RESA
479700930420     C* CONTROLLO CONFORMITA
479800930423     C     W98NRC        IFLE      *ZEROS
479900930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
480000930420     C                   ELSE
480100930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
480200930420     C                   END
480300930420     C*
480400930420     C                   ENDSR
480500070827     c
480600131218     c********************************************************************
480700131218     c     CtrlavH1030   BEGSR
480800131218     c********************************************************************
480900131218     c                   clear                   wlav1030          1
481000140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
481100131218     c                   z-add     1             xx                4 0
481200131218     c                   dow       evc(xx)<>*blanks
481300140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
481400140127     c                   if        evd(xx)=d98dcm
481500131218     c                   if        evo(xx)<=1030
481600131218     c                   eval      wlav1030='S'
481700131218     c                   leave
481800131218     c                   endif
481900131218     c                   endif
482000131218     c                   add       1             xx
482100131218     c                   enddo
482200131218     c                   ENDSR
482300070827     c********************************************************************
482400070827     c* Controllo record GEN di FIAR5
482500070827     c********************************************************************
482600070827     c     LeggiAR5      BEGSR
482700070827     c                   clear                   DCRPART           1
482800141104     c                   clear                   dar5gen
482900141104     c                   clear                   gc1PART           2
483000141104     c                   clear                   gc2PART           2
483100070827     c     kar5          chain     fiar501l
483200070827    1c                   if        not %found
483300070827     c                   eval      dcrpart='S'
483400141104     c                   eval      gc1part=d98gc1
483500141104     c                   eval      gc2part=d98gc2
483600070827   x1c                   else
483700070827     c* i dati devono essere quelli immessi dalla partenza
483800070827     c                   movel     ar5uni        dar5gen
483900070827     c
484000070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
484100070827     c*  ma il record GEN è stato scritto per altri motivi
484200070827    2c                   if        §ar5mdp='S'
484300070827     c                   movel     d98dcr        alfadcr           8
484400141104     c                   eval      gc1part=§ar5gc1
484500141104     c                   eval      gc2part=§ar5gc2
484600070827     c
484700070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
484800070827     c                   eval      dcrpart='S'
484900070827    3c                   endif
485000070827     c
485100070827   x2c                   else
485200070827     c                   eval      dcrpart='S'
485300141106     c                   eval      gc1part=d98gc1
485400141106     c                   eval      gc2part=d98gc2
485500070827    2c                   endif
485600070827    1c                   endif
485700070827     c
485800140402     c* Memorizzo il flag pickup 2
485900140402     c                   eval      wpick2=§AR5PK2
486000140402     c
486100150401     c*
486200150414     c**                 if        d98cas>0 or d98ffd<>' ' or
486300150414     c**                           d98tc1='A'  or d98tc2='A' or
486400150414     c**                           d98tc1='S'  or d98tc2='S'
486500150401     c     kar4          chain     fiar401l
486600150401     c                   if        %found(fiar401l)
486700150401     c                   eval      dsbl4L=ar4not
486800150401     c                   endif
486900150401     c* Solo per fermo deposito verifico se inviato alert
487000150401     c                   if        d98ffd<>' '
487100150401     c                   clear                   fnlv82ds
487200150401     c                   eval      ILV82AAS=d98aas
487300150401     c                   eval      ILV82LNP=d98lnp
487400150401     c                   eval      ILV82NRS=d98nrs
487500150401     c                   eval      ILV82NSP=d98nsp
487600150401     c                   eval      ILV82TIPA='FD'
487700150401     c                   call      'FNLV82R'
487800150401     c                   parm                    fnlv82ds
487900150401     c                   endif
488000150414     c**                 endif
488100150401
488200150401     c                   eval      callar5='S'
488300150401
488400070827     c                   ENDSR
488500930913     C********************************************************************
488600930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
488700930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
488800930913     C*            DATA/ORA (FINE RANGE)
488900930913     C********************************************************************
489000930913     C     CALCNS        BEGSR
489100930914     C*
489200930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
489300930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
489400930914     C     WPODTI        IFEQ      *ZEROS
489500960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
489600930914     C                   ELSE
489700930914     C                   Z-ADD     WPODTI        WPOIRC
489800930914     C                   END
489900930914     C*
490000930914     C* SE NON ANCORA CONSEGNATA
490100930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
490200050922     c* in realtà nella data consegna c'e' la data fine calcolo
490300050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
490400930914     C     WPODCM        IFEQ      *ZEROS
490500050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
490600930914     C                   ELSE
490700930916     C                   Z-ADD     WPODCM        WPOFRC
490800930914     C                   END
490900930914     C*
491000930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
491100930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
491200930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
491300930914     C     WPODFC        ANDLT     WPOIRC
491400930914     C                   ELSE
491500930914     C*
491600930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
491700930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
491800930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
491900930914     C     WPODFC        ANDGT     WPOFRC
492000930914     C                   ELSE
492100930916     C*
492200930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
492300930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
492400930916     C     WPODFC        ANDGT     WPOFRC
492500930916     C                   ELSE
492600930916     C*
492700930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
492800930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
492900930916     C     WPODFC        ANDLT     WPOIRC
493000930916     C                   ELSE
493100930914     C*
493200930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
493300930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
493400930914     C     WPODIC        IFNE      *ZEROS
493500930914     C     WPODIC        ANDGT     WPOFRC
493600930914     C                   Z-ADD     WPOFRC        WPODIC
493700930914     C                   ENDIF
493800930914     C*
493900930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
494000930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
494100930914     C     WPODFC        IFGT      WPOFRC
494200930914     C                   Z-ADD     WPOFRC        WPODFC
494300930914     C                   ENDIF
494400930914     C*
494500930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
494600930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
494700930914     C     WPODIC        IFNE      *ZEROS
494800930914     C     WPODIC        ANDLT     WPOIRC
494900930914     C                   Z-ADD     WPOIRC        WPODIC
495000930914     C                   ENDIF
495100930914     C*
495200930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
495300930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
495400930914     C     WPODFC        IFLT      WPOIRC
495500930914     C                   Z-ADD     WPOIRC        WPODFC
495600930914     C                   ENDIF
495700930914     C*
495800930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
495900931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
496000930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
496100930916     C*
496200931210     C***********WPODIC    IFNE *ZEROS
496300931210     C***********WPODFC    ANDNE*ZEROS
496400931210     C*********************ADD  12        WPODFC
496500931210     C*********************ENDIF
496600930914     C     WPODIC        IFEQ      *ZEROS
496700930914     C     WPODFC        ANDNE     *ZEROS
496800931130     C                   Z-ADD     WPODFC        WPODIC
496900931210     C*********************ADD  12        WPODFC
497000930916     C                   ENDIF
497100930913     C*
497200930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
497300930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
497400930914     C* INTERNA
497500050908     c                   eval      Usacons='S'
497600930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
497700930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
497800931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
497900931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
498000940324     C                   EXSR      RO9COV
498100931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
498200930914     C* CLIENTE
498300930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
498400931210     C                   EXSR      RO9COV
498500931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
498600050908     c                   eval      Usacons=' '
498700930914     C*
498800930916     C                   ENDIF                                                  <-- 5 ---
498900930916     C                   ENDIF                                                  <-- 4 ---
499000930916     C                   ENDIF                                                  <-- 3 ---
499100930914     C                   ENDIF                                                  <-- 2 ---
499200930914     C                   ENDIF                                                  <-- 1 ---
499300930913     C*
499400930913     C                   ENDSR
499500930325     C********************************************************************
499600930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
499700930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
499800930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
499900930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
500000930325     C********************************************************************
500100930325     C     RO1CDP        BEGSR
500200930414     C*
500300931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
500400931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
500500931126     C*
500600931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
500700931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
500800150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
500900150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
501000931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
501100060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
501200930326     C*
501300930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
501400931130     C                   Z-ADD     WR1POI        WR1POO            9 0
501500930325     C*
501600930325     C* CICLO FINO A CHE:
501700930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
501800930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
501900930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
502000930406     C     WR1POO        ANDLT     WPOLIM
502100930414     C*
502200930407     C                   Z-ADD     WR1POO        WPOROU            9 0
502300930325     C                   MOVEL     'N'           WSLITT            1
502400930414     C*
502500930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
502600930414     C*
502700060420     C**   WSLITT        IFEQ      'N'                                          *ORA VALIDA
502800060420     C**                 EXSR      CHKGIA                                       *CONTR.GIACENZA
502900060420     C*+                 END
503000930414     C*
503100060420     C**   WSLITT        IFEQ      'N'                                          *ORA VALIDA
503200060420     C**                 EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
503300060420     C**                 END
503400930414     C*
503500060420     C**   WSLITT        IFEQ      'N'                                          *ORA VALIDA
503600060420     C**                 EXSR      CHKCEPA                                      *CONTR.LASC.AVVISO
503700060420     C**                 END
503800060420     C*
503900931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
504000930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
504100930414     C                   END
504200930511     C*
504300931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
504400930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
504500930511     C                   END
504600930414     C*
504700930325     C     WSLITT        IFEQ      'N'
504800930423     C                   SUB       12            WR1NOI            5 0
504900930325     C                   END
505000931130     C                   ADD       12            WR1POO            9 0
505100930414     C*
505200931223     C                   ENDDO                                                  <-- 1 ---
505300931126     C*
505400931126     C                   ELSE                                                   <-- 0 -->
505500931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
505600931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
505700931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
505800960129     C     WPOLAF        IFGT      WPODCG
505900960129     C                   Z-ADD     WPOLAF        WTM090            9 0
506000960129     C                   ELSE
506100960129     C                   Z-ADD     WPODCG        WTM090
506200960129     C                   ENDIF
506300960129     C     WPOGIF        IFGT      WTM090
506400960129     C                   Z-ADD     WPOGIF        WTM090
506500960129     C                   ENDIF
506600060420     C     WPOpaif       IFGT      WTM090
506700060420     C                   Z-ADD     WPOpaif       WTM090
506800060420     C                   ENDIF
506900960129     C*
507000960129     C     12            ADD       WTM090        WR1POO
507100960129     C                   ENDIF                                                  <-- 0 ---
507200930512     C*
507300931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
507400931202     C     WR1POO        SUB       12            WR7POI
507500931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
507600931202     C                   ADD       12            WR7POO
507700931202     C     WR7POO        IFGT      WR1POO
507800931202     C                   Z-ADD     WR7POO        WR1POO
507900931202     C                   END
508000930406     C*
508100930325     C                   ENDSR
508200930414     C********************************************************************
508300930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
508400930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
508500930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
508600930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
508700930414     C********************************************************************
508800930414     C     RO2COV        BEGSR
508900091014     c                   clear                   salvaEveL
509000091014     c                   clear                   salvaEveG
509100931130     C*
509200930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
509300930414     C                   Z-ADD     *ZEROS        WR2NOV
509400930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
509500931130     C                   Z-ADD     WR2POI        WR2POW
509600930414     C*
509700930414     C* CICLO FINO A CHE:
509800930414     C*   - DATA INIZIO SUPERATO DATA FINE
509900930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
510000931223     C     WR2POW        ANDLT     WPOLIM
510100930414     C*
510200930414     C                   Z-ADD     WR2POW        WPOROU
510300930414     C                   MOVEL     'N'           WSLITT            1
510400930414     C*
510500930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
510600930414     C*
510700930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
510800930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
510900930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
511000930414     C                   END
511100930414     C*
511200930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
511300930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
511400930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
511500930618     C                   END
511600060420     c
511700060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
511800060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
511900060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
512000060420     C                   END
512100060420     c
512200931201     C*
512300931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
512400931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
512500931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
512600931201     C                   END
512700060418     c
512800060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
512900060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
513000060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
513100060418     C                   END
513200930414     C*
513300930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
513400930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
513500930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
513600930414     C                   END
513700930511     C*
513800930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
513900930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
514000930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
514100930511     C                   END
514200930414     C*
514300930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
514400930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
514500930414     C     WSLITT        IFEQ      'N'
514600930423     C                   ADD       12            WR2NOV            5 0
514700930414     C                   END
514800931130     C                   ADD       12            WR2POW            9 0
514900930414     C*
515000931223     C                   ENDDO                                                  <-- 1 ---
515100930414     C*
515200930414     C                   ENDSR
515300941228     C********************************************************************
515400941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
515500941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
515600941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
515700941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
515800941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
515900941228     C********************************************************************
516000941228     C     RO2COP        BEGSR
516100941228     C*
516200941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
516300941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
516400941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
516500941228     C                   Z-ADD     *ZEROS        WR2NOV
516600941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
516700941228     C                   Z-ADD     WR2POI        WR2POW
516800941228     C*
516900941228     C* CICLO FINO A CHE:
517000941228     C*   - DATA INIZIO SUPERATO DATA FINE
517100941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
517200941228     C*
517300950215     C                   Z-ADD     WR2POW        WPOROU
517400941228     C                   MOVEL     'N'           WSLITT
517500941228     C* TRASFORMA IL PROGR. ORE IN DATA
517600950215     C*******              Z-ADDWR2POW    WORE
517700950215     C*******              EXSR OREDAT
517800950215     C*******              Z-ADDWDAT      W12DAT  80
517900950215     C*******              Z-ADDWRES      W12RES  30
518000941228     C*
518100941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
518200941228     C*
518300941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
518400941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
518500941228     C     WSLITT        IFEQ      'N'
518600941228     C                   ADD       12            WR2NOV            5 0
518700941228     C                   END
518800941228     C                   ADD       12            WR2POW            9 0
518900941228     C*
519000941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
519100941228     C* ENTRI IN LOOP
519200941228     C     WR2NOV        IFGT      999
519300941228     C                   LEAVE                                                  *VA A ENDDO
519400941228     C                   END
519500941228     C*
519600941228     C                   ENDDO                                                  <-- 1 ---
519700941228     C*
519800941228     C                   ENDSR
519900930510     C********************************************************************
520000930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
520100930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
520200930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
520300930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
520400050905     c*                     il num ore deve essere un multiplo di 12
520500050905     c*                     oppure 1 ora
520600930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
520700930510     C********************************************************************
520800930510     C     RO4CDP        BEGSR
520900930510     C*
521000930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
521100930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
521200050905     c                   movel     '1'           wgiro             1
521300930510     C*
521400930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
521500930510     C                   Z-ADD     WR4POI        WR4POO            9 0
521600930510     C*
521700930510     C* CICLO FINO A CHE:
521800930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
521900930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
522000930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
522100930510     C     WR4POO        ANDLT     WPOLIM
522200930510     C*
522300141106     c                   if        nosomma<>'N'
522400931130     C                   ADD       12            WR4POO            9 0
522500141106     c                   endif
522600930510     C                   Z-ADD     WR4POO        WPOROU            9 0
522700930510     C                   MOVEL     'N'           WSLITT            1
522800930510     C*
522900930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
523000050905     c
523100050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
523200930510     C     WSLITT        IFEQ      'N'
523300050905     c
523400050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
523500050905     c*  se cado in giorno non festivo
523600050905     c                   if        wr4noi=1 and wgiro='1'
523700050905     c                   sub       12            wr4poo
523800050905     c                   add       wr4noi        wr4poo
523900050905     c                   endif
524000050905     c*
524100050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
524200050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
524300050905     c*  calcolate dal pgm
524400050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
524500050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
524600050905     c*  del giorno sucessivo non festivo
524700050905     c                   if        wr4noi=1 and wgiro<>'1'
524800050905     c                   add       wr4noi        wr4poo
524900050905     c                   endif
525000050905     c
525100930510     C                   SUB       12            WR4NOI            5 0
525200930510     C                   END
525300141106     c
525400141106     c                   if        nosomma= 'N'
525500141106     C                   ADD       12            WR4POO            9 0
525600141106     c                   endif
525700930510     C*
525800050905     c                   eval      wgiro='2'
525900931223     C                   ENDDO                                                  <-- 1 ---
526000930510     C*
526100930510     C                   ENDSR
526200931130     C********************************************************************
526300931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
526400931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
526500931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
526600931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
526700931130     C********************************************************************
526800931130     C     RO5COV        BEGSR
526900931130     C*
527000931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
527100060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
527200060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
527300060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
527400931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
527500931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
527600931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
527700931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
527800931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
527900931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
528000931130     C                   Z-ADD     *ZEROS        WR5NOV
528100940714     C*
528200940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
528300940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
528400931207     C*
528500931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
528600940908     C     WIC           IFEQ      'I'
528700090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
528800090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
528900931207     C                   ELSE
529000090827     C*****              Z-ADD     D98TCC        WPOTCT
529100090827     C                   Z-ADD     w99TCC        WPOTCT
529200931207     C                   END
529300931130     C*
529400931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
529500060420     C                   clear                   COV
529600931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
529700931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
529800931207     C                   Z-ADD     2             CO                3 0
529900931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
530000931207     C                   EXSR      RO5SUB
530100981102     C                   DO        99            IC                3 0
530200931207     C     LAF(IC)       IFNE      *ZEROS
530300931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
530400931207     C                   EXSR      RO5SUB
530500931207     C                   ENDIF
530600931207     C                   ENDDO
530700060420     c
530800060420     C                   DO        99            IC                3 0
530900060420     C     paif(IC)      IFNE      *ZEROS
531000060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
531100060420     C                   EXSR      RO5SUB
531200060420     C                   ENDIF
531300060420     C                   ENDDO
531400931207     C* ORDINA LA SCHIERA COV
531500931207     C                   SORTA     COV
531600931130     C*
531700931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
531800931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
531900981102     C                   DO        98            IC                             --- 1 -->
532000931207     C     IC            ADD       1             I1C               3 0
532100931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
532200931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
532300931207     C     COV(I1C)      OREQ      *ZEROS
532400931207     C                   ELSE
532500931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
532600931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
532700931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
532800931130     C                   EXSR      RO2COV
532900931207     C                   ADD       WR2NOV        WR5NOV            9 0
533000931209     C* SE NON SONO IN DATA CONSEGNA
533100931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
533200931207     C     WR5NOV        IFLT      WPOTCT
533300931209     C     COV(I1C)      ANDNE     WPODCM
5334009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5335009709111009 C     WPOTCT        SUB       12            WR5NOV
533600931130     C                   END
533700931207     C                   ENDIF                                                  <-- 2 ---
533800931207     C                   ENDDO                                                  <-- 1 ---
533900940714     C                   ENDIF                                                  <-- 0 ---
534000931130     C*
534100931130     C                   ENDSR
534200931207     C********************************************************************
534300931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
534400931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
534500931207     C********************************************************************
534600931207     C     RO5SUB        BEGSR
534700931207     C*
534800931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
534900931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
535000931207     C     WR5COV        ANDLT     WR5POF
535100931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
535200931207     C                   MOVEL     'N'           WR5PRE            1
535300981102     C*---                 DO   30        I
535400981102     C                   DO        99            I
535500931207     C     WR5COV        IFEQ      COV(I)
535600931207     C                   MOVEL     'S'           WR5PRE
535700931207     C                   LEAVE
535800931207     C                   ENDIF
535900931207     C                   ENDDO
536000931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
536100931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
536200931207     C                   ADD       1             CO                3 0
536300931207     C                   Z-ADD     WR5COV        COV(CO)
536400931207     C                   ENDIF                                                  <-- 2 ---
536500931207     C                   ENDIF                                                  <-- 1 ---
536600931207     C*
536700931207     C                   ENDSR
536800931201     C********************************************************************
536900060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
537000931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
537100931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
537200931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
537300931201     C********************************************************************
537400931201     C     RO6CDT        BEGSR
537500931201     C*
537600931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
537700931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
537800931201     C*
537900931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
538000931201     C                   Z-ADD     WR6POI        WR6POO            9 0
538100931201     C*
538200931201     C* CICLO FINO A CHE:
538300931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
538400931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
538500931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
538600931201     C     WR6POO        ANDLT     WPOLIM
538700931201     C*
538800931201     C                   Z-ADD     WR6POO        WPOROU            9 0
538900931201     C                   MOVEL     'N'           WSLITT            1
539000931201     C*
539100931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
539200931201     C*
539300931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
539400931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
539500931201     C                   END
539600931201     C*
539700931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
539800931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
539900931201     C                   END
540000931201     C*
540100931201     C     WSLITT        IFEQ      'N'
540200931201     C                   SUB       12            WR6NOI            5 0
540300931201     C                   END
540400931201     C                   ADD       12            WR6POO            9 0
540500931201     C*
540600931223     C                   ENDDO                                                  <-- 1 ---
540700931201     C*
540800931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
540900931202     C     WR6POO        SUB       12            WR7POI
541000931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
541100931207     C                   ADD       12            WR7POO
541200931202     C     WR7POO        IFGT      WR6POO
541300931201     C                   Z-ADD     WR7POO        WR6POO
541400931202     C                   END
541500931201     C*
541600931201     C                   ENDSR
541700931201     C********************************************************************
541800060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
541900060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
542000060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
542100060407     C*            INPUT:  -WR7POI (PROGR.ORE)
542200931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
542300931201     C********************************************************************
542400931201     C     RO7TCF        BEGSR
542500060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
542600060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
542700060407     C                   MOVEL     'N'           WSLITT
542800931201     C*
542900931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
543000931201     C                   Z-ADD     WR7POI        WR7POO            9 0
543100931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
543200931201     C* COINCIDA CON UN TURNO DI CHIUSURA
543300060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
543400931201     C     D98GC2        ANDEQ     *BLANKS
543500060407   x1C                   ELSE                                                   <-- 1 -->
543600931214     C*
543700931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
543800931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
543900931214     C*
544000931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
544100931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
544200931223     C     WPOROU        ANDLT     WPOLIM
544300931214     C* MEMORIZZO INIZIO E FINE RANGE
544400931202     C     WPOTI         IFEQ      *ZEROS
544500931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
544600931202     C                   END
544700931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
544800931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
544900931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
545000931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
545100931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
545200931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
545300931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
545400931201     C                   MOVEL     'N'           WSLITT
545500931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
545600060407     c* Se non trovat, controllo se c'e' evento part che giustifica
545700060407     c                   if        wslitt='N'
545800060407     c                   exsr      datcepg
545900060407     c                   endif
546000060407     c
546100931201     C                   ENDDO                                                  <-- 2 ---
546200931214     C*
546300931201     C                   Z-ADD     WPOROU        WR7POO
546400931214     C*
546500931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
546600931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
546700931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
546800931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
546900931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
547000931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
547100931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
547200931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
547300931209     C                   ELSE                                                   <-- 3 -->
547400931209     C     WIC           IFEQ      'I'
547500931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
547600931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
547700931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
547800931201     C                   ELSE
547900931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
548000931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
548100931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
548200931201     C                   ENDIF
548300931209     C                   ENDIF                                                  <-- 3 ---
548400931209     C                   ENDIF                                                  <-- 2 ---
548500931214     C*
548600060407    1C                   ENDIF                                                  <-- 1 ---
548700060407     c
548800060407     c* Se non trovato turno di chiusura, verifico se c'e'
548900060407     c*  evento particolare che GIUSTIFICA
549000060407     c                   if        wpoti=0
549100060418     C                   EXSR      RO7CEPG
549200060407     c                   endif
549300931201     C*
549400931201     C                   ENDSR
549500060406     C********************************************************************
549600060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
549700060406     c*                        che giustifica
549800060407     C*            INPUT:-WR7POI (PROGR.ORE)
549900060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
550000060406     C********************************************************************
550100060407     C     RO7CEPG       BEGSR
550200060406     C*
550300060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
550400060407     C                   Z-ADD     WR7POI        WR7POO            9 0
550500060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
550600060406     C* coincide
550700060406    1C                   if        wevpag='S'                                   <-- 1 -->
550800060406     C*
550900060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
551000060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
551100060406     C                   MOVEL     'N'           WSLITT
551200060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
551300060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
551400060406     C*
551500060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
551600060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
551700060406     C     WPOROU        ANDLT     WPOLIM
551800060406     C* MEMORIZZO INIZIO E FINE RANGE
551900060406    3C     WPOTI         IFEQ      *ZEROS
552000060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
552100060406    3C                   END
552200060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
552300060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
552400060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
552500060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
552600060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
552700060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
552800060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
552900060406     C                   MOVEL     'N'           WSLITT
553000060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
553100060406     C* Se non trovato,
553200060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
553300060406    3c                   if        wslitt='N'
553400060406     c                   exsr      DATTUC
553500060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
553600060406    2C                   ENDDO                                                  <-- 2 ---
553700060406     C*
553800060407     C                   Z-ADD     WPOROU        WR7POO
553900060406     C*
554000060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
554100060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
554200060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
554300060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
554400060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
554500060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
554600060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
554700060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
554800060406   x3C                   ELSE                                                   <-- 3 -->
554900060406    4C     WIC           IFEQ      'I'
555000060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
555100060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
555200060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
555300060406   x4C                   ELSE
555400060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
555500060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
555600060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
555700060406    4C                   ENDIF
555800060406    3C                   ENDIF                                                  <-- 3 ---
555900060406    2C                   ENDIF                                                  <-- 2 ---
556000060406     C*
556100060406    1C                   ENDIF                                                  <-- 1 ---
556200060406     C*
556300060406     C                   ENDSR
556400931207     C********************************************************************
556500931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
556600931207     C*            INPUT:  -WR8POI (PROGR.ORE)
556700931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
556800931207     C********************************************************************
556900931207     C     RO8FES        BEGSR
557000130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
557100130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
557200931207     C*
557300931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
557400130411     C     WR8POI        SUB       12            WR8POO            9 0
557500931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
557600931207     C                   MOVEL     'N'           WSLITT
557700130411     c
557800130411     c* Non salto i festivi ???
557900130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
558000130411     c
558100931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
558200130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
558300130411     C     WPOROU        ANDGT     WPODSP
558400130411     C                   SUB       12            WPOROU
558500130411     C                   MOVEL     'N'           WSLITT
558600130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
558700130411     C                   ENDDO                                                  <-- 2 ---
558800130411     c
558900130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
559000130411     C     D98GC2        ANDEQ     *BLANKS
559100130411   x1C                   ELSE                                                   <-- 1 -->
559200130411     c* eseguo controllo per i turni di chiusura
559300130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
559400130411     C*
559500130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
559600130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
559700130411     C     WPOROU        ANDgT     WPOdsp
559800130411     C* MEMORIZZO INIZIO E FINE RANGE
559900130411     C     WPOTf         IFEQ      *ZEROS
560000130411     C                   z-add     wporou        WPOTf
560100130411     C                   END
560200140826     C     WPOROU        SUB       wnoadd        WPOROU
560300130411     c
560400130411     C                   MOVEL     'N'           WSLITT
560500130411     c                   exsr      dattuc
560600130411     c                   if        wslitt='N'
560700130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
560800140826     c* Se slitta per festivo --> tolgo sempre 12 ore
560900140826     c                   z-add     12            WNOADD
561000130411     c                   endif
561100130411     C* SLITTA
561200140826     C     wporou        add       12            WPOTi
561300130411    2C                   ENDDO                                                  <-- 2 ---
561400130411     c
561500130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
561600130411     c
561700130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
561800130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
561900130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
562000130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
562100130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
562200130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
562300130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
562400130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
562500130411   x3C                   ELSE                                                   <-- 3 -->
562600130411    4C     WIC           IFEQ      'I'
562700130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
562800130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
562900130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
563000130411   x4C                   ELSE
563100130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
563200130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
563300130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
563400130411    4C                   ENDIF
563500130411    3C                   ENDIF                                                  <-- 3 ---
563600130411    2C                   ENDIF                                                  <-- 3 ---
563700130411     C*
563800130411    1C                   ENDIF                                                  <-- 1 ---
563900130411
564000130411     C**   WPOROU        ADD       12            WR8POO
564100130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
564200130411     c                   z-add     wr8poi        wr8poo
564300931207     C*
564400931207     C                   ENDSR
564500931210     C********************************************************************
564600931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
564700940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
564800931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
564900931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
565000931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
565100931210     C********************************************************************
565200931210     C     RO9COV        BEGSR
565300931210     C*
565400931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
565500931210     C                   Z-ADD     *ZEROS        WR9NOV
565600931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
565700931210     C                   Z-ADD     WR9POI        WR9POW
565800931210     C* CICLO FINO A CHE:
565900931210     C*   - DATA INIZIO SUPERATO DATA FINE
566000931223     C*   - RAGGIUNTO LIMITE MASSIMO
566100931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
566200931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
566300931223     C     WR9POW        ANDLT     WPOLIM
566400931210     C*
566500931210     C                   Z-ADD     WR9POW        WPOROU
566600931210     C                   MOVEL     'N'           WSLITT            1
566700940324     C* FESTIVI
566800931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
566900940324     C* GIACENZA
567000931214     C     WSLITT        IFEQ      'N'
567100941229£$   C     WPOAGC        IFNE      *ZEROS
567200940324£$   C     WPOCGC        ORNE      *ZEROS
567300941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
567400940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
567500940324£$   C                   MOVEL     'S'           WSLITT
567600940324£$   C                   END
567700940324£$   C                   END
567800940324£$   C                   END
567900960129     C* EVENTI GIACENZA
568000960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
568100960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
568200960129     C                   Z-ADD     *ZEROS        G                 3 0
568300960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
568400960129     C     G             ANDLT     IG
568500960129     C                   ADD       1             G
568600960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
568700960209     C     WPOROU        ANDLE     GCF(G)
568800960129     C                   MOVEL     'S'           WSLITT
568900960129     C                   ENDIF                                                  <-- 3 ---
569000960129     C                   ENDDO                                                  <-- 2 ---
569100960129     C                   ENDIF                                                  <-- 1 ---
569200960129     C                   ENDIF                                                  <-- 0 ---
569300940324     C* LASCIATO AVVISO
569400931214     C     WSLITT        IFEQ      'N'
569500941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
569600940324£$   C                   Z-ADD     *ZEROS        L                 3 0
569700940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
569800940324£$   C     L             ANDLT     IL
569900940324£$   C                   ADD       1             L
570000941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
570100941229£$   C     WPOROU        ANDLE     LCF(L)
570200050520     c     LCon(L)       andeq     'S'
570300940324£$   C                   MOVEL     'S'           WSLITT
570400940324£$   C                   END                                                    <-- 3 ---
570500940324£$   C                   ENDDO                                                  <-- 2 ---
570600940324£$   C                   END                                                    <-- 1 ---
570700931210     C                   END
570800940324     C* TURNO CHIUSURA
570900140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
571000140826     c*  di sistemare le ore da sommare 12 o 24...
571100931214     C     WSLITT        IFEQ      'N'
571200931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
571300931210     C                   END
571400940324     C* DATA CONSEGNA RICHIESTA
571500050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
571600050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
571700050907     c
571800931214     C     WSLITT        IFEQ      'N'
571900050907     c                   add       1             wporou
572000940324£$   C     WPOCRI        IFNE      *ZEROS
572100940324£$   C     WPOROU        IFLT      WPOCRC
572200940324£$   C                   MOVEL     'S'           WSLITT
572300940324£$   C                   END
572400940324£4   C                   END
572500050907     c                   sub       1             wporou
572600931210     C                   END
572700931210     C*
572800931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
572900931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
573000931210     C     WSLITT        IFEQ      'N'
573100931210     C                   ADD       12            WR9NOV            5 0
573200931210     C                   END
573300931210     C                   ADD       12            WR9POW            9 0
573400931210     C*
573500931223     C                   ENDDO                                                  <-- 1 ---
573600931210     C*
573700931210     C                   ENDSR
573800940628%%%  C********************************************************************
573900940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
574000940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
574100940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
574200940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
574300940628%%%  C********************************************************************
574400940628%%%  C     RO0GES        BEGSR
574500940628%%%  C*
574600940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
574700940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
574800940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
574900940628%%%  C*
575000940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
575100940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
575200940628%%%  C*
575300940628%%%  C* CICLO FINO A CHE:
575400940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
575500940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
575600940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
575700050922%%%  C     WR0POO        ANDGe     WPODSP
575800940628%%%  C*
575900940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
576000940628%%%  C                   MOVEL     'N'           WSLITT            1
576100940628%%%  C*
576200940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
576300940628%%%  C*
576400940628%%%  C     WSLITT        IFEQ      'N'
576500940628%%%  C                   SUB       12            WR0NOI            5 0
576600940628%%%  C                   END
576700940628%%%  C                   SUB       12            WR0POO            9 0
576800940628%%%  C*
576900940628%%%  C                   ENDDO                                                  <-- 1 ---
577000940628%%%  C*
577100940628%%%  C                   ADD       12            WR0POO            9 0
577200940628%%%  C*
577300940628%%%  C                   ENDSR
577400960911     C********************************************************************
577500960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
577600960911     C*            INPUT:  -W10POI (PROGR.ORE)
577700960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
577800960911     C********************************************************************
577900960911     C     R10FES        BEGSR
578000960911     C*
578100960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
578200960911     C                   Z-ADD     W10POI        W10POO            9 0
578300960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
578400960911     C                   MOVEL     'N'           WSLITT
578500960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
578600960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
578700960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
578800960911     C                   ADD       12            WPOROU
578900960911     C                   MOVEL     'N'           WSLITT
579000960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
579100960911     C                   ENDDO                                                  <-- 2 ---
579200960911     C                   Z-ADD     WPOROU        W10POO
579300960911     C*
579400960911     C                   ENDSR
579500930325     C********************************************************************
579600930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
579700930325     C********************************************************************
579800930325     C     CHKGIA        BEGSR
579900930406     C*
580000960129     C* APERTURA/CHIUSURA GIACENZA
580100930326     C     WPODAG        IFNE      *ZEROS
580200930406     C     WPODCG        ORNE      *ZEROS
580300930407     C     WPOROU        IFGE      WPODAG
580400930407     C     WPOROU        ANDLE     WPODCG
580500930325     C                   MOVEL     'S'           WSLITT
580600930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
580700930325     C                   END
580800930326     C                   END
580900960129     C*
581000960129     C* EVENTI GIACENZA
581100960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
581200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
581300960129     C                   Z-ADD     *ZEROS        G                 3 0
581400960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
581500960129     C     G             ANDLT     IG
581600960129     C                   ADD       1             G
581700960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
581800960129     C     WPOROU        ANDLE     GIF(G)
581900091009     c
582000091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
582100091009     c*  l'orario dell'evento
582200091009     c                   select
582300091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
582400091009     c* Se orario entro le 10.30  va bene
582500091014     c*  solo la prima volta per evento cioè quando controllo
582600091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
582700091014     c*  e non devo dare errore
582800091014     C                   MOVEL     'OSC  '       WR3TAB
582900091014     C                   eval      WR3HHI=evo(gip(G))
583000091014     C                   EXSR      RO3ORE
583100091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
583200091014     c*  in ritardo come per "E")
583300091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
583400091014     c                              wr3matpom='P'
583500091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
583600091009     C                   MOVEL     'S'           WSLITT
583700091009     c                   endif
583800091014     c                   eval      SalvaEveG=gip(G)
583900091009     c
584000091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
584100091009     c* Se orario entro le 10.30  va bene
584200091014     c*  solo la prima volta per evento cioè quando controllo
584300091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
584400091014     c*  e non devo dare errore
584500091014     C                   MOVEL     'OSC  '       WR3TAB
584600091014     C                   eval      WR3HHI=evo(gip(G))
584700091014     C                   EXSR      RO3ORE
584800091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
584900091014     c*  in ritardo come per "E")
585000091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
585100091014     c                              wr3matpom='P'
585200091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
585300091009     C                   MOVEL     'S'           WSLITT
585400091009     c                   endif
585500091014     c                   eval      SalvaEveG=gip(G)
585600091009     c
585700091009     c                   other
585800960129     C                   MOVEL     'S'           WSLITT
585900960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
586000091009     c                   endsl
586100091009
586200960129     C                   ENDIF                                                  <-- 3 ---
586300960129     C                   ENDDO                                                  <-- 2 ---
586400960129     C                   ENDIF                                                  <-- 1 ---
586500960129     C                   ENDIF                                                  <-- 0 ---
586600930406     C*
586700930325     C                   ENDSR
586800060420     C********************************************************************
586900930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
587000930325     C********************************************************************
587100930325     C     CHKLAV        BEGSR
587200930325     C*
587300941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
587400930421     C                   Z-ADD     *ZEROS        L                 3 0
587500930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
587600930421     C     L             ANDLT     IL
587700930421     C                   ADD       1             L
587800930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
587900930421     C     WPOROU        ANDLE     LAF(L)
588000090928     c
588100090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
588200091009     c*  l'orario dell'evento
588300091009     c                   select
588400091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
588500091009     c* Se orario entro le 10.30  va bene
588600091014     c*  solo la prima volta per evento cioè quando controllo
588700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
588800091014     c*  e non devo dare errore
588900091014     C                   MOVEL     'OSC  '       WR3TAB
589000091014     C                   eval      WR3HHI=evo(lap(L))
589100091014     C                   EXSR      RO3ORE
589200091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
589300091014     c*  in ritardo come per "E")
589400091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
589500091014     c                              wr3matpom='P'
589600091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
589700091009     C                   MOVEL     'S'           WSLITT
589800091009     c                   endif
589900091014     c                   eval      SalvaEveL=lap(L)
590000091009     c
590100091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
590200091009     c* Se orario entro le 10.30  va bene
590300091014     c*  solo la prima volta per evento cioè quando controllo
590400091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
590500091014     c*  e non devo dare errore
590600091014     C                   MOVEL     'OSC  '       WR3TAB
590700091014     C                   eval      WR3HHI=evo(lap(L))
590800091014     C                   EXSR      RO3ORE
590900091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
591000091014     c*  in ritardo come per "E")
591100091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
591200091014     c                              wr3matpom='P'
591300091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
591400091009     C                   MOVEL     'S'           WSLITT
591500091009     c                   endif
591600091014     c                   eval      SalvaEveL=lap(L)
591700090928     c
591800091009     c                   other
591900930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
592000091009     C                   MOVEL     'S'           WSLITT
592100091009     c                   endsl
592200090928     c
592300930421     C                   END                                                    <-- 3 ---
592400931223     C                   ENDDO                                                  <-- 2 ---
592500930421     C                   END                                                    <-- 1 ---
592600930325     C*
592700930325     C                   ENDSR
592800060420     C********************************************************************
592900060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
593000060420     c*                   che Allungano
593100060420     C********************************************************************
593200060420     C     CHKCEPA       BEGSR
593300060420     C*
593400060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
593500060420     C                   Z-ADD     *ZEROS        p                 3 0
593600060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
593700060420     C     p             ANDLT     IpA
593800060420     C                   ADD       1             P
593900060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
594000060420     C     WPOROU        ANDLE     paif(P)
594100060420     C                   MOVEL     'S'           WSLITT
594200060420     C                   ADD       12            W98EA
594300060420     C                   END                                                    <-- 3 ---
594400060420     C                   ENDDO                                                  <-- 2 ---
594500060420     C                   END                                                    <-- 1 ---
594600060420     C*
594700060420     C                   ENDSR
594800930414     C********************************************************************
594900930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
595000930414     C********************************************************************
595100930414     C     CHKCNT        BEGSR
595200930511     C*
595300930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
595400930611     C     WIC           IFEQ      'I'                                          *INTERNO
595500930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
595600930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
595700930611     C                   ELSE
595800930611     C     WPOROU        IFGE      WPOAII
595900930611     C     WPOROU        ANDLE     WPOAFI
596000930611     C                   MOVEL     'S'           WSLITT
596100930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
596200930611     C                   END
596300930611     C                   END
596400930611     C                   ELSE                                                   *CLIENTE
596500930611     C     WPOAIC        IFEQ      *ZEROS
596600930611     C     WPOAFC        ANDEQ     *ZEROS
596700930611     C                   ELSE
596800930611     C     WPOROU        IFGE      WPOAIC
596900930611     C     WPOROU        ANDLE     WPOAFC
597000930611     C                   MOVEL     'S'           WSLITT
597100930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
597200930611     C                   END
597300930611     C                   END
597400930611     C                   END
597500930414     C*
597600930414     C                   ENDSR
597700930511     C********************************************************************
597800930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
597900930511     C********************************************************************
598000930511     C     CHKFED        BEGSR
598100930511     C*
598200930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
598300930611     C     WIC           IFEQ      'I'                                          *INTERNO
598400930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
598500930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
598600930611     C                   ELSE
598700930611     C     WPOROU        IFGE      WPOFII
598800930611     C     WPOROU        ANDLE     WPOFFI
598900930611     C                   MOVEL     'S'           WSLITT
599000930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
599100930611     C                   END
599200930611     C                   END
599300930611     C                   ELSE                                                   *CLIENTE
599400930611     C     WPOFIC        IFEQ      *ZEROS
599500930611     C     WPOFFC        ANDEQ     *ZEROS
599600930611     C                   ELSE
599700930611     C     WPOROU        IFGE      WPOFIC
599800930611     C     WPOROU        ANDLE     WPOFFC
599900930611     C                   MOVEL     'S'           WSLITT
600000930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
600100930611     C                   END
600200930611     C                   END
600300930611     C                   END
600400930511     C*
600500930511     C                   ENDSR
600600930325     C********************************************************************
600700930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
600800930325     C********************************************************************
600900930325     C     CHKFES        BEGSR
601000060526     c* se già passata la data, non controllo wporou
601100060526     c                   if        wpodata>0
601200060526     c                   z-add     wpodata       w12dat
601300060526     c                   clear                   w12res
601400060526     c                   else
601500931130     C*
601600931130     C* TRASFORMA IL  PROGR.ORE IN DATA
601700931130     C                   Z-ADD     WPOROU        WORE
601800931130     C                   EXSR      OREDAT
601900941227     C                   Z-ADD     WDAT          W12DAT            8 0
602000931130     C                   Z-ADD     WRES          W12RES            3 0
602100060526     c                   endif
602200930325     C*
602300941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
602400930325     C* E' FESTIVO
602500090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
602600090108     C**                 Z-ADD     D98LNP        CLNTFP
602700090108     C**                 Z-ADD     *ZEROS        CLNTFA
602800090108     C**                 ENDIF
602900090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
603000090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
603100090108     c**   è possibile rendere festivo in terminal per un altro terminal
603200090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
603300930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
603400090108     C     WTC           oreq      'P'                                          *TRAZIONE
603500941227     C                   Z-ADD     D98TFP        CLNTFP
603600941227     C                   Z-ADD     D98TFA        CLNTFA
603700941228     C                   ENDIF
603800941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
603900941227     C                   Z-ADD     *ZEROS        CLNTFP
604000941227     C                   Z-ADD     D98LNA        CLNTFA
604100941228     C                   ENDIF
604200941227     C                   Z-ADD     W12DAT        DSDAT8
604300941227     C                   Z-ADD     DSAAS         CLNANN
604400941227     C                   Z-ADD     DSMES         CLNMES
604500930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
604600930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
604700930421     C                   Z-ADD     DSGIO         G
604800930415     C     W12RES        IFEQ      *ZEROS
604900941227     C                   MOVEL     CLNMAT        GFEDS
605000930415     C                   ELSE
605100941227     C                   MOVEL     CLNPOM        GFEDS
605200930415     C                   END
605300930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
605400930405     C                   MOVEL     *BLANKS       WC1Y
605500930421     C                   MOVEL     GFE(G)        WC1Y              8
605600930421     C                   Z-ADD     1             I
605700930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
605800930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
605900930405     C     WIC           IFEQ      'I'
606000930421     C     I1Y(I)        ANDEQ     'S'
606100930405     C                   MOVEL     'S'           WSLITT
606200930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
606300930405     C                   END
606400930405     C     WIC           IFEQ      'C'
606500930421     C     E1Y(I)        ANDEQ     'S'
606600930405     C                   MOVEL     'S'           WSLITT
606700930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
606800930405     C                   END
606900970204     C     WIC           IFEQ      'X'
607000970204     C     E1Y(I)        ANDEQ     'S'
607100970204     C                   MOVEL     'S'           WSLITT
607200970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
607300970204     C                   END
607400930405     C                   ELSE                                                   <-- 3 -->
607500930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
607600930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
607700930405     C                   END                                                    <-- 3 ---
607800930405     C                   END                                                    <-- 2 ---
607900930405     C                   END                                                    <-- 1 ---
608000930325     C*
608100060526     c                   clear                   wpodata
608200930325     C                   ENDSR
608300931201     C********************************************************************
608400931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
608500931201     C********************************************************************
608600931201     C     CHKTUC        BEGSR
608700931201     C*
608800931201     C     WIC           IFEQ      'I'                                          --- 1 -->
608900931201     C                   DO        TI            T                 3 0          --- 2 -->
609000931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
609100931201     C     WPOROU        ANDLE     TIF(T)
609200931201     C                   MOVEL     'S'           WSLITT
609300931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
609400931201     C                   LEAVE                                                  *VA A FINE DO
609500931201     C                   ENDIF                                                  <-- 3 ---
609600931201     C                   ENDDO                                                  <-- 2 ---
609700931201     C                   ELSE                                                   <-- 1 -->
609800931201     C                   DO        TC            T                 3 0          --- 2 -->
609900931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
610000931201     C     WPOROU        ANDLE     TCF(T)
610100931201     C                   MOVEL     'S'           WSLITT
610200931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
610300931201     C                   LEAVE                                                  *VA A FINE DO
610400931201     C                   ENDIF                                                  <-- 3 ---
610500931201     C                   ENDDO                                                  <-- 2 ---
610600931201     C                   ENDIF                                                  <-- 1 ---
610700931201     C*
610800931201     C                   ENDSR
610900060418     C********************************************************************
611000060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
611100060418     c*          - eventi particolari che giustificano
611200060418     C********************************************************************
611300060418     C     CHKCEPG       BEGSR
611400060418     C*
611500060418     C     WIC           IFEQ      'I'                                          --- 1 -->
611600060420     C                   DO        PGI           p                 3 0          --- 2 -->
611700060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
611800060420     C     WPOROU        ANDLE     PGIF(p)
611900060418     C                   MOVEL     'S'           WSLITT
612000060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
612100060418     C                   LEAVE                                                  *VA A FINE DO
612200060418     C                   ENDIF                                                  <-- 3 ---
612300060418     C                   ENDDO                                                  <-- 2 ---
612400060418     C                   ELSE                                                   <-- 1 -->
612500060420     C                   DO        PGC           p                 3 0          --- 2 -->
612600060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
612700060420     C     WPOROU        ANDLE     PGCF(p)
612800060418     C                   MOVEL     'S'           WSLITT
612900060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
613000060418     C                   LEAVE                                                  *VA A FINE DO
613100060418     C                   ENDIF                                                  <-- 3 ---
613200060418     C                   ENDDO                                                  <-- 2 ---
613300060418     C                   ENDIF                                                  <-- 1 ---
613400060418     C*
613500060418     C                   ENDSR
613600930512     C********************************************************************
613700931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
613800930512     C********************************************************************
613900931130     C     DATTUC        BEGSR
614000931214     C*
614100931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
614200931214     C                   Z-ADD     12            WNOADD            9 0
614300930325     C*
614400931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
614500931125     C     D98GC2        ORNE      *BLANKS
614600931130     C*
614700931213     C                   Z-ADD     WPOROU        WORE
614800931130     C                   EXSR      OREDAT                                       *ORE->DATA
614900931130     C                   Z-ADD     WDAT          W12DAT
615000931130     C                   Z-ADD     WRES          W12RES
615100930405     C*
615200941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
615300930405     C* DELLA SETTIMANA
615400941227     C                   Z-ADD     W12DAT        DSDAT8
615500941227     C                   Z-ADD     DSAAS         CLNANN
615600941227     C                   Z-ADD     DSMES         CLNMES
615700941227     C                   Z-ADD     *ZEROS        CLNTFP
615800941227     C                   Z-ADD     *ZEROS        CLNTFA
615900930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
616000930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
616100941227     C                   MOVEL     CLNMAT        GSEDS
616200960129     C                   Z-ADD     DSGIO         GI                6 0
616300960129     C                   MOVEL     GSE(GI)       WGSE              1
616400140911     c
616500140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
616600140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
616700140911     c                   if        wtuctut='S'
616800140911     c                   eval      wtuctut='P'
616900140911     c                   endif
617000930326     C*
617100931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
617200931213     C                   DO        4             IT                3 0          --- 3 -->
617300931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
617400931213     C                   MOVEL     TUC(IT)       W93GSE            1
617500931213     C                   MOVE      TUC(IT)       W93GMP            1
617600930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
617700930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
617800931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
617900930512     C     W93GSE        OREQ      ' '
618000140911     c     WTUCTUT       andeq     ' '
618100140911     c
618200930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
618300931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
618400930401     C     W93GMP        OREQ      ' '
618500930325     C                   MOVEL     'S'           WSLITT
618600931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
618700931213     C                   ELSE                                                   <-- 6 -->
618800001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
618900001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
619000001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
619100050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
619200050908     C     Usacons       IFNE      'S'                                          --- 7 -->
619300141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
619400141107     c*    orari giorno per giorno
619500141107     c                   if        OOR2FCSGL<>' '
619600141108     c                             and w12dat<>sav_12dat
619700141107     c                   eval      or_datarif=w12dat
619800141107     c                   eval      or_DDC    =w12dat
619900141107     c                   exsr      Reper_tempi
620000141108     c                   eval      sav_12dat=w12dat
620100141107     c                   endif
620200141107     c
620300930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
620400930325     C     W93GMP        IFEQ      'M'
620500930326     C     W12RES        ANDEQ     *ZEROS
620600141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
620700141111     c                             or OOR2STfS =0
620800141105     c                             or (tuc(IT)<>gc1part and
620900141105     c                              tuc(IT)<>gc2part)
621000140825     C                   MOVEL     'S'           WSLITT
621100140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
621200140911     c* slitto solo una volta  se chiuso tutti " M"
621300140911     c                   if        W93GSE=' '
621400140911     c                   eval      wTUCTUT='S'
621500140911     c                   endif
621600140911     c
621700140825     c                   endif
621800930325     C                   END
621900930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
622000930325     C     W93GMP        IFEQ      'P'
622100930326     C     W12RES        ANDNE     *ZEROS
622200140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
622300140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
622400141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
622500141111     c                             or OOR2STiS =0
622600141105     c                             or (tuc(IT)<>gc1part and
622700141105     c                              tuc(IT)<>gc2part)
622800930325     C                   MOVEL     'S'           WSLITT
622900140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
623000140911     c* slitto solo una volta  se chiuso tutti " P"
623100140911     c                   if        W93GSE=' '
623200140911     c                   eval      wTUCTUT='S'
623300140911     c                   endif
623400140825     c                   endif
623500140825     c
623600931213     C                   ENDIF
623700001128     C                   ENDIF                                                  <-- 7 ---
623800931213     C                   ENDIF                                                  <-- 6 ---
623900931213     C                   ENDIF                                                  <-- 5 ---
624000931213     C                   ENDIF                                                  <-- 4 ---
624100931213     C                   ENDDO                                                  <-- 3 ---
624200930405     C*
624300931213     C                   ENDIF                                                  <-- 2 ---
624400931213     C                   ENDIF                                                  <-- 1 ---
624500140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
624600140911     c                   if        wtuctut='P'  and wslitt='N'
624700140911     c                   clear                   wtuctut
624800140911     c                   endif
624900140911     c
625000930325     C                   ENDSR
625100060406     C********************************************************************
625200060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
625300060406     C********************************************************************
625400060406     C     DATCEPG       BEGSR
625500060406     C*
625600060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
625700060406     C                   Z-ADD     12            WNOADD            9 0
625800060406     C*
625900060406    1C                   if        wevpag='S'
626000060406     C*
626100060406     C                   Z-ADD     WPOROU        WORE
626200060406     C                   EXSR      OREDAT                                       *ORE->DATA
626300060406     C                   Z-ADD     WDAT          W12DAT
626400060406     C                   Z-ADD     WRES          W12RES
626500060418     c                   z-add     1             P
626600060406     c* Verifico se nella data esiste un evento Part che giustifica
626700060418     c     w12dat        lookup    pvd(P)                                 30
626800060418    2c                   dow       *in30 and pvf(P)<>'G'
626900060418     c                   add       1             P
627000060420     c     w12dat        lookup    pvd(P)                                 30
627100060406    2c                   enddo
627200060406     c
627300060406     c* 30 on --> TROVATO !!
627400060406    2c                   if        *in30
627500060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
627600060406     C                   MOVEL     'S'           WSLITT
627700060406     c                   z-add     24            wnoadd
627800060406    3C                   ENDIF
627900060406     C*
628000060406    2C                   ENDIF                                                  <-- 2 ---
628100060406    1C                   ENDIF                                                  <-- 1 ---
628200060406     C*
628300060406     C                   ENDSR
628400931210     C********************************************************************
628500931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
628600931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
628700931210     C********************************************************************
628800050805     C***  CHKDCR        BEGSR
628900931210     C*
629000050805     C***  WPOCRI        IFNE      *ZEROS
629100050805     C***  WPOROU        IFGE      WPOCRI
629200050805     C***  WPOROU        ANDLT     WPOCRF
629300050805     C***                ELSE
629400050805     C***                MOVEL     'S'           WSLITT
629500050805     C***                END
629600050805     C***                END
629700931210     C*
629800050805     C***                ENDSR
629900140822     C********************************************************************
630000140822     C*  Reperimento tempi di consegna per decidere se slittare
630100140822     C********************************************************************
630200140822     C     Reper_tempi   BEGSR
630300140822
630400140822     c                   clear                   trulorSds
630500140825     c                   clear                   trulor2ds
630600140822     c                   eval       IOREtfp = d98tfp
630700140822     c                   eval       IOREtfa = d98tfa
630800140825     c                   eval       IOREdti = d98dti
630900140825     c                   eval       IOREhti = d98hti
631000140825     c                   eval       IOREdcr = s98dcr
631100140825     c                   eval       IOREhcr = s98hcr
631200140825     c                   eval       IOREtcr = s98tcr
631300140825     c                   eval       IOREDEI = d98dei
631400140825     c                   eval       IOREOEI = d98oei
631500140825     c                   eval       IOREfil = d98lna
631600140825     c                   eval       IOREcap = d98cad
631700140825     c                   eval       IOREloc = d98lod
631800140825     c                   eval       IOREnar = d98nzd
631900140825     c
632000140825     c* Data di Riferimento:
632100140825     c* Se impostata la consegnabilità --> prendo questa
632200140822     c                   select
632300140825     c                   when      d98dfc>0
632400141107     c                   if        or_datarif<d98dfc and or_datarif>0
632500141107     c                   eval      ioredta=or_datarif
632600141107     c                   else
632700141107     c                   eval      ioredta=d98dfc
632800141107     c                   endif
632900140825     c* altrimenti data di consegna
633000140825     c                   when      d98dcm>0
633100141107     c                   if        or_datarif<d98dcm and or_datarif>0
633200141107     c                   eval      ioredta=or_datarif
633300141107     c                   else
633400140825     c                   eval      ioredta=d98dcm
633500141107     c                   endif
633600140825     c
633700140822     c                   other
633800140825     c* altrimenti data teorica consegna se > = oggi
633900140825     C                   time                    wrkoggiiso
634000140825     C     *iso          move      wrkoggiiso    ioredta
634100140822     c* se < di oggi, imposto oggi
634200140825     c                   if        d98dei>ioredta
634300140825     c                   eval      ioredta=d98dei
634400140822     c                   endif
634500141107     c
634600141107     c                   if        or_datarif<ioredta and or_datarif>0
634700141107     c                   eval      ioredta=or_datarif
634800141107     c                   endif
634900141107     c
635000140822     c                   endsl
635100140822     c
635200140825     c                   eval       IOREtser = 'C'
635300141107     c                   if         or_ddc>0 and or_ddc<d98ddc
635400141107     c                   eval       IOREDDC=or_ddc
635500141107     c                   eval       IOREnDC=8888888
635600141107     c                   eval       IOREcons = 'S'
635700141107     c                   else
635800140825     c                   eval       IOREDDC=d98ddc
635900140825     c                   eval       IOREnDC=wd98ndc
636000140825     c                   eval       IOREcons = d98cons
636100141107     c                   endif
636200141017     c                   eval       IOREtsp = d98tsp
636300140822     c
636400140822     c                   call      'TRULORSR'
636500140822     c                   parm                    kpjba
636600140822     c                   parm                    trulorSds
636700140825     c                   parm                    trulor2ds
636800140825     c
636900140825     c                   eval      wtrulor= 'S'
637000141107     c                   clear                   or_datarif        8 0
637100141107     c                   clear                   or_ddc            8 0
637200140822     C                   ENDSR
637300950406     C********************************************************************
637400950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
637500950406     C********************************************************************
637600950406     C     DATORE        BEGSR
637700950406     C*
637800950406     C                   Z-ADD     *ZEROS        WORE
637900950406     C     WDAT          IFNE      0
638000950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
638100950406     C     WAA           DIV       4             WQUA              4 0
638200950406     C                   MVR                     WRES
638300950406     C     WAA           MULT      365           WGIO              9 0
638400950406     C                   ADD       WQUA          WGIO
638500950406     C                   ADD       WGG           WGIO
638600950406     C     WMM           SUB       1             MC                3 0
638700950406     C                   DO        MC            WX                3 0
638800950406     C                   ADD       MES(WX)       WGIO
638900950406     C                   END
639000950406     C     WRES          IFEQ      0
639100950406     C     WMM           ANDLE     2
639200950406     C                   SUB       1             WGIO
639300950406     C                   END
639400950406     C     WGIO          MULT      24            WORE              9 0
639500950406     C                   ENDIF
639600950406     C*
639700950406     C                   ENDSR
639800930323     C********************************************************************
639900930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
640000930323     C********************************************************************
640100930323     C     OREDAT        BEGSR
640200930323     C*
640300950407     C                   SETOFF                                       717273
640400950407     C                   SETOFF                                       7475
640500930323     C     WORE          DIV       24            WGIO
640600930323     C                   MVR                     WRES              3 0
640700950406     C*
640800950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
640900950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
641000950406     C*  TROVO ANNO
641100950406     C     WGIO          DIV       365           WAA
641200950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
641300950406     C                   MVR                     WREST                71
641400950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
641500950406     C     WGIO          DIV       365           WAA
641600950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
641700950406     C*                                                    0 = 31/12/PR
641800950406     C   75
641900950406     CAN 71              DO                                                     BIS. E NON ERA
642000950406     C     WAA           SUB       1             WAA
642100950406     C                   Z-ADD     365           WREST
642200950406     C                   END
642300950406     C*
642400950406     C     WAA           DIV       4             WNBIS                                      T.
642500950406     C                   MVR                     WRBIS             3 073
642600950406     C   73
642700950406     CANN71                                                                     NON PIU' BISEST
642800950406     CORN73              ADD       1             WREST                          E'BISESTILE
642900950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
643000950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
643100950406     C  N73              SUB       1             $PM(1)
643200950406     C                   Z-ADD     1             WI                2 0
643300950406     C* TROVO IL MESE E GIORNI
643400950406     C     WREST         LOOKUP    $PM(WI)                            74  73
643500950406     C                   MOVE      WI            WMM
643600950406     C     WI            COMP      1                                      72
643700950406     C  N72
643800950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
643900950406     C   74              SUB       1             WI
644000950406     C  N72
644100950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
644200950406     C   72              MOVE      WREST         WGG
644300950406     C                   ADD       1900          WAA
644400950406     C*
644500930323     C*
644600930323     C                   ENDSR
6447009407151507 C********************************************************************
6448009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6449009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6450009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6451009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6452009407151507 C********************************************************************
6453009407151507 C     FINGIO        BEGSR
6454009407151507 C*
6455009407151507 C     WPORE         DIV       24            WGIO
6456009407151507 C                   MVR                     WRES
6457009407151507 C     WRES          IFEQ      *ZEROS
6458009407151507 C                   Z-ADD     12            WNORE             3 0
6459009407151507 C                   ELSE
6460009407151507 C                   Z-ADD     *ZEROS        WNORE
6461009407151507 C                   ENDIF
6462009407151507 C*
6463009407151507 C                   ENDSR
646400930517     C********************************************************************
646500971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
646600930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
646700930517     C********************************************************************
646800930517     C     CERCAL        BEGSR
646900930517     C*
647000930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
647100941227     C                   MOVE      CLNTFP        DCATFP
647200941227     C                   MOVE      CLNTFA        DCATFA
647300941227     C                   MOVE      CLNANN        DCAANN
647400941227     C                   MOVE      CLNMES        DCAMES
647500930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
647600930517     C                   Z-ADD     1             I
647700930517     C     DCALEN        LOOKUP    CAK(I)                                 99
647800930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
647900930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
648000941227     C     KEYCLN        CHAIN     AZCLN01L                           99
648100930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
648200930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
648300930517     C                   MOVEL     '1'           D98ERR
648400930517     C                   MOVEL     MSG(3)        D98MSG
648500930517     C                   ELSE                                                   <-- 2 -->
648600970401     C* CONTROLLA SE ANNULLATO
648700970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
648800970401     C                   MOVEL     '1'           D98ERR
648900970401     C                   MOVEL     MSG(3)        D98MSG
649000970401     C                   ELSE                                                   <-- 3 -->
649100930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
649200930517     C                   ADD       1             WINDCA            6 0
649300970401     C     WINDCA        IFLE      200                                          --- 4 -->
649400970401     C     WINDCA        ANDGT     *ZEROS
649500930517     C                   Z-ADD     WINDCA        I
649600930517     C                   MOVEL     DCALEN        CAK(I)
649700941227     C                   MOVEL     CLNMAT        CAM(I)
649800941227     C                   MOVEL     CLNPOM        CAP(I)
649900970401     C                   END                                                    <-- 4 ---
650000970401     C                   ENDIF                                                  <-- 3 ---
650100930517     C                   END                                                    <-- 2 ---
650200930517     C                   ELSE                                                   <-- 1 -->
650300930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
650400941227     C                   MOVEL     CAM(I)        CLNMAT
650500941227     C                   MOVEL     CAP(I)        CLNPOM
650600930517     C                   END                                                    <-- 1 ---
650700930517     C*
650800930517     C                   ENDSR
650900010821     C********************************************************************
651000010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
651100010821     C********************************************************************
651200010821     C     CHKVCA        BEGSR
651300010821     C*
651400010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
651500010821     C     D98CAS        IFGT      *ZEROS
651600010821     C     D98VCA        ANDEQ     *BLANKS
651700010821     C                   MOVEL     'ITL'         D98VCA
651800010821     C                   ENDIF
651900010822     C*
652000010822     C     D98VCA        IFNE      'ITL'
652100010822     C     D98VCA        ANDNE     'EUR'
652200010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
652300010822     C                   Z-ADD     D98CAS        INPIMP
652400010822     C                   MOVEL     D98VCA        INPDIV
652500010822     C                   EXSR      EXECNV
652600010822     C                   Z-ADD     OUTIMP        D98CAS
652700010822     C                   MOVEL     §GEDCN        D98VCA
652800010822     C                   ENDIF
652900010821     C*
653000010821     C                   ENDSR
653100010821     C********************************************************************
653200010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
653300010821     C********************************************************************
653400010821     C     EXECNV        BEGSR
653500010821     C*
653600010821     C                   CLEAR                   YEURDS
653700010821     C*
653800010821     C                   MOVEL     INPDIV        YECDVI
653900010821     C                   Z-ADD     INPIMP        YECIMI
654000010821     C                   MOVEL     §GEDCN        YECDVO
654100010821     C                   MOVEL     '3'           YECDCO
654200010821     C                   MOVEL     'H'           YECTAR
654300010821     C                   CALL      'YEURCO'
654400010821     C                   PARM                    YEURDS
654500010821     C*
654600010821     C                   Z-ADD     YECIMO        OUTIMP
654700010821     C*
654800010821     C                   ENDSR
654900011211     C*------------------------------------------------------------------------*
655000011211     C* Controllo importo massimo contrassegno.
655100011211     C*------------------------------------------------------------------------*
655200011211     C     REPMAXCSB     BEGSR
655300011211     C*
655400011211     C                   CLEAR                   Trul21DS
655500011211     C*
655600011211     C                   EVAL      I21TSp = d98TSP
655700011211     C                   EVAL      I21NzD = d98NZD
655800011211     C                   EVAL      I21LnP = d98LNP
655900011211     C                   EVAL      I21LnA = d98LNA
656000011211     C                   EVAL      I21Ksc = d98KSC
656100011211     C                   EVAL      I21TiC = d98TIC
656200011211     C                   EVAL      I21Imp = d98CAS
656300011211     C                   EVAL      I21Div = d98VCA
656400011211     C*
656500011211     C                   CALL      'TRUL21R'
656600011211     C                   PARM                    Trul21DS
656700011211     C*
656800011211     C                   ENDSR
656900011211     C*------------------------------------------------------------------------*
657000011211
657100930414     C********************************************************************
657200930414     C*   RUTINZ - ROUTINE INIZIALE
657300930414     C********************************************************************
657400930414     C     RUTINZ        BEGSR
657500930414     C*
657600930414     C* PARAMETRI IN INPUT ALLA ROUTINE
657700930414     C     *ENTRY        PLIST
657800970731     C                   PARM                    DS99
657900941227     C*
658000941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
658100941227     C     KEYBLP        KLIST
658200941227     C                   KFLD                    BLPAAS
658300941227     C                   KFLD                    BLPLNP
658400941227     C                   KFLD                    BLPNRS
658500941227     C                   KFLD                    BLPNSP
658600050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
658700941227     C     KEYGCP        KLIST
658800050405     C                   KFLD                    KWGCPAAS
658900050405     C                   KFLD                    KWGCPLNP
659000050405     C                   KFLD                    KWGCPNRS
659100050405     C                   KFLD                    KWGCPNSP
659200050405     C                   KFLD                    KWGCPFRG
659300050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
659400031125     C     KEYGC_P       KLIST
659500050405     C                   KFLD                    KWGCPAAS
659600050405     C                   KFLD                    KWGCPLNP
659700050405     C                   KFLD                    KWGCPNRS
659800050405     C                   KFLD                    KWGCPNSP
659900970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
660000150120     C**   KEYBLT        KLIST
660100150120     C**                 KFLD                    BLTAAS
660200150120     C**                 KFLD                    BLTLNP
660300150120     C**                 KFLD                    BLTNRS
660400150120     C**                 KFLD                    BLTNSP
660500941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
660600941227     C     KEYARB        KLIST
660700941227     C                   KFLD                    ARBAAS
660800941227     C                   KFLD                    ARBLNP
660900941227     C                   KFLD                    ARBNRS
661000941227     C                   KFLD                    ARBNSP
661100051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
661200941228     C     KEYAR9        KLIST
661300941228     C                   KFLD                    AR9AAS
661400941228     C                   KFLD                    AR9LNP
661500941228     C                   KFLD                    AR9NRS
661600941228     C                   KFLD                    AR9NSP
661700941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
661800050405     C     KEYGCP21      KLIST
661900050405     C                   KFLD                    KWGCPAAS
662000050405     C                   KFLD                    KWGCPLNP
662100050405     C                   KFLD                    KWGCPNRS
662200050405     C                   KFLD                    KWGCPNSP
662300050405     C                   KFLD                    KWGCPFRG
662400970912     C* CHIAVE LETTURA FNART01L - PARZIALE
662500150120     C***  KEYART        KLIST
662600150120     C***                KFLD                    ARTAAS
662700150120     C***                KFLD                    ARTLNP
662800150120     C***                KFLD                    ARTNRS
662900150120     C***                KFLD                    ARTNSP
663000950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
663100950102     C     KEYLBL        KLIST
663200950102     C                   KFLD                    LBLAAN
663300950102     C                   KFLD                    LBLLPN
663400950102     C                   KFLD                    LBLNRN
663500950102     C                   KFLD                    LBLNSN
663600930414     C*
663700070827     c     kAR5          Klist
663800070827     c                   Kfld                    d98Aas
663900070827     c                   Kfld                    d98Lnp
664000070827     c                   Kfld                    d98nrs
664100070827     c                   Kfld                    d98nsp
664200070827     c                   Kfld                    kAr5Trd
664300150325     c     kAR4          Klist
664400150325     c                   Kfld                    d98Aas
664500150325     c                   Kfld                    d98Lnp
664600150325     c                   Kfld                    d98nrs
664700150325     c                   Kfld                    d98nsp
664800150325     c                   Kfld                    kAr4Trc
664900150218     c*
665000150218     c     kart          Klist
665100150218     c                   Kfld                    d98Aas
665200150218     c                   Kfld                    d98Lnp
665300150218     c                   Kfld                    d98nrs
665400150218     c                   Kfld                    d98nsp
665500150218     c     kbrv          Klist
665600150218     c                   Kfld                    artfls
665700150218     c                   Kfld                    artlna
665800150218     c                   Kfld                    artnrs
665900150218     c                   Kfld                    artnsc
666000150325     C     Kfapd         klist
666100150325     C                   kfld                    kpdtip
666200150325     C                   kfld                    kpdpdr
666300930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
666400930414     C     KEYTAB        KLIST
666500930414     C                   KFLD                    KTBUTE
666600930414     C                   KFLD                    KTBCOD
666700930414     C                   KFLD                    KTBKEY
666800060406     C     Ktab          KLIST
666900060406     C                   KFLD                    KTBUTE
667000060406     C                   KFLD                    KTBCOD
667100930414     C*
667200941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
667300941227     C     KEYCLN        KLIST
667400941227     C                   KFLD                    CLNTFP
667500941227     C                   KFLD                    CLNTFA
667600941227     C                   KFLD                    CLNANN
667700941227     C                   KFLD                    CLNMES
667800941227     C*
667900941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
668000941227     C     KEYCAS        KLIST
668100941227     C                   KFLD                    CASPTA
668200941227     C                   KFLD                    CASLNP
668300941227     C                   KFLD                    CASLNA
668400941227     C                   KFLD                    CASTSP
668500941227     C                   KFLD                    CASDDE
668600930420     C*
668700990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
668800930420     C     KEYEVB        KLIST
668900930420     C                   KFLD                    KEBAAS
669000930420     C                   KFLD                    KEBLNP
669100930420     C                   KFLD                    KEBNRS
669200930420     C                   KFLD                    KEBNSP
669300010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
669400010821     C     KTBE          KLIST
669500010821     C                   KFLD                    TBECOD
669600010821     C                   KFLD                    TBEKE1
669700010821     C                   KFLD                    TBEKE2
669800930414     C*
669900010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
670000930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
670100930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
670200930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
670300930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
670400930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
670500930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
670600930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
670700930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
670800930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
670900010821     C     *LIKE         DEFINE    D98CAS        INPIMP
671000010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
671100010821     C     *LIKE         DEFINE    D98VCA        INPDIV
671200930414     C*
671300930414     C* RECUPERO IL CODICE DELL'AZIENDA
671400930414     C                   Z-ADD     1             CODUT             1 0
671500970701     C                   Z-ADD     *ZEROS        I                 6 0
671600930414     C*
671700930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
671800930414     C                   EXSR      AZZERA
671900930414     C*
672000930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
672100941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
672200941227     C                   MOVEL     'S'           CTABSN            1
672300930414     C                   EXSR      CARTAB
672400941227     C                   ENDIF
672500041116     c* Se passato umero di spedizioni apro i files , altrimenti no
672600041116     c                   if        d98nsp>0 and wapri=' '
672700041116     c                   open      fnblp01l
672800050405     c                   open      tigcp01l
672900150120     c***                open      fnblt01l
673000041116     c                   open      fnarb01l
673100051115     c                   open      fiar901l
673200150325     c                   open      fiar401l
673300050405     c                   open      tigcp21l
673400150218     c                   open      fnart01l
673500150218     c                   open      fnbrv07l
673600041116     c                   open      fnlbl01l
673700041116     c                   open      fnevb01l
673800070827     c                   open      fiar501l
673900041116     c                   eval      wapri='S'
674000041116     c                   endif
674100930414     C*
674200930414     C                   ENDSR
674300941227     C*------------------------------------------------------------------------*
674400941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
674500941227     C*------------------------------------------------------------------------*
674600941227     C     SUBDAT        BEGSR
674700941227     C*
674800941227     C                   TIME                    DTTIME
674900941227     C                   Z-ADD     DTANN         DSANN
675000941227     C                   Z-ADD     DTMES         DSMES
675100941227     C                   Z-ADD     DTGIO         DSGIO
675200941227     C                   Z-ADD     DTSEC         DSSEC
675300941227     C*
675400941227     C                   ENDSR
675500930414     C********************************************************************
675600930414     C*   CARTAB - CARICA LE TABELLE
675700930414     C********************************************************************
675800930414     C     CARTAB        BEGSR
675900930414     C*
676000930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
676100930414     C     D98ERR        IFEQ      '0'
676200930421     C                   Z-ADD     0             I
676300930414     C                   Z-ADD     CODUT         KTBUTE
676400930414     C                   MOVEL     '1Y'          KTBCOD
676500930414     C                   MOVEL     *BLANKS       KTBKEY
676600930414     C     KEYTAB        SETLL     TABEL00F                           99
676700930414     C  N99              READ      TABEL00F                               99
676800930414     C     *IN99         IFEQ      '1'
676900930414     C                   MOVEL     '1'           D98ERR
677000930414     C                   MOVEL     MSG(5)        D98MSG
677100930414     C                   END
677200930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
677300930414     C     TBLCOD        ANDEQ     '1Y'
677400930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
677500930421     C                   ADD       1             I
677600930421     C                   MOVEL     TBLKEY        C1Y(I)
677700930414     C                   MOVEL     TBLUNI        DS1Y
677800930421     C                   MOVEL     §1YFEI        I1Y(I)
677900930421     C                   MOVEL     §1YFEC        E1Y(I)
678000930414     C                   END                                                    <-- 3 ---
678100930414     C                   READ      TABEL00F                               99
678200930414     C                   END                                                    <-- 2 ---
678300930414     C                   END
678400930414     C*
678500930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
678600930414     C     D98ERR        IFEQ      '0'
678700930421     C                   Z-ADD     0             I
678800930414     C                   Z-ADD     CODUT         KTBUTE
678900930414     C                   MOVEL     '1X'          KTBCOD
679000930414     C                   MOVEL     *BLANKS       KTBKEY
679100930414     C     KEYTAB        SETLL     TABEL00F                           99
679200930414     C  N99              READ      TABEL00F                               99
679300930414     C     *IN99         IFEQ      '1'
679400930414     C                   MOVEL     '1'           D98ERR
679500930414     C                   MOVEL     MSG(1)        D98MSG
679600930414     C                   END
679700930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
679800930414     C     TBLCOD        ANDEQ     '1X'
679900930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
680000930421     C                   ADD       1             I
680100930421     C                   MOVEL     TBLKEY        C1X(I)
680200930414     C                   MOVEL     TBLUNI        DS1X
680300930421     C                   Z-ADD     §1XHHG        G1X(I)
680400930421     C                   Z-ADD     §1XHHO        O1X(I)
680500930421     C                   Z-ADD     §1XHHM        M1X(I)
680600930421     C                   Z-ADD     §1XHHP        P1X(I)
680700930421     C                   Z-ADD     §1XHHI        I1X(I)
680800930421     C                   Z-ADD     §1XHHS        S1X(I)
680900930414     C                   END                                                    <-- 3 ---
681000930414     C                   READ      TABEL00F                               99
681100931223     C                   ENDDO                                                  <-- 2 ---
681200050523     c
681300050523     c* controllo riempimento skiere
681400050523     c                   clear                   trul0sds
681500050523     c                   eval      i0sski='C1X/G1X/O1X/...'
681600050523     c                   eval      i0sele=%elem(c1x)
681700050523     c                   eval      i0spie=i
681800050523     c                   eval      i0sfile='TABEL00F  '
681900050523     c                   eval      i0scod=' 1X  '
682000050523     c                   eval      i0spgm='TNSD99R'
682100050523     c                   movel     trul0sds      kpjbu
682200050523     c                   call      'TRUL0SR'
682300050523     c                   parm                    kpjba
682400050523     C                   END
682500930421     C*
682600930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
682700930421     C     D98ERR        IFEQ      '0'
682800930421     C                   Z-ADD     0             I
682900930421     C                   Z-ADD     CODUT         KTBUTE
683000930421     C                   MOVEL     '2A'          KTBCOD
683100060406     C     Ktab          SETLL     TABEL00F                           99
683200060406     C  N99ktab          READe     TABEL00F                               99
683300930421     C     *IN99         IFEQ      '1'
683400930421     C                   MOVEL     '1'           D98ERR
683500930421     C                   MOVEL     MSG(8)        D98MSG
683600930421     C                   END
683700930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
683800930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
683900930421     C                   ADD       1             I
684000930421     C                   MOVEL     TBLKEY        C2A(I)
684100060406     C                   MOVEL     TBLUNI        ds2a
684200060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
684300060406     C                   MOVEL     w003a         D2A(I)
684400930421     C                   END                                                    <-- 3 ---
684500060406     C     ktab          READe     TABEL00F                               99
684600931223     C                   ENDDO                                                  <-- 2 ---
684700050523     c* controllo riempimento skiere
684800050523     c                   clear                   trul0sds
684900050523     c                   eval      i0sski='C2A/D2A'
685000050523     c                   eval      i0sele=%elem(c2a)
685100050523     c                   eval      i0spie=i
685200050523     c                   eval      i0sfile='TABEL00F  '
685300060406     c                   eval      i0scod='2A/2Z'
685400050523     c                   eval      i0spgm='TNSD99R'
685500050523     c                   movel     trul0sds      kpjbu
685600050523     c                   call      'TRUL0SR'
685700050523     c                   parm                    kpjba
685800930421     C                   END
685900060406     c* Tabella  2Z
686000060406     C     D98ERR        IFEQ      '0'
686100060406     C                   Z-ADD     CODUT         KTBUTE
686200060406     C                   MOVEL     '2Z'          KTBCOD
686300060406     C     Ktab          SETLL     TABEL00F                           99
686400060406     C  N99ktab          READe     TABEL00F                               99
686500060406     C     *IN99         IFEQ      '1'
686600060406     C                   MOVEL     '1'           D98ERR
686700060406     C                   MOVEL     MSG(33)       D98MSG
686800060406     C                   END
686900060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
687000060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
687100060406     C                   Z-ADD     1             I
687200060406     C                   MOVEL     TBLKEY        w003a
687300060406     c     w003a         lookup    c2a(i)                                 99
687400060406     C     *IN99         IFEQ      *off
687500060406     C                   MOVEL     '1'           D98ERR
687600060406     C                   MOVEL     MSG(34)       D98MSG
687700060406     C                   Else
687800060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
687900060406     C                   END                                                    <-- 3 ---
688000060406     C                   END                                                    <-- 3 ---
688100060406     C     ktab          READe     TABEL00F                               99
688200060406     C                   ENDDO                                                  <-- 2 ---
688300060406     C                   END
688400941227     C*
688500941227     C* CARICA TABELLA CODICI BOLLA -3A-
688600941227     C     D98ERR        IFEQ      '0'
688700941228     C                   Z-ADD     0             I
688800941227     C                   Z-ADD     CODUT         KTBUTE
688900941227     C                   MOVEL     '3A'          KTBCOD
689000941227     C                   MOVEL     *BLANKS       KTBKEY
689100941227     C     KEYTAB        SETLL     TABEL00F                           99
689200941227     C  N99              READ      TABEL00F                               99
689300941227     C     *IN99         IFEQ      '1'
689400941227     C                   MOVEL     '1'           D98ERR
689500941227     C                   MOVEL     MSG(22)       D98MSG
689600941227     C                   END
689700941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
689800941227     C     TBLCOD        ANDEQ     '3A'
689900941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
690000941227     C                   ADD       1             I
690100941227     C                   MOVEL     TBLKEY        C3A(I)
690200941227     C                   MOVEL     TBLUNI        DS3A
690300941227     C                   MOVEL     §3ATB1        T3A(I)
690400941228     C                   Z-ADD     §3AFCA        F3A(I)
690500990308     C                   MOVEL     §3ARBL        R3A(I)
690600941227     C                   END                                                    <-- 3 ---
690700941227     C                   READ      TABEL00F                               99
690800941227     C                   END                                                    <-- 2 ---
690900050523     c* controllo riempimento skiere
691000050523     c                   clear                   trul0sds
691100050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
691200050523     c                   eval      i0sele=%elem(c3a)
691300050523     c                   eval      i0spie=i
691400050523     c                   eval      i0sfile='TABEL00F  '
691500050523     c                   eval      i0scod=' 3A  '
691600050523     c                   eval      i0spgm='TNSD99R'
691700050523     c                   movel     trul0sds      kpjbu
691800050523     c                   call      'TRUL0SR'
691900050523     c                   parm                    kpjba
692000941227     C                   END
692100971210     C*
692200020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
692300020226     C                   Z-ADD     *ZEROS        J                 3 0
692400971210IF  0C     D98ERR        IFEQ      '0'
692500971210     C     *LOVAL        SETLL     AZORG01L
692600971210     C                   READ      AZORG01L                               98
692700971210DO  1C     *IN98         DOWEQ     *OFF
692800971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
692900971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
693000971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
693100020627     C                   MOVEL     ORGDE3        OG143
693200020702     C*
693300020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
693400020702     C                   CLEAR                   DNTW
693500020702     C*
693600020702     C                   MOVEL(P)  'NTW'         TBECOD
693700020702     C                   MOVEL(P)  §OGNTW        TBEKE1
693800020702     C     KTBE          CHAIN     TNTBE01L
693900020702     C                   IF        %found(TNTBE01L)
694000020702     C                   MOVEL     TBEUNI        DNTW
694100020702     C                   ENDIF
694200020702     C*
694300020702     C                   MOVEL     §NTWFIE       FL1(F)
694400020627     C*
694500020226     C* VERIFICO SE FILIALE FEDEX
694600020226     C                   IF        §OGNTW = 'FED'
694700020226     C                   ADD       1             J
694800020226     C                   Z-ADD     ORGFIL        FILFED(J)
694900020226     C                   ENDIF
695000020226     C*
695100971210E   2C                   ENDIF
695200971210     C                   READ      AZORG01L                               98
695300971210E   1C                   ENDDO
695400971210E   0C                   ENDIF
695500010821     C*
695600010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
695700010821     C                   MOVEL     'GED'         TBECOD
695800070514     C                   MOVEL     *BLANKS       TBEKE1
695900070514     C                   MOVEL     '1'           TBEKE1
696000010821     C                   MOVEL     *BLANKS       TBEKE2
696100010821     C     KTBE          CHAIN     TNTBE01L                           21
696200070514     C  N21              MOVEL     TBEUNI        DGED
696300150326     c
696400150326     C* REPERISCO LA tabella flag operativi per tabella OLP
696500150326     c                   clear                   dvpo
696600150326     C                   MOVEL     'VPO'         TBECOD
696700150326     C                   MOVEL     *BLANKS       TBEKE2
696800150326     C                   MOVEL(P)  'VPO'         TBEKE1
696900150326     C     KTBE          CHAIN     TNTBE01L                           21
697000150326     C  N21              MOVEL     TBEUNI        DVPO
697100150326     C*
697200031201     C*
697300031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
697400031201     C     D98ERR        IFEQ      '0'
697500060403     C                   Z-ADD     0             I7QW              3 0
697600031201     C                   Z-ADD     CODUT         KTBUTE
697700031201     C                   MOVEL     '7Q'          KTBCOD
697800031201     C                   MOVEL     *BLANKS       KTBKEY
697900031201     C     KEYTAB        SETLL     TABEL00F                           99
698000031201     C  N99              READ      TABEL00F                               99
698100031201     C     *IN99         IFEQ      '1'
698200031201     C                   MOVEL     '1'           D98ERR
698300031201     C                   MOVEL     MSG(8)        D98MSG
698400031201     C                   ENDIF
698500031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
698600031201     C     TBLCOD        ANDEQ     '7Q'
698700031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
698800031201     C                   MOVEL     TBLUNI        DS7Q
698900031201     C                   IF        §7Q4D1 = 'R' OR
699000031201     C                             §7Q4D2 = 'R' OR
699100031201     C                             §7Q4D3 = 'R' OR
699200031201     C                             §7Q4D4 = 'R' OR
699300031201     C                             §7Q4D5 = 'R'
699400031201     C                   ADD       1             I7QW
699500031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
699600031201     C                   ENDIF                                                  <-- 3 ---
699700031201     C                   ENDIF                                                  <-- 3 ---
699800031201     C                   READ      TABEL00F                               99
699900031201     C                   ENDDO                                                  <-- 2 ---
700000031201     C                   ENDIF
700100031201     C*
700200031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
700300031201     C     D98ERR        IFEQ      '0'
700400031201     C                   Z-ADD     CODUT         KTBUTE
700500031201     C                   MOVEL     '7W'          KTBCOD
700600031201     C                   MOVEL     *BLANKS       KTBKEY
700700031201     C     KEYTAB        SETLL     TABEL00F                           99
700800031201     C  N99              READ      TABEL00F                               99
700900031201     C     *IN99         IFEQ      '1'
701000031201     C                   MOVEL     '1'           D98ERR
701100031201     C                   MOVEL     MSG(8)        D98MSG
701200031201     C                   ENDIF
701300031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
701400031201     C     TBLCOD        ANDEQ     '7W'
701500031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
701600031201     C                   MOVEL     TBLUNI        DS7W
701700031201     C                   IF        §7W6D1 = 'C' OR
701800031201     C                             §7W6D2 = 'C' OR
701900031201     C                             §7W6D3 = 'C' OR
702000031201     C                             §7W6D4 = 'C' OR
702100031201     C                             §7W6D5 = 'C'
702200031201     C                   ADD       1             I7QW
702300031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
702400031201     C                   ENDIF                                                  <-- 3 ---
702500031201     C                   ENDIF                                                  <-- 3 ---
702600031201     C                   READ      TABEL00F                               99
702700031201     C                   ENDDO                                                  <-- 2 ---
702800050523     c* controllo riempimento skiere
702900050523     c                   clear                   trul0sds
703000050523     c                   eval      i0sski='SK7QW'
703100050523     c                   eval      i0sele=%elem(sk7qw)
703200050523     c                   eval      i0spie=i7qw
703300050523     c                   eval      i0sfile='TABEL00F  '
703400050523     c                   eval      i0scod='7Q-7W'
703500050523     c                   eval      i0spgm='TNSD99R'
703600050523     c                   movel     trul0sds      kpjbu
703700050523     c                   call      'TRUL0SR'
703800050523     c                   parm                    kpjba
703900031201     C                   ENDIF
704000930414     C*
704100930414     C                   ENDSR
704200930414     C********************************************************************
704300930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
704400930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
704500930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
704600930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
704700930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
704800930414     C********************************************************************
704900930414     C     RO3ORE        BEGSR
705000930414     C*
705100930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
705200930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
705300930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
705400050902     c                   movel     ' '           WR3MATPOM         1
705500931209     C                   Z-ADD     *ZEROS        WR3HHO
705600931209     C                   Z-ADD     *ZEROS        WR3HHA
705700930414     C                   MOVE      WR3TAB        WR3COD
705800930604     C                   MOVEL     D98LNA        WR3COD            8
705900930421     C                   Z-ADD     1             I
706000930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
706100930414     C     *IN99         IFEQ      '0'
706200930414     C                   MOVE      WR3TAB        WR3COD
706300930414     C                   MOVEL     '   '         WR3COD            8
706400930421     C                   Z-ADD     1             I
706500930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
706600930414     C                   END
706700930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
706800930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
706900930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
707000930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
707100930414     C                   ELSE                                                   <-- 2 -->
707200930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
707300930421     C                   Z-ADD     M1X(I)        WR3HHO
707400930510     C                   Z-ADD     I1X(I)        WR3HHA
707500050902     c                   movel     'M'           WR3MATPOM         1
707600930414     C                   END
707700930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
707800930421     C                   Z-ADD     P1X(I)        WR3HHO
707900930510     C                   Z-ADD     S1X(I)        WR3HHA
708000050902     c                   movel     'P'           WR3MATPOM
708100930414     C                   END
708200930414     C                   END                                                    <-- 2 ---
708300930414     C                   END                                                    <-- 1 ---
708400930414     C*
708500930414     C                   ENDSR
708600930423     C********************************************************************
708700930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
708800930423     C********************************************************************
708900930423     C     W98D98        BEGSR
709000930423     C*
709100941228     C*
709200941228     C* ORE REALI PARTENZA INTERNE
709300941228     C                   Z-ADD     W98RPI        W98               3 0
709400941228     C                   EXSR      W98999
709500941228     C                   Z-ADD     D98           D98RPI
709600941228     C* ORE REALI PARTENZA CLIENTE
709700941228     C                   Z-ADD     W98RPC        W98
709800941228     C                   EXSR      W98999
709900941228     C                   Z-ADD     D98           D98RPC
710000930423     C* ORE REALI TRAZIONE INTERNE
710100941228     C                   Z-ADD     W98RTI        W98
710200941228     C                   EXSR      W98999
710300941228     C                   Z-ADD     D98           D98RTI
710400930423     C* ORE REALI TRAZIONE CLIENTE
710500941228     C                   Z-ADD     W98RTC        W98
710600941228     C                   EXSR      W98999
710700941228     C                   Z-ADD     D98           D98RTC
710800930423     C* ORE REALI CONSEGNA INTERNE
710900941228     C                   Z-ADD     W98RCI        W98
711000941228     C                   EXSR      W98999
711100941228     C                   Z-ADD     D98           D98RCI
711200930423     C* ORE REALI CONSEGNA CLIENTE
711300941228     C                   Z-ADD     W98RCC        W98
711400941228     C                   EXSR      W98999
711500941228     C                   Z-ADD     D98           D98RCC
711600930423     C* ORE REALI RESA INTERNE
711700941228     C                   Z-ADD     W98RRI        W98
711800941228     C                   EXSR      W98999
711900941228     C                   Z-ADD     D98           D98RRI
712000930423     C* ORE REALI RESA CLIENTE
712100941228     C                   Z-ADD     W98RRC        W98
712200941228     C                   EXSR      W98999
712300941228     C                   Z-ADD     D98           D98RRC
712400941228     C* ORE DIFF. PARTENZA INTERNE
712500941228     C                   Z-ADD     W98FPI        W98
712600941228     C                   EXSR      W98999
712700941228     C                   Z-ADD     D98           D98FPI
712800941228     C* ORE DIFF. PARTENZA CLIENTE
712900941228     C                   Z-ADD     W98FPC        W98
713000941228     C                   EXSR      W98999
713100941228     C                   Z-ADD     D98           D98FPC
713200930423     C* ORE DIFF. TRAZIONE INTERNE
713300941228     C                   Z-ADD     W98FTI        W98
713400941228     C                   EXSR      W98999
713500941228     C                   Z-ADD     D98           D98FTI
713600930423     C* ORE DIFF. TRAZIONE CLIENTE
713700941228     C                   Z-ADD     W98FTC        W98
713800941228     C                   EXSR      W98999
713900941228     C                   Z-ADD     D98           D98FTC
714000930423     C* ORE DIFF. CONSEGNA INTERNE
714100941228     C                   Z-ADD     W98FCI        W98
714200941228     C                   EXSR      W98999
714300941228     C                   Z-ADD     D98           D98FCI
714400930423     C* ORE DIFF. CONSEGNA CLIENTE
714500941228     C                   Z-ADD     W98FCC        W98
714600941228     C                   EXSR      W98999
714700941228     C                   Z-ADD     D98           D98FCC
714800930423     C* ORE DIFF. RESA INTERNE
714900941228     C                   Z-ADD     W98FRI        W98
715000941228     C                   EXSR      W98999
715100941228     C                   Z-ADD     D98           D98FRI
715200930423     C* ORE DIFF. RESA CLIENTE
715300941228     C                   Z-ADD     W98FRC        W98
715400941228     C                   EXSR      W98999
715500941228     C                   Z-ADD     D98           D98FRC
715600941228     C* ORE NON CONF. PARTENZA INTERNE
715700941228     C     W98NPI        IFGT      999
715800941228     C                   Z-ADD     999           D98NPI
715900941228     C                   ELSE
716000941228     C                   Z-ADD     W98NPI        D98NPI
716100941228     C                   END
716200941228     C* ORE NON CONF. PARTENZA CLIENTE
716300941228     C     W98NPC        IFGT      999
716400941228     C                   Z-ADD     999           D98NPC
716500941228     C                   ELSE
716600941228     C                   Z-ADD     W98NPC        D98NPC
716700941228     C                   END
716800930423     C* ORE NON CONF. TRAZIONE INTERNE
716900930423     C     W98NTI        IFGT      999
717000930423     C                   Z-ADD     999           D98NTI
717100930423     C                   ELSE
717200930423     C                   Z-ADD     W98NTI        D98NTI
717300930423     C                   END
717400930423     C* ORE NON CONF. TRAZIONE CLIENTE
717500930423     C     W98NTC        IFGT      999
717600930423     C                   Z-ADD     999           D98NTC
717700930423     C                   ELSE
717800930423     C                   Z-ADD     W98NTC        D98NTC
717900930423     C                   END
718000930423     C* ORE NON CONF. CONSEGNA INTERNE
718100930423     C     W98NCI        IFGT      999
718200930423     C                   Z-ADD     999           D98NCI
718300930423     C                   ELSE
718400930423     C                   Z-ADD     W98NCI        D98NCI
718500930423     C                   END
718600930423     C* ORE NON CONF. CONSEGNA CLIENTE
718700930423     C     W98NCC        IFGT      999
718800930423     C                   Z-ADD     999           D98NCC
718900930423     C                   ELSE
719000930423     C                   Z-ADD     W98NCC        D98NCC
719100930423     C                   END
719200930423     C* ORE NON CONF. RESA INTERNE
719300930423     C     W98NRI        IFGT      999
719400930423     C                   Z-ADD     999           D98NRI
719500930423     C                   ELSE
719600930423     C                   Z-ADD     W98NRI        D98NRI
719700930423     C                   END
719800930423     C* ORE NON CONF. RESA CLIENTE
719900930423     C     W98NRC        IFGT      999
720000930423     C                   Z-ADD     999           D98NRC
720100930423     C                   ELSE
720200930423     C                   Z-ADD     W98NRC        D98NRC
720300930423     C                   END
720400930423     C* ORE X TURNO CHIUSURA INTERNE
720500941228     C                   Z-ADD     W98HHI        W98
720600941228     C                   EXSR      W98999
720700941228     C                   Z-ADD     D98           D98HHI
720800930423     C* ORE X TURNO CHIUSURA CLIENTE
720900941228     C                   Z-ADD     W98HHC        W98
721000941228     C                   EXSR      W98999
721100941228     C                   Z-ADD     D98           D98HHC
721200060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
721300060418     C                   Z-ADD     W98EGI        W98
721400060418     C                   EXSR      W98999
721500060418     C                   Z-ADD     D98           D98hEGI
721600060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
721700060418     C                   Z-ADD     W98EGC        W98
721800060418     C                   EXSR      W98999
721900060418     C                   Z-ADD     D98           D98hEGC
722000060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
722100060420     C                   Z-ADD     W98EAI        W98
722200060420     C                   EXSR      W98999
722300060420     C                   Z-ADD     D98           D98hEAI
722400060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
722500060420     C                   Z-ADD     W98EAC        W98
722600060420     C                   EXSR      W98999
722700060420     C                   Z-ADD     D98           D98hEAC
722800930423     C* ORE X LASC. AVVISO INTERNE
722900941228     C                   Z-ADD     W98HLI        W98
723000941228     C                   EXSR      W98999
723100941228     C                   Z-ADD     D98           D98HLI
723200930423     C* ORE X LASC. AVVISO CLIENTE
723300941228     C                   Z-ADD     W98HLC        W98
723400941228     C                   EXSR      W98999
723500941228     C                   Z-ADD     D98           D98HLC
723600930423     C* ORE X GIACENZA INTERNE
723700941228     C                   Z-ADD     W98HGI        W98
723800941228     C                   EXSR      W98999
723900941228     C                   Z-ADD     D98           D98HGI
724000930423     C* ORE X GIACENZA CLIENTE
724100941228     C                   Z-ADD     W98HGC        W98
724200941228     C                   EXSR      W98999
724300941228     C                   Z-ADD     D98           D98HGC
724400941228     C* ORE X FEST. PART. INTERNE
724500941228     C                   Z-ADD     W98HPI        W98
724600941228     C                   EXSR      W98999
724700941228     C                   Z-ADD     D98           D98HPI
724800941228     C* ORE X FEST. PART. CLIENTE
724900941228     C                   Z-ADD     W98HPC        W98
725000941228     C                   EXSR      W98999
725100941228     C                   Z-ADD     D98           D98HPC
725200930423     C* ORE X FEST. TRAZ. INTERNE
725300941228     C                   Z-ADD     W98HFI        W98
725400941228     C                   EXSR      W98999
725500941228     C                   Z-ADD     D98           D98HFI
725600930423     C* ORE X FEST. TRAZ. CLIENTE
725700941228     C                   Z-ADD     W98HFC        W98
725800941228     C                   EXSR      W98999
725900941228     C                   Z-ADD     D98           D98HFC
726000930423     C* ORE X FEST. CONS. INTERNE
726100941228     C                   Z-ADD     W98HEI        W98
726200941228     C                   EXSR      W98999
726300941228     C                   Z-ADD     D98           D98HEI
726400930423     C* ORE X FEST. CONS. CLIENTE
726500941228     C                   Z-ADD     W98HEC        W98
726600941228     C                   EXSR      W98999
726700941228     C                   Z-ADD     D98           D98HEC
726800930511     C* ORE X F.DEPOSITO INTERNE
726900941228     C                   Z-ADD     W98HDI        W98
727000941228     C                   EXSR      W98999
727100941228     C                   Z-ADD     D98           D98HDI
727200930511     C* ORE X F.DEPOSITO CLIENTE
727300941228     C                   Z-ADD     W98HDC        W98
727400941228     C                   EXSR      W98999
727500941228     C                   Z-ADD     D98           D98HDC
727600930511     C* ORE X CONTRASSEGNO INTERNE
727700941228     C                   Z-ADD     W98HOI        W98
727800941228     C                   EXSR      W98999
727900941228     C                   Z-ADD     D98           D98HOI
728000930511     C* ORE X CONTRASSEGNO CLIENTE
728100941228     C                   Z-ADD     W98HOC        W98
728200941228     C                   EXSR      W98999
728300941228     C                   Z-ADD     D98           D98HOC
728400930914     C* ORE DI CONSEGNABILITA' INTERNE
728500941228     C                   Z-ADD     W98OCI        W98
728600941228     C                   EXSR      W98000
728700941228     C                   Z-ADD     D98           D98OCI
728800930914     C* ORE DI CONSEGNABILITA' CLIENTE
728900941228     C                   Z-ADD     W98OCC        W98
729000941228     C                   EXSR      W98000
729100941228     C                   Z-ADD     D98           D98OCC
729200071122     C
729300071122     c* Per i codici bolla M prepagati annullati, azzero tutto
729400071122     c                   if        d98cbo='M '
729500071122     c                   clear                   d98rpi
729600071122     c                   clear                   d98rpc
729700071122     c                   clear                   d98rti
729800071122     c                   clear                   d98rtc
729900071122     c                   clear                   d98rci
730000071122     c                   clear                   d98rcc
730100071122     c                   clear                   d98rri
730200071122     c                   clear                   d98rrc
730300071122     c                   clear                   d98fpi
730400071122     c                   clear                   d98fpc
730500071122     c                   clear                   d98fti
730600071122     c                   clear                   d98ftc
730700071122     c                   clear                   d98fci
730800071122     c                   clear                   d98fcc
730900071122     c                   clear                   d98fri
731000071122     c                   clear                   d98frc
731100071122     c                   clear                   d98npi
731200071122     c                   clear                   d98npc
731300071122     c                   clear                   d98nti
731400071122     c                   clear                   d98ntc
731500071122     c                   clear                   d98nci
731600071122     c                   clear                   d98ncc
731700071122     c                   clear                   d98nri
731800071122     c                   clear                   d98nrc
731900071122     c                   clear                   d98OCi
732000071122     c                   clear                   d98OCC
732100071122     c                   endif
732200930423     C*
732300100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
732400100308     c*  calcolo
732500100308     c                   eval      d98dcr=s98dcr
732600100308     c                   eval      d98hcr=s98hcr
732700100308     c                   eval      d98tcr=s98tcr
732800100308     c
732900930423     C                   ENDSR
733000941228     C***********************************************************************
733100941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
733200941228     C***********************************************************************
733300941228     C     W98999        BEGSR
733400941228     C     W98           IFLT      -999
733500941228     C                   Z-ADD     -999          D98               3 0
733600941228     C                   ELSE
733700941228     C     W98           IFGT      999
733800941228     C                   Z-ADD     999           D98
733900941228     C                   ELSE
734000941228     C                   Z-ADD     W98           D98
734100941228     C                   END
734200941228     C                   END
734300941228     C                   ENDSR
734400941228     C***********************************************************************
734500941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
734600941228     C***********************************************************************
734700941228     C     W98000        BEGSR
734800941228     C     W98           IFLE      *ZEROS
734900941228     C                   Z-ADD     *ZEROS        D98
735000941228     C                   ELSE
735100941228     C     W98           IFGT      999
735200941228     C                   Z-ADD     999           D98
735300941228     C                   ELSE
735400941228     C                   Z-ADD     W98           D98
735500941228     C                   END
735600941228     C                   END
735700941228     C                   ENDSR
735800930305     C***********************************************************************
735900930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
736000930305     C***********************************************************************
736100930305     C     AZZERA        BEGSR
736200150219     c* data e ora del momento
736300150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
736400150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
736500150219     C                   Z-ADD     DTORA4        WHHODI            4 0
736600930305     C*
736700141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
736800141107     c                   clear                   or_ddc
736900141108     c                   clear                   sav_12dat         8 0
737000141107     c                   clear                   or_datarif
737100140402     c                   clear                   wpick2            1
737200140402     c                   clear                   Primotipo
737300090928     c                   clear                   Primodev
737400090928     c                   clear                   Primohev
737500070827     c                   clear                   CallAR5
737600071122     c                   clear                   d98cbo
737700100308     c                   clear                   s98dcr
737800100308     c                   clear                   s98hcr
737900100308     c                   clear                   s98tcr
738000141106     c                   clear                   nosomma           1
738100141229     c                   clear                   wtrasm
738200150324     c                   clear                   avvcmc
738300150325     c                   clear                   DSbl4L
738400150401     c                   clear                   fnlv82ds
738500150331     c                   clear                   DS7R
738600930323     C                   MOVEL     '0'           D98ERR
738700930323     C                   MOVEL     *BLANKS       D98MSG
738800931124     C                   Z-ADD     *ZEROS        D98TPI
738900931124     C                   Z-ADD     *ZEROS        D98TPC
739000930325     C                   Z-ADD     *ZEROS        D98TTI
739100930325     C                   Z-ADD     *ZEROS        D98TTC
739200930325     C                   Z-ADD     *ZEROS        D98TCI
739300930325     C                   Z-ADD     *ZEROS        D98TCC
739400090827     C                   Z-ADD     *ZEROS        w99TCI
739500090827     C                   Z-ADD     *ZEROS        w99TCC
739600930325     C                   Z-ADD     *ZEROS        D98TRI
739700930325     C                   Z-ADD     *ZEROS        D98TRC
739800090921     C                   Z-ADD     *ZEROS        w99TRI
739900090921     C                   Z-ADD     *ZEROS        w99TRC
740000931124     C                   Z-ADD     *ZEROS        D98RPI
740100931124     C                   Z-ADD     *ZEROS        D98RPC
740200930405     C                   Z-ADD     *ZEROS        D98RTI
740300930405     C                   Z-ADD     *ZEROS        D98RTC
740400930405     C                   Z-ADD     *ZEROS        D98RCI
740500930405     C                   Z-ADD     *ZEROS        D98RCC
740600930405     C                   Z-ADD     *ZEROS        D98RRI
740700930405     C                   Z-ADD     *ZEROS        D98RRC
740800931124     C                   Z-ADD     *ZEROS        D98FPI
740900931124     C                   Z-ADD     *ZEROS        D98FPC
741000930325     C                   Z-ADD     *ZEROS        D98FTI
741100930325     C                   Z-ADD     *ZEROS        D98FTC
741200930325     C                   Z-ADD     *ZEROS        D98FCI
741300930325     C                   Z-ADD     *ZEROS        D98FCC
741400930325     C                   Z-ADD     *ZEROS        D98FRI
741500930325     C                   Z-ADD     *ZEROS        D98FRC
741600931124     C                   MOVEL     *BLANKS       D98CPI
741700931124     C                   MOVEL     *BLANKS       D98CPC
741800930325     C                   MOVEL     *BLANKS       D98CTI
741900930325     C                   MOVEL     *BLANKS       D98CTC
742000930325     C                   MOVEL     *BLANKS       D98CCI
742100930325     C                   MOVEL     *BLANKS       D98CCC
742200930325     C                   MOVEL     *BLANKS       D98CRI
742300930325     C                   MOVEL     *BLANKS       D98CRC
742400931124     C                   Z-ADD     *ZEROS        D98NPI
742500931124     C                   Z-ADD     *ZEROS        D98NPC
742600930325     C                   Z-ADD     *ZEROS        D98NTI
742700930325     C                   Z-ADD     *ZEROS        D98NTC
742800930325     C                   Z-ADD     *ZEROS        D98NCI
742900930325     C                   Z-ADD     *ZEROS        D98NCC
743000930325     C                   Z-ADD     *ZEROS        D98NRI
743100930325     C                   Z-ADD     *ZEROS        D98NRC
743200930414     C                   Z-ADD     *ZEROS        D98HHI
743300930414     C                   Z-ADD     *ZEROS        D98HHC
743400930414     C                   Z-ADD     *ZEROS        D98HOI
743500930414     C                   Z-ADD     *ZEROS        D98HOC
743600930414     C                   Z-ADD     *ZEROS        D98HLI
743700930414     C                   Z-ADD     *ZEROS        D98HLC
743800930414     C                   Z-ADD     *ZEROS        D98HGI
743900930414     C                   Z-ADD     *ZEROS        D98HGC
744000931124     C                   Z-ADD     *ZEROS        D98HPI
744100931124     C                   Z-ADD     *ZEROS        D98HPC
744200930414     C                   Z-ADD     *ZEROS        D98HFI
744300930414     C                   Z-ADD     *ZEROS        D98HFC
744400930416     C                   Z-ADD     *ZEROS        D98HEI
744500930416     C                   Z-ADD     *ZEROS        D98HEC
744600930414     C                   Z-ADD     *ZEROS        D98HRI
744700930414     C                   Z-ADD     *ZEROS        D98HRC
744800930511     C                   Z-ADD     *ZEROS        D98HDI
744900930511     C                   Z-ADD     *ZEROS        D98HDC
745000930325     C                   Z-ADD     *ZEROS        D98DCI
745100930325     C                   Z-ADD     *ZEROS        D98HCI
745200930325     C                   Z-ADD     *ZEROS        D98DCE
745300930325     C                   Z-ADD     *ZEROS        D98HCE
745400931125     C                   Z-ADD     *ZEROS        D98OCI
745500931125     C                   Z-ADD     *ZEROS        D98OCC
745600931202     C                   Z-ADD     *ZEROS        D98DEI
745700931202     C                   Z-ADD     *ZEROS        D98OEI
745800931202     C                   Z-ADD     *ZEROS        D98DEE
745900931202     C                   Z-ADD     *ZEROS        D98OEE
746000150325     C                   clear                   D98dtctd
746100150326     C                   clear                   D98picklim
746200150325     C                   clear                   D98spcdee
746300150331     C                   clear                   fddata
746400140825     c
746500141017     c                   clear                   d98ddc
746600141017     c                   clear                   d98cons
746700141017     c                   clear                   wd98ndc
746800140910     c                   clear                   wd98fbc
746900150119     c                   clear                   wd98acc
747000140825     c                   clear                   wtrulor
747100150219     c
747200150219     c                   clear                   IMPdata
747300150219     c                   clear                   IMPfgs
747400150219     c                   clear                   DISdata
747500150219     c                   clear                   DISfgs
747600150219     c                   clear                   DISora
747700150219     c                   clear                   DISnpg
747800150219     c                   clear                   IDnpg
747900150219     c                   clear                   IDdata
748000150219     c                   clear                   IDora
748100150219     c                   clear                   IDfgs
748200931125     C* CAMPI VARI
748300931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
748400140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
748500140825      *   DALLA LETTURA DI QUESTI FILE                               *
748600931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
748700931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
748800931125     C     D98NSP        IFNE      *ZEROS
748900930525     C                   MOVEL     *BLANKS       D98TSP
749000941228     C                   MOVEL     *BLANKS       D98CAD
749100970728     C                   MOVEL     *BLANKS       D98LOD
749200930525     C                   Z-ADD     *ZEROS        D98TFP
749300930525     C                   Z-ADD     *ZEROS        D98TFA
749400930525     C                   Z-ADD     *ZEROS        D98LNA
749500930525     C                   Z-ADD     *ZEROS        D98DSP
749600941228     C                   Z-ADD     *ZEROS        D98MGS
749700930525     C                   Z-ADD     *ZEROS        D98DTI
749800930525     C                   Z-ADD     *ZEROS        D98HTI
749900930525     C                   Z-ADD     *ZEROS        D98DCM
750000930525     C                   Z-ADD     *ZEROS        D98HMC
750100930525     C                   MOVEL     *BLANKS       D98GC1
750200930525     C                   MOVEL     *BLANKS       D98GC2
750300010820     C                   Z-ADD     *ZEROS        D98CAS
750400010821     C                   MOVEL     *BLANKS       D98VCA
750500930525     C                   MOVEL     *BLANKS       D98TIC
750600930525     C                   Z-ADD     *ZEROS        D98DCR
750700930525     C                   Z-ADD     *ZEROS        D98HCR
750800930525     C                   MOVEL     *BLANKS       D98TCR
750900050914     C                   MOVEL     *BLANKS       Consideraora      1
751000930525     C                   MOVEL     *BLANKS       D98CMC
751100930525     C                   Z-ADD     *ZEROS        D98DAG
751200930525     C                   Z-ADD     *ZEROS        D98DCG
751300930525     C                   MOVEL     *BLANKS       D98FFD
751400930525     C                   Z-ADD     *ZEROS        D98NCD
751500930525     C                   MOVEL     *BLANKS       D98FNS
751600931124     C                   Z-ADD     *ZEROS        D98DAM
751700940628     C                   MOVEL     *BLANKS       D98FIN
751800940628     C                   Z-ADD     *ZEROS        D98KSC
751900940628     C                   MOVEL     *BLANKS       D98TBL
752000931125     C                   END
752100960229     C     D98TLA        IFEQ      'T'
752200960229     C                   Z-ADD     *ZEROS        D98DTI
752300960229     C                   Z-ADD     *ZEROS        D98HTI
752400960229     C                   ENDIF
752500931125     C* CAMPI WORK
752600930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
752700930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
752800150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
752900930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
753000941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
753100960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
753200930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
753300930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
753400940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
753500930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
753600150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
753700931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
753800931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
753900930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
754000930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
754100930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
754200930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
754300930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
754400940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
754500941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
754600930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
754700050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
754800050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
754900140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
755000930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
755100930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
755200930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
755300930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
755400930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
755500930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
755600930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
755700930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
755800930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
755900930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
756000930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
756100930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
756200930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
756300930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
756400140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
756500931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
756600060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
756700960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
756800931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
756900140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
757000140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
757100140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
757200941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
757300970304     C******************** Z-ADD*ZEROS    W98DBR  80
757400970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
757500150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
757600050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
757700930414     C*
757800941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
757900941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
758000930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
758100930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
758200930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
758300930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
758400930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
758500930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
758600941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
758700941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
758800930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
758900930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
759000930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
759100930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
759200930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
759300930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
759400941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
759500941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
759600930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
759700930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
759800930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
759900930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
760000930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
760100930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
760200930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
760300930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
760400060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
760500060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
760600060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
760700060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
760800930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
760900930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
761000930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
761100930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
761200930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
761300930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
761400930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
761500930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
761600930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
761700930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
761800930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
761900930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
762000930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
762100930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
762200941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
762300941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
762400930423     C*
762500930511     C                   Z-ADD     *ZEROS        W98HF             5 0
762600930423     C                   Z-ADD     *ZEROS        W98HH             5 0
762700060418     C                   Z-ADD     *ZEROS        W98EG             5 0
762800060420     C                   Z-ADD     *ZEROS        W98EA             5 0
762900930423     C                   Z-ADD     *ZEROS        W98HG             5 0
763000930423     C                   Z-ADD     *ZEROS        W98HL             5 0
763100930423     C                   Z-ADD     *ZEROS        W98HO             5 0
763200930511     C                   Z-ADD     *ZEROS        W98HD             5 0
763300930611     C*
763400930611     C                   Z-ADD     *ZEROS        WORELA            3 0
763500960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
763600930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
763700930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
763800931207     C                   Z-ADD     *ZEROS        WORECR            3 0
763900090902     c
764000090902     c                   clear                   wmancaTRAZ
764100150113     c                   clear                   wcaldpcee         1
764200150220     c                   clear                   evemic            2
764300930305     C*
764400060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
764500060406     C                   clear                   LAF                            *LASC.AVVISO FINE
764600091014     C                   clear                   LAp                            *posizione l.avviso
764700060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
764800060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
764900060406c    c                   clear                   LCon
765000060406     C                   clear                   EVD                            *DATA EVENTO
765100060406     C                   clear                   EVO                            *ORA EVENTO
765200060406     C                   clear                   EVC                            *CAUSALE EVENTO
765300131218     C                   clear                   EVT
765400060406     C                   clear                   pVD                            *DATA EVENTO
765500060406     C                   clear                   pVO                            *ORA EVENTO
765600060406     C                   clear                   pVC                            *CAUSALE EVENTO
765700060406     C                   clear                   pVF                            *CAUSALE EVENTO
765800060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
765900060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
766000060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
766100060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
766200060406     C                   clear                   COV                            *DATE FINE L.A. E GI
766300060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
766400060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
766500091014     C                   clear                   GIp                            *posizione vento gia
766600060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
766700060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
766800060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
766900931213     C*
767000060406     C                   clear                   TUC                            *TURNI CHIUSURA
767100931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
767200931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
767300060406     c
767400060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
767500060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
767600060406     c                   clear                   pgii
767700060406     c                   clear                   pgif
767800060406     c                   clear                   pgci
767900060406     c                   clear                   pgcf
768000060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
768100060420     c                   clear                   paii
768200060420     c                   clear                   paif
768300930420     C*
768400930305     C                   ENDSR
768500930512     C***********************************************************************
768600930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
768700930512     C***********************************************************************
768800930512     C     FORCON        BEGSR
768900930512     C*
769000931124     C                   Z-ADD     *ZEROS        D98RPI
769100931124     C                   Z-ADD     *ZEROS        D98RPC
769200930512     C                   Z-ADD     *ZEROS        D98RTI
769300930512     C                   Z-ADD     *ZEROS        D98RTC
769400930512     C                   Z-ADD     *ZEROS        D98RCI
769500930512     C                   Z-ADD     *ZEROS        D98RCC
769600930512     C                   Z-ADD     *ZEROS        D98RRI
769700930512     C                   Z-ADD     *ZEROS        D98RRC
769800931124     C                   Z-ADD     *ZEROS        D98FPI
769900931124     C                   Z-ADD     *ZEROS        D98FPC
770000930512     C                   Z-ADD     *ZEROS        D98FTI
770100930512     C                   Z-ADD     *ZEROS        D98FTC
770200930512     C                   Z-ADD     *ZEROS        D98FCI
770300930512     C                   Z-ADD     *ZEROS        D98FCC
770400930512     C                   Z-ADD     *ZEROS        D98FRI
770500930512     C                   Z-ADD     *ZEROS        D98FRC
770600931124     C                   MOVEL     'S'           D98CPI
770700931124     C                   MOVEL     'S'           D98CPC
770800930512     C                   MOVEL     'S'           D98CTI
770900930512     C                   MOVEL     'S'           D98CTC
771000930512     C                   MOVEL     'S'           D98CCI
771100930512     C                   MOVEL     'S'           D98CCC
771200930512     C                   MOVEL     'S'           D98CRI
771300930512     C                   MOVEL     'S'           D98CRC
771400931124     C                   Z-ADD     *ZEROS        D98NPI
771500931124     C                   Z-ADD     *ZEROS        D98NPC
771600930512     C                   Z-ADD     *ZEROS        D98NTI
771700930512     C                   Z-ADD     *ZEROS        D98NTC
771800930512     C                   Z-ADD     *ZEROS        D98NCI
771900930512     C                   Z-ADD     *ZEROS        D98NCC
772000930512     C                   Z-ADD     *ZEROS        D98NRI
772100930512     C                   Z-ADD     *ZEROS        D98NRC
772200930512     C                   Z-ADD     *ZEROS        D98HHI
772300930512     C                   Z-ADD     *ZEROS        D98HHC
772400930512     C                   Z-ADD     *ZEROS        D98HOI
772500930512     C                   Z-ADD     *ZEROS        D98HOC
772600930512     C                   Z-ADD     *ZEROS        D98HLI
772700930512     C                   Z-ADD     *ZEROS        D98HLC
772800930512     C                   Z-ADD     *ZEROS        D98HGI
772900930512     C                   Z-ADD     *ZEROS        D98HGC
773000931124     C                   Z-ADD     *ZEROS        D98HPI
773100931124     C                   Z-ADD     *ZEROS        D98HPC
773200930512     C                   Z-ADD     *ZEROS        D98HFI
773300930512     C                   Z-ADD     *ZEROS        D98HFC
773400930512     C                   Z-ADD     *ZEROS        D98HEI
773500930512     C                   Z-ADD     *ZEROS        D98HEC
773600930512     C                   Z-ADD     *ZEROS        D98HRI
773700930512     C                   Z-ADD     *ZEROS        D98HRC
773800930512     C                   Z-ADD     *ZEROS        D98HDI
773900930512     C                   Z-ADD     *ZEROS        D98HDC
774000930520     C*
774100930512     C                   ENDSR
774200930305     C********************************************************************
774300971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
774400950406031059090120151181212243273304334365
774500950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
774600950406312831303130313130313031
774700930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
774800971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
774900971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
775000971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
775100971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
775200971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
775300971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
775400971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
775500971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
775600971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
775700971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
775800971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
775900971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
776000971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
776100971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
776200971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
776300971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
776400971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
776500971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
776600971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
776700971127TNSD99R- Bolla inesistente.                                               BOIN20
776800971127TNSD99R-     Cap inesistente.                                             CAPI21
776900971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
777000971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
777100971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
777200971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
777300971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
777400971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
777500971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
777600971127TNSD99R- Contrassegno non trovato.                                        CONT29
777700971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
777800990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
777900990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
778000060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
778100060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
778200971127TNSD99R- 35                                                                   35
778300971127TNSD99R- 36                                                                   36
778400971127TNSD99R- 37                                                                   37
778500971127TNSD99R- 38                                                                   38
778600971127TNSD99R- 39                                                                   39
778700971127TNSD99R- 40                                                                   40
