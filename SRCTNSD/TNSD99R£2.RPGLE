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
232800150608     c* ai fini del calcolo affidabilità
232900150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
233000150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
233100931206     C                   ELSE
233200931206     C                   Z-ADD     WPOCRF        WPOCR2
233300931206     C                   END
233400150608     c* ai fini della data prevista consegna cliente
233500150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
233600150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
233700150608     C                   ELSE
233800150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
233900150608     C                   END
234000150331     c
234100150331     c                   if        wpocrf_dee>0
234200150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
234300150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
234400150330     C                   END
234500150331     C                   END
234600150331     C*
234700150331     c                   if        wpocrf_dee=0
234800150608     C     wpocr2_dee    sub       24            WPOCRF_dee
234900150331     c                   endif
235000150331     c
235100930526     C                   END                                                    <-- 1 ---
235200930511     C*
235300930511     C                   ENDSR
235400930511     C********************************************************************
235500930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
235600930511     C********************************************************************
235700930511     C     CALLAV        BEGSR
235800930511     C*
235900941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
236000930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
236100930421     C*
236200930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
236300930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
236400930421     C     1             DO        IE            E                 3 0          --- 2 -->
236500930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
236600930421     C                   Z-ADD     1             I
236700930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
236800060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
236900930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
237000930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
237100930421     C                   ADD       1             IL
237200930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
237300930421     C                   EXSR      DATORE
237400930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
237500091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
237600091014     c* Mi salvo la posizione dell'evento
237700091014     C                   Z-ADD     E             LAp(IL)
237800930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
237900930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
238000930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
238100930421     C                   EXSR      RO3ORE
238200930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
238300930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
238400930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
238500930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
238600091015     c**                 if        §2agla>1
238700091015     c**                 sub       1             §2agla
238800930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
238900091015     c**                 else
239000091015     c**                 clear                   whh
239100091015     c**                 endif
239200930510     C                   ADD       WHH           WORELA
239300940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
239400930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
239500090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
239600150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
239700930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
239800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
239900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
240000930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
240100930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
240200930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
240300930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
240400931209     C                   Z-ADD     LAF(IL)       WR7POI
240500931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
240600931202     C     WR7POO        IFGT      LAF(IL)
240700931201     C                   Z-ADD     WR7POO        LAF(IL)
240800931202     C                   END
240900930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
241000930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
241100091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
241200930510     C                   END                                                    <-- 3 ---
241300931223     C                   ENDDO                                                  <-- 2 ---
241400930421     C*
241500930421     C                   END                                                    <-- 1 ---
241600930618££$$ C*
241700930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
241800931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
241900941228££$$ C     1             DO        IL            I
242000930618££$$ C     LAF(I)        IFGT      WPOLAF
242100930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
242200930618££$$ C                   END
242300931223££$$ C                   ENDDO
242400930511     C*
242500930511     C                   ENDSR
242600940324£$   C********************************************************************
242700940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
242800940324£$   C********************************************************************
242900940324£$   C     CALLAC        BEGSR
243000941229     C*
243100950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
243200950407     C*
243300950210     C                   Z-ADD     WORELA        WTM060
243400941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
243500941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
243600941229     C     D98DAG        IFEQ      *ZEROS
243700050520     c     §2aftg        andeq     'S'
243800090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
243900090827     C     WORELA        ADD       w99TCC        WTM060            6 0
244000941229     C                   ENDIF
244100941229     C*
244200940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
244300940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
244400941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
244500941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
244600941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
244700940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
244800941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
244900050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
245000050520     c                   movel     §2aftg        LCON(IL)
245100950407     C*
245200950407     C                   ENDIF
245300960129     C*
245400940324£$   C                   ENDSR
245500930511     C********************************************************************
245600930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
245700930511     C********************************************************************
245800930511     C     CALGIA        BEGSR
245900960129     C*
246000960129     C*-------------------------------------------------------------------
246100960129     C* EVENTI GIACENZA
246200960129     C*-------------------------------------------------------------------
246300960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
246400960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
246500960129     C*
246600050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
246700041116     c     d98nsp        ifgt      0
246800031125     C                   EXSR      CARGIAC
246900041116     c                   endif
247000031125     C*
247100960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
247200960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
247300960129     C     1             DO        IE            E                 3 0          --- 2 -->
247400960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
247500960129     C                   Z-ADD     1             I
247600960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
247700060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
247800960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
247900960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
248000960129     C                   ADD       1             IG
248100960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
248200960129     C                   EXSR      DATORE
248300960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
248400140606     c* Mi salvo la posizione dell'evento
248500140606     C                   Z-ADD     E             gip(IG)
248600960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
248700960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
248800960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
248900960129     C                   EXSR      RO3ORE
249000960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
249100960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
249200960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
249300960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
249400960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
249500960129     C                   ADD       WHH           WOREGI
249600960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
249700960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
249800090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
249900090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
250000960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
250100960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
250200960129     C                   MOVEL     'C'           WIC                            *CLIENTE
250300960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
250400960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
250500960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
250600960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
250700960129     C                   Z-ADD     GIF(IG)       WR7POI
250800960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
250900031125     C     WR7POO        IFGT      GIF(IG)
251000031125     C                   Z-ADD     WR7POO        GIF(IG)
251100031125     C                   ENDIF
251200031201     C*
251300031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
251400031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
251500031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
251600031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
251700031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
251800031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
251900031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
252000031201     C                   IF        WPOATT > GIF(IG)
252100031201     C*
252200031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
252300031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
252400031201     C                   IF        WGGA = *BLANKS
252500031201     C                   SETOFF                                       54
252600031201     C                   ELSE
252700031201     C     WGGA          LOOKUP    SK7QW                                  54
252800031201     C                   ENDIF
252900031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
253000031201     C                   MOVEL     '1'           FLGGIACOK
253100031201     C                   ELSE
253200140127     c
253300031125     C*
253400031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
253500031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
253600031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
253700031125     C                   MOVEL     '0'           FLGGIACOK         1
253800031125     C*
253900031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
254000031125     C     1             DO        IAG           EG                3 0
254100031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
254200031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
254300031125     C                   EXSR      DATORE
254400031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
254500031125     C*
254600031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
254700031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
254800031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
254900031125     C                   EXSR      DATORE
255000031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
255100031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
255200031229     C* => NN LO CONSIDERO
255300031229     C                   IF        EVC(E+1) = 'G' AND
255400031229     C                             WPROXGII = WGCPDORE
255500031229     C                   MOVEL     '1'           FLGGIACOK
255600031229     C                   ENDIF
255700031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
255800031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
255900031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
256000031125     C                   EXSR      RO3ORE
256100031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
256200031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
256300031125     C* DELL'EVENTO GIACENZA CORRENTE
256400031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
256500060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
256600031125     C                   MOVEL     '1'           FLGGIACOK
256700031125     C                   LEAVE
256800031125     C                   ENDIF
256900031125     C                   ELSE
257000031229     C                   IF        EVC(E)  = 'G' AND
257100031229     C                             GII(IG) = WGCPDORE
257200031229     C                   MOVEL     '1'           FLGGIACOK
257300031229     C                   LEAVE
257400031229     C                   ENDIF
257500031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
257600031125     C                   MOVEL     '1'           FLGGIACOK
257700031125     C                   LEAVE
257800031125     C                   ENDIF
257900031125     C                   ENDIF
258000031125     C                   ENDDO
258100031201     C                   ENDIF
258200140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
258300140127     C                   IF        FLGGIACOK = '0'  and
258400140127     c                             d98dcm=evd(E)
258500140127     C                   MOVEL     '1'           FLGGIACOK
258600140127     c                   endif
258700031125     C*
258800031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
258900031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
259000031125     C* DELL'EVENTO GIACENZA CORRENTE
259100031125     C                   IF        FLGGIACOK = '0'
259200031125     C                   Z-ADD     *ZEROS        GIF(IG)
259300031125     C                   ELSE
259400031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
259500031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
259600031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
259700031125     C                   ENDIF
259800031125     C                   ELSE
259900031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
260000031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
260100031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
260200031125     C                   ENDIF
260300031125     C*
260400960129     C                   ENDIF                                                  <-- 3 ---
260500960129     C                   ENDDO                                                  <-- 2 ---
260600960129     C*
260700960129     C                   ENDIF                                                  <-- 1 ---
260800960129££$$ C*
260900960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
261000960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
261100960129££$$ C     1             DO        IG            I
261200960129££$$ C     GIF(I)        IFGT      WPOGIF
261300960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
261400960129££$$ C                   ENDIF
261500960129££$$ C                   ENDDO
261600941229     C*
261700941229     C*-------------------------------------------------------------------
261800941229     C* APERTURA GIACENZA
261900941229     C*-------------------------------------------------------------------
262000960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
262100960129     C*
262200960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
262300931125     C                   Z-ADD     D98DAG        WDAT
262400930325     C                   EXSR      DATORE
262500950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
262600960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
262700950413£££  C                   Z-ADD     WORE          WPODAG
262800960129$$£  C*********************ENDIF
262900930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
263000930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
263100930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
263200930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
263300930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
263400941229     C*
263500941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
263600941229     C                   Z-ADD     WPODAG        WPOAGC
263700960129     C*
263800960129     C                   ENDIF                                                  <-- 1 ---
263900941229     C*
264000941229     C*-------------------------------------------------------------------
264100930511     C* CHIUSURA GIACENZA
264200941229     C*-------------------------------------------------------------------
264300930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
264400930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
264500931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
264600931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
264700931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
264800931201     C                   Z-ADD     D98DCM        W98DCG
264900930416     C                   ELSE
265000930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
265100930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
265200931201     C                   Z-ADD     WDAODI        W98DCG
265300061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
265400061123     c*  chiusura giacenza, come già fa in altri posti
265500061123     c                   if        d98dfc>0
265600061123     C                   Z-ADD     d98dfc        W98DCG
265700061123     c                   endif
265800061123     c
265900930409     C                   END
266000930409     C                   END
266100930409     C                   END
266200930414     C* TRASF.DATA CHIUSURA
266300931201     C     W98DCG        IFNE      *ZEROS
266400931201     C                   Z-ADD     W98DCG        WDAT
266500930325     C                   EXSR      DATORE
266600930326     C                   Z-ADD     WORE          WPODCG
266700930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
266800930407     C* GIORNO DA ESCLUDERE
266900150216     C                   ADD       12            WPODCG
267000150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
267100150123     c*  il giorno di chiusura dovrebe già essere protetto
267200941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
267300941229     C     WPOCGC        IFEQ      *ZEROS
267400941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
267500941229     C                   ENDIF
267600930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
267700930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
267800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
267900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
268000150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
268100150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
268200930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
268300930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
268400930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
268500931209     C                   Z-ADD     WPODCG        WR7POI
268600931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
268700931202     C     WR7POO        IFGT      WPODCG
268800931201     C                   Z-ADD     WR7POO        WPODCG
268900930326     C                   END
269000931202     C                   END
269100930511     C*
269200930511     C                   ENDSR
269300031125     C********************************************************************
269400031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
269500031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
269600031125     C*             DEGLI EVENTI DI TIPO GIACENZA
269700031125     C********************************************************************
269800031125     C     CARGIAC       BEGSR
269900031125     C*
270000031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
270100031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
270200031125     C*
270300050405     C                   Z-ADD     D98AAS        KWGCPAAS
270400050405     C                   Z-ADD     D98LNP        KWGCPLNP
270500050405     C                   Z-ADD     D98NRS        KWGCPNRS
270600050405     C                   Z-ADD     D98NSP        KWGCPNSP
270700031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
270800050405     C     KEYGC_P       SETLL     TIGCP01L
270900050405     C                   IF        %equal(TIGCP01L)
271000050405     C     KEYGC_P       READE     TIGCP01L
271100050405     C                   DOW       not %eof(TIGCP01L)
271200031125     C                   ADD       1             IAG
271300031125     C                   EVAL      GCPD(IAG) = GCPDUR
271400050405     C     KEYGC_P       READE     TIGCP01L
271500031125     C                   ENDDO
271600031125     C                   ENDIF
271700031125     C                   ELSE
271800050405     C     KEYGC_P       SETLL     TIGCP21L
271900050405     C                   IF        %equal(TIGCP21L)
272000050405     C     KEYGC_P       READE     TIGCP21L
272100050405     C                   DOW       not %eof(TIGCP21L)
272200031125     C                   ADD       1             IAG
272300031125     C                   EVAL      GCPD(IAG) = GCPDUR
272400050405     C     KEYGC_P       READE     TIGCP21L
272500031125     C                   ENDDO
272600031125     C                   ENDIF
272700031125     C                   ENDIF
272800031125     C*
272900031125     C                   ENDSR
273000941229     C********************************************************************
273100960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
273200941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
273300941229     C********************************************************************
273400960129     C     CALGIC        BEGSR
273500941229     C*
273600960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
273700960129     C*
273800960129     C                   Z-ADD     WOREGI        WTM060
273900960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
274000960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
274100960129     C     D98DAG        IFEQ      *ZEROS
274200960129$$£  C     WOREGI        ADD       12            WTM060            6 0
274300960129     C                   ENDIF
274400960129     C*
274500960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
274600960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
274700960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
274800960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
274900960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
275000960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
275100960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
275200960129     C*
275300960129     C                   ENDIF
275400941229     C*
275500941229     C                   ENDSR
275600060418     C********************************************************************
275700060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
275800060418     c*            l'affidabilità
275900060418     C********************************************************************
276000060418     C     CALCEPA       BEGSR
276100060418     C*
276200060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
276300060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
276400060420     c                   clear                   numggcepa         3 0
276500060418     C*
276600060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
276700060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
276800060420     c                   eval      wgiroepa='0'
276900060418     c                   eval      wfine=' '
277000060418     c
277100060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
277200060420    2C     W             DOwle     IP                                           --- 2 -->
277300060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
277400060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
277500060418     c
277600060420    4c                   if        wgiroepa='0'
277700060420     c                   exsr      ElaInizCEPA
277800060418   x4c                   else
277900060420    5c                   if        pvd(W)>wdat
278000060418     c                   eval      wfine='S'
278100060418    5c                   endif
278200060418    4c                   endif
278300060418     c
278400060420    4c                   if        wfine=' ' and pvd(W)=wdat
278500060420     C*
278600060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
278700060420     c*  finale
278800060420     c                   add       1             numggcepa
278900060420     c
279000060420     C                   z-add     24            WORELA
279100060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
279200060418     c
279300060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
279400060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
279500060418     C                   MOVEL     'C'           WIC                            *CLIENTE
279600060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
279700060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
279800060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
279900060418     c
280000060418     c* Trasformo le ore in data e verifico se c'e' altro evento
280100060418     c*  in sta data che allunga ancora
280200060420     c                   z-add     wr4poo        wore
280300060418     c                   EXSR      OREDAT
280400060418    4c                   endif
280500060418     c
280600060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
280700060418    4c                   if        wfine='S'
280800060418     c                   EXSR      ELAFINECEPA
280900060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
281000060420     c*  inizio range
281100060420     c                   else
281200060420     c                   add       1             w
281300060418    4C                   ENDif                                                  <-- 3 ---
281400060420     c
281500060420     c                   else
281600060420     c                   add       1             w
281700060418    3C                   ENDif                                                  <-- 3 ---
281800060418    2C                   ENDDO                                                  <-- 2 ---
281900060418     c
282000060420     c                   if        wgiroepa='1'
282100060418     c                   EXSR      ELAFINECEPA
282200060418     c                   endif
282300060418     C*
282400060418    1C                   END                                                    <-- 1 ---
282500060418££$$ C*
282600060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
282700060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
282800060420££$$ C     1             DO        Ipa           w
282900060420££$$ C     paif(w)       IFGT      WPOpaif
283000060420££$$ C                   Z-ADD     paif(w)       WPOpaif
283100060418££$$ C                   END
283200060418££$$ C                   ENDDO
283300060418     C*
283400060418     C                   ENDSR
283500060420     C********************************************************************
283600060420     c* Elabora Inizio range eventi particolari che allungano
283700060420     C********************************************************************
283800060420     c     ElaInizCEPA   BEGSR
283900060420     C                   ADD       1             IPA
284000060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
284100060420     C                   EXSR      DATORE
284200060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
284300061017     c
284400060420     c                   eval      wgiroepa='1'
284500060420     c                   eval      wdat=pvd(w)
284600060420     C                   ENDSR
284700060418     C********************************************************************
284800060418     c* Elabora fine range eventi particolari che allungano
284900060418     C********************************************************************
285000060418     c     ElafineCEPA   BEGSR
285100060420     c     numggcepa     mult      24            worela
285200060420     c                   sub       12            worela
285300090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
285400090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
285500060418     c
285600060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
285700060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
285800060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
285900060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
286000060418     C                   Z-ADD     paif(IPA)     WR7POI
286100060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
286200060418    5C     WR7POO        IFGT      paif(IPA)
286300060418     C                   Z-ADD     WR7POO        paif(IPA)
286400060418    5C                   END
286500061017     c*Sommo 24 ore di inizio giornata
286600061017     c*            : non ho capito perchè ma ci vogliono
286700061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
286800090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
286900090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
287000061017     c* Se tempi di consegna inferiori alle 24 ore , devo
287100061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
287200061017     c*  partire prima
287300061017     c****               if        d98tcc<24
287400061017     c****               sub       12            paii(ipa)
287500061017     c****               endif
287600060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
287700060418     c* Imposto nuovo giro
287800060420     c                   eval      wgiroepa='0'
287900060418     c                   eval      wfine=' '
288000060420     c                   clear                   numggcepa         3 0
288100060418     c                   ENDSR
288200930511     C********************************************************************
288300930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
288400930511     C********************************************************************
288500010822     C     CALASS        BEGSR
288600010821     C*
288700011211     C                   EXSR      REPMAXCSB
288800930511     C*
288900930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
289000010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
289100011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
289200930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
289300930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
289400930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
289500930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
289600930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
289700930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
289800930511     C                   ELSE                                                   <-- 4 -->
2899009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
290000930512     C                   END                                                    <-- 4 ---
290100930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
290200930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
290300930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
290400930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
290500930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
290600930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
290700930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
290800930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
290900930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
291000930413     C                   END                                                    <-- 2 ---
291100011211     C                   END                                                    <-- 1 ---
291200930406     C*
291300930325     C                   ENDSR
291400930608     C********************************************************************
291500930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
291600930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2917009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
291800930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
291900930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
292000930608     C********************************************************************
292100930608     C     RDRASS        BEGSR
292200930608     C*
292300930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2924009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2925009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2926009407151507 C*********************EXSR RO3ORE
2927009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
292800930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2929009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2930009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2931009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2932009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2933009407191907 C                   Z-ADD     WPOASI        WPOROU
2934009407191907 C                   EXSR      CHKFES
2935009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2936009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2937009407191907 C                   ELSE                                                   *E' FERIALE
2938009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
2939009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
2940009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
2941009407191907 C                   ENDIF
294200930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
294300930608     C                   MOVEL     'ASS1 '       WR3TAB
294400930608     C                   Z-ADD     *ZEROS        WR3HHI
294500930608     C                   EXSR      RO3ORE
294600930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
294700930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
294800931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
294900931129     C                   MOVEL     'C'           WIC                            *CLIENTE
295000930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
295100930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
295200930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
295300930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
295400930608     C*
295500930608     C                   ENDSR
295600930511     C********************************************************************
295700930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
295800930511     C********************************************************************
295900930511     C     CALFED        BEGSR
296000150331     c                   clear                   fddata
296100150331     c                   clear                   WPOFDdata
296200930511     C*
296300051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
296400931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
296500051108     C     D98FFD        oreq      'M'                                          --- 1 -->
296600930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
296700930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
296800930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
296900930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
297000150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
297100150331     c                   if        §ar5dfd>*zeros
297200150331     c                   movel     §ar5dfd       fddata            8 0
297300150331     c                   endif
297400150331     c                   if        fddata>0 and ((d98dti>0 and
297500150331     c                             fddata>d98dti) or d98dti=0)
297600150331     C                   Z-ADD     fddata        WDAT
297700150331     C                   EXSR      DATORE
297800150331     C                   Z-ADD     WORE          WPOfddata
297900150331     c                   endif
298000150331     c
298100930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
298200930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
298300930511     C                   ELSE                                                   <-- 2 -->
2984009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
298500930512     C                   END                                                    <-- 2 ---
298600150331     c                   if        wpofddata>0 and wpofddata>wpofdi
2987001504081507 C                   Z-ADD     WPOfddata     WPOFDI
298800150331     c                   endif
298900150331
299000930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
299100930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
299200930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
299300930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
299400930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
299500930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
299600930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
299700930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
299800930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
299900930511     C                   END                                                    <-- 1 ---
300000930511     C*
300100930511     C                   ENDSR
300200930608     C********************************************************************
300300930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
300400930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3005009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
300600930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
300700930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
300800930608     C********************************************************************
300900930608     C     RDRFED        BEGSR
301000930608     C*
301100930608     C* CALCOLO ORE DEL FERMO DEPOSITO
301200930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3013009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3014001503311907 C                   Z-ADD     WPOFDI        WPOROU
3015001503311907 C                   EXSR      CHKFES
3016001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3017001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3018001503311907 C                   ELSE                                                   *E' FERIALE
3019001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3020001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3021001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3022001503311907 C                   ENDIF
302300150331     c* Se presente la particolarità consegna con più giorni di fermo depo
302400150331     c*  uso questi
302500150331     c                   if        §7rggfd>*zeros
302600150424     c                   movel     §7rggfd       wggfd             2 0
302700150331     c     wggfd         mult      24            Wnore
302800150331     c                   add       wnore         worefd
302900150331     c                   else
303000930608     C                   MOVEL     'FED1 '       WR3TAB
303100930608     C                   Z-ADD     *ZEROS        WR3HHI
303200930608     C                   EXSR      RO3ORE
303300940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
303400150331     c                   endif
303500930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
303600931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
303700931129     C                   MOVEL     'C'           WIC                            *CLIENTE
303800930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
303900930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
304000930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
304100930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
304200930608     C*
304300930608     C                   ENDSR
304400931201     C********************************************************************
304500931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
304600931201     C********************************************************************
304700931201     C     CALDTE        BEGSR
304800140825     c
304900140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
305000140825     c*  che tiene conto solo dei festivi per restituire in output
305100140825     c*  la data teorica di consegna
305200140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
305300140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
305400140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
305500140825     C                   ELSE
305600140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
305700140825     C                   END
305800140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
305900141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
306000141106     c*  quindi passo un flag
306100141106     c                   eval      nosomma='N'
306200140825     C                   EXSR      RO4CDP
306300141106     c                   eval      nosomma=' '
306400140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
306500140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
306600140825     c*  di consegna
306700140825     c                   EXSR      TRAS_DEI
306800140825     c
306900140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
307000140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
307100140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
307200140825     c                             ='M' or %subst(d98gc2:2:1)='P')
307300140825     c                   exsr      Reper_tempi
307400140825     c                   endif
307500931201     C*
307600140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
307700140825     c*  che tiene conto di:
307800140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
307900150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
308000931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
308100931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
308200931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
308300931201     C                   ELSE
308400931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
308500931201     C                   END
308600090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
308700090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
308800931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
308900140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
309000931201     C*
309100150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
309200931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
309300931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
309400090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
309500931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
309600150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
309700150608     c
309800150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
309900150608     c*  che non tiene conto dei tempi del C/Ass
310000150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
310100150609     c*
310200150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
310300150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
310400150609     c                   if        wpodti>0 and wpodti<wpoptc_dee
310500150609     c                   Z-ADD     WPODTI        WR6POI
310600150609     c                   else
310700150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
310800150609     c                   endif
310900150609     c
311000150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
311100150608     c                   eval      wdee='S'
311200150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
311300150608     c                   eval      wdee=' '
311400150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
311500141017
311600141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
311700150219     C**                 EVAL      WTC='C'
311800150219     C**                 EVAL      WIC='C'
311900150219     c**                 z-add     wpoattdata00  WPOROU
312000150219     c**                 z-add     wdaodi        wpodata
312100150219     c**                 exsr      chkfes
312200150219     c**                 if        wslitt='S'
312300150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
312400150219     c**                 z-add     24            wr6noi
312500150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
312600150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
312700150219     c**                 else
312800150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
312900150219     c**                 endif
313000141204     c* Ricalcolo WPOrifDatLav
313100141229     C**                 MOVEL     'OSC  '       WR3TAB
313200141229     C**                 Z-ADD     WHHODI        WR3HHI
313300141229     C**                 EXSR      RO3ORE
313400141229     c**
313500141229     C**                 EVAL      WTC='C'
313600141229     C**                 EVAL      WIC='C'
313700141229     c**                 z-add     wpoattdata00  WPOROU
313800141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
313900141229     c**                 z-add     wr3hho        wr6noi
314000141229     c**                 add       12            wr6noi
314100141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
314200141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
314300141229     c
314400141017     c
314500141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
314600141017     c*  ma sempre devo slittare i festivi
314700141017     C* CONSEGNABILITA'
314800150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
314900150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
315000150219     C**                 EVAL      WTC='C'
315100150219     C**                 EVAL      WIC='C'
315200150219     c**                 z-add     d98dfc        WPOdata
315300150219     c**                 exsr      chkfes
315400150219     c**                 if        wslitt='S'
315500150219     c**                 z-add     wpodfc_00     Wr6poi
315600150219     c**                 z-add     24            wr6noi
315700150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
315800150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
315900150219     c**                 else
316000150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
316100150219     c**                 endif
316200150219     c**                 endif
316300931201     C*
316400931201     C                   ENDSR
316500930511     C********************************************************************
316600930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
316700930511     C********************************************************************
316800930511     C     CALDPC        BEGSR
316900930325     C*
317000930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
317100930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
317200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
317300930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
317400930325     C                   ELSE
317500930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
317600930407     C                   END
317700090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
317800090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
317900930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
318000090902     c
318100931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
318200090901
318300930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
318400930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
318500930423     C* A QUELLA DI RICHIESTA CONSEGNA,
318600930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
318700931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
318800931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
318900931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
319000931206     C                   END
319100930423     C                   END
319200930325     C*
319300930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
319400930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
319500930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
319600090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
319700090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
319800930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
319900931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
320000930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
320100930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
320200930423     C* A QUELLA DI RICHIESTA CONSEGNA,
320300930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
320400931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
320500931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
320600931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
320700931206     C                   END
320800930423     C                   END
320900140822     C*
321000930324     C                   ENDSR
321100150113     C********************************************************************
321200150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
321300150113     C********************************************************************
321400150113     c     Cal_DEE       BEGSR
321500150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
321600150508     c                   clear                   wtempi00C         1
321700150219     c
321800150219     c* 1) Se presente l'arrivo della trazione lo uso
321900150219    1c                   select
322000150219     c                   when      wmancatraz=' '  and d98dti>0
322100150113     c                   Z-ADD     WPODTI        WR1POI
322200150219     c
322300150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
322400150219    1c                   when      IDdata>0
322500150325     c                   exsr      CalcoloIMP
322600150528     c* stato IMP/disguido solo se >  alla data di oggi
322700150528     c                   if        wr1poi > wpoattdata00
322800150427     c                   eval      d98spcdee='IMP'
322900150427     c                   endif
323000150219     c
323100150219     c                   other
323200150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
323300150225     c* devo far finta che sia arrivato.
323400150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
323500150508     c*  che tiene conto della data partenza PRIMO  collo
323600150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
323700150508     c*
323800150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
323900150508     c                   if        wr1poi >=wpoattdata00
324000150508     c                              and WPOPTC_DEE > WPOPTC
324100150508     c                   eval      d98spcdee='IMP'
324200150508     c                   endif
324300150219     c                   endsl
324400150113     c
324500150219     c* se il calcolo è < di oggi --> imposto oggi
324600150113     c                   if        wr1poi < wpoattdata00
324700150113     c                   eval      wr1poi= wpoattdata00
324800150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
324900150508     c                   eval      wTempi00C='S'
325000150113     c                   endif
325100150217     c
325200150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
325300150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
325400150427    1c                   if        wr1poi>wpoattdata00  and
325500150330     c                             (deeindist<>'S' or d98cons<>'S'
325600150330     c                             or d98ddc>wdaodi)
325700150427     c
325800150427     c* Se stato IMP lo mantengo per avere messaggio diverso
325900150427     c                   if        d98spcdee<>'IMP'
326000150304     c                   eval      d98spcdee='PRV'
326100150427     c                   endif
326200150427     c
326300150326   x1c                   else
326400150330     c                   exsr      CAL_OLP
326500150217     c
326600150528     c* Se passato l'orario oppure data distinta < data di oggi
326700150508     c* 1) spedizione in distinta          --> in consegna
326800150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
326900150528     c                             d98ddc<wdaodi)
327000150508    3c                   IF        deeindist='S' and d98cons='S'
327100150508     c                   eval      d98spcdee= 'DDC'
327200150508
327300150508   x3c                   else
327400150507     c
327500150507    4c                   if        wr1poi<=wpoattdata00
327600150507     c                   add       24            wr1poi
327700150507    4c                   endif
327800150507     c
327900150507     c                   eval      d98spcdee='OLP'
328000150507     c* memorizzo orario limite picking
328100150507     c                   eval      d98picklim=%editc(picklim:'X')
328200150507    3c                   endif
328300150507    2c                   endif
328400150507     c
328500150507    4c**                 if        §vpoolpnu>0  and §VPOOLPMN>0
328600150507    5c**                 do        §vpoolpnu
328700150507     c**                 add       §VPOOLPMN     picklim
328800150507     c**                 eval      wora=picklim
328900150507     c**                 eval      wdiff=70
329000150330     c
329100150507     c**                 dow       wdiff>=0
329200150507     c**                 eval      wdiff=wmn-60
329300150507     c**                 if        wdiff>=0
329400150507     c**                 eval      wmn=wmn-60
329500150507     c**                 eval      who=who+1
329600150507     c**                 endif
329700150507     c**                 enddo
329800150507     c**                 eval      picklim=wora
329900150507    6c**                 if        picklim>whhodi
330000150507     c**                 leave
330100150507    6c**                 endif
330200150507    5c**                 enddo
330300150507    4c**                 endif
330400150507    3c**                 endif
330500150326
330600150427     c*    2) non più in distinta o passati i tentativi di massimo avanzamento automatico:
330700150326     c*                           imposto stato OLP prevista cons NON modificabile
330800150507    3c**                 if        whhodi>picklim
330900150507    1c**                 if        wr1poi<=wpoattdata00
331000150507     c**                 add       24            wr1poi
331100150507     c**                 endif
331200150330     c
331300150507     c**                 eval      d98spcdee='OLP'
331400150507    3c**                 endif
331500150507    2c**                 endif
331600150507    c
331700150326     c* memorizzo orario limite picking
331800150507     c**                 eval      d98picklim=%editc(picklim:'X')
331900150326    1c                   endif
332000150217     c
332100150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
332200150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
332300150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
332400150113     C                   Z-ADD     Wr1poi        W10POI            9 0
332500150113     C                   EXSR      R10FES
332600150113     C                   Z-ADD     W10POO        wr1poi
332700150113     C
332800150508     c* Se devo sommare i tempi entro soglia --> lo faccio
332900150508     c                   if        wtempi00C='S'
333000150508     c                   if        w990CN=10
333100150508     c                   z-add     12            wr1noi
333200150508     c                   else
333300150508     c                   z-add     W990CN        wr1noi
333400150508     c                   endif
333500150508     c
333600150508     c                   else
333700150113     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
333800150508     c                   endif
333900150508     c
334000150605     c                   eval      wdee='S'
334100150113     C                   EXSR      RO1CDP
334200150605     c                   eval      wdee=' '
334300150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
334400150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
334500150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
334600150113     C* A QUELLA DI RICHIESTA CONSEGNA,
334700150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
334800150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
334900150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
335000150331     c*  2) IL        -> è il giorno indicato
335100150331     C     D98DCR        IFNE      *ZEROS
335200150331     C     D98tcr        andne     'P'
335300150331     C***  WPOdpcee      IFLE      WPOCR2
335400150331     C***  WPOCR2        SUB       12            WPOdpcee
335500150330     c
335600150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
335700150330     c*   dare la prima data utile di consegna
335800150330     C     WPOdpcee      IFLE      WPOCRF_dee
335900150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
336000150330     C                   z-add     WPOCRF_dee    WPOdpcee
336100150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
336200150409     c                   if        wpodpcee>wpoattdata00
336300150409     c                   eval      d98spcdee='PRV'
336400150409     c                   endif
336500150113     C                   END
336600150331     c
336700150220     c
336800150113     C                   END
336900150220     c
337000150113     c                   ENDSR
337100150330     C********************************************************************
337200150330     C*   Calcolo orario da tabella OLP
337300150330     C********************************************************************
337400150330     c     Cal_OLP       BEGSR
337500150330     c                   eval      picklim=0900
337600150330     c* Dopo l'ora limite picking, passo al giorno dopo
337700150330     C                   MOVEL(P)  'OLP'         TBECOD
337800150330     C                   MOVEL(P)  d98lna        TBEKE1
337900150330     C                   MOVEL(P)  wdaodi        TBEKE2
338000150330     C     KTBE          CHAIN     TNTBE01L
338100150330     C                   IF        not %found(TNTBE01L)
338200150330     c                   clear                   tbeke2
338300150330     C     KTBE          CHAIN     TNTBE01L
338400150330     c                   endif
338500150330     C                   IF        not %found(TNTBE01L)
338600150330     C                   MOVEL(P)  '000'         TBEKE1
338700150330     C     KTBE          CHAIN     TNTBE01L
338800150330     c                   endif
338900150330     c
339000150330     C                   IF        %found(TNTBE01L) and
339100150330     c                             %subst(tbeuni:1:4)>*zeros
339200150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
339300150330     c                   endif
339400150330     c                   ENDSR
339500150325     C********************************************************************
339600150325     C*   Riclcolo data se colli in IMP/disguido
339700150325     C********************************************************************
339800150325     c     CalcoloIMP    BEGSR
339900150325     C                   Z-ADD     IDdata        WDAT
340000150325     C                   EXSR      DATORE
340100150325     C                   Z-ADD     WORE          WR4POI
340200150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
340300150325    2c                   if        IDfgs<>D98lnp
340400150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
340500150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
340600150325     c* calcolo il terminal di partenza
340700150325     c                   clear                   fnlv55ds
340800150325     c                   eval      d55tla='L'
340900150325     c                   eval      d55tpt='P'
341000150325     c                   eval      d55lin=IDfgs
341100150325     c                   eval      d55drf=IDdata
341200150325     c                   call      'FNLV55R'
341300150325     c                   parm                    fnlv55ds
341400150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
341500150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
341600150325     c                   EXSR      RTTRAZ
341700150325    3c                   if        d98err<>'1'
341800150325     C                   Z-ADD     CASTcn        wr4noi
341900150325     c                   else
342000150325     C                   Z-ADD     d98ttc        wr4noi
342100150325    3C                   endif
342200150325
342300150325   x2c                   else
342400150325     C                   Z-ADD     d98ttc        wr4noi
342500150325    2C                   endif
342600150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
342700150325     c*  perchè partirà un giorno dopo
342800150325    3c                   if        IDnpg='P'
342900150325     c                   add       24            WR4noi
343000150325    3c                   endif
343100150325     c
343200150325     c                   exsr      ro4cdp
343300150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
343400150325     c                   ENDSR
343500930604     C********************************************************************
343600930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
343700930604     C********************************************************************
343800930604     C     TPODAT        BEGSR
343900960229     C*
344000960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
344100960229     C*
344200960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
344300960229     C     WPOPTI        IFEQ      *ZEROS
344400960229     C                   Z-ADD     *ZEROS        D98DTI
344500960229     C                   Z-ADD     *ZEROS        D98HTI
344600960229     C                   ELSE
344700960229     C                   Z-ADD     WPOPTI        WORE
344800960229     C                   EXSR      OREDAT
344900960229     C                   Z-ADD     WDAT          D98DTI
345000960229     C     WRES          MULT      100           D98HTI
345100960229     C                   END
345200960229     C*
345300960229     C                   ELSE                                                   <-- 1 -->
345400930604     C*
345500931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
345600140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
345700140825     c*******            exsr      Tras_DEI
345800140825     c
345900930604     C*
346000150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
346100140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
346200140820     c* è data da :
346300140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
346400140820     c*  Tenere  conto, ovvero slittare in avanti, per :
346500140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
346600140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
346700140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
346800140911     c*
346900140820     c*  Da azzerare nei seguenti casi:
347000150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
347100150324     c* 4) bolla export    7) Cod manca consegna
347200140911     c* 8) bolla con DCR DOPOil o PRIMAdel
347300150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
347400150119     c*10) se completamente non arrivata e collo in IMP
347500150119     c
347600140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
347700150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
347800140925     C*  - FD   - export -
347900150217     c                   movel     'S'           DEEinDIST         1
348000150113     c                   eval      wcaldpcee='S'
348100150304     c                   eval      D98SPCDEE='PRV'
348200150324     c                   clear                   avvcmc            1
348300150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
348400150323     c                   if        d98cmc<>*blanks
348500150324     C                   Z-ADD     1             II
348600150326     C     d98cmc        LOOKUP    C2A(II)                                99
348700150324     C                   MOVEL     D2A(II)       usa2A
348800150324    4C     §2AFTC        IFEQ      'A'
348900150326     c                   eval      avvcmc='A'
349000150323     c                   endif
349100150324    4C     §2AFTC        IFEQ      'G'
349200150326     c                   eval      avvcmc='G'
349300150324     c                   endif
349400150324     c                   endif
349500140820     c
349600140911     c                   z-add     d98lna        EX
349700150113     c*  la azzero nei casi previsti                 0
349800150601    1c                   select
349900150119     c* 1) Se la bolla e consegnata
350000140925     c                   when      d98dcm>0
350100150113     c                   clear                   wcaldpcee
350200150216     c                   eval      d98spcdee='CON'
350300150217     c                   movel     ' '           DEEinDIST         1
350400150323     c* 2) Se pratica di danno
350500150603     c                   when      flgdan='S'
350600150323     c                   clear                   wcaldpcee
350700150323     c                   eval      d98spcdee='CA '
350800150323     c                   eval      deeindist='S'
350900150324     c* 3) Se bloccata/rimpallo
351000150114     c                   when      wd98fbc='B'
351100150113     c                   clear                   wcaldpcee
351200150216     c                   eval      d98spcdee='BLO'
351300150217     c                   movel     ' '           DEEinDIST         1
351400150324     c* 4) Se bolla export
351500150324     c                   when      fl1(EX)='E'
351600150324     c                   clear                   wcaldpcee
351700150324     c                   movel     ' '           DEEinDIST         1
351800150324     c                   eval      d98spcdee='EXP'
351900150324     c* 5) Se giacente  o in attesa di apertura giacenza
352000150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
352100150324     c                   clear                   wcaldpcee
352200150324     c                   eval      d98spcdee='GIA'
352300150324     c                   eval      deeindist='S'
352400150119     c* 7) Se codice manca consegna
352500150326     c                   when      d98cmc<>*blanks and avvcmc='A'
352600150324     c                   clear                   wpodpcee
352700150324     c                   clear                   wcaldpcee
352800150324     c                   movel     ' '           DEEinDIST         1
352900150324     c                   eval      d98spcdee='ATT'
353000150410     c* 7) Bolla in sospensione consegna
353100150410     c                   when      §B4LBGE='S'
353200150410     c                   clear                   wpodpcee
353300150410     c                   clear                   wcaldpcee
353400150410     c                   movel     ' '           DEEinDIST         1
353500150410     c                   eval      d98spcdee='SOS'
353600150601   x1c                   other
353700150325     c*
353800150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
353900150331     c                   if        d98ffd='S'
354000150601    2c                   select
354100150403     c* FD escluso dai contatti
354200150601    2c                   when      §B4LTFD= 'N'
354300150403     c                   eval      d98spcdee='FD3'
354400150403     c                   clear                   wcaldpcee
354500150403     c                   movel     ' '           DEEinDIST         1
354600150403
354700150403     c* FD non contattato ( o da ricontattare)
354800150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
354900150401     c                             olv82sts<>'1'
355000150323     c                   clear                   wcaldpcee
355100150323     c                   movel     ' '           DEEinDIST         1
355200150323     c                   eval      d98spcdee='FFD'
355300150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
355400150424     c                   eval      wpodpcee=wpoffi
355500150403
355600150403     c* FD contattato e     IN distinta
355700150601    2c                   when      d98cons='S'
355800150401     c                   eval      d98spcdee='FD2'
355900150401     c                   clear                   wcaldpcee
356000150401     c                   movel     ' '           DEEinDIST         1
356100150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
356200150424     c                   eval      wpodpcee=wpoffi
356300150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
356400150427     c*  ritirare la merce
356500150427     C                   MOVEL     'C'           WTC                            *PARTENZA
356600150427     C                   MOVEL     'C'           WIC
356700150427     C                   Z-ADD     D98ddc        WDAT
356800150427     C                   EXSR      DATORE
356900150427     c                   z-add     wore          wr4poi
357000150427     c                   z-add     24            wr4noi
357100150427     C                   EXSR      RO4CDP
357200150427     c                   z-add     wr4poo        wore
357300150427     c                   exsr      OREDAT
357400150427     c                   movel     wdat          D98DTCTD
357500150601   x2c                   other
357600150403     c* FD contattato e NON in distinta
357700150331     c                   eval      d98spcdee='FD1'
357800150408     c                   eval      wpodpcee=wpoffi
357900150331     c                   clear                   wcaldpcee
358000150331     c                   movel     ' '           DEEinDIST         1
358100150601    2c                   endsl
358200150331     c
358300150331     c                   endif
358400150403     c
358500150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
358600150601    2c                   if        (d98tc1='A' or d98tc2='A' or
358700150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
358800150601     c                             and d98ffd= ' '
358900150325     c                   clear                   wcaldpcee
359000150325     c                   movel     ' '           DEEinDIST         1
359100150601    3c                   if        §B4LBAS= 'N'
359200150427     c                   eval      d98spcdee='AP3'
359300150403     c                   else
359400150403     c                   eval      d98spcdee='APP'
359500150601    3c                   endif
359600150601    2c                   endif
359700150325     c*   se la bolla ha il C/Assegno verifico se da contattare
359800150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
359900150325     c                   clear                   wcaldpcee
360000150325     c                   movel     ' '           DEEinDIST         1
360100150601    3c                   if        §B4LBAS= 'N'
360200150427     c                   eval      d98spcdee='CA3'
360300150403     c                   else
360400150403     c                   eval      d98spcdee='CAS'
360500150601    3c                   endif
360600150601    2c                   endif
360700150325     c* Per questi 3 casi devo impostare la data contatto destinatario
360800150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
360900150325     c                             d98spcdee='CAS'
361000150601    2c                   select
361100150601    2c                   when      d98dti>0 and wmancatraz= ' '
361200150325     c                   movel     d98dti        d98dtctd
361300150403     c* se piene le ore già portate ad un giorno lavorativo
361400150403     c                   if        wpodti>0
361500150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
361600150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
361700150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
361800150403     C                   Z-ADD     WPODTI        W10POI            9 0
361900150403     C                   EXSR      R10FES
362000150403     c                   z-add     w10poo        wore
362100150403     C                   EXSR      OREDAT
362200150403     C                   movel     WDAT          D98dtctd
362300150403     c                   endif
362400150403     c
362500150601    2c                   when      d98dam>0
362600150421     C                   Z-ADD     D98dam        WDAT
362700150421     C                   EXSR      DATORE
362800150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
362900150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
363000150421     C                   Z-ADD     Wore          W10POI            9 0
363100150421     C                   EXSR      R10FES
363200150421     c                   z-add     w10poo        wore
363300150421     C                   EXSR      OREDAT
363400150421     C                   movel     WDAT          D98dtctd
363500150601   x2c                   other
363600150325     c* data teorica di arrivo tenendo conto dell'IMP
363700150325     c                   exsr      letbrv
363800150325     c                   if        idDATA>0
363900150325     c                   exsr      CalcoloIMP
364000150325     c                   z-add     wr1poi        WORE
364100150325     c                   else
364200150325     c                   z-add     wpoptc        WORE
364300150325     c                   endif
364400150325     C                   EXSR      OREDAT
364500150325     c                   movel     wdat          d98dtctd
364600150601    2c                   endsl
364700150325     c
364800150325     c                   endif
364900150325     c
365000150601    1c                   endsl
365100140820     c
365200150113     c* Se la bolla non è in distinta calcolo la data se previsto
365300150220     c                   if        wcaldpcee='S' and
365400150220     c                             (d98cons<>'S'  or (d98cons='S' and
365500150220     c                             evemic<>'SI'))
365600150219     c
365700150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
365800150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
365900150219     c                   if        d98nsp>0 and wtrasm<>'D' and
366000150219     c                             d98dam=0
366100150219     c                   exsr      letBRV
366200150219     c                   endif
366300150219     c
366400150217     c                   eval      d98spcdee='PRG'
366500150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
366600150320     c                             d98tc1='S' or d98tc2='S')
366700150320     c                   eval      d98spcdee='APP'
366800150320     c                   endif
366900150320     c
367000150113     c                   exsr      cal_DEE
367100150113     c                   endif
367200150113     c
367300140820     C     WPOdpcee      IFEQ      *ZEROS
367400931202     C                   Z-ADD     *ZEROS        D98DEE
367500931202     C                   Z-ADD     *ZEROS        D98OEE
367600930604     C                   ELSE
367700140820     C                   Z-ADD     WPOdpcee      WORE
367800141021     C******             SUB       12            WORE
367900930604     C                   EXSR      OREDAT
368000931202     C                   Z-ADD     WDAT          D98DEE
368100090903     c                   if        d98tcc<>10
368200090903     C                   ADD       12            WRES
368300090903     c                   else
368400090903     C                   ADD       10            WRES
368500090903     c                   endif
368600090903
368700931202     C     WRES          MULT      100           D98OEE
368800090903     c
368900090903     c                   if        d98oee=1000
369000090903     c                   eval      d98oee=1030
369100090903     c                   endif
369200140925     C                   END
369300150311     c
369400150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
369500150311     c                   select
369600150311     c                   when      d98spcdee='OLP'
369700140820     c
369800150220     c* Se bolla in distinta --> data prevista = data distinta
369900150311     c                   when      deeindist='S'  and
370000150113     c                             d98cons='S'
370100150220
370200150225     c*  non la propongo senza MIC e se presente disguido o IMP
370300150220     c                   if        d98dee>d98ddc and evemic<>'SI'
370400150220     c                             and IDdata>0
370500150220     c                   else
370600150220
370700140820     c                   eval      d98dee=d98ddc
370800140925     c                   clear                   d98oee
370900150320     c* se la data è > di oggi stato PRV se non è in IMP
371000150320     c                   if        d98dee>wdaodi
371100150320     c                   if        d98spcdee<>'IMP'
371200150311     c                   eval      d98spcdee='PRV'
371300150320     c                   endif
371400150320     c
371500150225     c                   else
371600150217     c* se c'è il MIC
371700150217     c                   if        evemic='SI'
371800150217     c                   eval      d98spcdee='DDC'
371900150330     C***                else
372000150304     c* solo se in PRV, metto lo stato
372100150330     c* questo caso lo asterisco perchè dovrebbe essere già stato gestito
372200150330     c*  prima in quanto devo tenere conto di due aggiunte all'orario limite tab.OLP
372300150330     c***                if        d98spcdee='PRV'
372400150330     c***                eval      d98spcdee='PRG'
372500150330     c***                endif
372600150225     c                   endif
372700150217     c                   endif
372800150220     c                   endif
372900150217     c
373000150311     c                   endsl
373100931202     C*
373200931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
373300931202     C     WPODCI        IFEQ      *ZEROS
373400931202     C                   Z-ADD     *ZEROS        D98DCI
373500931202     C                   Z-ADD     *ZEROS        D98HCI
373600931202     C                   ELSE
373700931202     C                   Z-ADD     WPODCI        WORE
373800931202     C                   EXSR      OREDAT
373900931202     C                   Z-ADD     WDAT          D98DCI
374000090902     c                   if        d98tci<>10
374100931202     C                   ADD       12            WRES
374200090902     c                   else
374300090902     C                   ADD       10            WRES
374400090902     c                   endif
374500090902
374600931202     C     WRES          MULT      100           D98HCI
374700090901     c
374800090903     c                   if        d98hci=1000
374900090901     c                   eval      d98hci=1030
375000090901     c                   endif
375100090901     c
375200931202     C                   END
375300931202     C*
375400931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
375500931202     C     WPODCE        IFEQ      *ZEROS
375600931202     C                   Z-ADD     *ZEROS        D98DCE
375700931202     C                   Z-ADD     *ZEROS        D98HCE
375800931202     C                   ELSE
375900931202     C                   Z-ADD     WPODCE        WORE
376000931202//// C**************       SUB  12        WORE
376100931202     C                   EXSR      OREDAT
376200931202     C                   Z-ADD     WDAT          D98DCE
376300090902     c                   if        d98tcc >10
376400931202     C                   ADD       12            WRES
376500090902     c                   else
376600090902     C                   ADD       10            WRES
376700090902     c                   endif
376800090902     c
376900931202     C     WRES          MULT      100           D98HCE
377000090902     c
377100090903     c                   if        d98hce=1000
377200090902     c                   eval      d98hce=1030
377300090902     c                   endif
377400090902     c
377500931202     C                   END
377600960229     C*
377700960229     C                   ENDIF                                                  <-- 1 ---
377800930604     C*
377900930604     C                   ENDSR
378000150218     C********************************************************************
378100150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
378200150218     C********************************************************************
378300150218     c     LetBRV        BEGSR
378400150219     c                   clear                   bollaOK           1
378500150219     c                   clear                   IDnpg
378600150219     c                   clear                   IDdata
378700150219     c                   clear                   IDora
378800150219     c                   clear                   IDfgs
378900150219     c
379000150218     C     Kart          SETLL     FNART01L
379100150218     C     Kart          READE     FNART01L
379200150219    1c                   dow       not %eof(fnart01l)
379300150219     c                   clear                   DISfgs
379400150219     c                   clear                   DISdata
379500150219     c                   clear                   DISnpg
379600150219     c                   clear                   DISora
379700150219     c                   clear                   IMPdata
379800150219     c                   clear                   IMPfgs
379900150508     c* per ogni collo verifico se in IMP o disguidato
380000150218     c* al primo che NON trovo, esco come --> OK
380100150218     c     kbrv          setll     fnbrv07l
380200150218     c     kbrv          reade     fnbrv07l
380300150219    2c                   dow       not %eof(fnbrv07l)
380400150219    3c                   if        brvatr='D'  or brvnpg=3
380500150218     c* prendo la data della spunta
380600150218     c                   clear                   fnlv53ds
380700150219     c                   eval      d53tla='L'
380800150218     C                   MOVEL     BRvNPG        D53NPG
380900150218     C                   Z-ADD     BRvNFV        D53NFV
381000150218     C                   MOVEL     BRvFGS        D53FGS
381100150218     C                   CALL      'FNLV53R'
381200150218     C                   PARM                    fnlv53ds
381300150218    4c                   if        d53err<>*blanks
381400150219    5c                   if        brvdcs>0
381500150218     c                   eval      d53dfv=brvdcs
381600150218     c                   else
381700150219    6c                   if        brvdfs>0
381800150218     c                   eval      d53dfv=brvdfs
381900150219    6c                   endif
382000150219    5c                   endif
382100150219    4c                   endif
382200150218     c
382300150219    4c                   select
382400150219     c* se si tratta di IMP --> memorizzo
382500150219    4c                   when      d53spg='P' and d53dfv>impdata
382600150218     c                   eval      IMPdata=d53dfv
382700150219     c                   eval      IMPfgs =brvfgs
382800150219     c* se si tratta di DISGUIDO
382900150218     c                   when      brvatr='D'
383000150219     c
383100150219    5c                   if        d53dfv=DISdata
383200150219     c* Verifico l'ora
383300150219    6c                   if        brvhcs>0 and brvhcs>DISora
383400150219     c                   eval      DISdata=d53dfv
383500150219     c                   eval      DISfgs =brvfgs
383600150219     c                   eval      DISnpg =brvnpg
383700150219     c                   eval      DISora =brvhcs
383800150219   x6c                   else
383900150219    7c                   if        brvhms>0 and brvhms>DISora
384000150219     c                   eval      DISdata=d53dfv
384100150219     c                   eval      DISfgs =brvfgs
384200150219     c                   eval      DISnpg =brvnpg
384300150219     c                   eval      DISora =brvhms
384400150219    7c                   endif
384500150219    6c                   endif
384600150219    5c                   endif
384700150219     c
384800150219    5c                   if        d53dfv>DISdata
384900150219     c                   eval      DISdata=d53dfv
385000150219     c                   eval      DISfgs =brvfgs
385100150219     c                   eval      DISnpg =brvnpg
385200150219     c                   eval      DISora =brvhcs
385300150219     c                   if        disora=0
385400150219     c                   eval      DISora =brvhms
385500150219     c                   endif
385600150219    5c                   endif
385700150219     c
385800150219    4c                   endsl
385900150219    3c                   endif
386000150219     c     kbrv          reade     fnbrv07l
386100150219    2c                   enddo
386200150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
386300150219     c*  esco dal ciclo: la bolla è OK
386400150219    2c                   if        impdata=0 and disdata=0
386500150219     c                   eval      bollaOK='S'
386600150219     c                   clear                   IDdata
386700150219     c                   clear                   IDfgs
386800150219     c                   clear                   IDora
386900150219     c                   clear                   IDnpg
387000150219   x2c                   else
387100150219     c* memorizzo la data più alta
387200150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
387300150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
387400150219     c                   eval      IDdata=IMPdata
387500150219     c                   eval      IDfgs=IMPfgs
387600150219     c                   eval      IDora=0
387700150219     c                   eval      IDnpg='P'
387800150219    3c                   endif
387900150219    3c                   if        (DISdata>0 and DISdata=IDdata
388000150219     c                             and IDnpg<>'P' and DISora>IDora) or
388100150219     c                             (DISdata>0 and DISdata>IDdata)
388200150219     c                   eval      IDdata=DISdata
388300150219     c                   eval      IDfgs=DISfgs
388400150219     c                   eval      IDora=DISora
388500150219     c                   eval      IDnpg=%char(DISnpg)
388600150219    3c                   endif
388700150219    2c                   endif
388800150218     c
388900150218     C     Kart          READE     FNART01L
389000150219    1c                   enddo
389100150219     c
389200150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
389300150508     c**                 if        d98duc> 0 and d98duc>IDdata
389400150508     c**                 eval      bollaOK='S'
389500150508     c**                 clear                   IDdata
389600150508     c**                 clear                   IDfgs
389700150508     c**                 clear                   IDora
389800150508     c**                 clear                   IDnpg
389900150508     c**                 endif
390000150219     c
390100150218     c
390200150218     C                   ENDSR
390300140825     C********************************************************************
390400140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
390500140825     C********************************************************************
390600140825     c     TRAS_DEI      BEGSR
390700140825     C     WPODEI        IFEQ      *ZEROS
390800140825     C                   Z-ADD     *ZEROS        D98DEI
390900140825     C                   Z-ADD     *ZEROS        D98OEI
391000140825     C                   ELSE
391100140825     C                   Z-ADD     WPODEI        WORE
391200140825     C                   SUB       12            WORE
391300140825     C                   EXSR      OREDAT
391400140825     c
391500140825     C                   Z-ADD     WDAT          D98DEI
391600140825     c                   if        d98tci<>10
391700140825     C                   ADD       12            WRES
391800140825     c                   else
391900140825     C                   ADD       10            WRES
392000140825     c                   endif
392100140825     c
392200140825     C     WRES          MULT      100           D98OEI
392300140825     c
392400140825     c                   if        d98oei=1000
392500140825     c                   eval      d98oei=1030
392600140825     c                   endif
392700140825     c
392800140825     C                   END
392900140825     c                   ENDSR
393000930604     C********************************************************************
393100930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
393200930604     C********************************************************************
393300930604     C     CHKFOR        BEGSR
393400930604     C*
393500930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
393600930604     C* E QUINDI FORZO LA CONFORMITA' A SI
393700941227     C     D98ERR        IFEQ      '0'
393800930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
393900930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
394000930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
394100930604     C                   MOVEL     '1'           D98ERR
394200930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
394300930604     C                   END
394400930604     C                   END
394500990212     C*
394600990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
394700990212     C* E QUINDI FORZO LA CONFORMITA' A SI
394800990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
394900990212     C     D98ERR        IFEQ      '0'
395000990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
395100141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
395200990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
395300990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
395400990212     C                   MOVEL     '1'           D98ERR
395500990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
395600990212     C                   END
395700990212     C                   END
395800990308     C*
395900990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
396000990308     C* E QUINDI FORZO LA CONFORMITA' A SI
396100100614     C**   D98ERR        IFEQ      '0'
396200100614     C**   WFBR          IFEQ      'S'                                          *RESO
396300100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
396400100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
396500100614     C**                 MOVEL     '1'           D98ERR
396600100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
396700100614     C**                 END
396800100614     C**                 END
396900930604     C*
397000930604     C                   ENDSR
397100990212     C********************************************************************
397200990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
397300990212     C********************************************************************
397400990212     C     CHKDAN        BEGSR
397500990212     C*
397600990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
397700150603     c                   movel     'N'           opendan           1
397800040809      *
397900040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
398000040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
398100040809      * legate alla spedizione che passo
398200040809     c                   clear                   fidn12ds
398300040809     c                   eval      i12aas = D98AAS
398400040809     c                   eval      i12lnp = D98LNP
398500040809     c                   eval      i12nrs = D98NRS
398600040809     c                   eval      i12nsp = D98NSP
398700040809     c                   eval      i12fel = 'E'
398800040809     c                   eval      i12fan = 'E'
398900040809     c*
399000040809     c                   call      'FIDN12R'
399100040809     c                   parm                    fidn12ds
399200040809     c*
399300040809     c* se non ci sono errori
399400040809     c                   if        o12err = ' '
399500040809     c* se numero di CA trovate maggiore di zero
399600040809     c                   if        o12nca > 0
399700040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
399800150603     c
399900150603     c***                z-add     1             aa                2 0
400000150603     c***                dow       aa <= o12nca
400100150603     C* APERTA
400200150603     C***  skdch(aa)     IFEQ      0
400300150603     c***                movel     'S'           opendan
400400150603     c***                endif
400500150603     c*
400600150603     c***                add       1             aa
400700150603     c***                enddo
400800150603     c
400900040809     c                   endif
401000040809     c                   endif
401100990212     C*
401200990212     C                   ENDSR
401300941228     C********************************************************************
401400941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
401500941228     C********************************************************************
401600941228     C     CALORP        BEGSR
401700941228     C*
401800941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
401900941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
402000941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
402100960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
402200950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
402300950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
402400950110     C                   ELSE
402500950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
402600950110     C                   ENDIF
402700941228     C                   EXSR      RO2COP
402800941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
402900941228     C* SPED. IL GIORNO DOPO
403000941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
403100941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
403200941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
403300941228     C*
403400941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
403500941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
403600941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
403700941228     C                   Z-ADD     WPODSP        WR2POI
403800950110     C                   Z-ADD     WPOPPC        WR2POF
403900941228     C                   EXSR      RO2COP
404000941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
404100941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
404200941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
404300941228     C*
404400941228     C                   ENDSR
404500930414     C********************************************************************
404600930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
404700930414     C********************************************************************
404800930414     C     CALORT        BEGSR
404900930414     C*
405000930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
405100931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
405200930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
405300931126     C                   Z-ADD     *ZEROS        W98HF
405400950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
405500950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
405600950110     C                   ELSE
405700950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
405800950110     C                   ENDIF
405900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
406000931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
406100930608     C                   ELSE
406200931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
406300930608     C                   END
406400931126     C                   EXSR      RO2COV
406500931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
406600930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
406700930414     C*
406800930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
406900930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
407000930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
407100931126     C                   Z-ADD     *ZEROS        W98HF
407200950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
407300931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
407400931126     C                   EXSR      RO2COV
407500931126     C                   Z-ADD     WR2NOV        W98RTC
407600930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
407700930414     C*
407800930414     C                   ENDSR
407900930414     C********************************************************************
408000930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
408100930414     C********************************************************************
408200930414     C     CALORC        BEGSR
408300930511     C*
408400930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
408500930511     C* I TEMPI DI RITARDO AD OGGI
408600930511     C     WPODCM        IFEQ      *ZEROS
408700960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
408800930511     C                   END
408900940117     C*
409000940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
409100940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
409200940117     C* ALLA DATA DI FINE CONSEGNABILITA'
409300940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
409400940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
409500940118££££ C     WPODFC        IFNE      *ZEROS
409600940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
409700940118££££ C                   END
409800930414     C*
409900930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
410000930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
410100930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
410200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
410300931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
410400930608     C                   ELSE
410500931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
410600930608     C                   END
410700931126     C                   Z-ADD     WPODCM        WR5POF            9 0
410800931126     C                   EXSR      RO5COV
410900931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
411000930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
411100930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
411200060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
411300060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
411400930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
411500930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
411600930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
411700930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
411800930414     C*
411900930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
412000930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
412100930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
412200931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
412300931126     C                   Z-ADD     WPODCM        WR5POF            9 0
412400931126     C                   EXSR      RO5COV
412500931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
412600930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
412700930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
412800060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
412900060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
413000930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
413100930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
413200930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
413300930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
413400930414     C*
413500930414     C                   ENDSR
413600930414     C********************************************************************
413700930414     C*   CALORR - CALCOLA ORE REALI DI RESA
413800930414     C********************************************************************
413900930414     C     CALORR        BEGSR
414000930414     C*
414100950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
414200950110     C                   ADD       W98RTI        W98RRI
414300941228     C                   ADD       W98RCI        W98RRI
414400950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
414500950110     C                   ADD       W98RTC        W98RRC
414600941228     C                   ADD       W98RCC        W98RRC
414700930414     C*
414800930414     C                   ENDSR
414900930420     C********************************************************************
415000930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
415100930420     C********************************************************************
415200930420     C     CALDIF        BEGSR
415300930420     C*
415400941228     C* PARTENZA
415500941228     C     W98RPI        SUB       D98TPI        W98FPI
415600941228     C     W98RPC        SUB       D98TPC        W98FPC
415700941228     C*
415800930420     C* TRAZIONE
415900930423     C     W98RTI        SUB       D98TTI        W98FTI
416000930423     C     W98RTC        SUB       D98TTC        W98FTC
416100930420     C*
416200930420     C* CONSEGNA
416300090827     C***  W98RCI        SUB       D98TCI        W98FCI
416400090827     C     W98RCI        SUB       w99TCI        W98FCI
416500090827     C***  W98RCC        SUB       D98TCC        W98FCC
416600090827     C     W98RCC        SUB       w99TCC        W98FCC
416700930420     C*
416800930420     C* RESA
416900090921     C***  W98RRI        SUB       D98TRI        W98FRI
417000090921     C     W98RRI        SUB       w99TRI        W98FRI
417100090921     c
417200090921     C***  W98RRC        SUB       D98TRC        W98FRC
417300090921     C     W98RRC        SUB       w99TRC        W98FRC
417400930420     C*
417500930420     C                   ENDSR
417600930420     C********************************************************************
417700930420     C*   CALCNF - CALCOLA LE CONFORMITA'
417800930420     C********************************************************************
417900930420     C     CALCNF        BEGSR
418000941228     C*
418100941228     C*-------------------------------------------------------------------
418200941228     C* CONFORMITA' PARTENZA  INTERNA
418300941228     C*-------------------------------------------------------------------
418400941228     C                   Z-ADD     W98FPI        W98NPI
418500941228     C     W98FPI        IFLE      *ZEROS
418600941228     C                   MOVEL     'S'           D98CPI
418700941228     C                   ELSE
418800941228     C                   MOVEL     'N'           D98CPI
418900941228     C                   END
419000941228     C*
419100941228     C*-------------------------------------------------------------------
419200941228     C* CONFORMITA' PARTENZA  CLIENTE
419300941228     C*-------------------------------------------------------------------
419400941228     C                   Z-ADD     W98FPC        W98NPC
419500941228     C     W98FPC        IFLE      *ZEROS
419600941228     C                   MOVEL     'S'           D98CPC
419700941228     C                   ELSE
419800941228     C                   MOVEL     'N'           D98CPC
419900941228     C                   END
420000930420     C*
420100930420     C*-------------------------------------------------------------------
420200930420     C* CONFORMITA' TRAZIONE  INTERNA
420300930420     C*-------------------------------------------------------------------
420400930510     C                   Z-ADD     W98FTI        W98NTI
420500930423     C     W98FTI        IFLE      *ZEROS
420600930420     C                   MOVEL     'S'           D98CTI
420700930420     C                   ELSE
420800930420     C                   MOVEL     'N'           D98CTI
420900930420     C                   END
421000930420     C*
421100930420     C*-------------------------------------------------------------------
421200930420     C* CONFORMITA' TRAZIONE  CLIENTE
421300930420     C*-------------------------------------------------------------------
421400930510     C                   Z-ADD     W98FTC        W98NTC
421500930423     C     W98FTC        IFLE      *ZEROS
421600930420     C                   MOVEL     'S'           D98CTC
421700930420     C                   ELSE
421800930420     C                   MOVEL     'N'           D98CTC
421900930420     C                   END
422000930420     C*
422100930420     C*-------------------------------------------------------------------
422200930420     C* CONFORMITA' CONSEGNA  INTERNA
422300930420     C*-------------------------------------------------------------------
422400150226     c                   if        wmancatraz='S'
422500150226     c                   clear                   d98dti
422600150226     c                   clear                   d98hti
422700150226     c                   endif
422800150226     c
422900930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
423000930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
423100930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
423200050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
423300050914     c                   if        consideraora='S'
423400050914     c     w98fci        sub       24            w98nci
423500050914     c                   add       24            d98hrc
423600050914     c                   else
423700930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
423800050914     c                   endif
423900080116     c
424000110923     c                   select
424100080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
424200080116     c*  --> trasformo il ritardo in 0
424300090827     c                   when      d98cca='2'
424400080116     c                   z-add     0             w98nci
424500090827
424600080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
424700080116     c*  e sto leggendo la bolla in arrivo
424800080116     c*  --> trasformo il ritardo = 0
424900090827     c                   when      d98cca='1' and d98tbo='A'
425000080116     c                   z-add     0             w98nci
425100090902     c
425200090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
425300090902     c                   when      wmancaTRAZ='S'
425400090902     c                   z-add     0             w98nci
425500090902     c                   clear                   d98dti
425600090902     c                   clear                   d98hti
425700110923     c
425800110923     c* Ignoro ritardo se pefect order
425900130221     c                   when      d98xco >*zeros
426000110923     c                   z-add     0             w98nci
426100090827     c
426200090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
426300091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
426400091014     c*  controllo ora soglia  consegna da tabella OSC)
426500091009     c                   when      d98tci=10   and
426600091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
426700091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
426800091014     c*
426900091014     c                             d98hci=1030
427000091014     C                   MOVEL     'OSC  '       WR3TAB
427100091014     C                   eval      WR3HHI=d98hmc
427200091014     C                   EXSR      RO3ORE
427300131218
427400091014     c                   if        wr3matpom='M' and d98hmc>1030
427500131218     c                   exsr      ctrLAVH1030
427600131218
427700131218     c                   if        wlav1030<>'S'
427800091014     c                   add       12            w98nci
427900091014     c                   endif
428000131218     c                   endif
428100090928     c
428200090827     c                   endsl
428300050914     c
428400930524     C                   ELSE                                                   <-- 1 -->
428500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
428600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
428700930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
428800931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
428900930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
429000930608     C                   ELSE
429100930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
429200930608     C                   END
429300930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
429400930525     C                   EXSR      RO2COV
429500930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
429600930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
429700930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
429800930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
429900931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
430000930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
430100930608     C                   ELSE
430200930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
430300930608     C                   END
430400931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
430500930525     C                   EXSR      RO2COV
430600930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
430700930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
430800930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
430900930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
431000930525     C* E' IL RITARDO
431100930610     C     WNOCRI        SUB       W98RCI        W98NCI
431200050901     c
431300050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
431400050901     c*  --> trasformo il ritardo in anticipo
431500050901     c                   if        d98cca='2'
431600080116     c***                z-sub     w98nci        w98nci
431700080116     c                   z-add     0             w98nci
431800050901     c                   endif
431900111018     c* Ignoro ritardo se pefect order
432000130221     c                   if        d98xco >*zeros
432100111018     c                   z-add     0             w98nci
432200111018     c                   endif
432300061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
432400061121     c*  e sto leggendo la bolla in arrivo
432500061121     c*  --> trasformo il ritardo in anticipo
432600061121     c                   if        d98cca='1' and d98tbo='A'
432700080116     c***                z-sub     w98nci        w98nci
432800080116     c                   z-add     0             w98nci
432900061121     c                   endif
433000070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
433100070827     c*  mittente, --> consegnato in anticipo
433200070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
433300140403     c                   if        Callar5=' '
433400070827     c                   exsr      LeggiAR5
433500140403     c                   endif
433600070827     c
433700070827     c                   if        dcrpart<>'S'
433800070824     c                   z-sub     w98nci        w98nci
433900070824     c                   endif
434000070827     c
434100070824     c                   endif
434200050907     c
434300050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
434400050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
434500050907     c*  lo trasformo in anticipo perchè non sono in ritardo
434600050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
434700050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
434800050916     c***                if        d98dfc>0
434900070828     c                   if        d98dcm=0  and w98nci>0
435000050907     c                   z-sub     w98nci        w98nci
435100050907     c                   sub       oreILoDOPO    w98nci
435200050907     c                   endif
435300050901     c
435400930525     C                   ELSE                                                   <-- 2 -->
435500070521     c* non do ritardo se la consegna reale cade nel giorno
435600070521     c*  della consegna richiesta se presente IL
435700100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
435800070521     c                             wpodcm<=wpocr1
435900070521     c                   clear                   w98nci
436000100224     c                   else                                                   ---2a -->
436100930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
436200090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
436300090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
436400930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
436500930525     C* E' IL RITARDO O L'ANTICIPO
436600930525     C     W98RCI        SUB       WNOCRF        W98NCI
436700070115     c
436800070115     c* non do ritardo se comunque la consegna reale cade nel giorno
436900070115     c*  della consegna richiesta se presente IL
437000070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
437100070521     c***                          wpodcm<=wpocr1  and w98nci>0
437200070521     c***                clear                   w98nci
437300070521     c***                endif
437400070115     c
437500930525     C                   ELSE                                                   <-- 3 -->
437600930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
437700930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
437800930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
437900930525     C                   END                                                    <-- 3 ---
438000100224     C                   END                                                    <-- 2a---
438100100224
438200100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
438300100224     c                   select
438400100224     c                   when      d98cca='2'
438500100224     c                   z-add     0             w98nci
438600100224
438700100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
438800100224     c*  e sto leggendo la bolla in arrivo
438900100224     c                   when      d98cca='1' and d98tbo='A'
439000100224     c                   z-add     0             w98nci
439100100224     c                   endsl
439200100224     c
439300100224     C                   END                                                    <-- 2 ---
439400070521     C                   END                                                    <-- 1 ---
439500070521     c
439600970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
439700970915     C*/         WAZNCI    IFEQ 'S'
439800970915     C*/         W98NCI    ANDLT*ZEROS
439900970915     C*/                   Z-ADD*ZEROS    W98NCI
440000970915     C*/                   ENDIF
440100970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
440200050922     c* lo faccio solo se non c'e' la data cons richiesta
440300970915     C     D98DTI        IFEQ      *ZEROS
440400050922     c     d98dcr        andeq     *zeros
440500970915     C                   Z-ADD     *ZEROS        W98NCI
440600970915     C                   ENDIF
440700930524     C*
440800930524     C* CONTROLLO CONFORMITA
440900930423     C     W98NCI        IFLE      *ZEROS
441000930524     C                   MOVEL     'S'           D98CCI
441100930420     C                   ELSE
441200930524     C                   MOVEL     'N'           D98CCI
441300930420     C                   END
441400930420     C*
441500930420     C*-------------------------------------------------------------------
441600930420     C* CONFORMITA' CONSEGNA  CLIENTE
441700930420     C*-------------------------------------------------------------------
441800930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
441900930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
442000930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
442100050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
442200050914     c                   if        consideraora='S'
442300050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
442400050914     c                   else
442500050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
442600050914     c                   endif
442700080116     c
442800080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
442900080116     c*  --> trasformo il ritardo =0
443000090827     c                   select
443100090827     c                   when      d98cca='2'
443200080116     c                   z-add     0             w98ncc
443300090827
443400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
443500080116     c*  e sto leggendo la bolla in arrivo
443600080116     c*  --> trasformo il ritardo =0
443700090827     c                   when      d98cca='1' and d98tbo='A'
443800080116     c                   z-add     0             w98ncc
443900090902     c
444000090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
444100090902     c                   when      wmancaTRAZ='S'
444200090902     c                   z-add     0             w98ncc
444300110923     c* Ignoro ritardo se Perfect Order
444400130221     c                   when      d98xco >*zeros
444500110923     c                   z-add     0             w98ncc
444600090902     c
444700090827     c
444800090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
444900090827     c* sono  in ritardo di 12 ore
445000091009     c                   when      d98tcc=10  and
445100091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
445200091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
445300091014     c*
445400091014     c                             d98hce=1030
445500091014     C                   MOVEL     'OSC  '       WR3TAB
445600091014     C                   eval      WR3HHI=d98hmc
445700091014     C                   EXSR      RO3ORE
445800091014     c                   if        wr3matpom='M' and d98hmc>1030
445900131218     c                   exsr      ctrLAVH1030
446000131218
446100131218     c                   if        wlav1030<>'S'
446200131218     c                   add       12            w98ncc
446300131218     c                   endif
446400131218     c****               add       12            w98ncc
446500091014     c                   endif
446600131218     c
446700091014     c***                z-add     12            w98ncc
446800090827     c                   endsl
446900050914     c
447000930524     C                   ELSE                                                   <-- 1 -->
447100930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
447200930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
447300930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
447400930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
447500930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
447600930525     C                   EXSR      RO2COV
447700930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
447800930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
447900930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
448000930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
448100930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
448200931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
448300930525     C                   EXSR      RO2COV
448400930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
448500930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
448600930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
448700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
448800930525     C* E' IL RITARDO
448900930525     C     WNOCRI        SUB       W98RCC        W98NCC
449000050901     c
449100050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
449200050901     c*  --> trasformo il ritardo in anticipo
449300050901     c                   if        d98cca='2'
449400080116     c***                z-sub     w98ncc        w98ncc
449500080116     c                   z-add     0             w98ncc
449600050901     c                   endif
449700111018     c* Ignoro ritardo se pefect order
449800130221     c                   if        d98xco >*zeros
449900111018     c                   z-add     0             w98ncc
450000111018     c                   endif
450100061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
450200061121     c*  e sto leggendo la bolla in arrivo
450300061121     c*  --> trasformo il ritardo in anticipo
450400061121     c                   if        d98cca='1' and d98tbo='A'
450500080116     c***                z-sub     w98ncc        w98ncc
450600080116     c                   z-add     0             w98ncc
450700061121     c                   endif
450800070824     c
450900070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
451000070827     c*  mittente, --> consegnato in anticipo
451100070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
451200070827     c* SBR per vedere se tassativa
451300070827     c                   if        Callar5=' '
451400070827     c                   EXSR      LeggiAR5
451500070827     c                   endif
451600070827     c                   if        dcrpart<>'S'
451700070824     c                   z-sub     w98ncc        w98ncc
451800070824     c                   endif
451900070824     c                   endif
452000050907     c
452100050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
452200050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
452300050907     c*  lo trasformo in anticipo perchè non sono in ritardo
452400050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
452500050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
452600050916     c***                if        d98dfc>0
452700070828     c                   if        d98dcm=0 and w98ncc>0
452800050907     c                   z-sub     w98ncc        w98ncc
452900050907     c                   sub       oreILoDOPO    w98ncc
453000050907     c                   endif
453100050901     c
453200930525     C                   ELSE                                                   <-- 2 -->
453300070521     c* Non do ritardo se la consegna reale cade nel giorno
453400070521     c*  della consegna richiesta se presente IL
453500100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
453600070521     c                             wpodcm<=wpocr2
453700070521     c                   clear                   w98ncc
453800100224     c                   else                                                   <--2a -->
453900070521     c
454000930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
454100090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
454200090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
454300930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
454400930525     C* E' IL RITARDO O L'ANTICIPO
454500930525     C     W98RCC        SUB       WNOCRF        W98NCC
454600070115     c
454700070115     c* non do ritardo se comunque la consegna reale cade nel giorno
454800070115     c*  della consegna richiesta se presente IL
454900070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
455000070521     c***                          wpodcm<=wpocr2 and w98ncc>0
455100070521     c***                clear                   w98ncc
455200070521     c***                endif
455300070115     c
455400930525     C                   ELSE                                                   <-- 3 -->
455500930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
455600930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
455700930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
455800930525     C                   END                                                    <-- 3 ---
455900100224     C                   END                                                    <-- 2a---
456000100224
456100100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
456200100224     c                   select
456300100224     c                   when      d98cca='2'
456400100224     c                   z-add     0             w98nci
456500100224
456600100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
456700100224     c*  e sto leggendo la bolla in arrivo
456800100224     c                   when      d98cca='1' and d98tbo='A'
456900100224     c                   z-add     0             w98nci
457000100224     c                   endsl
457100100224     c
457200100224     C                   END                                                    <-- 2 ---
457300070521     C                   END                                                    <-- 1 ---
457400930524     C*
457500930524     C* CONTROLLO CONFORMITA
457600930423     C     W98NCC        IFLE      *ZEROS
457700930524     C                   MOVEL     'S'           D98CCC
457800930420     C                   ELSE
457900930524     C                   MOVEL     'N'           D98CCC
458000930420     C                   END
458100930420     C*
458200930420     C*-------------------------------------------------------------------
458300930420     C* CONFORMITA' RESA INTERNA
458400930420     C*-------------------------------------------------------------------
458500930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
458600930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
458700930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
458800050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
458900050914     c                   if        consideraora='S'
459000050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
459100050914     c                   else
459200050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
459300050914     c                   endif
459400080116     c
459500080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
459600090827     c                   select
459700090827     c                   when      d98cca='2' and w98nri>0
459800080116     c                   z-add     0             w98nri
459900090827     c
460000080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
460100080116     c*  e sto leggendo la bolla in arrivo
460200090827     c                   when      d98cca='1' and d98tbo='A' and
460300080116     c                                            w98nri>0
460400080116     c                   z-add     0             w98nri
460500110923     c
460600110923     c* Ignoro ritardo se Perfect Order
460700130221     c                   when      d98xco >*zeros
460800110923     c                   z-add     0             w98nri
460900110923     c
461000090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
461100090827     c* sono  in ritardo di 12 ore
461200091009     c                   when      d98tci=10  and
461300091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
461400091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
461500091014     c*
461600091014     c                             d98hci=1030
461700091014     C                   MOVEL     'OSC  '       WR3TAB
461800091014     C                   eval      WR3HHI=d98hmc
461900091014     C                   EXSR      RO3ORE
462000091014     c                   if        wr3matpom='M' and d98hmc>1030
462100131218     c                   exsr      ctrLAVH1030
462200131218
462300131218     c                   if        wlav1030<>'S'
462400091014     c                   add       12            w98nri
462500091014     c                   endif
462600131218     c                   endif
462700091014     c
462800091014     c***                z-add     12            w98nri
462900090827     c                   endsl
463000090827
463100930524     C                   ELSE                                                   <-- 1 -->
463200930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
463300930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
463400930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
463500931206     C     WPODCM        ANDLE     WPOCR1
463600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
463700930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
463800930525     C                   Z-ADD     WPODCM        WR2POI            9 0
463900931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
464000930525     C                   EXSR      RO2COV
464100930525     C     WR2NOV        IFNE      *ZEROS
464200930525     C     WR2NOV        MULT      -1            W98NRI
464300930525     C                   END
464400930524     C                   ELSE                                                   <-- 2 -->
464500930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
464600930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
464700930524     C     WPODCM        IFLT      WPOCRI
464800930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
464900930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
465000930524     C                   ELSE
465100931206     C                   Z-ADD     WPOCR1        WPORFI
465200930524     C                   Z-ADD     WPODCM        WPORFF
465300930524     C                   END
465400930524     C* CALCOLO ORE DI DIFFERENZA
465500930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
465600930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
465700930524     C                   Z-ADD     WPORFI        WR2POI            9 0
465800930524     C                   Z-ADD     WPORFF        WR2POF            9 0
465900930524     C                   EXSR      RO2COV
466000930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
466100050902     c
466200050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
466300100224     c                   if        d98cca='2' and w98nri>0
466400100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
466500080116     c***                z-sub     w98nri        w98nri
466600080116     c                   z-add     0             w98nri
466700050902     c                   endif
466800111018     c* Ignoro ritardo se pefect order
466900130221     c                   if        d98xco >*zeros
467000111018     c                   z-add     0             w98nri
467100111018     c                   endif
467200061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
467300061121     c*  e sto leggendo la bolla in arrivo
467400100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
467500100224     c***                if        d98cca='1' and d98tbo='A' and
467600100224     c***                                         w98nri>0 and wpodcm<wpocri
467700080116     c***                z-sub     w98nri        w98nri
467800080116     c                   z-add     0             w98nri
467900061121     c                   endif
468000070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
468100070827     c*  mittente, --> consegnato in anticipo
468200070828     c                   if        d98tcr=' ' and d98nsp>0  and
468300070828     c                                            w98nri>0 and wpodcm<wpocri
468400070827     c                   if        CallAR5=' '
468500070827     c                   EXSR      LeggiAR5
468600070824     c                   endif
468700070827     c                   if        dcrpart<>'S'
468800070824     c                   z-sub     w98nri        w98nri
468900070824     c                   endif
469000070824     c                   endif
469100050907     c
469200050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
469300050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
469400050907     c*  lo trasformo in anticipo perchè non sono in ritardo
469500050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
469600050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
469700050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
469800050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
469900050907     c                   z-sub     w98nri        w98nri
470000050907     c                   sub       oreILoDOPO    w98nri
470100050907     c                   endif
470200050907     c
470300050907     C                   END                                                    <-- 2 ---
470400930524     C                   END                                                    <-- 1 ---
470500930420     C* CALCOLO LE ORE DI RESA
470600930420     C* CONTROLLO CONFORMITA
470700930423     C     W98NRI        IFLE      *ZEROS
470800930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
470900930420     C                   ELSE
471000930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
471100930420     C                   END
471200930420     C*
471300930420     C*-------------------------------------------------------------------
471400930420     C* CONFORMITA' RESA CLIENTE
471500930420     C*-------------------------------------------------------------------
471600930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
471700930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
471800930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
471900050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
472000050914     c                   if        consideraora='S'
472100050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
472200050914     c                   else
472300050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
472400050914     c                   endif
472500090827     c
472600080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
472700090827     c                   select
472800090827     c                   when      d98cca='2' and w98nrc>0
472900080116     c                   z-add     0             w98nrc
473000090827
473100080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
473200080116     c*  e sto leggendo la bolla in arrivo
473300090827     c                   when      d98cca='1' and d98tbo='A' and
473400080116     c                                            w98nrc>0
473500080116     c                   z-add     0             w98nrc
473600110923     c
473700110923     c* Ignoro ritardo se Perfect Order
473800130221     c                   when      d98xco >*zeros
473900110923     c                   z-add     0             w98nrc
474000090827     c
474100090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
474200090827     c* sono  in ritardo di 12 ore
474300091009     c                   when      d98tcc=10  and
474400091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
474500091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
474600091014     c                             d98hce=1030
474700091014     C                   MOVEL     'OSC  '       WR3TAB
474800091014     C                   eval      WR3HHI=d98hmc
474900091014     C                   EXSR      RO3ORE
475000091014     c                   if        wr3matpom='M' and d98hmc>1030
475100131218     c                   exsr      ctrLAVH1030
475200131218
475300131218     c                   if        wlav1030<>'S'
475400091014     c                   add       12            w98nrc
475500091014     c                   endif
475600131218     c                   endif
475700091014     c
475800091014     c**                 z-add     12            w98nrc
475900090827     c                   endsl
476000090827     c
476100930524     C                   ELSE                                                   <-- 1 -->
476200930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
476300930524     C* DI DIFFERENZA SONO UGUALI A ZERO
476400930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
476500931206     C     WPODCM        ANDLE     WPOCR2
476600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
476700930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
476800930525     C                   Z-ADD     WPODCM        WR2POI            9 0
476900931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
477000930525     C                   EXSR      RO2COV
477100930525     C     WR2NOV        IFNE      *ZEROS
477200930525     C     WR2NOV        MULT      -1            W98NRC
477300930525     C                   END
477400930524     C                   ELSE                                                   <-- 2 -->
477500930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
477600930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
477700930524     C     WPODCM        IFLT      WPOCRI
477800930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
477900930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
478000930524     C                   ELSE
478100931206     C                   Z-ADD     WPOCR2        WPORFI
478200930524     C                   Z-ADD     WPODCM        WPORFF
478300930524     C                   END
478400930524     C* CALCOLO ORE DI DIFFERENZA
478500930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
478600930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
478700930524     C                   Z-ADD     WPORFI        WR2POI            9 0
478800930524     C                   Z-ADD     WPORFF        WR2POF            9 0
478900930524     C                   EXSR      RO2COV
479000930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
479100050902     c
479200050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
479300100224     c                   if        d98cca='2' and w98nrc>0
479400100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
479500080116     c***                z-sub     w98nrc        w98nrc
479600080116     c                   z-add     0             w98nrc
479700050902     c                   endif
479800111018     c* Ignoro ritardo se pefect order
479900130221     c                   if        d98xco >*zeros
480000111018     c                   z-add     0             w98nrc
480100111018     c                   endif
480200070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
480300061121     c*  e sto leggendo la bolla in arrivo
480400100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
480500100224     c***                if        d98cca='1' and d98tbo='A' and
480600100224     c***                                         w98nrc>0 and wpodcm<wpocri
480700080116     c***                z-sub     w98nrc        w98nrc
480800080116     c                   z-add     0             w98nrc
480900061121     c                   endif
481000070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
481100070827     c*  mittente, --> consegnato in anticipo
481200070828     c                   if        d98tcr=' ' and d98nsp>0 and
481300070828     c                                            w98nrc>0 and wpodcm<wpocri
481400070827     c                   if        CallAR5=' '
481500070827     c                   EXSR      LeggiAR5
481600070827     c                   endif
481700070827     c                   if        dcrpart<>'S'
481800070824     c                   z-sub     w98nrc        w98nrc
481900070824     c                   endif
482000070824     c                   endif
482100050907     c
482200050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
482300050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
482400050907     c*  lo trasformo in anticipo perchè non sono in ritardo
482500050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
482600050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
482700050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
482800050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
482900050907     c                   z-sub     w98nrc        w98nrc
483000050907     c                   sub       oreILoDOPO    w98nrc
483100050907     c                   endif
483200050907     c
483300050907     C                   END                                                    <-- 2 ---
483400930524     C                   END                                                    <-- 1 ---
483500930420     C* CALCOLO LE ORE DI RESA
483600930420     C* CONTROLLO CONFORMITA
483700930423     C     W98NRC        IFLE      *ZEROS
483800930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
483900930420     C                   ELSE
484000930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
484100930420     C                   END
484200930420     C*
484300930420     C                   ENDSR
484400070827     c
484500131218     c********************************************************************
484600131218     c     CtrlavH1030   BEGSR
484700131218     c********************************************************************
484800131218     c                   clear                   wlav1030          1
484900140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
485000131218     c                   z-add     1             xx                4 0
485100131218     c                   dow       evc(xx)<>*blanks
485200140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
485300140127     c                   if        evd(xx)=d98dcm
485400131218     c                   if        evo(xx)<=1030
485500131218     c                   eval      wlav1030='S'
485600131218     c                   leave
485700131218     c                   endif
485800131218     c                   endif
485900131218     c                   add       1             xx
486000131218     c                   enddo
486100131218     c                   ENDSR
486200070827     c********************************************************************
486300070827     c* Controllo record GEN di FIAR5
486400070827     c********************************************************************
486500070827     c     LeggiAR5      BEGSR
486600070827     c                   clear                   DCRPART           1
486700141104     c                   clear                   dar5gen
486800141104     c                   clear                   gc1PART           2
486900141104     c                   clear                   gc2PART           2
487000070827     c     kar5          chain     fiar501l
487100070827    1c                   if        not %found
487200070827     c                   eval      dcrpart='S'
487300141104     c                   eval      gc1part=d98gc1
487400141104     c                   eval      gc2part=d98gc2
487500070827   x1c                   else
487600070827     c* i dati devono essere quelli immessi dalla partenza
487700070827     c                   movel     ar5uni        dar5gen
487800070827     c
487900070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
488000070827     c*  ma il record GEN è stato scritto per altri motivi
488100070827    2c                   if        §ar5mdp='S'
488200070827     c                   movel     d98dcr        alfadcr           8
488300141104     c                   eval      gc1part=§ar5gc1
488400141104     c                   eval      gc2part=§ar5gc2
488500070827     c
488600070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
488700070827     c                   eval      dcrpart='S'
488800070827    3c                   endif
488900070827     c
489000070827   x2c                   else
489100070827     c                   eval      dcrpart='S'
489200141106     c                   eval      gc1part=d98gc1
489300141106     c                   eval      gc2part=d98gc2
489400070827    2c                   endif
489500070827    1c                   endif
489600070827     c
489700140402     c* Memorizzo il flag pickup 2
489800140402     c                   eval      wpick2=§AR5PK2
489900140402     c
490000150401     c*
490100150414     c**                 if        d98cas>0 or d98ffd<>' ' or
490200150414     c**                           d98tc1='A'  or d98tc2='A' or
490300150414     c**                           d98tc1='S'  or d98tc2='S'
490400150401     c     kar4          chain     fiar401l
490500150401     c                   if        %found(fiar401l)
490600150401     c                   eval      dsbl4L=ar4not
490700150401     c                   endif
490800150401     c* Solo per fermo deposito verifico se inviato alert
490900150401     c                   if        d98ffd<>' '
491000150401     c                   clear                   fnlv82ds
491100150401     c                   eval      ILV82AAS=d98aas
491200150401     c                   eval      ILV82LNP=d98lnp
491300150401     c                   eval      ILV82NRS=d98nrs
491400150401     c                   eval      ILV82NSP=d98nsp
491500150401     c                   eval      ILV82TIPA='FD'
491600150401     c                   call      'FNLV82R'
491700150401     c                   parm                    fnlv82ds
491800150401     c                   endif
491900150414     c**                 endif
492000150401
492100150401     c                   eval      callar5='S'
492200150401
492300070827     c                   ENDSR
492400930913     C********************************************************************
492500930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
492600930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
492700930913     C*            DATA/ORA (FINE RANGE)
492800930913     C********************************************************************
492900930913     C     CALCNS        BEGSR
493000930914     C*
493100930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
493200930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
493300930914     C     WPODTI        IFEQ      *ZEROS
493400960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
493500930914     C                   ELSE
493600930914     C                   Z-ADD     WPODTI        WPOIRC
493700930914     C                   END
493800930914     C*
493900930914     C* SE NON ANCORA CONSEGNATA
494000930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
494100050922     c* in realtà nella data consegna c'e' la data fine calcolo
494200050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
494300930914     C     WPODCM        IFEQ      *ZEROS
494400050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
494500930914     C                   ELSE
494600930916     C                   Z-ADD     WPODCM        WPOFRC
494700930914     C                   END
494800930914     C*
494900930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
495000930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
495100930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
495200930914     C     WPODFC        ANDLT     WPOIRC
495300930914     C                   ELSE
495400930914     C*
495500930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
495600930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
495700930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
495800930914     C     WPODFC        ANDGT     WPOFRC
495900930914     C                   ELSE
496000930916     C*
496100930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
496200930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
496300930916     C     WPODFC        ANDGT     WPOFRC
496400930916     C                   ELSE
496500930916     C*
496600930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
496700930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
496800930916     C     WPODFC        ANDLT     WPOIRC
496900930916     C                   ELSE
497000930914     C*
497100930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
497200930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
497300930914     C     WPODIC        IFNE      *ZEROS
497400930914     C     WPODIC        ANDGT     WPOFRC
497500930914     C                   Z-ADD     WPOFRC        WPODIC
497600930914     C                   ENDIF
497700930914     C*
497800930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
497900930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
498000930914     C     WPODFC        IFGT      WPOFRC
498100930914     C                   Z-ADD     WPOFRC        WPODFC
498200930914     C                   ENDIF
498300930914     C*
498400930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
498500930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
498600930914     C     WPODIC        IFNE      *ZEROS
498700930914     C     WPODIC        ANDLT     WPOIRC
498800930914     C                   Z-ADD     WPOIRC        WPODIC
498900930914     C                   ENDIF
499000930914     C*
499100930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
499200930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
499300930914     C     WPODFC        IFLT      WPOIRC
499400930914     C                   Z-ADD     WPOIRC        WPODFC
499500930914     C                   ENDIF
499600930914     C*
499700930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
499800931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
499900930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
500000930916     C*
500100931210     C***********WPODIC    IFNE *ZEROS
500200931210     C***********WPODFC    ANDNE*ZEROS
500300931210     C*********************ADD  12        WPODFC
500400931210     C*********************ENDIF
500500930914     C     WPODIC        IFEQ      *ZEROS
500600930914     C     WPODFC        ANDNE     *ZEROS
500700931130     C                   Z-ADD     WPODFC        WPODIC
500800931210     C*********************ADD  12        WPODFC
500900930916     C                   ENDIF
501000930913     C*
501100930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
501200930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
501300930914     C* INTERNA
501400050908     c                   eval      Usacons='S'
501500930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
501600930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
501700931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
501800931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
501900940324     C                   EXSR      RO9COV
502000931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
502100930914     C* CLIENTE
502200930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
502300931210     C                   EXSR      RO9COV
502400931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
502500050908     c                   eval      Usacons=' '
502600930914     C*
502700930916     C                   ENDIF                                                  <-- 5 ---
502800930916     C                   ENDIF                                                  <-- 4 ---
502900930916     C                   ENDIF                                                  <-- 3 ---
503000930914     C                   ENDIF                                                  <-- 2 ---
503100930914     C                   ENDIF                                                  <-- 1 ---
503200930913     C*
503300930913     C                   ENDSR
503400930325     C********************************************************************
503500930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
503600930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
503700930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
503800930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
503900930325     C********************************************************************
504000930325     C     RO1CDP        BEGSR
504100930414     C*
504200931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
504300931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
504400931126     C*
504500931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
504600931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
504700150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
504800150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
504900931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
505000060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
505100930326     C*
505200930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
505300931130     C                   Z-ADD     WR1POI        WR1POO            9 0
505400930325     C*
505500930325     C* CICLO FINO A CHE:
505600930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
505700930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
505800930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
505900930406     C     WR1POO        ANDLT     WPOLIM
506000930414     C*
506100930407     C                   Z-ADD     WR1POO        WPOROU            9 0
506200930325     C                   MOVEL     'N'           WSLITT            1
506300930414     C*
506400930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
506500060420     C*
506600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
506700150605     c     wdee          andeq     ' '
506800930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
506900930414     C                   END
507000930511     C*
507100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
507200150608     c     wdee          andeq     ' '
507300930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
507400930511     C                   END
507500930414     C*
507600930325     C     WSLITT        IFEQ      'N'
507700930423     C                   SUB       12            WR1NOI            5 0
507800930325     C                   END
507900931130     C                   ADD       12            WR1POO            9 0
508000930414     C*
508100931223     C                   ENDDO                                                  <-- 1 ---
508200931126     C*
508300931126     C                   ELSE                                                   <-- 0 -->
508400931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
508500931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
508600931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
508700960129     C     WPOLAF        IFGT      WPODCG
508800960129     C                   Z-ADD     WPOLAF        WTM090            9 0
508900960129     C                   ELSE
509000960129     C                   Z-ADD     WPODCG        WTM090
509100960129     C                   ENDIF
509200960129     C     WPOGIF        IFGT      WTM090
509300960129     C                   Z-ADD     WPOGIF        WTM090
509400960129     C                   ENDIF
509500060420     C     WPOpaif       IFGT      WTM090
509600060420     C                   Z-ADD     WPOpaif       WTM090
509700060420     C                   ENDIF
509800960129     C*
509900960129     C     12            ADD       WTM090        WR1POO
510000960129     C                   ENDIF                                                  <-- 0 ---
510100930512     C*
510200931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
510300931202     C     WR1POO        SUB       12            WR7POI
510400931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
510500931202     C                   ADD       12            WR7POO
510600931202     C     WR7POO        IFGT      WR1POO
510700931202     C                   Z-ADD     WR7POO        WR1POO
510800931202     C                   END
510900930406     C*
511000930325     C                   ENDSR
511100930414     C********************************************************************
511200930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
511300930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
511400930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
511500930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
511600930414     C********************************************************************
511700930414     C     RO2COV        BEGSR
511800091014     c                   clear                   salvaEveL
511900091014     c                   clear                   salvaEveG
512000931130     C*
512100930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
512200930414     C                   Z-ADD     *ZEROS        WR2NOV
512300930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
512400931130     C                   Z-ADD     WR2POI        WR2POW
512500930414     C*
512600930414     C* CICLO FINO A CHE:
512700930414     C*   - DATA INIZIO SUPERATO DATA FINE
512800930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
512900931223     C     WR2POW        ANDLT     WPOLIM
513000930414     C*
513100930414     C                   Z-ADD     WR2POW        WPOROU
513200930414     C                   MOVEL     'N'           WSLITT            1
513300930414     C*
513400930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
513500930414     C*
513600930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
513700930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
513800930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
513900930414     C                   END
514000930414     C*
514100930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
514200930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
514300930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
514400930618     C                   END
514500060420     c
514600060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
514700060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
514800060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
514900060420     C                   END
515000060420     c
515100931201     C*
515200931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
515300931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
515400931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
515500931201     C                   END
515600060418     c
515700060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
515800060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
515900060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
516000060418     C                   END
516100930414     C*
516200930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
516300930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
516400930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
516500930414     C                   END
516600930511     C*
516700930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
516800930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
516900930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
517000930511     C                   END
517100930414     C*
517200930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
517300930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
517400930414     C     WSLITT        IFEQ      'N'
517500930423     C                   ADD       12            WR2NOV            5 0
517600930414     C                   END
517700931130     C                   ADD       12            WR2POW            9 0
517800930414     C*
517900931223     C                   ENDDO                                                  <-- 1 ---
518000930414     C*
518100930414     C                   ENDSR
518200941228     C********************************************************************
518300941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
518400941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
518500941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
518600941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
518700941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
518800941228     C********************************************************************
518900941228     C     RO2COP        BEGSR
519000941228     C*
519100941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
519200941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
519300941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
519400941228     C                   Z-ADD     *ZEROS        WR2NOV
519500941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
519600941228     C                   Z-ADD     WR2POI        WR2POW
519700941228     C*
519800941228     C* CICLO FINO A CHE:
519900941228     C*   - DATA INIZIO SUPERATO DATA FINE
520000941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
520100941228     C*
520200950215     C                   Z-ADD     WR2POW        WPOROU
520300941228     C                   MOVEL     'N'           WSLITT
520400941228     C* TRASFORMA IL PROGR. ORE IN DATA
520500950215     C*******              Z-ADDWR2POW    WORE
520600950215     C*******              EXSR OREDAT
520700950215     C*******              Z-ADDWDAT      W12DAT  80
520800950215     C*******              Z-ADDWRES      W12RES  30
520900941228     C*
521000941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
521100941228     C*
521200941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
521300941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
521400941228     C     WSLITT        IFEQ      'N'
521500941228     C                   ADD       12            WR2NOV            5 0
521600941228     C                   END
521700941228     C                   ADD       12            WR2POW            9 0
521800941228     C*
521900941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
522000941228     C* ENTRI IN LOOP
522100941228     C     WR2NOV        IFGT      999
522200941228     C                   LEAVE                                                  *VA A ENDDO
522300941228     C                   END
522400941228     C*
522500941228     C                   ENDDO                                                  <-- 1 ---
522600941228     C*
522700941228     C                   ENDSR
522800930510     C********************************************************************
522900930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
523000930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
523100930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
523200930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
523300050905     c*                     il num ore deve essere un multiplo di 12
523400050905     c*                     oppure 1 ora
523500930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
523600930510     C********************************************************************
523700930510     C     RO4CDP        BEGSR
523800930510     C*
523900930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
524000930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
524100050905     c                   movel     '1'           wgiro             1
524200930510     C*
524300930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
524400930510     C                   Z-ADD     WR4POI        WR4POO            9 0
524500930510     C*
524600930510     C* CICLO FINO A CHE:
524700930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
524800930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
524900930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
525000930510     C     WR4POO        ANDLT     WPOLIM
525100930510     C*
525200141106     c                   if        nosomma<>'N'
525300931130     C                   ADD       12            WR4POO            9 0
525400141106     c                   endif
525500930510     C                   Z-ADD     WR4POO        WPOROU            9 0
525600930510     C                   MOVEL     'N'           WSLITT            1
525700930510     C*
525800930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
525900050905     c
526000050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
526100930510     C     WSLITT        IFEQ      'N'
526200050905     c
526300050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
526400050905     c*  se cado in giorno non festivo
526500050905     c                   if        wr4noi=1 and wgiro='1'
526600050905     c                   sub       12            wr4poo
526700050905     c                   add       wr4noi        wr4poo
526800050905     c                   endif
526900050905     c*
527000050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
527100050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
527200050905     c*  calcolate dal pgm
527300050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
527400050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
527500050905     c*  del giorno sucessivo non festivo
527600050905     c                   if        wr4noi=1 and wgiro<>'1'
527700050905     c                   add       wr4noi        wr4poo
527800050905     c                   endif
527900050905     c
528000930510     C                   SUB       12            WR4NOI            5 0
528100930510     C                   END
528200141106     c
528300141106     c                   if        nosomma= 'N'
528400141106     C                   ADD       12            WR4POO            9 0
528500141106     c                   endif
528600930510     C*
528700050905     c                   eval      wgiro='2'
528800931223     C                   ENDDO                                                  <-- 1 ---
528900930510     C*
529000930510     C                   ENDSR
529100931130     C********************************************************************
529200931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
529300931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
529400931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
529500931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
529600931130     C********************************************************************
529700931130     C     RO5COV        BEGSR
529800931130     C*
529900931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
530000060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
530100060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
530200060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
530300931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
530400931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
530500931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
530600931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
530700931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
530800931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
530900931130     C                   Z-ADD     *ZEROS        WR5NOV
531000940714     C*
531100940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
531200940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
531300931207     C*
531400931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
531500940908     C     WIC           IFEQ      'I'
531600090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
531700090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
531800931207     C                   ELSE
531900090827     C*****              Z-ADD     D98TCC        WPOTCT
532000090827     C                   Z-ADD     w99TCC        WPOTCT
532100931207     C                   END
532200931130     C*
532300931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
532400060420     C                   clear                   COV
532500931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
532600931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
532700931207     C                   Z-ADD     2             CO                3 0
532800931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
532900931207     C                   EXSR      RO5SUB
533000981102     C                   DO        99            IC                3 0
533100931207     C     LAF(IC)       IFNE      *ZEROS
533200931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
533300931207     C                   EXSR      RO5SUB
533400931207     C                   ENDIF
533500931207     C                   ENDDO
533600060420     c
533700060420     C                   DO        99            IC                3 0
533800060420     C     paif(IC)      IFNE      *ZEROS
533900060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
534000060420     C                   EXSR      RO5SUB
534100060420     C                   ENDIF
534200060420     C                   ENDDO
534300931207     C* ORDINA LA SCHIERA COV
534400931207     C                   SORTA     COV
534500931130     C*
534600931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
534700931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
534800981102     C                   DO        98            IC                             --- 1 -->
534900931207     C     IC            ADD       1             I1C               3 0
535000931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
535100931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
535200931207     C     COV(I1C)      OREQ      *ZEROS
535300931207     C                   ELSE
535400931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
535500931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
535600931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
535700931130     C                   EXSR      RO2COV
535800931207     C                   ADD       WR2NOV        WR5NOV            9 0
535900931209     C* SE NON SONO IN DATA CONSEGNA
536000931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
536100931207     C     WR5NOV        IFLT      WPOTCT
536200931209     C     COV(I1C)      ANDNE     WPODCM
5363009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5364009709111009 C     WPOTCT        SUB       12            WR5NOV
536500931130     C                   END
536600931207     C                   ENDIF                                                  <-- 2 ---
536700931207     C                   ENDDO                                                  <-- 1 ---
536800940714     C                   ENDIF                                                  <-- 0 ---
536900931130     C*
537000931130     C                   ENDSR
537100931207     C********************************************************************
537200931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
537300931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
537400931207     C********************************************************************
537500931207     C     RO5SUB        BEGSR
537600931207     C*
537700931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
537800931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
537900931207     C     WR5COV        ANDLT     WR5POF
538000931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
538100931207     C                   MOVEL     'N'           WR5PRE            1
538200981102     C*---                 DO   30        I
538300981102     C                   DO        99            I
538400931207     C     WR5COV        IFEQ      COV(I)
538500931207     C                   MOVEL     'S'           WR5PRE
538600931207     C                   LEAVE
538700931207     C                   ENDIF
538800931207     C                   ENDDO
538900931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
539000931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
539100931207     C                   ADD       1             CO                3 0
539200931207     C                   Z-ADD     WR5COV        COV(CO)
539300931207     C                   ENDIF                                                  <-- 2 ---
539400931207     C                   ENDIF                                                  <-- 1 ---
539500931207     C*
539600931207     C                   ENDSR
539700931201     C********************************************************************
539800060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
539900931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
540000931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
540100931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
540200931201     C********************************************************************
540300931201     C     RO6CDT        BEGSR
540400931201     C*
540500931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
540600931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
540700931201     C*
540800931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
540900931201     C                   Z-ADD     WR6POI        WR6POO            9 0
541000931201     C*
541100931201     C* CICLO FINO A CHE:
541200931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
541300931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
541400931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
541500931201     C     WR6POO        ANDLT     WPOLIM
541600931201     C*
541700931201     C                   Z-ADD     WR6POO        WPOROU            9 0
541800931201     C                   MOVEL     'N'           WSLITT            1
541900931201     C*
542000931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
542100931201     C*
542200931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
542300150605     c     wdee          andeq     ' '
542400931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
542500931201     C                   END
542600931201     C*
542700931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
542800150608     c     wdee          andeq     ' '
542900931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
543000931201     C                   END
543100931201     C*
543200931201     C     WSLITT        IFEQ      'N'
543300931201     C                   SUB       12            WR6NOI            5 0
543400931201     C                   END
543500931201     C                   ADD       12            WR6POO            9 0
543600931201     C*
543700931223     C                   ENDDO                                                  <-- 1 ---
543800931201     C*
543900931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
544000931202     C     WR6POO        SUB       12            WR7POI
544100931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
544200931207     C                   ADD       12            WR7POO
544300931202     C     WR7POO        IFGT      WR6POO
544400931201     C                   Z-ADD     WR7POO        WR6POO
544500931202     C                   END
544600931201     C*
544700931201     C                   ENDSR
544800931201     C********************************************************************
544900060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
545000060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
545100060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
545200060407     C*            INPUT:  -WR7POI (PROGR.ORE)
545300931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
545400931201     C********************************************************************
545500931201     C     RO7TCF        BEGSR
545600060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
545700060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
545800060407     C                   MOVEL     'N'           WSLITT
545900931201     C*
546000931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
546100931201     C                   Z-ADD     WR7POI        WR7POO            9 0
546200931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
546300931201     C* COINCIDA CON UN TURNO DI CHIUSURA
546400060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
546500931201     C     D98GC2        ANDEQ     *BLANKS
546600060407   x1C                   ELSE                                                   <-- 1 -->
546700931214     C*
546800931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
546900931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
547000931214     C*
547100931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
547200931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
547300931223     C     WPOROU        ANDLT     WPOLIM
547400931214     C* MEMORIZZO INIZIO E FINE RANGE
547500931202     C     WPOTI         IFEQ      *ZEROS
547600931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
547700931202     C                   END
547800931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
547900931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
548000931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
548100931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
548200931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
548300931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
548400931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
548500931201     C                   MOVEL     'N'           WSLITT
548600931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
548700060407     c* Se non trovat, controllo se c'e' evento part che giustifica
548800060407     c                   if        wslitt='N'
548900060407     c                   exsr      datcepg
549000060407     c                   endif
549100060407     c
549200931201     C                   ENDDO                                                  <-- 2 ---
549300931214     C*
549400931201     C                   Z-ADD     WPOROU        WR7POO
549500931214     C*
549600931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
549700931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
549800931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
549900931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
550000931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
550100931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
550200931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
550300931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
550400931209     C                   ELSE                                                   <-- 3 -->
550500931209     C     WIC           IFEQ      'I'
550600931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
550700931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
550800931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
550900931201     C                   ELSE
551000931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
551100931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
551200931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
551300931201     C                   ENDIF
551400931209     C                   ENDIF                                                  <-- 3 ---
551500931209     C                   ENDIF                                                  <-- 2 ---
551600931214     C*
551700060407    1C                   ENDIF                                                  <-- 1 ---
551800060407     c
551900060407     c* Se non trovato turno di chiusura, verifico se c'e'
552000060407     c*  evento particolare che GIUSTIFICA
552100060407     c                   if        wpoti=0
552200060418     C                   EXSR      RO7CEPG
552300060407     c                   endif
552400931201     C*
552500931201     C                   ENDSR
552600060406     C********************************************************************
552700060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
552800060406     c*                        che giustifica
552900060407     C*            INPUT:-WR7POI (PROGR.ORE)
553000060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
553100060406     C********************************************************************
553200060407     C     RO7CEPG       BEGSR
553300060406     C*
553400060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
553500060407     C                   Z-ADD     WR7POI        WR7POO            9 0
553600060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
553700060406     C* coincide
553800060406    1C                   if        wevpag='S'                                   <-- 1 -->
553900060406     C*
554000060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
554100060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
554200060406     C                   MOVEL     'N'           WSLITT
554300060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
554400060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
554500060406     C*
554600060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
554700060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
554800060406     C     WPOROU        ANDLT     WPOLIM
554900060406     C* MEMORIZZO INIZIO E FINE RANGE
555000060406    3C     WPOTI         IFEQ      *ZEROS
555100060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
555200060406    3C                   END
555300060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
555400060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
555500060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
555600060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
555700060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
555800060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
555900060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
556000060406     C                   MOVEL     'N'           WSLITT
556100060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
556200060406     C* Se non trovato,
556300060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
556400060406    3c                   if        wslitt='N'
556500060406     c                   exsr      DATTUC
556600060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
556700060406    2C                   ENDDO                                                  <-- 2 ---
556800060406     C*
556900060407     C                   Z-ADD     WPOROU        WR7POO
557000060406     C*
557100060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
557200060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
557300060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
557400060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
557500060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
557600060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
557700060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
557800060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
557900060406   x3C                   ELSE                                                   <-- 3 -->
558000060406    4C     WIC           IFEQ      'I'
558100060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
558200060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
558300060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
558400060406   x4C                   ELSE
558500060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
558600060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
558700060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
558800060406    4C                   ENDIF
558900060406    3C                   ENDIF                                                  <-- 3 ---
559000060406    2C                   ENDIF                                                  <-- 2 ---
559100060406     C*
559200060406    1C                   ENDIF                                                  <-- 1 ---
559300060406     C*
559400060406     C                   ENDSR
559500931207     C********************************************************************
559600931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
559700931207     C*            INPUT:  -WR8POI (PROGR.ORE)
559800931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
559900931207     C********************************************************************
560000931207     C     RO8FES        BEGSR
560100130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
560200130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
560300931207     C*
560400931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
560500130411     C     WR8POI        SUB       12            WR8POO            9 0
560600931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
560700931207     C                   MOVEL     'N'           WSLITT
560800130411     c
560900130411     c* Non salto i festivi ???
561000130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
561100130411     c
561200931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
561300130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
561400130411     C     WPOROU        ANDGT     WPODSP
561500130411     C                   SUB       12            WPOROU
561600130411     C                   MOVEL     'N'           WSLITT
561700130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
561800130411     C                   ENDDO                                                  <-- 2 ---
561900130411     c
562000130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
562100130411     C     D98GC2        ANDEQ     *BLANKS
562200130411   x1C                   ELSE                                                   <-- 1 -->
562300130411     c* eseguo controllo per i turni di chiusura
562400130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
562500130411     C*
562600130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
562700130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
562800130411     C     WPOROU        ANDgT     WPOdsp
562900130411     C* MEMORIZZO INIZIO E FINE RANGE
563000130411     C     WPOTf         IFEQ      *ZEROS
563100130411     C                   z-add     wporou        WPOTf
563200130411     C                   END
563300140826     C     WPOROU        SUB       wnoadd        WPOROU
563400130411     c
563500130411     C                   MOVEL     'N'           WSLITT
563600130411     c                   exsr      dattuc
563700130411     c                   if        wslitt='N'
563800130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
563900140826     c* Se slitta per festivo --> tolgo sempre 12 ore
564000140826     c                   z-add     12            WNOADD
564100130411     c                   endif
564200130411     C* SLITTA
564300140826     C     wporou        add       12            WPOTi
564400130411    2C                   ENDDO                                                  <-- 2 ---
564500130411     c
564600130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
564700130411     c
564800130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
564900130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
565000130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
565100130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
565200130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
565300130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
565400130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
565500130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
565600130411   x3C                   ELSE                                                   <-- 3 -->
565700130411    4C     WIC           IFEQ      'I'
565800130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
565900130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
566000130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
566100130411   x4C                   ELSE
566200130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
566300130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
566400130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
566500130411    4C                   ENDIF
566600130411    3C                   ENDIF                                                  <-- 3 ---
566700130411    2C                   ENDIF                                                  <-- 3 ---
566800130411     C*
566900130411    1C                   ENDIF                                                  <-- 1 ---
567000130411
567100130411     C**   WPOROU        ADD       12            WR8POO
567200130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
567300130411     c                   z-add     wr8poi        wr8poo
567400931207     C*
567500931207     C                   ENDSR
567600931210     C********************************************************************
567700931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
567800940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
567900931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
568000931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
568100931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
568200931210     C********************************************************************
568300931210     C     RO9COV        BEGSR
568400931210     C*
568500931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
568600931210     C                   Z-ADD     *ZEROS        WR9NOV
568700931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
568800931210     C                   Z-ADD     WR9POI        WR9POW
568900931210     C* CICLO FINO A CHE:
569000931210     C*   - DATA INIZIO SUPERATO DATA FINE
569100931223     C*   - RAGGIUNTO LIMITE MASSIMO
569200931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
569300931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
569400931223     C     WR9POW        ANDLT     WPOLIM
569500931210     C*
569600931210     C                   Z-ADD     WR9POW        WPOROU
569700931210     C                   MOVEL     'N'           WSLITT            1
569800940324     C* FESTIVI
569900931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
570000940324     C* GIACENZA
570100931214     C     WSLITT        IFEQ      'N'
570200941229£$   C     WPOAGC        IFNE      *ZEROS
570300940324£$   C     WPOCGC        ORNE      *ZEROS
570400941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
570500940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
570600940324£$   C                   MOVEL     'S'           WSLITT
570700940324£$   C                   END
570800940324£$   C                   END
570900940324£$   C                   END
571000960129     C* EVENTI GIACENZA
571100960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
571200960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
571300960129     C                   Z-ADD     *ZEROS        G                 3 0
571400960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
571500960129     C     G             ANDLT     IG
571600960129     C                   ADD       1             G
571700960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
571800960209     C     WPOROU        ANDLE     GCF(G)
571900960129     C                   MOVEL     'S'           WSLITT
572000960129     C                   ENDIF                                                  <-- 3 ---
572100960129     C                   ENDDO                                                  <-- 2 ---
572200960129     C                   ENDIF                                                  <-- 1 ---
572300960129     C                   ENDIF                                                  <-- 0 ---
572400940324     C* LASCIATO AVVISO
572500931214     C     WSLITT        IFEQ      'N'
572600941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
572700940324£$   C                   Z-ADD     *ZEROS        L                 3 0
572800940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
572900940324£$   C     L             ANDLT     IL
573000940324£$   C                   ADD       1             L
573100941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
573200941229£$   C     WPOROU        ANDLE     LCF(L)
573300050520     c     LCon(L)       andeq     'S'
573400940324£$   C                   MOVEL     'S'           WSLITT
573500940324£$   C                   END                                                    <-- 3 ---
573600940324£$   C                   ENDDO                                                  <-- 2 ---
573700940324£$   C                   END                                                    <-- 1 ---
573800931210     C                   END
573900940324     C* TURNO CHIUSURA
574000140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
574100140826     c*  di sistemare le ore da sommare 12 o 24...
574200931214     C     WSLITT        IFEQ      'N'
574300931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
574400931210     C                   END
574500940324     C* DATA CONSEGNA RICHIESTA
574600050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
574700050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
574800050907     c
574900931214     C     WSLITT        IFEQ      'N'
575000050907     c                   add       1             wporou
575100940324£$   C     WPOCRI        IFNE      *ZEROS
575200940324£$   C     WPOROU        IFLT      WPOCRC
575300940324£$   C                   MOVEL     'S'           WSLITT
575400940324£$   C                   END
575500940324£4   C                   END
575600050907     c                   sub       1             wporou
575700931210     C                   END
575800931210     C*
575900931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
576000931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
576100931210     C     WSLITT        IFEQ      'N'
576200931210     C                   ADD       12            WR9NOV            5 0
576300931210     C                   END
576400931210     C                   ADD       12            WR9POW            9 0
576500931210     C*
576600931223     C                   ENDDO                                                  <-- 1 ---
576700931210     C*
576800931210     C                   ENDSR
576900940628%%%  C********************************************************************
577000940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
577100940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
577200940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
577300940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
577400940628%%%  C********************************************************************
577500940628%%%  C     RO0GES        BEGSR
577600940628%%%  C*
577700940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
577800940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
577900940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
578000940628%%%  C*
578100940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
578200940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
578300940628%%%  C*
578400940628%%%  C* CICLO FINO A CHE:
578500940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
578600940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
578700940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
578800050922%%%  C     WR0POO        ANDGe     WPODSP
578900940628%%%  C*
579000940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
579100940628%%%  C                   MOVEL     'N'           WSLITT            1
579200940628%%%  C*
579300940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
579400940628%%%  C*
579500940628%%%  C     WSLITT        IFEQ      'N'
579600940628%%%  C                   SUB       12            WR0NOI            5 0
579700940628%%%  C                   END
579800940628%%%  C                   SUB       12            WR0POO            9 0
579900940628%%%  C*
580000940628%%%  C                   ENDDO                                                  <-- 1 ---
580100940628%%%  C*
580200940628%%%  C                   ADD       12            WR0POO            9 0
580300940628%%%  C*
580400940628%%%  C                   ENDSR
580500960911     C********************************************************************
580600960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
580700960911     C*            INPUT:  -W10POI (PROGR.ORE)
580800960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
580900960911     C********************************************************************
581000960911     C     R10FES        BEGSR
581100960911     C*
581200960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
581300960911     C                   Z-ADD     W10POI        W10POO            9 0
581400960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
581500960911     C                   MOVEL     'N'           WSLITT
581600960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
581700960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
581800960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
581900960911     C                   ADD       12            WPOROU
582000960911     C                   MOVEL     'N'           WSLITT
582100960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
582200960911     C                   ENDDO                                                  <-- 2 ---
582300960911     C                   Z-ADD     WPOROU        W10POO
582400960911     C*
582500960911     C                   ENDSR
582600930325     C********************************************************************
582700930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
582800930325     C********************************************************************
582900930325     C     CHKGIA        BEGSR
583000930406     C*
583100960129     C* APERTURA/CHIUSURA GIACENZA
583200930326     C     WPODAG        IFNE      *ZEROS
583300930406     C     WPODCG        ORNE      *ZEROS
583400930407     C     WPOROU        IFGE      WPODAG
583500930407     C     WPOROU        ANDLE     WPODCG
583600930325     C                   MOVEL     'S'           WSLITT
583700930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
583800930325     C                   END
583900930326     C                   END
584000960129     C*
584100960129     C* EVENTI GIACENZA
584200960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
584300960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
584400960129     C                   Z-ADD     *ZEROS        G                 3 0
584500960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
584600960129     C     G             ANDLT     IG
584700960129     C                   ADD       1             G
584800960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
584900960129     C     WPOROU        ANDLE     GIF(G)
585000091009     c
585100091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
585200091009     c*  l'orario dell'evento
585300091009     c                   select
585400091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
585500091009     c* Se orario entro le 10.30  va bene
585600091014     c*  solo la prima volta per evento cioè quando controllo
585700091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
585800091014     c*  e non devo dare errore
585900091014     C                   MOVEL     'OSC  '       WR3TAB
586000091014     C                   eval      WR3HHI=evo(gip(G))
586100091014     C                   EXSR      RO3ORE
586200091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
586300091014     c*  in ritardo come per "E")
586400091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
586500091014     c                              wr3matpom='P'
586600091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
586700091009     C                   MOVEL     'S'           WSLITT
586800091009     c                   endif
586900091014     c                   eval      SalvaEveG=gip(G)
587000091009     c
587100091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
587200091009     c* Se orario entro le 10.30  va bene
587300091014     c*  solo la prima volta per evento cioè quando controllo
587400091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
587500091014     c*  e non devo dare errore
587600091014     C                   MOVEL     'OSC  '       WR3TAB
587700091014     C                   eval      WR3HHI=evo(gip(G))
587800091014     C                   EXSR      RO3ORE
587900091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
588000091014     c*  in ritardo come per "E")
588100091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
588200091014     c                              wr3matpom='P'
588300091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
588400091009     C                   MOVEL     'S'           WSLITT
588500091009     c                   endif
588600091014     c                   eval      SalvaEveG=gip(G)
588700091009     c
588800091009     c                   other
588900960129     C                   MOVEL     'S'           WSLITT
589000960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
589100091009     c                   endsl
589200091009
589300960129     C                   ENDIF                                                  <-- 3 ---
589400960129     C                   ENDDO                                                  <-- 2 ---
589500960129     C                   ENDIF                                                  <-- 1 ---
589600960129     C                   ENDIF                                                  <-- 0 ---
589700930406     C*
589800930325     C                   ENDSR
589900060420     C********************************************************************
590000930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
590100930325     C********************************************************************
590200930325     C     CHKLAV        BEGSR
590300930325     C*
590400941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
590500930421     C                   Z-ADD     *ZEROS        L                 3 0
590600930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
590700930421     C     L             ANDLT     IL
590800930421     C                   ADD       1             L
590900930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
591000930421     C     WPOROU        ANDLE     LAF(L)
591100090928     c
591200090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
591300091009     c*  l'orario dell'evento
591400091009     c                   select
591500091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
591600091009     c* Se orario entro le 10.30  va bene
591700091014     c*  solo la prima volta per evento cioè quando controllo
591800091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
591900091014     c*  e non devo dare errore
592000091014     C                   MOVEL     'OSC  '       WR3TAB
592100091014     C                   eval      WR3HHI=evo(lap(L))
592200091014     C                   EXSR      RO3ORE
592300091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
592400091014     c*  in ritardo come per "E")
592500091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
592600091014     c                              wr3matpom='P'
592700091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
592800091009     C                   MOVEL     'S'           WSLITT
592900091009     c                   endif
593000091014     c                   eval      SalvaEveL=lap(L)
593100091009     c
593200091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
593300091009     c* Se orario entro le 10.30  va bene
593400091014     c*  solo la prima volta per evento cioè quando controllo
593500091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
593600091014     c*  e non devo dare errore
593700091014     C                   MOVEL     'OSC  '       WR3TAB
593800091014     C                   eval      WR3HHI=evo(lap(L))
593900091014     C                   EXSR      RO3ORE
594000091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
594100091014     c*  in ritardo come per "E")
594200091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
594300091014     c                              wr3matpom='P'
594400091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
594500091009     C                   MOVEL     'S'           WSLITT
594600091009     c                   endif
594700091014     c                   eval      SalvaEveL=lap(L)
594800090928     c
594900091009     c                   other
595000930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
595100091009     C                   MOVEL     'S'           WSLITT
595200091009     c                   endsl
595300090928     c
595400930421     C                   END                                                    <-- 3 ---
595500931223     C                   ENDDO                                                  <-- 2 ---
595600930421     C                   END                                                    <-- 1 ---
595700930325     C*
595800930325     C                   ENDSR
595900060420     C********************************************************************
596000060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
596100060420     c*                   che Allungano
596200060420     C********************************************************************
596300060420     C     CHKCEPA       BEGSR
596400060420     C*
596500060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
596600060420     C                   Z-ADD     *ZEROS        p                 3 0
596700060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
596800060420     C     p             ANDLT     IpA
596900060420     C                   ADD       1             P
597000060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
597100060420     C     WPOROU        ANDLE     paif(P)
597200060420     C                   MOVEL     'S'           WSLITT
597300060420     C                   ADD       12            W98EA
597400060420     C                   END                                                    <-- 3 ---
597500060420     C                   ENDDO                                                  <-- 2 ---
597600060420     C                   END                                                    <-- 1 ---
597700060420     C*
597800060420     C                   ENDSR
597900930414     C********************************************************************
598000930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
598100930414     C********************************************************************
598200930414     C     CHKCNT        BEGSR
598300930511     C*
598400930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
598500930611     C     WIC           IFEQ      'I'                                          *INTERNO
598600930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
598700930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
598800930611     C                   ELSE
598900930611     C     WPOROU        IFGE      WPOAII
599000930611     C     WPOROU        ANDLE     WPOAFI
599100930611     C                   MOVEL     'S'           WSLITT
599200930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
599300930611     C                   END
599400930611     C                   END
599500930611     C                   ELSE                                                   *CLIENTE
599600930611     C     WPOAIC        IFEQ      *ZEROS
599700930611     C     WPOAFC        ANDEQ     *ZEROS
599800930611     C                   ELSE
599900930611     C     WPOROU        IFGE      WPOAIC
600000930611     C     WPOROU        ANDLE     WPOAFC
600100930611     C                   MOVEL     'S'           WSLITT
600200930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
600300930611     C                   END
600400930611     C                   END
600500930611     C                   END
600600930414     C*
600700930414     C                   ENDSR
600800930511     C********************************************************************
600900930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
601000930511     C********************************************************************
601100930511     C     CHKFED        BEGSR
601200930511     C*
601300930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
601400930611     C     WIC           IFEQ      'I'                                          *INTERNO
601500930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
601600930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
601700930611     C                   ELSE
601800930611     C     WPOROU        IFGE      WPOFII
601900930611     C     WPOROU        ANDLE     WPOFFI
602000930611     C                   MOVEL     'S'           WSLITT
602100930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
602200930611     C                   END
602300930611     C                   END
602400930611     C                   ELSE                                                   *CLIENTE
602500930611     C     WPOFIC        IFEQ      *ZEROS
602600930611     C     WPOFFC        ANDEQ     *ZEROS
602700930611     C                   ELSE
602800930611     C     WPOROU        IFGE      WPOFIC
602900930611     C     WPOROU        ANDLE     WPOFFC
603000930611     C                   MOVEL     'S'           WSLITT
603100930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
603200930611     C                   END
603300930611     C                   END
603400930611     C                   END
603500930511     C*
603600930511     C                   ENDSR
603700930325     C********************************************************************
603800930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
603900930325     C********************************************************************
604000930325     C     CHKFES        BEGSR
604100060526     c* se già passata la data, non controllo wporou
604200060526     c                   if        wpodata>0
604300060526     c                   z-add     wpodata       w12dat
604400060526     c                   clear                   w12res
604500060526     c                   else
604600931130     C*
604700931130     C* TRASFORMA IL  PROGR.ORE IN DATA
604800931130     C                   Z-ADD     WPOROU        WORE
604900931130     C                   EXSR      OREDAT
605000941227     C                   Z-ADD     WDAT          W12DAT            8 0
605100931130     C                   Z-ADD     WRES          W12RES            3 0
605200060526     c                   endif
605300930325     C*
605400941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
605500930325     C* E' FESTIVO
605600090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
605700090108     C**                 Z-ADD     D98LNP        CLNTFP
605800090108     C**                 Z-ADD     *ZEROS        CLNTFA
605900090108     C**                 ENDIF
606000090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
606100090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
606200090108     c**   è possibile rendere festivo in terminal per un altro terminal
606300090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
606400930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
606500090108     C     WTC           oreq      'P'                                          *TRAZIONE
606600941227     C                   Z-ADD     D98TFP        CLNTFP
606700941227     C                   Z-ADD     D98TFA        CLNTFA
606800941228     C                   ENDIF
606900941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
607000941227     C                   Z-ADD     *ZEROS        CLNTFP
607100941227     C                   Z-ADD     D98LNA        CLNTFA
607200941228     C                   ENDIF
607300941227     C                   Z-ADD     W12DAT        DSDAT8
607400941227     C                   Z-ADD     DSAAS         CLNANN
607500941227     C                   Z-ADD     DSMES         CLNMES
607600930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
607700930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
607800930421     C                   Z-ADD     DSGIO         G
607900930415     C     W12RES        IFEQ      *ZEROS
608000941227     C                   MOVEL     CLNMAT        GFEDS
608100930415     C                   ELSE
608200941227     C                   MOVEL     CLNPOM        GFEDS
608300930415     C                   END
608400930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
608500930405     C                   MOVEL     *BLANKS       WC1Y
608600930421     C                   MOVEL     GFE(G)        WC1Y              8
608700930421     C                   Z-ADD     1             I
608800930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
608900930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
609000930405     C     WIC           IFEQ      'I'
609100930421     C     I1Y(I)        ANDEQ     'S'
609200930405     C                   MOVEL     'S'           WSLITT
609300930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
609400930405     C                   END
609500930405     C     WIC           IFEQ      'C'
609600930421     C     E1Y(I)        ANDEQ     'S'
609700930405     C                   MOVEL     'S'           WSLITT
609800930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
609900930405     C                   END
610000970204     C     WIC           IFEQ      'X'
610100970204     C     E1Y(I)        ANDEQ     'S'
610200970204     C                   MOVEL     'S'           WSLITT
610300970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
610400970204     C                   END
610500930405     C                   ELSE                                                   <-- 3 -->
610600930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
610700930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
610800930405     C                   END                                                    <-- 3 ---
610900930405     C                   END                                                    <-- 2 ---
611000930405     C                   END                                                    <-- 1 ---
611100930325     C*
611200060526     c                   clear                   wpodata
611300930325     C                   ENDSR
611400931201     C********************************************************************
611500931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
611600931201     C********************************************************************
611700931201     C     CHKTUC        BEGSR
611800931201     C*
611900931201     C     WIC           IFEQ      'I'                                          --- 1 -->
612000931201     C                   DO        TI            T                 3 0          --- 2 -->
612100931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
612200931201     C     WPOROU        ANDLE     TIF(T)
612300931201     C                   MOVEL     'S'           WSLITT
612400931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
612500931201     C                   LEAVE                                                  *VA A FINE DO
612600931201     C                   ENDIF                                                  <-- 3 ---
612700931201     C                   ENDDO                                                  <-- 2 ---
612800931201     C                   ELSE                                                   <-- 1 -->
612900931201     C                   DO        TC            T                 3 0          --- 2 -->
613000931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
613100931201     C     WPOROU        ANDLE     TCF(T)
613200931201     C                   MOVEL     'S'           WSLITT
613300931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
613400931201     C                   LEAVE                                                  *VA A FINE DO
613500931201     C                   ENDIF                                                  <-- 3 ---
613600931201     C                   ENDDO                                                  <-- 2 ---
613700931201     C                   ENDIF                                                  <-- 1 ---
613800931201     C*
613900931201     C                   ENDSR
614000060418     C********************************************************************
614100060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
614200060418     c*          - eventi particolari che giustificano
614300060418     C********************************************************************
614400060418     C     CHKCEPG       BEGSR
614500060418     C*
614600060418     C     WIC           IFEQ      'I'                                          --- 1 -->
614700060420     C                   DO        PGI           p                 3 0          --- 2 -->
614800060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
614900060420     C     WPOROU        ANDLE     PGIF(p)
615000060418     C                   MOVEL     'S'           WSLITT
615100060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
615200060418     C                   LEAVE                                                  *VA A FINE DO
615300060418     C                   ENDIF                                                  <-- 3 ---
615400060418     C                   ENDDO                                                  <-- 2 ---
615500060418     C                   ELSE                                                   <-- 1 -->
615600060420     C                   DO        PGC           p                 3 0          --- 2 -->
615700060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
615800060420     C     WPOROU        ANDLE     PGCF(p)
615900060418     C                   MOVEL     'S'           WSLITT
616000060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
616100060418     C                   LEAVE                                                  *VA A FINE DO
616200060418     C                   ENDIF                                                  <-- 3 ---
616300060418     C                   ENDDO                                                  <-- 2 ---
616400060418     C                   ENDIF                                                  <-- 1 ---
616500060418     C*
616600060418     C                   ENDSR
616700930512     C********************************************************************
616800931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
616900930512     C********************************************************************
617000931130     C     DATTUC        BEGSR
617100931214     C*
617200931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
617300931214     C                   Z-ADD     12            WNOADD            9 0
617400930325     C*
617500931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
617600931125     C     D98GC2        ORNE      *BLANKS
617700931130     C*
617800931213     C                   Z-ADD     WPOROU        WORE
617900931130     C                   EXSR      OREDAT                                       *ORE->DATA
618000931130     C                   Z-ADD     WDAT          W12DAT
618100931130     C                   Z-ADD     WRES          W12RES
618200930405     C*
618300941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
618400930405     C* DELLA SETTIMANA
618500941227     C                   Z-ADD     W12DAT        DSDAT8
618600941227     C                   Z-ADD     DSAAS         CLNANN
618700941227     C                   Z-ADD     DSMES         CLNMES
618800941227     C                   Z-ADD     *ZEROS        CLNTFP
618900941227     C                   Z-ADD     *ZEROS        CLNTFA
619000930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
619100930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
619200941227     C                   MOVEL     CLNMAT        GSEDS
619300960129     C                   Z-ADD     DSGIO         GI                6 0
619400960129     C                   MOVEL     GSE(GI)       WGSE              1
619500140911     c
619600140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
619700140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
619800140911     c                   if        wtuctut='S'
619900140911     c                   eval      wtuctut='P'
620000140911     c                   endif
620100930326     C*
620200931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
620300931213     C                   DO        4             IT                3 0          --- 3 -->
620400931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
620500931213     C                   MOVEL     TUC(IT)       W93GSE            1
620600931213     C                   MOVE      TUC(IT)       W93GMP            1
620700930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
620800930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
620900931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
621000930512     C     W93GSE        OREQ      ' '
621100140911     c     WTUCTUT       andeq     ' '
621200140911     c
621300930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
621400931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
621500930401     C     W93GMP        OREQ      ' '
621600930325     C                   MOVEL     'S'           WSLITT
621700931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
621800931213     C                   ELSE                                                   <-- 6 -->
621900001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
622000001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
622100001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
622200050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
622300050908     C     Usacons       IFNE      'S'                                          --- 7 -->
622400141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
622500141107     c*    orari giorno per giorno
622600141107     c                   if        OOR2FCSGL<>' '
622700141108     c                             and w12dat<>sav_12dat
622800141107     c                   eval      or_datarif=w12dat
622900141107     c                   eval      or_DDC    =w12dat
623000141107     c                   exsr      Reper_tempi
623100141108     c                   eval      sav_12dat=w12dat
623200141107     c                   endif
623300141107     c
623400930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
623500930325     C     W93GMP        IFEQ      'M'
623600930326     C     W12RES        ANDEQ     *ZEROS
623700141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
623800141111     c                             or OOR2STfS =0
623900141105     c                             or (tuc(IT)<>gc1part and
624000141105     c                              tuc(IT)<>gc2part)
624100140825     C                   MOVEL     'S'           WSLITT
624200140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
624300140911     c* slitto solo una volta  se chiuso tutti " M"
624400140911     c                   if        W93GSE=' '
624500140911     c                   eval      wTUCTUT='S'
624600140911     c                   endif
624700140911     c
624800140825     c                   endif
624900930325     C                   END
625000930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
625100930325     C     W93GMP        IFEQ      'P'
625200930326     C     W12RES        ANDNE     *ZEROS
625300140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
625400140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
625500141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
625600141111     c                             or OOR2STiS =0
625700141105     c                             or (tuc(IT)<>gc1part and
625800141105     c                              tuc(IT)<>gc2part)
625900930325     C                   MOVEL     'S'           WSLITT
626000140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
626100140911     c* slitto solo una volta  se chiuso tutti " P"
626200140911     c                   if        W93GSE=' '
626300140911     c                   eval      wTUCTUT='S'
626400140911     c                   endif
626500140825     c                   endif
626600140825     c
626700931213     C                   ENDIF
626800001128     C                   ENDIF                                                  <-- 7 ---
626900931213     C                   ENDIF                                                  <-- 6 ---
627000931213     C                   ENDIF                                                  <-- 5 ---
627100931213     C                   ENDIF                                                  <-- 4 ---
627200931213     C                   ENDDO                                                  <-- 3 ---
627300930405     C*
627400931213     C                   ENDIF                                                  <-- 2 ---
627500931213     C                   ENDIF                                                  <-- 1 ---
627600140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
627700140911     c                   if        wtuctut='P'  and wslitt='N'
627800140911     c                   clear                   wtuctut
627900140911     c                   endif
628000140911     c
628100930325     C                   ENDSR
628200060406     C********************************************************************
628300060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
628400060406     C********************************************************************
628500060406     C     DATCEPG       BEGSR
628600060406     C*
628700060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
628800060406     C                   Z-ADD     12            WNOADD            9 0
628900060406     C*
629000060406    1C                   if        wevpag='S'
629100060406     C*
629200060406     C                   Z-ADD     WPOROU        WORE
629300060406     C                   EXSR      OREDAT                                       *ORE->DATA
629400060406     C                   Z-ADD     WDAT          W12DAT
629500060406     C                   Z-ADD     WRES          W12RES
629600060418     c                   z-add     1             P
629700060406     c* Verifico se nella data esiste un evento Part che giustifica
629800060418     c     w12dat        lookup    pvd(P)                                 30
629900060418    2c                   dow       *in30 and pvf(P)<>'G'
630000060418     c                   add       1             P
630100060420     c     w12dat        lookup    pvd(P)                                 30
630200060406    2c                   enddo
630300060406     c
630400060406     c* 30 on --> TROVATO !!
630500060406    2c                   if        *in30
630600060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
630700060406     C                   MOVEL     'S'           WSLITT
630800060406     c                   z-add     24            wnoadd
630900060406    3C                   ENDIF
631000060406     C*
631100060406    2C                   ENDIF                                                  <-- 2 ---
631200060406    1C                   ENDIF                                                  <-- 1 ---
631300060406     C*
631400060406     C                   ENDSR
631500931210     C********************************************************************
631600931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
631700931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
631800931210     C********************************************************************
631900050805     C***  CHKDCR        BEGSR
632000931210     C*
632100050805     C***  WPOCRI        IFNE      *ZEROS
632200050805     C***  WPOROU        IFGE      WPOCRI
632300050805     C***  WPOROU        ANDLT     WPOCRF
632400050805     C***                ELSE
632500050805     C***                MOVEL     'S'           WSLITT
632600050805     C***                END
632700050805     C***                END
632800931210     C*
632900050805     C***                ENDSR
633000140822     C********************************************************************
633100140822     C*  Reperimento tempi di consegna per decidere se slittare
633200140822     C********************************************************************
633300140822     C     Reper_tempi   BEGSR
633400140822
633500140822     c                   clear                   trulorSds
633600140825     c                   clear                   trulor2ds
633700140822     c                   eval       IOREtfp = d98tfp
633800140822     c                   eval       IOREtfa = d98tfa
633900140825     c                   eval       IOREdti = d98dti
634000140825     c                   eval       IOREhti = d98hti
634100140825     c                   eval       IOREdcr = s98dcr
634200140825     c                   eval       IOREhcr = s98hcr
634300140825     c                   eval       IOREtcr = s98tcr
634400140825     c                   eval       IOREDEI = d98dei
634500140825     c                   eval       IOREOEI = d98oei
634600140825     c                   eval       IOREfil = d98lna
634700140825     c                   eval       IOREcap = d98cad
634800140825     c                   eval       IOREloc = d98lod
634900140825     c                   eval       IOREnar = d98nzd
635000140825     c
635100140825     c* Data di Riferimento:
635200140825     c* Se impostata la consegnabilità --> prendo questa
635300140822     c                   select
635400140825     c                   when      d98dfc>0
635500141107     c                   if        or_datarif<d98dfc and or_datarif>0
635600141107     c                   eval      ioredta=or_datarif
635700141107     c                   else
635800141107     c                   eval      ioredta=d98dfc
635900141107     c                   endif
636000140825     c* altrimenti data di consegna
636100140825     c                   when      d98dcm>0
636200141107     c                   if        or_datarif<d98dcm and or_datarif>0
636300141107     c                   eval      ioredta=or_datarif
636400141107     c                   else
636500140825     c                   eval      ioredta=d98dcm
636600141107     c                   endif
636700140825     c
636800140822     c                   other
636900140825     c* altrimenti data teorica consegna se > = oggi
637000140825     C                   time                    wrkoggiiso
637100140825     C     *iso          move      wrkoggiiso    ioredta
637200140822     c* se < di oggi, imposto oggi
637300140825     c                   if        d98dei>ioredta
637400140825     c                   eval      ioredta=d98dei
637500140822     c                   endif
637600141107     c
637700141107     c                   if        or_datarif<ioredta and or_datarif>0
637800141107     c                   eval      ioredta=or_datarif
637900141107     c                   endif
638000141107     c
638100140822     c                   endsl
638200140822     c
638300140825     c                   eval       IOREtser = 'C'
638400141107     c                   if         or_ddc>0 and or_ddc<d98ddc
638500141107     c                   eval       IOREDDC=or_ddc
638600141107     c                   eval       IOREnDC=8888888
638700141107     c                   eval       IOREcons = 'S'
638800141107     c                   else
638900140825     c                   eval       IOREDDC=d98ddc
639000140825     c                   eval       IOREnDC=wd98ndc
639100140825     c                   eval       IOREcons = d98cons
639200141107     c                   endif
639300141017     c                   eval       IOREtsp = d98tsp
639400140822     c
639500140822     c                   call      'TRULORSR'
639600140822     c                   parm                    kpjba
639700140822     c                   parm                    trulorSds
639800140825     c                   parm                    trulor2ds
639900140825     c
640000140825     c                   eval      wtrulor= 'S'
640100141107     c                   clear                   or_datarif        8 0
640200141107     c                   clear                   or_ddc            8 0
640300140822     C                   ENDSR
640400950406     C********************************************************************
640500950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
640600950406     C********************************************************************
640700950406     C     DATORE        BEGSR
640800950406     C*
640900950406     C                   Z-ADD     *ZEROS        WORE
641000950406     C     WDAT          IFNE      0
641100950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
641200950406     C     WAA           DIV       4             WQUA              4 0
641300950406     C                   MVR                     WRES
641400950406     C     WAA           MULT      365           WGIO              9 0
641500950406     C                   ADD       WQUA          WGIO
641600950406     C                   ADD       WGG           WGIO
641700950406     C     WMM           SUB       1             MC                3 0
641800950406     C                   DO        MC            WX                3 0
641900950406     C                   ADD       MES(WX)       WGIO
642000950406     C                   END
642100950406     C     WRES          IFEQ      0
642200950406     C     WMM           ANDLE     2
642300950406     C                   SUB       1             WGIO
642400950406     C                   END
642500950406     C     WGIO          MULT      24            WORE              9 0
642600950406     C                   ENDIF
642700950406     C*
642800950406     C                   ENDSR
642900930323     C********************************************************************
643000930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
643100930323     C********************************************************************
643200930323     C     OREDAT        BEGSR
643300930323     C*
643400950407     C                   SETOFF                                       717273
643500950407     C                   SETOFF                                       7475
643600930323     C     WORE          DIV       24            WGIO
643700930323     C                   MVR                     WRES              3 0
643800950406     C*
643900950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
644000950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
644100950406     C*  TROVO ANNO
644200950406     C     WGIO          DIV       365           WAA
644300950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
644400950406     C                   MVR                     WREST                71
644500950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
644600950406     C     WGIO          DIV       365           WAA
644700950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
644800950406     C*                                                    0 = 31/12/PR
644900950406     C   75
645000950406     CAN 71              DO                                                     BIS. E NON ERA
645100950406     C     WAA           SUB       1             WAA
645200950406     C                   Z-ADD     365           WREST
645300950406     C                   END
645400950406     C*
645500950406     C     WAA           DIV       4             WNBIS                                      T.
645600950406     C                   MVR                     WRBIS             3 073
645700950406     C   73
645800950406     CANN71                                                                     NON PIU' BISEST
645900950406     CORN73              ADD       1             WREST                          E'BISESTILE
646000950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
646100950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
646200950406     C  N73              SUB       1             $PM(1)
646300950406     C                   Z-ADD     1             WI                2 0
646400950406     C* TROVO IL MESE E GIORNI
646500950406     C     WREST         LOOKUP    $PM(WI)                            74  73
646600950406     C                   MOVE      WI            WMM
646700950406     C     WI            COMP      1                                      72
646800950406     C  N72
646900950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
647000950406     C   74              SUB       1             WI
647100950406     C  N72
647200950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
647300950406     C   72              MOVE      WREST         WGG
647400950406     C                   ADD       1900          WAA
647500950406     C*
647600930323     C*
647700930323     C                   ENDSR
6478009407151507 C********************************************************************
6479009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6480009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6481009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6482009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6483009407151507 C********************************************************************
6484009407151507 C     FINGIO        BEGSR
6485009407151507 C*
6486009407151507 C     WPORE         DIV       24            WGIO
6487009407151507 C                   MVR                     WRES
6488009407151507 C     WRES          IFEQ      *ZEROS
6489009407151507 C                   Z-ADD     12            WNORE             3 0
6490009407151507 C                   ELSE
6491009407151507 C                   Z-ADD     *ZEROS        WNORE
6492009407151507 C                   ENDIF
6493009407151507 C*
6494009407151507 C                   ENDSR
649500930517     C********************************************************************
649600971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
649700930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
649800930517     C********************************************************************
649900930517     C     CERCAL        BEGSR
650000930517     C*
650100930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
650200941227     C                   MOVE      CLNTFP        DCATFP
650300941227     C                   MOVE      CLNTFA        DCATFA
650400941227     C                   MOVE      CLNANN        DCAANN
650500941227     C                   MOVE      CLNMES        DCAMES
650600930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
650700930517     C                   Z-ADD     1             I
650800930517     C     DCALEN        LOOKUP    CAK(I)                                 99
650900930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
651000930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
651100941227     C     KEYCLN        CHAIN     AZCLN01L                           99
651200930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
651300930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
651400930517     C                   MOVEL     '1'           D98ERR
651500930517     C                   MOVEL     MSG(3)        D98MSG
651600930517     C                   ELSE                                                   <-- 2 -->
651700970401     C* CONTROLLA SE ANNULLATO
651800970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
651900970401     C                   MOVEL     '1'           D98ERR
652000970401     C                   MOVEL     MSG(3)        D98MSG
652100970401     C                   ELSE                                                   <-- 3 -->
652200930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
652300930517     C                   ADD       1             WINDCA            6 0
652400970401     C     WINDCA        IFLE      200                                          --- 4 -->
652500970401     C     WINDCA        ANDGT     *ZEROS
652600930517     C                   Z-ADD     WINDCA        I
652700930517     C                   MOVEL     DCALEN        CAK(I)
652800941227     C                   MOVEL     CLNMAT        CAM(I)
652900941227     C                   MOVEL     CLNPOM        CAP(I)
653000970401     C                   END                                                    <-- 4 ---
653100970401     C                   ENDIF                                                  <-- 3 ---
653200930517     C                   END                                                    <-- 2 ---
653300930517     C                   ELSE                                                   <-- 1 -->
653400930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
653500941227     C                   MOVEL     CAM(I)        CLNMAT
653600941227     C                   MOVEL     CAP(I)        CLNPOM
653700930517     C                   END                                                    <-- 1 ---
653800930517     C*
653900930517     C                   ENDSR
654000010821     C********************************************************************
654100010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
654200010821     C********************************************************************
654300010821     C     CHKVCA        BEGSR
654400010821     C*
654500010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
654600010821     C     D98CAS        IFGT      *ZEROS
654700010821     C     D98VCA        ANDEQ     *BLANKS
654800010821     C                   MOVEL     'ITL'         D98VCA
654900010821     C                   ENDIF
655000010822     C*
655100010822     C     D98VCA        IFNE      'ITL'
655200010822     C     D98VCA        ANDNE     'EUR'
655300010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
655400010822     C                   Z-ADD     D98CAS        INPIMP
655500010822     C                   MOVEL     D98VCA        INPDIV
655600010822     C                   EXSR      EXECNV
655700010822     C                   Z-ADD     OUTIMP        D98CAS
655800010822     C                   MOVEL     §GEDCN        D98VCA
655900010822     C                   ENDIF
656000010821     C*
656100010821     C                   ENDSR
656200010821     C********************************************************************
656300010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
656400010821     C********************************************************************
656500010821     C     EXECNV        BEGSR
656600010821     C*
656700010821     C                   CLEAR                   YEURDS
656800010821     C*
656900010821     C                   MOVEL     INPDIV        YECDVI
657000010821     C                   Z-ADD     INPIMP        YECIMI
657100010821     C                   MOVEL     §GEDCN        YECDVO
657200010821     C                   MOVEL     '3'           YECDCO
657300010821     C                   MOVEL     'H'           YECTAR
657400010821     C                   CALL      'YEURCO'
657500010821     C                   PARM                    YEURDS
657600010821     C*
657700010821     C                   Z-ADD     YECIMO        OUTIMP
657800010821     C*
657900010821     C                   ENDSR
658000011211     C*------------------------------------------------------------------------*
658100011211     C* Controllo importo massimo contrassegno.
658200011211     C*------------------------------------------------------------------------*
658300011211     C     REPMAXCSB     BEGSR
658400011211     C*
658500011211     C                   CLEAR                   Trul21DS
658600011211     C*
658700011211     C                   EVAL      I21TSp = d98TSP
658800011211     C                   EVAL      I21NzD = d98NZD
658900011211     C                   EVAL      I21LnP = d98LNP
659000011211     C                   EVAL      I21LnA = d98LNA
659100011211     C                   EVAL      I21Ksc = d98KSC
659200011211     C                   EVAL      I21TiC = d98TIC
659300011211     C                   EVAL      I21Imp = d98CAS
659400011211     C                   EVAL      I21Div = d98VCA
659500011211     C*
659600011211     C                   CALL      'TRUL21R'
659700011211     C                   PARM                    Trul21DS
659800011211     C*
659900011211     C                   ENDSR
660000011211     C*------------------------------------------------------------------------*
660100011211
660200930414     C********************************************************************
660300930414     C*   RUTINZ - ROUTINE INIZIALE
660400930414     C********************************************************************
660500930414     C     RUTINZ        BEGSR
660600930414     C*
660700930414     C* PARAMETRI IN INPUT ALLA ROUTINE
660800930414     C     *ENTRY        PLIST
660900970731     C                   PARM                    DS99
661000941227     C*
661100941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
661200941227     C     KEYBLP        KLIST
661300941227     C                   KFLD                    BLPAAS
661400941227     C                   KFLD                    BLPLNP
661500941227     C                   KFLD                    BLPNRS
661600941227     C                   KFLD                    BLPNSP
661700050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
661800941227     C     KEYGCP        KLIST
661900050405     C                   KFLD                    KWGCPAAS
662000050405     C                   KFLD                    KWGCPLNP
662100050405     C                   KFLD                    KWGCPNRS
662200050405     C                   KFLD                    KWGCPNSP
662300050405     C                   KFLD                    KWGCPFRG
662400050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
662500031125     C     KEYGC_P       KLIST
662600050405     C                   KFLD                    KWGCPAAS
662700050405     C                   KFLD                    KWGCPLNP
662800050405     C                   KFLD                    KWGCPNRS
662900050405     C                   KFLD                    KWGCPNSP
663000970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
663100150120     C**   KEYBLT        KLIST
663200150120     C**                 KFLD                    BLTAAS
663300150120     C**                 KFLD                    BLTLNP
663400150120     C**                 KFLD                    BLTNRS
663500150120     C**                 KFLD                    BLTNSP
663600941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
663700941227     C     KEYARB        KLIST
663800941227     C                   KFLD                    ARBAAS
663900941227     C                   KFLD                    ARBLNP
664000941227     C                   KFLD                    ARBNRS
664100941227     C                   KFLD                    ARBNSP
664200051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
664300941228     C     KEYAR9        KLIST
664400941228     C                   KFLD                    AR9AAS
664500941228     C                   KFLD                    AR9LNP
664600941228     C                   KFLD                    AR9NRS
664700941228     C                   KFLD                    AR9NSP
664800941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
664900050405     C     KEYGCP21      KLIST
665000050405     C                   KFLD                    KWGCPAAS
665100050405     C                   KFLD                    KWGCPLNP
665200050405     C                   KFLD                    KWGCPNRS
665300050405     C                   KFLD                    KWGCPNSP
665400050405     C                   KFLD                    KWGCPFRG
665500970912     C* CHIAVE LETTURA FNART01L - PARZIALE
665600150120     C***  KEYART        KLIST
665700150120     C***                KFLD                    ARTAAS
665800150120     C***                KFLD                    ARTLNP
665900150120     C***                KFLD                    ARTNRS
666000150120     C***                KFLD                    ARTNSP
666100950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
666200950102     C     KEYLBL        KLIST
666300950102     C                   KFLD                    LBLAAN
666400950102     C                   KFLD                    LBLLPN
666500950102     C                   KFLD                    LBLNRN
666600950102     C                   KFLD                    LBLNSN
666700930414     C*
666800070827     c     kAR5          Klist
666900070827     c                   Kfld                    d98Aas
667000070827     c                   Kfld                    d98Lnp
667100070827     c                   Kfld                    d98nrs
667200070827     c                   Kfld                    d98nsp
667300070827     c                   Kfld                    kAr5Trd
667400150325     c     kAR4          Klist
667500150325     c                   Kfld                    d98Aas
667600150325     c                   Kfld                    d98Lnp
667700150325     c                   Kfld                    d98nrs
667800150325     c                   Kfld                    d98nsp
667900150325     c                   Kfld                    kAr4Trc
668000150218     c*
668100150218     c     kart          Klist
668200150218     c                   Kfld                    d98Aas
668300150218     c                   Kfld                    d98Lnp
668400150218     c                   Kfld                    d98nrs
668500150218     c                   Kfld                    d98nsp
668600150218     c     kbrv          Klist
668700150218     c                   Kfld                    artfls
668800150218     c                   Kfld                    artlna
668900150218     c                   Kfld                    artnrs
669000150218     c                   Kfld                    artnsc
669100150325     C     Kfapd         klist
669200150325     C                   kfld                    kpdtip
669300150325     C                   kfld                    kpdpdr
669400930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
669500930414     C     KEYTAB        KLIST
669600930414     C                   KFLD                    KTBUTE
669700930414     C                   KFLD                    KTBCOD
669800930414     C                   KFLD                    KTBKEY
669900060406     C     Ktab          KLIST
670000060406     C                   KFLD                    KTBUTE
670100060406     C                   KFLD                    KTBCOD
670200930414     C*
670300941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
670400941227     C     KEYCLN        KLIST
670500941227     C                   KFLD                    CLNTFP
670600941227     C                   KFLD                    CLNTFA
670700941227     C                   KFLD                    CLNANN
670800941227     C                   KFLD                    CLNMES
670900941227     C*
671000941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
671100941227     C     KEYCAS        KLIST
671200941227     C                   KFLD                    CASPTA
671300941227     C                   KFLD                    CASLNP
671400941227     C                   KFLD                    CASLNA
671500941227     C                   KFLD                    CASTSP
671600941227     C                   KFLD                    CASDDE
671700930420     C*
671800990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
671900930420     C     KEYEVB        KLIST
672000930420     C                   KFLD                    KEBAAS
672100930420     C                   KFLD                    KEBLNP
672200930420     C                   KFLD                    KEBNRS
672300930420     C                   KFLD                    KEBNSP
672400010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
672500010821     C     KTBE          KLIST
672600010821     C                   KFLD                    TBECOD
672700010821     C                   KFLD                    TBEKE1
672800010821     C                   KFLD                    TBEKE2
672900930414     C*
673000010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
673100930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
673200930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
673300930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
673400930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
673500930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
673600930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
673700930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
673800930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
673900930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
674000010821     C     *LIKE         DEFINE    D98CAS        INPIMP
674100010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
674200010821     C     *LIKE         DEFINE    D98VCA        INPDIV
674300930414     C*
674400930414     C* RECUPERO IL CODICE DELL'AZIENDA
674500930414     C                   Z-ADD     1             CODUT             1 0
674600970701     C                   Z-ADD     *ZEROS        I                 6 0
674700930414     C*
674800930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
674900930414     C                   EXSR      AZZERA
675000930414     C*
675100930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
675200941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
675300941227     C                   MOVEL     'S'           CTABSN            1
675400930414     C                   EXSR      CARTAB
675500941227     C                   ENDIF
675600041116     c* Se passato umero di spedizioni apro i files , altrimenti no
675700041116     c                   if        d98nsp>0 and wapri=' '
675800041116     c                   open      fnblp01l
675900050405     c                   open      tigcp01l
676000150120     c***                open      fnblt01l
676100041116     c                   open      fnarb01l
676200051115     c                   open      fiar901l
676300150325     c                   open      fiar401l
676400050405     c                   open      tigcp21l
676500150218     c                   open      fnart01l
676600150218     c                   open      fnbrv07l
676700041116     c                   open      fnlbl01l
676800041116     c                   open      fnevb01l
676900070827     c                   open      fiar501l
677000041116     c                   eval      wapri='S'
677100041116     c                   endif
677200930414     C*
677300930414     C                   ENDSR
677400941227     C*------------------------------------------------------------------------*
677500941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
677600941227     C*------------------------------------------------------------------------*
677700941227     C     SUBDAT        BEGSR
677800941227     C*
677900941227     C                   TIME                    DTTIME
678000941227     C                   Z-ADD     DTANN         DSANN
678100941227     C                   Z-ADD     DTMES         DSMES
678200941227     C                   Z-ADD     DTGIO         DSGIO
678300941227     C                   Z-ADD     DTSEC         DSSEC
678400941227     C*
678500941227     C                   ENDSR
678600930414     C********************************************************************
678700930414     C*   CARTAB - CARICA LE TABELLE
678800930414     C********************************************************************
678900930414     C     CARTAB        BEGSR
679000930414     C*
679100930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
679200930414     C     D98ERR        IFEQ      '0'
679300930421     C                   Z-ADD     0             I
679400930414     C                   Z-ADD     CODUT         KTBUTE
679500930414     C                   MOVEL     '1Y'          KTBCOD
679600930414     C                   MOVEL     *BLANKS       KTBKEY
679700930414     C     KEYTAB        SETLL     TABEL00F                           99
679800930414     C  N99              READ      TABEL00F                               99
679900930414     C     *IN99         IFEQ      '1'
680000930414     C                   MOVEL     '1'           D98ERR
680100930414     C                   MOVEL     MSG(5)        D98MSG
680200930414     C                   END
680300930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
680400930414     C     TBLCOD        ANDEQ     '1Y'
680500930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
680600930421     C                   ADD       1             I
680700930421     C                   MOVEL     TBLKEY        C1Y(I)
680800930414     C                   MOVEL     TBLUNI        DS1Y
680900930421     C                   MOVEL     §1YFEI        I1Y(I)
681000930421     C                   MOVEL     §1YFEC        E1Y(I)
681100930414     C                   END                                                    <-- 3 ---
681200930414     C                   READ      TABEL00F                               99
681300930414     C                   END                                                    <-- 2 ---
681400930414     C                   END
681500930414     C*
681600930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
681700930414     C     D98ERR        IFEQ      '0'
681800930421     C                   Z-ADD     0             I
681900930414     C                   Z-ADD     CODUT         KTBUTE
682000930414     C                   MOVEL     '1X'          KTBCOD
682100930414     C                   MOVEL     *BLANKS       KTBKEY
682200930414     C     KEYTAB        SETLL     TABEL00F                           99
682300930414     C  N99              READ      TABEL00F                               99
682400930414     C     *IN99         IFEQ      '1'
682500930414     C                   MOVEL     '1'           D98ERR
682600930414     C                   MOVEL     MSG(1)        D98MSG
682700930414     C                   END
682800930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
682900930414     C     TBLCOD        ANDEQ     '1X'
683000930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
683100930421     C                   ADD       1             I
683200930421     C                   MOVEL     TBLKEY        C1X(I)
683300930414     C                   MOVEL     TBLUNI        DS1X
683400930421     C                   Z-ADD     §1XHHG        G1X(I)
683500930421     C                   Z-ADD     §1XHHO        O1X(I)
683600930421     C                   Z-ADD     §1XHHM        M1X(I)
683700930421     C                   Z-ADD     §1XHHP        P1X(I)
683800930421     C                   Z-ADD     §1XHHI        I1X(I)
683900930421     C                   Z-ADD     §1XHHS        S1X(I)
684000930414     C                   END                                                    <-- 3 ---
684100930414     C                   READ      TABEL00F                               99
684200931223     C                   ENDDO                                                  <-- 2 ---
684300050523     c
684400050523     c* controllo riempimento skiere
684500050523     c                   clear                   trul0sds
684600050523     c                   eval      i0sski='C1X/G1X/O1X/...'
684700050523     c                   eval      i0sele=%elem(c1x)
684800050523     c                   eval      i0spie=i
684900050523     c                   eval      i0sfile='TABEL00F  '
685000050523     c                   eval      i0scod=' 1X  '
685100050523     c                   eval      i0spgm='TNSD99R'
685200050523     c                   movel     trul0sds      kpjbu
685300050523     c                   call      'TRUL0SR'
685400050523     c                   parm                    kpjba
685500050523     C                   END
685600930421     C*
685700930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
685800930421     C     D98ERR        IFEQ      '0'
685900930421     C                   Z-ADD     0             I
686000930421     C                   Z-ADD     CODUT         KTBUTE
686100930421     C                   MOVEL     '2A'          KTBCOD
686200060406     C     Ktab          SETLL     TABEL00F                           99
686300060406     C  N99ktab          READe     TABEL00F                               99
686400930421     C     *IN99         IFEQ      '1'
686500930421     C                   MOVEL     '1'           D98ERR
686600930421     C                   MOVEL     MSG(8)        D98MSG
686700930421     C                   END
686800930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
686900930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
687000930421     C                   ADD       1             I
687100930421     C                   MOVEL     TBLKEY        C2A(I)
687200060406     C                   MOVEL     TBLUNI        ds2a
687300060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
687400060406     C                   MOVEL     w003a         D2A(I)
687500930421     C                   END                                                    <-- 3 ---
687600060406     C     ktab          READe     TABEL00F                               99
687700931223     C                   ENDDO                                                  <-- 2 ---
687800050523     c* controllo riempimento skiere
687900050523     c                   clear                   trul0sds
688000050523     c                   eval      i0sski='C2A/D2A'
688100050523     c                   eval      i0sele=%elem(c2a)
688200050523     c                   eval      i0spie=i
688300050523     c                   eval      i0sfile='TABEL00F  '
688400060406     c                   eval      i0scod='2A/2Z'
688500050523     c                   eval      i0spgm='TNSD99R'
688600050523     c                   movel     trul0sds      kpjbu
688700050523     c                   call      'TRUL0SR'
688800050523     c                   parm                    kpjba
688900930421     C                   END
689000060406     c* Tabella  2Z
689100060406     C     D98ERR        IFEQ      '0'
689200060406     C                   Z-ADD     CODUT         KTBUTE
689300060406     C                   MOVEL     '2Z'          KTBCOD
689400060406     C     Ktab          SETLL     TABEL00F                           99
689500060406     C  N99ktab          READe     TABEL00F                               99
689600060406     C     *IN99         IFEQ      '1'
689700060406     C                   MOVEL     '1'           D98ERR
689800060406     C                   MOVEL     MSG(33)       D98MSG
689900060406     C                   END
690000060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
690100060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
690200060406     C                   Z-ADD     1             I
690300060406     C                   MOVEL     TBLKEY        w003a
690400060406     c     w003a         lookup    c2a(i)                                 99
690500060406     C     *IN99         IFEQ      *off
690600060406     C                   MOVEL     '1'           D98ERR
690700060406     C                   MOVEL     MSG(34)       D98MSG
690800060406     C                   Else
690900060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
691000060406     C                   END                                                    <-- 3 ---
691100060406     C                   END                                                    <-- 3 ---
691200060406     C     ktab          READe     TABEL00F                               99
691300060406     C                   ENDDO                                                  <-- 2 ---
691400060406     C                   END
691500941227     C*
691600941227     C* CARICA TABELLA CODICI BOLLA -3A-
691700941227     C     D98ERR        IFEQ      '0'
691800941228     C                   Z-ADD     0             I
691900941227     C                   Z-ADD     CODUT         KTBUTE
692000941227     C                   MOVEL     '3A'          KTBCOD
692100941227     C                   MOVEL     *BLANKS       KTBKEY
692200941227     C     KEYTAB        SETLL     TABEL00F                           99
692300941227     C  N99              READ      TABEL00F                               99
692400941227     C     *IN99         IFEQ      '1'
692500941227     C                   MOVEL     '1'           D98ERR
692600941227     C                   MOVEL     MSG(22)       D98MSG
692700941227     C                   END
692800941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
692900941227     C     TBLCOD        ANDEQ     '3A'
693000941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
693100941227     C                   ADD       1             I
693200941227     C                   MOVEL     TBLKEY        C3A(I)
693300941227     C                   MOVEL     TBLUNI        DS3A
693400941227     C                   MOVEL     §3ATB1        T3A(I)
693500941228     C                   Z-ADD     §3AFCA        F3A(I)
693600990308     C                   MOVEL     §3ARBL        R3A(I)
693700941227     C                   END                                                    <-- 3 ---
693800941227     C                   READ      TABEL00F                               99
693900941227     C                   END                                                    <-- 2 ---
694000050523     c* controllo riempimento skiere
694100050523     c                   clear                   trul0sds
694200050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
694300050523     c                   eval      i0sele=%elem(c3a)
694400050523     c                   eval      i0spie=i
694500050523     c                   eval      i0sfile='TABEL00F  '
694600050523     c                   eval      i0scod=' 3A  '
694700050523     c                   eval      i0spgm='TNSD99R'
694800050523     c                   movel     trul0sds      kpjbu
694900050523     c                   call      'TRUL0SR'
695000050523     c                   parm                    kpjba
695100941227     C                   END
695200971210     C*
695300020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
695400020226     C                   Z-ADD     *ZEROS        J                 3 0
695500971210IF  0C     D98ERR        IFEQ      '0'
695600971210     C     *LOVAL        SETLL     AZORG01L
695700971210     C                   READ      AZORG01L                               98
695800971210DO  1C     *IN98         DOWEQ     *OFF
695900971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
696000971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
696100971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
696200020627     C                   MOVEL     ORGDE3        OG143
696300020702     C*
696400020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
696500020702     C                   CLEAR                   DNTW
696600020702     C*
696700020702     C                   MOVEL(P)  'NTW'         TBECOD
696800020702     C                   MOVEL(P)  §OGNTW        TBEKE1
696900020702     C     KTBE          CHAIN     TNTBE01L
697000020702     C                   IF        %found(TNTBE01L)
697100020702     C                   MOVEL     TBEUNI        DNTW
697200020702     C                   ENDIF
697300020702     C*
697400020702     C                   MOVEL     §NTWFIE       FL1(F)
697500020627     C*
697600020226     C* VERIFICO SE FILIALE FEDEX
697700020226     C                   IF        §OGNTW = 'FED'
697800020226     C                   ADD       1             J
697900020226     C                   Z-ADD     ORGFIL        FILFED(J)
698000020226     C                   ENDIF
698100020226     C*
698200971210E   2C                   ENDIF
698300971210     C                   READ      AZORG01L                               98
698400971210E   1C                   ENDDO
698500971210E   0C                   ENDIF
698600010821     C*
698700010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
698800010821     C                   MOVEL     'GED'         TBECOD
698900070514     C                   MOVEL     *BLANKS       TBEKE1
699000070514     C                   MOVEL     '1'           TBEKE1
699100010821     C                   MOVEL     *BLANKS       TBEKE2
699200010821     C     KTBE          CHAIN     TNTBE01L                           21
699300070514     C  N21              MOVEL     TBEUNI        DGED
699400150326     c
699500150326     C* REPERISCO LA tabella flag operativi per tabella OLP
699600150326     c                   clear                   dvpo
699700150326     C                   MOVEL     'VPO'         TBECOD
699800150326     C                   MOVEL     *BLANKS       TBEKE2
699900150326     C                   MOVEL(P)  'VPO'         TBEKE1
700000150326     C     KTBE          CHAIN     TNTBE01L                           21
700100150326     C  N21              MOVEL     TBEUNI        DVPO
700200150326     C*
700300031201     C*
700400031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
700500031201     C     D98ERR        IFEQ      '0'
700600060403     C                   Z-ADD     0             I7QW              3 0
700700031201     C                   Z-ADD     CODUT         KTBUTE
700800031201     C                   MOVEL     '7Q'          KTBCOD
700900031201     C                   MOVEL     *BLANKS       KTBKEY
701000031201     C     KEYTAB        SETLL     TABEL00F                           99
701100031201     C  N99              READ      TABEL00F                               99
701200031201     C     *IN99         IFEQ      '1'
701300031201     C                   MOVEL     '1'           D98ERR
701400031201     C                   MOVEL     MSG(8)        D98MSG
701500031201     C                   ENDIF
701600031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
701700031201     C     TBLCOD        ANDEQ     '7Q'
701800031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
701900031201     C                   MOVEL     TBLUNI        DS7Q
702000031201     C                   IF        §7Q4D1 = 'R' OR
702100031201     C                             §7Q4D2 = 'R' OR
702200031201     C                             §7Q4D3 = 'R' OR
702300031201     C                             §7Q4D4 = 'R' OR
702400031201     C                             §7Q4D5 = 'R'
702500031201     C                   ADD       1             I7QW
702600031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
702700031201     C                   ENDIF                                                  <-- 3 ---
702800031201     C                   ENDIF                                                  <-- 3 ---
702900031201     C                   READ      TABEL00F                               99
703000031201     C                   ENDDO                                                  <-- 2 ---
703100031201     C                   ENDIF
703200031201     C*
703300031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
703400031201     C     D98ERR        IFEQ      '0'
703500031201     C                   Z-ADD     CODUT         KTBUTE
703600031201     C                   MOVEL     '7W'          KTBCOD
703700031201     C                   MOVEL     *BLANKS       KTBKEY
703800031201     C     KEYTAB        SETLL     TABEL00F                           99
703900031201     C  N99              READ      TABEL00F                               99
704000031201     C     *IN99         IFEQ      '1'
704100031201     C                   MOVEL     '1'           D98ERR
704200031201     C                   MOVEL     MSG(8)        D98MSG
704300031201     C                   ENDIF
704400031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
704500031201     C     TBLCOD        ANDEQ     '7W'
704600031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
704700031201     C                   MOVEL     TBLUNI        DS7W
704800031201     C                   IF        §7W6D1 = 'C' OR
704900031201     C                             §7W6D2 = 'C' OR
705000031201     C                             §7W6D3 = 'C' OR
705100031201     C                             §7W6D4 = 'C' OR
705200031201     C                             §7W6D5 = 'C'
705300031201     C                   ADD       1             I7QW
705400031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
705500031201     C                   ENDIF                                                  <-- 3 ---
705600031201     C                   ENDIF                                                  <-- 3 ---
705700031201     C                   READ      TABEL00F                               99
705800031201     C                   ENDDO                                                  <-- 2 ---
705900050523     c* controllo riempimento skiere
706000050523     c                   clear                   trul0sds
706100050523     c                   eval      i0sski='SK7QW'
706200050523     c                   eval      i0sele=%elem(sk7qw)
706300050523     c                   eval      i0spie=i7qw
706400050523     c                   eval      i0sfile='TABEL00F  '
706500050523     c                   eval      i0scod='7Q-7W'
706600050523     c                   eval      i0spgm='TNSD99R'
706700050523     c                   movel     trul0sds      kpjbu
706800050523     c                   call      'TRUL0SR'
706900050523     c                   parm                    kpjba
707000031201     C                   ENDIF
707100930414     C*
707200930414     C                   ENDSR
707300930414     C********************************************************************
707400930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
707500930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
707600930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
707700930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
707800930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
707900930414     C********************************************************************
708000930414     C     RO3ORE        BEGSR
708100930414     C*
708200930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
708300930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
708400930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
708500050902     c                   movel     ' '           WR3MATPOM         1
708600931209     C                   Z-ADD     *ZEROS        WR3HHO
708700931209     C                   Z-ADD     *ZEROS        WR3HHA
708800930414     C                   MOVE      WR3TAB        WR3COD
708900930604     C                   MOVEL     D98LNA        WR3COD            8
709000930421     C                   Z-ADD     1             I
709100930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
709200930414     C     *IN99         IFEQ      '0'
709300930414     C                   MOVE      WR3TAB        WR3COD
709400930414     C                   MOVEL     '   '         WR3COD            8
709500930421     C                   Z-ADD     1             I
709600930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
709700930414     C                   END
709800930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
709900930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
710000930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
710100930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
710200930414     C                   ELSE                                                   <-- 2 -->
710300930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
710400930421     C                   Z-ADD     M1X(I)        WR3HHO
710500930510     C                   Z-ADD     I1X(I)        WR3HHA
710600050902     c                   movel     'M'           WR3MATPOM         1
710700930414     C                   END
710800930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
710900930421     C                   Z-ADD     P1X(I)        WR3HHO
711000930510     C                   Z-ADD     S1X(I)        WR3HHA
711100050902     c                   movel     'P'           WR3MATPOM
711200930414     C                   END
711300930414     C                   END                                                    <-- 2 ---
711400930414     C                   END                                                    <-- 1 ---
711500930414     C*
711600930414     C                   ENDSR
711700930423     C********************************************************************
711800930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
711900930423     C********************************************************************
712000930423     C     W98D98        BEGSR
712100930423     C*
712200941228     C*
712300941228     C* ORE REALI PARTENZA INTERNE
712400941228     C                   Z-ADD     W98RPI        W98               3 0
712500941228     C                   EXSR      W98999
712600941228     C                   Z-ADD     D98           D98RPI
712700941228     C* ORE REALI PARTENZA CLIENTE
712800941228     C                   Z-ADD     W98RPC        W98
712900941228     C                   EXSR      W98999
713000941228     C                   Z-ADD     D98           D98RPC
713100930423     C* ORE REALI TRAZIONE INTERNE
713200941228     C                   Z-ADD     W98RTI        W98
713300941228     C                   EXSR      W98999
713400941228     C                   Z-ADD     D98           D98RTI
713500930423     C* ORE REALI TRAZIONE CLIENTE
713600941228     C                   Z-ADD     W98RTC        W98
713700941228     C                   EXSR      W98999
713800941228     C                   Z-ADD     D98           D98RTC
713900930423     C* ORE REALI CONSEGNA INTERNE
714000941228     C                   Z-ADD     W98RCI        W98
714100941228     C                   EXSR      W98999
714200941228     C                   Z-ADD     D98           D98RCI
714300930423     C* ORE REALI CONSEGNA CLIENTE
714400941228     C                   Z-ADD     W98RCC        W98
714500941228     C                   EXSR      W98999
714600941228     C                   Z-ADD     D98           D98RCC
714700930423     C* ORE REALI RESA INTERNE
714800941228     C                   Z-ADD     W98RRI        W98
714900941228     C                   EXSR      W98999
715000941228     C                   Z-ADD     D98           D98RRI
715100930423     C* ORE REALI RESA CLIENTE
715200941228     C                   Z-ADD     W98RRC        W98
715300941228     C                   EXSR      W98999
715400941228     C                   Z-ADD     D98           D98RRC
715500941228     C* ORE DIFF. PARTENZA INTERNE
715600941228     C                   Z-ADD     W98FPI        W98
715700941228     C                   EXSR      W98999
715800941228     C                   Z-ADD     D98           D98FPI
715900941228     C* ORE DIFF. PARTENZA CLIENTE
716000941228     C                   Z-ADD     W98FPC        W98
716100941228     C                   EXSR      W98999
716200941228     C                   Z-ADD     D98           D98FPC
716300930423     C* ORE DIFF. TRAZIONE INTERNE
716400941228     C                   Z-ADD     W98FTI        W98
716500941228     C                   EXSR      W98999
716600941228     C                   Z-ADD     D98           D98FTI
716700930423     C* ORE DIFF. TRAZIONE CLIENTE
716800941228     C                   Z-ADD     W98FTC        W98
716900941228     C                   EXSR      W98999
717000941228     C                   Z-ADD     D98           D98FTC
717100930423     C* ORE DIFF. CONSEGNA INTERNE
717200941228     C                   Z-ADD     W98FCI        W98
717300941228     C                   EXSR      W98999
717400941228     C                   Z-ADD     D98           D98FCI
717500930423     C* ORE DIFF. CONSEGNA CLIENTE
717600941228     C                   Z-ADD     W98FCC        W98
717700941228     C                   EXSR      W98999
717800941228     C                   Z-ADD     D98           D98FCC
717900930423     C* ORE DIFF. RESA INTERNE
718000941228     C                   Z-ADD     W98FRI        W98
718100941228     C                   EXSR      W98999
718200941228     C                   Z-ADD     D98           D98FRI
718300930423     C* ORE DIFF. RESA CLIENTE
718400941228     C                   Z-ADD     W98FRC        W98
718500941228     C                   EXSR      W98999
718600941228     C                   Z-ADD     D98           D98FRC
718700941228     C* ORE NON CONF. PARTENZA INTERNE
718800941228     C     W98NPI        IFGT      999
718900941228     C                   Z-ADD     999           D98NPI
719000941228     C                   ELSE
719100941228     C                   Z-ADD     W98NPI        D98NPI
719200941228     C                   END
719300941228     C* ORE NON CONF. PARTENZA CLIENTE
719400941228     C     W98NPC        IFGT      999
719500941228     C                   Z-ADD     999           D98NPC
719600941228     C                   ELSE
719700941228     C                   Z-ADD     W98NPC        D98NPC
719800941228     C                   END
719900930423     C* ORE NON CONF. TRAZIONE INTERNE
720000930423     C     W98NTI        IFGT      999
720100930423     C                   Z-ADD     999           D98NTI
720200930423     C                   ELSE
720300930423     C                   Z-ADD     W98NTI        D98NTI
720400930423     C                   END
720500930423     C* ORE NON CONF. TRAZIONE CLIENTE
720600930423     C     W98NTC        IFGT      999
720700930423     C                   Z-ADD     999           D98NTC
720800930423     C                   ELSE
720900930423     C                   Z-ADD     W98NTC        D98NTC
721000930423     C                   END
721100930423     C* ORE NON CONF. CONSEGNA INTERNE
721200930423     C     W98NCI        IFGT      999
721300930423     C                   Z-ADD     999           D98NCI
721400930423     C                   ELSE
721500930423     C                   Z-ADD     W98NCI        D98NCI
721600930423     C                   END
721700930423     C* ORE NON CONF. CONSEGNA CLIENTE
721800930423     C     W98NCC        IFGT      999
721900930423     C                   Z-ADD     999           D98NCC
722000930423     C                   ELSE
722100930423     C                   Z-ADD     W98NCC        D98NCC
722200930423     C                   END
722300930423     C* ORE NON CONF. RESA INTERNE
722400930423     C     W98NRI        IFGT      999
722500930423     C                   Z-ADD     999           D98NRI
722600930423     C                   ELSE
722700930423     C                   Z-ADD     W98NRI        D98NRI
722800930423     C                   END
722900930423     C* ORE NON CONF. RESA CLIENTE
723000930423     C     W98NRC        IFGT      999
723100930423     C                   Z-ADD     999           D98NRC
723200930423     C                   ELSE
723300930423     C                   Z-ADD     W98NRC        D98NRC
723400930423     C                   END
723500930423     C* ORE X TURNO CHIUSURA INTERNE
723600941228     C                   Z-ADD     W98HHI        W98
723700941228     C                   EXSR      W98999
723800941228     C                   Z-ADD     D98           D98HHI
723900930423     C* ORE X TURNO CHIUSURA CLIENTE
724000941228     C                   Z-ADD     W98HHC        W98
724100941228     C                   EXSR      W98999
724200941228     C                   Z-ADD     D98           D98HHC
724300060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
724400060418     C                   Z-ADD     W98EGI        W98
724500060418     C                   EXSR      W98999
724600060418     C                   Z-ADD     D98           D98hEGI
724700060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
724800060418     C                   Z-ADD     W98EGC        W98
724900060418     C                   EXSR      W98999
725000060418     C                   Z-ADD     D98           D98hEGC
725100060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
725200060420     C                   Z-ADD     W98EAI        W98
725300060420     C                   EXSR      W98999
725400060420     C                   Z-ADD     D98           D98hEAI
725500060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
725600060420     C                   Z-ADD     W98EAC        W98
725700060420     C                   EXSR      W98999
725800060420     C                   Z-ADD     D98           D98hEAC
725900930423     C* ORE X LASC. AVVISO INTERNE
726000941228     C                   Z-ADD     W98HLI        W98
726100941228     C                   EXSR      W98999
726200941228     C                   Z-ADD     D98           D98HLI
726300930423     C* ORE X LASC. AVVISO CLIENTE
726400941228     C                   Z-ADD     W98HLC        W98
726500941228     C                   EXSR      W98999
726600941228     C                   Z-ADD     D98           D98HLC
726700930423     C* ORE X GIACENZA INTERNE
726800941228     C                   Z-ADD     W98HGI        W98
726900941228     C                   EXSR      W98999
727000941228     C                   Z-ADD     D98           D98HGI
727100930423     C* ORE X GIACENZA CLIENTE
727200941228     C                   Z-ADD     W98HGC        W98
727300941228     C                   EXSR      W98999
727400941228     C                   Z-ADD     D98           D98HGC
727500941228     C* ORE X FEST. PART. INTERNE
727600941228     C                   Z-ADD     W98HPI        W98
727700941228     C                   EXSR      W98999
727800941228     C                   Z-ADD     D98           D98HPI
727900941228     C* ORE X FEST. PART. CLIENTE
728000941228     C                   Z-ADD     W98HPC        W98
728100941228     C                   EXSR      W98999
728200941228     C                   Z-ADD     D98           D98HPC
728300930423     C* ORE X FEST. TRAZ. INTERNE
728400941228     C                   Z-ADD     W98HFI        W98
728500941228     C                   EXSR      W98999
728600941228     C                   Z-ADD     D98           D98HFI
728700930423     C* ORE X FEST. TRAZ. CLIENTE
728800941228     C                   Z-ADD     W98HFC        W98
728900941228     C                   EXSR      W98999
729000941228     C                   Z-ADD     D98           D98HFC
729100930423     C* ORE X FEST. CONS. INTERNE
729200941228     C                   Z-ADD     W98HEI        W98
729300941228     C                   EXSR      W98999
729400941228     C                   Z-ADD     D98           D98HEI
729500930423     C* ORE X FEST. CONS. CLIENTE
729600941228     C                   Z-ADD     W98HEC        W98
729700941228     C                   EXSR      W98999
729800941228     C                   Z-ADD     D98           D98HEC
729900930511     C* ORE X F.DEPOSITO INTERNE
730000941228     C                   Z-ADD     W98HDI        W98
730100941228     C                   EXSR      W98999
730200941228     C                   Z-ADD     D98           D98HDI
730300930511     C* ORE X F.DEPOSITO CLIENTE
730400941228     C                   Z-ADD     W98HDC        W98
730500941228     C                   EXSR      W98999
730600941228     C                   Z-ADD     D98           D98HDC
730700930511     C* ORE X CONTRASSEGNO INTERNE
730800941228     C                   Z-ADD     W98HOI        W98
730900941228     C                   EXSR      W98999
731000941228     C                   Z-ADD     D98           D98HOI
731100930511     C* ORE X CONTRASSEGNO CLIENTE
731200941228     C                   Z-ADD     W98HOC        W98
731300941228     C                   EXSR      W98999
731400941228     C                   Z-ADD     D98           D98HOC
731500930914     C* ORE DI CONSEGNABILITA' INTERNE
731600941228     C                   Z-ADD     W98OCI        W98
731700941228     C                   EXSR      W98000
731800941228     C                   Z-ADD     D98           D98OCI
731900930914     C* ORE DI CONSEGNABILITA' CLIENTE
732000941228     C                   Z-ADD     W98OCC        W98
732100941228     C                   EXSR      W98000
732200941228     C                   Z-ADD     D98           D98OCC
732300071122     C
732400071122     c* Per i codici bolla M prepagati annullati, azzero tutto
732500071122     c                   if        d98cbo='M '
732600071122     c                   clear                   d98rpi
732700071122     c                   clear                   d98rpc
732800071122     c                   clear                   d98rti
732900071122     c                   clear                   d98rtc
733000071122     c                   clear                   d98rci
733100071122     c                   clear                   d98rcc
733200071122     c                   clear                   d98rri
733300071122     c                   clear                   d98rrc
733400071122     c                   clear                   d98fpi
733500071122     c                   clear                   d98fpc
733600071122     c                   clear                   d98fti
733700071122     c                   clear                   d98ftc
733800071122     c                   clear                   d98fci
733900071122     c                   clear                   d98fcc
734000071122     c                   clear                   d98fri
734100071122     c                   clear                   d98frc
734200071122     c                   clear                   d98npi
734300071122     c                   clear                   d98npc
734400071122     c                   clear                   d98nti
734500071122     c                   clear                   d98ntc
734600071122     c                   clear                   d98nci
734700071122     c                   clear                   d98ncc
734800071122     c                   clear                   d98nri
734900071122     c                   clear                   d98nrc
735000071122     c                   clear                   d98OCi
735100071122     c                   clear                   d98OCC
735200071122     c                   endif
735300930423     C*
735400100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
735500100308     c*  calcolo
735600100308     c                   eval      d98dcr=s98dcr
735700100308     c                   eval      d98hcr=s98hcr
735800100308     c                   eval      d98tcr=s98tcr
735900100308     c
736000930423     C                   ENDSR
736100941228     C***********************************************************************
736200941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
736300941228     C***********************************************************************
736400941228     C     W98999        BEGSR
736500941228     C     W98           IFLT      -999
736600941228     C                   Z-ADD     -999          D98               3 0
736700941228     C                   ELSE
736800941228     C     W98           IFGT      999
736900941228     C                   Z-ADD     999           D98
737000941228     C                   ELSE
737100941228     C                   Z-ADD     W98           D98
737200941228     C                   END
737300941228     C                   END
737400941228     C                   ENDSR
737500941228     C***********************************************************************
737600941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
737700941228     C***********************************************************************
737800941228     C     W98000        BEGSR
737900941228     C     W98           IFLE      *ZEROS
738000941228     C                   Z-ADD     *ZEROS        D98
738100941228     C                   ELSE
738200941228     C     W98           IFGT      999
738300941228     C                   Z-ADD     999           D98
738400941228     C                   ELSE
738500941228     C                   Z-ADD     W98           D98
738600941228     C                   END
738700941228     C                   END
738800941228     C                   ENDSR
738900930305     C***********************************************************************
739000930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
739100930305     C***********************************************************************
739200930305     C     AZZERA        BEGSR
739300150219     c* data e ora del momento
739400150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
739500150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
739600150219     C                   Z-ADD     DTORA4        WHHODI            4 0
739700930305     C*
739800141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
739900150603     C                   MOVEL     'N'           opendan
740000141107     c                   clear                   or_ddc
740100141108     c                   clear                   sav_12dat         8 0
740200141107     c                   clear                   or_datarif
740300140402     c                   clear                   wpick2            1
740400140402     c                   clear                   Primotipo
740500090928     c                   clear                   Primodev
740600090928     c                   clear                   Primohev
740700070827     c                   clear                   CallAR5
740800071122     c                   clear                   d98cbo
740900100308     c                   clear                   s98dcr
741000100308     c                   clear                   s98hcr
741100100308     c                   clear                   s98tcr
741200141106     c                   clear                   nosomma           1
741300141229     c                   clear                   wtrasm
741400150324     c                   clear                   avvcmc
741500150325     c                   clear                   DSbl4L
741600150401     c                   clear                   fnlv82ds
741700150331     c                   clear                   DS7R
741800150605     c                   clear                   wDEE              1
741900930323     C                   MOVEL     '0'           D98ERR
742000930323     C                   MOVEL     *BLANKS       D98MSG
742100931124     C                   Z-ADD     *ZEROS        D98TPI
742200931124     C                   Z-ADD     *ZEROS        D98TPC
742300930325     C                   Z-ADD     *ZEROS        D98TTI
742400930325     C                   Z-ADD     *ZEROS        D98TTC
742500930325     C                   Z-ADD     *ZEROS        D98TCI
742600930325     C                   Z-ADD     *ZEROS        D98TCC
742700090827     C                   Z-ADD     *ZEROS        w99TCI
742800090827     C                   Z-ADD     *ZEROS        w99TCC
742900930325     C                   Z-ADD     *ZEROS        D98TRI
743000930325     C                   Z-ADD     *ZEROS        D98TRC
743100090921     C                   Z-ADD     *ZEROS        w99TRI
743200090921     C                   Z-ADD     *ZEROS        w99TRC
743300931124     C                   Z-ADD     *ZEROS        D98RPI
743400931124     C                   Z-ADD     *ZEROS        D98RPC
743500930405     C                   Z-ADD     *ZEROS        D98RTI
743600930405     C                   Z-ADD     *ZEROS        D98RTC
743700930405     C                   Z-ADD     *ZEROS        D98RCI
743800930405     C                   Z-ADD     *ZEROS        D98RCC
743900930405     C                   Z-ADD     *ZEROS        D98RRI
744000930405     C                   Z-ADD     *ZEROS        D98RRC
744100931124     C                   Z-ADD     *ZEROS        D98FPI
744200931124     C                   Z-ADD     *ZEROS        D98FPC
744300930325     C                   Z-ADD     *ZEROS        D98FTI
744400930325     C                   Z-ADD     *ZEROS        D98FTC
744500930325     C                   Z-ADD     *ZEROS        D98FCI
744600930325     C                   Z-ADD     *ZEROS        D98FCC
744700930325     C                   Z-ADD     *ZEROS        D98FRI
744800930325     C                   Z-ADD     *ZEROS        D98FRC
744900931124     C                   MOVEL     *BLANKS       D98CPI
745000931124     C                   MOVEL     *BLANKS       D98CPC
745100930325     C                   MOVEL     *BLANKS       D98CTI
745200930325     C                   MOVEL     *BLANKS       D98CTC
745300930325     C                   MOVEL     *BLANKS       D98CCI
745400930325     C                   MOVEL     *BLANKS       D98CCC
745500930325     C                   MOVEL     *BLANKS       D98CRI
745600930325     C                   MOVEL     *BLANKS       D98CRC
745700931124     C                   Z-ADD     *ZEROS        D98NPI
745800931124     C                   Z-ADD     *ZEROS        D98NPC
745900930325     C                   Z-ADD     *ZEROS        D98NTI
746000930325     C                   Z-ADD     *ZEROS        D98NTC
746100930325     C                   Z-ADD     *ZEROS        D98NCI
746200930325     C                   Z-ADD     *ZEROS        D98NCC
746300930325     C                   Z-ADD     *ZEROS        D98NRI
746400930325     C                   Z-ADD     *ZEROS        D98NRC
746500930414     C                   Z-ADD     *ZEROS        D98HHI
746600930414     C                   Z-ADD     *ZEROS        D98HHC
746700930414     C                   Z-ADD     *ZEROS        D98HOI
746800930414     C                   Z-ADD     *ZEROS        D98HOC
746900930414     C                   Z-ADD     *ZEROS        D98HLI
747000930414     C                   Z-ADD     *ZEROS        D98HLC
747100930414     C                   Z-ADD     *ZEROS        D98HGI
747200930414     C                   Z-ADD     *ZEROS        D98HGC
747300931124     C                   Z-ADD     *ZEROS        D98HPI
747400931124     C                   Z-ADD     *ZEROS        D98HPC
747500930414     C                   Z-ADD     *ZEROS        D98HFI
747600930414     C                   Z-ADD     *ZEROS        D98HFC
747700930416     C                   Z-ADD     *ZEROS        D98HEI
747800930416     C                   Z-ADD     *ZEROS        D98HEC
747900930414     C                   Z-ADD     *ZEROS        D98HRI
748000930414     C                   Z-ADD     *ZEROS        D98HRC
748100930511     C                   Z-ADD     *ZEROS        D98HDI
748200930511     C                   Z-ADD     *ZEROS        D98HDC
748300930325     C                   Z-ADD     *ZEROS        D98DCI
748400930325     C                   Z-ADD     *ZEROS        D98HCI
748500930325     C                   Z-ADD     *ZEROS        D98DCE
748600930325     C                   Z-ADD     *ZEROS        D98HCE
748700931125     C                   Z-ADD     *ZEROS        D98OCI
748800931125     C                   Z-ADD     *ZEROS        D98OCC
748900931202     C                   Z-ADD     *ZEROS        D98DEI
749000931202     C                   Z-ADD     *ZEROS        D98OEI
749100931202     C                   Z-ADD     *ZEROS        D98DEE
749200931202     C                   Z-ADD     *ZEROS        D98OEE
749300150325     C                   clear                   D98dtctd
749400150326     C                   clear                   D98picklim
749500150325     C                   clear                   D98spcdee
749600150331     C                   clear                   fddata
749700140825     c
749800141017     c                   clear                   d98ddc
749900141017     c                   clear                   d98cons
750000141017     c                   clear                   wd98ndc
750100140910     c                   clear                   wd98fbc
750200150119     c                   clear                   wd98acc
750300140825     c                   clear                   wtrulor
750400150219     c
750500150219     c                   clear                   IMPdata
750600150219     c                   clear                   IMPfgs
750700150219     c                   clear                   DISdata
750800150219     c                   clear                   DISfgs
750900150219     c                   clear                   DISora
751000150219     c                   clear                   DISnpg
751100150219     c                   clear                   IDnpg
751200150219     c                   clear                   IDdata
751300150219     c                   clear                   IDora
751400150219     c                   clear                   IDfgs
751500931125     C* CAMPI VARI
751600931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
751700140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
751800140825      *   DALLA LETTURA DI QUESTI FILE                               *
751900931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
752000931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
752100931125     C     D98NSP        IFNE      *ZEROS
752200930525     C                   MOVEL     *BLANKS       D98TSP
752300941228     C                   MOVEL     *BLANKS       D98CAD
752400970728     C                   MOVEL     *BLANKS       D98LOD
752500930525     C                   Z-ADD     *ZEROS        D98TFP
752600930525     C                   Z-ADD     *ZEROS        D98TFA
752700930525     C                   Z-ADD     *ZEROS        D98LNA
752800930525     C                   Z-ADD     *ZEROS        D98DSP
752900941228     C                   Z-ADD     *ZEROS        D98MGS
753000930525     C                   Z-ADD     *ZEROS        D98DTI
753100930525     C                   Z-ADD     *ZEROS        D98HTI
753200930525     C                   Z-ADD     *ZEROS        D98DCM
753300930525     C                   Z-ADD     *ZEROS        D98HMC
753400930525     C                   MOVEL     *BLANKS       D98GC1
753500930525     C                   MOVEL     *BLANKS       D98GC2
753600010820     C                   Z-ADD     *ZEROS        D98CAS
753700010821     C                   MOVEL     *BLANKS       D98VCA
753800930525     C                   MOVEL     *BLANKS       D98TIC
753900930525     C                   Z-ADD     *ZEROS        D98DCR
754000930525     C                   Z-ADD     *ZEROS        D98HCR
754100930525     C                   MOVEL     *BLANKS       D98TCR
754200050914     C                   MOVEL     *BLANKS       Consideraora      1
754300930525     C                   MOVEL     *BLANKS       D98CMC
754400930525     C                   Z-ADD     *ZEROS        D98DAG
754500930525     C                   Z-ADD     *ZEROS        D98DCG
754600930525     C                   MOVEL     *BLANKS       D98FFD
754700930525     C                   Z-ADD     *ZEROS        D98NCD
754800930525     C                   MOVEL     *BLANKS       D98FNS
754900931124     C                   Z-ADD     *ZEROS        D98DAM
755000940628     C                   MOVEL     *BLANKS       D98FIN
755100940628     C                   Z-ADD     *ZEROS        D98KSC
755200940628     C                   MOVEL     *BLANKS       D98TBL
755300931125     C                   END
755400960229     C     D98TLA        IFEQ      'T'
755500960229     C                   Z-ADD     *ZEROS        D98DTI
755600960229     C                   Z-ADD     *ZEROS        D98HTI
755700960229     C                   ENDIF
755800931125     C* CAMPI WORK
755900930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
756000930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
756100150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
756200930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
756300941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
756400960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
756500930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
756600930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
756700940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
756800930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
756900150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
757000931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
757100931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
757200150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
757300930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
757400930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
757500930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
757600930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
757700930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
757800940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
757900941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
758000930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
758100050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
758200050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
758300140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
758400930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
758500930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
758600930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
758700930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
758800930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
758900930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
759000930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
759100930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
759200930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
759300930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
759400930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
759500930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
759600930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
759700930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
759800140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
759900931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
760000060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
760100960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
760200931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
760300140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
760400140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
760500150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
760600140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
760700941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
760800970304     C******************** Z-ADD*ZEROS    W98DBR  80
760900970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
761000150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
761100050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
761200930414     C*
761300941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
761400941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
761500930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
761600930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
761700930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
761800930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
761900930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
762000930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
762100941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
762200941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
762300930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
762400930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
762500930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
762600930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
762700930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
762800930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
762900941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
763000941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
763100930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
763200930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
763300930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
763400930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
763500930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
763600930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
763700930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
763800930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
763900060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
764000060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
764100060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
764200060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
764300930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
764400930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
764500930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
764600930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
764700930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
764800930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
764900930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
765000930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
765100930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
765200930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
765300930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
765400930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
765500930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
765600930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
765700941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
765800941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
765900930423     C*
766000930511     C                   Z-ADD     *ZEROS        W98HF             5 0
766100930423     C                   Z-ADD     *ZEROS        W98HH             5 0
766200060418     C                   Z-ADD     *ZEROS        W98EG             5 0
766300060420     C                   Z-ADD     *ZEROS        W98EA             5 0
766400930423     C                   Z-ADD     *ZEROS        W98HG             5 0
766500930423     C                   Z-ADD     *ZEROS        W98HL             5 0
766600930423     C                   Z-ADD     *ZEROS        W98HO             5 0
766700930511     C                   Z-ADD     *ZEROS        W98HD             5 0
766800930611     C*
766900930611     C                   Z-ADD     *ZEROS        WORELA            3 0
767000960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
767100930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
767200930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
767300931207     C                   Z-ADD     *ZEROS        WORECR            3 0
767400090902     c
767500090902     c                   clear                   wmancaTRAZ
767600150113     c                   clear                   wcaldpcee         1
767700150220     c                   clear                   evemic            2
767800930305     C*
767900060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
768000060406     C                   clear                   LAF                            *LASC.AVVISO FINE
768100091014     C                   clear                   LAp                            *posizione l.avviso
768200060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
768300060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
768400060406c    c                   clear                   LCon
768500060406     C                   clear                   EVD                            *DATA EVENTO
768600060406     C                   clear                   EVO                            *ORA EVENTO
768700060406     C                   clear                   EVC                            *CAUSALE EVENTO
768800131218     C                   clear                   EVT
768900060406     C                   clear                   pVD                            *DATA EVENTO
769000060406     C                   clear                   pVO                            *ORA EVENTO
769100060406     C                   clear                   pVC                            *CAUSALE EVENTO
769200060406     C                   clear                   pVF                            *CAUSALE EVENTO
769300060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
769400060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
769500060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
769600060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
769700060406     C                   clear                   COV                            *DATE FINE L.A. E GI
769800060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
769900060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
770000091014     C                   clear                   GIp                            *posizione vento gia
770100060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
770200060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
770300060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
770400931213     C*
770500060406     C                   clear                   TUC                            *TURNI CHIUSURA
770600931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
770700931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
770800060406     c
770900060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
771000060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
771100060406     c                   clear                   pgii
771200060406     c                   clear                   pgif
771300060406     c                   clear                   pgci
771400060406     c                   clear                   pgcf
771500060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
771600060420     c                   clear                   paii
771700060420     c                   clear                   paif
771800930420     C*
771900930305     C                   ENDSR
772000930512     C***********************************************************************
772100930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
772200930512     C***********************************************************************
772300930512     C     FORCON        BEGSR
772400930512     C*
772500931124     C                   Z-ADD     *ZEROS        D98RPI
772600931124     C                   Z-ADD     *ZEROS        D98RPC
772700930512     C                   Z-ADD     *ZEROS        D98RTI
772800930512     C                   Z-ADD     *ZEROS        D98RTC
772900930512     C                   Z-ADD     *ZEROS        D98RCI
773000930512     C                   Z-ADD     *ZEROS        D98RCC
773100930512     C                   Z-ADD     *ZEROS        D98RRI
773200930512     C                   Z-ADD     *ZEROS        D98RRC
773300931124     C                   Z-ADD     *ZEROS        D98FPI
773400931124     C                   Z-ADD     *ZEROS        D98FPC
773500930512     C                   Z-ADD     *ZEROS        D98FTI
773600930512     C                   Z-ADD     *ZEROS        D98FTC
773700930512     C                   Z-ADD     *ZEROS        D98FCI
773800930512     C                   Z-ADD     *ZEROS        D98FCC
773900930512     C                   Z-ADD     *ZEROS        D98FRI
774000930512     C                   Z-ADD     *ZEROS        D98FRC
774100931124     C                   MOVEL     'S'           D98CPI
774200931124     C                   MOVEL     'S'           D98CPC
774300930512     C                   MOVEL     'S'           D98CTI
774400930512     C                   MOVEL     'S'           D98CTC
774500930512     C                   MOVEL     'S'           D98CCI
774600930512     C                   MOVEL     'S'           D98CCC
774700930512     C                   MOVEL     'S'           D98CRI
774800930512     C                   MOVEL     'S'           D98CRC
774900931124     C                   Z-ADD     *ZEROS        D98NPI
775000931124     C                   Z-ADD     *ZEROS        D98NPC
775100930512     C                   Z-ADD     *ZEROS        D98NTI
775200930512     C                   Z-ADD     *ZEROS        D98NTC
775300930512     C                   Z-ADD     *ZEROS        D98NCI
775400930512     C                   Z-ADD     *ZEROS        D98NCC
775500930512     C                   Z-ADD     *ZEROS        D98NRI
775600930512     C                   Z-ADD     *ZEROS        D98NRC
775700930512     C                   Z-ADD     *ZEROS        D98HHI
775800930512     C                   Z-ADD     *ZEROS        D98HHC
775900930512     C                   Z-ADD     *ZEROS        D98HOI
776000930512     C                   Z-ADD     *ZEROS        D98HOC
776100930512     C                   Z-ADD     *ZEROS        D98HLI
776200930512     C                   Z-ADD     *ZEROS        D98HLC
776300930512     C                   Z-ADD     *ZEROS        D98HGI
776400930512     C                   Z-ADD     *ZEROS        D98HGC
776500931124     C                   Z-ADD     *ZEROS        D98HPI
776600931124     C                   Z-ADD     *ZEROS        D98HPC
776700930512     C                   Z-ADD     *ZEROS        D98HFI
776800930512     C                   Z-ADD     *ZEROS        D98HFC
776900930512     C                   Z-ADD     *ZEROS        D98HEI
777000930512     C                   Z-ADD     *ZEROS        D98HEC
777100930512     C                   Z-ADD     *ZEROS        D98HRI
777200930512     C                   Z-ADD     *ZEROS        D98HRC
777300930512     C                   Z-ADD     *ZEROS        D98HDI
777400930512     C                   Z-ADD     *ZEROS        D98HDC
777500930520     C*
777600930512     C                   ENDSR
777700930305     C********************************************************************
777800971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
777900950406031059090120151181212243273304334365
778000950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
778100950406312831303130313130313031
778200930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
778300971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
778400971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
778500971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
778600971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
778700971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
778800971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
778900971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
779000971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
779100971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
779200971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
779300971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
779400971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
779500971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
779600971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
779700971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
779800971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
779900971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
780000971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
780100971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
780200971127TNSD99R- Bolla inesistente.                                               BOIN20
780300971127TNSD99R-     Cap inesistente.                                             CAPI21
780400971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
780500971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
780600971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
780700971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
780800971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
780900971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
781000971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
781100971127TNSD99R- Contrassegno non trovato.                                        CONT29
781200971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
781300990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
781400990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
781500060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
781600060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
781700971127TNSD99R- 35                                                                   35
781800971127TNSD99R- 36                                                                   36
781900971127TNSD99R- 37                                                                   37
782000971127TNSD99R- 38                                                                   38
782100971127TNSD99R- 39                                                                   39
782200971127TNSD99R- 40                                                                   40
