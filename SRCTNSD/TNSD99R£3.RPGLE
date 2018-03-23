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
179500150630     C********************************************************************
179600150630     C*   Ricalcolo tempi trazione cliente in base all'arrivo ultimo collo
179700150630     C********************************************************************
179800150630     C     Ric_TRAZ      BEGSR
179900150630     C*
180000150630     C* PORTO TEMPO DI TRAZIONE TEORICO cliente IN UN CAMPO DI WORK
180100150630     C                   Z-ADD     D98TTc        W98TTc            3 0
180200150630     C*
180300150630     C* TROVO ASTA PARTENZA A SECONDA DEL TEMPO TEORICO DI TRAZIONE
180400150630     C* SE TRAZ.=24,48,.. --> 00     , SE TRAZ.=36,60,.. --> 12
180500150630     C     W98TTc        DIV       24            WDIV              3 0
180600150630     C                   MVR                     WRES              3 0
180700150630     C     WRES          IFEQ      *ZEROS
180800150630     C                   Z-ADD     *ZEROS        WASTA
180900150630     C                   ELSE
181000150630     C                   Z-ADD     12            WASTA
181100150630     C                   END
181200150630     C*
181300150630     C* TRASFORMO LA DATA DI ARRIVO REALE DELLA TRAZIONE
181400150630     C     D98DTI        IFNE      *ZEROS
181500150630     C                   Z-ADD     D98DTI        WDAT
181600150630     C                   EXSR      DATORE
181700150630     C                   Z-ADD     WORE          WPODTIC
181800150630     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
181900150630     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
182000150630     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
182100150630     C                   Z-ADD     WPODTIC       W10POI            9 0
182200150630     C                   EXSR      R10FES
182300150630     C                   Z-ADD     W10POO        WPODTIC
182400150630     C* AGGIUNGO LE ORA D'ASTA
182500150630     C*******            ADD       WASTA         WPODTIC
182600150630     C* AGGIUNGO ORE DI SOGLIA SOLO SE NON HO SLITTATO PER FESTIVO
182700150630     C     W10POI        IFEQ      W10POO
182800150630     C* AGGIUNGO L'ORA ARROTONDATA DALLA PRIMA TABELLA SOGLIA ARRIVI TRAZIONE
182900150630     C                   MOVEL     'OSA  '       WR3TAB            5            * H.  5,00 CIRCA
183000150630     C                   Z-ADD     D98HTI        WR3HHI            4 0
183100150630     C                   EXSR      RO3ORE
183200150630     C                   ADD       WR3HHO        WPODTIC
183300150630     C                   ADD       WR3HHO        W98TTc
183400150630     C* AGGIUNGO L'ORA ARROTONDATA DALLA SECONDA TABELLA SOGLIA ARRIVI TRAZIONE
183500150630     C                   MOVEL     'OSA1 '       WR3TAB            5            * H. 12,00 CIRCA
183600150630     C                   Z-ADD     D98HTI        WR3HHI            4 0
183700150630     C                   EXSR      RO3ORE
183800150630     C                   ADD       WR3HHO        WPODTIC
183900150630     C                   ADD       WR3HHO        W98TTc
184000150630     C                   ENDIF                                                  *SE NON FESTIVO
184100150630     C                   ENDIF                                                  *SE NON FESTIVO
184200150630     C*
184300150630     C* SE LA TRAZIONE E' STATA SPOSTATA IN AVANTI (IN RITARDO)
184400150630     C* VERIFICO DI NUOVO QUALE TEMPO DI CONSEGNA DEVO CONSIDERARE ORE 00 O 12
184500150630     C     D98TTc        IFNE      W98TTc                                       --- 1 -->
184600150630     C     W98TTc        DIV       24            WDIV              3 0
184700150630     C                   MVR                     WRES              3 0
184800150630     C     WRES          IFEQ      *ZEROS                                       --- 2 -->
184900150630     c                   eval      wTsogliaE='0'
185000150630     C                   ELSE                                                   <-- 2 -->
185100150630     c                   eval      wTsogliaE='1'
185200150630     C                   ENDIF                                                  <-- 2 ---
185300150630     C                   ENDIF                                                  <-- 2 ---
185400150630     c
185500150630     C                   ENDSR
185600930325     C********************************************************************
185700930325     C*   TDATPO - TRASFORMA LE DATE IN PROGRESSIVI ORE
185800930325     C********************************************************************
185900930325     C     TDATPO        BEGSR
186000930406     C*
186100930420     C* TRASFORMO LA DATA ODIERNA
186200930416     C                   Z-ADD     WDAODI        WDAT
186300930416     C                   EXSR      DATORE
186400050922     C                   Z-ADD     WORE          WPOATTData00                   *OGGI H.00
186500960911     C                   MOVEL     'OSC  '       WR3TAB
186600960911     C                   Z-ADD     WHHODI        WR3HHI
186700960911     C                   EXSR      RO3ORE
186800050922     C     WPOATTData00  ADD       WR3HHO        WPOATT            9 0          *OGGI H. ATTUALE
186900050922     c                   if        wr3hho=12
187000050922     c     wpoatt        add       12            wpoattdata24
187100050922     c                   else
187200050922     c                   z-add     wpoatt        wpoattdata24
187300050922     c                   endif
187400930325     C*
187500930420     C* TRASFORMO LA DATA DI SPEDIZIONE
187600931125     C     D98DSP        IFEQ      *ZEROS
187700930326     C                   MOVEL     '1'           D98ERR
187800930326     C                   MOVEL     MSG(2)        D98MSG
187900930326     C                   ELSE
188000931125     C                   Z-ADD     D98DSP        WDAT
188100930325     C                   EXSR      DATORE
188200930406     C                   Z-ADD     WORE          WPODSP
188300930326     C                   END
188400930414     C*
188500930420     C* DETERMINO DATA LIMITE IN CASO DI ERRORI
188600930414     C                   MOVEL     'LIM  '       WR3TAB
188700930414     C                   Z-ADD     *ZEROS        WR3HHI
188800930414     C                   EXSR      RO3ORE
188900931222     C     WR3HHO        MULT      10            WTM060            6 0
189000931222     C     WPODSP        ADD       WTM060        WPOLIM
189100140822     c
189200930325     C*
189300930420     C* TRASFORMO LA DATA DI CONSEGNA
189400931125     C     D98DCM        IFNE      *ZEROS
189500931125     C                   Z-ADD     D98DCM        WDAT
189600930325     C                   EXSR      DATORE
189700930326     C                   Z-ADD     WORE          WPODCM
189800930325     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
189900930326     C                   MOVEL     'OSC  '       WR3TAB
190000931125     C                   Z-ADD     D98HMC        WR3HHI
190100930326     C                   EXSR      RO3ORE
190200930326     C                   ADD       WR3HHO        WPODCM
190300930326     C                   END
190400941228     C*
190500941228     C* TRASFORMO LA DATA DI PARTENZA DELL'ULTIMO COLLO
190600941228     C     D98DUC        IFNE      *ZEROS
190700941228     C                   Z-ADD     D98DUC        WDAT
190800941228     C                   EXSR      DATORE
190900941228     C                   Z-ADD     WORE          WPODUC
191000970310     C* AGGIUNGO L'ORA ARROTONDATA DALLA TABELLA
191100970310     C                   MOVEL     'OSP  '       WR3TAB
191200970310     C                   Z-ADD     W98HUC        WR3HHI
191300970310     C                   EXSR      RO3ORE
191400970310     C     D98DRF        IFNE      *ZEROS
191500050928     c     w98huc        andgt     1200
191600970310     C                   SUB       12            WR3HHO
191700970310     C                   ENDIF
191800970310     C                   ADD       WR3HHO        WPODUC
191900941228     C* SE DATA SPEDIZIONE MAGGIORE DATA PART.ULT.COLLO , DT.SPED.->DT.PART.U.C.
192000070605     C***  WPODSP        IFGT      WPODUC
192100070605     C***                Z-ADD     WPODSP        WPODUC
192200070605     C***                ENDIF
192300941228     C                   ENDIF
192400960402     C*
192500141215     C* TRASFORMO LA DATA RITIRO oltre data prevista in VPO
192600970304     C     D98DBR        IFNE      *ZEROS
192700970304     C                   Z-ADD     D98DBR        WDAT
192800960402     C                   EXSR      DATORE
192900960402     C                   Z-ADD     WORE          WPODBR
193000070514     C* SE DATA = 0  --> PRENDO DATA SPEDIZIONE
193100960402     C                   ELSE
193200960402     C                   Z-ADD     WPODSP        WPODBR
193300960402     C                   ENDIF
193400930913     C*
193500960729     C* TRASFORMAZIONE IN ORE DELLE DATE PER IL CONTROLLO DELLA
193600930913     C* CONSEGNABILITA'
193700960729     C     FLCONS        IFEQ      'S'                                          --- 0 -->
193800950407     C*
193900930913     C* ==> DATA E ORA INIZIO CONSEGNABILITA'
194000931125     C     D98DIC        IFNE      *ZEROS                                       --- 1 -->
194100931125     C                   Z-ADD     D98DIC        WDAT
194200930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
194300930913     C                   Z-ADD     WORE          WPODIC
194400930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
194500931125     C                   Z-ADD     D98OIC        WR3HHI
194600930913     C                   EXSR      RO3ORE
194700930913     C                   ADD       WR3HHO        WPODIC
194800931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
194900950407     C*
195000930913     C* ==> DATA E ORA FINE CONSEGNABILITA'
195100931125     C     D98DFC        IFNE      *ZEROS                                       --- 1 -->
195200931125     C                   Z-ADD     D98DFC        WDAT
195300930913     C                   EXSR      DATORE                                       *CALC PROGR. ORE
195400930913     C                   Z-ADD     WORE          WPODFC
195500140822     C                   Z-ADD     WORE          WPODFC_00
195600930916     C                   MOVEL     'OSCO '       WR3TAB                         *AGG.ORE ARROTONDAM.
195700931125     C                   Z-ADD     D98OFC        WR3HHI
195800930913     C                   EXSR      RO3ORE
195900930913     C                   ADD       WR3HHO        WPODFC
196000931125     C                   ENDIF                                                  <-- 1 --- DATA = ZER
196100930913     C*
196200930913     C* SE E' VALIDA LA DATA INIZIO E NON LA DATA FINE CONSEGNABILITA'--> ERRORE
196300930913     C     D98ERR        IFEQ      '0'
196400930913     C     WPODFC        IFEQ      *ZEROS
196500930913     C     WPODIC        ANDNE     *ZEROS
196600930913     C                   MOVEL     '1'           D98ERR
196700930913     C                   MOVEL     MSG(17)       D98MSG
196800930913     C                   ENDIF
196900930913     C                   ENDIF
197000930907     C*
197100930914     C* SE LA DATA INIZIO CONSEGNAB. > DATA FINE CONSEGNAB. ---> ERRORE
197200930913     C     D98ERR        IFEQ      '0'
197300930913     C     WPODIC        IFGT      WPODFC
197400930907     C                   MOVEL     '1'           D98ERR
197500930913     C                   MOVEL     MSG(18)       D98MSG
197600930913     C                   ENDIF
197700930913     C                   ENDIF
197800950407     C*
197900950407     C                   ENDIF                                                  <-- 0 ---C'E'CONSEGN
198000930511     C*
198100930511     C                   ENDSR
198200950110     C********************************************************************
198300950110     C*   CALDPP - CALCOLA DATE PREVISTE DI PARTENZA ULTIMO COLLO
198400950110     C********************************************************************
198500950110     C     CALDPP        BEGSR
198600950110     C*
198700950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
198800950110     C                   MOVEL     'P'           WTC                            *PARTENZA
198900950110     C                   MOVEL     'I'           WIC                            *INTERNA
199000960402     C                   Z-ADD     WPODBR        WR4POI            9 0          *DATA BORDERO'
199100950110     C                   Z-ADD     D98TPI        WR4NOI                         *ORE TEOR.PART.INT.
199200950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
199300950110     C                   Z-ADD     WR4POO        WPOPPI            9 0          *DT.PREV.PART.U.C.IN
199400950110     C*
199500950110     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   CLIENTE ************
199600950110     C                   MOVEL     'P'           WTC                            *PARTENZA
199700950110     C                   MOVEL     'C'           WIC                            *CLIENTE
199800950110     C                   Z-ADD     WPODSP        WR4POI                         *DATA SPEDIZ.
199900950110     C                   Z-ADD     D98TPC        WR4NOI                         *ORE TEOR.PART.CLI.
200000950110     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
200100950110     C                   Z-ADD     WR4POO        WPOPPC            9 0          *DT.PREV.PART.U.C.CL
200200960229     C*
200300960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
200400050922     c
200500050922     C* DETERMINO LA DATA PREVISTA DI PARTENZA U.C.   INTERNA ************
200600050922     c
200700050922%%%  C* SE LA BOLLA NON E' ANCORA PARTITA (DATA PART.ULT.COLLO = 0 ) e non
200800090903%%%  C*  non ancora arrivata (Arrivo trazione=0 e consegna =0),
200900050922%%%  C*  imposto Data partenza ultimo collo=data di oggi  -
201000090903%%%  C*  i tempi di consegna interni e di trazione. Attribuisco eventuale
201100050922%%%  C*  ritardo alla partenza.
201200050922     c* Il tutto solo se la data di oggi > della
201300050922     c*  data teorica di partenza determinata
201400050922     c*
201500950110%%%  C     D98ERR        IFEQ      '0'
201600950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
201700950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZIONE
201800950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
201900050922     c     wpoattData24  andgt     wpoppi
202000960911%%%  C********             MOVEL'OSC  '   WR3TAB
202100960911%%%  C********             Z-ADDWHHODI    WR3HHI           *ORA ATTUALE
202200960911%%%  C********             EXSR RO3ORE
202300960911%%%  C********   WPOODI    ADD  WR3HHO    WR0POI  90       *DATA ATTUALE
202400050922%%%  C                   Z-ADD     WPOATTData24  WR0POI            9 0          *DATA ATTUALE
202500090827%%%  C***  D98TTI        ADD       D98TCI        WR0NOI                         *ORE DA TOGLIERE
202600090827%%%  C     D98TTI        ADD       w99TCI        WR0NOI                         *ORE DA TOGLIERE
202700950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
202800050922%%%  C*****              Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
202900050922     c* La nuova data calcolata non può essere < della minima prevista
203000050922     c                   if        wr0poo>wpoppi
203100050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
203200050922     c                   endif
203300050922     c
203400950110%%%  C                   ENDIF
203500950110%%%  C                   ENDIF
203600950110%%%  C*
203700950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
203800050915%%%  C*  ARRIVATA ALLA FILIALE DI ARRIVO, imposto data part. U.C =
203900050915%%%  C*  data arrivo - tempi di trazione interno. Eventuale ritardo
204000050915%%%  C*  alla prtenza.
204100950110%%%  C     D98ERR        IFEQ      '0'
204200950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
204300950110%%%  C     WPODTI        ANDNE     0                                            *DATA ARR. TRAZ.
204400950110%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
204500950110%%%  C                   Z-ADD     WPODTI        WR0POI            9 0          *DATA CONSEGNA
204600950110%%%  C                   Z-ADD     D98TTI        WR0NOI                         *ORE DA TOGLIERE
204700950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
204800050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
204900950110%%%  C                   ENDIF
205000950110%%%  C                   ENDIF
205100950110%%%  C*
205200950110%%%  C* SE LA BOLLA NON E' ANCORA PARTITA MA RISULTA
205300950110%%%  C* CONSEGNATA (?) ATTRIBUISCO LA RESPONSABILITA' DEL
205400950110%%%  C* RITARDO ALLA PARTENZA IMPOSTANDO LA DATA DI PART.U.C.  UGUALE ALLA DATA
205500950110%%%  C* DI CONSEGNA MENO IL TEMPO DI TRAZIONE INTERNO E IL TEMPO DI CONSEGNA.
205600950110%%%  C     D98ERR        IFEQ      '0'
205700950110%%%  C     WPODUC        IFEQ      0                                            *DATA PART.ULT.COLLO
205800950110%%%  C     WPODTI        ANDEQ     0                                            *DATA ARR. TRAZ.
205900950110%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
206000950110%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
206100090827%%%  C***  D98TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
206200090827%%%  C     w99TCI        ADD       D98TTI        WR0NOI                         *ORE DA TOGLIERE
206300950110%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
206400050922%%%  C                   Z-ADD     WR0POO        WPOPPI                         *ORA PREV.PART.TRAZ.
206500950110%%%  C                   ENDIF
206600950110%%%  C                   ENDIF
206700960229     C*
206800960229     C                   ENDIF                                                  <-- 1 ---
206900950110     C*
207000950110     C                   ENDSR
207100930604     C********************************************************************
207200930604     C*   CALDPT - CALCOLA DATE PREVISTE DI TRAZIONE
207300930604     C********************************************************************
207400930604     C     CALDPT        BEGSR
207500090902     c                   clear                   RicalcoloTRAZ     1
207600930604     C*
207700930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
207800931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
207900931129     C                   MOVEL     'I'           WIC                            *INTERNA
208000050915     c* se non presente la data partenza ultimo collo, uso la teorica
208100950110     C     D98DUC        IFEQ      *ZEROS
208200950110     C                   Z-ADD     WPOPPI        WR4POI            9 0          *DATA PREV.PART.->
208300950110     C                   ELSE
208400950110     C                   Z-ADD     WPODUC        WR4POI                         *DATA PART.U.C. ->
208500950110     C                   ENDIF
208600930604     C                   Z-ADD     D98TTI        WR4NOI                         *ORE TEOR.TRAZ.INT.
208700930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
208800930604     C                   Z-ADD     WR4POO        WPOPTI            9 0          *DT.PREV.ARR.TRAZ.IN
208900930604     C*
209000930604     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE ************
209100931129     C                   MOVEL     'T'           WTC                            *TRAZIONE
209200931129     C                   MOVEL     'C'           WIC                            *CLIENTE
209300950110     C                   Z-ADD     WPOPPC        WR4POI                         *DATA PREV.PART.CLI.
209400930604     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.CLI.
209500930604     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA PREV
209600930604     C                   Z-ADD     WR4POO        WPOPTC            9 0          *DT.PREV.ARR.TRAZ.CL
209700150508
209800150508     C* DETERMINO LA DATA PREVISTA DI ARRIVO TRAZIONE CLIENTE per prevista consegna
209900150508     c* se presente la data partenza PRIMO  collo --> la uso
210000150508     c                   if        w98dpc=0
210100150508     C                   Z-ADD     Wpoptc        WPOPTC_DEE        9 0
210200150508     c                   else
210300150508     C                   MOVEL     'T'           WTC                            *TRAZIONE
210400150508     C                   MOVEL     'C'           WIC                            *INTERNA
210500150508     C                   Z-ADD     W98DPC        WDAT
210600150508     C                   EXSR      DATORE
210700150508     C                   Z-ADD     WORE          WR4POI                         *DATA PART.U.C. ->
210800150508     C                   Z-ADD     D98TTC        WR4NOI                         *ORE TEOR.TRAZ.INT.
210900150508     C                   EXSR      RO4CDP
211000150508     C                   Z-ADD     WR4POO        WPOPTC_DEE        9 0
211100150508     c                   endif
211200960229     C*
211300960229     C     D98TLA        IFNE      'T'                                          --- 1 -->
211400940624%%%  C*
211500050915     C************** DATA PREVISTA DI ARRIVO TRAZIONE INTERNA ************
211600050915
211700940624%%%  C* SE LA BOLLA NON E' ANCORA ARRIVATA ALLA FILIALE DI ARRIVO (DATA ARRIVO
211800050915%%%  C* TRAZ. = 0  DATA CONSEGNA = 0 ) imposto data arrivo trazione =
211900050915%%%  C*   data di oggi - tempo di consegna interno . Eventuale ritardo è    ATA
212000050915     c*   attribuito alla trazione
212100940624%%%  C     D98ERR        IFEQ      '0'
212200940624%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
212300940624%%%  C     WPODCM        ANDEQ     0                                            *DATA CONSEGNA
212400090903     c     wpoatt        andge     wpopti
212500050922%%%  C                   Z-ADD     WPOATT        WR0POI            9 0          *DATA ATTUALE
212600090827%%%  C****               Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
212700090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
212800940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
212900050922%%%  C****               Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
213000050922     c                   if        wr0poo>wpopti
213100050922     c                   eval      wpopti=wr0poo
213200050922%%%  C                   endif
213300090902     c*
213400090902     c* Se la data attuale WPOATT > data prevista arrivo trazione teorica(DTI=0)
213500090902     c*  allora ricalcolo tutte le date impostando come data di arrivo
213600090902     c*  della trazione udate e utime
213700090902     c                   eval      RicalcoloTRAZ='S'
213800090902     c
213900940624%%%  C                   ENDIF
214000940624%%%  C                   ENDIF
214100940628%%%  C*
214200050915%%%  C* SE LA BOLLA NON presenta data arrivo ultimo collo ma è consegnaSULTA
214300050915%%%  C* ta, imposto data arrivo trazione = data consegna - tempi di con
214400050915%%%  C* segna. L'eventuale ritardo è attribuito alla trazione          LLA DATA
214500940628%%%  C     D98ERR        IFEQ      '0'
214600940628%%%  C     WPODTI        IFEQ      0                                            *DATA ARR. TRAZIONE
214700940628%%%  C     WPODCM        ANDNE     0                                            *DATA CONSEGNA
214800940628%%%  C                   Z-ADD     WPODCM        WR0POI            9 0          *DATA CONSEGNA
214900090827%%%  C*****              Z-ADD     D98TCI        WR0NOI                         *ORE DA TOGLIERE
215000090827%%%  C                   Z-ADD     w99TCI        WR0NOI                         *ORE DA TOGLIERE
215100940628%%%  C                   EXSR      RO0GES                                       *ROUT.SLITTA INDIETR
215200050922%%%  C                   Z-ADD     WR0POO        WPOPTI                         *ORA PREV.ARR.TRAZ.
215300940628%%%  C                   ENDIF
215400940628%%%  C                   ENDIF
215500960229     C*
215600960229     C                   ENDIF                                                  <-- 1 ---
215700930604     C*
215800930604     C                   ENDSR
215900930511     C********************************************************************
216000930511     C*   CALDCR - DETERMINO RANGE PER DATA CONSEGNA RICHIESTA
216100930511     C********************************************************************
216200930511     C     CALDCR        BEGSR
216300050914     c                   clear                   consideraora
216400100308     c
216500100308     c* Dopo gli ennesimi problemi sull'ora richiesta modificato come segue:
216600100308     c*
216700100308     c* 1) se tipo servizio "E" --> ignoro sia data che ora
216800100308     c* 1) se presente solo ORA --> ignoro sempre
216900100308     c* 2) se presente data e ora IL --> ignoro l'ora
217000100308     c* 3) se presente data e ora DOPO IL/PRIMA DEL --> considero
217100100309     c***                if        d98tsp='E'
217200100309     c***                clear                   d98dcr
217300100309     c***                clear                   d98hcr
217400100309     c**+                clear                   d98tcr
217500100309     c***                endif
217600100308     c
217700100308     c                   if        d98hcr>0
217800100308     c
217900100308     c                   if        d98dcr=0
218000100308     c                   clear                   d98hcr
218100100308     c                   endif
218200100308
218300100308     c                   if        d98dcr>0 and d98tcr=' '
218400100308     c                   clear                   d98hcr
218500100308     c                   endif
218600100308     c                   endif
218700941228     C*
218800941228     C* SE ESISTE SOLO L'ORA DI CONSEGNA RICHIESTA E NON LA DATA
218900941228     C* METTO NELLA DATA CONS.RICH. LA DATA DI CONSEGNA MERCE
219000050901     c*  solo se l'ora consegna = a quella richiesta dal cliente
219100050901     c*  (per uguale si intende entrambe nel mattino o entrambe
219200050901     c*   nel pomeriggio), altrimenti ignoro l'ora richiesta
219300050901     c*  (EDPES 1/9/05)
219400050831     C***  D98HCR        IFNE      *ZEROS                                       --- 1 --> *ORA CONS.
219500050831     C***  D98DCR        ANDEQ     *ZEROS                                                 *DT.CONS.R
219600050831     C***  D98DCM        ANDNE     *ZEROS                                                 *DT.CONSEG
219700050831     C***                Z-ADD     D98DCM        D98DCR
219800050831     C***                ENDIF
219900050901     c*
220000070521     c* se presente solo ora senza data DOPO IL o PRIMA DEL --> ignoro
220100050914     c
220200100308    1c***                if        d98hcr>0 and d98dcr=0  and d98tcr<>' '
220300100308     c***                clear                   d98hcr
220400100308     c***                clear                   d98tcr
220500100308     c***                endif
220600050914     c* Se presente solo ora senza data, aggiungo 24 ore ai tempi
220700050914     c*  di consegna, se l'ora di consegna corrisponde all'ora
220800070521     c*  richiesta dal cliente
220900100308    1c***                if        d98hcr>0 and d98dcr=0  and d98dcm>0
221000100308     C***                MOVEL     'OSC  '       WR3TAB
221100100308     C***                Z-ADD     D98HMC        WR3HHI
221200100308     C***                EXSR      RO3ORE
221300100308     c***                z-add     wr3hho        salvaoreHMC
221400050901     c
221500100308     C***                Z-ADD     D98Hcr        WR3HHI
221600100308     C***                EXSR      RO3ORE
221700100308     c***                z-add     wr3hho        salvaoreHCR
221800100308     c***
221900050914     c* Se le ore corrispondono memorizzo di aggiungere 24 ore alla consegna
222000100308    2c***                if        salvaoreHMC=salvaoreHCR
222100100308     C***                eval      Consideraora='S'
222200100308     c***                else
222300100308     C***                eval      Consideraora=' '
222400100308     c***                clear                   d98hcr
222500100308    2c***                endif
222600100308    1c***                endif
222700930511     C*
222800930511     C* ESEGUO CALCOLO SOLO SE ESISTE LA DATA DI CONSEGNA RICHIESTA
222900931125     C     D98DCR        IFNE      *ZEROS                                       --- 1 -->
223000931125     C                   Z-ADD     D98DCR        WDAT
223100930325     C                   EXSR      DATORE
223200930326     C                   Z-ADD     WORE          WPODCR
223300930420     C*
223400930420     C* A SECONDA DEL TIPO DI CONSEGNA RICHIESTA DETERMINO RANGE
223500930420     C*
223600931125     C     D98TCR        IFEQ      'D'                                          *CONS.RICH.DOPO IL .
223700930420     C* DATA CONS.RICH. + ORE TABELLA = DATA INIZIO RANGE
223800930326     C                   MOVEL     'CRD  '       WR3TAB
223900931125     C                   Z-ADD     D98HCR        WR3HHI
224000930419     C                   EXSR      RO3ORE
224100930420     C     WPODCR        ADD       WR3HHO        WPOCRI                         *INIZIO RANGE
224200940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
224300050902     c
224400931207     C* DATA INIZIO RANGE + ORE TABELLA + ORE ESPR/NORM = DATA FINE RANGE
224500930420     C                   MOVEL     'CRD1 '       WR3TAB
224600930420     C                   Z-ADD     *ZEROS        WR3HHI
224700930420     C                   EXSR      RO3ORE
224800931207     C                   Z-ADD     WR3HHO        WORECR                         *ORE PER CONS.RICH.
224900090827     C*****              ADD       D98TCC        WORECR                         *FINE RANGE
225000090827     C                   ADD       w99TCC        WORECR                         *FINE RANGE
225100050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
225200060407     c*  se ancora non consegnabile la spedizione perchè in anticipo
225300050907     c                   z-add     worecr        oreILoDOPO
225400931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
225500931207     C                   MOVEL     'C'           WTC                            *CONSEGNA
225600931207     C                   MOVEL     'C'           WIC                            *CLIENTE
225700931207     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
225800150330     C                   Z-ADD     WORECR        WR4NOI
225900931207     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
226000931207     C                   Z-ADD     WR4POO        WPOCRF                         *DATA FINE CONS.RICH
226100931209     C     WPOCRF        SUB       12            WR7POI            9 0
226200931209     C                   MOVEL     'X'           WIC
226300931207     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
226400931209     C                   ADD       12            WR7POO
226500931207     C     WR7POO        IFGT      WPOCRF
226600931207     C                   Z-ADD     WR7POO        WPOCRF
226700931207     C                   END
226800150330     c* PER LA NUOVA DATA PREVISTA CONSEGNA DEE faccio lo stesso calcolo
226900150330     C*  ma come risultato deve uscire la prima DATA DISPONIBILE,
227000150330     C*  per cui non sommo le ore di cpertura (24)   ma solo i tempi di consegna
227100150330     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
227200150330     C                   MOVEL     'C'           WTC                            *CONSEGNA
227300150330     C                   MOVEL     'C'           WIC                            *CLIENTE
227400150330     C                   Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
227500150330     C                   Z-ADD     w99TCC        WR4NOI
227600150330     c                   sub       12            wr4noi
227700150330     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
227800150330     C                   Z-ADD     WR4POO        WPOCRF_dee                     *DATA FINE CONS.RICH
227900150330     C                   Z-ADD     WR4POO        wr7poi                         *DATA FINE CONS.RICH
228000150330     c
228100150330     C***  WPOCRF_dee    SUB       12            WR7POI            9 0
228200150330     C                   MOVEL     'X'           WIC
228300150330     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
228400150330     C******             ADD       12            WR7POO
228500150330     C     WR7POO        IFGT      WPOCRF_dee
228600150330     C                   Z-ADD     WR7POO        WPOCRF_dee
228700150330     C                   END
228800050902     c
228900050902     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
229000050902     c*  sono le ore di fine della non consegnabilità
229100050905     c* Se aggiungendo 1 finisco in giorno festivo
229200050905     c*  cerco primo giorno non festivo
229300050902     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
229400050907     C****               MOVEL     'C'           WTC                            *CONSEGNA
229500050907     C****               MOVEL     'C'           WIC                            *CLIENTE
229600050907     C****               Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
229700050907     C****               Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
229800050907     C****               EXSR      RO4CDP                                       *ROUT.CALC.DATA
229900050907     C****               z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
230000050907     c
230100050907     c                   add       1             wpocri
230200050902     c
230300050902   FFC                   Z-ADD     WPOCRI        WPOCRC
230400050902     c
230500930325     C                   END
230600150330     C*
230700931125     C     D98TCR        IFEQ      'P'                                          *CONS.RICH.PRIMA DEL
230800930420     C* DATA SPED. = DATA INIZIO RANGE
230900930420     C                   Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
231000940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
231100940324£$   C                   Z-ADD     WPOCRI        WPOCRC
231200930420     C* DATA CONS.RICH. - 24 ORE + ORE TABELLA = DATA FINE RANGE
231300930420     C                   MOVEL     'CRP  '       WR3TAB
231400931125     C                   Z-ADD     D98HCR        WR3HHI
231500930420     C                   EXSR      RO3ORE
231600931207     C     WPODCR        ADD       WR3HHO        WPOCRF
231700931207     C                   SUB       24            WPOCRF                         *FINE RANGE
231800931207     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
231900931207     C* ANDANDO INDIETRO NEL TEMPO
232000050907     C* In realtà non devo saltare i festivi: infatti se devo consegnare
232100050907     c* PRIMA DI LENEDì, per le filiali che  vanno in consegna il sabato
232200050907     c*  sabato è ok, mentre  saltando i festivi considera come ultima
232300050907     c* data utile venedì sera
232400130411     C                   MOVEL     'C'           WTC                            *CONSEGNA
232500130411     C                   MOVEL     'X'           WIC                            *CLIENTE
232600931207     C                   Z-ADD     WPOCRF        WR8POI            9 0          *DATA RIF.PER CALC.D
232700931207     C                   EXSR      RO8FES                                       *ROUT.SLITTA INDIETR
232800931207     C                   Z-ADD     WR8POO        WPOCRF                         *DATA FINE RANGE C.R
232900930326     C                   END
233000930526     C*
233100931125     C     D98TCR        IFEQ      'P'
233200931125     C     D98TCR        OREQ      'D'
233300930526     C                   ELSE                                                   *CONS.RICH.IL ..
233400940324£$   C* MEMORIZZO DATA CONSEGNA RICHIESTA PER CALCOLO CONSEGNABILITA'
233500940324£$   C                   Z-ADD     WPODCR        WPOCRC
233600930526     C* DATA SPED. = DATA INIZIO RANGE
233700050902     C****               Z-ADD     WPODSP        WPOCRI                         *INIZIO RANGE
233800050902     C* 02/09/05 --> data richista = DATA INIZIO RANGE
233900050902     C                   Z-ADD     WPODcr        WPOCRI                         *INIZIO RANGE
234000930526     C* DATA CONS.RICH. + ORE TABELLA = DATA FINE RANGE
234100930526     C                   MOVEL     'CRI  '       WR3TAB
234200931125     C                   Z-ADD     D98HCR        WR3HHI
234300930526     C                   EXSR      RO3ORE
234400050907     c* mi salvo le ore di consegnabilità da aggiungere alla fine
234500050907     c*  se ancora non conseganbile la spedizione perchè in anticipo
234600050907     c                   z-add     wr3hho        oreILoDOPO
234700930526     C     WPODCR        ADD       WR3HHO        WPOCRF                         *FINE RANGE
234800050902     c
234900050902     c* se presente anche ora ed è pomeriggio, sommo 12 ore all'inizio range
235000050902     c                   if        wr3MATPOM='P' and d98hcr>0
235100050902     c                   add       12            wpocri
235200100308     c* tolgo 12 ore dalle ore conseganbili
235300050907     c                   sub       12            oreILoDOPO
235400050902     c                   endif
235500050902     c
235600931209     C     WPOCRF        SUB       12            WR7POI            9 0
235700931209     C                   MOVEL     'X'           WIC
235800931209     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
235900931209     C                   ADD       12            WR7POO
236000931209     C     WR7POO        IFGT      WPOCRF
236100931209     C                   Z-ADD     WR7POO        WPOCRF
236200931209     C                   END
236300050905     c
236400050905     c* All'inizio RANGE aggiunto 1 ora perchè in realtà le ore trovate
236500050905     c*  sono le ore di fine della non consegnabilità
236600050905     c* Se aggiungendo 1 finisco in giorno festivo
236700050905     c*  cerco primo giorno non festivo
236800050905     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
236900050907     C**                 MOVEL     'C'           WTC                            *CONSEGNA
237000050907     C**                 MOVEL     'C'           WIC                            *CLIENTE
237100050907     C**                 Z-ADD     WPOCRI        WR4POI            9 0          *DATA RIF.PER CALC.D
237200050907     C**                 Z-ADD     1             WR4NOI                         *N.ORE DA AGG.
237300050907     C**                 EXSR      RO4CDP                                       *ROUT.CALC.DATA
237400050907     C**                 z-add     wr4poo        wpocri                         *DATA FINE CONS.RICH
237500050907     c
237600050907     c                   add       1             wpocri
237700050905     c
237800050905   FFC                   Z-ADD     WPOCRI        WPOCRC
237900050905     c
238000930526     C                   END
238100931206     C*
238200931206     C* SE LA DATA DI FINE RANGE E' INFERIORE ALLA DATA DI CONSEGNA TEORICA
238300931206     C* LA IMPOSTO COME LA CONS.TEORICA
238400140825     C     WPOCRF        IFLT      WPODEI_dcr                                   *FINE CO.RI < CO.TEO
238500140825     C                   Z-ADD     WPODEI_dcr    WPOCR1                         *->FINE CONS.RICH.IN
238600931206     C                   ELSE
238700931206     C                   Z-ADD     WPOCRF        WPOCR1
238800931206     C                   END
238900150608     c* ai fini del calcolo affidabilità
239000150608     C     WPOCRF        IFLT      WPOaff_dcr                                   *FINE CO.RI < CO.TEO
239100150608     C                   Z-ADD     WPOaff_dcr    WPOCR2                         *->FINE CONS.RICH.CL
239200931206     C                   ELSE
239300931206     C                   Z-ADD     WPOCRF        WPOCR2
239400931206     C                   END
239500150608     c* ai fini della data prevista consegna cliente
239600150608     C     WPOCRF        IFLT      WPOdee_dcr                                   *FINE CO.RI < CO.TEO
239700150608     C                   Z-ADD     WPOdee_dcr    WPOCR2_dee                     *->FINE CONS.RICH.CL
239800150608     C                   ELSE
239900150608     C                   Z-ADD     WPOCRF        WPOCR2_dee
240000150608     C                   END
240100150331     c
240200150331     c                   if        wpocrf_dee>0
240300150330     C     WPOCRF_dee    IFLT      WPODEE_dcr                                   *FINE CO.RI < CO.TEO
240400150330     C                   Z-ADD     WPODEE_dcr    WPOCRF_dee                     *->FINE CONS.RICH.CL
240500150330     C                   END
240600150331     C                   END
240700150331     C*
240800150331     c                   if        wpocrf_dee=0
240900150608     C     wpocr2_dee    sub       24            WPOCRF_dee
241000150331     c                   endif
241100150331     c
241200930526     C                   END                                                    <-- 1 ---
241300930511     C*
241400930511     C                   ENDSR
241500930511     C********************************************************************
241600930511     C*   CALLAV - DETERMINO RANGE PER LASCIATO AVVISO
241700930511     C********************************************************************
241800930511     C     CALLAV        BEGSR
241900930511     C*
242000941228     C* SOLO SE ESISTONO EVENTI DI TIPO LAS.AVV.
242100930421     C     WEELAV        IFEQ      'S'                                          --- 1 -->
242200930421     C*
242300930421     C                   Z-ADD     0             IL                3 0          *INDICE LASC.AVVISI
242400930421     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
242500930421     C     1             DO        IE            E                 3 0          --- 2 -->
242600930510     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
242700930421     C                   Z-ADD     1             I
242800930510     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
242900060406     C                   MOVEL     D2A(I)        usa2a                          *REC.TAB.NELLA DS TA
243000930421     C* SE CAUSALE EVENTO E' DI TIPO LASC.AVVISO MEMORIZZO
243100930511     C     §2AFTC        IFEQ      'A'                                          --- 3 -->
243200930421     C                   ADD       1             IL
243300930510     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
243400930421     C                   EXSR      DATORE
243500930510     C                   Z-ADD     WORE          LAI(IL)                        *INIZIO LASC.AVVISO
243600091015     C**                 ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
243700091014     c* Mi salvo la posizione dell'evento
243800091014     C                   Z-ADD     E             LAp(IL)
243900930510     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
244000930510     C                   MOVEL     'LAV  '       WR3TAB                         *COD.REC.TABELLA
244100930510     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
244200930421     C                   EXSR      RO3ORE
244300930510     C                   ADD       WR3HHO        LAI(IL)                        *INIZIO LASC.AVVISO
244400930510     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
244500930611     C                   Z-ADD     WR3HHA        WORELA                         *N.ORE LASCIATO AVVI
244600930510     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE LASC.AVV.
244700091015     c**                 if        §2agla>1
244800091015     c**                 sub       1             §2agla
244900930510     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
245000091015     c**                 else
245100091015     c**                 clear                   whh
245200091015     c**                 endif
245300930510     C                   ADD       WHH           WORELA
245400940324£$   C                   EXSR      CALLAC                                       *CALC.FINE L.A.X CON
245500930510     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
245600090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
245700150317     C**********         ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
245800930421     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
245900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
246000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
246100930510     C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
246200930510     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
246300930510     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
246400930510     C                   Z-ADD     WR4POO        LAF(IL)                        *DATA FINE LASC.AVV.
246500931209     C                   Z-ADD     LAF(IL)       WR7POI
246600931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
246700931202     C     WR7POO        IFGT      LAF(IL)
246800931201     C                   Z-ADD     WR7POO        LAF(IL)
246900931202     C                   END
247000930513     C* AGGIUNGO 12 ORE ALL'INIZIO L.AVV. PER NON CONSIDERARE VALIDE LE 12 ORE
247100930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
247200091015     C                   ADD       12            LAI(IL)                        *INIZIO LASC.AVVISO
247300930510     C                   END                                                    <-- 3 ---
247400931223     C                   ENDDO                                                  <-- 2 ---
247500930421     C*
247600930421     C                   END                                                    <-- 1 ---
247700930618££$$ C*
247800930618££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
247900931129££$$ C                   Z-ADD     *ZEROS        WPOLAF
248000941228££$$ C     1             DO        IL            I
248100930618££$$ C     LAF(I)        IFGT      WPOLAF
248200930618££$$ C                   Z-ADD     LAF(I)        WPOLAF
248300930618££$$ C                   END
248400931223££$$ C                   ENDDO
248500930511     C*
248600930511     C                   ENDSR
248700940324£$   C********************************************************************
248800940324£$   C*   CALLAC - CALCOLA FINE LASC.AVV. PER DETERMINARE LA CONSEGNABILITA'
248900940324£$   C********************************************************************
249000940324£$   C     CALLAC        BEGSR
249100941229     C*
249200950407     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
249300950407     C*
249400950210     C                   Z-ADD     WORELA        WTM060
249500941229     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE L.AVV. LE
249600941229     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
249700941229     C     D98DAG        IFEQ      *ZEROS
249800050520     c     §2aftg        andeq     'S'
249900090827     C***  WORELA        ADD       D98TCC        WTM060            6 0
250000090827     C     WORELA        ADD       w99TCC        WTM060            6 0
250100941229     C                   ENDIF
250200941229     C*
250300940324£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
250400940324£$   C                   MOVEL     'C'           WIC                            *CLIENTE
250500941229£$   C                   Z-ADD     LAI(IL)       LCI(IL)                        *DATA INIZ LASC.AVV.
250600941229£$   C                   Z-ADD     LAI(IL)       WR4POI            9 0          *DATA RIF.PER CALC.D
250700941229£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
250800940324£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
250900941229£$   C                   Z-ADD     WR4POO        LCF(IL)                        *DATA FINE LASC.AVV.
251000050520     c* Memorizzo se si tratta di l.avviso consegnabile o meno
251100050520     c                   movel     §2aftg        LCON(IL)
251200950407     C*
251300950407     C                   ENDIF
251400960129     C*
251500940324£$   C                   ENDSR
251600930511     C********************************************************************
251700930511     C*   CALGIA - DETERMINO RANGE PER GIACENZA
251800930511     C********************************************************************
251900930511     C     CALGIA        BEGSR
252000960129     C*
252100960129     C*-------------------------------------------------------------------
252200960129     C* EVENTI GIACENZA
252300960129     C*-------------------------------------------------------------------
252400960129     C* SOLO SE ESISTONO EVENTI DI TIPO GIACENZA
252500960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
252600960129     C*
252700050405     C* CARICO LE GIACENZE (TIGCP01/TIGCP21) DELLA BOLLA CORRENTE
252800041116     c     d98nsp        ifgt      0
252900031125     C                   EXSR      CARGIAC
253000041116     c                   endif
253100031125     C*
253200960129     C                   Z-ADD     0             IG                3 0          *INDICE GIACENZE
253300960129     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
253400960129     C     1             DO        IE            E                 3 0          --- 2 -->
253500960129     C* LEGGO TAB. CAUSALE EVENTO (LO DEVE TROVARE PERCHE' GIA' CONTROLLATO)
253600960129     C                   Z-ADD     1             I
253700960129     C     EVC(E)        LOOKUP    C2A(I)                                 99    *CERCA CAUSALE EVENT
253800060406     C                   MOVEL     d2a(I)        usa2a                          *REC.TAB.NELLA DS TA
253900960129     C* SE CAUSALE EVENTO E' DI TIPO GIACENZA MEMORIZZO
254000960129     C     §2AFTC        IFEQ      'G'                                          --- 3 -->
254100960129     C                   ADD       1             IG
254200960129     C                   Z-ADD     EVD(E)        WDAT                           *DATA EVENTO
254300960129     C                   EXSR      DATORE
254400960129     C                   Z-ADD     WORE          GII(IG)                        *INIZIO GIACENZA
254500140606     c* Mi salvo la posizione dell'evento
254600140606     C                   Z-ADD     E             gip(IG)
254700960129     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
254800960129     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
254900960129     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
255000960129     C                   EXSR      RO3ORE
255100960129     C                   ADD       WR3HHO        GII(IG)                        *INIZIO GIACENZA
255200960129     C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA E INIZIARE CALC.FINE RANGE
255300960129     C                   Z-ADD     WR3HHA        WOREGI                         *N.ORE GIACENZA
255400960129     C* RECUPERO DALLA TAB. I GIORNI DA AGG. PER OTTENERE DATA FINE GIACENZA
255500960129     C     §2AGLA        MULT      24            WHH               3 0          *N.GIORNI PER 24 ORE
255600960129     C                   ADD       WHH           WOREGI
255700960129£$   C                   EXSR      CALGIC                                       *CALC.FINE GIA.X CON
255800960129     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
255900090827     C*****              ADD       D98TCC        WOREGI                         *TEMPO TEOR.CONS.
256000090827     C                   ADD       w99TCC        WOREGI                         *TEMPO TEOR.CONS.
256100960129     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
256200960129     C                   MOVEL     'C'           WTC                            *CONSEGNA
256300960129     C                   MOVEL     'C'           WIC                            *CLIENTE
256400960129     C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
256500960129     C                   Z-ADD     WOREGI        WR4NOI                         *N.ORE DA AGG.
256600960129     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
256700960129     C                   Z-ADD     WR4POO        GIF(IG)                        *DATA FINE GIACENZA
256800960129     C                   Z-ADD     GIF(IG)       WR7POI
256900960129     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
257000031125     C     WR7POO        IFGT      GIF(IG)
257100031125     C                   Z-ADD     WR7POO        GIF(IG)
257200031125     C                   ENDIF
257300031201     C*
257400031201     C* SE LA DATA ODIERNA CADE OLTRE IL PERIODO DI FINE COPERTURA AFFIDABILITA'
257500031201     C* DATO DALL'EVENTO GIACENZA CORRENTE => VERIFICO CHE ESISTA UNA APERTURA
257600031201     C* GIACENZA DALLA DATA INIZIALE DEL PERIODO DI COPERTURA AFFIDABILITA'
257700031201     C* DELL'EVENTO GIACENZA CORRENTE IN AVANTI; SE NN PRESENTE APERTURA GIACENZA
257800031201     C* VANIFICO LA COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE
257900031201     C* (WPOATT  = DATA ODIERNA "ORIZZATA"
258000031201     C*  GIF(IG) = DATA FINE COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE "ORIZZATA")
258100031201     C                   IF        WPOATT > GIF(IG)
258200031201     C*
258300031201     C* SE LA BOLLA HA PARTICOLARITA' GIACENZA TALI DA PREVEDERE LA NN APERTURA GIACENZA
258400031201     C* NN EFFETTUO I CONTROLLI D CUI SOTTO
258500031201     C                   IF        WGGA = *BLANKS
258600031201     C                   SETOFF                                       54
258700031201     C                   ELSE
258800031201     C     WGGA          LOOKUP    SK7QW                                  54
258900031201     C                   ENDIF
259000031201     C                   IF        *IN54 = *ON                                  * NO APERT. GIACENZA
259100031201     C                   MOVEL     '1'           FLGGIACOK
259200031201     C                   ELSE
259300140127     c
259400031125     C*
259500031125     C* AZZERO FLAG DI WRK CHE INDICA SE PRESENTI APERTURE GIACENZE POSTERIORI ALLA DATA INIZIALE
259600031229     C* COPERTURA AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE (QUESTA INIZIALIZZAZIONE SIGNIFICA
259700031125     C* CHE D DEFAULT SI DA X ASSUNTO CHE NN VI SIA UNA APERTURA GIACENZA)
259800031125     C                   MOVEL     '0'           FLGGIACOK         1
259900031125     C*
260000031125     C* CICLO DI LETTURA DI TUTTE LE GIACENZE DELLA BOLLA
260100031125     C     1             DO        IAG           EG                3 0
260200031125     C* "ORIZZO" LA DATA APERTURA GIACENZA
260300031125     C                   Z-ADD     GCPD(EG)      WDAT                           *DATA GIACENZA
260400031125     C                   EXSR      DATORE
260500031125     C                   Z-ADD     WORE          WGCPDORE          9 0          *DATA GIAC. IN ORE
260600031125     C*
260700031125     C* CALCOLO LA DATA INIZIO COPERTURA AFFIDABILITA' "ORIZZATA" DEL SUCCESSIVO EVENTO GIACENZA
260800031125     C                   IF        E+1 <= IE                                    *EVENTO SUCCESSIVO
260900031125     C                   EVAL      WDAT = EVD(E+1)                              *DATA EVENTO
261000031125     C                   EXSR      DATORE
261100031125     C                   Z-ADD     WORE          WPROXGII          9 0          *PROX INIZIO GIAC.
261200031229     C* SE IL SUCCESSIVO EVENTO GIACENZA HA CODICE 'G' E HA STESSA DATA DELL'APERTURA GIACENZA
261300031229     C* => NN LO CONSIDERO
261400031229     C                   IF        EVC(E+1) = 'G' AND
261500031229     C                             WPROXGII = WGCPDORE
261600031229     C                   MOVEL     '1'           FLGGIACOK
261700031229     C                   ENDIF
261800031125     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA EVENTO
261900031125     C                   MOVEL     'GIA  '       WR3TAB                         *COD.REC.TABELLA
262000031125     C                   Z-ADD     EVO(E)        WR3HHI                         *ORA EVENTO
262100031125     C                   EXSR      RO3ORE
262200031125     C                   ADD       WR3HHO        WPROXGII                       *INIZIO GIACENZA
262300031125     C* EFFETTUO TEST X VERIFICARE SE E' MAGGIORE DELLA DATA DI INIZIO COPERTURA AFFIDABILITA'
262400031125     C* DELL'EVENTO GIACENZA CORRENTE
262500031229     C                   IF        WGCPDORE >= GII(IG) AND                      *DATA GIA>=DATA EV C
262600060227     C                             WGCPDORE <= WPROXGII                         *DATA GIA< DATA EV P
262700031125     C                   MOVEL     '1'           FLGGIACOK
262800031125     C                   LEAVE
262900031125     C                   ENDIF
263000031125     C                   ELSE
263100031229     C                   IF        EVC(E)  = 'G' AND
263200031229     C                             GII(IG) = WGCPDORE
263300031229     C                   MOVEL     '1'           FLGGIACOK
263400031229     C                   LEAVE
263500031229     C                   ENDIF
263600031229     C                   IF        WGCPDORE >= GII(IG)                          *DATA GIA>=DATA EV C
263700031125     C                   MOVEL     '1'           FLGGIACOK
263800031125     C                   LEAVE
263900031125     C                   ENDIF
264000031125     C                   ENDIF
264100031125     C                   ENDDO
264200031201     C                   ENDIF
264300140127     c* Se la data consegna bolla ha la stessa data delle'evento --> lo considero valido
264400140127     C                   IF        FLGGIACOK = '0'  and
264500140127     c                             d98dcm=evd(E)
264600140127     C                   MOVEL     '1'           FLGGIACOK
264700140127     c                   endif
264800031125     C*
264900031125     C* SE NN ESISTE UNA APERTURA GIACENZA PIU' RECENTE RISPETTO ALLA DATA DI INIZIO COPERTURA'
265000031125     C* AFFIDABILITA' DELL'EVENTO GIACENZA CORRENTE => VANIFICO (AZZERO) LA COPERTURA AFFIDABILITA'
265100031125     C* DELL'EVENTO GIACENZA CORRENTE
265200031125     C                   IF        FLGGIACOK = '0'
265300031125     C                   Z-ADD     *ZEROS        GIF(IG)
265400031125     C                   ELSE
265500031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
265600031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
265700031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
265800031125     C                   ENDIF
265900031125     C                   ELSE
266000031125     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
266100031125     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
266200031125     C                   ADD       12            GII(IG)                        *INIZIO GIACENZA
266300031125     C                   ENDIF
266400031125     C*
266500960129     C                   ENDIF                                                  <-- 3 ---
266600960129     C                   ENDDO                                                  <-- 2 ---
266700960129     C*
266800960129     C                   ENDIF                                                  <-- 1 ---
266900960129££$$ C*
267000960129££$$ C* MEMORIZZO LA DATA DI FINE GIACENZA PIU' ALTA
267100960129££$$ C                   Z-ADD     *ZEROS        WPOGIF
267200960129££$$ C     1             DO        IG            I
267300960129££$$ C     GIF(I)        IFGT      WPOGIF
267400960129££$$ C                   Z-ADD     GIF(I)        WPOGIF
267500960129££$$ C                   ENDIF
267600960129££$$ C                   ENDDO
267700941229     C*
267800941229     C*-------------------------------------------------------------------
267900941229     C* APERTURA GIACENZA
268000941229     C*-------------------------------------------------------------------
268100960129     C     D98DAG        IFNE      *ZEROS                                       --- 1 -->
268200960129     C*
268300960129£££  C* TROVO IL PROGRESSIVO ORE DELLA DATA DI APERTURA GIAC.
268400931125     C                   Z-ADD     D98DAG        WDAT
268500930325     C                   EXSR      DATORE
268600950413£££  C*COME INIZIO GIAC. PRENDO SEMPRE LA DATA INFERIORE TRA EVENTO E APER.GIAC.
268700960129$$£  C***********WORE      IFLT WPODAG                     *DATA A.G. < EVENTO
268800950413£££  C                   Z-ADD     WORE          WPODAG
268900960129$$£  C*********************ENDIF
269000930513     C* AGGIUNGO 12 ORE ALL'INIZIO GIAC. PER NON CONSIDERARE VALIDE LE 12 ORE
269100930513     C* SUCCESSIVE ALLA PREVISTA CONSEGNA
269200930513     C*********************ADD  12        WPODAG           *INIZIO GIAC.
269300930513     C* TOLTO ADD 12 PERCHE' SULLA BOLLA LA DATA APERT.GIAC. SI RIFERISCE SPESSO
269400930513     C* AD UNA TENTATA CONSEGNA DEL GIORNO PRECEDENTE
269500941229     C*
269600941229     C* MEMORIZZO APERTURA GIACENZA PER CONTROLLO CONSEGNABILITA'
269700941229     C                   Z-ADD     WPODAG        WPOAGC
269800960129     C*
269900960129     C                   ENDIF                                                  <-- 1 ---
270000941229     C*
270100941229     C*-------------------------------------------------------------------
270200930511     C* CHIUSURA GIACENZA
270300941229     C*-------------------------------------------------------------------
270400930409     C* SE LA GIACENZA NON E' ANCORA CHIUSA MA PER PROBLEMI VARI LA SPEDIZ
270500930409     C* E' GIA' STATA CONSEGNATA FORZO LA DATA DI CHIUSURA GIAC.=DATA CONSEGNA
270600931125     C     D98DAG        IFNE      *ZEROS                                       *E' APERTA
270700931201     C     W98DCG        IFEQ      *ZEROS                                       *NON E' CHIUSA
270800931125     C     D98DCM        IFNE      *ZEROS                                       *E' CONSEGNATA
270900931201     C                   Z-ADD     D98DCM        W98DCG
271000930416     C                   ELSE
271100930416     C* IN OGNI CASO SE APERTA E NON ANCORA CHIUSA AD OGGI IMPOSTO LA DATA
271200930416     C* ODIERNA NELLA DATA CHIUSURA GIACENZA
271300931201     C                   Z-ADD     WDAODI        W98DCG
271400061123     c* se c'e' la data fine consegnabilità, imposto questa nella data
271500061123     c*  chiusura giacenza, come già fa in altri posti
271600061123     c                   if        d98dfc>0
271700061123     C                   Z-ADD     d98dfc        W98DCG
271800061123     c                   endif
271900061123     c
272000930409     C                   END
272100930409     C                   END
272200930409     C                   END
272300930414     C* TRASF.DATA CHIUSURA
272400931201     C     W98DCG        IFNE      *ZEROS
272500931201     C                   Z-ADD     W98DCG        WDAT
272600930325     C                   EXSR      DATORE
272700930326     C                   Z-ADD     WORE          WPODCG
272800930414     C* AGGIUNGO 12 ORE PER CONSIDERARE ANCHE IL GIORNO DI CHIUSURA COME
272900930407     C* GIORNO DA ESCLUDERE
273000150216     C                   ADD       12            WPODCG
273100150216     c* ?? sospeso-non sommo più le 12 ore in quanto voglio dare solo i tempi di consegna
273200150123     c*  il giorno di chiusura dovrebe già essere protetto
273300941229     C* IMPOSTO LA DATA DI FINE GIACENZA PER IL CALCOLO DELLA CONSEGNABILITA'
273400941229     C     WPOCGC        IFEQ      *ZEROS
273500941229     C                   Z-ADD     WPODCG        WPOCGC                         *FINE GIAC.X CONSEGN
273600941229     C                   ENDIF
273700930414     C* AGGIUNGO ORE CONSEGNA TEORICHE ESPRESSO E
273800930414     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
273900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
274000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
274100150123     C***                Z-ADD     W99CCE        WR4NOI                         *N.ORE DA AGGIUNGERE
274200150123     C                   Z-ADD     W99tcc        WR4NOI                         *N.ORE DA AGGIUNGERE
274300930511     C                   Z-ADD     WPODCG        WR4POI                         *DATA RIF.PER CALC.D
274400930511     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
274500930511     C                   Z-ADD     WR4POO        WPODCG                         *DATA FINE GIACENZA
274600931209     C                   Z-ADD     WPODCG        WR7POI
274700931201     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
274800931202     C     WR7POO        IFGT      WPODCG
274900931201     C                   Z-ADD     WR7POO        WPODCG
275000930326     C                   END
275100931202     C                   END
275200930511     C*
275300930511     C                   ENDSR
275400031125     C********************************************************************
275500031125     C*   CARGIAC - CARICA LE DATE DELLE APERTURE GIACENZA DELLA BOLLA CORRENTE
275600031125     C*             E LE "ORIZZA" X RENDERLE OMOGENEE (PARAGONABILI) ALLE DATE
275700031125     C*             DEGLI EVENTI DI TIPO GIACENZA
275800031125     C********************************************************************
275900031125     C     CARGIAC       BEGSR
276000031125     C*
276100031125     C* INIZIALIZZO INDICE DI SCHIERA GIACENZE
276200031125     C                   Z-ADD     *ZEROS        IAG               3 0          *INDICE APERT. GIAC.
276300031125     C*
276400050405     C                   Z-ADD     D98AAS        KWGCPAAS
276500050405     C                   Z-ADD     D98LNP        KWGCPLNP
276600050405     C                   Z-ADD     D98NRS        KWGCPNRS
276700050405     C                   Z-ADD     D98NSP        KWGCPNSP
276800031125     C     D98TBO        IFEQ      'P'                                          * BOLLA IN PARTENZA
276900050405     C     KEYGC_P       SETLL     TIGCP01L
277000050405     C                   IF        %equal(TIGCP01L)
277100050405     C     KEYGC_P       READE     TIGCP01L
277200050405     C                   DOW       not %eof(TIGCP01L)
277300031125     C                   ADD       1             IAG
277400031125     C                   EVAL      GCPD(IAG) = GCPDUR
277500050405     C     KEYGC_P       READE     TIGCP01L
277600031125     C                   ENDDO
277700031125     C                   ENDIF
277800031125     C                   ELSE
277900050405     C     KEYGC_P       SETLL     TIGCP21L
278000050405     C                   IF        %equal(TIGCP21L)
278100050405     C     KEYGC_P       READE     TIGCP21L
278200050405     C                   DOW       not %eof(TIGCP21L)
278300031125     C                   ADD       1             IAG
278400031125     C                   EVAL      GCPD(IAG) = GCPDUR
278500050405     C     KEYGC_P       READE     TIGCP21L
278600031125     C                   ENDDO
278700031125     C                   ENDIF
278800031125     C                   ENDIF
278900031125     C*
279000031125     C                   ENDSR
279100941229     C********************************************************************
279200960129     C*   CALGIC - DETERMINA RANGE GIACENZA PER CONTROLLO CONSEGNABILITA'
279300941229     C*            SE C'E' EVENTO GIACENZA MA NON ANCORA APERTURA GIACENZA
279400941229     C********************************************************************
279500960129     C     CALGIC        BEGSR
279600941229     C*
279700960129     C     FLCONS        IFEQ      'S'                                          *FLAG CONSEGNAB.
279800960129     C*
279900960129     C                   Z-ADD     WOREGI        WTM060
280000960129     C* SE NON E' ANCORA STATA APERTA GIACENZA AGGIUNGO AL FINE GIAC. LE
280100960129     C* ORE DI CONS.CLIENTE PER DARE IN FILIALE IL TEMPO DI APRIRE GIACENZA
280200960129     C     D98DAG        IFEQ      *ZEROS
280300960129$$£  C     WOREGI        ADD       12            WTM060            6 0
280400960129     C                   ENDIF
280500960129     C*
280600960129£$   C                   MOVEL     'C'           WTC                            *CONSEGNA
280700960129£$   C                   MOVEL     'C'           WIC                            *CLIENTE
280800960208£$   C                   Z-ADD     GII(IG)       GCI(IG)                        *DATA INIZ GIAC.CONS
280900960208£$   C                   Z-ADD     GII(IG)       WR4POI            9 0          *DATA RIF.PER CALC.D
281000960129£$   C                   Z-ADD     WTM060        WR4NOI                         *N.ORE DA AGG.
281100960129£$   C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
281200960208£$   C                   Z-ADD     WR4POO        GCF(IG)                        *DATA FINE GIAC.CONS
281300960129     C*
281400960129     C                   ENDIF
281500941229     C*
281600941229     C                   ENDSR
281700060418     C********************************************************************
281800060418     C*   CALCEPA- DETERMINO RANGE PER eventi particolari che ALLUNGANO
281900060418     c*            l'affidabilità
282000060418     C********************************************************************
282100060418     C     CALCEPA       BEGSR
282200060418     C*
282300060418     C* SOLO SE ESISTONO EVENTI particolari che allungano
282400060418    1C     WEvPaA        IFEQ      'S'                                          --- 1 -->
282500060420     c                   clear                   numggcepa         3 0
282600060418     C*
282700060418     C                   Z-ADD     0             IPA               3 0          *INDICE LASC.AVVISI
282800060420     C                   Z-ADD     1             W                 3 0          *INDICE LASC.AVVISI
282900060420     c                   eval      wgiroepa='0'
283000060418     c                   eval      wfine=' '
283100060418     c
283200060418     C* CICLO DI LETTURA DI TUTTI GLI EVENTI DELLA BOLLA
283300060420    2C     W             DOwle     IP                                           --- 2 -->
283400060418     C* SE CAUSALE EVENTO E' DI TIPO eventi particolare che allunga --> elab
283500060420    3C                   IF        pvf(W)='A'                                   --- 3 -->
283600060418     c
283700060420    4c                   if        wgiroepa='0'
283800060420     c                   exsr      ElaInizCEPA
283900060418   x4c                   else
284000060420    5c                   if        pvd(W)>wdat
284100060418     c                   eval      wfine='S'
284200060418    5c                   endif
284300060418    4c                   endif
284400060418     c
284500060420    4c                   if        wfine=' ' and pvd(W)=wdat
284600060420     C*
284700060420     c* Sommo i giorni di eventi che allungano consecutivi per il calcolo
284800060420     c*  finale
284900060420     c                   add       1             numggcepa
285000060420     c
285100060420     C                   z-add     24            WORELA
285200060420     C     wore          add       worela        PAIF(IPA)                      *DATA FINE LASC.AVV.
285300060418     c
285400060418     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
285500060418     C                   MOVEL     'C'           WTC                            *CONSEGNA
285600060418     C                   MOVEL     'C'           WIC                            *CLIENTE
285700060420     C                   Z-ADD     wore          WR4POI            9 0          *DATA RIF.PER CALC.D
285800060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
285900060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
286000060418     c
286100060418     c* Trasformo le ore in data e verifico se c'e' altro evento
286200060418     c*  in sta data che allunga ancora
286300060420     c                   z-add     wr4poo        wore
286400060418     c                   EXSR      OREDAT
286500060418    4c                   endif
286600060418     c
286700060418     C* AGGIUNGO ORE TEORICHE CONSEGNA CLIENTE ALLE ORE DELLA TABELLA
286800060418    4c                   if        wfine='S'
286900060418     c                   EXSR      ELAFINECEPA
287000060420     c* Se elaboro fine non vado avanti col numeratore, per impstare
287100060420     c*  inizio range
287200060420     c                   else
287300060420     c                   add       1             w
287400060418    4C                   ENDif                                                  <-- 3 ---
287500060420     c
287600060420     c                   else
287700060420     c                   add       1             w
287800060418    3C                   ENDif                                                  <-- 3 ---
287900060418    2C                   ENDDO                                                  <-- 2 ---
288000060418     c
288100060420     c                   if        wgiroepa='1'
288200060418     c                   EXSR      ELAFINECEPA
288300060418     c                   endif
288400060418     C*
288500060418    1C                   END                                                    <-- 1 ---
288600060418££$$ C*
288700060418££$$ C* MEMORIZZO LA DATA DI FINE L.AVVISO PIU' ALTA
288800060420££$$ C                   Z-ADD     *ZEROS        WPOpaif
288900060420££$$ C     1             DO        Ipa           w
289000060420££$$ C     paif(w)       IFGT      WPOpaif
289100060420££$$ C                   Z-ADD     paif(w)       WPOpaif
289200060418££$$ C                   END
289300060418££$$ C                   ENDDO
289400060418     C*
289500060418     C                   ENDSR
289600060420     C********************************************************************
289700060420     c* Elabora Inizio range eventi particolari che allungano
289800060420     C********************************************************************
289900060420     c     ElaInizCEPA   BEGSR
290000060420     C                   ADD       1             IPA
290100060420     C                   Z-ADD     PVD(w)        WDAT                           *DATA EVENTO
290200060420     C                   EXSR      DATORE
290300060420     C                   Z-ADD     WORE          PAII(IPA)                      *INIZIO LASC.AVVISO
290400061017     c
290500060420     c                   eval      wgiroepa='1'
290600060420     c                   eval      wdat=pvd(w)
290700060420     C                   ENDSR
290800060418     C********************************************************************
290900060418     c* Elabora fine range eventi particolari che allungano
291000060418     C********************************************************************
291100060418     c     ElafineCEPA   BEGSR
291200060420     c     numggcepa     mult      24            worela
291300060420     c                   sub       12            worela
291400090827     C*****              ADD       D98TCC        WORELA                         *TEMPO TEOR.CONS.
291500090827     C                   ADD       w99TCC        WORELA                         *TEMPO TEOR.CONS.
291600060418     c
291700060418     C                   Z-ADD     paii(IPA)     WR4POI            9 0          *DATA RIF.PER CALC.D
291800060418     C                   Z-ADD     WORELA        WR4NOI                         *N.ORE DA AGG.
291900060418     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
292000060418     C                   Z-ADD     WR4POO        paif(IPA)                      *DATA FINE LASC.AVV.
292100060418     C                   Z-ADD     paif(IPA)     WR7POI
292200060418     C                   EXSR      RO7TCF                                       *CONTR.T.CHIUSURA
292300060418    5C     WR7POO        IFGT      paif(IPA)
292400060418     C                   Z-ADD     WR7POO        paif(IPA)
292500060418    5C                   END
292600061017     c*Sommo 24 ore di inizio giornata
292700061017     c*            : non ho capito perchè ma ci vogliono
292800061017     C****               add       24            paii(IPA)                      *INIZIO LASC.AVVISO
292900090827     C*****              add       d98tcc        paii(IPA)                      *INIZIO LASC.AVVISO
293000090827     C                   add       w99tcc        paii(IPA)                      *INIZIO LASC.AVVISO
293100061017     c* Se tempi di consegna inferiori alle 24 ore , devo
293200061017     c*  togliere 12 ore all'inizio dell'evento particolare per farlo
293300061017     c*  partire prima
293400061017     c****               if        d98tcc<24
293500061017     c****               sub       12            paii(ipa)
293600061017     c****               endif
293700060420     C*******            add       12            paif(IPA)                      *INIZIO LASC.AVVISO
293800060418     c* Imposto nuovo giro
293900060420     c                   eval      wgiroepa='0'
294000060418     c                   eval      wfine=' '
294100060420     c                   clear                   numggcepa         3 0
294200060418     c                   ENDSR
294300930511     C********************************************************************
294400930511     C*   CALASS - DETERMINO RANGE PER CONTRASSEGNO
294500930511     C********************************************************************
294600010822     C     CALASS        BEGSR
294700010821     C*
294800011211     C                   EXSR      REPMAXCSB
294900930511     C*
295000930511     C* ESEGUO CALCOLO SOLO SE ESISTE UN IMPORTO CONTRASSEGNO
295100010820     C     D98CAS        IFNE      *ZEROS                                       --- 1 -->
295200011211     C     O21FCA        IFNE      *BLANKS                                      *NON ABIL---3-->
295300930608     C* ---> CALCOLO RANGE INTERNO PER CONTRASSEGNO
295400930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
295500930604     C*   (SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI USO
295600930608     C*   LA DATA PREVISTA DI ARRIVO TRAZIONE INTERNA)
295700930511     C     D98DTI        IFEQ      *ZEROS                                       --- 4 -->
295800930608     C                   Z-ADD     WPOPTI        WPOASI            9 0          *PREV.ARR.TRAZ.INT.-
295900930511     C                   ELSE                                                   <-- 4 -->
2960009407151507 C                   Z-ADD     WPODTI        WPOASI                         *ARR.TRAZ.REALE ->
296100930512     C                   END                                                    <-- 4 ---
296200930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE CONT
296300930608     C                   Z-ADD     WPOASI        WPOAII                         *INIZIO RANGE INTERN
296400930608     C                   Z-ADD     WPOASF        WPOAFI                         *FINE RANGE INTERNO
296500930608     C* ---> CALCOLO RANGE CLIENTE PER CONTRASSEGNO
296600930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
296700930608     C                   Z-ADD     WPOPTC        WPOASI            9 0          *PREV.ARR.TRAZ.CLI.-
296800930608     C                   EXSR      RDRASS                                       *ROUT.DET.RANGE.CONT
296900930608     C                   Z-ADD     WPOASI        WPOAIC                         *INIZIO RANGE CLIENT
297000930608     C                   Z-ADD     WPOASF        WPOAFC                         *FINE RANGE CLIENTE
297100930413     C                   END                                                    <-- 2 ---
297200011211     C                   END                                                    <-- 1 ---
297300930406     C*
297400930325     C                   ENDSR
297500930608     C********************************************************************
297600930608     C*   RDRASS - ROUTINE PER DETERMINARE RANGE CONTRASSEGNO
297700930608     C*            INPUT:  - WPOASI (PROGR.ORE INIZIO CONTRASS.)
2978009407151507 C*********************- WHHASI (ORA INIZIO CONTRASS.)
297900930608     C*            OUTPUT: - WPOASI (PROGR.ORE INIZIO CONTRASS.)
298000930608     C*                    - WPOASF (PROGR.ORE FINE CONTRASS.)
298100930608     C********************************************************************
298200930608     C     RDRASS        BEGSR
298300930608     C*
298400930608     C* AGGIUNGO LE EVENTUALI ORE DOVUTE ALL'ARROTONDAMENTO ORA CONTRASSEGNO
2985009407151507 C*********************MOVEL'ASS  '   WR3TAB           *COD.REC.TABELLA
2986009407151507 C*********************Z-ADDWHHASI    WR3HHI           *ORA ARRIVO
2987009407151507 C*********************EXSR RO3ORE
2988009407151507 C*********************ADD  WR3HHO    WPOASI           *INIZIO CONTRASSEGNO
298900930608     C* ---> CALCOLO ORE DEL CONTRASSEGNO
2990009407151507 C*********** AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA (DA TAB. 'ASS')
2991009407151507 C*********************Z-ADDWR3HHA    WOREAS           *ORE FINE GIORN.->
2992009407151507 C* AGGIUNGO ORE PER ARRIVARE A FINE GIORNATA
2993009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
2994009407191907 C                   Z-ADD     WPOASI        WPOROU
2995009407191907 C                   EXSR      CHKFES
2996009407191907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
2997009407191907 C                   Z-ADD     24            WOREAS                         *CONCEDO IL 1° GG FE
2998009407191907 C                   ELSE                                                   *E' FERIALE
2999009407151507 C                   Z-ADD     WPOASI        WPORE             9 0          *ORE INIZIO CONTR. -
3000009407151507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3001009407151507 C                   Z-ADD     WNORE         WOREAS                         *-> ORE CONTRASS.
3002009407191907 C                   ENDIF
300300930608     C* AGGIUNGO TEMPO STANDARD PER CONTRASSEGNO (48 ORE ?)
300400930608     C                   MOVEL     'ASS1 '       WR3TAB
300500930608     C                   Z-ADD     *ZEROS        WR3HHI
300600930608     C                   EXSR      RO3ORE
300700930608     C                   ADD       WR3HHO        WOREAS                         *ORE TAB. ->
300800930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
300900931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
301000931129     C                   MOVEL     'C'           WIC                            *CLIENTE
301100930608     C                   Z-ADD     WPOASI        WR4POI            9 0          *DATA RIF.PER CALC.D
301200930608     C                   Z-ADD     WOREAS        WR4NOI                         *N.ORE DA AGG.
301300930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
301400930608     C                   Z-ADD     WR4POO        WPOASF                         *DATA FINE CONTRASS.
301500930608     C*
301600930608     C                   ENDSR
301700930511     C********************************************************************
301800930511     C*   CALFED - DETERMINO RANGE PER FERMO DEPOSITO
301900930511     C********************************************************************
302000930511     C     CALFED        BEGSR
302100150331     c                   clear                   fddata
302200150331     c                   clear                   WPOFDdata
302300930511     C*
302400051108     C* ESEGUO CALCOLO SOLO SE FLAG FERMO DEPOSITO = S o = M
302500931125     C     D98FFD        IFEQ      'S'                                          --- 1 -->
302600051108     C     D98FFD        oreq      'M'                                          --- 1 -->
302700930608     C* -----> CALCOLO RANGE INTERNO PER FERMO DEPOSITO
302800930511     C* PRENDO COME INIZIO RANGE LA DATA DI ARRIVO TRAZIONE
302900930511     C*    SE GIA' GESTITA LA DATA DI ARRIVO TRAZIONE USO QUELLA ALTRIMENTI LA
303000930608     C*    DATA PREVISTA DI ARRIVO TRAZIONE INTERNA
303100150331     c*    in realtà prendo la più ala tra questa e la data di inserimento FD
303200150331     c                   if        §ar5dfd>*zeros
303300150331     c                   movel     §ar5dfd       fddata            8 0
303400150331     c                   endif
303500150331     c                   if        fddata>0 and ((d98dti>0 and
303600150331     c                             fddata>d98dti) or d98dti=0)
303700150331     C                   Z-ADD     fddata        WDAT
303800150331     C                   EXSR      DATORE
303900150331     C                   Z-ADD     WORE          WPOfddata
304000150331     c                   endif
304100150331     c
304200930511     C     D98DTI        IFEQ      *ZEROS                                       --- 2 -->
304300930608     C                   Z-ADD     WPOPTI        WPOFDI                         *PREV.ARR.TRAZ.INT.
304400930511     C                   ELSE                                                   <-- 2 -->
3045009407151507 C                   Z-ADD     WPODTI        WPOFDI                         *ARR.TRAZ.REALE ->
304600930512     C                   END                                                    <-- 2 ---
304700150331     c                   if        wpofddata>0 and wpofddata>wpofdi
3048001504081507 C                   Z-ADD     WPOfddata     WPOFDI
304900150331     c                   endif
305000150331
305100930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
305200930608     C                   Z-ADD     WPOFDI        WPOFII                         *INIZIO RANGE INTERN
305300930608     C                   Z-ADD     WPOFDF        WPOFFI                         *FINE RANGE INTERNO
305400930608     C* ---> CALCOLO RANGE CLIENTE PER FERMO DEPOSITO
305500930608     C* PRENDO COME INIZIO RANGE LA DATA DI PREVISTO ARRIVO TRAZIONE CLIENTE
305600930608     C                   Z-ADD     WPOPTC        WPOFDI            9 0          *PREV.ARR.TRAZ.CLI.-
305700930608     C                   EXSR      RDRFED                                       *ROUT.DET.RANGE F.DE
305800930608     C                   Z-ADD     WPOFDI        WPOFIC                         *INIZIO RANGE CLIENT
305900930608     C                   Z-ADD     WPOFDF        WPOFFC                         *FINE RANGE CLIENTE
306000930511     C                   END                                                    <-- 1 ---
306100930511     C*
306200930511     C                   ENDSR
306300930608     C********************************************************************
306400930608     C*   RDRFED - ROUTINE PER DETERMINARE RANGE FERMO DEPOSITO
306500930608     C*            INPUT:  - WPOFDI (PROGR.ORE INIZIO F.DEP.)
3066009407151507 C*********************- WHHFDI (ORA INIZIO F.DEP.)
306700930608     C*            OUTPUT: - WPOFDI (PROGR.ORE INIZIO F.DEP.)
306800930608     C*                    - WPOFDF (PROGR.ORE FINE F.DEP.)
306900930608     C********************************************************************
307000930608     C     RDRFED        BEGSR
307100930608     C*
307200930608     C* CALCOLO ORE DEL FERMO DEPOSITO
307300930608     C* ORE PER ARRIVARE FINE GIORNATA + TEMPO TAB. = N.ORE F.DEPOSITO
3074009407191907 C* CONTROLLO SE GIORNO DI ARRIVO DELLA TRAZIONE E' FESTIVO
3075001503311907 C                   Z-ADD     WPOFDI        WPOROU
3076001503311907 C                   EXSR      CHKFES
3077001503311907 C     WSLITT        IFEQ      'S'                                          *E' FESTIVO
3078001503311907 C                   Z-ADD     24            WOREFD                         *CONCEDO IL 1° GG FE
3079001503311907 C                   ELSE                                                   *E' FERIALE
3080001503311507 C                   Z-ADD     WPOFDI        WPORE             9 0          *ORE INIZIO FD  ->
3081001503311507 C                   EXSR      FINGIO                                       *CALC. ORE A FINE GI
3082001503311507 C                   Z-ADD     WNORE         WOREFD                         *-> ORE DI FERMO DEP
3083001503311907 C                   ENDIF
308400150331     c* Se presente la particolarità consegna con più giorni di fermo depo
308500150331     c*  uso questi
308600150331     c                   if        §7rggfd>*zeros
308700150424     c                   movel     §7rggfd       wggfd             2 0
308800150331     c     wggfd         mult      24            Wnore
308900150331     c                   add       wnore         worefd
309000150331     c                   else
309100930608     C                   MOVEL     'FED1 '       WR3TAB
309200930608     C                   Z-ADD     *ZEROS        WR3HHI
309300930608     C                   EXSR      RO3ORE
309400940715     C                   ADD       WR3HHO        WOREFD                         *ORE TAB. -> ORE FD
309500150331     c                   endif
309600930608     C* LANCIO ROUT. CHE DA UNA DATA + ORE MI DA DATA RISULTANTE (SALTA FESTIVI)
309700931129     C                   MOVEL     'C'           WTC                            *CONSEGNA
309800931129     C                   MOVEL     'C'           WIC                            *CLIENTE
309900930608     C                   Z-ADD     WPOFDI        WR4POI            9 0          *DATA RIF.PER CALC.D
310000930608     C                   Z-ADD     WOREFD        WR4NOI                         *N.ORE DA AGG.
310100930608     C                   EXSR      RO4CDP                                       *ROUT.CALC.DATA
310200930608     C                   Z-ADD     WR4POO        WPOFDF                         *DATA FINE F.DEPOSIT
310300930608     C*
310400930608     C                   ENDSR
310500931201     C********************************************************************
310600931201     C*   CALDTE - CALCOLA DATE TEORICHE DI CONSEGNA
310700931201     C********************************************************************
310800931201     C     CALDTE        BEGSR
310900140825     c
311000140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA *******************
311100140825     c*  che tiene conto solo dei festivi per restituire in output
311200140825     c*  la data teorica di consegna
311300140825     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
311400140825     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
311500140825     C                   Z-ADD     WPOPTI        WR4POI            9 0          *DATA PRE.ARR.TR.INT
311600140825     C                   ELSE
311700140825     C                   Z-ADD     WPODTI        WR4POI                         *DATA ARRIVO TRAZ.
311800140825     C                   END
311900140825     C                   Z-ADD     w99TCI        WR4NOI                         *ORE TEOR.CONS.INT.
312000141106     c* non devo sommare le prime 12 ore in ro4cdp altrimenti sbaglia
312100141106     c*  quindi passo un flag
312200141106     c                   eval      nosomma='N'
312300140825     C                   EXSR      RO4CDP
312400141106     c                   eval      nosomma=' '
312500140825     C                   Z-ADD     WR4POO        WPODEI                         *DT.CONS.PREV.INTERN
312600140825     c* La trasforno subito in data perchè mi serve per reperire gli orari
312700140825     c*  di consegna
312800140825     c                   EXSR      TRAS_DEI
312900140825     c
313000140825     c* Se sono previsti giorni di chiusura P o M reperisco i tempi di consegna
313100140825     c                   if        wTRULOR =' ' and (%subst(d98gc1:2:1)='M' or
313200140825     c                             %subst(d98gc1:2:1)='P' or %subst(d98gc2:2:1)
313300140825     c                             ='M' or %subst(d98gc2:2:1)='P')
313400140825     c                   exsr      Reper_tempi
313500140825     c                   endif
313600931201     C*
313700140825     C* DETERMINO LA DATA TEORICA DI CONSEGNA INTERNA ai fini della CON.RICH ****************
313800140825     c*  che tiene conto di:
313900140825     c*  festivi / C/Ass / f.deposito / turno chiusura / eventi partic
314000150605     c*  per determinare la data FINE consegna richiesta che non deve essere <
314100931201     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
314200931201     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
314300931201     C                   Z-ADD     WPOPTI        WR6POI            9 0          *DATA PRE.ARR.TR.INT
314400931201     C                   ELSE
314500931201     C                   Z-ADD     WPODTI        WR6POI                         *DATA ARRIVO TRAZ.
314600931201     C                   END
314700090827     C***                Z-ADD     D98TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314800090827     C                   Z-ADD     w99TCI        WR6NOI                         *ORE TEOR.CONS.INT.
314900931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA TEOR
315000140825     C                   Z-ADD     WR6POO        WPODEI_dcr                     *DT.CONS.PREV.INTERN
315100931201     C*
315200150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per affidabilità ***
315300931201     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
315400931201     C                   Z-ADD     WPOPTC        WR6POI                         *DATA PRE.ARR.TR.CLI
315500090827     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
315600931201     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
315700150608     C                   Z-ADD     WR6POO        WPOaff_dcr                     *DATA PREV.CONS.CLI.
315800150608     c
315900150608     C* DETERMINO LA DATA TEORICA DI CONSEGNA ESTERNA per la DEE*********
316000150608     c*  che non tiene conto dei tempi del C/Ass
316100150608     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
316200150609     c*
316300150609     c* uso la data più bassa tra la data arrivo trazione e la data teorica di arrivo
316400150609     c*  partendo dalla data reale di partenza o teorica di partenza se manca
316500150630     c                   if        wpodti >0 and wpodti <wpoptc_dee
316600150630     c                   Z-ADD     WPODTI        WR6POI
316700150609     c                   else
316800150609     C                   Z-ADD     WPOPTC_dee    WR6POI                         *DATA PRE.ARR.TR.CLI
316900150609     c                   endif
317000150609     c
317100150608     C                   Z-ADD     w99TCC        WR6NOI                         *ORE TEOR.CONS.CLI.
317200150608     c                   eval      wdee='S'
317300150608     C                   EXSR      RO6CDT                                       *ROUT.CALC.DATA PREV
317400150608     c                   eval      wdee=' '
317500150608     C                   Z-ADD     WR6POO        WPODEE_dcr                     *DATA PREV.CONS.CLI.
317600141017
317700141017     c* Verifico se il giorno odierno è festivo: se festivo slitto in avanti
317800150219     C**                 EVAL      WTC='C'
317900150219     C**                 EVAL      WIC='C'
318000150219     c**                 z-add     wpoattdata00  WPOROU
318100150219     c**                 z-add     wdaodi        wpodata
318200150219     c**                 exsr      chkfes
318300150219     c**                 if        wslitt='S'
318400150219     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
318500150219     c**                 z-add     24            wr6noi
318600150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
318700150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
318800150219     c**                 else
318900150219     C**                 Z-ADD     Wpoattdata00  WPOrifDatLav      9 0          data attuale 1°lavor
319000150219     c**                 endif
319100141204     c* Ricalcolo WPOrifDatLav
319200141229     C**                 MOVEL     'OSC  '       WR3TAB
319300141229     C**                 Z-ADD     WHHODI        WR3HHI
319400141229     C**                 EXSR      RO3ORE
319500141229     c**
319600141229     C**                 EVAL      WTC='C'
319700141229     C**                 EVAL      WIC='C'
319800141229     c**                 z-add     wpoattdata00  WPOROU
319900141229     C**                 Z-ADD     WPOROU        WR6POI            9 0          *DATA RIF.PER CALC.D
320000141229     c**                 z-add     wr3hho        wr6noi
320100141229     c**                 add       12            wr6noi
320200141229     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
320300141229     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
320400141229     c
320500141017     c
320600141017     c* Se è presente la consegnabilità, uso questa al posto della data del giorno
320700141017     c*  ma sempre devo slittare i festivi
320800141017     C* CONSEGNABILITA'
320900150219     C**   FLCONS        IFEQ      'S'                                          --- 0 -->
321000150219     C**   D98DFC        andne     *ZEROS                                       --- 1 -->
321100150219     C**                 EVAL      WTC='C'
321200150219     C**                 EVAL      WIC='C'
321300150219     c**                 z-add     d98dfc        WPOdata
321400150219     c**                 exsr      chkfes
321500150219     c**                 if        wslitt='S'
321600150219     c**                 z-add     wpodfc_00     Wr6poi
321700150219     c**                 z-add     24            wr6noi
321800150219     C**                 EXSR      RO6cdt                                       *ROUT.CALC.DATA
321900150219     C**   wr6poo        sub       12            WPOrifDatLav      9 0          data attuale 1°lavor
322000150219     c**                 else
322100150219     C**                 Z-ADD     Wpodfc_00     WPOrifDatLav      9 0          data attuale 1°lavor
322200150219     c**                 endif
322300150219     c**                 endif
322400931201     C*
322500931201     C                   ENDSR
322600930511     C********************************************************************
322700930511     C*   CALDPC - CALCOLA DATE PREVISTE DI CONSEGNA
322800930511     C********************************************************************
322900930511     C     CALDPC        BEGSR
323000930325     C*
323100930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA INTERNA *******************
323200930512     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
323300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
323400930512     C                   Z-ADD     WPOPTI        WR1POI            9 0          *DATA PRE.ARR.TR.INT
323500930325     C                   ELSE
323600930326     C                   Z-ADD     WPODTI        WR1POI                         *DATA ARRIVO TRAZ.
323700930407     C                   END
323800090827     C*****              Z-ADD     D98TCI        WR1NOI                         *ORE TEOR.CONS.INT.
323900090827     C                   Z-ADD     w99TCI        WR1NOI                         *ORE TEOR.CONS.INT.
324000930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
324100090902     c
324200931202//// C     WR1POO        SUB       12            WPODCI                         *DT.CONS.PREV.INTERN
324300090901
324400930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
324500930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
324600930423     C* A QUELLA DI RICHIESTA CONSEGNA,
324700930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
324800931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
324900931206     C     WPODCI        IFLE      WPOCR1                                       *DT PR.CO.< DT RI.CO
325000931206     C     WPOCR1        SUB       12            WPODCI                         *DT.CONS.RICH.FINE->
325100931206     C                   END
325200930423     C                   END
325300930325     C*
325400930325     C* DETERMINO LA DATA PREVISTA DI CONSEGNA ESTERNA *******************
325500930405     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
325600930414     C                   Z-ADD     WPOPTC        WR1POI                         *DATA PRE.ARR.TR.CLI
325700090827     C*****              Z-ADD     D98TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325800090827     C                   Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
325900930326     C                   EXSR      RO1CDP                                       *ROUT.CALC.DATA PREV
326000931202//// C     WR1POO        SUB       12            WPODCE                         *DATA PREV.CONS.CLI.
326100930423     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
326200930423     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
326300930423     C* A QUELLA DI RICHIESTA CONSEGNA,
326400930423     C* FORZO NELLA DATA DI PREVISTA CONSEGNA LA DATA DI FINE RANGE RICH.CONS.
326500931125     C     D98DCR        IFNE      *ZEROS                                       *C'E' DATA CONS.RICH
326600931206     C     WPODCE        IFLE      WPOCR2                                       *DT PR.CO.< DT RI.CO
326700931206     C     WPOCR2        SUB       12            WPODCE                         *DT.CONS.RICH.FINE=>
326800931206     C                   END
326900930423     C                   END
327000140822     C*
327100930324     C                   ENDSR
327200150113     C********************************************************************
327300150113     C* DETERMINO LA nuova data"Prevista consegna Esterna"***************
327400150113     C********************************************************************
327500150113     c     Cal_DEE       BEGSR
327600150113     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
327700150508     c                   clear                   wtempi00C         1
327800150219     c
327900150219     c* 1) Se presente l'arrivo della trazione lo uso
328000150219    1c                   select
328100150219     c                   when      wmancatraz=' '  and d98dti>0
328200150630     c                   exsr      Ric_traz
328300150630     c                   Z-ADD     WPODTIC       WR1POI
328400150219     c
328500150219     c* 2) Se presente  IMP/disguido devo ricalcolare da qui
328600150219    1c                   when      IDdata>0
328700150325     c                   exsr      CalcoloIMP
328800150528     c* stato IMP/disguido solo se >  alla data di oggi
328900150528     c                   if        wr1poi > wpoattdata00
329000150427     c                   eval      d98spcdee='IMP'
329100150427     c                   endif
329200150219     c
329300150219     c                   other
329400150225     c* 3)  collo partito ma non ancora arrivato: fino all'ora termine picking
329500150225     c* devo far finta che sia arrivato.
329600150508     c* uso i tempi teorici partenza + trazione cliente per la data prevista cons
329700150508     c*  che tiene conto della data partenza PRIMO  collo
329800150508     C                   Z-ADD     WPOPTC_DEE    WR1POI                         *DATA PRE.ARR.TR.CLI
329900150508     c*
330000150508     c*  se maggiore di oggi potrebbe essere stata una doppia partenza --> imp
330100150508     c                   if        wr1poi >=wpoattdata00
330200150508     c                              and WPOPTC_DEE > WPOPTC
330300150508     c                   eval      d98spcdee='IMP'
330400150508     c                   endif
330500150219     c                   endsl
330600150113     c
330700150219     c* se il calcolo è < di oggi --> imposto oggi
330800150113     c                   if        wr1poi < wpoattdata00
330900150113     c                   eval      wr1poi= wpoattdata00
331000150508     c* I tempi di consegna cliente da considerare sono quelli entro soglia
331100150508     c                   eval      wTempi00C='S'
331200150113     c                   endif
331300150217     c
331400150330     c* se il calcolo è maggiore di oggi non controllo l'orario limite picking
331500150427     c*  e non scrivo in lavorazione ma cons prevista ( se non è in imp)
331600150427    1c                   if        wr1poi>wpoattdata00  and
331700150330     c                             (deeindist<>'S' or d98cons<>'S'
331800150330     c                             or d98ddc>wdaodi)
331900150427     c
332000150427     c* Se stato IMP lo mantengo per avere messaggio diverso
332100150427     c                   if        d98spcdee<>'IMP'
332200150304     c                   eval      d98spcdee='PRV'
332300150427     c                   endif
332400150427     c
332500150326   x1c                   else
332600150330     c                   exsr      CAL_OLP
332700150217     c
332800150528     c* Se passato l'orario oppure data distinta < data di oggi
332900150508     c* 1) spedizione in distinta          --> in consegna
333000150528    2C                   IF        WHHODI>PICKLIM or (d98cons='S' and
333100150528     c                             d98ddc<wdaodi)
333200150508    3c                   IF        deeindist='S' and d98cons='S'
333300150508     c                   eval      d98spcdee= 'DDC'
333400150508
333500150508   x3c                   else
333600150507     c
333700150507    4c                   if        wr1poi<=wpoattdata00
333800150507     c                   add       24            wr1poi
333900150507    4c                   endif
334000150507     c
334100150507     c                   eval      d98spcdee='OLP'
334200150507     c* memorizzo orario limite picking
334300150507     c                   eval      d98picklim=%editc(picklim:'X')
334400150507    3c                   endif
334500150507    2c                   endif
334600150507     c
334700150507    4c**                 if        §vpoolpnu>0  and §VPOOLPMN>0
334800150507    5c**                 do        §vpoolpnu
334900150507     c**                 add       §VPOOLPMN     picklim
335000150507     c**                 eval      wora=picklim
335100150507     c**                 eval      wdiff=70
335200150330     c
335300150507     c**                 dow       wdiff>=0
335400150507     c**                 eval      wdiff=wmn-60
335500150507     c**                 if        wdiff>=0
335600150507     c**                 eval      wmn=wmn-60
335700150507     c**                 eval      who=who+1
335800150507     c**                 endif
335900150507     c**                 enddo
336000150507     c**                 eval      picklim=wora
336100150507    6c**                 if        picklim>whhodi
336200150507     c**                 leave
336300150507    6c**                 endif
336400150507    5c**                 enddo
336500150507    4c**                 endif
336600150507    3c**                 endif
336700150326
336800150427     c*    2) non più in distinta o passati i tentativi di massimo avanzamento automatico:
336900150326     c*                           imposto stato OLP prevista cons NON modificabile
337000150507    3c**                 if        whhodi>picklim
337100150507    1c**                 if        wr1poi<=wpoattdata00
337200150507     c**                 add       24            wr1poi
337300150507     c**                 endif
337400150330     c
337500150507     c**                 eval      d98spcdee='OLP'
337600150507    3c**                 endif
337700150507    2c**                 endif
337800150507    c
337900150326     c* memorizzo orario limite picking
338000150507     c**                 eval      d98picklim=%editc(picklim:'X')
338100150326    1c                   endif
338200150217     c
338300150113     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
338400150113     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
338500150113     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
338600150113     C                   Z-ADD     Wr1poi        W10POI            9 0
338700150113     C                   EXSR      R10FES
338800150113     C                   Z-ADD     W10POO        wr1poi
338900150113     C
339000150508     c* Se devo sommare i tempi entro soglia --> lo faccio
339100150629    1c                   if        wtempi00C='S'
339200150508     c                   z-add     W990CN        wr1noi
339300150629   x1c                   else
339400150629     c* In base all'arrivo ultimo collo uso tempo 00 o tempi 12
339500150629    2c                   if        WTSogliaE='0'
339600150629     C                   Z-ADD     w990CN        WR1NOI                         *ORE TEOR.CONS.CLI.
339700150629     c                   else
339800150629     C                   Z-ADD     w991CN        WR1NOI                         *ORE TEOR.CONS.CLI.
339900150629    2c                   endif
340000150629    1c                   endif
340100150629     c
340200150629    1c                   if        WR1NOI=10
340300150629     c                   z-add     12            wr1noi
340400150629     c                   endif
340500150629     C******             Z-ADD     w99TCC        WR1NOI                         *ORE TEOR.CONS.CLI.
340600150629    1c******             endif
340700150508     c
340800150605     c                   eval      wdee='S'
340900150113     C                   EXSR      RO1CDP
341000150605     c                   eval      wdee=' '
341100150113//// C     WR1POO        SUB       12            WPOdpcee                       *DATA PREV.CONS.CLI.
341200150113     C* SE ESISTE LA DATA DI CONSEGNA RICHIESTA IL.. , DOPO IL.. O PRIMA DEL..
341300150113     C* E SE LA DATA DI PREVISTA CONSEGNA APPENA CALCOLATA E' INFERIORE
341400150113     C* A QUELLA DI RICHIESTA CONSEGNA,
341500150331     C* CONSEGNA RICHIESTA: faccio dei calcoli a parte. In particolare
341600150331     c*  1) PRIMA DEL -> calcolo normale perchè devo indciare il primo giorno utile per la cons
341700150331     c*  2) DOPO  IL  -> non devo dare tempo aggiuntivo ma il primo giorno dopo la consegna
341800150331     c*  2) IL        -> è il giorno indicato
341900150331     C     D98DCR        IFNE      *ZEROS
342000150331     C     D98tcr        andne     'P'
342100150331     C***  WPOdpcee      IFLE      WPOCR2
342200150331     C***  WPOCR2        SUB       12            WPOdpcee
342300150330     c
342400150330     c* utilizzo la nuova data a cui non ho sommato i tempi di copertura perchè devo
342500150330     c*   dare la prima data utile di consegna
342600150330     C     WPOdpcee      IFLE      WPOCRF_dee
342700150330     C**** WPOCRF_dee    SUB       12            WPOdpcee
342800150330     C                   z-add     WPOCRF_dee    WPOdpcee
342900150409     C* SE PRESENTE CON RICHIESTA > di oggi tolgo la scritta PRG e metto PRV
343000150409     c                   if        wpodpcee>wpoattdata00
343100150409     c                   eval      d98spcdee='PRV'
343200150409     c                   endif
343300150113     C                   END
343400150331     c
343500150220     c
343600150113     C                   END
343700150220     c
343800150113     c                   ENDSR
343900150330     C********************************************************************
344000150330     C*   Calcolo orario da tabella OLP
344100150330     C********************************************************************
344200150330     c     Cal_OLP       BEGSR
344300150330     c                   eval      picklim=0900
344400150330     c* Dopo l'ora limite picking, passo al giorno dopo
344500150330     C                   MOVEL(P)  'OLP'         TBECOD
344600150330     C                   MOVEL(P)  d98lna        TBEKE1
344700150330     C                   MOVEL(P)  wdaodi        TBEKE2
344800150330     C     KTBE          CHAIN     TNTBE01L
344900150330     C                   IF        not %found(TNTBE01L)
345000150330     c                   clear                   tbeke2
345100150330     C     KTBE          CHAIN     TNTBE01L
345200150330     c                   endif
345300150330     C                   IF        not %found(TNTBE01L)
345400150330     C                   MOVEL(P)  '000'         TBEKE1
345500150330     C     KTBE          CHAIN     TNTBE01L
345600150330     c                   endif
345700150330     c
345800150330     C                   IF        %found(TNTBE01L) and
345900150330     c                             %subst(tbeuni:1:4)>*zeros
346000150330     c                   eval      picklim=%int(%subst(tbeuni:1:4))
346100150330     c                   endif
346200150330     c                   ENDSR
346300150325     C********************************************************************
346400150325     C*   Riclcolo data se colli in IMP/disguido
346500150325     C********************************************************************
346600150325     c     CalcoloIMP    BEGSR
346700150325     C                   Z-ADD     IDdata        WDAT
346800150325     C                   EXSR      DATORE
346900150325     C                   Z-ADD     WORE          WR4POI
347000150325     c* calcolo i tempi di trazione dalla fil disguido/IMP alla lna
347100150325    2c                   if        IDfgs<>D98lnp
347200150325     C                   Z-ADD     IDfgs         LTP(1)                         LINEA   /LINEA
347300150325     C                   Z-ADD     IDfgs         LTP(3)                         LINEA   /TERMINAL
347400150325     c* calcolo il terminal di partenza
347500150325     c                   clear                   fnlv55ds
347600150325     c                   eval      d55tla='L'
347700150325     c                   eval      d55tpt='P'
347800150325     c                   eval      d55lin=IDfgs
347900150325     c                   eval      d55drf=IDdata
348000150325     c                   call      'FNLV55R'
348100150325     c                   parm                    fnlv55ds
348200150325     C                   Z-ADD     D55TFP        LTP(2)                         TERMINAL/LINEA
348300150325     C                   Z-ADD     D55TFP        LTP(4)                         TERMINAL/TERMINAL
348400150325     c                   EXSR      RTTRAZ
348500150325    3c                   if        d98err<>'1'
348600150325     C                   Z-ADD     CASTcn        wr4noi
348700150325     c                   else
348800150325     C                   Z-ADD     d98ttc        wr4noi
348900150325    3C                   endif
349000150325
349100150325   x2c                   else
349200150325     C                   Z-ADD     d98ttc        wr4noi
349300150325    2C                   endif
349400150325     c* se si tratta di IMP aggiungo 24 ore ai tempi di trazione
349500150325     c*  perchè partirà un giorno dopo
349600150325    3c                   if        IDnpg='P'
349700150325     c                   add       24            WR4noi
349800150325    3c                   endif
349900150325     c
350000150325     c                   exsr      ro4cdp
350100150325     C                   Z-ADD     WR4POO        WR1POI                         *DATA PRE.ARR.TR.CLI
350200150325     c                   ENDSR
350300930604     C********************************************************************
350400930604     C*   TPODAT - TRASFORMA I PROGRESSIVI ORE IN DATE
350500930604     C********************************************************************
350600930604     C     TPODAT        BEGSR
350700960229     C*
350800960229     C     D98TLA        IFEQ      'T'                                          --- 1 -->
350900960229     C*
351000960229     C* TRASFORMO LA DATA DI PREVISTO ARRIVO DELLA TRAZIONE *****
351100960229     C     WPOPTI        IFEQ      *ZEROS
351200960229     C                   Z-ADD     *ZEROS        D98DTI
351300960229     C                   Z-ADD     *ZEROS        D98HTI
351400960229     C                   ELSE
351500960229     C                   Z-ADD     WPOPTI        WORE
351600960229     C                   EXSR      OREDAT
351700960229     C                   Z-ADD     WDAT          D98DTI
351800960229     C     WRES          MULT      100           D98HTI
351900960229     C                   END
352000960229     C*
352100960229     C                   ELSE                                                   <-- 1 -->
352200930604     C*
352300931202     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA *******************
352400140825     c* l'ho spostata sopra. non vedo controindicazioni perchè mi serve la data già calcolata
352500140825     c*******            exsr      Tras_DEI
352600140825     c
352700930604     C*
352800150113     C* Calcolo la nuova "Data Prevista Consegna esterna" ******************************
352900140820     c* La data serve da visualizzare in Int Bolle e negli Allert al cliente
353000140820     c* è data da :
353100140820     c* data spedizione + tempi partenza cliente + tempi trazione cliente + tempi cons cliente
353200140820     c*  Tenere  conto, ovvero slittare in avanti, per :
353300140820     c* 1) Festivi         2) C/Assegno           3) Turni chiusura(escluso P-pomeriggio)
353400140820     c* 4) Lasciato Avviso 5) Consegna Richiesta
353500140911     c*  Se la data così  calcolata è minore di oggi: proporre il primo giorno lavorativo
353600140911     c*
353700140820     c*  Da azzerare nei seguenti casi:
353800150324     c* 1) Bolla Consegnata   2) con C.A.  3) bloccata/rimpallo
353900150324     c* 4) bolla export    7) Cod manca consegna
354000140911     c* 8) bolla con DCR DOPOil o PRIMAdel
354100150113     c* 9) appuntamento o supermercato SENZA consegna richiesta
354200150119     c*10) se completamente non arrivata e collo in IMP
354300150119     c
354400140911     c*  Se bolla in distinta : la data diventa uguale alla data distinta
354500150119     c*  tranne alcuni casi in cui rimare a zero anche se in distinta(vedi flg deeindist)
354600140925     C*  - FD   - export -
354700150217     c                   movel     'S'           DEEinDIST         1
354800150113     c                   eval      wcaldpcee='S'
354900150304     c                   eval      D98SPCDEE='PRV'
355000150324     c                   clear                   avvcmc            1
355100150323     c* se c'è mancata consegna verifico se Giacenza o l.avviso
355200150323     c                   if        d98cmc<>*blanks
355300150324     C                   Z-ADD     1             II
355400150326     C     d98cmc        LOOKUP    C2A(II)                                99
355500150324     C                   MOVEL     D2A(II)       usa2A
355600150324    4C     §2AFTC        IFEQ      'A'
355700150326     c                   eval      avvcmc='A'
355800150323     c                   endif
355900150324    4C     §2AFTC        IFEQ      'G'
356000150326     c                   eval      avvcmc='G'
356100150324     c                   endif
356200150324     c                   endif
356300140820     c
356400140911     c                   z-add     d98lna        EX
356500150113     c*  la azzero nei casi previsti                 0
356600150601    1c                   select
356700150119     c* 1) Se la bolla e consegnata
356800140925     c                   when      d98dcm>0
356900150113     c                   clear                   wcaldpcee
357000150216     c                   eval      d98spcdee='CON'
357100150217     c                   movel     ' '           DEEinDIST         1
357200150323     c* 2) Se pratica di danno
357300150603     c                   when      flgdan='S'
357400150323     c                   clear                   wcaldpcee
357500150323     c                   eval      d98spcdee='CA '
357600150323     c                   eval      deeindist='S'
357700150324     c* 3) Se bloccata/rimpallo
357800150114     c                   when      wd98fbc='B'
357900150113     c                   clear                   wcaldpcee
358000150216     c                   eval      d98spcdee='BLO'
358100150217     c                   movel     ' '           DEEinDIST         1
358200150324     c* 4) Se bolla export
358300150324     c                   when      fl1(EX)='E'
358400150324     c                   clear                   wcaldpcee
358500150324     c                   movel     ' '           DEEinDIST         1
358600150324     c                   eval      d98spcdee='EXP'
358700150324     c* 5) Se giacente  o in attesa di apertura giacenza
358800150324     c                   when      (d98dag>0 and d98dcg=0) or avvcmc='G'
358900150324     c                   clear                   wcaldpcee
359000150324     c                   eval      d98spcdee='GIA'
359100150324     c                   eval      deeindist='S'
359200150119     c* 7) Se codice manca consegna
359300150326     c                   when      d98cmc<>*blanks and avvcmc='A'
359400150324     c                   clear                   wpodpcee
359500150324     c                   clear                   wcaldpcee
359600150324     c                   movel     ' '           DEEinDIST         1
359700150324     c                   eval      d98spcdee='ATT'
359800150410     c* 7) Bolla in sospensione consegna
359900150410     c                   when      §B4LBGE='S'
360000150410     c                   clear                   wpodpcee
360100150410     c                   clear                   wcaldpcee
360200150410     c                   movel     ' '           DEEinDIST         1
360300150410     c                   eval      d98spcdee='SOS'
360400150616     c
360500150616     c* 8) Bolla di recupero: non metto nessuno stato se non "in consegna"
360600150616     c                   when      wrbl='R'
360700150616     c                   clear                   wcaldpcee
360800150616     c                   eval      d98spcdee='   '
360900150616     c                   eval      deeindist='S'
361000150601   x1c                   other
361100150325     c*
361200150403     c*  Se la bolla è in FERMO DEPOSITO verifico se da contattare
361300150331     c                   if        d98ffd='S'
361400150601    2c                   select
361500150403     c* FD escluso dai contatti
361600150601    2c                   when      §B4LTFD= 'N'
361700150403     c                   eval      d98spcdee='FD3'
361800150403     c                   clear                   wcaldpcee
361900150403     c                   movel     ' '           DEEinDIST         1
362000150403
362100150403     c* FD non contattato ( o da ricontattare)
362200150601    2c                   when      §B4LTFD<>' '  and  olv82sts<>'0'  and
362300150401     c                             olv82sts<>'1'
362400150323     c                   clear                   wcaldpcee
362500150323     c                   movel     ' '           DEEinDIST         1
362600150323     c                   eval      d98spcdee='FFD'
362700150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
362800150424     c                   eval      wpodpcee=wpoffi
362900150403
363000150403     c* FD contattato e     IN distinta
363100150601    2c                   when      d98cons='S'
363200150401     c                   eval      d98spcdee='FD2'
363300150401     c                   clear                   wcaldpcee
363400150401     c                   movel     ' '           DEEinDIST         1
363500150424     c* Passo anche la data ritiro entro IL che serve ai pgm di invio alert
363600150424     c                   eval      wpodpcee=wpoffi
363700150427     c* passo anche la data di contatto ovvero da quanto il destinatario può
363800150427     c*  ritirare la merce
363900150427     C                   MOVEL     'C'           WTC                            *PARTENZA
364000150427     C                   MOVEL     'C'           WIC
364100150427     C                   Z-ADD     D98ddc        WDAT
364200150427     C                   EXSR      DATORE
364300150427     c                   z-add     wore          wr4poi
364400150427     c                   z-add     24            wr4noi
364500150427     C                   EXSR      RO4CDP
364600150427     c                   z-add     wr4poo        wore
364700150427     c                   exsr      OREDAT
364800150427     c                   movel     wdat          D98DTCTD
364900150601   x2c                   other
365000150403     c* FD contattato e NON in distinta
365100150331     c                   eval      d98spcdee='FD1'
365200150408     c                   eval      wpodpcee=wpoffi
365300150331     c                   clear                   wcaldpcee
365400150331     c                   movel     ' '           DEEinDIST         1
365500150601    2c                   endsl
365600150331     c
365700150331     c                   endif
365800150403     c
365900150325     c*   se la bolla è Appunt o Supermerc verifico se da contattare
366000150601    2c                   if        (d98tc1='A' or d98tc2='A' or
366100150325     c                             d98tc1='S' or d98tc2='S') and §B4LBAS<>' '
366200150601     c                             and d98ffd= ' '
366300150325     c                   clear                   wcaldpcee
366400150325     c                   movel     ' '           DEEinDIST         1
366500150601    3c                   if        §B4LBAS= 'N'
366600150427     c                   eval      d98spcdee='AP3'
366700150403     c                   else
366800150403     c                   eval      d98spcdee='APP'
366900150601    3c                   endif
367000150601    2c                   endif
367100150325     c*   se la bolla ha il C/Assegno verifico se da contattare
367200150601    2c                   if        d98cas>0 and §B4LTCA<>' ' and d98ffd= ' '
367300150325     c                   clear                   wcaldpcee
367400150325     c                   movel     ' '           DEEinDIST         1
367500150601    3c                   if        §B4LBAS= 'N'
367600150427     c                   eval      d98spcdee='CA3'
367700150403     c                   else
367800150403     c                   eval      d98spcdee='CAS'
367900150601    3c                   endif
368000150601    2c                   endif
368100150325     c* Per questi 3 casi devo impostare la data contatto destinatario
368200150325     c                   if        d98spcdee='FFD' or d98spcdee='APP'   or
368300150325     c                             d98spcdee='CAS'
368400150601    2c                   select
368500150601    2c                   when      d98dti>0 and wmancatraz= ' '
368600150325     c                   movel     d98dti        d98dtctd
368700150403     c* se piene le ore già portate ad un giorno lavorativo
368800150403     c                   if        wpodti>0
368900150403     C* SE FESTIVO SLITTO AVANTI FINO AL PRIMO GIORNO FERIALE
369000150403     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
369100150403     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
369200150403     C                   Z-ADD     WPODTI        W10POI            9 0
369300150403     C                   EXSR      R10FES
369400150403     c                   z-add     w10poo        wore
369500150403     C                   EXSR      OREDAT
369600150403     C                   movel     WDAT          D98dtctd
369700150403     c                   endif
369800150403     c
369900150601    2c                   when      d98dam>0
370000150421     C                   Z-ADD     D98dam        WDAT
370100150421     C                   EXSR      DATORE
370200150421     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
370300150421     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
370400150421     C                   Z-ADD     Wore          W10POI            9 0
370500150421     C                   EXSR      R10FES
370600150421     c                   z-add     w10poo        wore
370700150421     C                   EXSR      OREDAT
370800150421     C                   movel     WDAT          D98dtctd
370900150601   x2c                   other
371000150325     c* data teorica di arrivo tenendo conto dell'IMP
371100150325     c                   exsr      letbrv
371200150325     c                   if        idDATA>0
371300150325     c                   exsr      CalcoloIMP
371400150325     c                   z-add     wr1poi        WORE
371500150325     c                   else
371600150325     c                   z-add     wpoptc        WORE
371700150325     c                   endif
371800150325     C                   EXSR      OREDAT
371900150325     c                   movel     wdat          d98dtctd
372000150601    2c                   endsl
372100150325     c
372200150325     c                   endif
372300150325     c
372400150601    1c                   endsl
372500140820     c
372600150113     c* Se la bolla non è in distinta calcolo la data se previsto
372700150220     c                   if        wcaldpcee='S' and
372800150220     c                             (d98cons<>'S'  or (d98cons='S' and
372900150220     c                             evemic<>'SI'))
373000150219     c
373100150219     c*  Se bolla completamente NON arrivata verifico se colli in IMP o disguido
373200150219     c*  si intende TUTTI. altrimenti non cambio il calcolo
373300150219     c                   if        d98nsp>0 and wtrasm<>'D' and
373400150219     c                             d98dam=0
373500150219     c                   exsr      letBRV
373600150219     c                   endif
373700150219     c
373800150217     c                   eval      d98spcdee='PRG'
373900150320     c                   if        d98dcr=0 and (d98tc1='A' or d98tc2='A' or
374000150320     c                             d98tc1='S' or d98tc2='S')
374100150320     c                   eval      d98spcdee='APP'
374200150320     c                   endif
374300150320     c
374400150113     c                   exsr      cal_DEE
374500150113     c                   endif
374600150113     c
374700140820     C     WPOdpcee      IFEQ      *ZEROS
374800931202     C                   Z-ADD     *ZEROS        D98DEE
374900931202     C                   Z-ADD     *ZEROS        D98OEE
375000930604     C                   ELSE
375100140820     C                   Z-ADD     WPOdpcee      WORE
375200141021     C******             SUB       12            WORE
375300930604     C                   EXSR      OREDAT
375400931202     C                   Z-ADD     WDAT          D98DEE
375500090903     c                   if        d98tcc<>10
375600090903     C                   ADD       12            WRES
375700090903     c                   else
375800090903     C                   ADD       10            WRES
375900090903     c                   endif
376000090903
376100931202     C     WRES          MULT      100           D98OEE
376200090903     c
376300090903     c                   if        d98oee=1000
376400090903     c                   eval      d98oee=1030
376500090903     c                   endif
376600140925     C                   END
376700150311     c
376800150311     c* se lo stato è OLP --> significa prevista consegna "blindata"
376900150311     c                   select
377000150311     c                   when      d98spcdee='OLP'
377100140820     c
377200150220     c* Se bolla in distinta --> data prevista = data distinta
377300150311     c                   when      deeindist='S'  and
377400150113     c                             d98cons='S'
377500150220
377600150225     c*  non la propongo senza MIC e se presente disguido o IMP
377700150220     c                   if        d98dee>d98ddc and evemic<>'SI'
377800150220     c                             and IDdata>0
377900150220     c                   else
378000150220
378100140820     c                   eval      d98dee=d98ddc
378200140925     c                   clear                   d98oee
378300150320     c* se la data è > di oggi stato PRV se non è in IMP
378400150320     c                   if        d98dee>wdaodi
378500150320     c                   if        d98spcdee<>'IMP'
378600150311     c                   eval      d98spcdee='PRV'
378700150320     c                   endif
378800150320     c
378900150225     c                   else
379000150217     c* se c'è il MIC
379100150217     c                   if        evemic='SI'
379200150217     c                   eval      d98spcdee='DDC'
379300150330     C***                else
379400150304     c* solo se in PRV, metto lo stato
379500150330     c* questo caso lo asterisco perchè dovrebbe essere già stato gestito
379600150330     c*  prima in quanto devo tenere conto di due aggiunte all'orario limite tab.OLP
379700150330     c***                if        d98spcdee='PRV'
379800150330     c***                eval      d98spcdee='PRG'
379900150330     c***                endif
380000150225     c                   endif
380100150217     c                   endif
380200150220     c                   endif
380300150217     c
380400150311     c                   endsl
380500931202     C*
380600931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA INTERNA *******************
380700931202     C     WPODCI        IFEQ      *ZEROS
380800931202     C                   Z-ADD     *ZEROS        D98DCI
380900931202     C                   Z-ADD     *ZEROS        D98HCI
381000931202     C                   ELSE
381100931202     C                   Z-ADD     WPODCI        WORE
381200931202     C                   EXSR      OREDAT
381300931202     C                   Z-ADD     WDAT          D98DCI
381400090902     c                   if        d98tci<>10
381500931202     C                   ADD       12            WRES
381600090902     c                   else
381700090902     C                   ADD       10            WRES
381800090902     c                   endif
381900090902
382000931202     C     WRES          MULT      100           D98HCI
382100090901     c
382200090903     c                   if        d98hci=1000
382300090901     c                   eval      d98hci=1030
382400090901     c                   endif
382500090901     c
382600931202     C                   END
382700931202     C*
382800931202     C* TRASFORMO LA DATA DI CONSEGNA PREVISTA ESTERNA *******************
382900931202     C     WPODCE        IFEQ      *ZEROS
383000931202     C                   Z-ADD     *ZEROS        D98DCE
383100931202     C                   Z-ADD     *ZEROS        D98HCE
383200931202     C                   ELSE
383300931202     C                   Z-ADD     WPODCE        WORE
383400931202//// C**************       SUB  12        WORE
383500931202     C                   EXSR      OREDAT
383600931202     C                   Z-ADD     WDAT          D98DCE
383700090902     c                   if        d98tcc >10
383800931202     C                   ADD       12            WRES
383900090902     c                   else
384000090902     C                   ADD       10            WRES
384100090902     c                   endif
384200090902     c
384300931202     C     WRES          MULT      100           D98HCE
384400090902     c
384500090903     c                   if        d98hce=1000
384600090902     c                   eval      d98hce=1030
384700090902     c                   endif
384800090902     c
384900931202     C                   END
385000960229     C*
385100960229     C                   ENDIF                                                  <-- 1 ---
385200930604     C*
385300930604     C                   ENDSR
385400150218     C********************************************************************
385500150218     C* leggo le spunte dei colli per verificare se sono in IMP o disguido
385600150218     C********************************************************************
385700150218     c     LetBRV        BEGSR
385800150219     c                   clear                   bollaOK           1
385900150219     c                   clear                   IDnpg
386000150219     c                   clear                   IDdata
386100150219     c                   clear                   IDora
386200150219     c                   clear                   IDfgs
386300150219     c
386400150218     C     Kart          SETLL     FNART01L
386500150218     C     Kart          READE     FNART01L
386600150219    1c                   dow       not %eof(fnart01l)
386700150219     c                   clear                   DISfgs
386800150219     c                   clear                   DISdata
386900150219     c                   clear                   DISnpg
387000150219     c                   clear                   DISora
387100150219     c                   clear                   IMPdata
387200150219     c                   clear                   IMPfgs
387300150508     c* per ogni collo verifico se in IMP o disguidato
387400150218     c* al primo che NON trovo, esco come --> OK
387500150218     c     kbrv          setll     fnbrv07l
387600150218     c     kbrv          reade     fnbrv07l
387700150219    2c                   dow       not %eof(fnbrv07l)
387800150219    3c                   if        brvatr='D'  or brvnpg=3
387900150218     c* prendo la data della spunta
388000150218     c                   clear                   fnlv53ds
388100150219     c                   eval      d53tla='L'
388200150218     C                   MOVEL     BRvNPG        D53NPG
388300150218     C                   Z-ADD     BRvNFV        D53NFV
388400150218     C                   MOVEL     BRvFGS        D53FGS
388500150218     C                   CALL      'FNLV53R'
388600150218     C                   PARM                    fnlv53ds
388700150218    4c                   if        d53err<>*blanks
388800150219    5c                   if        brvdcs>0
388900150218     c                   eval      d53dfv=brvdcs
389000150218     c                   else
389100150219    6c                   if        brvdfs>0
389200150218     c                   eval      d53dfv=brvdfs
389300150219    6c                   endif
389400150219    5c                   endif
389500150219    4c                   endif
389600150218     c
389700150219    4c                   select
389800150219     c* se si tratta di IMP --> memorizzo
389900150219    4c                   when      d53spg='P' and d53dfv>impdata
390000150218     c                   eval      IMPdata=d53dfv
390100150219     c                   eval      IMPfgs =brvfgs
390200150219     c* se si tratta di DISGUIDO
390300150218     c                   when      brvatr='D'
390400150219     c
390500150219    5c                   if        d53dfv=DISdata
390600150219     c* Verifico l'ora
390700150219    6c                   if        brvhcs>0 and brvhcs>DISora
390800150219     c                   eval      DISdata=d53dfv
390900150219     c                   eval      DISfgs =brvfgs
391000150219     c                   eval      DISnpg =brvnpg
391100150219     c                   eval      DISora =brvhcs
391200150219   x6c                   else
391300150219    7c                   if        brvhms>0 and brvhms>DISora
391400150219     c                   eval      DISdata=d53dfv
391500150219     c                   eval      DISfgs =brvfgs
391600150219     c                   eval      DISnpg =brvnpg
391700150219     c                   eval      DISora =brvhms
391800150219    7c                   endif
391900150219    6c                   endif
392000150219    5c                   endif
392100150219     c
392200150219    5c                   if        d53dfv>DISdata
392300150219     c                   eval      DISdata=d53dfv
392400150219     c                   eval      DISfgs =brvfgs
392500150219     c                   eval      DISnpg =brvnpg
392600150219     c                   eval      DISora =brvhcs
392700150219     c                   if        disora=0
392800150219     c                   eval      DISora =brvhms
392900150219     c                   endif
393000150219    5c                   endif
393100150219     c
393200150219    4c                   endsl
393300150219    3c                   endif
393400150219     c     kbrv          reade     fnbrv07l
393500150219    2c                   enddo
393600150219     c* se per il collo non ho trovano nè spunta IMP nè spunta DIS
393700150219     c*  esco dal ciclo: la bolla è OK
393800150219    2c                   if        impdata=0 and disdata=0
393900150219     c                   eval      bollaOK='S'
394000150219     c                   clear                   IDdata
394100150219     c                   clear                   IDfgs
394200150219     c                   clear                   IDora
394300150219     c                   clear                   IDnpg
394400150219   x2c                   else
394500150219     c* memorizzo la data più alta
394600150219     c* IMP vince sul disguido per stessa data, senza guardare l'ora
394700150219    3c                   if        IMPdata>0 and IMPdata>=IDdata
394800150219     c                   eval      IDdata=IMPdata
394900150219     c                   eval      IDfgs=IMPfgs
395000150219     c                   eval      IDora=0
395100150219     c                   eval      IDnpg='P'
395200150219    3c                   endif
395300150219    3c                   if        (DISdata>0 and DISdata=IDdata
395400150219     c                             and IDnpg<>'P' and DISora>IDora) or
395500150219     c                             (DISdata>0 and DISdata>IDdata)
395600150219     c                   eval      IDdata=DISdata
395700150219     c                   eval      IDfgs=DISfgs
395800150219     c                   eval      IDora=DISora
395900150219     c                   eval      IDnpg=%char(DISnpg)
396000150219    3c                   endif
396100150219    2c                   endif
396200150218     c
396300150218     C     Kart          READE     FNART01L
396400150219    1c                   enddo
396500150219     c
396600150219     c* se la bolla è in IMP o disguido, ma la partenza ultimo collo è > ignoro
396700150508     c**                 if        d98duc> 0 and d98duc>IDdata
396800150508     c**                 eval      bollaOK='S'
396900150508     c**                 clear                   IDdata
397000150508     c**                 clear                   IDfgs
397100150508     c**                 clear                   IDora
397200150508     c**                 clear                   IDnpg
397300150508     c**                 endif
397400150219     c
397500150218     c
397600150218     C                   ENDSR
397700140825     C********************************************************************
397800140825     C* TRASFORMO LA DATA DI CONSEGNA TEORICA INTERNA
397900140825     C********************************************************************
398000140825     c     TRAS_DEI      BEGSR
398100140825     C     WPODEI        IFEQ      *ZEROS
398200140825     C                   Z-ADD     *ZEROS        D98DEI
398300140825     C                   Z-ADD     *ZEROS        D98OEI
398400140825     C                   ELSE
398500140825     C                   Z-ADD     WPODEI        WORE
398600140825     C                   SUB       12            WORE
398700140825     C                   EXSR      OREDAT
398800140825     c
398900140825     C                   Z-ADD     WDAT          D98DEI
399000140825     c                   if        d98tci<>10
399100140825     C                   ADD       12            WRES
399200140825     c                   else
399300140825     C                   ADD       10            WRES
399400140825     c                   endif
399500140825     c
399600140825     C     WRES          MULT      100           D98OEI
399700140825     c
399800140825     c                   if        d98oei=1000
399900140825     c                   eval      d98oei=1030
400000140825     c                   endif
400100140825     c
400200140825     C                   END
400300140825     c                   ENDSR
400400930604     C********************************************************************
400500930604     C*   CHKFOR - CONTROLLA PER EVENTUALE FORZATURA
400600930604     C********************************************************************
400700930604     C     CHKFOR        BEGSR
400800930604     C*
400900930604     C* SE E' UNA BOLLA DI RECUPERO NON POSSO CALCOLARE I TEMPI CORRETTI
401000930604     C* E QUINDI FORZO LA CONFORMITA' A SI
401100941227     C     D98ERR        IFEQ      '0'
401200930604     C     D98NCD        IFEQ      *ZEROS                                       *N.SEGNACOLLO DAL
401300930604     C     D98FNS        ANDNE     'S'                                          *SEGNACOLLI NON SEQ.
401400930604     C                   EXSR      FORCON                                       *FORZA CONFORMITA
401500930604     C                   MOVEL     '1'           D98ERR
401600930604     C                   MOVEL     MSG(10)       D98MSG                         *BOLLA DI RECUPERO
401700930604     C                   END
401800930604     C                   END
401900990212     C*
402000990212     C* SE C'E' PRATICA DI DANNO NON POSSO CALCOLARE I TEMPI CORRETTI
402100990212     C* E QUINDI FORZO LA CONFORMITA' A SI
402200990323     C* (SE IL PGM NON VIENE LANCIATO PER IL CONTROLLO DELLA CONSEGNABILITA')
402300990212     C     D98ERR        IFEQ      '0'
402400990323     C     FLCONS        ANDNE     'S'                                          *NO LANCIO CONSEGNAB
402500141204     C******             EXSR      CHKDAN                                       *CONTROLLA DANNI
402600990212     C     FLGDAN        IFEQ      'S'                                          *C'E DANNO
402700990212     C                   EXSR      FORCON                                       *FORZA CONFORMITA
402800990212     C                   MOVEL     '1'           D98ERR
402900990212     C                   MOVEL     MSG(31)       D98MSG                         *PRATICA DANNO
403000990212     C                   END
403100990212     C                   END
403200990308     C*
403300990308     C* SE E' UN RESO CONTOSERVIZIO NON POSSO CALCOLARE I TEMPI CORRETTI
403400990308     C* E QUINDI FORZO LA CONFORMITA' A SI
403500100614     C**   D98ERR        IFEQ      '0'
403600100614     C**   WFBR          IFEQ      'S'                                          *RESO
403700100614     C**   WRBL          ANDEQ     'C'                                          *CONTOSERVIZIO
403800100614     C**                 EXSR      FORCON                                       *FORZA CONFORMITA
403900100614     C**                 MOVEL     '1'           D98ERR
404000100614     C**                 MOVEL     MSG(32)       D98MSG                         *RESO/CONTOSERV.
404100100614     C**                 END
404200100614     C**                 END
404300930604     C*
404400930604     C                   ENDSR
404500990212     C********************************************************************
404600990212     C*   CHKDAN - CONTROLLA SE PRESENTE PRATICA DI DANNO
404700990212     C********************************************************************
404800990212     C     CHKDAN        BEGSR
404900990212     C*
405000990212     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
405100150603     c                   movel     'N'           opendan           1
405200040809      *
405300040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
405400040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
405500040809      * legate alla spedizione che passo
405600040809     c                   clear                   fidn12ds
405700040809     c                   eval      i12aas = D98AAS
405800040809     c                   eval      i12lnp = D98LNP
405900040809     c                   eval      i12nrs = D98NRS
406000040809     c                   eval      i12nsp = D98NSP
406100040809     c                   eval      i12fel = 'E'
406200040809     c                   eval      i12fan = 'E'
406300040809     c*
406400040809     c                   call      'FIDN12R'
406500040809     c                   parm                    fidn12ds
406600040809     c*
406700040809     c* se non ci sono errori
406800040809     c                   if        o12err = ' '
406900040809     c* se numero di CA trovate maggiore di zero
407000040809     c                   if        o12nca > 0
407100040809     C                   MOVEL     'S'           FLGDAN                         *C'E' DANNO
407200150603     c
407300150603     c***                z-add     1             aa                2 0
407400150603     c***                dow       aa <= o12nca
407500150603     C* APERTA
407600150603     C***  skdch(aa)     IFEQ      0
407700150603     c***                movel     'S'           opendan
407800150603     c***                endif
407900150603     c*
408000150603     c***                add       1             aa
408100150603     c***                enddo
408200150603     c
408300040809     c                   endif
408400040809     c                   endif
408500990212     C*
408600990212     C                   ENDSR
408700941228     C********************************************************************
408800941228     C*   CALORP - CALCOLA ORE REALI DI PARTENZA
408900941228     C********************************************************************
409000941228     C     CALORP        BEGSR
409100941228     C*
409200941228     C* CALCOLO LE ORE REALI DI PARTENZA INTERNE *****************************
409300941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
409400941228     C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
409500960402     C                   Z-ADD     WPODBR        WR2POI            9 0          *DATA BORDERO ->
409600950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
409700950110     C                   Z-ADD     WPOPPI        WR2POF            9 0          *DATA PREV.PART. ->
409800950110     C                   ELSE
409900950110     C                   Z-ADD     WPODUC        WR2POF                         *DATA PART.ULT.C. ->
410000950110     C                   ENDIF
410100941228     C                   EXSR      RO2COP
410200941228     C* AGGIUNGO 24 ORE PER AVERE IN TABELLA 24 PER SPED. IN GIORNATA E 48 PER
410300941228     C* SPED. IL GIORNO DOPO
410400941228     C***********WR2NOV    ADD  24        W98RPI           *ORE REALI PART.INT.
410500941228     C                   Z-ADD     WR2NOV        W98RPI                         *ORE REALI PART.INT.
410600941228     C                   Z-ADD     W98HF         W98HPI                         *ORE X FEST.PART.INT
410700941228     C*
410800941228     C* CALCOLO LE ORE REALI DI PARTENZA CLIENTE *******************************
410900941228     C                   MOVEL     'P'           WTC                            *PART/TRAZ/CONS.
411000941228     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
411100941228     C                   Z-ADD     WPODSP        WR2POI
411200950110     C                   Z-ADD     WPOPPC        WR2POF
411300941228     C                   EXSR      RO2COP
411400941228     C***********WR2NOV    ADD  24        W98RPC           *ORE REALI PART.CLI.
411500941228     C                   Z-ADD     WR2NOV        W98RPC                         *ORE REALI PART.CLI.
411600941228     C                   Z-ADD     W98HF         W98HPC                         *ORE X FEST.PART.CLI
411700941228     C*
411800941228     C                   ENDSR
411900930414     C********************************************************************
412000930414     C*   CALORT - CALCOLA ORE REALI DI TRAZIONE
412100930414     C********************************************************************
412200930414     C     CALORT        BEGSR
412300930414     C*
412400930414     C* CALCOLO LE ORE REALI DI TRAZIONE INTERNE *****************************
412500931126     C                   MOVEL     'T'           WTC               1            *TRAZIONE/CONSEGNA
412600930414     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
412700931126     C                   Z-ADD     *ZEROS        W98HF
412800950110     C     D98DUC        IFEQ      *ZEROS                                       *DATA PART.ULT.COLLO
412900950110     C                   Z-ADD     WPOPPI        WR2POI                         *DATA PREV.PART.->
413000950110     C                   ELSE
413100950110     C                   Z-ADD     WPODUC        WR2POI                         *DATA PART.ULT.C.->
413200950110     C                   ENDIF
413300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
413400931126     C                   Z-ADD     WPOPTI        WR2POF                         *DATA PRE.ARR.TR.INT
413500930608     C                   ELSE
413600931126     C                   Z-ADD     WPODTI        WR2POF                         *DATA ARRIVO TRAZ.
413700930608     C                   END
413800931126     C                   EXSR      RO2COV
413900931126     C                   Z-ADD     WR2NOV        W98RTI                         *ORE REALI TRAZ.INT.
414000930423     C                   Z-ADD     W98HF         W98HFI                         *ORE X FEST.TRAZ.INT
414100930414     C*
414200930414     C* CALCOLO LE ORE REALI DI TRAZIONE CLIENTE *******************************
414300930414     C                   MOVEL     'T'           WTC                            *TRAZIONE/CONSEGNA
414400930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
414500931126     C                   Z-ADD     *ZEROS        W98HF
414600950110     C                   Z-ADD     WPOPPC        WR2POI                         *PREV.PART.ULT.C.->
414700931126     C                   Z-ADD     WPOPTC        WR2POF                         *PREV.ARR.TRAZ.CLI->
414800931126     C                   EXSR      RO2COV
414900931126     C                   Z-ADD     WR2NOV        W98RTC
415000930423     C                   Z-ADD     W98HF         W98HFC                         *ORE X FEST.TRAZ.CLI
415100930414     C*
415200930414     C                   ENDSR
415300930414     C********************************************************************
415400930414     C*   CALORC - CALCOLA ORE REALI DI CONSEGNA
415500930414     C********************************************************************
415600930414     C     CALORC        BEGSR
415700930511     C*
415800930511     C* SE NON ESISTE LA DATA DI CONSEGNA FORZO LA DATA ODIERNA PER AVERE ALMENO
415900930511     C* I TEMPI DI RITARDO AD OGGI
416000930511     C     WPODCM        IFEQ      *ZEROS
416100960911     C                   Z-ADD     WPOATT        WPODCM                         *DATA CONSEGNA
416200930511     C                   END
416300940117     C*
416400940117     C* COMUNQUE SE E' STATA PASSATA LA DATA DI FINE CONSEGNABILITA' LA
416500940117     C* FORZO NELLA DATA DI CONSEGNA PER LIMITARE IL CALCOLO DELLE ORE
416600940117     C* ALLA DATA DI FINE CONSEGNABILITA'
416700940117     C* AL PROGRESSIVO ORE AGGIUNGO LE ORE ARROTONDATE
416800940117     C* DALLA TABELLA '1X' CAUSALE 'OSC' (ORA SOGLIA CONSEGNE)
416900940118££££ C     WPODFC        IFNE      *ZEROS
417000940118//// C     WPODFC        ADD       12            WPODCM                         *DATA CONSEGNA
417100940118££££ C                   END
417200930414     C*
417300930414     C* CALCOLO LE ORE REALI DI CONSEGNA INTERNE ***************************
417400930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
417500930420     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
417600931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
417700931126     C                   Z-ADD     WPOPTI        WR5POI            9 0          *DATA PRE.ARR.TR.INT
417800930608     C                   ELSE
417900931126     C                   Z-ADD     WPODTI        WR5POI                         *DATA ARRIVO TRAZ.
418000930608     C                   END
418100931126     C                   Z-ADD     WPODCM        WR5POF            9 0
418200931126     C                   EXSR      RO5COV
418300931126     C                   Z-ADD     WR5NOV        W98RCI                         *ORE REALI CONS.INT.
418400930423     C                   Z-ADD     W98HF         W98HEI                         *ORE X FEST.CONS.INT
418500930423     C                   Z-ADD     W98HH         W98HHI                         *ORE X TURNI CHIUSUR
418600060418     C                   Z-ADD     W98EG         W98EGI                         *ORE X TURNI CHIUSUR
418700060420     C                   Z-ADD     W98EA         W98EAI                         *ORE X TURNI CHIUSUR
418800930423     C                   Z-ADD     W98HG         W98HGI                         *ORE X GIACENZA
418900930423     C                   Z-ADD     W98HL         W98HLI                         *ORE X LASC.AVVISO
419000930511     C                   Z-ADD     W98HO         W98HOI                         *ORE X CONTRASSEGNO
419100930511     C                   Z-ADD     W98HD         W98HDI                         *ORE X F.DEPOSITO
419200930414     C*
419300930414     C* CALCOLO LE ORE REALI DI CONSEGNA CLIENTE ***************************
419400930414     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
419500930414     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
419600931126     C                   Z-ADD     WPOPTC        WR5POI            9 0          *PREV.ARR.TRAZ.CLI->
419700931126     C                   Z-ADD     WPODCM        WR5POF            9 0
419800931126     C                   EXSR      RO5COV
419900931126     C                   Z-ADD     WR5NOV        W98RCC                         *ORE REALI CONS.CLI.
420000930423     C                   Z-ADD     W98HF         W98HEC                         *ORE X FEST.CONS.CLI
420100930423     C                   Z-ADD     W98HH         W98HHC                         *ORE X TURNI CHIUSUR
420200060418     C                   Z-ADD     W98EG         W98EGC                         *ORE X TURNI CHIUSUR
420300060420     C                   Z-ADD     W98EA         W98EAC                         *ORE X TURNI CHIUSUR
420400930423     C                   Z-ADD     W98HG         W98HGC                         *ORE X GIACENZA
420500930423     C                   Z-ADD     W98HL         W98HLC                         *ORE X LASC.AVVISO
420600930511     C                   Z-ADD     W98HO         W98HOC                         *ORE X CONTRASSEGNO
420700930511     C                   Z-ADD     W98HD         W98HDC                         *ORE X F.DEPOSITO
420800930414     C*
420900930414     C                   ENDSR
421000930414     C********************************************************************
421100930414     C*   CALORR - CALCOLA ORE REALI DI RESA
421200930414     C********************************************************************
421300930414     C     CALORR        BEGSR
421400930414     C*
421500950110     C                   Z-ADD     W98RPI        W98RRI                         *INTERNE
421600950110     C                   ADD       W98RTI        W98RRI
421700941228     C                   ADD       W98RCI        W98RRI
421800950110     C                   Z-ADD     W98RPC        W98RRC                         *CLIENTE
421900950110     C                   ADD       W98RTC        W98RRC
422000941228     C                   ADD       W98RCC        W98RRC
422100930414     C*
422200930414     C                   ENDSR
422300930420     C********************************************************************
422400930420     C*   CALDIF - CALCOLA ORE DI DIFFERENZA TRA REALI E TEORICHE
422500930420     C********************************************************************
422600930420     C     CALDIF        BEGSR
422700930420     C*
422800941228     C* PARTENZA
422900941228     C     W98RPI        SUB       D98TPI        W98FPI
423000941228     C     W98RPC        SUB       D98TPC        W98FPC
423100941228     C*
423200930420     C* TRAZIONE
423300930423     C     W98RTI        SUB       D98TTI        W98FTI
423400930423     C     W98RTC        SUB       D98TTC        W98FTC
423500930420     C*
423600930420     C* CONSEGNA
423700090827     C***  W98RCI        SUB       D98TCI        W98FCI
423800090827     C     W98RCI        SUB       w99TCI        W98FCI
423900090827     C***  W98RCC        SUB       D98TCC        W98FCC
424000090827     C     W98RCC        SUB       w99TCC        W98FCC
424100930420     C*
424200930420     C* RESA
424300090921     C***  W98RRI        SUB       D98TRI        W98FRI
424400090921     C     W98RRI        SUB       w99TRI        W98FRI
424500090921     c
424600090921     C***  W98RRC        SUB       D98TRC        W98FRC
424700090921     C     W98RRC        SUB       w99TRC        W98FRC
424800930420     C*
424900930420     C                   ENDSR
425000930420     C********************************************************************
425100930420     C*   CALCNF - CALCOLA LE CONFORMITA'
425200930420     C********************************************************************
425300930420     C     CALCNF        BEGSR
425400941228     C*
425500941228     C*-------------------------------------------------------------------
425600941228     C* CONFORMITA' PARTENZA  INTERNA
425700941228     C*-------------------------------------------------------------------
425800941228     C                   Z-ADD     W98FPI        W98NPI
425900941228     C     W98FPI        IFLE      *ZEROS
426000941228     C                   MOVEL     'S'           D98CPI
426100941228     C                   ELSE
426200941228     C                   MOVEL     'N'           D98CPI
426300941228     C                   END
426400941228     C*
426500941228     C*-------------------------------------------------------------------
426600941228     C* CONFORMITA' PARTENZA  CLIENTE
426700941228     C*-------------------------------------------------------------------
426800941228     C                   Z-ADD     W98FPC        W98NPC
426900941228     C     W98FPC        IFLE      *ZEROS
427000941228     C                   MOVEL     'S'           D98CPC
427100941228     C                   ELSE
427200941228     C                   MOVEL     'N'           D98CPC
427300941228     C                   END
427400930420     C*
427500930420     C*-------------------------------------------------------------------
427600930420     C* CONFORMITA' TRAZIONE  INTERNA
427700930420     C*-------------------------------------------------------------------
427800930510     C                   Z-ADD     W98FTI        W98NTI
427900930423     C     W98FTI        IFLE      *ZEROS
428000930420     C                   MOVEL     'S'           D98CTI
428100930420     C                   ELSE
428200930420     C                   MOVEL     'N'           D98CTI
428300930420     C                   END
428400930420     C*
428500930420     C*-------------------------------------------------------------------
428600930420     C* CONFORMITA' TRAZIONE  CLIENTE
428700930420     C*-------------------------------------------------------------------
428800930510     C                   Z-ADD     W98FTC        W98NTC
428900930423     C     W98FTC        IFLE      *ZEROS
429000930420     C                   MOVEL     'S'           D98CTC
429100930420     C                   ELSE
429200930420     C                   MOVEL     'N'           D98CTC
429300930420     C                   END
429400930420     C*
429500930420     C*-------------------------------------------------------------------
429600930420     C* CONFORMITA' CONSEGNA  INTERNA
429700930420     C*-------------------------------------------------------------------
429800150226     c                   if        wmancatraz='S'
429900150226     c                   clear                   d98dti
430000150226     c                   clear                   d98hti
430100150226     c                   endif
430200150226     c
430300930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
430400930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
430500930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
430600050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
430700050914     c                   if        consideraora='S'
430800050914     c     w98fci        sub       24            w98nci
430900050914     c                   add       24            d98hrc
431000050914     c                   else
431100930524     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
431200050914     c                   endif
431300080116     c
431400110923     c                   select
431500080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
431600080116     c*  --> trasformo il ritardo in 0
431700090827     c                   when      d98cca='2'
431800080116     c                   z-add     0             w98nci
431900090827
432000080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
432100080116     c*  e sto leggendo la bolla in arrivo
432200080116     c*  --> trasformo il ritardo = 0
432300090827     c                   when      d98cca='1' and d98tbo='A'
432400080116     c                   z-add     0             w98nci
432500090902     c
432600090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
432700090902     c                   when      wmancaTRAZ='S'
432800090902     c                   z-add     0             w98nci
432900090902     c                   clear                   d98dti
433000090902     c                   clear                   d98hti
433100110923     c
433200110923     c* Ignoro ritardo se pefect order
433300130221     c                   when      d98xco >*zeros
433400110923     c                   z-add     0             w98nci
433500090827     c
433600090928     c* Consegna  ore 10 --> se consegnata tra le 10,31 e le 12,59
433700091014     c* sono  in ritardo di 12 ore (ma la data consegna deve cadere al mattino
433800091014     c*  controllo ora soglia  consegna da tabella OSC)
433900091009     c                   when      d98tci=10   and
434000091012     c***                          primotipo=' ' and w98nci=0 and d98hmc>1030
434100091014     c***                          d98hci=1030   and w98nci=0 and d98hmc>1030
434200091014     c*
434300091014     c                             d98hci=1030
434400091014     C                   MOVEL     'OSC  '       WR3TAB
434500091014     C                   eval      WR3HHI=d98hmc
434600091014     C                   EXSR      RO3ORE
434700131218
434800091014     c                   if        wr3matpom='M' and d98hmc>1030
434900131218     c                   exsr      ctrLAVH1030
435000131218
435100131218     c                   if        wlav1030<>'S'
435200091014     c                   add       12            w98nci
435300091014     c                   endif
435400131218     c                   endif
435500090928     c
435600090827     c                   endsl
435700050914     c
435800930524     C                   ELSE                                                   <-- 1 -->
435900930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (INIZ.RANGE)
436000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
436100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
436200931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
436300930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
436400930608     C                   ELSE
436500930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
436600930608     C                   END
436700930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
436800930525     C                   EXSR      RO2COV
436900930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *ORE PER CONS.RICH.I
437000930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONS. RICHIESTA (FINE RANGE)
437100930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
437200930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
437300931125     C     D98DTI        IFEQ      *ZEROS                                       *DATA ARR.TRAZ.
437400930608     C                   Z-ADD     WPOPTI        WR2POI            9 0          *DATA PRE.ARR.TR.INT
437500930608     C                   ELSE
437600930608     C                   Z-ADD     WPODTI        WR2POI                         *DATA ARRIVO TRAZ.
437700930608     C                   END
437800931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
437900930525     C                   EXSR      RO2COV
438000930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *ORE PER CONS.RICH.F
438100930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
438200930525     C     W98RCI        IFLT      WNOCRI                                       --- 2 -->
438300930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
438400930525     C* E' IL RITARDO
438500930610     C     WNOCRI        SUB       W98RCI        W98NCI
438600050901     c
438700050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
438800050901     c*  --> trasformo il ritardo in anticipo
438900050901     c                   if        d98cca='2'
439000080116     c***                z-sub     w98nci        w98nci
439100080116     c                   z-add     0             w98nci
439200050901     c                   endif
439300111018     c* Ignoro ritardo se pefect order
439400130221     c                   if        d98xco >*zeros
439500111018     c                   z-add     0             w98nci
439600111018     c                   endif
439700061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
439800061121     c*  e sto leggendo la bolla in arrivo
439900061121     c*  --> trasformo il ritardo in anticipo
440000061121     c                   if        d98cca='1' and d98tbo='A'
440100080116     c***                z-sub     w98nci        w98nci
440200080116     c                   z-add     0             w98nci
440300061121     c                   endif
440400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
440500070827     c*  mittente, --> consegnato in anticipo
440600070828     c                   if        d98tcr=' ' and d98nsp>0  and d98dcm>0
440700140403     c                   if        Callar5=' '
440800070827     c                   exsr      LeggiAR5
440900140403     c                   endif
441000070827     c
441100070827     c                   if        dcrpart<>'S'
441200070824     c                   z-sub     w98nci        w98nci
441300070824     c                   endif
441400070827     c
441500070824     c                   endif
441600050907     c
441700050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
441800050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
441900050907     c*  lo trasformo in anticipo perchè non sono in ritardo
442000050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
442100050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
442200050916     c***                if        d98dfc>0
442300070828     c                   if        d98dcm=0  and w98nci>0
442400050907     c                   z-sub     w98nci        w98nci
442500050907     c                   sub       oreILoDOPO    w98nci
442600050907     c                   endif
442700050901     c
442800930525     C                   ELSE                                                   <-- 2 -->
442900070521     c* non do ritardo se la consegna reale cade nel giorno
443000070521     c*  della consegna richiesta se presente IL
443100100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
443200070521     c                             wpodcm<=wpocr1
443300070521     c                   clear                   w98nci
443400100224     c                   else                                                   ---2a -->
443500930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
443600090827     C***  D98TCI        IFLT      WNOCRF                                       --- 3 -->
443700090827     C     w99TCI        IFLT      WNOCRF                                       --- 3 -->
443800930525     C* LA DIFFERENZA TRA N. ORE PER CONS.RICH.FINE E N.ORE PER CONS.REALE
443900930525     C* E' IL RITARDO O L'ANTICIPO
444000930525     C     W98RCI        SUB       WNOCRF        W98NCI
444100070115     c
444200070115     c* non do ritardo se comunque la consegna reale cade nel giorno
444300070115     c*  della consegna richiesta se presente IL
444400070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
444500070521     c***                          wpodcm<=wpocr1  and w98nci>0
444600070521     c***                clear                   w98nci
444700070521     c***                endif
444800070115     c
444900930525     C                   ELSE                                                   <-- 3 -->
445000930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
445100930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
445200930525     C                   Z-ADD     W98FCI        W98NCI                         *ORE NON CONF.CONS.I
445300930525     C                   END                                                    <-- 3 ---
445400100224     C                   END                                                    <-- 2a---
445500100224
445600100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
445700100224     c                   select
445800100224     c                   when      d98cca='2'
445900100224     c                   z-add     0             w98nci
446000100224
446100100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
446200100224     c*  e sto leggendo la bolla in arrivo
446300100224     c                   when      d98cca='1' and d98tbo='A'
446400100224     c                   z-add     0             w98nci
446500100224     c                   endsl
446600100224     c
446700100224     C                   END                                                    <-- 2 ---
446800070521     C                   END                                                    <-- 1 ---
446900070521     c
447000970915     C*/*  SE CONSEGNA PARZIALE IMPORT AZZERO ORE NON CONF.CONS.INT.
447100970915     C*/         WAZNCI    IFEQ 'S'
447200970915     C*/         W98NCI    ANDLT*ZEROS
447300970915     C*/                   Z-ADD*ZEROS    W98NCI
447400970915     C*/                   ENDIF
447500970915     C* SE DATA ARRIVO TRAZIONE = 0, AZZERO ORE NON CONF.CONS.INT.
447600050922     c* lo faccio solo se non c'e' la data cons richiesta
447700970915     C     D98DTI        IFEQ      *ZEROS
447800050922     c     d98dcr        andeq     *zeros
447900970915     C                   Z-ADD     *ZEROS        W98NCI
448000970915     C                   ENDIF
448100930524     C*
448200930524     C* CONTROLLO CONFORMITA
448300930423     C     W98NCI        IFLE      *ZEROS
448400930524     C                   MOVEL     'S'           D98CCI
448500930420     C                   ELSE
448600930524     C                   MOVEL     'N'           D98CCI
448700930420     C                   END
448800930420     C*
448900930420     C*-------------------------------------------------------------------
449000930420     C* CONFORMITA' CONSEGNA  CLIENTE
449100930420     C*-------------------------------------------------------------------
449200930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
449300930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
449400930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
449500050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
449600050914     c                   if        consideraora='S'
449700050914     C     w98fcc        sub       24            W98NCC                         *ORE NON CONF.CONS.C
449800050914     c                   else
449900050914     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.C
450000050914     c                   endif
450100080116     c
450200080116     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
450300080116     c*  --> trasformo il ritardo =0
450400090827     c                   select
450500090827     c                   when      d98cca='2'
450600080116     c                   z-add     0             w98ncc
450700090827
450800080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
450900080116     c*  e sto leggendo la bolla in arrivo
451000080116     c*  --> trasformo il ritardo =0
451100090827     c                   when      d98cca='1' and d98tbo='A'
451200080116     c                   z-add     0             w98ncc
451300090902     c
451400090902     c* Se manca data arrivo trazione --> azzero i tempi di consegna
451500090902     c                   when      wmancaTRAZ='S'
451600090902     c                   z-add     0             w98ncc
451700110923     c* Ignoro ritardo se Perfect Order
451800130221     c                   when      d98xco >*zeros
451900110923     c                   z-add     0             w98ncc
452000090902     c
452100090827     c
452200090827     c* Consegna conforme ore 10 -->se consegnata tra le 10,31 e le 12,59
452300090827     c* sono  in ritardo di 12 ore
452400091009     c                   when      d98tcc=10  and
452500091012     c***                          primotipo=' ' and w98ncc=0 and d98hmc>1030
452600091014     c***                          d98hce=1030   and w98ncc=0 and d98hmc>1030
452700091014     c*
452800091014     c                             d98hce=1030
452900091014     C                   MOVEL     'OSC  '       WR3TAB
453000091014     C                   eval      WR3HHI=d98hmc
453100091014     C                   EXSR      RO3ORE
453200091014     c                   if        wr3matpom='M' and d98hmc>1030
453300131218     c                   exsr      ctrLAVH1030
453400131218
453500131218     c                   if        wlav1030<>'S'
453600131218     c                   add       12            w98ncc
453700131218     c                   endif
453800131218     c****               add       12            w98ncc
453900091014     c                   endif
454000131218     c
454100091014     c***                z-add     12            w98ncc
454200090827     c                   endsl
454300050914     c
454400930524     C                   ELSE                                                   <-- 1 -->
454500930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
454600930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
454700930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
454800930608     C                   Z-ADD     WPOPTC        WR2POI            9 0          *PREV.ARR.TRAZ.CLI->
454900930525     C                   Z-ADD     WPOCRI        WR2POF            9 0
455000930525     C                   EXSR      RO2COV
455100930525     C                   Z-ADD     WR2NOV        WNOCRI            9 0          *N.ORE X CONS.RICH.I
455200930525     C* CALCOLO TEMPO TRA LA DATA DI ARRIVO E DATA CONSEGNA RICHIESTA
455300930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
455400930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
455500930608     C                   Z-ADD     WPOPTC        WR2POI            9 0
455600931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
455700930525     C                   EXSR      RO2COV
455800930525     C                   Z-ADD     WR2NOV        WNOCRF            9 0          *N.ORE X CONS.RICH.F
455900930525     C* SE N.ORE CONS.REALI < N.ORE INIZ.RANGE
456000930525     C     W98RCC        IFLT      WNOCRI                                       --- 2 -->
456100930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA REALE E N.ORE INIZIO RANGE
456200930525     C* E' IL RITARDO
456300930525     C     WNOCRI        SUB       W98RCC        W98NCC
456400050901     c
456500050901     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
456600050901     c*  --> trasformo il ritardo in anticipo
456700050901     c                   if        d98cca='2'
456800080116     c***                z-sub     w98ncc        w98ncc
456900080116     c                   z-add     0             w98ncc
457000050901     c                   endif
457100111018     c* Ignoro ritardo se pefect order
457200130221     c                   if        d98xco >*zeros
457300111018     c                   z-add     0             w98ncc
457400111018     c                   endif
457500061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
457600061121     c*  e sto leggendo la bolla in arrivo
457700061121     c*  --> trasformo il ritardo in anticipo
457800061121     c                   if        d98cca='1' and d98tbo='A'
457900080116     c***                z-sub     w98ncc        w98ncc
458000080116     c                   z-add     0             w98ncc
458100061121     c                   endif
458200070824     c
458300070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
458400070827     c*  mittente, --> consegnato in anticipo
458500070828     c                   if        d98tcr=' ' and d98nsp>0 and d98dcm>0
458600070827     c* SBR per vedere se tassativa
458700070827     c                   if        Callar5=' '
458800070827     c                   EXSR      LeggiAR5
458900070827     c                   endif
459000070827     c                   if        dcrpart<>'S'
459100070824     c                   z-sub     w98ncc        w98ncc
459200070824     c                   endif
459300070824     c                   endif
459400050907     c
459500050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
459600050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
459700050907     c*  lo trasformo in anticipo perchè non sono in ritardo
459800050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
459900050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
460000050916     c***                if        d98dfc>0
460100070828     c                   if        d98dcm=0 and w98ncc>0
460200050907     c                   z-sub     w98ncc        w98ncc
460300050907     c                   sub       oreILoDOPO    w98ncc
460400050907     c                   endif
460500050901     c
460600930525     C                   ELSE                                                   <-- 2 -->
460700070521     c* Non do ritardo se la consegna reale cade nel giorno
460800070521     c*  della consegna richiesta se presente IL
460900100224     c                   if        d98tcr=' ' and wpodcm>=wpocri and            ---2a -->
461000070521     c                             wpodcm<=wpocr2
461100070521     c                   clear                   w98ncc
461200100224     c                   else                                                   <--2a -->
461300070521     c
461400930525     C* SE N.ORE CONSEGNA TEORICHE < N.ORE CONSEGNA RICHIESTA (FINE RANGE)
461500090827     C***  D98TCC        IFLT      WNOCRF                                       --- 3 -->
461600090827     C     w99TCC        IFLT      WNOCRF                                       --- 3 -->
461700930525     C* LA DIFFERENZA TRA N. ORE PER CONSEGNA RICHIESTA E N.ORE PER CONS.REALE
461800930525     C* E' IL RITARDO O L'ANTICIPO
461900930525     C     W98RCC        SUB       WNOCRF        W98NCC
462000070115     c
462100070115     c* non do ritardo se comunque la consegna reale cade nel giorno
462200070115     c*  della consegna richiesta se presente IL
462300070521     c***                if        d98tcr=' ' and wpodcm>=wpocri and
462400070521     c***                          wpodcm<=wpocr2 and w98ncc>0
462500070521     c***                clear                   w98ncc
462600070521     c***                endif
462700070115     c
462800930525     C                   ELSE                                                   <-- 3 -->
462900930525     C* SE DATA DI CONSEGNA PREVISTA >= DATA DI CONSEGNA RICHIESTA (FINE RANGE)
463000930525     C* PRENDO PER BUONA LA DIFFERENZA TEMPI
463100930525     C                   Z-ADD     W98FCC        W98NCC                         *ORE NON CONF.CONS.I
463200930525     C                   END                                                    <-- 3 ---
463300100224     C                   END                                                    <-- 2a---
463400100224
463500100224     c* Ignoro il ritardo se la consegna anomala è 2-RESO mittente
463600100224     c                   select
463700100224     c                   when      d98cca='2'
463800100224     c                   z-add     0             w98nci
463900100224
464000100224     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
464100100224     c*  e sto leggendo la bolla in arrivo
464200100224     c                   when      d98cca='1' and d98tbo='A'
464300100224     c                   z-add     0             w98nci
464400100224     c                   endsl
464500100224     c
464600100224     C                   END                                                    <-- 2 ---
464700070521     C                   END                                                    <-- 1 ---
464800930524     C*
464900930524     C* CONTROLLO CONFORMITA
465000930423     C     W98NCC        IFLE      *ZEROS
465100930524     C                   MOVEL     'S'           D98CCC
465200930420     C                   ELSE
465300930524     C                   MOVEL     'N'           D98CCC
465400930420     C                   END
465500930420     C*
465600930420     C*-------------------------------------------------------------------
465700930420     C* CONFORMITA' RESA INTERNA
465800930420     C*-------------------------------------------------------------------
465900930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
466000930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
466100930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
466200050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
466300050914     c                   if        consideraora='S'
466400050914     C     w98fri        sub       24            W98NRI                         *ORE NON CONF.RESA.I
466500050914     c                   else
466600050914     C                   Z-ADD     W98FRI        W98NRI                         *ORE NON CONF.RESA.I
466700050914     c                   endif
466800080116     c
466900080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
467000090827     c                   select
467100090827     c                   when      d98cca='2' and w98nri>0
467200080116     c                   z-add     0             w98nri
467300090827     c
467400080116     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
467500080116     c*  e sto leggendo la bolla in arrivo
467600090827     c                   when      d98cca='1' and d98tbo='A' and
467700080116     c                                            w98nri>0
467800080116     c                   z-add     0             w98nri
467900110923     c
468000110923     c* Ignoro ritardo se Perfect Order
468100130221     c                   when      d98xco >*zeros
468200110923     c                   z-add     0             w98nri
468300110923     c
468400090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
468500090827     c* sono  in ritardo di 12 ore
468600091009     c                   when      d98tci=10  and
468700091012     c**                           primotipo=' ' and w98nri=0 and d98hmc>1030
468800091014     c**                           d98hci=1030   and w98nri=0 and d98hmc>1030
468900091014     c*
469000091014     c                             d98hci=1030
469100091014     C                   MOVEL     'OSC  '       WR3TAB
469200091014     C                   eval      WR3HHI=d98hmc
469300091014     C                   EXSR      RO3ORE
469400091014     c                   if        wr3matpom='M' and d98hmc>1030
469500131218     c                   exsr      ctrLAVH1030
469600131218
469700131218     c                   if        wlav1030<>'S'
469800091014     c                   add       12            w98nri
469900091014     c                   endif
470000131218     c                   endif
470100091014     c
470200091014     c***                z-add     12            w98nri
470300090827     c                   endsl
470400090827
470500930524     C                   ELSE                                                   <-- 1 -->
470600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
470700930524     C* DI DIFFERENZA SONO UGUALI A DIFF. TEORICHE/REALI
470800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
470900931206     C     WPODCM        ANDLE     WPOCR1
471000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
471100930525     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
471200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
471300931206     C                   Z-ADD     WPOCR1        WR2POF            9 0
471400930525     C                   EXSR      RO2COV
471500930525     C     WR2NOV        IFNE      *ZEROS
471600930525     C     WR2NOV        MULT      -1            W98NRI
471700930525     C                   END
471800930524     C                   ELSE                                                   <-- 2 -->
471900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
472000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
472100930524     C     WPODCM        IFLT      WPOCRI
472200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
472300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
472400930524     C                   ELSE
472500931206     C                   Z-ADD     WPOCR1        WPORFI
472600930524     C                   Z-ADD     WPODCM        WPORFF
472700930524     C                   END
472800930524     C* CALCOLO ORE DI DIFFERENZA
472900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
473000930524     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
473100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
473200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
473300930524     C                   EXSR      RO2COV
473400930524     C                   Z-ADD     WR2NOV        W98NRI                         *ORE NON CONF.RESA.I
473500050902     c
473600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
473700100224     c                   if        d98cca='2' and w98nri>0
473800100224     c****               if        d98cca='2' and w98nri>0 and wpodcm<wpocri
473900080116     c***                z-sub     w98nri        w98nri
474000080116     c                   z-add     0             w98nri
474100050902     c                   endif
474200111018     c* Ignoro ritardo se pefect order
474300130221     c                   if        d98xco >*zeros
474400111018     c                   z-add     0             w98nri
474500111018     c                   endif
474600061121     c* Ignoro il ritardo se la consegna anomala è 1-DITOTTATA
474700061121     c*  e sto leggendo la bolla in arrivo
474800100224     c                   if        d98cca='1' and d98tbo='A' and w98nri>0
474900100224     c***                if        d98cca='1' and d98tbo='A' and
475000100224     c***                                         w98nri>0 and wpodcm<wpocri
475100080116     c***                z-sub     w98nri        w98nri
475200080116     c                   z-add     0             w98nri
475300061121     c                   endif
475400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
475500070827     c*  mittente, --> consegnato in anticipo
475600070828     c                   if        d98tcr=' ' and d98nsp>0  and
475700070828     c                                            w98nri>0 and wpodcm<wpocri
475800070827     c                   if        CallAR5=' '
475900070827     c                   EXSR      LeggiAR5
476000070824     c                   endif
476100070827     c                   if        dcrpart<>'S'
476200070824     c                   z-sub     w98nri        w98nri
476300070824     c                   endif
476400070824     c                   endif
476500050907     c
476600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
476700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
476800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
476900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
477000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
477100050916     c**                 if        d98dfc>0 and w98nri>0 and wpodcm<wpocri
477200050916     c                   if        d98dcm=0 and w98nri>0 and wpodcm<wpocri
477300050907     c                   z-sub     w98nri        w98nri
477400050907     c                   sub       oreILoDOPO    w98nri
477500050907     c                   endif
477600050907     c
477700050907     C                   END                                                    <-- 2 ---
477800930524     C                   END                                                    <-- 1 ---
477900930420     C* CALCOLO LE ORE DI RESA
478000930420     C* CONTROLLO CONFORMITA
478100930423     C     W98NRI        IFLE      *ZEROS
478200930420     C                   MOVEL     'S'           D98CRI                         *CONF.RESA INTERNA
478300930420     C                   ELSE
478400930420     C                   MOVEL     'N'           D98CRI                         *CONF.RESA INTERNA
478500930420     C                   END
478600930420     C*
478700930420     C*-------------------------------------------------------------------
478800930420     C* CONFORMITA' RESA CLIENTE
478900930420     C*-------------------------------------------------------------------
479000930524     C* SE NON ESISTE LA DATA DI CONSEGNA RICHIESTA PRENDO PER BUONI I VALORI
479100930524     C* DI DIFFERENZA TRA TEORICHE E REALI, ALTRIMENTI CALCOLO ORE DI NON CONF.
479200930524     C     D98DCR        IFEQ      *ZEROS                                       --- 1 -->
479300050914     c* Se Presente ora richiesta, entro le 24 ore non c'è ritardo
479400050914     c                   if        consideraora='S'
479500050914     C     w98frc        sub       24            W98NRC                         *ORE NON CONF.RESA.C
479600050914     c                   else
479700050914     C                   Z-ADD     W98FRC        W98NRC                         *ORE NON CONF.RESA.C
479800050914     c                   endif
479900090827     c
480000080116     c* Non considero ritardi in presenza di consegna anomala RESO MIT
480100090827     c                   select
480200090827     c                   when      d98cca='2' and w98nrc>0
480300080116     c                   z-add     0             w98nrc
480400090827
480500080116     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
480600080116     c*  e sto leggendo la bolla in arrivo
480700090827     c                   when      d98cca='1' and d98tbo='A' and
480800080116     c                                            w98nrc>0
480900080116     c                   z-add     0             w98nrc
481000110923     c
481100110923     c* Ignoro ritardo se Perfect Order
481200130221     c                   when      d98xco >*zeros
481300110923     c                   z-add     0             w98nrc
481400090827     c
481500090827     c* Consegna conforme, ore 10 --> se consegnata tra le 10,31 e le 12,59
481600090827     c* sono  in ritardo di 12 ore
481700091009     c                   when      d98tcc=10  and
481800091012     c**                           primotipo=' ' and w98nrc=0 and d98hmc>1030
481900091014     c**                           d98hce=1030   and w98nrc=0 and d98hmc>1030
482000091014     c                             d98hce=1030
482100091014     C                   MOVEL     'OSC  '       WR3TAB
482200091014     C                   eval      WR3HHI=d98hmc
482300091014     C                   EXSR      RO3ORE
482400091014     c                   if        wr3matpom='M' and d98hmc>1030
482500131218     c                   exsr      ctrLAVH1030
482600131218
482700131218     c                   if        wlav1030<>'S'
482800091014     c                   add       12            w98nrc
482900091014     c                   endif
483000131218     c                   endif
483100091014     c
483200091014     c**                 z-add     12            w98nrc
483300090827     c                   endsl
483400090827     c
483500930524     C                   ELSE                                                   <-- 1 -->
483600930524     C* SE LA DATA DI CONSEGNA E' ALL'INTERNO DEL RANGE DI CONS.RICH. LE ORE
483700930524     C* DI DIFFERENZA SONO UGUALI A ZERO
483800930524     C     WPODCM        IFGE      WPOCRI                                       --- 2 -->
483900931206     C     WPODCM        ANDLE     WPOCR2
484000930525     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
484100930525     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
484200930525     C                   Z-ADD     WPODCM        WR2POI            9 0
484300931206     C                   Z-ADD     WPOCR2        WR2POF            9 0
484400930525     C                   EXSR      RO2COV
484500930525     C     WR2NOV        IFNE      *ZEROS
484600930525     C     WR2NOV        MULT      -1            W98NRC
484700930525     C                   END
484800930524     C                   ELSE                                                   <-- 2 -->
484900930524     C* SE DATA CONSEGNA INFERIORE DATA CONS.RICH.INFERIORE, CALCOLO DIFFERENZA
485000930524     C* TRA QUESTE DUE DATE ALTRIMENTI TRA LA DATA CONS. E DATA CONS.RICH.SUPER.
485100930524     C     WPODCM        IFLT      WPOCRI
485200930524     C                   Z-ADD     WPODCM        WPORFI            9 0          *RIFERIMENTO INIZIO
485300930524     C                   Z-ADD     WPOCRI        WPORFF            9 0          *RIFERIMENTO FINE
485400930524     C                   ELSE
485500931206     C                   Z-ADD     WPOCR2        WPORFI
485600930524     C                   Z-ADD     WPODCM        WPORFF
485700930524     C                   END
485800930524     C* CALCOLO ORE DI DIFFERENZA
485900930524     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
486000930524     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
486100930524     C                   Z-ADD     WPORFI        WR2POI            9 0
486200930524     C                   Z-ADD     WPORFF        WR2POF            9 0
486300930524     C                   EXSR      RO2COV
486400930524     C                   Z-ADD     WR2NOV        W98NRC                         *ORE NON CONF.RESA.C
486500050902     c
486600050902     c* Non considero ritardi in presenza di consegna anomala RESO MIT
486700100224     c                   if        d98cca='2' and w98nrc>0
486800100224     c***                if        d98cca='2' and w98nrc>0 and wpodcm<wpocri
486900080116     c***                z-sub     w98nrc        w98nrc
487000080116     c                   z-add     0             w98nrc
487100050902     c                   endif
487200111018     c* Ignoro ritardo se pefect order
487300130221     c                   if        d98xco >*zeros
487400111018     c                   z-add     0             w98nrc
487500111018     c                   endif
487600070521     c* Ignoro il ritardo se la consegna anomala è 1-DIROTTATA
487700061121     c*  e sto leggendo la bolla in arrivo
487800100224     c                   if        d98cca='1' and d98tbo='A' and w98nrc>0
487900100224     c***                if        d98cca='1' and d98tbo='A' and
488000100224     c***                                         w98nrc>0 and wpodcm<wpocri
488100080116     c***                z-sub     w98nrc        w98nrc
488200080116     c                   z-add     0             w98nrc
488300061121     c                   endif
488400070827     c* Ignoro il ritardo se cons richiesta non immessa in partenza cioè dal
488500070827     c*  mittente, --> consegnato in anticipo
488600070828     c                   if        d98tcr=' ' and d98nsp>0 and
488700070828     c                                            w98nrc>0 and wpodcm<wpocri
488800070827     c                   if        CallAR5=' '
488900070827     c                   EXSR      LeggiAR5
489000070827     c                   endif
489100070827     c                   if        dcrpart<>'S'
489200070824     c                   z-sub     w98nrc        w98nrc
489300070824     c                   endif
489400070824     c                   endif
489500050907     c
489600050907     c* Se sto calcolando la consegnabilità, eventuale ritardo per data
489700050907     c*  consegnabilità < data consegna richiesta (IL o DOPO IL)
489800050907     c*  lo trasformo in anticipo perchè non sono in ritardo
489900050907     c*  ma la spedizione non è ancora da consegnare (in sto caso
490000050907     c*  ignoro la data consegna se già ci fosse, il pgm fa già così)
490100050916     c***                if        d98dfc>0 and w98nrc>0 and wpodcm<wpocri
490200050916     c                   if        d98dcm=0 and w98nrc>0 and wpodcm<wpocri
490300050907     c                   z-sub     w98nrc        w98nrc
490400050907     c                   sub       oreILoDOPO    w98nrc
490500050907     c                   endif
490600050907     c
490700050907     C                   END                                                    <-- 2 ---
490800930524     C                   END                                                    <-- 1 ---
490900930420     C* CALCOLO LE ORE DI RESA
491000930420     C* CONTROLLO CONFORMITA
491100930423     C     W98NRC        IFLE      *ZEROS
491200930420     C                   MOVEL     'S'           D98CRC                         *CONF.RESA CLIENTE
491300930420     C                   ELSE
491400930420     C                   MOVEL     'N'           D98CRC                         *CONF.RESA CLIENTE
491500930420     C                   END
491600930420     C*
491700930420     C                   ENDSR
491800070827     c
491900131218     c********************************************************************
492000131218     c     CtrlavH1030   BEGSR
492100131218     c********************************************************************
492200131218     c                   clear                   wlav1030          1
492300140127     c* Se c'è ritardo, verifico se presente evento l.AVV o GIA con orario < nella stessa data
492400131218     c                   z-add     1             xx                4 0
492500131218     c                   dow       evc(xx)<>*blanks
492600140127     c******             if        evt(xx)='A' and evd(xx)=d98dcm
492700140127     c                   if        evd(xx)=d98dcm
492800131218     c                   if        evo(xx)<=1030
492900131218     c                   eval      wlav1030='S'
493000131218     c                   leave
493100131218     c                   endif
493200131218     c                   endif
493300131218     c                   add       1             xx
493400131218     c                   enddo
493500131218     c                   ENDSR
493600070827     c********************************************************************
493700070827     c* Controllo record GEN di FIAR5
493800070827     c********************************************************************
493900070827     c     LeggiAR5      BEGSR
494000070827     c                   clear                   DCRPART           1
494100141104     c                   clear                   dar5gen
494200141104     c                   clear                   gc1PART           2
494300141104     c                   clear                   gc2PART           2
494400070827     c     kar5          chain     fiar501l
494500070827    1c                   if        not %found
494600070827     c                   eval      dcrpart='S'
494700141104     c                   eval      gc1part=d98gc1
494800141104     c                   eval      gc2part=d98gc2
494900070827   x1c                   else
495000070827     c* i dati devono essere quelli immessi dalla partenza
495100070827     c                   movel     ar5uni        dar5gen
495200070827     c
495300070827     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
495400070827     c*  ma il record GEN è stato scritto per altri motivi
495500070827    2c                   if        §ar5mdp='S'
495600070827     c                   movel     d98dcr        alfadcr           8
495700141104     c                   eval      gc1part=§ar5gc1
495800141104     c                   eval      gc2part=§ar5gc2
495900070827     c
496000070827    3c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
496100070827     c                   eval      dcrpart='S'
496200070827    3c                   endif
496300070827     c
496400070827   x2c                   else
496500070827     c                   eval      dcrpart='S'
496600141106     c                   eval      gc1part=d98gc1
496700141106     c                   eval      gc2part=d98gc2
496800070827    2c                   endif
496900070827    1c                   endif
497000070827     c
497100140402     c* Memorizzo il flag pickup 2
497200140402     c                   eval      wpick2=§AR5PK2
497300140402     c
497400150401     c*
497500150414     c**                 if        d98cas>0 or d98ffd<>' ' or
497600150414     c**                           d98tc1='A'  or d98tc2='A' or
497700150414     c**                           d98tc1='S'  or d98tc2='S'
497800150401     c     kar4          chain     fiar401l
497900150401     c                   if        %found(fiar401l)
498000150401     c                   eval      dsbl4L=ar4not
498100150401     c                   endif
498200150401     c* Solo per fermo deposito verifico se inviato alert
498300150401     c                   if        d98ffd<>' '
498400150401     c                   clear                   fnlv82ds
498500150401     c                   eval      ILV82AAS=d98aas
498600150401     c                   eval      ILV82LNP=d98lnp
498700150401     c                   eval      ILV82NRS=d98nrs
498800150401     c                   eval      ILV82NSP=d98nsp
498900150401     c                   eval      ILV82TIPA='FD'
499000150401     c                   call      'FNLV82R'
499100150401     c                   parm                    fnlv82ds
499200150401     c                   endif
499300150414     c**                 endif
499400150401
499500150401     c                   eval      callar5='S'
499600150401
499700070827     c                   ENDSR
499800930913     C********************************************************************
499900930913     C*   CALCNS - CALCOLA LE ORE DI CONSEGNABILITA' ALL'INTERNO DI UN
500000930913     C*            DETERMINATO RANGE DI DATE/ORA OPPURE AD UNA PARTICOLARE
500100930913     C*            DATA/ORA (FINE RANGE)
500200930913     C********************************************************************
500300930913     C     CALCNS        BEGSR
500400930914     C*
500500930914     C* SE NON ANCORA GESTITA DATA ARR.TRAZ.REALE
500600930914     C* UTILIZZO LA DATA PREVISTA ARRIVO TRAZIONE INT. COME LIMITE MINIMO RANGE
500700930914     C     WPODTI        IFEQ      *ZEROS
500800960912     C                   Z-ADD     WPOPTI        WPOIRC            9 0          *INIZIO RANGE CONSEG
500900930914     C                   ELSE
501000930914     C                   Z-ADD     WPODTI        WPOIRC
501100930914     C                   END
501200930914     C*
501300930914     C* SE NON ANCORA CONSEGNATA
501400930917     C* UTILIZZO LA DATA ODIERNA COME LIMITE MASSIMO RANGE
501500050922     c* in realtà nella data consegna c'e' la data fine calcolo
501600050922     c*  conseganbilità per cui non dovrebbe mai essere vuoto il campo
501700930914     C     WPODCM        IFEQ      *ZEROS
501800050922     C                   Z-ADD     WPOATTData00  WPOFRC            9 0          >FINE RANGE CONSEGNA
501900930914     C                   ELSE
502000930916     C                   Z-ADD     WPODCM        WPOFRC
502100930914     C                   END
502200930914     C*
502300930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
502400930914     C* SONO ENTRAMBE INFERIORI ALLA DATA DI ARRIVO DELLA TRAZ. --> NON CALCOLO
502500930914     C     WPODIC        IFLT      WPOIRC                                       --- 1 -->
502600930914     C     WPODFC        ANDLT     WPOIRC
502700930914     C                   ELSE
502800930914     C*
502900930914     C* SE LA DATA DI INIZIO CONSEGNABILITA E LA DATA FINE CONSEGNABILITA
503000930914     C* SONO ENTRAMBE SUPERIORI ALLA DATA DI CONSEGNA --> NON CALCOLO
503100930914     C     WPODIC        IFGT      WPOFRC                                       --- 2 -->
503200930914     C     WPODFC        ANDGT     WPOFRC
503300930914     C                   ELSE
503400930916     C*
503500930916     C* SE IN INPUT SOLO DATA FINE ED E' > DELLA DATA CONSEGNA --> NON CALCOLO
503600930916     C     WPODIC        IFEQ      *ZEROS                                       --- 3 -->
503700930916     C     WPODFC        ANDGT     WPOFRC
503800930916     C                   ELSE
503900930916     C*
504000930916     C* SE IN INPUT SOLO DATA FINE ED E' < DELLA DATA ARR.TRAZ.--> NON CALCOLO
504100930916     C     WPODIC        IFEQ      *ZEROS                                       --- 4 -->
504200930916     C     WPODFC        ANDLT     WPOIRC
504300930916     C                   ELSE
504400930914     C*
504500930916     C* SE LA DATA DI INIZIO CONSEGNAB. > DATA CONSEGNA MERCE -->            1
504600930914     C* IMPOSTO LA DATA INIZIO COME LA DATA CONSEGNA MERCE
504700930914     C     WPODIC        IFNE      *ZEROS
504800930914     C     WPODIC        ANDGT     WPOFRC
504900930914     C                   Z-ADD     WPOFRC        WPODIC
505000930914     C                   ENDIF
505100930914     C*
505200930916     C* SE LA DATA DI FINE CONSEGNAB. > DATA CONSEGNA MERCE --->             2
505300930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
505400930914     C     WPODFC        IFGT      WPOFRC
505500930914     C                   Z-ADD     WPOFRC        WPODFC
505600930914     C                   ENDIF
505700930914     C*
505800930916     C* SE LA DATA DI INIZIO CONSEGN. < DATA ARRIVO TRAZIONE --->            3
505900930914     C* IMPOSTO LA DATA INIZIO COME LA DATA ARRIVO TRAZIONE
506000930914     C     WPODIC        IFNE      *ZEROS
506100930914     C     WPODIC        ANDLT     WPOIRC
506200930914     C                   Z-ADD     WPOIRC        WPODIC
506300930914     C                   ENDIF
506400930914     C*
506500930916     C* SE LA DATA DI FINE CONSEGN. < DATA ARRIVO TRAZIONE -->               4
506600930914     C* IMPOSTO LA DATA FINE COME LA DATA CONSEGNA MERCE
506700930914     C     WPODFC        IFLT      WPOIRC
506800930914     C                   Z-ADD     WPOIRC        WPODFC
506900930914     C                   ENDIF
507000930914     C*
507100930914     C* SE E' VALIDA LA DATA FINE E NON LA DATA INIZIO CONSEGNABILITA'-->
507200931210     C* IMPOSTO IL RANGE PER IL CALCOLO (LA ROUTINE 9 RICHIEDE IN INPUT DUE
507300930915     C* PROGRESSIVI DIVERSI QUINDI AUMENTO IL SECONDO DI 12 ORE)
507400930916     C*
507500931210     C***********WPODIC    IFNE *ZEROS
507600931210     C***********WPODFC    ANDNE*ZEROS
507700931210     C*********************ADD  12        WPODFC
507800931210     C*********************ENDIF
507900930914     C     WPODIC        IFEQ      *ZEROS
508000930914     C     WPODFC        ANDNE     *ZEROS
508100931130     C                   Z-ADD     WPODFC        WPODIC
508200931210     C*********************ADD  12        WPODFC
508300930916     C                   ENDIF
508400930913     C*
508500930913     C* ESEGUO CALCOLO SOLO SE E' STATO DATO IN INPUT IL RANGE DI CONSEGNABILITA
508600930916     C     WPODFC        IFNE      *ZEROS                                       --- 5 -->
508700930914     C* INTERNA
508800050908     c                   eval      Usacons='S'
508900930914     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
509000930914     C                   MOVEL     'I'           WIC                            *INTERNO/CLIENTE
509100931210     C                   Z-ADD     WPODIC        WR9POI            9 0          *INIZIO CONSEGNAB.
509200931210     C                   Z-ADD     WPODFC        WR9POF            9 0          *FINE CONSEGNAB.
509300940324     C                   EXSR      RO9COV
509400931210     C                   Z-ADD     WR9NOV        W98OCI                         *ORE DI CONSEGNAB.IN
509500930914     C* CLIENTE
509600930914     C                   MOVEL     'C'           WIC                            *INTERNO/CLIENTE
509700931210     C                   EXSR      RO9COV
509800931210     C                   Z-ADD     WR9NOV        W98OCC                         *ORE DI CONSEGNAB.CL
509900050908     c                   eval      Usacons=' '
510000930914     C*
510100930916     C                   ENDIF                                                  <-- 5 ---
510200930916     C                   ENDIF                                                  <-- 4 ---
510300930916     C                   ENDIF                                                  <-- 3 ---
510400930914     C                   ENDIF                                                  <-- 2 ---
510500930914     C                   ENDIF                                                  <-- 1 ---
510600930913     C*
510700930913     C                   ENDSR
510800930325     C********************************************************************
510900930414     C*   RO1CDP - ROUTINE (1) PER DETERMINARE DATE PREVISTE
511000930406     C*            INPUT:  -WR1POI (PROGR.ORE DATA DI PARTENZA)
511100930406     C*                    -WR1NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
511200930406     C*           OUTPUT:  -WR1POO (PROGR.ORE DATA PREVISTA)
511300930325     C********************************************************************
511400930325     C     RO1CDP        BEGSR
511500930414     C*
511600931126     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
511700931126     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
511800931126     C*
511900931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
512000931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
512100150319     C*****WPOLAF        IFEQ      *ZEROS                                       --- 0 -->*FINE L.AVV
512200150319     C     WPOGIF        ifeq      *ZEROS                                                *EVEN.GIAC
512300931126     C     WPODCG        ANDEQ     *ZEROS                                                *DATA CH.GI
512400060420     C     WPOpaif       ANDEQ     *ZEROS                                                *DATA CH.GI
512500930326     C*
512600930414     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
512700931130     C                   Z-ADD     WR1POI        WR1POO            9 0
512800930325     C*
512900930325     C* CICLO FINO A CHE:
513000930325     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
513100930407     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
513200930414     C     WR1NOI        DOWGT     *ZEROS                                       --- 1 -->
513300930406     C     WR1POO        ANDLT     WPOLIM
513400930414     C*
513500930407     C                   Z-ADD     WR1POO        WPOROU            9 0
513600930325     C                   MOVEL     'N'           WSLITT            1
513700930414     C*
513800930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
513900060420     C*
514000931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
514100150605     c     wdee          andeq     ' '
514200930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
514300930414     C                   END
514400930511     C*
514500931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
514600150608     c     wdee          andeq     ' '
514700930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
514800930511     C                   END
514900930414     C*
515000930325     C     WSLITT        IFEQ      'N'
515100930423     C                   SUB       12            WR1NOI            5 0
515200930325     C                   END
515300931130     C                   ADD       12            WR1POO            9 0
515400930414     C*
515500931223     C                   ENDDO                                                  <-- 1 ---
515600931126     C*
515700931126     C                   ELSE                                                   <-- 0 -->
515800931126     C* SE VI SONO GIACENZE O LASCIATO AVVISO PRENDO COME DATA DI PREVISTA
515900931126     C* CONSEGNA, LA MAGGIORE TRA LA DATA DI FINE RANGE GIAC. O L.AVV.
516000931126     C* AGGIUNGO 12 ORE PERCHE LE DATE PREVISTE SI RIFERISCONO AL TERMINE ULTIMO
516100960129     C     WPOLAF        IFGT      WPODCG
516200960129     C                   Z-ADD     WPOLAF        WTM090            9 0
516300960129     C                   ELSE
516400960129     C                   Z-ADD     WPODCG        WTM090
516500960129     C                   ENDIF
516600960129     C     WPOGIF        IFGT      WTM090
516700960129     C                   Z-ADD     WPOGIF        WTM090
516800960129     C                   ENDIF
516900060420     C     WPOpaif       IFGT      WTM090
517000060420     C                   Z-ADD     WPOpaif       WTM090
517100060420     C                   ENDIF
517200960129     C*
517300960129     C     12            ADD       WTM090        WR1POO
517400960129     C                   ENDIF                                                  <-- 0 ---
517500930512     C*
517600931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
517700931202     C     WR1POO        SUB       12            WR7POI
517800931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
517900931202     C                   ADD       12            WR7POO
518000931202     C     WR7POO        IFGT      WR1POO
518100931202     C                   Z-ADD     WR7POO        WR1POO
518200931202     C                   END
518300930406     C*
518400930325     C                   ENDSR
518500930414     C********************************************************************
518600930414     C*   RO2COV - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE
518700930414     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
518800930414     C*                    -WR2POF (PROGR.ORE DATA FINE)
518900930414     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
519000930414     C********************************************************************
519100930414     C     RO2COV        BEGSR
519200091014     c                   clear                   salvaEveL
519300091014     c                   clear                   salvaEveG
519400931130     C*
519500930414     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
519600930414     C                   Z-ADD     *ZEROS        WR2NOV
519700930414     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
519800931130     C                   Z-ADD     WR2POI        WR2POW
519900930414     C*
520000930414     C* CICLO FINO A CHE:
520100930414     C*   - DATA INIZIO SUPERATO DATA FINE
520200930415     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
520300931223     C     WR2POW        ANDLT     WPOLIM
520400930414     C*
520500930414     C                   Z-ADD     WR2POW        WPOROU
520600930414     C                   MOVEL     'N'           WSLITT            1
520700930414     C*
520800930414     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
520900930414     C*
521000930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
521100930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
521200930414     C                   EXSR      CHKGIA                                       *CONTR.GIACENZA
521300930414     C                   END
521400930414     C*
521500930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
521600930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
521700930414     C                   EXSR      CHKLAV                                       *CONTR.LASC.AVVISO
521800930618     C                   END
521900060420     c
522000060420     C     WTC           IFEQ      'C'                                          *CONSEGNA
522100060420     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
522200060420     C                   EXSR      CHKcepa                                      *CONTR.TURNO CHIUSUR
522300060420     C                   END
522400060420     c
522500931201     C*
522600931201     C     WTC           IFEQ      'C'                                          *CONSEGNA
522700931201     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
522800931201     C                   EXSR      CHKTUC                                       *CONTR.TURNO CHIUSUR
522900931201     C                   END
523000060418     c
523100060418     C     WTC           IFEQ      'C'                                          *CONSEGNA
523200060418     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
523300060418     C                   EXSR      CHKCEPG                                      *CONTR.TURNO CHIUSUR
523400060418     C                   END
523500930414     C*
523600930414     C     WTC           IFEQ      'C'                                          *CONSEGNA
523700930414     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
523800930414     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
523900930414     C                   END
524000930511     C*
524100930511     C     WTC           IFEQ      'C'                                          *CONSEGNA
524200930511     C     WSLITT        ANDEQ     'N'                                          *ORA VALIDA
524300930511     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
524400930511     C                   END
524500930414     C*
524600930414     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
524700930414     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
524800930414     C     WSLITT        IFEQ      'N'
524900930423     C                   ADD       12            WR2NOV            5 0
525000930414     C                   END
525100931130     C                   ADD       12            WR2POW            9 0
525200930414     C*
525300931223     C                   ENDDO                                                  <-- 1 ---
525400930414     C*
525500930414     C                   ENDSR
525600941228     C********************************************************************
525700941228     C*   RO2COP - ROUTINE (2) PER CALCOLO ORE VALIDE TRA DUE DATE -PARTENZA-
525800941228     C*            INPUT:  -WR2POI (PROGR.ORE DATA INIZIO)
525900941228     C*                    -WR2POF (PROGR.ORE DATA FINE)
526000941228     C*                    -WR2IC  (I=INTERNO,C=CLIENTE)
526100941228     C*           OUTPUT:  -WR2NOV (N.ORE VALIDE TRA LE DUE DATE)
526200941228     C********************************************************************
526300941228     C     RO2COP        BEGSR
526400941228     C*
526500941228     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
526600941228     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
526700941228     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
526800941228     C                   Z-ADD     *ZEROS        WR2NOV
526900941228     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
527000941228     C                   Z-ADD     WR2POI        WR2POW
527100941228     C*
527200941228     C* CICLO FINO A CHE:
527300941228     C*   - DATA INIZIO SUPERATO DATA FINE
527400941228     C     WR2POW        DOWLT     WR2POF                                       --- 1 -->
527500941228     C*
527600950215     C                   Z-ADD     WR2POW        WPOROU
527700941228     C                   MOVEL     'N'           WSLITT
527800941228     C* TRASFORMA IL PROGR. ORE IN DATA
527900950215     C*******              Z-ADDWR2POW    WORE
528000950215     C*******              EXSR OREDAT
528100950215     C*******              Z-ADDWDAT      W12DAT  80
528200950215     C*******              Z-ADDWRES      W12RES  30
528300941228     C*
528400941228     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
528500941228     C*
528600941228     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
528700941228     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
528800941228     C     WSLITT        IFEQ      'N'
528900941228     C                   ADD       12            WR2NOV            5 0
529000941228     C                   END
529100941228     C                   ADD       12            WR2POW            9 0
529200941228     C*
529300941228     C* SE LE ORE VALIDE SONO SUPERIORI A 999 ESCO PER EVITARE CHE IL PGM
529400941228     C* ENTRI IN LOOP
529500941228     C     WR2NOV        IFGT      999
529600941228     C                   LEAVE                                                  *VA A ENDDO
529700941228     C                   END
529800941228     C*
529900941228     C                   ENDDO                                                  <-- 1 ---
530000941228     C*
530100941228     C                   ENDSR
530200930510     C********************************************************************
530300930510     C*   RO4CDP - ROUTINE (4) PER DETERMINARE UNA DATA PARTENDO DA UNA
530400930511     C*            DATA E DA UN N.DI ORE SALTANDO SOLO I FESTIVI.
530500930510     C*            INPUT:  -WR4POI (PROGR.ORE DATA DI PARTENZA)
530600930510     C*                    -WR4NOI (N.ORE DA AGGIUNGERE ALLA DATA)
530700050905     c*                     il num ore deve essere un multiplo di 12
530800050905     c*                     oppure 1 ora
530900930510     C*           OUTPUT:  -WR4POO (PROGR.ORE DATA RAGGIUNTA)
531000930510     C********************************************************************
531100930510     C     RO4CDP        BEGSR
531200930510     C*
531300930510     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER DETERMINARE DATA
531400930510     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
531500050905     c                   movel     '1'           wgiro             1
531600930510     C*
531700930510     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
531800930510     C                   Z-ADD     WR4POI        WR4POO            9 0
531900930510     C*
532000930510     C* CICLO FINO A CHE:
532100930510     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA)
532200930510     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
532300930510     C     WR4NOI        DOWGT     *ZEROS                                       --- 1 -->
532400930510     C     WR4POO        ANDLT     WPOLIM
532500930510     C*
532600141106     c                   if        nosomma<>'N'
532700931130     C                   ADD       12            WR4POO            9 0
532800141106     c                   endif
532900930510     C                   Z-ADD     WR4POO        WPOROU            9 0
533000930510     C                   MOVEL     'N'           WSLITT            1
533100930510     C*
533200930510     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
533300050905     c
533400050905     C* Se non è giorno festivo, aggiungo le ore esatte se < 12
533500930510     C     WSLITT        IFEQ      'N'
533600050905     c
533700050905     c* Se è il primo giro se ho sommato 1 ora, tolgo le 1 in più
533800050905     c*  se cado in giorno non festivo
533900050905     c                   if        wr4noi=1 and wgiro='1'
534000050905     c                   sub       12            wr4poo
534100050905     c                   add       wr4noi        wr4poo
534200050905     c                   endif
534300050905     c*
534400050905     c* Se num ore da sommare è 1 e non sono al primo giro, significa che
534500050905     c*  ho trovato dei festivi per cui devo aggiungere 1 ora alle ore
534600050905     c*  calcolate dal pgm
534700050905     c* non devo togliere 12 vore perchè non funzionarebbe: aggingendo
534800050905     c*  12 arriva alle 24 del giorno festivo ma per lui sono le 00:00
534900050905     c*  del giorno sucessivo non festivo
535000050905     c                   if        wr4noi=1 and wgiro<>'1'
535100050905     c                   add       wr4noi        wr4poo
535200050905     c                   endif
535300050905     c
535400930510     C                   SUB       12            WR4NOI            5 0
535500930510     C                   END
535600141106     c
535700141106     c                   if        nosomma= 'N'
535800141106     C                   ADD       12            WR4POO            9 0
535900141106     c                   endif
536000930510     C*
536100050905     c                   eval      wgiro='2'
536200931223     C                   ENDDO                                                  <-- 1 ---
536300930510     C*
536400930510     C                   ENDSR
536500931130     C********************************************************************
536600931130     C*   RO5COV - ROUTINE (5) PER CALCOLO ORE VALIDE TRA DUE DATE (ORE REALI)
536700931130     C*            INPUT:  -WR5POI (PROGR.ORE DATA INIZIO)
536800931130     C*                    -WR5POF (PROGR.ORE DATA FINE)
536900931130     C*           OUTPUT:  -WR5NOV (N.ORE VALIDE TRA LE DUE DATE)
537000931130     C********************************************************************
537100931130     C     RO5COV        BEGSR
537200931130     C*
537300931130     C* AZZERO IL DETTAGLIO N.ORE CONSIDERATE PER CALCOLO ORE VALIDE
537400060418     C                   Z-ADD     *ZEROS        W98EG                          *TURNO CHIUSURA
537500060420     C                   Z-ADD     *ZEROS        W98EA                          *TURNO CHIUSURA
537600060418     C                   Z-ADD     *ZEROS        W98HH                          *TURNO CHIUSURA
537700931130     C                   Z-ADD     *ZEROS        W98HL                          *LASCIATO AVVISO
537800931130     C                   Z-ADD     *ZEROS        W98HG                          *GIACENZA
537900931130     C                   Z-ADD     *ZEROS        W98HF                          *FESTIVI
538000931130     C                   Z-ADD     *ZEROS        W98HO                          *CONTRASSEGNO
538100931130     C                   Z-ADD     *ZEROS        W98HD                          *FERMO DEPOSITO
538200931130     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
538300931130     C                   Z-ADD     *ZEROS        WR5NOV
538400940714     C*
538500940714     C* SE LA DATA INIZIALE E' MAGGIORE O UGUALE ALLA DATA FINALE ESCO E ORE=0
538600940714     C     WR5POI        IFLT      WR5POF                                       --- 0 -->
538700931207     C*
538800931207     C* TROVO IL TEMPO DI CONSEGNA TEORICO
538900940908     C     WIC           IFEQ      'I'
539000090827     C*****              Z-ADD     D98TCI        WPOTCT            9 0
539100090827     C                   Z-ADD     w99TCI        WPOTCT            9 0
539200931207     C                   ELSE
539300090827     C*****              Z-ADD     D98TCC        WPOTCT
539400090827     C                   Z-ADD     w99TCC        WPOTCT
539500931207     C                   END
539600931130     C*
539700931207     C* CARICO IN SCHIERA GLI ELEMENTI NECESSARI
539800060420     C                   clear                   COV
539900931207     C                   Z-ADD     WR5POI        COV(1)                         *INIZIO
540000931207     C                   Z-ADD     WR5POF        COV(2)                         *FINE
540100931207     C                   Z-ADD     2             CO                3 0
540200931207     C                   Z-ADD     WPODCG        WR5COV            9 0          *FINE GIAC.
540300931207     C                   EXSR      RO5SUB
540400981102     C                   DO        99            IC                3 0
540500931207     C     LAF(IC)       IFNE      *ZEROS
540600931207     C                   Z-ADD     LAF(IC)       WR5COV                         *FINE L.AVV.
540700931207     C                   EXSR      RO5SUB
540800931207     C                   ENDIF
540900931207     C                   ENDDO
541000060420     c
541100060420     C                   DO        99            IC                3 0
541200060420     C     paif(IC)      IFNE      *ZEROS
541300060420     C                   Z-ADD     paif(IC)      WR5COV                         *FINE L.AVV.
541400060420     C                   EXSR      RO5SUB
541500060420     C                   ENDIF
541600060420     C                   ENDDO
541700931207     C* ORDINA LA SCHIERA COV
541800931207     C                   SORTA     COV
541900931130     C*
542000931207     C                   Z-ADD     *ZEROS        WR5NOV            9 0
542100931207     C* LOOP PER TUTTI GLI ELEMENTI DELLA SCHIERA
542200981102     C                   DO        98            IC                             --- 1 -->
542300931207     C     IC            ADD       1             I1C               3 0
542400931207     C* SE UNO DEGLI ELEMENTI E' A ZERO NON CALCOLO
542500931207     C     COV(IC)       IFEQ      *ZEROS                                       --- 2 -->
542600931207     C     COV(I1C)      OREQ      *ZEROS
542700931207     C                   ELSE
542800931130     C* LANCIO PRIMA PARTE DA DATA INPUT INIZIO E DATA FINE L.AVV. O GIAC.
542900931207     C                   Z-ADD     COV(IC)       WR2POI                         *INIZIO
543000931207     C                   Z-ADD     COV(I1C)      WR2POF                         *FINE
543100931130     C                   EXSR      RO2COV
543200931207     C                   ADD       WR2NOV        WR5NOV            9 0
543300931209     C* SE NON SONO IN DATA CONSEGNA
543400931209     C* E SE SONO IN ANTICIPO IMPOSTO ORE VALIDE FINO AL MOMENTO = TEORICHE
543500931207     C     WR5NOV        IFLT      WPOTCT
543600931209     C     COV(I1C)      ANDNE     WPODCM
5437009709111009 C******************** Z-ADDWPOTCT    WR5NOV
5438009709111009 C     WPOTCT        SUB       12            WR5NOV
543900931130     C                   END
544000931207     C                   ENDIF                                                  <-- 2 ---
544100931207     C                   ENDDO                                                  <-- 1 ---
544200940714     C                   ENDIF                                                  <-- 0 ---
544300931130     C*
544400931130     C                   ENDSR
544500931207     C********************************************************************
544600931207     C*   RO5SUB - AGGIUNGE UNA DATA ALLA SCHIERA COV
544700931207     C*            INPUT:  -WR5COV (PROGR.ORE DATA DA INSERIRE IN SCHIERA)
544800931207     C********************************************************************
544900931207     C     RO5SUB        BEGSR
545000931207     C*
545100931207     C* DEVE ESSERE COMPRESA TRA LA DATA INIZIO E FINE DATE IN INPUT ALLA RO5
545200931207     C     WR5COV        IFGT      WR5POI                                       --- 1 -->
545300931207     C     WR5COV        ANDLT     WR5POF
545400931207     C* CONTROLLA SE E' GIA' PRESENTE IN SCHIERA
545500931207     C                   MOVEL     'N'           WR5PRE            1
545600981102     C*---                 DO   30        I
545700981102     C                   DO        99            I
545800931207     C     WR5COV        IFEQ      COV(I)
545900931207     C                   MOVEL     'S'           WR5PRE
546000931207     C                   LEAVE
546100931207     C                   ENDIF
546200931207     C                   ENDDO
546300931207     C* NON DEVE ESSERE GIA' PRESENTE IN SCHIERA
546400931207     C     WR5PRE        IFEQ      'N'                                          --- 2 -->
546500931207     C                   ADD       1             CO                3 0
546600931207     C                   Z-ADD     WR5COV        COV(CO)
546700931207     C                   ENDIF                                                  <-- 2 ---
546800931207     C                   ENDIF                                                  <-- 1 ---
546900931207     C*
547000931207     C                   ENDSR
547100931201     C********************************************************************
547200060407     C*   RO6CDT - ROUTINE  PER DETERMINARE DATE PREVISTE (NO L.AVV.    E GIAC.)
547300931201     C*            INPUT:  -WR6POI (PROGR.ORE DATA DI PARTENZA)
547400931201     C*                    -WR6NOI (N.ORE DA UTILIZZ.PER RAGGIUNGERE DATA PREV.)
547500931201     C*           OUTPUT:  -WR6POO (PROGR.ORE DATA PREVISTA)
547600931201     C********************************************************************
547700931201     C     RO6CDT        BEGSR
547800931201     C*
547900931201     C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
548000931201     C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
548100931201     C*
548200931201     C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
548300931201     C                   Z-ADD     WR6POI        WR6POO            9 0
548400931201     C*
548500931201     C* CICLO FINO A CHE:
548600931201     C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
548700931201     C*   - RAGGIUNTA DATA LIMITE PER ERRORI
548800931201     C     WR6NOI        DOWGT     *ZEROS                                       --- 1 -->
548900931201     C     WR6POO        ANDLT     WPOLIM
549000931201     C*
549100931201     C                   Z-ADD     WR6POO        WPOROU            9 0
549200931201     C                   MOVEL     'N'           WSLITT            1
549300931201     C*
549400931201     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
549500931201     C*
549600931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
549700150605     c     wdee          andeq     ' '
549800931201     C                   EXSR      CHKCNT                                       *CONTR.CONTRASSEGNO
549900931201     C                   END
550000931201     C*
550100931201     C     WSLITT        IFEQ      'N'                                          *ORA VALIDA
550200150608     c     wdee          andeq     ' '
550300931201     C                   EXSR      CHKFED                                       *CONTR.F.DEPOSITO
550400931201     C                   END
550500931201     C*
550600931201     C     WSLITT        IFEQ      'N'
550700931201     C                   SUB       12            WR6NOI            5 0
550800931201     C                   END
550900931201     C                   ADD       12            WR6POO            9 0
551000931201     C*
551100931223     C                   ENDDO                                                  <-- 1 ---
551200931201     C*
551300931201     C* SE LA DATA PREVISTA COINCIDE CON T.CHIUSURA SLITTA AVANTI
551400931202     C     WR6POO        SUB       12            WR7POI
551500931201     C                   EXSR      RO7TCF                                       *CONTROLLA T.CHIUSUR
551600931207     C                   ADD       12            WR7POO
551700931202     C     WR7POO        IFGT      WR6POO
551800931201     C                   Z-ADD     WR7POO        WR6POO
551900931202     C                   END
552000931201     C*
552100931201     C                   ENDSR
552200931201     C********************************************************************
552300060407     C* RO7TCF - ROUTINE PER SLITTARE PROGR. ORE PER:                  O FESTIVO
552400060407     C*          1) TURNO DI CHIUSURA                                  O FESTIVO
552500060407     C*          2) EVENTI PARTICOLARI CHE GIUSTIFICANO                O FESTIVO
552600060407     C*            INPUT:  -WR7POI (PROGR.ORE)
552700931201     C*           OUTPUT:  -WR7POO (PROGR.ORE DOPO SLITTAMENTI PER T.CHIUSURA)
552800931201     C********************************************************************
552900931201     C     RO7TCF        BEGSR
553000060407     C                   Z-ADD     *ZEROS        WPOTI             9 0
553100060407     C                   Z-ADD     *ZEROS        WPOTF             9 0
553200060407     C                   MOVEL     'N'           WSLITT
553300931201     C*
553400931201     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
553500931201     C                   Z-ADD     WR7POI        WR7POO            9 0
553600931201     C* SE ESISTONO TURNI DI CHIUSURA, CONTROLLO CHE LA DATA PREVISTA NON
553700931201     C* COINCIDA CON UN TURNO DI CHIUSURA
553800060407    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
553900931201     C     D98GC2        ANDEQ     *BLANKS
554000060407   x1C                   ELSE                                                   <-- 1 -->
554100931214     C*
554200931202     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
554300931202     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
554400931214     C*
554500931201     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
554600931202     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
554700931223     C     WPOROU        ANDLT     WPOLIM
554800931214     C* MEMORIZZO INIZIO E FINE RANGE
554900931202     C     WPOTI         IFEQ      *ZEROS
555000931214     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
555100931202     C                   END
555200931214     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
555300931214     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
555400931214     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
555500931214     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
555600931214     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
555700931214     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
555800931214     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
555900931201     C                   MOVEL     'N'           WSLITT
556000931201     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
556100060407     c* Se non trovat, controllo se c'e' evento part che giustifica
556200060407     c                   if        wslitt='N'
556300060407     c                   exsr      datcepg
556400060407     c                   endif
556500060407     c
556600931201     C                   ENDDO                                                  <-- 2 ---
556700931214     C*
556800931201     C                   Z-ADD     WPOROU        WR7POO
556900931214     C*
557000931209     C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
557100931209     C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
557200931209     C                   ADD       1             TI                             *INDICE TURNI INTERN
557300931209     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
557400931209     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
557500931209     C                   ADD       1             TC                             *INDICE TURNI CLIENT
557600931209     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
557700931209     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
557800931209     C                   ELSE                                                   <-- 3 -->
557900931209     C     WIC           IFEQ      'I'
558000931201     C                   ADD       1             TI                             *INDICE TURNI INTERN
558100931201     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
558200931201     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
558300931201     C                   ELSE
558400931201     C                   ADD       1             TC                             *INDICE TURNI CLIENT
558500931201     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
558600931201     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
558700931201     C                   ENDIF
558800931209     C                   ENDIF                                                  <-- 3 ---
558900931209     C                   ENDIF                                                  <-- 2 ---
559000931214     C*
559100060407    1C                   ENDIF                                                  <-- 1 ---
559200060407     c
559300060407     c* Se non trovato turno di chiusura, verifico se c'e'
559400060407     c*  evento particolare che GIUSTIFICA
559500060407     c                   if        wpoti=0
559600060418     C                   EXSR      RO7CEPG
559700060407     c                   endif
559800931201     C*
559900931201     C                   ENDSR
560000060406     C********************************************************************
560100060407     C*   RO7CEPG -ROUTINE     PER SLITTARE PROGR. ORE SE C'E'EvPartic O FESTIVO
560200060406     c*                        che giustifica
560300060407     C*            INPUT:-WR7POI (PROGR.ORE)
560400060407     C*           OUTPUT:-WR7POO (PROGR.ORE DOPO SLITTAMENTI PER EvPartIUSURA)
560500060406     C********************************************************************
560600060407     C     RO7CEPG       BEGSR
560700060406     C*
560800060406     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
560900060407     C                   Z-ADD     WR7POI        WR7POO            9 0
561000060406     C* SE ESISTe almeno un evento particolare e se data prevista      ON
561100060406     C* coincide
561200060406    1C                   if        wevpag='S'                                   <-- 1 -->
561300060406     C*
561400060406     C                   Z-ADD     *ZEROS        WPOTI             9 0
561500060406     C                   Z-ADD     *ZEROS        WPOTF             9 0
561600060406     C                   MOVEL     'N'           WSLITT
561700060407     C                   Z-ADD     WR7POO        WPOROU                         *CAMPO LAVORO
561800060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
561900060406     C*
562000060406     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
562100060406    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
562200060406     C     WPOROU        ANDLT     WPOLIM
562300060406     C* MEMORIZZO INIZIO E FINE RANGE
562400060406    3C     WPOTI         IFEQ      *ZEROS
562500060406     C                   Z-ADD     WPOROU        WPOTI                          *INIZIO RANGE
562600060406    3C                   END
562700060406     C* SLITTA AVANTI E CONTROLLA SE VI SONO FESTIVI
562800060406     C                   Z-ADD     WPOROU        WR4POI                         *PROGR. ORE
562900060406     C                   Z-ADD     WNOADD        WR4NOI                         *ORE DA AGGIUNGERE
563000060406     C                   EXSR      RO4CDP                                       *SALTA FESTIVI
563100060406     C                   Z-ADD     WR4POO        WPOROU                         *PROGR. ORE RAGGIUNT
563200060406     C     WPOROU        SUB       12            WPOTF                          *FINE RANGE
563300060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN altro EvPARt
563400060406     C                   MOVEL     'N'           WSLITT
563500060406     C                   EXSR      DATCEPG                                      *CONTROLLA T.CHIUSUR
563600060406     C* Se non trovato,
563700060406     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
563800060406    3c                   if        wslitt='N'
563900060406     c                   exsr      DATTUC
564000060406    3C                   endif                                                  *PROGR. ORE RAGGIUNT
564100060406    2C                   ENDDO                                                  <-- 2 ---
564200060406     C*
564300060407     C                   Z-ADD     WPOROU        WR7POO
564400060406     C*
564500060406    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
564600060406    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
564700060406     C                   ADD       1             PGI                            *INDICE TURNI INTERN
564800060406     C                   Z-ADD     WPOTI         pGII(pgi)                      *INIZIO
564900060406     C                   Z-ADD     WPOTF         pGIF(pgi)                      *FINE
565000060406     C                   ADD       1             pGC                            *INDICE TURNI CLIENT
565100060406     C                   Z-ADD     WPOTI         pGCI(pgc)                      *INIZIO
565200060406     C                   Z-ADD     WPOTF         pGCF(pgc)                      *FINE
565300060406   x3C                   ELSE                                                   <-- 3 -->
565400060406    4C     WIC           IFEQ      'I'
565500060406     C                   ADD       1             pgi                            *INDICE TURNI INTERN
565600060406     C                   Z-ADD     WPOTI         pgII(pgi)                      *INIZIO
565700060406     C                   Z-ADD     WPOTF         pgIF(pgi)                      *FINE
565800060406   x4C                   ELSE
565900060406     C                   ADD       1             pgc                            *INDICE TURNI CLIENT
566000060406     C                   Z-ADD     WPOTI         pgCI(pgc)                      *INIZIO
566100060406     C                   Z-ADD     WPOTF         pgCF(pgc)                      *FINE
566200060406    4C                   ENDIF
566300060406    3C                   ENDIF                                                  <-- 3 ---
566400060406    2C                   ENDIF                                                  <-- 2 ---
566500060406     C*
566600060406    1C                   ENDIF                                                  <-- 1 ---
566700060406     C*
566800060406     C                   ENDSR
566900931207     C********************************************************************
567000931207     C*   RO8FES - ROUTINE (8) PER SLITTARE INDIETRO PROGR. ORE SE C'E' FESTIVO
567100931207     C*            INPUT:  -WR8POI (PROGR.ORE)
567200931207     C*           OUTPUT:  -WR8POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
567300931207     C********************************************************************
567400931207     C     RO8FES        BEGSR
567500130411     C                   Z-ADD     *ZEROS        WPOTI             9 0
567600130411     C                   Z-ADD     *ZEROS        WPOTF             9 0
567700931207     C*
567800931207     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
567900130411     C     WR8POI        SUB       12            WR8POO            9 0
568000931207     C                   Z-ADD     WR8POO        WPOROU                         *CAMPO LAVORO
568100931207     C                   MOVEL     'N'           WSLITT
568200130411     c
568300130411     c* Non salto i festivi ???
568400130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
568500130411     c
568600931207     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
568700130411     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
568800130411     C     WPOROU        ANDGT     WPODSP
568900130411     C                   SUB       12            WPOROU
569000130411     C                   MOVEL     'N'           WSLITT
569100130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
569200130411     C                   ENDDO                                                  <-- 2 ---
569300130411     c
569400130411    1C     D98GC1        IFEQ      *BLANKS                                      --- 1 -->
569500130411     C     D98GC2        ANDEQ     *BLANKS
569600130411   x1C                   ELSE                                                   <-- 1 -->
569700130411     c* eseguo controllo per i turni di chiusura
569800130411     C                   EXSR      DATTUC                                       *CONTROLLA T.CHIUSUR
569900130411     C*
570000130411     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN T.CHIUS.
570100130411    2C     WSLITT        DOWEQ     'S'                                          --- 2 -->
570200130411     C     WPOROU        ANDgT     WPOdsp
570300130411     C* MEMORIZZO INIZIO E FINE RANGE
570400130411     C     WPOTf         IFEQ      *ZEROS
570500130411     C                   z-add     wporou        WPOTf
570600130411     C                   END
570700140826     C     WPOROU        SUB       wnoadd        WPOROU
570800130411     c
570900130411     C                   MOVEL     'N'           WSLITT
571000130411     c                   exsr      dattuc
571100130411     c                   if        wslitt='N'
571200130411     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
571300140826     c* Se slitta per festivo --> tolgo sempre 12 ore
571400140826     c                   z-add     12            WNOADD
571500130411     c                   endif
571600130411     C* SLITTA
571700140826     C     wporou        add       12            WPOTi
571800130411    2C                   ENDDO                                                  <-- 2 ---
571900130411     c
572000130411     C* CONTROLLO SE AL PROGRESSIVO ORE RAGGIUNTO C'E' UN T.CHIUSURA
572100130411     c
572200130411    2C     WPOTI         IFNE      *ZEROS                                       --- 2 -->
572300130411    3C     WIC           IFEQ      'X'                                          --- 3 --> CLI+INT
572400130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
572500130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
572600130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
572700130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
572800130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
572900130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
573000130411   x3C                   ELSE                                                   <-- 3 -->
573100130411    4C     WIC           IFEQ      'I'
573200130411     C                   ADD       1             TI                             *INDICE TURNI INTERN
573300130411     C                   Z-ADD     WPOTI         TII(TI)                        *INIZIO
573400130411     C                   Z-ADD     WPOTF         TIF(TI)                        *FINE
573500130411   x4C                   ELSE
573600130411     C                   ADD       1             TC                             *INDICE TURNI CLIENT
573700130411     C                   Z-ADD     WPOTI         TCI(TC)                        *INIZIO
573800130411     C                   Z-ADD     WPOTF         TCF(TC)                        *FINE
573900130411    4C                   ENDIF
574000130411    3C                   ENDIF                                                  <-- 3 ---
574100130411    2C                   ENDIF                                                  <-- 3 ---
574200130411     C*
574300130411    1C                   ENDIF                                                  <-- 1 ---
574400130411
574500130411     C**   WPOROU        ADD       12            WR8POO
574600130411     c* dopo il controllo dei gg chiusura lascio la data che c'e'
574700130411     c                   z-add     wr8poi        wr8poo
574800931207     C*
574900931207     C                   ENDSR
575000931210     C********************************************************************
575100931210     C*   RO9COV - ROUTINE (9) PER CALCOLO ORE VALIDE TRA DUE DATE
575200940324     C*            UTILIZZATA PAR CALCOLO CONSEGNABILITA'
575300931210     C*            INPUT:  -WR9POI (PROGR.ORE DATA INIZIO)
575400931210     C*                    -WR9POF (PROGR.ORE DATA FINE)
575500931210     C*           OUTPUT:  -WR9NOV (N.ORE VALIDE TRA LE DUE DATE)
575600931210     C********************************************************************
575700931210     C     RO9COV        BEGSR
575800931210     C*
575900931210     C* AZZERO IL NUMERO ORE VALIDE TRA DUE DATE
576000931210     C                   Z-ADD     *ZEROS        WR9NOV
576100931210     C* IMPOSTO IL CAMPO DI LAVORO CON LE ORE DELLA DATA DI INIZIO
576200931210     C                   Z-ADD     WR9POI        WR9POW
576300931210     C* CICLO FINO A CHE:
576400931210     C*   - DATA INIZIO SUPERATO DATA FINE
576500931223     C*   - RAGGIUNTO LIMITE MASSIMO
576600931210     C***********WR9POW    DOWLTWR9POF                     --- 1 -->
576700931210     C     WR9POW        DOWLE     WR9POF                                       --- 1 -->
576800931223     C     WR9POW        ANDLT     WPOLIM
576900931210     C*
577000931210     C                   Z-ADD     WR9POW        WPOROU
577100931210     C                   MOVEL     'N'           WSLITT            1
577200940324     C* FESTIVI
577300931210     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
577400940324     C* GIACENZA
577500931214     C     WSLITT        IFEQ      'N'
577600941229£$   C     WPOAGC        IFNE      *ZEROS
577700940324£$   C     WPOCGC        ORNE      *ZEROS
577800941229£$   C     WPOROU        IFGE      WPOAGC                                       *APERTURA GIACENZA
577900940324£$   C     WPOROU        ANDLE     WPOCGC                                       *CHIUSURA GIACENZA
578000940324£$   C                   MOVEL     'S'           WSLITT
578100940324£$   C                   END
578200940324£$   C                   END
578300940324£$   C                   END
578400960129     C* EVENTI GIACENZA
578500960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
578600960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
578700960129     C                   Z-ADD     *ZEROS        G                 3 0
578800960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
578900960129     C     G             ANDLT     IG
579000960129     C                   ADD       1             G
579100960209     C     WPOROU        IFGE      GCI(G)                                       --- 3 -->
579200960209     C     WPOROU        ANDLE     GCF(G)
579300960129     C                   MOVEL     'S'           WSLITT
579400960129     C                   ENDIF                                                  <-- 3 ---
579500960129     C                   ENDDO                                                  <-- 2 ---
579600960129     C                   ENDIF                                                  <-- 1 ---
579700960129     C                   ENDIF                                                  <-- 0 ---
579800940324     C* LASCIATO AVVISO
579900931214     C     WSLITT        IFEQ      'N'
580000941228£$   C     WEELAV        IFEQ      'S'                                          --- 1 -->
580100940324£$   C                   Z-ADD     *ZEROS        L                 3 0
580200940324£$   C     WSLITT        DOWEQ     'N'                                          --- 2 -->
580300940324£$   C     L             ANDLT     IL
580400940324£$   C                   ADD       1             L
580500941229£$   C     WPOROU        IFGE      LCI(L)                                       --- 3 -->
580600941229£$   C     WPOROU        ANDLE     LCF(L)
580700050520     c     LCon(L)       andeq     'S'
580800940324£$   C                   MOVEL     'S'           WSLITT
580900940324£$   C                   END                                                    <-- 3 ---
581000940324£$   C                   ENDDO                                                  <-- 2 ---
581100940324£$   C                   END                                                    <-- 1 ---
581200931210     C                   END
581300940324     C* TURNO CHIUSURA
581400140826     C* NELLA CONSEGNABILITÀ le mezze giornate sono ignorate per cui non mi preoccupo
581500140826     c*  di sistemare le ore da sommare 12 o 24...
581600931214     C     WSLITT        IFEQ      'N'
581700931210     C                   EXSR      DATTUC                                       *CONTR.TURNO CHIUSUR
581800931210     C                   END
581900940324     C* DATA CONSEGNA RICHIESTA
582000050907     c* visto che ho sommato 1 ora all'inizio periodo consegnabilità
582100050907     c*  sommo 1 anche qui altrimenti non mi da la spedizione consegnabile
582200050907     c
582300931214     C     WSLITT        IFEQ      'N'
582400050907     c                   add       1             wporou
582500940324£$   C     WPOCRI        IFNE      *ZEROS
582600940324£$   C     WPOROU        IFLT      WPOCRC
582700940324£$   C                   MOVEL     'S'           WSLITT
582800940324£$   C                   END
582900940324£4   C                   END
583000050907     c                   sub       1             wporou
583100931210     C                   END
583200931210     C*
583300931210     C* SE NON DEVO SLITTARE PER QUALCHE MOTIVO SIGNIFICA CHE ERA TEMPO UTILE
583400931210     C* PER LA CONSEGNA QUINDI AUMENTO TEMPO VALIDO TRA LE DUA DATE
583500931210     C     WSLITT        IFEQ      'N'
583600931210     C                   ADD       12            WR9NOV            5 0
583700931210     C                   END
583800931210     C                   ADD       12            WR9POW            9 0
583900931210     C*
584000931223     C                   ENDDO                                                  <-- 1 ---
584100931210     C*
584200931210     C                   ENDSR
584300940628%%%  C********************************************************************
584400940628%%%  C*   RO0GES - ROUTINE (0) PER DETERMINARE DATE PREVISTE ALL'INDIETRO
584500940628%%%  C*            INPUT:  -WR0POI (PROGR.ORE DATA DI PARTENZA)
584600940628%%%  C*                    -WR0NOI (N.ORE DA TOGLIERE PER RAGG. DATA PREV.)
584700940628%%%  C*           OUTPUT:  -WR0POO (PROGR.ORE DATA PREVISTA)
584800940628%%%  C********************************************************************
584900940628%%%  C     RO0GES        BEGSR
585000940628%%%  C*
585100940628%%%  C* PER CALCOLO DATA PREVISTA USO TEMPI CONSEGNA
585200940628%%%  C                   MOVEL     'C'           WTC                            *TRAZIONE/CONSEGNA
585300940628%%%  C                   MOVEL     'I'           WIC               1            *INTERNO/CLIENTE
585400940628%%%  C*
585500940628%%%  C* PARTO CON I TENTATIVI DALLA DATA DI RIFERIMENTO
585600940628%%%  C     WR0POI        SUB       12            WR0POO            9 0
585700940628%%%  C*
585800940628%%%  C* CICLO FINO A CHE:
585900940628%%%  C*   - ESAURITE LE ORE A DISPOSIZIONE (TROVATA DATA PREVISTA)
586000940628%%%  C*   - RAGGIUNTA DATA SPEDIZIONE
586100940628%%%  C     WR0NOI        DOWGT     *ZEROS                                       --- 1 -->
586200050922%%%  C     WR0POO        ANDGe     WPODSP
586300940628%%%  C*
586400940628%%%  C                   Z-ADD     WR0POO        WPOROU            9 0
586500940628%%%  C                   MOVEL     'N'           WSLITT            1
586600940628%%%  C*
586700940628%%%  C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
586800940628%%%  C*
586900940628%%%  C     WSLITT        IFEQ      'N'
587000940628%%%  C                   SUB       12            WR0NOI            5 0
587100940628%%%  C                   END
587200940628%%%  C                   SUB       12            WR0POO            9 0
587300940628%%%  C*
587400940628%%%  C                   ENDDO                                                  <-- 1 ---
587500940628%%%  C*
587600940628%%%  C                   ADD       12            WR0POO            9 0
587700940628%%%  C*
587800940628%%%  C                   ENDSR
587900960911     C********************************************************************
588000960911     C*   R10FES - ROUTINE (10) PER SLITTARE AVANTI PROGR. ORE SE C'E' FESTIVO
588100960911     C*            INPUT:  -W10POI (PROGR.ORE)
588200960911     C*           OUTPUT:  -W10POO (PROGR.ORE DOPO SLITTAMENTI PER FESTIVI)
588300960911     C********************************************************************
588400960911     C     R10FES        BEGSR
588500960911     C*
588600960911     C* IMPOSTO IL PROGRESSIVO ORE IN OUTPUT COME IL PROGRESSIVO ORE IN INPUT
588700960911     C                   Z-ADD     W10POI        W10POO            9 0
588800960911     C                   Z-ADD     W10POO        WPOROU                         *CAMPO LAVORO
588900960911     C                   MOVEL     'N'           WSLITT
589000960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
589100960911     C* RIMANGO NEL LOOP FINO A QUANDO LA DATA TROVATA NON E' IN UN FESTIVO
589200960911     C     WSLITT        DOWEQ     'S'                                          --- 2 -->
589300960911     C                   ADD       12            WPOROU
589400960911     C                   MOVEL     'N'           WSLITT
589500960911     C                   EXSR      CHKFES                                       *CONTROLLA FESTIVI
589600960911     C                   ENDDO                                                  <-- 2 ---
589700960911     C                   Z-ADD     WPOROU        W10POO
589800960911     C*
589900960911     C                   ENDSR
590000930325     C********************************************************************
590100930325     C*   CHKGIA - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE GIACENZA
590200930325     C********************************************************************
590300930325     C     CHKGIA        BEGSR
590400930406     C*
590500960129     C* APERTURA/CHIUSURA GIACENZA
590600930326     C     WPODAG        IFNE      *ZEROS
590700930406     C     WPODCG        ORNE      *ZEROS
590800930407     C     WPOROU        IFGE      WPODAG
590900930407     C     WPOROU        ANDLE     WPODCG
591000930325     C                   MOVEL     'S'           WSLITT
591100930414     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
591200930325     C                   END
591300930326     C                   END
591400960129     C*
591500960129     C* EVENTI GIACENZA
591600960129     C     WSLITT        IFEQ      'N'                                          --- 0 -->
591700960129     C     WEEGIA        IFEQ      'S'                                          --- 1 -->
591800960129     C                   Z-ADD     *ZEROS        G                 3 0
591900960129     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
592000960129     C     G             ANDLT     IG
592100960129     C                   ADD       1             G
592200960129     C     WPOROU        IFGE      GII(G)                                       --- 3 -->
592300960129     C     WPOROU        ANDLE     GIF(G)
592400091009     c
592500091009     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
592600091009     c*  l'orario dell'evento
592700091009     c                   select
592800091014     c                   when      WIC='I'  and d98tci=10 and salvaEveG<>gip(G)
592900091009     c* Se orario entro le 10.30  va bene
593000091014     c*  solo la prima volta per evento cioè quando controllo
593100091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
593200091014     c*  e non devo dare errore
593300091014     C                   MOVEL     'OSC  '       WR3TAB
593400091014     C                   eval      WR3HHI=evo(gip(G))
593500091014     C                   EXSR      RO3ORE
593600091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
593700091014     c*  in ritardo come per "E")
593800091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
593900091014     c                              wr3matpom='P'
594000091009     C                   ADD       12            W98HG                          *N.ORE SLITT.X L.AVV
594100091009     C                   MOVEL     'S'           WSLITT
594200091009     c                   endif
594300091014     c                   eval      SalvaEveG=gip(G)
594400091009     c
594500091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveG<>gip(G)
594600091009     c* Se orario entro le 10.30  va bene
594700091014     c*  solo la prima volta per evento cioè quando controllo
594800091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
594900091014     c*  e non devo dare errore
595000091014     C                   MOVEL     'OSC  '       WR3TAB
595100091014     C                   eval      WR3HHI=evo(gip(G))
595200091014     C                   EXSR      RO3ORE
595300091014     c* eeseguo il controllo solo per evento ddel mattino (al pomerigigo è già
595400091014     c*  in ritardo come per "E")
595500091014     c                   if        (wr3matpom='M' and evo(gip(G))<=1030) or
595600091014     c                              wr3matpom='P'
595700091009     C                   ADD       12            W98Hg                          *N.ORE SLITT.X L.AVV
595800091009     C                   MOVEL     'S'           WSLITT
595900091009     c                   endif
596000091014     c                   eval      SalvaEveG=gip(G)
596100091009     c
596200091009     c                   other
596300960129     C                   MOVEL     'S'           WSLITT
596400960129     C                   ADD       12            W98HG                          *N.ORE SLITT.X GIAC.
596500091009     c                   endsl
596600091009
596700960129     C                   ENDIF                                                  <-- 3 ---
596800960129     C                   ENDDO                                                  <-- 2 ---
596900960129     C                   ENDIF                                                  <-- 1 ---
597000960129     C                   ENDIF                                                  <-- 0 ---
597100930406     C*
597200930325     C                   ENDSR
597300060420     C********************************************************************
597400930421     C*   CHKLAV - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE LASCIATO AVV.
597500930325     C********************************************************************
597600930325     C     CHKLAV        BEGSR
597700930325     C*
597800941228     C     WEELAV        IFEQ      'S'                                          --- 1 -->
597900930421     C                   Z-ADD     *ZEROS        L                 3 0
598000930421     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
598100930421     C     L             ANDLT     IL
598200930421     C                   ADD       1             L
598300930421     C     WPOROU        IFGE      LAI(L)                                       --- 3 -->
598400930421     C     WPOROU        ANDLE     LAF(L)
598500090928     c
598600090928     c* Se consegna ore 10 30 e si tratta del primo evento, controllo
598700091009     c*  l'orario dell'evento
598800091009     c                   select
598900091014     c                   when      WIC='I'  and d98tci=10 and salvaEveL<>lap(L)
599000091009     c* Se orario entro le 10.30  va bene
599100091014     c*  solo la prima volta per evento cioè quando controllo
599200091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
599300091014     c*  e non devo dare errore
599400091014     C                   MOVEL     'OSC  '       WR3TAB
599500091014     C                   eval      WR3HHI=evo(lap(L))
599600091014     C                   EXSR      RO3ORE
599700091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
599800091014     c*  in ritardo come per "E")
599900091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
600000091014     c                              wr3matpom='P'
600100091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
600200091009     C                   MOVEL     'S'           WSLITT
600300091009     c                   endif
600400091014     c                   eval      SalvaEveL=lap(L)
600500091009     c
600600091014     c                   when      WIC='C'  and d98tcc=10 and salvaEveL<>lap(L)
600700091009     c* Se orario entro le 10.30  va bene
600800091014     c*  solo la prima volta per evento cioè quando controllo
600900091014     c*  le prime 12 ore perchè al secondo giro è per il completamento delle 24 ore
601000091014     c*  e non devo dare errore
601100091014     C                   MOVEL     'OSC  '       WR3TAB
601200091014     C                   eval      WR3HHI=evo(lap(L))
601300091014     C                   EXSR      RO3ORE
601400091015     c* Eseguo il controllo solo per evento del mattino (al pomeriggio è già
601500091014     c*  in ritardo come per "E")
601600091014     c                   if        (wr3matpom='M' and evo(lap(L))<=1030) or
601700091014     c                              wr3matpom='P'
601800091009     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
601900091009     C                   MOVEL     'S'           WSLITT
602000091009     c                   endif
602100091014     c                   eval      SalvaEveL=lap(L)
602200090928     c
602300091009     c                   other
602400930414     C                   ADD       12            W98HL                          *N.ORE SLITT.X L.AVV
602500091009     C                   MOVEL     'S'           WSLITT
602600091009     c                   endsl
602700090928     c
602800930421     C                   END                                                    <-- 3 ---
602900931223     C                   ENDDO                                                  <-- 2 ---
603000930421     C                   END                                                    <-- 1 ---
603100930325     C*
603200930325     C                   ENDSR
603300060420     C********************************************************************
603400060420     C*   CONTROLLA SE LA DATA E' COMPRESA NEI RANGE eventi particolari
603500060420     c*                   che Allungano
603600060420     C********************************************************************
603700060420     C     CHKCEPA       BEGSR
603800060420     C*
603900060420     C     WEvpaA        IFEQ      'S'                                          --- 1 -->
604000060420     C                   Z-ADD     *ZEROS        p                 3 0
604100060420     C     WSLITT        DOWEQ     'N'                                          --- 2 -->
604200060420     C     p             ANDLT     IpA
604300060420     C                   ADD       1             P
604400060420     C     WPOROU        IFGE      paii(P)                                      --- 3 -->
604500060420     C     WPOROU        ANDLE     paif(P)
604600060420     C                   MOVEL     'S'           WSLITT
604700060420     C                   ADD       12            W98EA
604800060420     C                   END                                                    <-- 3 ---
604900060420     C                   ENDDO                                                  <-- 2 ---
605000060420     C                   END                                                    <-- 1 ---
605100060420     C*
605200060420     C                   ENDSR
605300930414     C********************************************************************
605400930414     C*   CHKCNT - CONTROLLA IL CONTRASSEGNO
605500930414     C********************************************************************
605600930414     C     CHKCNT        BEGSR
605700930511     C*
605800930511     C* SE VI SONO ORE PER CONTRASSEGNO E DATA RIENTRA NEL RANGE CONTRASS.-> SLI
605900930611     C     WIC           IFEQ      'I'                                          *INTERNO
606000930611     C     WPOAII        IFEQ      *ZEROS                                       *INIZIO
606100930611     C     WPOAFI        ANDEQ     *ZEROS                                       *FINE
606200930611     C                   ELSE
606300930611     C     WPOROU        IFGE      WPOAII
606400930611     C     WPOROU        ANDLE     WPOAFI
606500930611     C                   MOVEL     'S'           WSLITT
606600930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
606700930611     C                   END
606800930611     C                   END
606900930611     C                   ELSE                                                   *CLIENTE
607000930611     C     WPOAIC        IFEQ      *ZEROS
607100930611     C     WPOAFC        ANDEQ     *ZEROS
607200930611     C                   ELSE
607300930611     C     WPOROU        IFGE      WPOAIC
607400930611     C     WPOROU        ANDLE     WPOAFC
607500930611     C                   MOVEL     'S'           WSLITT
607600930611     C                   ADD       12            W98HO                          *N.ORE SLITT.X CONTR
607700930611     C                   END
607800930611     C                   END
607900930611     C                   END
608000930414     C*
608100930414     C                   ENDSR
608200930511     C********************************************************************
608300930511     C*   CHKFED - CONTROLLA IL FERMO DEPOSITO
608400930511     C********************************************************************
608500930511     C     CHKFED        BEGSR
608600930511     C*
608700930511     C* SE VI SONO ORE PER F.DEPOSITO E DATA RIENTRA NEL RANGE F.DEP. -> SLITTA
608800930611     C     WIC           IFEQ      'I'                                          *INTERNO
608900930611     C     WPOFII        IFEQ      *ZEROS                                       *INIZIO
609000930611     C     WPOFFI        ANDEQ     *ZEROS                                       *FINE
609100930611     C                   ELSE
609200930611     C     WPOROU        IFGE      WPOFII
609300930611     C     WPOROU        ANDLE     WPOFFI
609400930611     C                   MOVEL     'S'           WSLITT
609500930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
609600930611     C                   END
609700930611     C                   END
609800930611     C                   ELSE                                                   *CLIENTE
609900930611     C     WPOFIC        IFEQ      *ZEROS
610000930611     C     WPOFFC        ANDEQ     *ZEROS
610100930611     C                   ELSE
610200930611     C     WPOROU        IFGE      WPOFIC
610300930611     C     WPOROU        ANDLE     WPOFFC
610400930611     C                   MOVEL     'S'           WSLITT
610500930611     C                   ADD       12            W98HD                          *N.ORE SLITT.X F.DEP
610600930611     C                   END
610700930611     C                   END
610800930611     C                   END
610900930511     C*
611000930511     C                   ENDSR
611100930325     C********************************************************************
611200930325     C*   CHKFES - CONTROLLA SE LA DATA E' UN FESTIVO
611300930325     C********************************************************************
611400930325     C     CHKFES        BEGSR
611500060526     c* se già passata la data, non controllo wporou
611600060526     c                   if        wpodata>0
611700060526     c                   z-add     wpodata       w12dat
611800060526     c                   clear                   w12res
611900060526     c                   else
612000931130     C*
612100931130     C* TRASFORMA IL  PROGR.ORE IN DATA
612200931130     C                   Z-ADD     WPOROU        WORE
612300931130     C                   EXSR      OREDAT
612400941227     C                   Z-ADD     WDAT          W12DAT            8 0
612500931130     C                   Z-ADD     WRES          W12RES            3 0
612600060526     c                   endif
612700930325     C*
612800941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E CONTROLLA SE IL GIORNO
612900930325     C* E' FESTIVO
613000090108     C**   WTC           IFEQ      'P'                                          *PARTENZA
613100090108     C**                 Z-ADD     D98LNP        CLNTFP
613200090108     C**                 Z-ADD     *ZEROS        CLNTFA
613300090108     C**                 ENDIF
613400090108     c** Per il momento anche l'affidab partenza tiene conto del calendario
613500090108     c**   ter-ter per consdierare eventuali festivi. In questo modo
613600090108     c**   è possibile rendere festivo in terminal per un altro terminal
613700090108     c*    e basta (ad esempio per neve non sono partiti i traini x la lig)
613800930405     C     WTC           IFEQ      'T'                                          *TRAZIONE
613900090108     C     WTC           oreq      'P'                                          *TRAZIONE
614000941227     C                   Z-ADD     D98TFP        CLNTFP
614100941227     C                   Z-ADD     D98TFA        CLNTFA
614200941228     C                   ENDIF
614300941228     C     WTC           IFEQ      'C'                                          *CONSEGNA
614400941227     C                   Z-ADD     *ZEROS        CLNTFP
614500941227     C                   Z-ADD     D98LNA        CLNTFA
614600941228     C                   ENDIF
614700941227     C                   Z-ADD     W12DAT        DSDAT8
614800941227     C                   Z-ADD     DSAAS         CLNANN
614900941227     C                   Z-ADD     DSMES         CLNMES
615000930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
615100930517     C     D98ERR        IFEQ      '0'                                          --- 1 -->
615200930421     C                   Z-ADD     DSGIO         G
615300930415     C     W12RES        IFEQ      *ZEROS
615400941227     C                   MOVEL     CLNMAT        GFEDS
615500930415     C                   ELSE
615600941227     C                   MOVEL     CLNPOM        GFEDS
615700930415     C                   END
615800930421     C     GFE(G)        IFNE      *BLANKS                                      --- 2 -->
615900930405     C                   MOVEL     *BLANKS       WC1Y
616000930421     C                   MOVEL     GFE(G)        WC1Y              8
616100930421     C                   Z-ADD     1             I
616200930421     C     WC1Y          LOOKUP    C1Y(I)                                 99
616300930405     C     *IN99         IFEQ      '1'                                          --- 3 -->
616400930405     C     WIC           IFEQ      'I'
616500930421     C     I1Y(I)        ANDEQ     'S'
616600930405     C                   MOVEL     'S'           WSLITT
616700930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
616800930405     C                   END
616900930405     C     WIC           IFEQ      'C'
617000930421     C     E1Y(I)        ANDEQ     'S'
617100930405     C                   MOVEL     'S'           WSLITT
617200930414     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
617300930405     C                   END
617400970204     C     WIC           IFEQ      'X'
617500970204     C     E1Y(I)        ANDEQ     'S'
617600970204     C                   MOVEL     'S'           WSLITT
617700970204     C                   ADD       12            W98HF                          *N.ORE SLITT.X FESTI
617800970204     C                   END
617900930405     C                   ELSE                                                   <-- 3 -->
618000930510     C                   MOVEL     '1'           D98ERR                         *SIMBOLO CALEND.BART
618100930510     C                   MOVEL     MSG(4)        D98MSG                         *NON TROVATO
618200930405     C                   END                                                    <-- 3 ---
618300930405     C                   END                                                    <-- 2 ---
618400930405     C                   END                                                    <-- 1 ---
618500930325     C*
618600060526     c                   clear                   wpodata
618700930325     C                   ENDSR
618800931201     C********************************************************************
618900931201     C*   CHKTUC - CONTROLLA SE LA DATA E' COMPRESA NEI RANGE TURNO CHIUSURA
619000931201     C********************************************************************
619100931201     C     CHKTUC        BEGSR
619200931201     C*
619300931201     C     WIC           IFEQ      'I'                                          --- 1 -->
619400931201     C                   DO        TI            T                 3 0          --- 2 -->
619500931201     C     WPOROU        IFGE      TII(T)                                       --- 3 -->
619600931201     C     WPOROU        ANDLE     TIF(T)
619700931201     C                   MOVEL     'S'           WSLITT
619800931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
619900931201     C                   LEAVE                                                  *VA A FINE DO
620000931201     C                   ENDIF                                                  <-- 3 ---
620100931201     C                   ENDDO                                                  <-- 2 ---
620200931201     C                   ELSE                                                   <-- 1 -->
620300931201     C                   DO        TC            T                 3 0          --- 2 -->
620400931201     C     WPOROU        IFGE      TCI(T)                                       --- 3 -->
620500931201     C     WPOROU        ANDLE     TCF(T)
620600931201     C                   MOVEL     'S'           WSLITT
620700931202     C                   ADD       12            W98HH                          *N.ORE SLITT.X T.CH.
620800931201     C                   LEAVE                                                  *VA A FINE DO
620900931201     C                   ENDIF                                                  <-- 3 ---
621000931201     C                   ENDDO                                                  <-- 2 ---
621100931201     C                   ENDIF                                                  <-- 1 ---
621200931201     C*
621300931201     C                   ENDSR
621400060418     C********************************************************************
621500060418     C*   CHKcepG- CONTROLLA SE LA DATA E' COMPRESA NEI RANGE          USURA
621600060418     c*          - eventi particolari che giustificano
621700060418     C********************************************************************
621800060418     C     CHKCEPG       BEGSR
621900060418     C*
622000060418     C     WIC           IFEQ      'I'                                          --- 1 -->
622100060420     C                   DO        PGI           p                 3 0          --- 2 -->
622200060420     C     WPOROU        IFGE      PGII(p)                                      --- 3 -->
622300060420     C     WPOROU        ANDLE     PGIF(p)
622400060418     C                   MOVEL     'S'           WSLITT
622500060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
622600060418     C                   LEAVE                                                  *VA A FINE DO
622700060418     C                   ENDIF                                                  <-- 3 ---
622800060418     C                   ENDDO                                                  <-- 2 ---
622900060418     C                   ELSE                                                   <-- 1 -->
623000060420     C                   DO        PGC           p                 3 0          --- 2 -->
623100060420     C     WPOROU        IFGE      PGCI(p)                                      --- 3 -->
623200060420     C     WPOROU        ANDLE     PGCF(p)
623300060418     C                   MOVEL     'S'           WSLITT
623400060418     C                   ADD       12            W98EG                          *N.ORE SLITT.X T.CH.
623500060418     C                   LEAVE                                                  *VA A FINE DO
623600060418     C                   ENDIF                                                  <-- 3 ---
623700060418     C                   ENDDO                                                  <-- 2 ---
623800060418     C                   ENDIF                                                  <-- 1 ---
623900060418     C*
624000060418     C                   ENDSR
624100930512     C********************************************************************
624200931130     C*   DATTUC - CONTROLLA SE LA DATA E' IN UN TURNO DI CHIUSURA
624300930512     C********************************************************************
624400931130     C     DATTUC        BEGSR
624500931214     C*
624600931214     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
624700931214     C                   Z-ADD     12            WNOADD            9 0
624800930325     C*
624900931125     C     D98GC1        IFNE      *BLANKS                                      --- 1 -->
625000931125     C     D98GC2        ORNE      *BLANKS
625100931130     C*
625200931213     C                   Z-ADD     WPOROU        WORE
625300931130     C                   EXSR      OREDAT                                       *ORE->DATA
625400931130     C                   Z-ADD     WDAT          W12DAT
625500931130     C                   Z-ADD     WRES          W12RES
625600930405     C*
625700941227     C* LEGGE SUL FILE AZCLN00F IL RECORD DEL MESE E RECUPERA IL GIORNO
625800930405     C* DELLA SETTIMANA
625900941227     C                   Z-ADD     W12DAT        DSDAT8
626000941227     C                   Z-ADD     DSAAS         CLNANN
626100941227     C                   Z-ADD     DSMES         CLNMES
626200941227     C                   Z-ADD     *ZEROS        CLNTFP
626300941227     C                   Z-ADD     *ZEROS        CLNTFA
626400930517     C                   EXSR      CERCAL                                       *CERCA REC.CALENDARI
626500930517     C     D98ERR        IFEQ      '0'                                          --- 2 -->
626600941227     C                   MOVEL     CLNMAT        GSEDS
626700960129     C                   Z-ADD     DSGIO         GI                6 0
626800960129     C                   MOVEL     GSE(GI)       WGSE              1
626900140911     c
627000140911     c* se ho già dato 24 ore per le chiusue " P" o " M" al secondo giro
627100140911     c*  ignoro tale turno ma poi devo pulire il flag per i successivi richiami
627200140911     c                   if        wtuctut='S'
627300140911     c                   eval      wtuctut='P'
627400140911     c                   endif
627500930326     C*
627600931214     C* CONTROLLO IL TURNO SU TUTTA LA SCHIERA DEI TURNI DI CHIUSURA
627700931213     C                   DO        4             IT                3 0          --- 3 -->
627800931213     C     TUC(IT)       IFNE      *BLANKS                                      --- 4 -->
627900931213     C                   MOVEL     TUC(IT)       W93GSE            1
628000931213     C                   MOVE      TUC(IT)       W93GMP            1
628100930512     C* SE E' LO STESSO GIORNO DELLA SETTIMANA (ES. 4 E 4) OPPURE
628200930512     C* SE IL GIORNO E' UGUALE A SPAZIO (TUTTI I GIORNI DELLA SETT.) CONTROLLO
628300931213     C     WGSE          IFEQ      W93GSE                                       --- 5 -->
628400930512     C     W93GSE        OREQ      ' '
628500140911     c     WTUCTUT       andeq     ' '
628600140911     c
628700930325     C* SE E' CHIUSO TUTTO IL GIORNO FACCIO SLITTARE
628800931213     C     W93GMP        IFEQ      'G'                                          --- 6 -->
628900930401     C     W93GMP        OREQ      ' '
629000930325     C                   MOVEL     'S'           WSLITT
629100931214     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
629200931213     C                   ELSE                                                   <-- 6 -->
629300001128     C* PER IL CALCOLO DELLA CONSEGNABILITA' NON DEVE SLITTARE SE MATTINA / POMERIGGIO
629400001128     C* CHIUSO PERCHE' IL LANCIO DEL TABULATO CONSEGNABILITA' AVVIENE LA MATTINA E PER
629500001128     C* LE BOLLE CON "M" DEVE DARE LA CONSEGNABILITA'
629600050908     C***  FLCONS        IFNE      'S'                                          --- 7 -->
629700050908     C     Usacons       IFNE      'S'                                          --- 7 -->
629800141107     c* Se c'è una chiusura M o P e la filiale ha la soglia, devo calcolare gli
629900141107     c*    orari giorno per giorno
630000141107     c                   if        OOR2FCSGL<>' '
630100141108     c                             and w12dat<>sav_12dat
630200141107     c                   eval      or_datarif=w12dat
630300141107     c                   eval      or_DDC    =w12dat
630400141107     c                   exsr      Reper_tempi
630500141108     c                   eval      sav_12dat=w12dat
630600141107     c                   endif
630700141107     c
630800930325     C* SE E' CHIUSO LA MATTINA E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
630900930325     C     W93GMP        IFEQ      'M'
631000930326     C     W12RES        ANDEQ     *ZEROS
631100141110     c                   if        (OOR2STFS<=1300 and OOR2STfS >0)
631200141111     c                             or OOR2STfS =0
631300141105     c                             or (tuc(IT)<>gc1part and
631400141105     c                              tuc(IT)<>gc2part)
631500140825     C                   MOVEL     'S'           WSLITT
631600140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
631700140911     c* slitto solo una volta  se chiuso tutti " M"
631800140911     c                   if        W93GSE=' '
631900140911     c                   eval      wTUCTUT='S'
632000140911     c                   endif
632100140911     c
632200140825     c                   endif
632300930325     C                   END
632400930325     C* SE E' CHIUSO IL POMERIG. E LE ORE DALLA DATA DA CONTROLLARE=0 -> SLITT.
632500930325     C     W93GMP        IFEQ      'P'
632600930326     C     W12RES        ANDNE     *ZEROS
632700140825     c* Reperisco i tempi di consegna: slitto solo se sono orari solo al pomeriggio
632800140825     c*  per consegna entro pom, o solo al mattino se sono orari entro matt.
632900141111     c                   if        (OOR2STIS> 1300 and OOR2STIS >0)
633000141111     c                             or OOR2STiS =0
633100141105     c                             or (tuc(IT)<>gc1part and
633200141105     c                              tuc(IT)<>gc2part)
633300930325     C                   MOVEL     'S'           WSLITT
633400140825     C                   Z-ADD     24            WNOADD                         *ORE DI SLITTAMENTO
633500140911     c* slitto solo una volta  se chiuso tutti " P"
633600140911     c                   if        W93GSE=' '
633700140911     c                   eval      wTUCTUT='S'
633800140911     c                   endif
633900140825     c                   endif
634000140825     c
634100931213     C                   ENDIF
634200001128     C                   ENDIF                                                  <-- 7 ---
634300931213     C                   ENDIF                                                  <-- 6 ---
634400931213     C                   ENDIF                                                  <-- 5 ---
634500931213     C                   ENDIF                                                  <-- 4 ---
634600931213     C                   ENDDO                                                  <-- 3 ---
634700930405     C*
634800931213     C                   ENDIF                                                  <-- 2 ---
634900931213     C                   ENDIF                                                  <-- 1 ---
635000140911     C* pulisco il campo s già utilizzato e non slitto più avanti per un altro turno chiusura
635100140911     c                   if        wtuctut='P'  and wslitt='N'
635200140911     c                   clear                   wtuctut
635300140911     c                   endif
635400140911     c
635500930325     C                   ENDSR
635600060406     C********************************************************************
635700060406     C*  DATcepG- CONTROLLA SE LA DATA E' evento particolare giustifica
635800060406     C********************************************************************
635900060406     C     DATCEPG       BEGSR
636000060406     C*
636100060406     C* IMPOSTA LE ORE STANDARD DI SLITTAMENTO
636200060406     C                   Z-ADD     12            WNOADD            9 0
636300060406     C*
636400060406    1C                   if        wevpag='S'
636500060406     C*
636600060406     C                   Z-ADD     WPOROU        WORE
636700060406     C                   EXSR      OREDAT                                       *ORE->DATA
636800060406     C                   Z-ADD     WDAT          W12DAT
636900060406     C                   Z-ADD     WRES          W12RES
637000060418     c                   z-add     1             P
637100060406     c* Verifico se nella data esiste un evento Part che giustifica
637200060418     c     w12dat        lookup    pvd(P)                                 30
637300060418    2c                   dow       *in30 and pvf(P)<>'G'
637400060418     c                   add       1             P
637500060420     c     w12dat        lookup    pvd(P)                                 30
637600060406    2c                   enddo
637700060406     c
637800060406     c* 30 on --> TROVATO !!
637900060406    2c                   if        *in30
638000060406    3C     Usacons       IFNE      'S'                                          --- 7 -->
638100060406     C                   MOVEL     'S'           WSLITT
638200060406     c                   z-add     24            wnoadd
638300060406    3C                   ENDIF
638400060406     C*
638500060406    2C                   ENDIF                                                  <-- 2 ---
638600060406    1C                   ENDIF                                                  <-- 1 ---
638700060406     C*
638800060406     C                   ENDSR
638900931210     C********************************************************************
639000931214     C*   CHKDCR - CONTROLLA SE LA DATA E' COMPRESA NEL RANGE
639100931214     C*            PER LA DATA DI CONSEGNA RICHIESTA
639200931210     C********************************************************************
639300050805     C***  CHKDCR        BEGSR
639400931210     C*
639500050805     C***  WPOCRI        IFNE      *ZEROS
639600050805     C***  WPOROU        IFGE      WPOCRI
639700050805     C***  WPOROU        ANDLT     WPOCRF
639800050805     C***                ELSE
639900050805     C***                MOVEL     'S'           WSLITT
640000050805     C***                END
640100050805     C***                END
640200931210     C*
640300050805     C***                ENDSR
640400140822     C********************************************************************
640500140822     C*  Reperimento tempi di consegna per decidere se slittare
640600140822     C********************************************************************
640700140822     C     Reper_tempi   BEGSR
640800140822
640900140822     c                   clear                   trulorSds
641000140825     c                   clear                   trulor2ds
641100140822     c                   eval       IOREtfp = d98tfp
641200140822     c                   eval       IOREtfa = d98tfa
641300140825     c                   eval       IOREdti = d98dti
641400140825     c                   eval       IOREhti = d98hti
641500140825     c                   eval       IOREdcr = s98dcr
641600140825     c                   eval       IOREhcr = s98hcr
641700140825     c                   eval       IOREtcr = s98tcr
641800140825     c                   eval       IOREDEI = d98dei
641900140825     c                   eval       IOREOEI = d98oei
642000140825     c                   eval       IOREfil = d98lna
642100140825     c                   eval       IOREcap = d98cad
642200140825     c                   eval       IOREloc = d98lod
642300140825     c                   eval       IOREnar = d98nzd
642400140825     c
642500140825     c* Data di Riferimento:
642600140825     c* Se impostata la consegnabilità --> prendo questa
642700140822     c                   select
642800140825     c                   when      d98dfc>0
642900141107     c                   if        or_datarif<d98dfc and or_datarif>0
643000141107     c                   eval      ioredta=or_datarif
643100141107     c                   else
643200141107     c                   eval      ioredta=d98dfc
643300141107     c                   endif
643400140825     c* altrimenti data di consegna
643500140825     c                   when      d98dcm>0
643600141107     c                   if        or_datarif<d98dcm and or_datarif>0
643700141107     c                   eval      ioredta=or_datarif
643800141107     c                   else
643900140825     c                   eval      ioredta=d98dcm
644000141107     c                   endif
644100140825     c
644200140822     c                   other
644300140825     c* altrimenti data teorica consegna se > = oggi
644400140825     C                   time                    wrkoggiiso
644500140825     C     *iso          move      wrkoggiiso    ioredta
644600140822     c* se < di oggi, imposto oggi
644700140825     c                   if        d98dei>ioredta
644800140825     c                   eval      ioredta=d98dei
644900140822     c                   endif
645000141107     c
645100141107     c                   if        or_datarif<ioredta and or_datarif>0
645200141107     c                   eval      ioredta=or_datarif
645300141107     c                   endif
645400141107     c
645500140822     c                   endsl
645600140822     c
645700140825     c                   eval       IOREtser = 'C'
645800141107     c                   if         or_ddc>0 and or_ddc<d98ddc
645900141107     c                   eval       IOREDDC=or_ddc
646000141107     c                   eval       IOREnDC=8888888
646100141107     c                   eval       IOREcons = 'S'
646200141107     c                   else
646300140825     c                   eval       IOREDDC=d98ddc
646400140825     c                   eval       IOREnDC=wd98ndc
646500140825     c                   eval       IOREcons = d98cons
646600141107     c                   endif
646700141017     c                   eval       IOREtsp = d98tsp
646800140822     c
646900140822     c                   call      'TRULORSR'
647000140822     c                   parm                    kpjba
647100140822     c                   parm                    trulorSds
647200140825     c                   parm                    trulor2ds
647300140825     c
647400140825     c                   eval      wtrulor= 'S'
647500141107     c                   clear                   or_datarif        8 0
647600141107     c                   clear                   or_ddc            8 0
647700140822     C                   ENDSR
647800950406     C********************************************************************
647900950406     C*   DATORE - TRASFORMA UNA DATA IN NUMERO ORE DAL 01.01.0001
648000950406     C********************************************************************
648100950406     C     DATORE        BEGSR
648200950406     C*
648300950406     C                   Z-ADD     *ZEROS        WORE
648400950406     C     WDAT          IFNE      0
648500950406     C                   SUB       1900          WAA                            ANNO A 2 CIFRE
648600950406     C     WAA           DIV       4             WQUA              4 0
648700950406     C                   MVR                     WRES
648800950406     C     WAA           MULT      365           WGIO              9 0
648900950406     C                   ADD       WQUA          WGIO
649000950406     C                   ADD       WGG           WGIO
649100950406     C     WMM           SUB       1             MC                3 0
649200950406     C                   DO        MC            WX                3 0
649300950406     C                   ADD       MES(WX)       WGIO
649400950406     C                   END
649500950406     C     WRES          IFEQ      0
649600950406     C     WMM           ANDLE     2
649700950406     C                   SUB       1             WGIO
649800950406     C                   END
649900950406     C     WGIO          MULT      24            WORE              9 0
650000950406     C                   ENDIF
650100950406     C*
650200950406     C                   ENDSR
650300930323     C********************************************************************
650400930323     C*   OREDAT - TRASFORMA IL NUMERO ORE IN DATA
650500930323     C********************************************************************
650600930323     C     OREDAT        BEGSR
650700930323     C*
650800950407     C                   SETOFF                                       717273
650900950407     C                   SETOFF                                       7475
651000930323     C     WORE          DIV       24            WGIO
651100930323     C                   MVR                     WRES              3 0
651200950406     C*
651300950406     C                   Z-ADD     GPM           $PM                            *SCHIERE RUNTIME ->
651400950406     C                   Z-ADD     MES           $GM                                 SCHIERE LAVORO
651500950406     C*  TROVO ANNO
651600950406     C     WGIO          DIV       365           WAA
651700950406     C     WAA           DIV       4             WNBIS             3 0          QUANTI BISEST.
651800950406     C                   MVR                     WREST                71
651900950406     C     WGIO          SUB       WNBIS         WGIO                           GG - BISESTILI
652000950406     C     WGIO          DIV       365           WAA
652100950406     C                   MVR                     WREST             3 0    75    GG.RESTANTI
652200950406     C*                                                    0 = 31/12/PR
652300950406     C   75
652400950406     CAN 71              DO                                                     BIS. E NON ERA
652500950406     C     WAA           SUB       1             WAA
652600950406     C                   Z-ADD     365           WREST
652700950406     C                   END
652800950406     C*
652900950406     C     WAA           DIV       4             WNBIS                                      T.
653000950406     C                   MVR                     WRBIS             3 073
653100950406     C   73
653200950406     CANN71                                                                     NON PIU' BISEST
653300950406     CORN73              ADD       1             WREST                          E'BISESTILE
653400950406     C  N73              ADD       1             $GM(2)                         FEBB.SE BIS.
653500950406     C  N73              ADD       1             $PM                            SE BIS.+1GG
653600950406     C  N73              SUB       1             $PM(1)
653700950406     C                   Z-ADD     1             WI                2 0
653800950406     C* TROVO IL MESE E GIORNI
653900950406     C     WREST         LOOKUP    $PM(WI)                            74  73
654000950406     C                   MOVE      WI            WMM
654100950406     C     WI            COMP      1                                      72
654200950406     C  N72
654300950406     CAN 73              MOVE      $GM(WI)       WGG                            ULTIMO DEL MM
654400950406     C   74              SUB       1             WI
654500950406     C  N72
654600950406     CANN73WREST         SUB       $PM(WI)       WGG                            ALTRI GG.
654700950406     C   72              MOVE      WREST         WGG
654800950406     C                   ADD       1900          WAA
654900950406     C*
655000930323     C*
655100930323     C                   ENDSR
6552009407151507 C********************************************************************
6553009407151507 C*   FINGIO - PORTA IL PROGRESSIVO ORE A FINE GIORNATA
6554009407151507 C*            SE MATTINO PORTA ALLA SERA ALTRIMENTI NON FA NULLA
6555009407151507 C*            INPUT  - WPORE (PROGRESSIVO DATA DA CONTROLLARE)
6556009407151507 C*            OUTPUT - WNORE (N. DI ORE PER ARRIVARE A FINE GIORNATA)
6557009407151507 C********************************************************************
6558009407151507 C     FINGIO        BEGSR
6559009407151507 C*
6560009407151507 C     WPORE         DIV       24            WGIO
6561009407151507 C                   MVR                     WRES
6562009407151507 C     WRES          IFEQ      *ZEROS
6563009407151507 C                   Z-ADD     12            WNORE             3 0
6564009407151507 C                   ELSE
6565009407151507 C                   Z-ADD     *ZEROS        WNORE
6566009407151507 C                   ENDIF
6567009407151507 C*
6568009407151507 C                   ENDSR
656900930517     C********************************************************************
657000971113     C*   CERCAL - CERCA UN RECORD DEL CALENDARIO
657100930517     C*            SE NON LO TROVA IN MEMORIA LO LEGGE E LO MEMORIZZA
657200930517     C********************************************************************
657300930517     C     CERCAL        BEGSR
657400930517     C*
657500930517     C* PORTA I CAMPI DELLA CHIAVE DEL FILE IN DS PER LETTURA SCHIERA
657600941227     C                   MOVE      CLNTFP        DCATFP
657700941227     C                   MOVE      CLNTFA        DCATFA
657800941227     C                   MOVE      CLNANN        DCAANN
657900941227     C                   MOVE      CLNMES        DCAMES
658000930517     C* CONTROLLA SE GIA' CARICATO IN SCHIERA
658100930517     C                   Z-ADD     1             I
658200930517     C     DCALEN        LOOKUP    CAK(I)                                 99
658300930517     C* NON L'HA TROVATO IN SCHIERA LO LEGGE DAL FILE
658400930517     C     *IN99         IFEQ      '0'                                          --- 1 -->
658500941227     C     KEYCLN        CHAIN     AZCLN01L                           99
658600930517     C* NON L'HA TROVATO NEMMENO SUL FILE, ERRORE
658700930517     C     *IN99         IFEQ      '1'                                          --- 2 -->
658800930517     C                   MOVEL     '1'           D98ERR
658900930517     C                   MOVEL     MSG(3)        D98MSG
659000930517     C                   ELSE                                                   <-- 2 -->
659100970401     C* CONTROLLA SE ANNULLATO
659200970401     C     CLNATB        IFNE      *BLANKS                                      --- 3 -->
659300970401     C                   MOVEL     '1'           D98ERR
659400970401     C                   MOVEL     MSG(3)        D98MSG
659500970401     C                   ELSE                                                   <-- 3 -->
659600930517     C* LO MEMORIZZA IN SCHIERA SE LA SCHIERA NON E' GIA' PIENA
659700930517     C                   ADD       1             WINDCA            6 0
659800970401     C     WINDCA        IFLE      200                                          --- 4 -->
659900970401     C     WINDCA        ANDGT     *ZEROS
660000930517     C                   Z-ADD     WINDCA        I
660100930517     C                   MOVEL     DCALEN        CAK(I)
660200941227     C                   MOVEL     CLNMAT        CAM(I)
660300941227     C                   MOVEL     CLNPOM        CAP(I)
660400970401     C                   END                                                    <-- 4 ---
660500970401     C                   ENDIF                                                  <-- 3 ---
660600930517     C                   END                                                    <-- 2 ---
660700930517     C                   ELSE                                                   <-- 1 -->
660800930517     C* L'HA TROVATO IN SCHIERA PORTA I CALORI NEI CAMPI DEL FILE
660900941227     C                   MOVEL     CAM(I)        CLNMAT
661000941227     C                   MOVEL     CAP(I)        CLNPOM
661100930517     C                   END                                                    <-- 1 ---
661200930517     C*
661300930517     C                   ENDSR
661400010821     C********************************************************************
661500010821     C*   CHKVCA - EFFETTUA CONSIDERAZIONI SU DIVISE
661600010821     C********************************************************************
661700010821     C     CHKVCA        BEGSR
661800010821     C*
661900010821     C* TESTA LA DIVISA SE = *BLANKS => FORZO A ITL
662000010821     C     D98CAS        IFGT      *ZEROS
662100010821     C     D98VCA        ANDEQ     *BLANKS
662200010821     C                   MOVEL     'ITL'         D98VCA
662300010821     C                   ENDIF
662400010822     C*
662500010822     C     D98VCA        IFNE      'ITL'
662600010822     C     D98VCA        ANDNE     'EUR'
662700010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
662800010822     C                   Z-ADD     D98CAS        INPIMP
662900010822     C                   MOVEL     D98VCA        INPDIV
663000010822     C                   EXSR      EXECNV
663100010822     C                   Z-ADD     OUTIMP        D98CAS
663200010822     C                   MOVEL     §GEDCN        D98VCA
663300010822     C                   ENDIF
663400010821     C*
663500010821     C                   ENDSR
663600010821     C********************************************************************
663700010821     C*   EXECNV - EFFETTUA LA CONVERSIONE NELLA MONETA DI CONTO "CORRENTE"
663800010821     C********************************************************************
663900010821     C     EXECNV        BEGSR
664000010821     C*
664100010821     C                   CLEAR                   YEURDS
664200010821     C*
664300010821     C                   MOVEL     INPDIV        YECDVI
664400010821     C                   Z-ADD     INPIMP        YECIMI
664500010821     C                   MOVEL     §GEDCN        YECDVO
664600010821     C                   MOVEL     '3'           YECDCO
664700010821     C                   MOVEL     'H'           YECTAR
664800010821     C                   CALL      'YEURCO'
664900010821     C                   PARM                    YEURDS
665000010821     C*
665100010821     C                   Z-ADD     YECIMO        OUTIMP
665200010821     C*
665300010821     C                   ENDSR
665400011211     C*------------------------------------------------------------------------*
665500011211     C* Controllo importo massimo contrassegno.
665600011211     C*------------------------------------------------------------------------*
665700011211     C     REPMAXCSB     BEGSR
665800011211     C*
665900011211     C                   CLEAR                   Trul21DS
666000011211     C*
666100011211     C                   EVAL      I21TSp = d98TSP
666200011211     C                   EVAL      I21NzD = d98NZD
666300011211     C                   EVAL      I21LnP = d98LNP
666400011211     C                   EVAL      I21LnA = d98LNA
666500011211     C                   EVAL      I21Ksc = d98KSC
666600011211     C                   EVAL      I21TiC = d98TIC
666700011211     C                   EVAL      I21Imp = d98CAS
666800011211     C                   EVAL      I21Div = d98VCA
666900011211     C*
667000011211     C                   CALL      'TRUL21R'
667100011211     C                   PARM                    Trul21DS
667200011211     C*
667300011211     C                   ENDSR
667400011211     C*------------------------------------------------------------------------*
667500011211
667600930414     C********************************************************************
667700930414     C*   RUTINZ - ROUTINE INIZIALE
667800930414     C********************************************************************
667900930414     C     RUTINZ        BEGSR
668000930414     C*
668100930414     C* PARAMETRI IN INPUT ALLA ROUTINE
668200930414     C     *ENTRY        PLIST
668300970731     C                   PARM                    DS99
668400941227     C*
668500941228     C* CHIAVE LETTURA FNBLP01L - COMPLETA
668600941227     C     KEYBLP        KLIST
668700941227     C                   KFLD                    BLPAAS
668800941227     C                   KFLD                    BLPLNP
668900941227     C                   KFLD                    BLPNRS
669000941227     C                   KFLD                    BLPNSP
669100050405     C* CHIAVE LETTURA TICGP01L - PARZIALE
669200941227     C     KEYGCP        KLIST
669300050405     C                   KFLD                    KWGCPAAS
669400050405     C                   KFLD                    KWGCPLNP
669500050405     C                   KFLD                    KWGCPNRS
669600050405     C                   KFLD                    KWGCPNSP
669700050405     C                   KFLD                    KWGCPFRG
669800050405     C* CHIAVE LETTURA TICGP01L/TIGCP21L - PARZIALE
669900031125     C     KEYGC_P       KLIST
670000050405     C                   KFLD                    KWGCPAAS
670100050405     C                   KFLD                    KWGCPLNP
670200050405     C                   KFLD                    KWGCPNRS
670300050405     C                   KFLD                    KWGCPNSP
670400970912     C* CHIAVE LETTURA FNBLT01L - PARZIALE
670500150120     C**   KEYBLT        KLIST
670600150120     C**                 KFLD                    BLTAAS
670700150120     C**                 KFLD                    BLTLNP
670800150120     C**                 KFLD                    BLTNRS
670900150120     C**                 KFLD                    BLTNSP
671000941227     C* CHIAVE LETTURA FNARB01L - COMPLETA
671100941227     C     KEYARB        KLIST
671200941227     C                   KFLD                    ARBAAS
671300941227     C                   KFLD                    ARBLNP
671400941227     C                   KFLD                    ARBNRS
671500941227     C                   KFLD                    ARBNSP
671600051115     C* CHIAVE LETTURA FiAR901L - COMPLETA
671700941228     C     KEYAR9        KLIST
671800941228     C                   KFLD                    AR9AAS
671900941228     C                   KFLD                    AR9LNP
672000941228     C                   KFLD                    AR9NRS
672100941228     C                   KFLD                    AR9NSP
672200941227     C* CHIAVE LETTURA FLCGA01L - COMPLETA
672300050405     C     KEYGCP21      KLIST
672400050405     C                   KFLD                    KWGCPAAS
672500050405     C                   KFLD                    KWGCPLNP
672600050405     C                   KFLD                    KWGCPNRS
672700050405     C                   KFLD                    KWGCPNSP
672800050405     C                   KFLD                    KWGCPFRG
672900970912     C* CHIAVE LETTURA FNART01L - PARZIALE
673000150120     C***  KEYART        KLIST
673100150120     C***                KFLD                    ARTAAS
673200150120     C***                KFLD                    ARTLNP
673300150120     C***                KFLD                    ARTNRS
673400150120     C***                KFLD                    ARTNSP
673500950102     C* CHIAVE LETTURA FLLBL01L - COMPLETA
673600950102     C     KEYLBL        KLIST
673700950102     C                   KFLD                    LBLAAN
673800950102     C                   KFLD                    LBLLPN
673900950102     C                   KFLD                    LBLNRN
674000950102     C                   KFLD                    LBLNSN
674100930414     C*
674200070827     c     kAR5          Klist
674300070827     c                   Kfld                    d98Aas
674400070827     c                   Kfld                    d98Lnp
674500070827     c                   Kfld                    d98nrs
674600070827     c                   Kfld                    d98nsp
674700070827     c                   Kfld                    kAr5Trd
674800150325     c     kAR4          Klist
674900150325     c                   Kfld                    d98Aas
675000150325     c                   Kfld                    d98Lnp
675100150325     c                   Kfld                    d98nrs
675200150325     c                   Kfld                    d98nsp
675300150325     c                   Kfld                    kAr4Trc
675400150218     c*
675500150218     c     kart          Klist
675600150218     c                   Kfld                    d98Aas
675700150218     c                   Kfld                    d98Lnp
675800150218     c                   Kfld                    d98nrs
675900150218     c                   Kfld                    d98nsp
676000150218     c     kbrv          Klist
676100150218     c                   Kfld                    artfls
676200150218     c                   Kfld                    artlna
676300150218     c                   Kfld                    artnrs
676400150218     c                   Kfld                    artnsc
676500150325     C     Kfapd         klist
676600150325     C                   kfld                    kpdtip
676700150325     C                   kfld                    kpdpdr
676800930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
676900930414     C     KEYTAB        KLIST
677000930414     C                   KFLD                    KTBUTE
677100930414     C                   KFLD                    KTBCOD
677200930414     C                   KFLD                    KTBKEY
677300060406     C     Ktab          KLIST
677400060406     C                   KFLD                    KTBUTE
677500060406     C                   KFLD                    KTBCOD
677600930414     C*
677700941227     C* CHIAVE LETTURA AZCLN01L - COMPLETA
677800941227     C     KEYCLN        KLIST
677900941227     C                   KFLD                    CLNTFP
678000941227     C                   KFLD                    CLNTFA
678100941227     C                   KFLD                    CLNANN
678200941227     C                   KFLD                    CLNMES
678300941227     C*
678400941227     C* CHIAVE LETTURA AZCAS01L - COMPLETA
678500941227     C     KEYCAS        KLIST
678600941227     C                   KFLD                    CASPTA
678700941227     C                   KFLD                    CASLNP
678800941227     C                   KFLD                    CASLNA
678900941227     C                   KFLD                    CASTSP
679000941227     C                   KFLD                    CASDDE
679100930420     C*
679200990810     C* CHIAVE LETTURA FNEVB01L - COMPLETA
679300930420     C     KEYEVB        KLIST
679400930420     C                   KFLD                    KEBAAS
679500930420     C                   KFLD                    KEBLNP
679600930420     C                   KFLD                    KEBNRS
679700930420     C                   KFLD                    KEBNSP
679800010823     C* CHIAVE LETTURA TNTBE00F - PARZIALE
679900010821     C     KTBE          KLIST
680000010821     C                   KFLD                    TBECOD
680100010821     C                   KFLD                    TBEKE1
680200010821     C                   KFLD                    TBEKE2
680300930414     C*
680400010821     C* DEFINISCO CAMPI CHIAVE E CAMPI DI WRK
680500930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
680600930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
680700930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
680800930420     C     *LIKE         DEFINE    EVBAAS        KEBAAS
680900930420     C     *LIKE         DEFINE    EVBLNP        KEBLNP
681000930420     C     *LIKE         DEFINE    EVBNRS        KEBNRS
681100930420     C     *LIKE         DEFINE    EVBNSP        KEBNSP
681200930420     C     *LIKE         DEFINE    EVBDEV        KEBDEV
681300930420     C     *LIKE         DEFINE    EVBHEV        KEBHEV
681400010821     C     *LIKE         DEFINE    D98CAS        INPIMP
681500010821     C     *LIKE         DEFINE    D98CAS        OUTIMP
681600010821     C     *LIKE         DEFINE    D98VCA        INPDIV
681700930414     C*
681800930414     C* RECUPERO IL CODICE DELL'AZIENDA
681900930414     C                   Z-ADD     1             CODUT             1 0
682000970701     C                   Z-ADD     *ZEROS        I                 6 0
682100930414     C*
682200930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
682300930414     C                   EXSR      AZZERA
682400930414     C*
682500930414     C* CARICA LE TABELLE NEL CASO NON SIANO GIA' STATE CARICATE
682600941227     C     CTABSN        IFNE      'S'                                          --- 1 -->
682700941227     C                   MOVEL     'S'           CTABSN            1
682800930414     C                   EXSR      CARTAB
682900941227     C                   ENDIF
683000041116     c* Se passato umero di spedizioni apro i files , altrimenti no
683100041116     c                   if        d98nsp>0 and wapri=' '
683200041116     c                   open      fnblp01l
683300050405     c                   open      tigcp01l
683400150120     c***                open      fnblt01l
683500041116     c                   open      fnarb01l
683600051115     c                   open      fiar901l
683700150325     c                   open      fiar401l
683800050405     c                   open      tigcp21l
683900150218     c                   open      fnart01l
684000150218     c                   open      fnbrv07l
684100041116     c                   open      fnlbl01l
684200041116     c                   open      fnevb01l
684300070827     c                   open      fiar501l
684400041116     c                   eval      wapri='S'
684500041116     c                   endif
684600930414     C*
684700930414     C                   ENDSR
684800941227     C*------------------------------------------------------------------------*
684900941227     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
685000941227     C*------------------------------------------------------------------------*
685100941227     C     SUBDAT        BEGSR
685200941227     C*
685300941227     C                   TIME                    DTTIME
685400941227     C                   Z-ADD     DTANN         DSANN
685500941227     C                   Z-ADD     DTMES         DSMES
685600941227     C                   Z-ADD     DTGIO         DSGIO
685700941227     C                   Z-ADD     DTSEC         DSSEC
685800941227     C*
685900941227     C                   ENDSR
686000930414     C********************************************************************
686100930414     C*   CARTAB - CARICA LE TABELLE
686200930414     C********************************************************************
686300930414     C     CARTAB        BEGSR
686400930414     C*
686500930414     C* CARICA TABELLA SIMBOLI CALENDARIO -1Y-
686600930414     C     D98ERR        IFEQ      '0'
686700930421     C                   Z-ADD     0             I
686800930414     C                   Z-ADD     CODUT         KTBUTE
686900930414     C                   MOVEL     '1Y'          KTBCOD
687000930414     C                   MOVEL     *BLANKS       KTBKEY
687100930414     C     KEYTAB        SETLL     TABEL00F                           99
687200930414     C  N99              READ      TABEL00F                               99
687300930414     C     *IN99         IFEQ      '1'
687400930414     C                   MOVEL     '1'           D98ERR
687500930414     C                   MOVEL     MSG(5)        D98MSG
687600930414     C                   END
687700930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
687800930414     C     TBLCOD        ANDEQ     '1Y'
687900930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
688000930421     C                   ADD       1             I
688100930421     C                   MOVEL     TBLKEY        C1Y(I)
688200930414     C                   MOVEL     TBLUNI        DS1Y
688300930421     C                   MOVEL     §1YFEI        I1Y(I)
688400930421     C                   MOVEL     §1YFEC        E1Y(I)
688500930414     C                   END                                                    <-- 3 ---
688600930414     C                   READ      TABEL00F                               99
688700930414     C                   END                                                    <-- 2 ---
688800930414     C                   END
688900930414     C*
689000930414     C* CARICA TABELLA TEMPI FISSI DI DELIVERY -1X-
689100930414     C     D98ERR        IFEQ      '0'
689200930421     C                   Z-ADD     0             I
689300930414     C                   Z-ADD     CODUT         KTBUTE
689400930414     C                   MOVEL     '1X'          KTBCOD
689500930414     C                   MOVEL     *BLANKS       KTBKEY
689600930414     C     KEYTAB        SETLL     TABEL00F                           99
689700930414     C  N99              READ      TABEL00F                               99
689800930414     C     *IN99         IFEQ      '1'
689900930414     C                   MOVEL     '1'           D98ERR
690000930414     C                   MOVEL     MSG(1)        D98MSG
690100930414     C                   END
690200930414     C     *IN99         DOWEQ     '0'                                          --- 2 -->
690300930414     C     TBLCOD        ANDEQ     '1X'
690400930414     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
690500930421     C                   ADD       1             I
690600930421     C                   MOVEL     TBLKEY        C1X(I)
690700930414     C                   MOVEL     TBLUNI        DS1X
690800930421     C                   Z-ADD     §1XHHG        G1X(I)
690900930421     C                   Z-ADD     §1XHHO        O1X(I)
691000930421     C                   Z-ADD     §1XHHM        M1X(I)
691100930421     C                   Z-ADD     §1XHHP        P1X(I)
691200930421     C                   Z-ADD     §1XHHI        I1X(I)
691300930421     C                   Z-ADD     §1XHHS        S1X(I)
691400930414     C                   END                                                    <-- 3 ---
691500930414     C                   READ      TABEL00F                               99
691600931223     C                   ENDDO                                                  <-- 2 ---
691700050523     c
691800050523     c* controllo riempimento skiere
691900050523     c                   clear                   trul0sds
692000050523     c                   eval      i0sski='C1X/G1X/O1X/...'
692100050523     c                   eval      i0sele=%elem(c1x)
692200050523     c                   eval      i0spie=i
692300050523     c                   eval      i0sfile='TABEL00F  '
692400050523     c                   eval      i0scod=' 1X  '
692500050523     c                   eval      i0spgm='TNSD99R'
692600050523     c                   movel     trul0sds      kpjbu
692700050523     c                   call      'TRUL0SR'
692800050523     c                   parm                    kpjba
692900050523     C                   END
693000930421     C*
693100930421     C* CARICA TABELLA CAUSALI EVENTI BOLLA -2A-
693200930421     C     D98ERR        IFEQ      '0'
693300930421     C                   Z-ADD     0             I
693400930421     C                   Z-ADD     CODUT         KTBUTE
693500930421     C                   MOVEL     '2A'          KTBCOD
693600060406     C     Ktab          SETLL     TABEL00F                           99
693700060406     C  N99ktab          READe     TABEL00F                               99
693800930421     C     *IN99         IFEQ      '1'
693900930421     C                   MOVEL     '1'           D98ERR
694000930421     C                   MOVEL     MSG(8)        D98MSG
694100930421     C                   END
694200930421     C     *IN99         DOWEQ     '0'                                          --- 2 -->
694300930421     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
694400930421     C                   ADD       1             I
694500930421     C                   MOVEL     TBLKEY        C2A(I)
694600060406     C                   MOVEL     TBLUNI        ds2a
694700060406     c                   eval      w003a=w2aftc+%editc(w2agla:'X')+w2aftg
694800060406     C                   MOVEL     w003a         D2A(I)
694900930421     C                   END                                                    <-- 3 ---
695000060406     C     ktab          READe     TABEL00F                               99
695100931223     C                   ENDDO                                                  <-- 2 ---
695200050523     c* controllo riempimento skiere
695300050523     c                   clear                   trul0sds
695400050523     c                   eval      i0sski='C2A/D2A'
695500050523     c                   eval      i0sele=%elem(c2a)
695600050523     c                   eval      i0spie=i
695700050523     c                   eval      i0sfile='TABEL00F  '
695800060406     c                   eval      i0scod='2A/2Z'
695900050523     c                   eval      i0spgm='TNSD99R'
696000050523     c                   movel     trul0sds      kpjbu
696100050523     c                   call      'TRUL0SR'
696200050523     c                   parm                    kpjba
696300930421     C                   END
696400060406     c* Tabella  2Z
696500060406     C     D98ERR        IFEQ      '0'
696600060406     C                   Z-ADD     CODUT         KTBUTE
696700060406     C                   MOVEL     '2Z'          KTBCOD
696800060406     C     Ktab          SETLL     TABEL00F                           99
696900060406     C  N99ktab          READe     TABEL00F                               99
697000060406     C     *IN99         IFEQ      '1'
697100060406     C                   MOVEL     '1'           D98ERR
697200060406     C                   MOVEL     MSG(33)       D98MSG
697300060406     C                   END
697400060406     C     *IN99         DOWEQ     '0'                                          --- 2 -->
697500060406     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
697600060406     C                   Z-ADD     1             I
697700060406     C                   MOVEL     TBLKEY        w003a
697800060406     c     w003a         lookup    c2a(i)                                 99
697900060406     C     *IN99         IFEQ      *off
698000060406     C                   MOVEL     '1'           D98ERR
698100060406     C                   MOVEL     MSG(34)       D98MSG
698200060406     C                   Else
698300060406     C                   eval      D2Z(i)=%subst(tbluni:23:5)
698400060406     C                   END                                                    <-- 3 ---
698500060406     C                   END                                                    <-- 3 ---
698600060406     C     ktab          READe     TABEL00F                               99
698700060406     C                   ENDDO                                                  <-- 2 ---
698800060406     C                   END
698900941227     C*
699000941227     C* CARICA TABELLA CODICI BOLLA -3A-
699100941227     C     D98ERR        IFEQ      '0'
699200941228     C                   Z-ADD     0             I
699300941227     C                   Z-ADD     CODUT         KTBUTE
699400941227     C                   MOVEL     '3A'          KTBCOD
699500941227     C                   MOVEL     *BLANKS       KTBKEY
699600941227     C     KEYTAB        SETLL     TABEL00F                           99
699700941227     C  N99              READ      TABEL00F                               99
699800941227     C     *IN99         IFEQ      '1'
699900941227     C                   MOVEL     '1'           D98ERR
700000941227     C                   MOVEL     MSG(22)       D98MSG
700100941227     C                   END
700200941227     C     *IN99         DOWEQ     '0'                                          --- 2 -->
700300941227     C     TBLCOD        ANDEQ     '3A'
700400941227     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
700500941227     C                   ADD       1             I
700600941227     C                   MOVEL     TBLKEY        C3A(I)
700700941227     C                   MOVEL     TBLUNI        DS3A
700800941227     C                   MOVEL     §3ATB1        T3A(I)
700900941228     C                   Z-ADD     §3AFCA        F3A(I)
701000990308     C                   MOVEL     §3ARBL        R3A(I)
701100941227     C                   END                                                    <-- 3 ---
701200941227     C                   READ      TABEL00F                               99
701300941227     C                   END                                                    <-- 2 ---
701400050523     c* controllo riempimento skiere
701500050523     c                   clear                   trul0sds
701600050523     c                   eval      i0sski='C3A/T3A/F3A/R3A'
701700050523     c                   eval      i0sele=%elem(c3a)
701800050523     c                   eval      i0spie=i
701900050523     c                   eval      i0sfile='TABEL00F  '
702000050523     c                   eval      i0scod=' 3A  '
702100050523     c                   eval      i0spgm='TNSD99R'
702200050523     c                   movel     trul0sds      kpjbu
702300050523     c                   call      'TRUL0SR'
702400050523     c                   parm                    kpjba
702500941227     C                   END
702600971210     C*
702700020226     C* CARICA FLAG ITALIA/ESTERO/FEDEX DALL'ORGANIGRAMMA
702800020226     C                   Z-ADD     *ZEROS        J                 3 0
702900971210IF  0C     D98ERR        IFEQ      '0'
703000971210     C     *LOVAL        SETLL     AZORG01L
703100971210     C                   READ      AZORG01L                               98
703200971210DO  1C     *IN98         DOWEQ     *OFF
703300971210IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
703400971210     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
703500971211     C                   Z-ADD     ORGFIL        F                 4 0          *COD.FILIALE=INDICE
703600020627     C                   MOVEL     ORGDE3        OG143
703700020702     C*
703800020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
703900020702     C                   CLEAR                   DNTW
704000020702     C*
704100020702     C                   MOVEL(P)  'NTW'         TBECOD
704200020702     C                   MOVEL(P)  §OGNTW        TBEKE1
704300020702     C     KTBE          CHAIN     TNTBE01L
704400020702     C                   IF        %found(TNTBE01L)
704500020702     C                   MOVEL     TBEUNI        DNTW
704600020702     C                   ENDIF
704700020702     C*
704800020702     C                   MOVEL     §NTWFIE       FL1(F)
704900020627     C*
705000020226     C* VERIFICO SE FILIALE FEDEX
705100020226     C                   IF        §OGNTW = 'FED'
705200020226     C                   ADD       1             J
705300020226     C                   Z-ADD     ORGFIL        FILFED(J)
705400020226     C                   ENDIF
705500020226     C*
705600971210E   2C                   ENDIF
705700971210     C                   READ      AZORG01L                               98
705800971210E   1C                   ENDDO
705900971210E   0C                   ENDIF
706000010821     C*
706100010821     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
706200010821     C                   MOVEL     'GED'         TBECOD
706300070514     C                   MOVEL     *BLANKS       TBEKE1
706400070514     C                   MOVEL     '1'           TBEKE1
706500010821     C                   MOVEL     *BLANKS       TBEKE2
706600010821     C     KTBE          CHAIN     TNTBE01L                           21
706700070514     C  N21              MOVEL     TBEUNI        DGED
706800150326     c
706900150326     C* REPERISCO LA tabella flag operativi per tabella OLP
707000150326     c                   clear                   dvpo
707100150326     C                   MOVEL     'VPO'         TBECOD
707200150326     C                   MOVEL     *BLANKS       TBEKE2
707300150326     C                   MOVEL(P)  'VPO'         TBEKE1
707400150326     C     KTBE          CHAIN     TNTBE01L                           21
707500150326     C  N21              MOVEL     TBEUNI        DVPO
707600150326     C*
707700031201     C*
707800031201     C* CARICA TABELLA GESTIONE PARTICOLARITA' GIACENZA (7Q)
707900031201     C     D98ERR        IFEQ      '0'
708000060403     C                   Z-ADD     0             I7QW              3 0
708100031201     C                   Z-ADD     CODUT         KTBUTE
708200031201     C                   MOVEL     '7Q'          KTBCOD
708300031201     C                   MOVEL     *BLANKS       KTBKEY
708400031201     C     KEYTAB        SETLL     TABEL00F                           99
708500031201     C  N99              READ      TABEL00F                               99
708600031201     C     *IN99         IFEQ      '1'
708700031201     C                   MOVEL     '1'           D98ERR
708800031201     C                   MOVEL     MSG(8)        D98MSG
708900031201     C                   ENDIF
709000031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
709100031201     C     TBLCOD        ANDEQ     '7Q'
709200031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
709300031201     C                   MOVEL     TBLUNI        DS7Q
709400031201     C                   IF        §7Q4D1 = 'R' OR
709500031201     C                             §7Q4D2 = 'R' OR
709600031201     C                             §7Q4D3 = 'R' OR
709700031201     C                             §7Q4D4 = 'R' OR
709800031201     C                             §7Q4D5 = 'R'
709900031201     C                   ADD       1             I7QW
710000031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
710100031201     C                   ENDIF                                                  <-- 3 ---
710200031201     C                   ENDIF                                                  <-- 3 ---
710300031201     C                   READ      TABEL00F                               99
710400031201     C                   ENDDO                                                  <-- 2 ---
710500031201     C                   ENDIF
710600031201     C*
710700031201     C* CARICA TABELLA ESTENSINE GESTIONE PARTICOLARITA' GIACENZA (7W)
710800031201     C     D98ERR        IFEQ      '0'
710900031201     C                   Z-ADD     CODUT         KTBUTE
711000031201     C                   MOVEL     '7W'          KTBCOD
711100031201     C                   MOVEL     *BLANKS       KTBKEY
711200031201     C     KEYTAB        SETLL     TABEL00F                           99
711300031201     C  N99              READ      TABEL00F                               99
711400031201     C     *IN99         IFEQ      '1'
711500031201     C                   MOVEL     '1'           D98ERR
711600031201     C                   MOVEL     MSG(8)        D98MSG
711700031201     C                   ENDIF
711800031201     C     *IN99         DOWEQ     '0'                                          --- 2 -->
711900031201     C     TBLCOD        ANDEQ     '7W'
712000031201     C     TBLFLG        IFEQ      *BLANKS                                      --- 3 -->
712100031201     C                   MOVEL     TBLUNI        DS7W
712200031201     C                   IF        §7W6D1 = 'C' OR
712300031201     C                             §7W6D2 = 'C' OR
712400031201     C                             §7W6D3 = 'C' OR
712500031201     C                             §7W6D4 = 'C' OR
712600031201     C                             §7W6D5 = 'C'
712700031201     C                   ADD       1             I7QW
712800031201     C                   MOVEL     TBLKEY        SK7QW(I7QW)
712900031201     C                   ENDIF                                                  <-- 3 ---
713000031201     C                   ENDIF                                                  <-- 3 ---
713100031201     C                   READ      TABEL00F                               99
713200031201     C                   ENDDO                                                  <-- 2 ---
713300050523     c* controllo riempimento skiere
713400050523     c                   clear                   trul0sds
713500050523     c                   eval      i0sski='SK7QW'
713600050523     c                   eval      i0sele=%elem(sk7qw)
713700050523     c                   eval      i0spie=i7qw
713800050523     c                   eval      i0sfile='TABEL00F  '
713900050523     c                   eval      i0scod='7Q-7W'
714000050523     c                   eval      i0spgm='TNSD99R'
714100050523     c                   movel     trul0sds      kpjbu
714200050523     c                   call      'TRUL0SR'
714300050523     c                   parm                    kpjba
714400031201     C                   ENDIF
714500930414     C*
714600930414     C                   ENDSR
714700930414     C********************************************************************
714800930414     C*   RO3ORE - ROUTINE (3) PER RECUPERARE I TEMPI DI TABELLA 1X
714900930510     C*            INPUT:  -WR3TAB (CODICE RECORD IN TABELLA)
715000930510     C*                    -WR3HHI (ORARIO IN ORE/MINUTI)
715100930510     C*           OUTPUT:  -WR3HHO (ORE DA AGGIUNGERE ALLA DATA)
715200930510     C*                    -WR3HHA (ORE DA AGG. PER CALCOLO FINE RANGE)
715300930414     C********************************************************************
715400930414     C     RO3ORE        BEGSR
715500930414     C*
715600930414     C* CERCO I TEMPI NELLA SCHIERA DELLA TABELLA CARICATA IN MEMORIA
715700930604     C* PRIMA COME FILIALE DI ARRIVO E POI COME GENERALE
715800930414     C* E A SECONDA DELL'ORA IN INPUT ALLA SUBROUTINE IMPOSTA ORE DA AGGIUNGERE
715900050902     c                   movel     ' '           WR3MATPOM         1
716000931209     C                   Z-ADD     *ZEROS        WR3HHO
716100931209     C                   Z-ADD     *ZEROS        WR3HHA
716200930414     C                   MOVE      WR3TAB        WR3COD
716300930604     C                   MOVEL     D98LNA        WR3COD            8
716400930421     C                   Z-ADD     1             I
716500930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *FILIALE
716600930414     C     *IN99         IFEQ      '0'
716700930414     C                   MOVE      WR3TAB        WR3COD
716800930414     C                   MOVEL     '   '         WR3COD            8
716900930421     C                   Z-ADD     1             I
717000930421     C     WR3COD        LOOKUP    C1X(I)                                 99    *GENERALE
717100930414     C                   END
717200930414     C     *IN99         IFEQ      '1'                                          --- 1 -->
717300930414     C     WR3HHI        IFEQ      *ZEROS                                       --- 2 -->
717400930510     C                   Z-ADD     G1X(I)        WR3HHO            3 0          *ORE PER ORARIO=ZERO
717500930510     C                   Z-ADD     I1X(I)        WR3HHA            3 0
717600930414     C                   ELSE                                                   <-- 2 -->
717700930421     C     WR3HHI        IFLT      O1X(I)                                       *MINORE ORA DI SOGLI
717800930421     C                   Z-ADD     M1X(I)        WR3HHO
717900930510     C                   Z-ADD     I1X(I)        WR3HHA
718000050902     c                   movel     'M'           WR3MATPOM         1
718100930414     C                   END
718200930421     C     WR3HHI        IFGE      O1X(I)                                       *MAGG.ORA DI SOGLIA
718300930421     C                   Z-ADD     P1X(I)        WR3HHO
718400930510     C                   Z-ADD     S1X(I)        WR3HHA
718500050902     c                   movel     'P'           WR3MATPOM
718600930414     C                   END
718700930414     C                   END                                                    <-- 2 ---
718800930414     C                   END                                                    <-- 1 ---
718900930414     C*
719000930414     C                   ENDSR
719100930423     C********************************************************************
719200930423     C*   W98D98 - PORTA I CAMPI DI LAVORO NELLA DS
719300930423     C********************************************************************
719400930423     C     W98D98        BEGSR
719500930423     C*
719600941228     C*
719700941228     C* ORE REALI PARTENZA INTERNE
719800941228     C                   Z-ADD     W98RPI        W98               3 0
719900941228     C                   EXSR      W98999
720000941228     C                   Z-ADD     D98           D98RPI
720100941228     C* ORE REALI PARTENZA CLIENTE
720200941228     C                   Z-ADD     W98RPC        W98
720300941228     C                   EXSR      W98999
720400941228     C                   Z-ADD     D98           D98RPC
720500930423     C* ORE REALI TRAZIONE INTERNE
720600941228     C                   Z-ADD     W98RTI        W98
720700941228     C                   EXSR      W98999
720800941228     C                   Z-ADD     D98           D98RTI
720900930423     C* ORE REALI TRAZIONE CLIENTE
721000941228     C                   Z-ADD     W98RTC        W98
721100941228     C                   EXSR      W98999
721200941228     C                   Z-ADD     D98           D98RTC
721300930423     C* ORE REALI CONSEGNA INTERNE
721400941228     C                   Z-ADD     W98RCI        W98
721500941228     C                   EXSR      W98999
721600941228     C                   Z-ADD     D98           D98RCI
721700930423     C* ORE REALI CONSEGNA CLIENTE
721800941228     C                   Z-ADD     W98RCC        W98
721900941228     C                   EXSR      W98999
722000941228     C                   Z-ADD     D98           D98RCC
722100930423     C* ORE REALI RESA INTERNE
722200941228     C                   Z-ADD     W98RRI        W98
722300941228     C                   EXSR      W98999
722400941228     C                   Z-ADD     D98           D98RRI
722500930423     C* ORE REALI RESA CLIENTE
722600941228     C                   Z-ADD     W98RRC        W98
722700941228     C                   EXSR      W98999
722800941228     C                   Z-ADD     D98           D98RRC
722900941228     C* ORE DIFF. PARTENZA INTERNE
723000941228     C                   Z-ADD     W98FPI        W98
723100941228     C                   EXSR      W98999
723200941228     C                   Z-ADD     D98           D98FPI
723300941228     C* ORE DIFF. PARTENZA CLIENTE
723400941228     C                   Z-ADD     W98FPC        W98
723500941228     C                   EXSR      W98999
723600941228     C                   Z-ADD     D98           D98FPC
723700930423     C* ORE DIFF. TRAZIONE INTERNE
723800941228     C                   Z-ADD     W98FTI        W98
723900941228     C                   EXSR      W98999
724000941228     C                   Z-ADD     D98           D98FTI
724100930423     C* ORE DIFF. TRAZIONE CLIENTE
724200941228     C                   Z-ADD     W98FTC        W98
724300941228     C                   EXSR      W98999
724400941228     C                   Z-ADD     D98           D98FTC
724500930423     C* ORE DIFF. CONSEGNA INTERNE
724600941228     C                   Z-ADD     W98FCI        W98
724700941228     C                   EXSR      W98999
724800941228     C                   Z-ADD     D98           D98FCI
724900930423     C* ORE DIFF. CONSEGNA CLIENTE
725000941228     C                   Z-ADD     W98FCC        W98
725100941228     C                   EXSR      W98999
725200941228     C                   Z-ADD     D98           D98FCC
725300930423     C* ORE DIFF. RESA INTERNE
725400941228     C                   Z-ADD     W98FRI        W98
725500941228     C                   EXSR      W98999
725600941228     C                   Z-ADD     D98           D98FRI
725700930423     C* ORE DIFF. RESA CLIENTE
725800941228     C                   Z-ADD     W98FRC        W98
725900941228     C                   EXSR      W98999
726000941228     C                   Z-ADD     D98           D98FRC
726100941228     C* ORE NON CONF. PARTENZA INTERNE
726200941228     C     W98NPI        IFGT      999
726300941228     C                   Z-ADD     999           D98NPI
726400941228     C                   ELSE
726500941228     C                   Z-ADD     W98NPI        D98NPI
726600941228     C                   END
726700941228     C* ORE NON CONF. PARTENZA CLIENTE
726800941228     C     W98NPC        IFGT      999
726900941228     C                   Z-ADD     999           D98NPC
727000941228     C                   ELSE
727100941228     C                   Z-ADD     W98NPC        D98NPC
727200941228     C                   END
727300930423     C* ORE NON CONF. TRAZIONE INTERNE
727400930423     C     W98NTI        IFGT      999
727500930423     C                   Z-ADD     999           D98NTI
727600930423     C                   ELSE
727700930423     C                   Z-ADD     W98NTI        D98NTI
727800930423     C                   END
727900930423     C* ORE NON CONF. TRAZIONE CLIENTE
728000930423     C     W98NTC        IFGT      999
728100930423     C                   Z-ADD     999           D98NTC
728200930423     C                   ELSE
728300930423     C                   Z-ADD     W98NTC        D98NTC
728400930423     C                   END
728500930423     C* ORE NON CONF. CONSEGNA INTERNE
728600930423     C     W98NCI        IFGT      999
728700930423     C                   Z-ADD     999           D98NCI
728800930423     C                   ELSE
728900930423     C                   Z-ADD     W98NCI        D98NCI
729000930423     C                   END
729100930423     C* ORE NON CONF. CONSEGNA CLIENTE
729200930423     C     W98NCC        IFGT      999
729300930423     C                   Z-ADD     999           D98NCC
729400930423     C                   ELSE
729500930423     C                   Z-ADD     W98NCC        D98NCC
729600930423     C                   END
729700930423     C* ORE NON CONF. RESA INTERNE
729800930423     C     W98NRI        IFGT      999
729900930423     C                   Z-ADD     999           D98NRI
730000930423     C                   ELSE
730100930423     C                   Z-ADD     W98NRI        D98NRI
730200930423     C                   END
730300930423     C* ORE NON CONF. RESA CLIENTE
730400930423     C     W98NRC        IFGT      999
730500930423     C                   Z-ADD     999           D98NRC
730600930423     C                   ELSE
730700930423     C                   Z-ADD     W98NRC        D98NRC
730800930423     C                   END
730900930423     C* ORE X TURNO CHIUSURA INTERNE
731000941228     C                   Z-ADD     W98HHI        W98
731100941228     C                   EXSR      W98999
731200941228     C                   Z-ADD     D98           D98HHI
731300930423     C* ORE X TURNO CHIUSURA CLIENTE
731400941228     C                   Z-ADD     W98HHC        W98
731500941228     C                   EXSR      W98999
731600941228     C                   Z-ADD     D98           D98HHC
731700060418     C* ORE X eventi partic.che GIUSTIFICANO INTERNE
731800060418     C                   Z-ADD     W98EGI        W98
731900060418     C                   EXSR      W98999
732000060418     C                   Z-ADD     D98           D98hEGI
732100060418     C* ORE X eventi partic.che GIUSTIFICANO CLIENTE
732200060418     C                   Z-ADD     W98EGC        W98
732300060418     C                   EXSR      W98999
732400060418     C                   Z-ADD     D98           D98hEGC
732500060420     C* ORE X eventi partic.che ALLUNGANO    INTERNE
732600060420     C                   Z-ADD     W98EAI        W98
732700060420     C                   EXSR      W98999
732800060420     C                   Z-ADD     D98           D98hEAI
732900060420     C* ORE X eventi partic.che ALLUNGANO    CLIENTE
733000060420     C                   Z-ADD     W98EAC        W98
733100060420     C                   EXSR      W98999
733200060420     C                   Z-ADD     D98           D98hEAC
733300930423     C* ORE X LASC. AVVISO INTERNE
733400941228     C                   Z-ADD     W98HLI        W98
733500941228     C                   EXSR      W98999
733600941228     C                   Z-ADD     D98           D98HLI
733700930423     C* ORE X LASC. AVVISO CLIENTE
733800941228     C                   Z-ADD     W98HLC        W98
733900941228     C                   EXSR      W98999
734000941228     C                   Z-ADD     D98           D98HLC
734100930423     C* ORE X GIACENZA INTERNE
734200941228     C                   Z-ADD     W98HGI        W98
734300941228     C                   EXSR      W98999
734400941228     C                   Z-ADD     D98           D98HGI
734500930423     C* ORE X GIACENZA CLIENTE
734600941228     C                   Z-ADD     W98HGC        W98
734700941228     C                   EXSR      W98999
734800941228     C                   Z-ADD     D98           D98HGC
734900941228     C* ORE X FEST. PART. INTERNE
735000941228     C                   Z-ADD     W98HPI        W98
735100941228     C                   EXSR      W98999
735200941228     C                   Z-ADD     D98           D98HPI
735300941228     C* ORE X FEST. PART. CLIENTE
735400941228     C                   Z-ADD     W98HPC        W98
735500941228     C                   EXSR      W98999
735600941228     C                   Z-ADD     D98           D98HPC
735700930423     C* ORE X FEST. TRAZ. INTERNE
735800941228     C                   Z-ADD     W98HFI        W98
735900941228     C                   EXSR      W98999
736000941228     C                   Z-ADD     D98           D98HFI
736100930423     C* ORE X FEST. TRAZ. CLIENTE
736200941228     C                   Z-ADD     W98HFC        W98
736300941228     C                   EXSR      W98999
736400941228     C                   Z-ADD     D98           D98HFC
736500930423     C* ORE X FEST. CONS. INTERNE
736600941228     C                   Z-ADD     W98HEI        W98
736700941228     C                   EXSR      W98999
736800941228     C                   Z-ADD     D98           D98HEI
736900930423     C* ORE X FEST. CONS. CLIENTE
737000941228     C                   Z-ADD     W98HEC        W98
737100941228     C                   EXSR      W98999
737200941228     C                   Z-ADD     D98           D98HEC
737300930511     C* ORE X F.DEPOSITO INTERNE
737400941228     C                   Z-ADD     W98HDI        W98
737500941228     C                   EXSR      W98999
737600941228     C                   Z-ADD     D98           D98HDI
737700930511     C* ORE X F.DEPOSITO CLIENTE
737800941228     C                   Z-ADD     W98HDC        W98
737900941228     C                   EXSR      W98999
738000941228     C                   Z-ADD     D98           D98HDC
738100930511     C* ORE X CONTRASSEGNO INTERNE
738200941228     C                   Z-ADD     W98HOI        W98
738300941228     C                   EXSR      W98999
738400941228     C                   Z-ADD     D98           D98HOI
738500930511     C* ORE X CONTRASSEGNO CLIENTE
738600941228     C                   Z-ADD     W98HOC        W98
738700941228     C                   EXSR      W98999
738800941228     C                   Z-ADD     D98           D98HOC
738900930914     C* ORE DI CONSEGNABILITA' INTERNE
739000941228     C                   Z-ADD     W98OCI        W98
739100941228     C                   EXSR      W98000
739200941228     C                   Z-ADD     D98           D98OCI
739300930914     C* ORE DI CONSEGNABILITA' CLIENTE
739400941228     C                   Z-ADD     W98OCC        W98
739500941228     C                   EXSR      W98000
739600941228     C                   Z-ADD     D98           D98OCC
739700071122     C
739800071122     c* Per i codici bolla M prepagati annullati, azzero tutto
739900071122     c                   if        d98cbo='M '
740000071122     c                   clear                   d98rpi
740100071122     c                   clear                   d98rpc
740200071122     c                   clear                   d98rti
740300071122     c                   clear                   d98rtc
740400071122     c                   clear                   d98rci
740500071122     c                   clear                   d98rcc
740600071122     c                   clear                   d98rri
740700071122     c                   clear                   d98rrc
740800071122     c                   clear                   d98fpi
740900071122     c                   clear                   d98fpc
741000071122     c                   clear                   d98fti
741100071122     c                   clear                   d98ftc
741200071122     c                   clear                   d98fci
741300071122     c                   clear                   d98fcc
741400071122     c                   clear                   d98fri
741500071122     c                   clear                   d98frc
741600071122     c                   clear                   d98npi
741700071122     c                   clear                   d98npc
741800071122     c                   clear                   d98nti
741900071122     c                   clear                   d98ntc
742000071122     c                   clear                   d98nci
742100071122     c                   clear                   d98ncc
742200071122     c                   clear                   d98nri
742300071122     c                   clear                   d98nrc
742400071122     c                   clear                   d98OCi
742500071122     c                   clear                   d98OCC
742600071122     c                   endif
742700930423     C*
742800100308     c* Imposto comunque i campi della consegna richiesta anche se li ho ignorati ai fini del
742900100308     c*  calcolo
743000100308     c                   eval      d98dcr=s98dcr
743100100308     c                   eval      d98hcr=s98hcr
743200100308     c                   eval      d98tcr=s98tcr
743300100308     c
743400930423     C                   ENDSR
743500941228     C***********************************************************************
743600941228     C*   W98999 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
743700941228     C***********************************************************************
743800941228     C     W98999        BEGSR
743900941228     C     W98           IFLT      -999
744000941228     C                   Z-ADD     -999          D98               3 0
744100941228     C                   ELSE
744200941228     C     W98           IFGT      999
744300941228     C                   Z-ADD     999           D98
744400941228     C                   ELSE
744500941228     C                   Z-ADD     W98           D98
744600941228     C                   END
744700941228     C                   END
744800941228     C                   ENDSR
744900941228     C***********************************************************************
745000941228     C*   W98000 - IMPOSTA DS ESTERNA DAI CAMPI DI LAVORO
745100941228     C***********************************************************************
745200941228     C     W98000        BEGSR
745300941228     C     W98           IFLE      *ZEROS
745400941228     C                   Z-ADD     *ZEROS        D98
745500941228     C                   ELSE
745600941228     C     W98           IFGT      999
745700941228     C                   Z-ADD     999           D98
745800941228     C                   ELSE
745900941228     C                   Z-ADD     W98           D98
746000941228     C                   END
746100941228     C                   END
746200941228     C                   ENDSR
746300930305     C***********************************************************************
746400930305     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
746500930305     C***********************************************************************
746600930305     C     AZZERA        BEGSR
746700150219     c* data e ora del momento
746800150219     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
746900150219     C                   Z-ADD     DSDAT8        WDAODI            8 0
747000150219     C                   Z-ADD     DTORA4        WHHODI            4 0
747100930305     C*
747200141204     C                   MOVEL     'N'           FLGDAN            1            *NO DANNO
747300150603     C                   MOVEL     'N'           opendan
747400141107     c                   clear                   or_ddc
747500141108     c                   clear                   sav_12dat         8 0
747600141107     c                   clear                   or_datarif
747700140402     c                   clear                   wpick2            1
747800140402     c                   clear                   Primotipo
747900090928     c                   clear                   Primodev
748000090928     c                   clear                   Primohev
748100070827     c                   clear                   CallAR5
748200071122     c                   clear                   d98cbo
748300100308     c                   clear                   s98dcr
748400100308     c                   clear                   s98hcr
748500100308     c                   clear                   s98tcr
748600141106     c                   clear                   nosomma           1
748700141229     c                   clear                   wtrasm
748800150324     c                   clear                   avvcmc
748900150325     c                   clear                   DSbl4L
749000150401     c                   clear                   fnlv82ds
749100150331     c                   clear                   DS7R
749200150605     c                   clear                   wDEE              1
749300150629     c                   clear                   wtsogliaE
749400930323     C                   MOVEL     '0'           D98ERR
749500930323     C                   MOVEL     *BLANKS       D98MSG
749600931124     C                   Z-ADD     *ZEROS        D98TPI
749700931124     C                   Z-ADD     *ZEROS        D98TPC
749800930325     C                   Z-ADD     *ZEROS        D98TTI
749900930325     C                   Z-ADD     *ZEROS        D98TTC
750000930325     C                   Z-ADD     *ZEROS        D98TCI
750100930325     C                   Z-ADD     *ZEROS        D98TCC
750200090827     C                   Z-ADD     *ZEROS        w99TCI
750300090827     C                   Z-ADD     *ZEROS        w99TCC
750400930325     C                   Z-ADD     *ZEROS        D98TRI
750500930325     C                   Z-ADD     *ZEROS        D98TRC
750600090921     C                   Z-ADD     *ZEROS        w99TRI
750700090921     C                   Z-ADD     *ZEROS        w99TRC
750800931124     C                   Z-ADD     *ZEROS        D98RPI
750900931124     C                   Z-ADD     *ZEROS        D98RPC
751000930405     C                   Z-ADD     *ZEROS        D98RTI
751100930405     C                   Z-ADD     *ZEROS        D98RTC
751200930405     C                   Z-ADD     *ZEROS        D98RCI
751300930405     C                   Z-ADD     *ZEROS        D98RCC
751400930405     C                   Z-ADD     *ZEROS        D98RRI
751500930405     C                   Z-ADD     *ZEROS        D98RRC
751600931124     C                   Z-ADD     *ZEROS        D98FPI
751700931124     C                   Z-ADD     *ZEROS        D98FPC
751800930325     C                   Z-ADD     *ZEROS        D98FTI
751900930325     C                   Z-ADD     *ZEROS        D98FTC
752000930325     C                   Z-ADD     *ZEROS        D98FCI
752100930325     C                   Z-ADD     *ZEROS        D98FCC
752200930325     C                   Z-ADD     *ZEROS        D98FRI
752300930325     C                   Z-ADD     *ZEROS        D98FRC
752400931124     C                   MOVEL     *BLANKS       D98CPI
752500931124     C                   MOVEL     *BLANKS       D98CPC
752600930325     C                   MOVEL     *BLANKS       D98CTI
752700930325     C                   MOVEL     *BLANKS       D98CTC
752800930325     C                   MOVEL     *BLANKS       D98CCI
752900930325     C                   MOVEL     *BLANKS       D98CCC
753000930325     C                   MOVEL     *BLANKS       D98CRI
753100930325     C                   MOVEL     *BLANKS       D98CRC
753200931124     C                   Z-ADD     *ZEROS        D98NPI
753300931124     C                   Z-ADD     *ZEROS        D98NPC
753400930325     C                   Z-ADD     *ZEROS        D98NTI
753500930325     C                   Z-ADD     *ZEROS        D98NTC
753600930325     C                   Z-ADD     *ZEROS        D98NCI
753700930325     C                   Z-ADD     *ZEROS        D98NCC
753800930325     C                   Z-ADD     *ZEROS        D98NRI
753900930325     C                   Z-ADD     *ZEROS        D98NRC
754000930414     C                   Z-ADD     *ZEROS        D98HHI
754100930414     C                   Z-ADD     *ZEROS        D98HHC
754200930414     C                   Z-ADD     *ZEROS        D98HOI
754300930414     C                   Z-ADD     *ZEROS        D98HOC
754400930414     C                   Z-ADD     *ZEROS        D98HLI
754500930414     C                   Z-ADD     *ZEROS        D98HLC
754600930414     C                   Z-ADD     *ZEROS        D98HGI
754700930414     C                   Z-ADD     *ZEROS        D98HGC
754800931124     C                   Z-ADD     *ZEROS        D98HPI
754900931124     C                   Z-ADD     *ZEROS        D98HPC
755000930414     C                   Z-ADD     *ZEROS        D98HFI
755100930414     C                   Z-ADD     *ZEROS        D98HFC
755200930416     C                   Z-ADD     *ZEROS        D98HEI
755300930416     C                   Z-ADD     *ZEROS        D98HEC
755400930414     C                   Z-ADD     *ZEROS        D98HRI
755500930414     C                   Z-ADD     *ZEROS        D98HRC
755600930511     C                   Z-ADD     *ZEROS        D98HDI
755700930511     C                   Z-ADD     *ZEROS        D98HDC
755800930325     C                   Z-ADD     *ZEROS        D98DCI
755900930325     C                   Z-ADD     *ZEROS        D98HCI
756000930325     C                   Z-ADD     *ZEROS        D98DCE
756100930325     C                   Z-ADD     *ZEROS        D98HCE
756200931125     C                   Z-ADD     *ZEROS        D98OCI
756300931125     C                   Z-ADD     *ZEROS        D98OCC
756400931202     C                   Z-ADD     *ZEROS        D98DEI
756500931202     C                   Z-ADD     *ZEROS        D98OEI
756600931202     C                   Z-ADD     *ZEROS        D98DEE
756700931202     C                   Z-ADD     *ZEROS        D98OEE
756800150325     C                   clear                   D98dtctd
756900150326     C                   clear                   D98picklim
757000150325     C                   clear                   D98spcdee
757100150331     C                   clear                   fddata
757200140825     c
757300141017     c                   clear                   d98ddc
757400141017     c                   clear                   d98cons
757500141017     c                   clear                   wd98ndc
757600140910     c                   clear                   wd98fbc
757700150119     c                   clear                   wd98acc
757800140825     c                   clear                   wtrulor
757900150219     c
758000150219     c                   clear                   IMPdata
758100150219     c                   clear                   IMPfgs
758200150219     c                   clear                   DISdata
758300150219     c                   clear                   DISfgs
758400150219     c                   clear                   DISora
758500150219     c                   clear                   DISnpg
758600150219     c                   clear                   IDnpg
758700150219     c                   clear                   IDdata
758800150219     c                   clear                   IDora
758900150219     c                   clear                   IDfgs
759000931125     C* CAMPI VARI
759100931125      *   SE IL PGM TROVA IN NUMERO SPEDIZIONE LEGGE GLI ARCHIVI     *
759200140825      *   FNBLP E FNARB E QUINDI AZZERA I CAMPI CHE VERRANNO RIEMPITI*
759300140825      *   DALLA LETTURA DI QUESTI FILE                               *
759400931125      *   ALTRIMENTI UTILIZZA PER I CALCOLI I CAMPI                  *
759500931125      *   DELLA DS PASSATI DAL PGM CHIAMANTE SENZA AZZERARLI         *
759600931125     C     D98NSP        IFNE      *ZEROS
759700930525     C                   MOVEL     *BLANKS       D98TSP
759800941228     C                   MOVEL     *BLANKS       D98CAD
759900970728     C                   MOVEL     *BLANKS       D98LOD
760000930525     C                   Z-ADD     *ZEROS        D98TFP
760100930525     C                   Z-ADD     *ZEROS        D98TFA
760200930525     C                   Z-ADD     *ZEROS        D98LNA
760300930525     C                   Z-ADD     *ZEROS        D98DSP
760400941228     C                   Z-ADD     *ZEROS        D98MGS
760500930525     C                   Z-ADD     *ZEROS        D98DTI
760600930525     C                   Z-ADD     *ZEROS        D98HTI
760700930525     C                   Z-ADD     *ZEROS        D98DCM
760800930525     C                   Z-ADD     *ZEROS        D98HMC
760900930525     C                   MOVEL     *BLANKS       D98GC1
761000930525     C                   MOVEL     *BLANKS       D98GC2
761100010820     C                   Z-ADD     *ZEROS        D98CAS
761200010821     C                   MOVEL     *BLANKS       D98VCA
761300930525     C                   MOVEL     *BLANKS       D98TIC
761400930525     C                   Z-ADD     *ZEROS        D98DCR
761500930525     C                   Z-ADD     *ZEROS        D98HCR
761600930525     C                   MOVEL     *BLANKS       D98TCR
761700050914     C                   MOVEL     *BLANKS       Consideraora      1
761800930525     C                   MOVEL     *BLANKS       D98CMC
761900930525     C                   Z-ADD     *ZEROS        D98DAG
762000930525     C                   Z-ADD     *ZEROS        D98DCG
762100930525     C                   MOVEL     *BLANKS       D98FFD
762200930525     C                   Z-ADD     *ZEROS        D98NCD
762300930525     C                   MOVEL     *BLANKS       D98FNS
762400931124     C                   Z-ADD     *ZEROS        D98DAM
762500940628     C                   MOVEL     *BLANKS       D98FIN
762600940628     C                   Z-ADD     *ZEROS        D98KSC
762700940628     C                   MOVEL     *BLANKS       D98TBL
762800931125     C                   END
762900960229     C     D98TLA        IFEQ      'T'
763000960229     C                   Z-ADD     *ZEROS        D98DTI
763100960229     C                   Z-ADD     *ZEROS        D98HTI
763200960229     C                   ENDIF
763300931125     C* CAMPI WORK
763400930406     C                   Z-ADD     *ZEROS        WPODSP            9 0
763500930406     C                   Z-ADD     *ZEROS        WPODTI            9 0
763600150630     C                   Z-ADD     *ZEROS        WPODTIC           9 0
763700150331     C                   Z-ADD     *ZEROS        WPOfddata         9 0
763800930406     C                   Z-ADD     *ZEROS        WPODCM            9 0
763900941228     C                   Z-ADD     *ZEROS        WPODUC            9 0
764000960402     C                   Z-ADD     *ZEROS        WPODBR            9 0
764100930406     C                   Z-ADD     *ZEROS        WPODCR            9 0
764200930420     C                   Z-ADD     *ZEROS        WPOCRI            9 0
764300940324£$   C                   Z-ADD     *ZEROS        WPOCRC            9 0
764400930420     C                   Z-ADD     *ZEROS        WPOCRF            9 0
764500150330     C                   Z-ADD     *ZEROS        WPOCRF_dee        9 0
764600931206     C                   Z-ADD     *ZEROS        WPOCR1            9 0
764700931206     C                   Z-ADD     *ZEROS        WPOCR2            9 0
764800150608     C                   Z-ADD     *ZEROS        WPOCR2_dee        9 0
764900930419     C                   Z-ADD     *ZEROS        WPODCI            9 0
765000930420     C                   Z-ADD     *ZEROS        WPODCE            9 0
765100930419     C                   Z-ADD     *ZEROS        WPODCF            9 0
765200930406     C                   Z-ADD     *ZEROS        WPODAG            9 0
765300930406     C                   Z-ADD     *ZEROS        WPODCG            9 0
765400940324£$   C                   Z-ADD     *ZEROS        WPOCGC            9 0
765500941229     C                   Z-ADD     *ZEROS        WPOAGC            9 0
765600930406     C                   Z-ADD     *ZEROS        WPOLIM            9 0
765700050922     C                   Z-ADD     *ZEROS        WPOATTData00      9 0
765800050922     C                   Z-ADD     *ZEROS        WPOATTData24      9 0
765900140820     C                   Z-ADD     *ZEROS        WPOrifDatLav      9 0
766000930511     C                   Z-ADD     *ZEROS        WPOASI            9 0
766100930511     C                   Z-ADD     *ZEROS        WPOASF            9 0
766200930608     C                   Z-ADD     *ZEROS        WPOAII            9 0
766300930608     C                   Z-ADD     *ZEROS        WPOAFI            9 0
766400930608     C                   Z-ADD     *ZEROS        WPOAIC            9 0
766500930608     C                   Z-ADD     *ZEROS        WPOAFC            9 0
766600930511     C                   Z-ADD     *ZEROS        WPOFDI            9 0
766700930511     C                   Z-ADD     *ZEROS        WPOFDF            9 0
766800930608     C                   Z-ADD     *ZEROS        WPOFII            9 0
766900930608     C                   Z-ADD     *ZEROS        WPOFFI            9 0
767000930608     C                   Z-ADD     *ZEROS        WPOFIC            9 0
767100930608     C                   Z-ADD     *ZEROS        WPOFFC            9 0
767200930913     C                   Z-ADD     *ZEROS        WPODIC            9 0
767300930913     C                   Z-ADD     *ZEROS        WPODFC            9 0
767400140822     C                   Z-ADD     *ZEROS        WPODFC_00         9 0
767500931129     C                   Z-ADD     *ZEROS        WPOLAF            9 0
767600060420     C                   Z-ADD     *ZEROS        WPOpaif           9 0
767700960129     C                   Z-ADD     *ZEROS        WPOGIF            9 0
767800931202     C                   Z-ADD     *ZEROS        WPODEI            9 0
767900140825     C                   Z-ADD     *ZEROS        WPODEI_dcr        9 0
768000140825     C                   Z-ADD     *ZEROS        WPODEE_dcr        9 0
768100150608     C                   Z-ADD     *ZEROS        WPOAFF_dcr        9 0
768200140820     C                   Z-ADD     *ZEROS        WPOdpcee          9 0
768300941229     C                   Z-ADD     *ZEROS        W98DCG            8 0
768400970304     C******************** Z-ADD*ZEROS    W98DBR  80
768500970310     C                   Z-ADD     *ZEROS        W98HUC            4 0
768600150508     C                   Z-ADD     *ZEROS        W98DPC            8 0
768700050922     C*****              Z-ADD     *ZEROS        W98DTI            8 0
768800930414     C*
768900941228     C                   Z-ADD     *ZEROS        W98RPI            5 0
769000941228     C                   Z-ADD     *ZEROS        W98RPC            5 0
769100930423     C                   Z-ADD     *ZEROS        W98RTI            5 0
769200930423     C                   Z-ADD     *ZEROS        W98RTC            5 0
769300930423     C                   Z-ADD     *ZEROS        W98RCI            5 0
769400930423     C                   Z-ADD     *ZEROS        W98RCC            5 0
769500930423     C                   Z-ADD     *ZEROS        W98RRI            5 0
769600930423     C                   Z-ADD     *ZEROS        W98RRC            5 0
769700941228     C                   Z-ADD     *ZEROS        W98FPI            5 0
769800941228     C                   Z-ADD     *ZEROS        W98FPC            5 0
769900930423     C                   Z-ADD     *ZEROS        W98FTI            5 0
770000930423     C                   Z-ADD     *ZEROS        W98FTC            5 0
770100930423     C                   Z-ADD     *ZEROS        W98FCI            5 0
770200930423     C                   Z-ADD     *ZEROS        W98FCC            5 0
770300930423     C                   Z-ADD     *ZEROS        W98FRI            5 0
770400930423     C                   Z-ADD     *ZEROS        W98FRC            5 0
770500941228     C                   Z-ADD     *ZEROS        W98NPI            5 0
770600941228     C                   Z-ADD     *ZEROS        W98NPC            5 0
770700930423     C                   Z-ADD     *ZEROS        W98NTI            5 0
770800930423     C                   Z-ADD     *ZEROS        W98NTC            5 0
770900930423     C                   Z-ADD     *ZEROS        W98NCI            5 0
771000930423     C                   Z-ADD     *ZEROS        W98NCC            5 0
771100930423     C                   Z-ADD     *ZEROS        W98NRI            5 0
771200930423     C                   Z-ADD     *ZEROS        W98NRC            5 0
771300930423     C                   Z-ADD     *ZEROS        W98HHI            5 0
771400930423     C                   Z-ADD     *ZEROS        W98HHC            5 0
771500060418     C                   Z-ADD     *ZEROS        W98EGI            5 0
771600060420     C                   Z-ADD     *ZEROS        W98EAI            5 0
771700060418     C                   Z-ADD     *ZEROS        W98EGC            5 0
771800060420     C                   Z-ADD     *ZEROS        W98EAC            5 0
771900930423     C                   Z-ADD     *ZEROS        W98HLI            5 0
772000930423     C                   Z-ADD     *ZEROS        W98HLC            5 0
772100930423     C                   Z-ADD     *ZEROS        W98HGI            5 0
772200930423     C                   Z-ADD     *ZEROS        W98HGC            5 0
772300930423     C                   Z-ADD     *ZEROS        W98HFI            5 0
772400930423     C                   Z-ADD     *ZEROS        W98HFC            5 0
772500930423     C                   Z-ADD     *ZEROS        W98HEI            5 0
772600930423     C                   Z-ADD     *ZEROS        W98HEC            5 0
772700930511     C                   Z-ADD     *ZEROS        W98HDI            5 0
772800930511     C                   Z-ADD     *ZEROS        W98HDC            5 0
772900930511     C                   Z-ADD     *ZEROS        W98HOI            5 0
773000930511     C                   Z-ADD     *ZEROS        W98HOC            5 0
773100930914     C                   Z-ADD     *ZEROS        W98OCI            5 0
773200930914     C                   Z-ADD     *ZEROS        W98OCC            5 0
773300941228     C                   Z-ADD     *ZEROS        W98HPI            5 0
773400941228     C                   Z-ADD     *ZEROS        W98HPC            5 0
773500930423     C*
773600930511     C                   Z-ADD     *ZEROS        W98HF             5 0
773700930423     C                   Z-ADD     *ZEROS        W98HH             5 0
773800060418     C                   Z-ADD     *ZEROS        W98EG             5 0
773900060420     C                   Z-ADD     *ZEROS        W98EA             5 0
774000930423     C                   Z-ADD     *ZEROS        W98HG             5 0
774100930423     C                   Z-ADD     *ZEROS        W98HL             5 0
774200930423     C                   Z-ADD     *ZEROS        W98HO             5 0
774300930511     C                   Z-ADD     *ZEROS        W98HD             5 0
774400930611     C*
774500930611     C                   Z-ADD     *ZEROS        WORELA            3 0
774600960129     C                   Z-ADD     *ZEROS        WOREGI            3 0
774700930611     C                   Z-ADD     *ZEROS        WOREAS            3 0
774800930611     C                   Z-ADD     *ZEROS        WOREFD            3 0
774900931207     C                   Z-ADD     *ZEROS        WORECR            3 0
775000090902     c
775100090902     c                   clear                   wmancaTRAZ
775200150113     c                   clear                   wcaldpcee         1
775300150220     c                   clear                   evemic            2
775400930305     C*
775500060406     C                   clear                   LAI                            *LASC.AVVISO INIZIO
775600060406     C                   clear                   LAF                            *LASC.AVVISO FINE
775700091014     C                   clear                   LAp                            *posizione l.avviso
775800060406£$   C                   clear                   LCI                            *L.AVV.INIZ X CONSEG
775900060406£$   C                   clear                   LCF                            *L.AVV.FINE X CONSEG
776000060406c    c                   clear                   LCon
776100060406     C                   clear                   EVD                            *DATA EVENTO
776200060406     C                   clear                   EVO                            *ORA EVENTO
776300060406     C                   clear                   EVC                            *CAUSALE EVENTO
776400131218     C                   clear                   EVT
776500060406     C                   clear                   pVD                            *DATA EVENTO
776600060406     C                   clear                   pVO                            *ORA EVENTO
776700060406     C                   clear                   pVC                            *CAUSALE EVENTO
776800060406     C                   clear                   pVF                            *CAUSALE EVENTO
776900060406     C                   clear                   TII                            *T.CHIUS. INT. INIZI
777000060406     C                   clear                   TIF                            *T.CHIUS. INT. FINE
777100060406     C                   clear                   TCI                            *T.CHIUS. CLI. INIZI
777200060406     C                   clear                   TCF                            *T.CHIUS. CLI. FINE
777300060406     C                   clear                   COV                            *DATE FINE L.A. E GI
777400060406     C                   clear                   GII                            *EVENTI GIAC. INIZIO
777500060406     C                   clear                   GIF                            *EVENTI GIAC. FINE
777600091014     C                   clear                   GIp                            *posizione vento gia
777700060406     C                   clear                   GCI                            *EV.GIAC.IN.X CONSEG
777800060406     C                   clear                   GCF                            *EV.GIAC.FI.X CONSEG
777900060406     C                   clear                   GCPD                           *EV.GIAC.FI.X CONSEG
778000931213     C*
778100060406     C                   clear                   TUC                            *TURNI CHIUSURA
778200931201     C                   Z-ADD     *ZEROS        TI                3 0          *INDICE T.CHIUS. INT
778300931201     C                   Z-ADD     *ZEROS        TC                3 0          *INDICE T.CHIUS. CLI
778400060406     c
778500060406     C                   Z-ADD     *ZEROS        pgi               3 0          *INDICE T.CHIUS. INT
778600060406     C                   Z-ADD     *ZEROS        pgc               3 0          *INDICE T.CHIUS. CLI
778700060406     c                   clear                   pgii
778800060406     c                   clear                   pgif
778900060406     c                   clear                   pgci
779000060406     c                   clear                   pgcf
779100060420     C                   Z-ADD     *ZEROS        pai               3 0          *INDICE T.CHIUS. CLI
779200060420     c                   clear                   paii
779300060420     c                   clear                   paif
779400930420     C*
779500930305     C                   ENDSR
779600930512     C***********************************************************************
779700930513     C*   FORCON - FORZA CONFORMITA' = SI PER LA BOLLA
779800930512     C***********************************************************************
779900930512     C     FORCON        BEGSR
780000930512     C*
780100931124     C                   Z-ADD     *ZEROS        D98RPI
780200931124     C                   Z-ADD     *ZEROS        D98RPC
780300930512     C                   Z-ADD     *ZEROS        D98RTI
780400930512     C                   Z-ADD     *ZEROS        D98RTC
780500930512     C                   Z-ADD     *ZEROS        D98RCI
780600930512     C                   Z-ADD     *ZEROS        D98RCC
780700930512     C                   Z-ADD     *ZEROS        D98RRI
780800930512     C                   Z-ADD     *ZEROS        D98RRC
780900931124     C                   Z-ADD     *ZEROS        D98FPI
781000931124     C                   Z-ADD     *ZEROS        D98FPC
781100930512     C                   Z-ADD     *ZEROS        D98FTI
781200930512     C                   Z-ADD     *ZEROS        D98FTC
781300930512     C                   Z-ADD     *ZEROS        D98FCI
781400930512     C                   Z-ADD     *ZEROS        D98FCC
781500930512     C                   Z-ADD     *ZEROS        D98FRI
781600930512     C                   Z-ADD     *ZEROS        D98FRC
781700931124     C                   MOVEL     'S'           D98CPI
781800931124     C                   MOVEL     'S'           D98CPC
781900930512     C                   MOVEL     'S'           D98CTI
782000930512     C                   MOVEL     'S'           D98CTC
782100930512     C                   MOVEL     'S'           D98CCI
782200930512     C                   MOVEL     'S'           D98CCC
782300930512     C                   MOVEL     'S'           D98CRI
782400930512     C                   MOVEL     'S'           D98CRC
782500931124     C                   Z-ADD     *ZEROS        D98NPI
782600931124     C                   Z-ADD     *ZEROS        D98NPC
782700930512     C                   Z-ADD     *ZEROS        D98NTI
782800930512     C                   Z-ADD     *ZEROS        D98NTC
782900930512     C                   Z-ADD     *ZEROS        D98NCI
783000930512     C                   Z-ADD     *ZEROS        D98NCC
783100930512     C                   Z-ADD     *ZEROS        D98NRI
783200930512     C                   Z-ADD     *ZEROS        D98NRC
783300930512     C                   Z-ADD     *ZEROS        D98HHI
783400930512     C                   Z-ADD     *ZEROS        D98HHC
783500930512     C                   Z-ADD     *ZEROS        D98HOI
783600930512     C                   Z-ADD     *ZEROS        D98HOC
783700930512     C                   Z-ADD     *ZEROS        D98HLI
783800930512     C                   Z-ADD     *ZEROS        D98HLC
783900930512     C                   Z-ADD     *ZEROS        D98HGI
784000930512     C                   Z-ADD     *ZEROS        D98HGC
784100931124     C                   Z-ADD     *ZEROS        D98HPI
784200931124     C                   Z-ADD     *ZEROS        D98HPC
784300930512     C                   Z-ADD     *ZEROS        D98HFI
784400930512     C                   Z-ADD     *ZEROS        D98HFC
784500930512     C                   Z-ADD     *ZEROS        D98HEI
784600930512     C                   Z-ADD     *ZEROS        D98HEC
784700930512     C                   Z-ADD     *ZEROS        D98HRI
784800930512     C                   Z-ADD     *ZEROS        D98HRC
784900930512     C                   Z-ADD     *ZEROS        D98HDI
785000930512     C                   Z-ADD     *ZEROS        D98HDC
785100930520     C*
785200930512     C                   ENDSR
785300930305     C********************************************************************
785400971127** ======== SCHIERA: GPM  (GIORNI PROGRESSIVI MESE)        ================
785500950406031059090120151181212243273304334365
785600950406** ======== SCHIERA: MES  (GIORNI DEL MESE)                ================
785700950406312831303130313130313031
785800930304** ======== SCHIERA: MSG  (MESSAGGI)                       ================
785900971127TNSD99R- Tabella '1X' non trovata. Avvertire CED.                         TB1X 1
786000971127TNSD99R- Bolla non ancora spedita. Impossibile calcolare delivery.        BONS 2
786100971127TNSD99R- Calendario non trovato. Avvertire CED.                           CBNT 3
786200971127TNSD99R- Simbolo calendario non trovato in tabella "1Y". Avvertire CED.   SCNT 4
786300971127TNSD99R- Tabella '1Y' non trovata. Avvertire CED.                         TB1Y 5
786400971127TNSD99R- Tabella '7A' non trovata. Avvertire CED.                         TB7A 6
786500971127TNSD99R- Tabella '1A' non trovata. Avvertire CED.                         TB1A 7
786600971127TNSD99R- Tabella '2A' non trovata. Avvertire CED.                         TB2A 8
786700971127TNSD99R- Causale evento non trovata in tabella '2A'. Avvertire CED.       EV2A 9
786800971127TNSD99R- Bolla di recupero. Impossibile calcolare delivery.               BREC10
786900971127TNSD99R- Filiale di arrivo non meccanizzata. Impossibile calc.delivery.   FANM11
787000971127TNSD99R- Data per calcolo consegnabilità maggiore data consegna.          DCCM12
787100971127TNSD99R- Data inizio consegnabilità errata.                               DICE13
787200971127TNSD99R- Ora inizio consegnabilità errata.                                OICE14
787300971127TNSD99R- Data fine consegnabilità errata.                                 DFCE15
787400971127TNSD99R- Ora fine consegnabilità errata.                                  OFCE16
787500971127TNSD99R- Manca la data di fine consegnabilità. Impossibile calcolarla.    MDFC17
787600971127TNSD99R- Data inizio consegnabilità maggiore data fine consegnabilità.    DIDF18
787700971127TNSD99R- Data per calcolo consegnabilità inferiore data arrivo trazione.  DCCI19
787800971127TNSD99R- Bolla inesistente.                                               BOIN20
787900971127TNSD99R-     Cap inesistente.                                             CAPI21
788000971127TNSD99R- Tabella '3A' non trovata.                                        TB3A22
788100971127TNSD99R- Codice bolla non trovato in tabella '3A'.                        CBON23
788200971127TNSD99R-     Nazione non trovata in tab. '15'.                            NANT24
788300971127TNSD99R-     Tabella '15' non trovata. Avvertire CED.                     TB1525
788400971127TNSD99R- Non sono stati trovati tempi di partenza accettabili.            NTTP26
788500971127TNSD99R- Non sono stati trovati tempi di trazione accettabili.            NTTT27
788600971127TNSD99R-     Non sono stati trovati tempi di consegna accettabili.        NTTC28
788700971127TNSD99R- Contrassegno non trovato.                                        CONT29
788800971127TNSD99R- Data riferimento inferiore alla data spedizione !!.              DRFI30
788900990212TNSD99R- Bolla con pratica danni. Impossibile calcolare delivery.         BPDA31
789000990308TNSD99R- Bolla di reso in contoservizio. Impossibile calc. delivery.      BRCS32
789100060406TNSD99R- Tabella '2Z' non trovata. Avvertire CED.                         TB2A 8
789200060406TNSD99R- Per un record di tabella 2Z non esiste la 2A.Avvertire CED           34
789300971127TNSD99R- 35                                                                   35
789400971127TNSD99R- 36                                                                   36
789500971127TNSD99R- 37                                                                   37
789600971127TNSD99R- 38                                                                   38
789700971127TNSD99R- 39                                                                   39
789800971127TNSD99R- 40                                                                   40
